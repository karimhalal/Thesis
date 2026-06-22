
# IPTW-DR-AJ: DOUBLY ROBUST IPTW (double confounder+ AALEN-JOHANSEN CAUSE-SPECIFIC COX
#
# Exposure  : first_drug_class (3-category)
#               0 = No prescription (reference)
#               1 = Opioid initiated
#               2 = BZD initiated
#
# Outcome   : event == 1 (NMS-related death)
# Competing : event == 2 (other-cause death)
# Censored  : event == 0
#
# Estimand  : ATT (Average Treatment effect on the Treated)
#             For each treated level a ∈ {1, 2}:
#               ATT_RD(a) = E[ CIF₁(t | A=a, L) − CIF₁(t | A=0, L) | A=a ]
#               ATT_RR(a) = E[ CIF₁(t | A=a, L) | A=a ] /
#                           E[ CIF₁(t | A=0, L) | A=a ]
#
# Method    : Step 1 — Multinomial PS model (nnet::multinom) → stabilised ATT weights
#             Step 2 — Weighted doubly-robust cause-specific Cox
#                      (exposure + full DAG covariates in outcome model)
#             Step 3 — AJ CIF integration, standardised over the TREATED group (ATT)
#             Step 4 — Non-parametric bootstrap variance per imputed dataset
#             Step 5 — Rubin's rules pooling on log-RR and RD scales
#
# PS model  : Minimal adjustment set (dags.R — dag_controlled_substance):
#               Age, alcohol consumption, chronic conditions (self report),
#               material deprivation, mental health conditions, pain,
#               self-report drug use, sex, survey cycle
#             Note: bidirected edge prescription drug abuse <-> self-report
#             drug use implies an unmeasured common cause; self-report drug
#             use is still adjusted as marked [adjusted] in the DAG.
#
# DAG ref   : dags.R — dag_controlled_substance
# Precondition: associative_modelling.R must be sourced first (term vectors)

library(nnet)
library(survival)
library(dplyr)
library(purrr)
library(tibble)
library(ggplot2)



# 1.  ADJUSTMENT SET TERMS (dag_controlled_substance minimal adjustment set)
#
# Inherits all .* term vectors from associative_modelling.R.
# Adjusted nodes: Age, alcohol consumption, chronic conditions (self report),
# material deprivation, mental health conditions, pain, self-report drug use, sex.

.fdrug_adj_ff  <- c(
  .age_ff,
  .alc_ff,          # alcohol consumption (ALWDWKY)
  .mdep_ff,
  .mhccc_ff,        # mental health conditions: CCC_280/290
  .pain_ff,
  .drug_ff,         # self-report drug use (drgdvyac)
  .sex_ff,
  .cycle_ff,
  .int_age_pain_ff,
  .int_age_alc_ff   # age × alcohol interaction
)

.fdrug_adj_raw <- c(
  .age_raw,
  .alc_raw,
  .mdep_raw,
  .mhccc_raw,
  .pain_raw,
  .drug_raw,
  .sex_raw,
  .cycle_raw,
  .int_age_pain_raw,
  .int_age_alc_raw
)

# PS model uses raw (uncentered) terms; interaction terms excluded to reduce
# collinearity risk in multinom().
.fdrug_ps_terms <- c(
  .age_raw,
  .alc_raw,
  .mdep_raw,
  .mhccc_raw,
  .pain_raw,
  .drug_raw,
  .sex_raw,
  .cycle_raw
)

frm_ps <- as.formula(
  paste("factor(first_drug_class) ~",
        paste(.fdrug_ps_terms, collapse = " + "))
)

# Doubly-robust outcome model formulas (functional forms + exposure)
frm_fdrug_ff_e1 <- .mf(
  "Surv(time_to_event, event1 == 1)",
  c("factor(first_drug_class)", .fdrug_adj_ff)
)

# Competing event model: exposure included for full double-robustness protection.
# The DAG encodes no direct causal path from prescription initiation to
# other-cause mortality, but including the exposure guards against model
# misspecification in the nuisance model.
frm_fdrug_ff_e2 <- .mf("Surv(time_to_event, event1 == 2)",
                        c("factor(first_drug_class)", .fdrug_adj_ff))


# 2.  STABILISED ATT WEIGHTS FROM MULTINOMIAL PS
#
# For each pairwise comparison a vs 0 (a ∈ {1, 2}):
#
#   Treated (A = a): w = 1
#   Control (A = 0): w = P(A=a|L) / P(A=0|L)
#
# These are the propensity odds and are the correct ATT weights. No stabilisation
# or trimming is applied: for ATT the weights are already bounded by the formula
# (controls with low treatment propensity receive small weights).
#
# Returns: weights (named list of full-length vectors, NA for excluded obs),
#          ps_fit, ps_mat, ps_check (per-level diagnostics).

# Drop single-level factor predictors from a formula and return the pruned formula.
# Avoids "contrasts can only be computed for variables with 2 or more levels"
# when bootstrap resamples or sparse imputed datasets eliminate a category.
# Diagnose why coxph would see zero complete cases.
# Prints: nrow, per-variable NA counts, number of complete rows.
# Stops with the names of the offending columns so the caller knows what to fix.
.diagnose_coxph_data <- function(formula, dat, label = "") {
  n <- nrow(dat)
  vars <- intersect(all.vars(formula), names(dat))
  na_counts <- vapply(vars, function(v) sum(is.na(dat[[v]])), integer(1L))
  n_cc <- sum(complete.cases(dat[, vars, drop = FALSE]))
  msg <- paste0(
    label, ": nrow=", n, ", complete_cases=", n_cc,
    if (any(na_counts > 0))
      paste0("\n  NA counts: ",
             paste(names(na_counts[na_counts > 0]),
                   na_counts[na_counts > 0], sep = "=", collapse = ", "))
    else ""
  )
  if (n == 0L || n_cc == 0L) stop(msg) else message(msg)
  invisible(NULL)
}

.prune_ps_formula <- function(formula, data) {
  data      <- droplevels(data)
  rhs_vars  <- all.vars(formula[-2L])
  bad       <- vapply(rhs_vars, function(v) {
    x <- data[[v]]
    is.factor(x) && nlevels(x) < 2L
  }, logical(1L))
  if (any(bad)) {
    warning("PS formula: dropping single-level predictors: ",
            paste(rhs_vars[bad], collapse = ", "), call. = FALSE)
    formula <- reformulate(rhs_vars[!bad], response = deparse(formula[[2L]]))
  }
  list(formula = formula, data = data)
}

.compute_att_sw <- function(data,
                             ps_formula     = frm_ps,
                             exposure_var   = "first_drug_class",
                             treated_levels = c("1", "2"),
                             ref_level      = "0") {

  A  <- as.character(data[[exposure_var]])
  n  <- nrow(data)

  pruned     <- .prune_ps_formula(ps_formula, data)
  ps_formula <- pruned$formula
  data       <- pruned$data


  #fit multinomial model to estimate treatment propensities and extract them in ps_mat object
  ps_fit <- nnet::multinom(
    ps_formula, data = data, trace = FALSE,
    MaxNWts = 10000, maxit = 500
  )
  ps_mat <- predict(ps_fit, type = "probs")

  # multinom() returns a vector (not matrix) when only two classes appear
if (is.null(dim(ps_mat))) {
  lev <- sort(unique(A))          # whatever two classes are actually here
  ps_mat <- cbind(1 - ps_mat, ps_mat)
  colnames(ps_mat) <- lev
}

  weights_list <- setNames(vector("list", length(treated_levels)), treated_levels)
  ps_check     <- list()

  for (a in treated_levels) {
    in_comp  <- A %in% c(ref_level, a)
    A_sub    <- A[in_comp]
    ps_a_sub <- ps_mat[in_comp, a]
    ps_0_sub <- ps_mat[in_comp, ref_level]

    n_a <- sum(A_sub == a)
    n_0 <- sum(A_sub == ref_level)

    w_sub <- ifelse(A_sub == a, 1.0, ps_a_sub / ps_0_sub)###CONFIRM THE CONVERSION HERE

    ps_check[[a]] <- list(
      n_treated           = n_a,
      n_control           = n_0,
      ctrl_weight_summary = summary(w_sub[A_sub == ref_level]),
      ctrl_ESS            = sum(w_sub[A_sub == ref_level])^2 /
                              sum(w_sub[A_sub == ref_level]^2)
    )

    w_full          <- rep(NA_real_, n)
    w_full[in_comp] <- w_sub
    weights_list[[a]] <- w_full
  }

  list(weights = weights_list, ps_fit = ps_fit, ps_mat = ps_mat, ps_check = ps_check)
}


# 3.  ATT MARGINAL CIF (DOUBLY ROBUST AJ STANDARDISATION)
#
# Given weighted cause-specific Cox models fit on the restricted subsample
# {A ∈ {0, a}}, compute the ATT marginal CIF by standardising the individual-
# level Aalen-Johansen CIF over the TREATED group only (A = a):
#
#   CIF_ATT(t | do(A=x)) = (1/n_a) Σ_{i: A_i=a} CIF̂₁(t | A=x, L_i)
#
# for x ∈ {a, 0}. The difference/ratio of these two quantities is the ATT.

#extract baseline hazards at event times
.get_H0_at <- function(bh, query_times) {
  idx <- findInterval(query_times, bh$time)
  ifelse(idx == 0L, 0.0, bh$hazard[idx])
}

.aj_marginal_cif <- function(fit1, fit2, data_std,
                              exposure_var,
                              intervention_level,
                              all_levels,
                              time_horizon) {

  d_int <- data_std
  d_int[[exposure_var]] <- factor(intervention_level, levels = all_levels)

  #extract and exponentiate linear predictors
  lp1   <- predict(fit1, newdata = d_int, type = "lp")
  lp2   <- predict(fit2, newdata = d_int, type = "lp")
  e_lp1 <- exp(lp1)
  e_lp2 <- exp(lp2)

  #return centered baseline hazards
  bh1 <- basehaz(fit1, centered = TRUE)
  bh2 <- basehaz(fit2, centered = TRUE)


  event_times <- sort(unique(c(
    bh1$time[bh1$time <= time_horizon],
    bh2$time[bh2$time <= time_horizon]
  )))
  if (length(event_times) == 0L)
    stop("No events observed before time_horizon = ", time_horizon)

  H1_0 <- .get_H0_at(bh1, event_times)
  H2_0 <- .get_H0_at(bh2, event_times)
  K    <- length(event_times)

  H1_mat <- outer(e_lp1, H1_0)
  H2_mat <- outer(e_lp2, H2_0)
  S_mat  <- exp(-H1_mat - H2_mat)

  dH1_0    <- c(H1_0[1L], diff(H1_0))
  S_lag    <- cbind(1.0, S_mat[, -K, drop = FALSE])
  dH1_mat  <- outer(e_lp1, dH1_0)
  cif_incr <- S_lag * dH1_mat

  mean(rowSums(cif_incr))
}


compute_att_cif_dr <- function(fit1, fit2,
                                data_comp,
                                exposure_var    = "first_drug_class",
                                treated_level   = "1",
                                ref_level       = "0",
                                all_levels      = c("0", "1", "2"),
                                time_horizon    = 10) {

  data_treated <- data_comp[data_comp[[exposure_var]] == treated_level, ]

  cif_a   <- .aj_marginal_cif(fit1, fit2, data_treated,
                               exposure_var, treated_level,
                               all_levels, time_horizon)

  cif_ref <- .aj_marginal_cif(fit1, fit2, data_treated,
                               exposure_var, ref_level,
                               all_levels, time_horizon)

  list(
    cif_treated = cif_a,
    cif_ref     = cif_ref,
    att_rd      = cif_a - cif_ref,
    log_att_rr  = log(cif_a) - log(cif_ref)
  )
}


# 4.  CORE: ONE IMPUTED DATASET — POINT ESTIMATES + BOOTSTRAP

iptw_dr_aj_one_dataset <- function(data,
                                    ps_formula      = frm_ps,
                                    formula_e1      = frm_fdrug_ff_e1,
                                    formula_e2      = frm_fdrug_ff_e2,
                                    exposure_var    = "first_drug_class",
                                    treated_levels  = c("1", "2"),
                                    ref_level       = "0",
                                    all_levels      = c("0", "1", "2"),
                                    time_horizon    = 10,
                                    n_boot          = 500,
                                    seed            = 42L,
                                    ph_diagnostics  = FALSE,
                                    covariate_cols  = .fdrug_ps_terms) {

  stopifnot(all(c("time_to_event", "event1", exposure_var) %in% names(data)))

  # Normalise exposure to bare numeric-code characters ("0","1","2").
  # haven_labelled: strip labels then coerce; factor: use underlying integer if
  # levels don't match all_levels (avoids getting label text from as.character).
  .norm_exposure <- function(x, all_levels) {
    if (inherits(x, "haven_labelled")) {
      as.character(as.integer(x))
    } else if (is.factor(x) && !all(levels(x) %in% all_levels)) {
      as.character(as.integer(x) - 1L)  # 1-based factor → 0-based code
    } else {
      as.character(x)
    }
  }
  data[[exposure_var]] <- .norm_exposure(data[[exposure_var]], all_levels)

  obs_levels <- unique(na.omit(data[[exposure_var]]))
  missing_levels <- setdiff(c(ref_level, treated_levels), obs_levels)
  if (length(missing_levels))
    stop("Exposure levels not found after coercion: ",
         paste(missing_levels, collapse = ", "),
         ". Observed values: ", paste(sort(obs_levels), collapse = ", "),
         ". Check storage type of '", exposure_var, "'.")

  # Crude formulas: exposure term only, no covariate adjustment
  frm_e1_crude <- as.formula(
    paste0("Surv(time_to_event, event1 == 1) ~ factor(", exposure_var, ")")
  )
  frm_e2_crude <- as.formula(
    paste0("Surv(time_to_event, event1 == 2) ~ factor(", exposure_var, ")")
  )

  estimate_fn <- function(dat, return_wt = FALSE,
                          mode = c("iptw_dr", "adjusted_unweighted", "crude")) {
    mode <- match.arg(mode)
    dat[[exposure_var]] <- as.character(dat[[exposure_var]])

    if (mode == "iptw_dr") {
      wt_out <- .compute_att_sw(
        data           = dat,
        ps_formula     = ps_formula,
        exposure_var   = exposure_var,
        treated_levels = treated_levels,
        ref_level      = ref_level
      )
    } else {
      wt_out <- list(
        weights = setNames(
          lapply(treated_levels, function(a) rep(1.0, nrow(dat))),
          treated_levels
        ),
        ps_fit = NULL, ps_mat = NULL, ps_check = NULL
      )
    }

    f1 <- if (mode == "crude") frm_e1_crude else formula_e1
    f2 <- if (mode == "crude") frm_e2_crude else formula_e2

    results_per_level <- setNames(vector("list", length(treated_levels)),
                                  treated_levels)

    for (a in treated_levels) {
      w_full   <- wt_out$weights[[a]]
      in_comp  <- dat[[exposure_var]] %in% c(ref_level, a)
      dat_comp <- dat[in_comp, ]
      w_comp   <- w_full[in_comp]

      if (nrow(dat_comp) == 0L)
        stop("No rows match exposure levels '", a, "' or '", ref_level,
             "' — check .norm_exposure() output for '", exposure_var, "'.")

      # Drop the empty exposure level (the third class is absent from this
      # pairwise subsample) so coxph does not emit an aliased NA coefficient —
      # cox.zph()/Schoenfeld residuals reject aliased terms. Estimates are
      # unchanged: the dropped dummy column is all-zero and contributes nothing.
      dat_comp[[exposure_var]] <- droplevels(factor(dat_comp[[exposure_var]], levels = all_levels))
      dat_comp[[".w"]]         <- w_comp

      # Retain design/response matrices only on the iptw_dr point-estimate fits
      # when diagnostics are requested, so cox.zph() and the residual plots can
      # reuse these exact fits. Bootstrap and other modes stay lean (x/y FALSE).
      keep_xy <- isTRUE(return_wt) && mode == "iptw_dr" && isTRUE(ph_diagnostics)

      if (mode != "crude") {
        .diagnose_coxph_data(f1, dat_comp,
                              paste0("event1 comparison=", a, " vs ", ref_level))
        .diagnose_coxph_data(f2, dat_comp,
                              paste0("event2 comparison=", a, " vs ", ref_level))
      }

      fit1 <- do.call(coxph, list(
        formula = f1, data = dat_comp,
        weights = dat_comp[[".w"]], ties = "efron", x = keep_xy, y = keep_xy
      ))
      fit2 <- do.call(coxph, list(
        formula = f2, data = dat_comp,
        weights = dat_comp[[".w"]], ties = "efron", x = keep_xy, y = keep_xy
      ))

      results_per_level[[a]] <- compute_att_cif_dr(
        fit1          = fit1,
        fit2          = fit2,
        data_comp     = dat_comp,
        exposure_var  = exposure_var,
        treated_level = a,
        ref_level     = ref_level,
        all_levels    = all_levels,
        time_horizon  = time_horizon
      )
      results_per_level[[a]]$cox_fit1 <- fit1
      results_per_level[[a]]$cox_fit2 <- fit2

      # Retain the weighted pairwise subsample (incl. .w) only on the iptw_dr
      # point-estimate path when PH diagnostics are requested — cox_assumptions()
      # needs it for the log-log plot grouped by exposure.
      if (keep_xy)
        results_per_level[[a]]$dat_comp <- dat_comp
    }

    if (return_wt) list(estimates = results_per_level, wt_out = wt_out)
    else           results_per_level
  }

  message("    Computing point estimates (IPTW-DR-AJ)...")
  pe_full  <- estimate_fn(data, return_wt = TRUE, mode = "iptw_dr")
  pe_iptw  <- pe_full$estimates
  wt_out   <- pe_full$wt_out

  # Cox PH assumption diagnostics on the iptw_dr point-estimate models (no
  # refit — these are the exact weighted fits used for the point estimates,
  # retained with x/y = TRUE above). Both cause-specific models are checked for
  # each treated comparison: cause 1 = NMS death, cause 2 = other-cause death.
  ph_diag <- NULL
  if (ph_diagnostics) {
    message("    Cox PH assumption diagnostics (iptw_dr)...")
    ph_diag <- setNames(vector("list", length(treated_levels)), treated_levels)
    for (a in treated_levels) {
      dat_d <- pe_iptw[[a]]$dat_comp

      z1 <- cox_assumptions(
        pe_iptw[[a]]$cox_fit1, dat_d, exposure_var = exposure_var,
        label    = paste0("IPTW-DR cause 1 (NMS death) | ", a, " vs ", ref_level),
        time_var = "time_to_event", event_var = "event1", event_code = 1L
      )
      z2 <- cox_assumptions(
        pe_iptw[[a]]$cox_fit2, dat_d, exposure_var = exposure_var,
        label    = paste0("IPTW-DR cause 2 (other death) | ", a, " vs ", ref_level),
        time_var = "time_to_event", event_var = "event1", event_code = 2L
      )
      ph_diag[[a]] <- list(cause1 = z1$ph, cause2 = z2$ph)
    }
  }

  message("    Computing point estimates (adjusted, unweighted)...")
  pe_adj   <- estimate_fn(data, mode = "adjusted_unweighted")

  message("    Computing point estimates (crude)...")
  pe_crude <- estimate_fn(data, mode = "crude")

  point_estimates <- list(
    iptw_dr             = pe_iptw,
    adjusted_unweighted = pe_adj,
    crude               = pe_crude
  )

  plain_cols <- intersect(
    covariate_cols[!grepl(":", covariate_cols)],
    names(data)
  )

  diag <- setNames(vector("list", length(treated_levels)), treated_levels)

  for (a in treated_levels) {
    in_comp <- data[[exposure_var]] %in% c(ref_level, a)
    dat_sub <- data[in_comp, ]
    w_full  <- wt_out$weights[[a]]
    w_sub   <- w_full[in_comp]
    A_sub   <- as.character(dat_sub[[exposure_var]])
    is_trt  <- A_sub == a

    smd_rows <- lapply(plain_cols, function(v) {
      x <- suppressWarnings(as.numeric(dat_sub[[v]]))
      if (all(is.na(x))) return(NULL)
      mu_t  <- mean(x[is_trt],  na.rm = TRUE)
      mu_c  <- mean(x[!is_trt], na.rm = TRUE)
      mu_cw <- weighted.mean(x[!is_trt], w_sub[!is_trt], na.rm = TRUE)
      sd_p  <- sqrt((var(x[is_trt], na.rm = TRUE) + var(x[!is_trt], na.rm = TRUE)) / 2)
      if (is.na(sd_p) || sd_p == 0) return(NULL)
      tibble(
        covariate = v,
        smd_unwt  = (mu_t - mu_c)  / sd_p,
        smd_wtd   = (mu_t - mu_cw) / sd_p
      )
    })

    smd_df <- bind_rows(smd_rows)

    smd_table <- if (nrow(smd_df) > 0) {
      as.data.frame(
        smd_df %>%
          mutate(
            smd_unwt  = round(smd_unwt, 3),
            smd_wtd   = round(smd_wtd,  3),
            flag_unwt = ifelse(abs(smd_unwt) > 0.10, "*", " "),
            flag_wtd  = ifelse(abs(smd_wtd)  > 0.10, "*", " ")
          ) %>%
          select(covariate, smd_unwt, flag_unwt, smd_wtd, flag_wtd)
      )
    } else NULL

    diag[[a]] <- list(
      ps_check     = wt_out$ps_check[[a]],
      ctrl_weights = w_sub[!is_trt],
      ps_overlap   = list(ps = wt_out$ps_mat[in_comp, a], is_trt = is_trt),
      smd_df       = smd_df,
      smd_table    = smd_table
    )
  }

  message("    Bootstrapping (", n_boot, " reps, 3 estimators)...")
  n <- nrow(data)
  set.seed(seed)
  boot_seeds <- sample.int(.Machine$integer.max, n_boot)

  .safe_boot_mode <- function(dat, mode) {
    tryCatch(
      {
        res <- estimate_fn(dat, mode = mode)
        list(
          att_rd     = vapply(treated_levels,
                              function(a) res[[a]]$att_rd,     numeric(1L)),
          log_att_rr = vapply(treated_levels,
                              function(a) res[[a]]$log_att_rr, numeric(1L))
        )
      },
      error = function(e) list(
        att_rd     = setNames(rep(NA_real_, length(treated_levels)), treated_levels),
        log_att_rr = setNames(rep(NA_real_, length(treated_levels)), treated_levels)
      )
    )
  }

  run_one_boot <- function(b) {
    set.seed(boot_seeds[b])
    boot_dat <- data[sample(n, replace = TRUE), ]
    list(
      iptw_dr             = .safe_boot_mode(boot_dat, "iptw_dr"),
      adjusted_unweighted = .safe_boot_mode(boot_dat, "adjusted_unweighted"),
      crude               = .safe_boot_mode(boot_dat, "crude")
    )
  }

  boot_list <- lapply(seq_len(n_boot), function(b) {
    if (b %% 100 == 0) message("      Bootstrap rep ", b, " / ", n_boot)
    run_one_boot(b)
  })

  .extract_boot_var <- function(meth) {
    rd_mat <- do.call(cbind, lapply(boot_list, function(b) b[[meth]]$att_rd))
    rr_mat <- do.call(cbind, lapply(boot_list, function(b) b[[meth]]$log_att_rr))
    rownames(rd_mat) <- rownames(rr_mat) <- treated_levels
    list(
      var_rd     = apply(rd_mat, 1L, var, na.rm = TRUE),
      var_log_rr = apply(rr_mat, 1L, var, na.rm = TRUE),
      n_failed   = sum(is.na(rd_mat[1L, ]))
    )
  }

  bv_iptw  <- .extract_boot_var("iptw_dr")
  bv_adj   <- .extract_boot_var("adjusted_unweighted")
  bv_crude <- .extract_boot_var("crude")

  n_failed <- bv_iptw$n_failed
  if (n_failed > 0L)
    warning(n_failed, " IPTW-DR bootstrap replicates failed and were discarded.")

  list(
    point_estimates = point_estimates,
    boot_var_rd = list(
      iptw_dr             = bv_iptw$var_rd,
      adjusted_unweighted = bv_adj$var_rd,
      crude               = bv_crude$var_rd
    ),
    boot_var_log_rr = list(
      iptw_dr             = bv_iptw$var_log_rr,
      adjusted_unweighted = bv_adj$var_log_rr,
      crude               = bv_crude$var_log_rr
    ),
    n_boot = n_boot - n_failed,
    diag    = diag,
    ph_diag = ph_diag
  )
}


# 5.  RUBIN'S RULES POOLING ACROSS IMPUTATIONS

pool_iptw_att_rubin <- function(results_list,
                                 treated_levels  = c("1", "2"),
                                 ref_level       = "0",
                                 time_horizon    = 10,
                                 exposure_labels = c(
                                   "1" = "Opioid initiated",
                                   "2" = "BZD initiated"
                                 )) {

  m <- length(results_list)

  .rubin <- function(Q_vec, U_vec) {
    Q_bar  <- mean(Q_vec)
    U_bar  <- mean(U_vec)
    B      <- if (m > 1L) var(Q_vec) else 0
    T_var  <- U_bar + (1 + 1/m) * B
    lambda <- if (T_var > 0) ((1 + 1/m) * B) / T_var else 0
    df_br  <- if (lambda > 0) (m - 1) / lambda^2 else Inf
    t_crit <- if (is.finite(df_br) && df_br < 100) qt(0.975, df = df_br) else qnorm(0.975)
    list(est = Q_bar, se = sqrt(T_var), t_crit = t_crit, fmi = lambda, df = df_br)
  }

  methods <- c("iptw_dr", "adjusted_unweighted", "crude")

  map_dfr(methods, function(meth) {
    map_dfr(treated_levels, function(a) {
      rd_vec  <- vapply(results_list, function(r) r$point_estimates[[meth]][[a]]$att_rd,     numeric(1L))
      lrr_vec <- vapply(results_list, function(r) r$point_estimates[[meth]][[a]]$log_att_rr, numeric(1L))
      var_rd  <- vapply(results_list, function(r) r$boot_var_rd[[meth]][[a]],                numeric(1L))
      var_lrr <- vapply(results_list, function(r) r$boot_var_log_rr[[meth]][[a]],            numeric(1L))

      cif_a_vec   <- vapply(results_list, function(r) r$point_estimates[[meth]][[a]]$cif_treated, numeric(1L))
      cif_ref_vec <- vapply(results_list, function(r) r$point_estimates[[meth]][[a]]$cif_ref,     numeric(1L))

      rd_pool <- .rubin(rd_vec,  var_rd)
      rr_pool <- .rubin(lrr_vec, var_lrr)

      tibble(
        method          = meth,
        exposure_level  = a,
        exposure_label  = exposure_labels[a],
        cif_treated     = mean(cif_a_vec),
        cif_ref         = mean(cif_ref_vec),
        att_rd          = rd_pool$est,
        se_att_rd       = rd_pool$se,
        att_rd_lower_95 = rd_pool$est - rd_pool$t_crit * rd_pool$se,
        att_rd_upper_95 = rd_pool$est + rd_pool$t_crit * rd_pool$se,
        att_rr          = exp(rr_pool$est),
        att_rr_lower_95 = exp(rr_pool$est - rr_pool$t_crit * rr_pool$se),
        att_rr_upper_95 = exp(rr_pool$est + rr_pool$t_crit * rr_pool$se),
        fmi_rd          = rd_pool$fmi,
        df_barnard      = rd_pool$df,
        m_imputations   = m,
        time_horizon    = time_horizon
      )
    })
  })
}


# 6.  MAIN WRAPPER

run_iptw_att_analysis <- function(imputed_list,
                                   ps_formula      = frm_ps,
                                   formula_e1      = frm_fdrug_ff_e1,
                                   formula_e2      = frm_fdrug_ff_e2,
                                   exposure_var    = "first_drug_class",
                                   treated_levels  = c("1", "2"),
                                   ref_level       = "0",
                                   all_levels      = c("0", "1", "2"),
                                   time_horizon    = 10,
                                   n_boot          = 500,
                                   seed            = 2024L,
                                   ph_diagnostics  = TRUE,
                                   covariate_cols  = .fdrug_ps_terms) {

  imputed_list <- .as_data_list(imputed_list)
  m            <- length(imputed_list)

  message(
    "\n=== IPTW-DR-AJ (ATT) ANALYSIS ===",
    "\n  Exposure     : ", exposure_var,
    " (0=No-Rx [ref], 1=Opioid, 2=BZD)",
    "\n  Estimand     : ATT — effect among initiators",
    "\n  Comparisons  : ",
    paste(treated_levels, "vs", ref_level, collapse = "; "),
    "\n  Method       : ATT-IPTW (multinomial PS, propensity odds weights) +",
    " doubly-robust cause-specific Cox + AJ CIF",
    "\n  Outcome      : event=1 (NMS death) | Competing: event=2",
    "\n  Horizon      : ", time_horizon, " years",
    "\n  Bootstrap    : ", n_boot, " reps per dataset",
    "\n  Datasets     : ", m, " imputed\n"
  )

  results_list <- vector("list", m)

  for (i in seq_len(m)) {
    message("── Imputation ", i, " / ", m,
            " ──────────────────────────────────────")
    results_list[[i]] <- iptw_dr_aj_one_dataset(
      data           = imputed_list[[i]],
      ps_formula     = ps_formula,
      formula_e1     = formula_e1,
      formula_e2     = formula_e2,
      exposure_var   = exposure_var,
      treated_levels = treated_levels,
      ref_level      = ref_level,
      all_levels     = all_levels,
      time_horizon   = time_horizon,
      n_boot         = n_boot,
      seed           = seed + i,
      ph_diagnostics = ph_diagnostics && (i == 1L),  # first imputation only
      covariate_cols = covariate_cols
    )
  }

  message("\nPooling across imputations (Rubin's rules)...")
  pooled <- pool_iptw_att_rubin(
    results_list   = results_list,
    treated_levels = treated_levels,
    ref_level      = ref_level,
    time_horizon   = time_horizon
  )

  message("Done.\n")
  list(
    pooled             = pooled,
    imputation_results = results_list,
    imputation_diag    = lapply(results_list, `[[`, "diag"),
    ph_diag            = results_list[[1L]]$ph_diag   # cox.zph tables (imp. 1)
  )
}


