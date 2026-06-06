# 7.  WEIGHT DIAGNOSTICS
#
# Accepts the pre-computed diagnostic bundle from run_iptw_att_analysis()$imputation_diag[[i]].
# No raw data or formulas needed — all inputs are produced by iptw_dr_aj_one_dataset().
#
# Per comparison (treated level a vs reference 0) produces:
#   - Console: n, ESS, weight summary, SMD table (|SMD| > 0.10 flagged)
#   - Plot 1 (PS overlap):    density of P(A=a|L) split by group
#   - Plot 2 (Love plot):     |SMD| per covariate, unweighted vs IPTW weighted
#   - Plot 3 (Weight violin): stabilised ATT weight distribution, reference group
#
# Returns invisibly: named list of plots per treated level ($overlap, $love, $violin).
# Usage: diagnose_ps_weights(result$imputation_diag[[1]])
library(ggplot2)
diagnose_ps_weights <- function(diag_out,
                                 exposure_labels = c(
                                   "1" = "Opioid initiated",
                                   "2" = "BZD initiated"
                                 )) {

  all_plots <- setNames(vector("list", length(diag_out)), names(diag_out))

  for (a in names(diag_out)) {
    diag_a  <- diag_out[[a]]
    lbl     <- exposure_labels[a]
    chk     <- diag_a$ps_check
    is_trt  <- diag_a$ps_overlap$is_trt
    ps_a    <- diag_a$ps_overlap$ps
    ctrl_w  <- diag_a$ctrl_weights
    smd_df  <- diag_a$smd_df

    # console output
    cat("\n", strrep("─", 60), "\n",
        "Weight diagnostics: ", lbl, " vs No prescription\n",
        strrep("─", 60), "\n", sep = "")
    cat("  n treated            :", chk$n_treated, "\n")
    cat("  n reference          :", chk$n_control, "\n")
    cat("  Control ESS (post-trim):", round(chk$ctrl_ESS, 1), "\n")
    cat("  Weight trim cutpoint (", chk$trim_quantile * 100, "th pct):",
        round(chk$trim_cutpoint, 3), "\n")
    cat("  Control weight summary (post-trim):\n")
    print(chk$ctrl_weight_summary)

    if (!is.null(diag_a$smd_table)) {
      cat("\n  Standardised Mean Differences (|SMD| > 0.10 flagged with *):\n")
      print(diag_a$smd_table, row.names = FALSE)
    }

    # Propensity score overlap
    # Density of P(A=a|L) for each group. Regions where the reference density
    # approaches zero flag positivity violations (extreme extrapolation).
    overlap_df <- tibble(
      ps    = ps_a,
      group = ifelse(is_trt, lbl, "No prescription (ref)")
    )

    p_overlap <- ggplot(overlap_df, aes(x = ps, fill = group, colour = group)) +
      geom_density(alpha = 0.35, linewidth = 0.7) +
      geom_rug(alpha = 0.25, linewidth = 0.35, sides = "b",
               length = unit(0.03, "npc")) +
      scale_fill_manual(values   = c("steelblue4", "firebrick4")) +
      scale_colour_manual(values = c("steelblue4", "firebrick4")) +
      labs(
        title    = paste("Propensity Score Overlap —", lbl),
        subtitle = paste0("P(A = ", a, " | L)  |  n = ",
                          chk$n_treated, " treated, ",
                          chk$n_control, " reference"),
        x      = paste0("P(A = ", a, " | L)"),
        y      = "Density",
        fill   = NULL,
        colour = NULL
      ) +
      theme_minimal(base_size = 11) +
      theme(
        legend.position  = "top",
        panel.grid.minor = element_blank(),
        plot.title       = element_text(face = "bold"),
        plot.subtitle    = element_text(colour = "grey40")
      )
    print(p_overlap)

    # Love plot
    # Absolute SMD per covariate before and after IPTW. Points above the 0.10
    # threshold after weighting indicate residual imbalance.
    p_love <- NULL

    if (nrow(smd_df) > 0) {
      love_df <- bind_rows(
        smd_df %>% mutate(estimator = "Unweighted",    abs_smd = abs(smd_unwt)),
        smd_df %>% mutate(estimator = "IPTW weighted", abs_smd = abs(smd_wtd))
      ) %>%
        mutate(
          estimator = factor(estimator,
                             levels = c("Unweighted", "IPTW weighted")),
          covariate = factor(covariate,
                             levels = smd_df$covariate[order(abs(smd_df$smd_unwt))])
        )

      p_love <- ggplot(love_df,
                       aes(x = abs_smd, y = covariate,
                           colour = estimator, shape = estimator)) +
        geom_vline(xintercept = 0.10, linetype = "dashed",
                   colour = "grey40", linewidth = 0.6) +
        geom_line(aes(group = covariate), colour = "grey75", linewidth = 0.5) +
        geom_point(size = 3) +
        scale_colour_manual(values = c("Unweighted"    = "grey50",
                                       "IPTW weighted" = "firebrick4")) +
        scale_shape_manual(values  = c("Unweighted"    = 1,
                                       "IPTW weighted" = 16)) +
        scale_x_continuous(
          limits = c(0, max(max(love_df$abs_smd, na.rm = TRUE) * 1.05, 0.15)),
          expand = c(0, 0)
        ) +
        annotate("text", x = 0.10, y = -Inf,
                 label = " 0.10", hjust = -0.1, vjust = -0.5,
                 size = 3, colour = "grey40") +
        labs(
          title    = paste("Love Plot —", lbl),
          subtitle = paste0("Max |SMD| after IPTW = ",
                            round(max(abs(smd_df$smd_wtd), na.rm = TRUE), 3)),
          x      = "|Standardised Mean Difference|",
          y      = NULL,
          colour = NULL,
          shape  = NULL
        ) +
        theme_minimal(base_size = 11) +
        theme(
          legend.position    = "top",
          panel.grid.minor   = element_blank(),
          panel.grid.major.y = element_blank(),
          plot.title         = element_text(face = "bold"),
          plot.subtitle      = element_text(colour = "grey40")
        )
      print(p_love)
    }

    #VIolin plots for weight distribution
    # ATT treated weights are identically 1 by construction; violin shows the
    # reference group only. Heavy right tails flag high-leverage observations.
    violin_df <- tibble(weight = ctrl_w)

    p_violin <- ggplot(violin_df, aes(x = 1, y = weight)) +
      geom_violin(fill = "steelblue4", colour = "grey30",
                  alpha = 0.45, trim = FALSE, linewidth = 0.5) +
      geom_boxplot(width = 0.08, outlier.shape = NA,
                   colour = "grey20", fill = "white") +
      geom_hline(yintercept = 1, linetype = "dashed",
                 colour = "firebrick4", linewidth = 0.7) +
      annotate("text", x = 1.35, y = 1,
               label = "w = 1 (treated)", vjust = -0.4,
               size = 3, colour = "firebrick4") +
      scale_x_continuous(breaks = NULL) +
      labs(
        title    = paste("Weight Distribution (Reference Group) —", lbl),
        subtitle = paste0(
          "n = ", chk$n_control,
          "  |  median = ", round(median(ctrl_w), 2),
          "  |  max = ",    round(max(ctrl_w),    2),
          "  |  ESS = ",    round(chk$ctrl_ESS,   0)
        ),
        x = NULL,
        y = "Stabilised ATT weight"
      ) +
      theme_minimal(base_size = 11) +
      theme(
        panel.grid.major.x = element_blank(),
        panel.grid.minor   = element_blank(),
        plot.title         = element_text(face = "bold"),
        plot.subtitle      = element_text(colour = "grey40")
      )
    print(p_violin)

    all_plots[[a]] <- list(overlap = p_overlap, love = p_love, violin = p_violin)
  }

  invisible(all_plots)
}


# 8.  RESULTS: TABLE AND FOREST PLOT

print_iptw_att_table <- function(pooled) {
  cat(
    "\n=== IPTW-DR-AJ RESULTS (ATT) ===",
    "\nExposure  : first_drug_class (0=No prescription [ref])",
    "\nOutcome   : NMS Death (event=1) at", pooled$time_horizon[1], "years",
    "\nEstimand  : ATT — counterfactual no-prescription among initiators",
    "\nMethod    : Stabilised IPTW + doubly-robust cause-specific Cox + AJ CIF\n\n"
  )

  out <- pooled %>%
    mutate(
      `CIF initiators (%)`  = sprintf("%.3f%%", cif_treated * 100),
      `CIF no-Rx, cfl (%)`  = sprintf("%.3f%%", cif_ref     * 100),
      `ATT RD (95% CI)`     = sprintf("%+.4f (%+.4f, %+.4f)",
                                       att_rd, att_rd_lower_95, att_rd_upper_95),
      `ATT RR (95% CI)`     = sprintf("%.2f (%.2f, %.2f)",
                                       att_rr, att_rr_lower_95, att_rr_upper_95),
      FMI                   = sprintf("%.3f", fmi_rd)
    ) %>%
    select(
      Exposure             = exposure_label,
      `CIF initiators (%)`,
      `CIF no-Rx, cfl (%)`,
      `ATT RD (95% CI)`,
      `ATT RR (95% CI)`,
      FMI
    )

  print(as.data.frame(out), row.names = FALSE)
  invisible(pooled)
}


plot_iptw_att_forest <- function(pooled, scale = c("rr", "rd")) {
  scale <- match.arg(scale)

  if (scale == "rr") {
    p <- ggplot(pooled, aes(x = att_rr, y = exposure_label)) +
      geom_vline(xintercept = 1, linetype = "dashed",
                 colour = "grey50", linewidth = 0.6) +
      geom_errorbarh(
        aes(xmin = att_rr_lower_95, xmax = att_rr_upper_95),
        height = 0.15, colour = "firebrick4", linewidth = 0.8
      ) +
      geom_point(colour = "firebrick4", size = 3.5) +
      scale_x_log10(
        breaks = c(0.5, 1, 2, 5, 10),
        labels = c("0.5", "1", "2", "5", "10")
      ) +
      labs(
        title    = "ATT Risk Ratios: Prescription Initiation and NMS Death",
        subtitle = paste0(
          "IPTW-DR-AJ | Ref: No prescription (counterfactual) | ",
          round(pooled$time_horizon[1]), "-year horizon"
        ),
        x       = "ATT Risk Ratio (log scale, 95% CI)",
        y       = NULL,
        caption = paste0(
          "Stabilised IPTW (multinomial PS) + doubly-robust cause-specific Cox. ",
          "Rubin's rules, m=", pooled$m_imputations[1], " imputations."
        )
      )
  } else {
    p <- ggplot(pooled, aes(x = att_rd * 100, y = exposure_label)) +
      geom_vline(xintercept = 0, linetype = "dashed",
                 colour = "grey50", linewidth = 0.6) +
      geom_errorbarh(
        aes(xmin = att_rd_lower_95 * 100, xmax = att_rd_upper_95 * 100),
        height = 0.15, colour = "firebrick4", linewidth = 0.8
      ) +
      geom_point(colour = "firebrick4", size = 3.5) +
      labs(
        title    = "ATT Risk Differences: Prescription Initiation and NMS Death",
        subtitle = paste0(
          "IPTW-DR-AJ | Ref: No prescription (counterfactual) | ",
          round(pooled$time_horizon[1]), "-year horizon"
        ),
        x       = "ATT Risk Difference (percentage points, 95% CI)",
        y       = NULL,
        caption = paste0(
          "Stabilised IPTW (multinomial PS) + doubly-robust cause-specific Cox. ",
          "Rubin's rules, m=", pooled$m_imputations[1], " imputations."
        )
      )
  }

  p +
    theme_minimal(base_size = 12) +
    theme(
      plot.title         = element_text(face = "bold"),
      plot.subtitle      = element_text(colour = "grey40"),
      axis.text.y        = element_text(size = 11),
      panel.grid.minor   = element_blank(),
      panel.grid.major.y = element_blank()
    )
}
