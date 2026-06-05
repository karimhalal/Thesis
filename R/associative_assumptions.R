#the following function generates visual tests of associative model
#for cause specific
cox_assumptions <- function(fit, data, exposure_var = NULL, label = "Model") {

  # 1. Schoenfeld residuals — global and per-term PH test
  ph <- cox.zph(fit, transform = "km")
  cat("\n── Schoenfeld PH Test —", label, "──\n")
  print(ph)
  plot(ph)

  # 2. Log-log plot by exposure group
  if (!is.null(exposure_var) && exposure_var %in% names(data)) {
    grp       <- factor(data[[exposure_var]])
    tmp       <- data
    tmp$.grp  <- grp
    km_fit    <- survfit(Surv(survt, event == 1) ~ .grp, data = tmp)
    km_s      <- summary(km_fit)
    km_df     <- tibble(
      time   = km_s$time,
      surv   = km_s$surv,
      lower  = km_s$lower,
      upper  = km_s$upper,
      strata = sub("^\\.grp=", "", km_s$strata)
    ) %>%
      filter(surv > 0, surv < 1, lower > 0, upper > 0, upper < 1) %>%
      mutate(
        lls       = log(-log(surv)),
        lls_lower = log(-log(upper)),   # log-log inverts CI order
        lls_upper = log(-log(lower)),
        lt        = log(time)
      )

    print(
      ggplot(km_df, aes(x = lt, colour = strata, fill = strata)) +
        geom_ribbon(aes(ymin = lls_lower, ymax = lls_upper),
                    alpha = 0.12, colour = NA) +
        geom_step(aes(y = lls)) +
        labs(title   = paste("Log-Log Plot —", label),
             x       = "log(Time)",
             y       = "log(-log(S(t)))",
             colour  = exposure_var,
             fill    = exposure_var) +
        theme_minimal(base_size = 11) +
        theme(panel.grid.minor = element_blank())
    )
  }
  # 3. Martingale residuals vs linear predictor
  mart <- residuals(fit, type = "martingale")
  lp   <- predict(fit, type = "lp")
  print(
    ggplot(tibble(lp = lp, mart = mart), aes(x = lp, y = mart)) +
      geom_point(alpha = 0.15, size = 0.6) +
      geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"),
                  se = TRUE, colour = "steelblue3", linewidth = 0.8) +
      geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
      labs(title = paste("Martingale Residuals —", label),
           x     = "Linear Predictor",
           y     = "Martingale Residual") +
      theme_minimal(base_size = 11)
  )

  # 4. dfbeta plots — first min(4, p) terms (exposure term(s) first)
  dfb <- residuals(fit, type = "dfbeta")
  for (k in seq_len(min(4L, ncol(dfb)))) {
    print(
      ggplot(tibble(idx = seq_len(nrow(dfb)), d = dfb[, k]),
             aes(x = idx, y = d)) +
        geom_point(alpha = 0.2, size = 0.5) +
        geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
        labs(title = paste0("dfbeta — ", label, "  |  ", colnames(dfb)[k]),
             x     = "Subject Index",
             y     = "dfbeta") +
        theme_minimal(base_size = 11)
    )
  }

  invisible(list(ph = ph))
}