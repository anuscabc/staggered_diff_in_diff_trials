# ===================================================
# CLEAN ENVIRONMENT
# ===================================================
rm(list = ls())
gc()
if (interactive()) cat("\014")  # Clear console only in interactive use

# ===================================================
# LOAD LIBRARIES (install on‑the‑fly if needed)
# ===================================================
required_pkgs <- c("tidyverse", "data.table", "fixest", "did", "sandwich", "lmtest", "here", "fs", "broom")
lapply(required_pkgs, function(p) {
  if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
  library(p, character.only = TRUE)
})

# ===================================================
# PATHS (portable)
# ===================================================
root_dir   <- here::here()               # project root (where this script sits)
output_tbl <- fs::path(root_dir, "tables")
output_fig <- fs::path(root_dir, "figures")
fs::dir_create(c(output_tbl, output_fig))

# ===================================================
# READ DATA
# ===================================================
df <- read_csv(fs::path(root_dir, "simulated_panel.csv"))

# ===================================================
# PARAMETERS
# ===================================================
outcome_vars <- c("y1", "y2", "y3")   # add/remove outcomes as desired

# ===================================================
# LOOP THROUGH OUTCOMES
# ===================================================
for (yvar in outcome_vars) {
  
  # -------------------------------------------------
  # 1. Prepare analysis data
  # -------------------------------------------------
  df_var <- df %>%
    filter(!is.na(.data[[yvar]])) %>%
    mutate(
      post  = as.integer(year >= 2010),
      did   = treated * post,
      first_treat = ifelse(treated == 0, NA, first_treat),  # NA for never‑treated
      event_time  = ifelse(treated == 1, year - first_treat, NA_real_)
    )
  
  # -------------------------------------------------
  # 2. Model 1 – Simple DiD (robust HC1 SEs)
  # -------------------------------------------------
  formula_lm <- reformulate(termlabels = c("treated", "post", "did"), response = yvar)
  did_lm     <- lm(formula_lm, data = df_var)
  robust_vcv <- sandwich::vcovHC(did_lm, type = "HC1")
  model1_df  <- broom::tidy(did_lm, conf.int = FALSE) %>%
    mutate(
      std_error = sqrt(diag(robust_vcv)),
      t_value   = estimate / std_error,
      p_value   = 2 * pt(abs(t_value), df = did_lm$df.residual, lower.tail = FALSE),
      model     = "Model_1_SimpleDiD"
    )
  
  model1_stats <- tibble(
    model     = "Model_1_SimpleDiD",
    term      = c("r_squared", "n_obs"),
    estimate  = c(summary(did_lm)$r.squared, nobs(did_lm)),
    std_error = NA_real_,
    t_value   = NA_real_,
    p_value   = NA_real_
  )
  
  # -------------------------------------------------
  # 3. Model 2 – Two‑way FE Event Study
  # -------------------------------------------------
  formula_fe <- as.formula(sprintf("%s ~ i(event_time, treated, ref = -1) | firm_id + year", yvar))
  did_event  <- feols(formula_fe, cluster = ~firm_id, data = df_var)
  
  coef_df <- broom::tidy(did_event, conf.int = FALSE) %>%
    mutate(model = "Model_2_FE_EventStudy") %>%
    select(model, term, estimate, std_error = std.error, t_value = statistic, p_value = p.value)
  
  model2_stats <- tibble(
    model     = "Model_2_FE_EventStudy",
    term      = c("r_squared", "n_obs"),
    estimate  = c(r2(did_event), nobs(did_event)),
    std_error = NA_real_,
    t_value   = NA_real_,
    p_value   = NA_real_
  )
  
  es_plot <- iplot(did_event, ref.line = -1, main = sprintf("Event Study – %s", yvar))
  ggplot2::ggsave(filename = fs::path(output_fig, sprintf("event_study_%s.png", yvar)),
                  plot = es_plot, width = 8, height = 6, dpi = 300)
  
  # -------------------------------------------------
  # 4. Model 3 – Callaway & Sant'Anna staggered DiD
  # -------------------------------------------------
  did_cs <- did::att_gt(
    yname      = yvar,
    tname      = "year",
    idname     = "firm_id",
    gname      = "first_treat",
    xformla    = ~ 1,
    data       = df_var,
    est_method = "dr"
  )
  
  agg_cs <- did::aggte(did_cs, type = "group")
  
  cs_df <- tibble(
    model     = "Model_3_CallawaySantanna",
    term      = paste0("group_effect_", agg_cs$egt),
    estimate  = agg_cs$att.egt,
    std_error = agg_cs$se.egt,
    t_value   = estimate / std_error,
    p_value   = 2 * pnorm(abs(t_value), lower.tail = FALSE)
  )
  
  model3_stats <- tibble(
    model     = "Model_3_CallawaySantanna",
    term      = c("overall_att", "n_obs"),
    estimate  = c(agg_cs$overall.att, did_cs$N),
    std_error = c(agg_cs$overall.se, NA_real_),
    t_value   = c(agg_cs$overall.att / agg_cs$overall.se, NA_real_),
    p_value   = c(2 * pnorm(abs(agg_cs$overall.att / agg_cs$overall.se), lower.tail = FALSE), NA_real_)
  )
  
  cs_plot <- ggdid(did_cs)
  ggplot2::ggsave(filename = fs::path(output_fig, sprintf("cs_group_%s.png", yvar)),
                  plot = cs_plot, width = 8, height = 6, dpi = 300)
  
  # -------------------------------------------------
  # 5. Model 4 – Sun & Abraham (2021)
  # -------------------------------------------------
  formula_sunab <- as.formula(sprintf("%s ~ sunab(first_treat, year) | firm_id + year", yvar))
  did_sunab     <- feols(formula_sunab, cluster = ~firm_id, data = df_var)
  
  sunab_df <- broom::tidy(did_sunab, conf.int = FALSE) %>%
    mutate(model = "Model_4_SunAbraham") %>%
    select(model, term, estimate, std_error = std.error, t_value = statistic, p_value = p.value)
  
  sunab_stats <- tibble(
    model     = "Model_4_SunAbraham",
    term      = c("r_squared", "n_obs"),
    estimate  = c(r2(did_sunab), nobs(did_sunab)),
    std_error = NA_real_,
    t_value   = NA_real_,
    p_value   = NA_real_
  )
  
  sa_plot <- iplot(did_sunab, ref.line = 0, main = sprintf("Sun & Abraham – %s", yvar))
  ggplot2::ggsave(filename = fs::path(output_fig, sprintf("sunab_%s.png", yvar)),
                  plot = sa_plot, width = 8, height = 6, dpi = 300)
  
  # -------------------------------------------------
  # 6. Combine & export results
  # -------------------------------------------------
  final_df <- bind_rows(
    model1_df, model1_stats,
    coef_df,   model2_stats,
    cs_df,     model3_stats,
    sunab_df,  sunab_stats
  )
  
  write_csv(final_df, fs::path(output_tbl, sprintf("did_results_%s.csv", yvar)))
  
  message(sprintf("Finished %s – %d rows saved.", yvar, nrow(final_df)))
}
}