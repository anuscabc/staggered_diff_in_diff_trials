# ===================================================
# CLEAN ENVIRONMENT
# ===================================================
rm(list = ls())
gc()
cat("\014")

# ===================================================
# LOAD LIBRARIES
# ===================================================
library(tidyverse)
library(data.table)
library(did)
library(fixest)
library(fs)

# ===================================================
# SET WORKING DIRECTORY
# ===================================================
setwd("/Users/api970/Desktop/Project_InnovationBox/staggered_diff_in_diff_trials")

# ===================================================
# CREATE OUTPUT FOLDER FOR PLOTS
# ===================================================
dir_plot <- "figures/model_3"
dir_create(dir_plot)

# ===================================================
# READ DATA
# ===================================================
df <- read_csv("simulated_panel.csv")

# ===================================================
# DEFINE OUTCOMES
# ===================================================
outcomes <- c("y1", "y2", "y3")

# ===================================================
# STORAGE
# ===================================================
group_time_list <- list()
overall_list    <- list()

# ===================================================
# LOOP OVER OUTCOMES
# ===================================================
for (yvar in outcomes) {
  df_var <- df %>%
    filter(!is.na(.data[[yvar]]))
  
  # Ensure never-treated units have Inf
  df_var$first_treat <- ifelse(df_var$treated == 0, Inf, df_var$first_treat)
  
  # Estimate ATT(g,t)
  did_cs <- att_gt(
    yname = yvar,
    tname = "year",
    idname = "firm_id",
    gname = "first_treat",
    xformla = ~1,
    data = df_var,
    est_method = "dr"
  )
  
  # Save plot
  cs_plot <- ggdid(did_cs)
  ggsave(
    filename = file.path(dir_plot, paste0("ggdid_group_plot_", yvar, ".png")),
    plot = cs_plot,
    width = 8,
    height = 20,
    dpi = 300
  )
  
  # Group-time effects
  group_df <- tibble(
    outcome     = yvar,
    group       = did_cs$group,
    time        = did_cs$t,
    event_time  = did_cs$t - did_cs$group,
    estimate    = did_cs$att,
    std_error   = did_cs$se,
    t_value     = estimate / std_error,
    p_value     = 2 * pnorm(abs(t_value), lower.tail = FALSE)
  )
  
  # Aggregate ATT
  agg_cs <- aggte(did_cs, type = "group")
  overall_df <- tibble(
    outcome     = yvar,
    estimate    = agg_cs$overall.att,
    std_error   = agg_cs$overall.se,
    t_value     = agg_cs$overall.att / agg_cs$overall.se,
    p_value     = 2 * pnorm(abs(t_value), lower.tail = FALSE)
  )
  
  # Store results
  group_time_list[[yvar]] <- group_df
  overall_list[[yvar]]    <- overall_df
}

# ===================================================
# EXPORT RESULTS
# ===================================================
group_time_df <- bind_rows(group_time_list)
overall_df    <- bind_rows(overall_list)

write_csv(group_time_df, "model_3_group_time_effects.csv")
write_csv(overall_df,    "model_3_overall_effects.csv")