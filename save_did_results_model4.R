# CLEAN ENVIRONMENT
rm(list = ls())
gc()
cat("\014")

# LOAD LIBRARIES
library(tidyverse)
library(fixest)
library(broom)
library(fs)

# SET WORKING DIRECTORY
setwd("/Users/api970/Desktop/Project_InnovationBox/staggered_diff_in_diff_trials")

# CREATE OUTPUT FOLDER FOR PLOTS
dir_plot <- "figures/model_4_sunab"
dir_create(dir_plot)

# READ DATA
df <- read_csv("simulated_panel.csv")

# Define outcomes
outcomes <- c("y1", "y2", "y3")

# Storage for tidy results
model4_list <- list()

for (yvar in outcomes) {
  
  df_var <- df %>% filter(!is.na(.data[[yvar]]))
  
  df_var$first_treat <- ifelse(df_var$treated == 0, Inf, df_var$first_treat)
  
  # Estimate Sun & Abraham model
  formula_sunab <- as.formula(paste0(yvar, " ~ sunab(first_treat, year) | firm_id + year"))
  did_sunab <- feols(formula_sunab, cluster = ~firm_id, data = df_var)
  
  # Extract tidy coefficients for event times
  tidy_sunab <- tidy(did_sunab, conf.int = TRUE) %>%
    mutate(
      outcome     = yvar,
      event_time  = as.integer(str_extract(term, "(?<=::)-?\\d+"))
    ) %>%
    select(outcome, term, event_time, estimate, std_error = std.error, t_value = statistic, p_value = p.value, conf.low, conf.high)
  
  model4_list[[yvar]] <- tidy_sunab
  
  # Save plot
  png(filename = file.path("figures/model_4_sunab", paste0("sunab_plot_", yvar, ".png")),
      width = 800, height = 600)
  iplot(did_sunab, ref.line = 0, main = paste("Sun & Abraham: Dynamic Effects –", yvar))
  dev.off()
  
}

# Combine and export
model4_df <- bind_rows(model4_list)
write_csv(model4_df, "model_4_sunab_effects.csv")
