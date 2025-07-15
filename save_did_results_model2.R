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
library(fixest)
library(broom)
library(fs)

# ===================================================
# SET WORKING DIRECTORY
# ===================================================
setwd("/Users/api970/Desktop/Project_InnovationBox/staggered_diff_in_diff_trials")

# ===================================================
# CREATE OUTPUT FOLDER FOR PLOTS
# ===================================================
dir_plot <- "figures/model_2"
dir_create(dir_plot)

# ===================================================
# READ DATA
# ===================================================
df <- read_csv("simulated_panel.csv")

# ===================================================
# MODEL 2: FE Event Study
# ===================================================
outcomes <- c("y1", "y2", "y3")
model2_list <- list()

for (yvar in outcomes) {
  
  df_var <- df %>%
    filter(!is.na(.data[[yvar]])) %>%
    mutate(event_time = ifelse(treated == 1, year - first_treat, NA))
  
  # Estimate model
  formula_event <- as.formula(paste0(yvar, " ~ i(event_time, treated, ref = -1) | firm_id + year"))
  did_event <- feols(formula_event, cluster = ~firm_id, data = df_var)
  
  # Tidy output
  tidy_event <- tidy(did_event, conf.int = TRUE) %>%
    mutate(
      outcome     = yvar,
      event_time  = as.integer(str_extract(term, "-?\\d+"))
    ) %>%
    select(outcome, term, event_time, estimate,
           std_error = std.error, t_value = statistic, p_value = p.value,
           conf.low, conf.high)
  
  model2_list[[yvar]] <- tidy_event
  
  # Save base R plot
  png(filename = file.path(dir_plot, paste0("event_study_", yvar, ".png")),
      width = 800, height = 600)
  print(iplot(did_event, ref.line = -1, main = paste("Event Study –", yvar)))
  dev.off()
}

# ===================================================
# EXPORT RESULTS
# ===================================================
model2_df <- bind_rows(model2_list)
write_csv(model2_df, "model_2_event_study_results.csv")
