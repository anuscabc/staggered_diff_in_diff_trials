# ===================================================
# CLEAN ENVIRONMENT
# ===================================================
rm(list = ls())           # Clear environment
gc()                      # Garbage collection
cat("\014")               # Clear console

# ===================================================
# LOAD LIBRARIES
# ===================================================
library(tidyverse)        
library(data.table)       
library(did)              
library(fixest)      

# ===================================================
# SET WORKING DIRECTORY
# ===================================================
setwd("..")

# ===================================================
# READ AND PREP DATA
# ===================================================
df <- read_csv("simulated_panel.csv")


outcomes <-c('y1', 'y2', 'y3')

# For each of the outcomes prep all needed estimation data


# Model 1 
# Initialize storage list
model1_results <- list()
for (yvar in outcomes) {
  df_var <- df %>%
    filter(!is.na(.data[[yvar]])) %>%
    mutate(
      post = as.integer(year >= 2010),
      did  = treated * post
    )
  
  # Run model
  formula_m1 <- as.formula(paste(yvar, "~ treated + post + did"))
  did_lm     <- lm(formula_m1, data = df_var)
  robust_vcv <- vcovHC(did_lm, type = "HC1")
  se_vals    <- sqrt(diag(robust_vcv))
  
  # Extract robust p-values
  coefs <- coef(did_lm)
  p_vals <- 2 * pt(abs(coefs / se_vals), df = did_lm$df.residual, lower.tail = FALSE)
  
  # Extract and store values
  did_coef     <- coefs["did"]
  se_did       <- se_vals["did"]
  p_did        <- p_vals["did"]
  
  post_coef    <- coefs["post"]
  se_post      <- se_vals["post"]
  p_post       <- p_vals["post"]
  
  treated_coef <- coefs["treated"]
  se_treated   <- se_vals["treated"]
  p_treated    <- p_vals["treated"]
  
  r_sq  <- summary(did_lm)$r.squared
  n_obs <- nobs(did_lm)
  
  model1_results[[yvar]] <- tibble(
    outcome              = yvar,
    did_coefficient      = did_coef,
    se_did               = se_did,
    p_did                = p_did,
    post_coefficient     = post_coef,
    se_post              = se_post,
    p_post               = p_post,
    treated_coefficient  = treated_coef,
    se_treated           = se_treated,
    p_treated            = p_treated,
    r_stat               = r_sq,
    n_obs                = n_obs
  )
}

# Combine and save
model1_df <- bind_rows(model1_results)
write_csv(model1_df, "model_1_outcomes.csv")
  
