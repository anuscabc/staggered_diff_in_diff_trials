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
setwd("/Users/api970/Desktop/Project_InnovationBox/staggered_diff_in_diff_trials")

# ===================================================
# READ AND PREP DATA
# ===================================================
df <- read_csv("simulated_panel.csv")

# Remove missing outcome values
df <- df %>%
  filter(!is.na(y1))

# Create post-treatment and DiD interaction term
df <- df %>%
  mutate(
    post = ifelse(year >= 2010, 1, 0),
    did  = treated * post
  )

# Ensure first_treat is 0 for never-treated firms
df <- df %>%
  mutate(first_treat = ifelse(treated == 0, 0, first_treat))

# ===================================================
# MODEL 1: Simple DiD (no fixed effects)
# ===================================================

# This is baseline model not suitable for even study
did_lm <- lm(y1 ~ treated + post + did, data = df)
summary(did_lm)

# ===================================================
# MODEL 2: DiD with Firm and Year Fixed Effects
# ===================================================

df <- df %>%
  mutate(
    event_time = ifelse(treated == 1, year - first_treat, NA)
  )

did_event <- feols(
  y3 ~ i(event_time, treated, ref = -1) | firm_id + year,
  cluster = "firm_id",
  data = df
)

iplot(did_event, ref.line = -1, main = "Event Study Plot (relative to treatment)")

# ===================================================
# MODEL 3: Staggered DiD (Callaway & Sant’Anna, no covariates)
# ===================================================

did_staggered1 <- att_gt(
  yname = "y1",
  tname = "year",
  idname = "firm_id",
  gname = "first_treat",
  xformla = ~1,
  data = df,
  est_method = "dr"
)

agg_effect1 <- aggte(did_staggered1, type = "group")
summary(agg_effect1)
ggdid(did_staggered1)

# ===================================================
# MODEL 4: Sun & Abraham (2021) Specification
# ===================================================

did_sunab <- feols(
  y1 ~ sunab(first_treat, year) | firm_id + year,
  cluster = "firm_id",        # Cluster SEs at firm level
  data = df
)

# View results
summary(did_sunab)

# Plot dynamic treatment effects (optional)
iplot(did_sunab, ref.line = 0, main = "Sun & Abraham: Dynamic Effects")
