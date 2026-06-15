# Clean start
rm(list = ls(all.names = TRUE)); cat("\014"); graphics.off()

# Packages
pkgs <- c("readr","dplyr","tidyr","did","fixest","ggplot2","purrr","stringr","tibble")
to_install <- setdiff(pkgs, rownames(installed.packages()))
if(length(to_install)) install.packages(to_install, repos = "https://cloud.r-project.org")
invisible(lapply(pkgs, library, character.only = TRUE))

# ----------------------------- USER INPUTS --------------------------------
outcomes <- c("base_income")

# Output folder (single place, no per-outcome subfolders)
out_root <- "outputs"
dir.create(out_root, showWarnings = FALSE, recursive = TRUE)

# ----------------------------- Load data ----------------------------------
df <- read_csv("workers_panel_firm_entry_exit_varsize.csv",
               col_types = cols(
                 year = col_integer(),
                 business_id = col_integer(),
                 personal_id = col_integer(),
                 education_level = col_factor(levels = c("low","medium","high")),
                 age = col_integer(),
                 tenure = col_integer(),
                 firm_entry_year = col_integer(),
                 firm_exit_year  = col_integer(),
                 first_year_treatment = col_integer(),
                 treatment = col_integer(),
                 base_income = col_double()
               )
)

# 0) Basic guards
df <- df %>% filter(!is.na(first_year_treatment), !is.na(year))

# 1) Pre & post firms
df1 <- df %>%
  group_by(business_id) %>%
  mutate(has_pre  = any(year <  first_year_treatment),
         has_post = any(year >= first_year_treatment)) %>%
  ungroup() %>%
  filter(has_pre & has_post)

# 2) Keep years with at least one untreated
years_with_controls <- df1 %>%
  group_by(year) %>%
  summarise(any_untreated = any(treatment == 0), .groups = "drop") %>%
  filter(any_untreated) %>%
  pull(year)
df2 <- df1 %>% filter(year %in% years_with_controls)

# 3) Trim cohorts after last control year
last_control_year <- max(df2$year[df2$treatment == 0], na.rm = TRUE)
df3_base <- df2 %>% filter(first_year_treatment <= last_control_year)

# 4) Event-time bounds from trimmed sample
et_bounds <- df3_base %>%
  mutate(event_time = year - first_year_treatment) %>%
  summarise(min_e = max(min(event_time), -3, na.rm = TRUE),
            max_e = min(max(event_time),  5, na.rm = TRUE))
min_e <- et_bounds$min_e[1]; max_e <- et_bounds$max_e[1]

# ------------------ Collectors to write a single CSV per artifact ----------
gt_list      <- list()  # group-time ATT(g,t)
simple_list  <- list()  # aggregated simple ATT
dyn_list     <- list()  # dynamic ATT(e)
bounds_list  <- list()  # event-time bounds (for reproducibility)
metrics_list <- list()  # counts + TWFE R^2, N

# ------------------ Loop over outcomes ------------------------------------
for (y in outcomes) {
  message("\n==============================")
  message("Outcome: ", y)
  message("==============================")
  
  # Outcome-specific sample
  df3 <- df3_base %>% filter(!is.na(.data[[y]])) %>%
    mutate(event_time = year - first_year_treatment)
  
  # Record bounds for reproducibility
  bounds_list[[y]] <- tibble(
    outcome = y,
    min_e = min_e,
    max_e = max_e
  )
  
  # ------------------ Callaway & Sant’Anna (repeated cross-sections) -------
  attgt <- did::att_gt(
    yname = y,
    tname = "year",
    gname = "first_year_treatment",
    data  = df3,
    panel = FALSE,
    control_group = "notyettreated",
    clustervars   = "business_id"
  )
  
  # Safely extract vectors (allow older/newer did versions)
  time_vec <- if (!is.null(attgt$year)) attgt$year else attgt$t
  att_vec  <- attgt$att
  se_vec   <- attgt$se
  grp_vec  <- attgt$group
  
  # Group-time table
  gt_tbl <- tibble(
    outcome = y,
    group   = grp_vec,
    year    = time_vec,
    att     = att_vec,
    se      = se_vec
  ) %>%
    mutate(
      event_time = year - group,
      ci_low  = att - 1.96 * se,
      ci_high = att + 1.96 * se
    )
  gt_list[[y]] <- gt_tbl
  
  # Aggregated ATT (simple)
  agg_simple <- did::aggte(attgt, type = "simple", na.rm = TRUE)
  simple_list[[y]] <- tibble(
    outcome = y,
    att     = agg_simple$overall.att,
    se      = agg_simple$overall.se,
    ci_low  = agg_simple$overall.att - 1.96 * agg_simple$overall.se,
    ci_high = agg_simple$overall.att + 1.96 * agg_simple$overall.se
  )
  
  # Dynamic ATT(e)
  agg_dyn <- did::aggte(attgt, type = "dynamic", na.rm = TRUE,
                        min_e = min_e, max_e = max_e)
  dyn_df <- tibble(
    outcome    = y,
    event_time = agg_dyn$egt,
    att        = agg_dyn$att.egt,
    se         = agg_dyn$se.egt
  ) %>%
    filter(!is.infinite(event_time), !is.na(att)) %>%
    mutate(
      ci_low  = att - 1.96 * se,
      ci_high = att + 1.96 * se
    )
  dyn_list[[y]] <- dyn_df
  
  # ------------------ Metrics: sizes + TWFE R^2 (auxiliary) ----------------
  # Note: att_gt has no R^2. We compute a *separate* TWFE fit to report R^2.
  # y ~ event-time dummies (ref -1) with worker & year FE
  # This is for *diagnostics only* and not the C&S identification.
  twfe_fit <- tryCatch(
    fixest::feols(
      formula = as.formula(paste0(y, " ~ i(event_time, ref = -1) | personal_id + year")),
      data = df3
    ),
    error = function(e) NULL
  )
  
  r2        <- tryCatch(as.numeric(fixest::fitstat(twfe_fit, "r2")), silent = TRUE, error = function(e) NA_real_)
  r2_within <- tryCatch(as.numeric(fixest::fitstat(twfe_fit, "r2_within")), silent = TRUE, error = function(e) NA_real_)
  n_twfe    <- tryCatch(as.numeric(fixest::nobs(twfe_fit)), silent = TRUE, error = function(e) NA_real_)
  
  metrics_list[[y]] <- tibble(
    outcome = y,
    n_rows_used = nrow(df3),
    n_businesses = dplyr::n_distinct(df3$business_id),
    n_persons    = dplyr::n_distinct(df3$personal_id),
    n_groups     = dplyr::n_distinct(df3$first_year_treatment),
    n_years      = dplyr::n_distinct(df3$year),
    n_gt_cells   = length(att_vec),
    n_gt_used    = sum(!is.na(att_vec)),
    last_control_year = last_control_year,
    twfe_r2        = r2,
    twfe_r2_within = r2_within,
    twfe_nobs      = n_twfe
  )
}

# ------------------ Write combined CSVs (one file per artifact) -----------
gt_all      <- dplyr::bind_rows(gt_list)
simple_all  <- dplyr::bind_rows(simple_list)
dyn_all     <- dplyr::bind_rows(dyn_list)
bounds_all  <- dplyr::bind_rows(bounds_list)
metrics_all <- dplyr::bind_rows(metrics_list)

readr::write_csv(gt_all,      file.path(out_root, "group_time_ATT.csv"))
readr::write_csv(simple_all,  file.path(out_root, "agg_simple.csv"))
readr::write_csv(dyn_all,     file.path(out_root, "dynamic_event_time.csv"))
readr::write_csv(bounds_all,  file.path(out_root, "event_time_bounds.csv"))
readr::write_csv(metrics_all, file.path(out_root, "run_metrics.csv"))

message("Wrote CSVs to: ", normalizePath(out_root))

