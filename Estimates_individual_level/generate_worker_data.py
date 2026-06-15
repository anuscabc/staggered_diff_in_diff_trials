import pandas as pd
import numpy as np

np.random.seed(42)

# ---------------- Parameters ----------------
n_firms = 100                        # number of firms
years = np.arange(2007, 2021)        # observation window

# Draw firm sizes: 2..1000 workers per firm (inclusive)
firm_sizes = np.random.randint(2, 1001, size=n_firms)

# Build worker <-> firm mapping
firm_ids = np.arange(100, 100 + n_firms)
business_ids = np.repeat(firm_ids, firm_sizes)
n_workers = business_ids.size
personal_ids = np.arange(1, n_workers + 1)

# ------------- Firm-level timing -------------
# Entry in 2007..2012, exit in 2015..2020 (ensure >= 2 years after entry)
firm_entry_years = np.random.choice(np.arange(2007, 2013), size=n_firms, replace=True)
firm_exit_years  = np.random.choice(np.arange(2015, 2021), size=n_firms, replace=True)
firm_exit_years  = np.maximum(firm_exit_years, firm_entry_years + 2)

# First treatment year: strictly after entry, before 2019
first_treatment_years = np.array([
    np.random.choice(np.arange(entry + 1, 2019)) for entry in firm_entry_years
])

firm_info = pd.DataFrame({
    'business_id': firm_ids,
    'firm_entry_year': firm_entry_years,
    'firm_exit_year': firm_exit_years,
    'first_year_treatment': first_treatment_years,
    'firm_size': firm_sizes
})

# ------------- Worker characteristics -------------
education_levels = np.random.choice(['low', 'medium', 'high'],
                                    size=n_workers, p=[0.3, 0.5, 0.2])
ages    = np.random.randint(22, 50, n_workers)   # starting ages in 2007
tenures = np.random.randint(1, 6,  n_workers)    # starting tenure in 2007

workers_df = pd.DataFrame({
    'personal_id': personal_ids,
    'business_id': business_ids,
    'education_level': education_levels,
    'age_start': ages,
    'tenure_start': tenures
}).merge(firm_info, on='business_id', how='left')

# ------------- Expand to panel (active years only) -------------
rows = []
for _, w in workers_df.iterrows():
    active_years = np.arange(w.firm_entry_year, w.firm_exit_year + 1)
    for y in active_years:
        rows.append((
            y, w.personal_id, w.business_id, w.education_level,
            w.age_start, w.tenure_start, w.first_year_treatment,
            w.firm_entry_year, w.firm_exit_year
        ))

df = pd.DataFrame(rows, columns=[
    'year','personal_id','business_id','education_level',
    'age_start','tenure_start','first_year_treatment',
    'firm_entry_year','firm_exit_year'
])

# ------------- Age, tenure, treatment -------------
df['age']     = df['age_start']    + (df['year'] - 2007)
df['tenure']  = df['tenure_start'] + (df['year'] - 2007)
df['treatment'] = (df['year'] >= df['first_year_treatment']).astype(int)

# ------------- Generate income -------------
edu_effect      = df['education_level'].map({'low': 0, 'medium': 4000, 'high': 9000})
age_effect      = 350 * df['age']
tenure_effect   = 250 * df['tenure']
treatment_effect= 7000 * df['treatment']
trend_effect    = 300 * (df['year'] - 2007)
epsilon         = np.random.normal(0, 100, len(df))

df['base_income'] = (
    25_000 + edu_effect + age_effect + tenure_effect +
    trend_effect + treatment_effect + epsilon
)

# ------------- Final cleanup & save -------------
df = df.sort_values(['business_id','personal_id','year']).reset_index(drop=True)
df = df[['year','business_id','personal_id','education_level','age','tenure',
         'firm_entry_year','firm_exit_year','first_year_treatment',
         'treatment','base_income']]

df.to_csv("workers_panel_firm_entry_exit_varsize.csv", index=False)

print(df.shape)
print("Firms:", n_firms, " | Total workers:", n_workers)
print("Firm size summary (min/median/mean/max):",
      firm_sizes.min(), np.median(firm_sizes), round(firm_sizes.mean(),1), firm_sizes.max())