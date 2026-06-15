"""
Simulate a staggered DiD panel of INCUMBENT workers.

Design
------
- Treatment is assigned at the FIRM level (staggered, 2010-2018).
- Workers are INCUMBENTS: each worker must be employed at their firm for
  at least `min_pre_years` (default 3) calendar years before that firm's
  treatment year.
- Each worker is attached to EXACTLY ONE firm; after they exit they do
  not re-enter the dataset.
- Post-treatment spell lengths are VARIABLE: workers exit at different
  times after the treatment event.
- Comparison: incumbent workers at treated firms vs. incumbent workers at
  never-treated firms (who satisfy the same tenure floor w.r.t. the
  earliest possible treatment cohort).
- True ATT is heterogeneous across cohorts (increases with cohort year)
  so that the CS / SA estimators are genuinely tested.
"""

import numpy as np
import pandas as pd
import os


class IncumbentWorkerSimulator:
    def __init__(
        self,
        n_firms: int = 500,
        workers_per_firm_range: tuple = (15, 60),
        start_year: int = 2005,
        end_year: int = 2023,
        treat_cohorts: tuple = tuple(range(2010, 2019)),   # 2010..2018
        treat_share: float = 0.50,
        min_pre_years: int = 3,   # incumbency requirement
        min_post_years: int = 1,  # every treated incumbent survives >= 1 yr post
        seed: int = 42,
    ):
        np.random.seed(seed)

        self.n_firms = n_firms
        self.w_min, self.w_max = workers_per_firm_range
        self.start_year = start_year
        self.end_year = end_year
        self.treat_cohorts = list(treat_cohorts)
        self.treat_share = treat_share
        self.min_pre_years = min_pre_years
        self.min_post_years = min_post_years

        # Heterogeneous true ATT per cohort: rises from 1.5 to 5.5
        self.cohort_att = {
            g: round(1.5 + 0.5 * i, 2)
            for i, g in enumerate(self.treat_cohorts)
        }

        # Earliest cohort determines the incumbency floor for control firms
        self._ref_year = min(self.treat_cohorts)   # 2010

    # ------------------------------------------------------------------
    def _firm_setup(self):
        firm_ids = np.arange(1, self.n_firms + 1)
        n_treated = round(self.n_firms * self.treat_share)
        treated_ids = set(
            np.random.choice(firm_ids, size=n_treated, replace=False)
        )

        firm_treat = {}
        for fid in firm_ids:
            firm_treat[fid] = (
                int(np.random.choice(self.treat_cohorts))
                if fid in treated_ids
                else 0          # 0 = never treated (did package convention)
            )

        firm_fe = {fid: np.random.normal(0, 1.0) for fid in firm_ids}
        year_fe = {
            yr: np.random.normal(0, 0.4)
            for yr in range(self.start_year, self.end_year + 1)
        }
        return firm_ids, firm_treat, firm_fe, year_fe

    # ------------------------------------------------------------------
    def simulate(self) -> pd.DataFrame:
        firm_ids, firm_treat, firm_fe, year_fe = self._firm_setup()

        rows = []
        worker_id = 0

        for fid in firm_ids:
            g = firm_treat[fid]            # 0 means never treated
            n_workers = np.random.randint(self.w_min, self.w_max + 1)
            ffe = firm_fe[fid]

            for _ in range(n_workers):
                worker_id += 1
                wfe = np.random.normal(0, 1.0)

                if g > 0:
                    # TREATED FIRM
                    # Incumbency: start at most (g - min_pre_years) years into window
                    latest_start = g - self.min_pre_years
                    if latest_start < self.start_year:
                        # Infeasible for this firm; skip worker
                        worker_id -= 1
                        continue
                    w_start = int(np.random.randint(self.start_year, latest_start + 1))

                    # Variable post-treatment spell: exit in [g+min_post, end_year]
                    w_exit = int(
                        np.random.randint(g + self.min_post_years, self.end_year + 1)
                    )

                    first_treat = g
                else:
                    # NEVER-TREATED FIRM
                    # Workers must be incumbents by the reference year (earliest cohort).
                    # → start at most (ref_year - min_pre_years)
                    latest_start = self._ref_year - self.min_pre_years  # 2007
                    if latest_start < self.start_year:
                        latest_start = self.start_year
                    w_start = int(np.random.randint(self.start_year, latest_start + 1))

                    # Variable exit: after reference year, throughout window
                    w_exit = int(
                        np.random.randint(self._ref_year + 1, self.end_year + 1)
                    )
                    first_treat = 0

                # Expand worker spell into worker-year rows
                for yr in range(w_start, w_exit + 1):
                    post = int(first_treat > 0 and yr >= first_treat)
                    true_att = self.cohort_att.get(first_treat, 0.0) if first_treat > 0 else 0.0
                    noise = np.random.normal(0, 1.0)

                    y = (
                        wfe
                        + ffe
                        + year_fe.get(yr, 0.0)
                        + true_att * post
                        + noise
                    )

                    rows.append({
                        "personal_id":  worker_id,
                        "business_id":  fid,
                        "year":         yr,
                        "first_treat":  first_treat,   # 0 = never treated
                        "treated":      int(first_treat > 0),
                        "post":         post,
                        "tenure_at_entry": g - w_start if g > 0 else 0,
                        "true_att":     true_att,
                        "y":            round(y, 6),
                    })

        df = pd.DataFrame(rows)
        df = df.sort_values(["business_id", "personal_id", "year"]).reset_index(drop=True)
        return df

    # ------------------------------------------------------------------
    def export_to_csv(self, path: str) -> pd.DataFrame:
        df = self.simulate()
        df.to_csv(path, index=False)

        n_firms_treated   = df[df["treated"] == 1]["business_id"].nunique()
        n_firms_control   = df[df["treated"] == 0]["business_id"].nunique()
        n_workers_treated = df[df["treated"] == 1]["personal_id"].nunique()
        n_workers_control = df[df["treated"] == 0]["personal_id"].nunique()

        print(f"\nWorker incumbent panel exported to: {os.path.abspath(path)}")
        print(f"  Total rows    : {len(df):>10,}")
        print(f"  Total workers : {df['personal_id'].nunique():>10,}")
        print(f"  Total firms   : {df['business_id'].nunique():>10,}")
        print(f"  Treated firms : {n_firms_treated:>10,}  |  workers: {n_workers_treated:,}")
        print(f"  Control firms : {n_firms_control:>10,}  |  workers: {n_workers_control:,}")
        print(f"  Calendar years: {df['year'].min()} – {df['year'].max()}")
        print(f"\n  True ATT by cohort:")
        for g, att in self.cohort_att.items():
            print(f"    Cohort {g}: ATT = {att}")

        # Verify incumbency constraint
        inc_check = (
            df[df["treated"] == 1]
            .groupby("personal_id")
            .agg(first_treat=("first_treat", "first"), earliest_year=("year", "min"))
            .assign(pre_years=lambda x: x["first_treat"] - x["earliest_year"])
        )
        assert inc_check["pre_years"].min() >= 3, "Incumbency constraint violated!"
        print(f"\n  Min pre-treatment years (treated workers): "
              f"{int(inc_check['pre_years'].min())}  ✓")
        return df


# -----------------------------------------------------------------------
if __name__ == "__main__":
    # Save relative to project root (one level up from simulate_data/)
    out_path = os.path.join(
        os.path.dirname(__file__), "..", "worker_panel_incumbents.csv"
    )
    sim = IncumbentWorkerSimulator(n_firms=500, seed=42)
    sim.export_to_csv(out_path)
