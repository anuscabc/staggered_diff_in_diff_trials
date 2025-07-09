import numpy as np
import pandas as pd

class FirmPanelSimulator:
    def __init__(self, 
        n_firms=500,
        start_year=2000,
        end_year=2010,
        seed=42):

        np.random.seed(seed)

        self.n_firms = n_firms
        self.start_year = start_year
        self.end_year = end_year
        self.firm_ids = list(range(1, n_firms + 1))
        print(self.firm_ids)
        self.years = list(range(start_year, end_year + 1))
        print(self.years)
        self.panel = None

        # Probability of dropping a firm-year row
        self.drop_prob = 0.2  

        # Randomply assign treatment for the firms 


    def simulate(self):
        # Generate full balanced panel
        self.panel = pd.MultiIndex.from_product(
            [self.firm_ids, self.years],
            names=["firm_id", "year"]
        ).to_frame(index=False)

        # Drop random firm-years
        self.panel = self.simulate_entry_exit()

        # Assign random treatment 
        self.panel = self.assign_treatment()

        # Randomly assign year of treatment 
        self.panel = self.assign_treatment_years()


        return self.panel


    def simulate_entry_exit(self):
        panel = self.panel.copy()
        mask = np.random.rand(len(panel)) > self.drop_prob
        panel = panel[mask].copy()

        # Entry, exit, firm age
        entry_years = panel.groupby("firm_id")["year"].min()
        exit_years = panel.groupby("firm_id")["year"].max()

        panel["entry_year"] = panel["firm_id"].map(entry_years)
        panel["exit_year"] = panel["firm_id"].map(exit_years)
        panel["firm_age"] = panel["year"] - panel["entry_year"]

        return panel
    
    def assign_treatment(self):
        treated_firms = np.random.choice(self.firm_ids, size=self.n_firms // 2, replace=False)
        self.panel["treated"] = self.panel["firm_id"].isin(treated_firms).astype(int)

    def assign_treatment_years(self, treat_years=range(2002, 2008)):
        # Randomly assign a first_treat year for treated firms
        treated_firms = self.panel[self.panel["treated"] == 1]["firm_id"].unique()
        treat_year_map = {
            fid: np.random.choice(treat_years) for fid in treated_firms
        }

        # Untreated firms get np.inf as first_treat
        all_firms = self.panel["firm_id"].unique()
        for fid in all_firms:
            if fid not in treat_year_map:
                treat_year_map[fid] = np.inf

        self.panel["first_treat"] = self.panel["firm_id"].map(treat_year_map)

        return self.panel