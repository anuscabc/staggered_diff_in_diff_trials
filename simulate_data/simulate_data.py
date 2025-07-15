import numpy as np
import pandas as pd

class FirmPanelSimulator:
    def __init__(self, 
        n_firms=8000,
        start_year=2007,
        end_year=2023,
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


    def export_to_csv(self, filename="simulated_panel.csv"):
        self.panel = self.simulate()
        if self.panel is not None:
            self.panel.to_csv(filename, index=False)
            print(f"Panel data exported to {filename}")
        else:
            print("No panel data found. Run .simulate() first.")


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
        # Generate outcomes for the staggered treatment effect
        self.panel = self.simulate_outcomes()

        return self.panel


    def simulate_entry_exit(self):
        panel = self.panel.copy()
        mask = np.random.rand(len(panel)) > self.drop_prob
        panel = panel[mask].copy()
        entry_years = panel.groupby("firm_id")["year"].min()
        exit_years = panel.groupby("firm_id")["year"].max()
        panel["entry_year"] = panel["firm_id"].map(entry_years)
        panel["exit_year"] = panel["firm_id"].map(exit_years)
        panel["firm_age"] = panel["year"] - panel["entry_year"]
        return panel
    
    def assign_treatment(self):
        treated_firms = np.random.choice(self.firm_ids, size=self.n_firms // 2, replace=False)
        self.panel["treated"] = self.panel["firm_id"].isin(treated_firms).astype(int)
        return self.panel

    def assign_treatment_years(self, treat_years=range(2010, 2019)):
        treated_firms = self.panel[self.panel["treated"] == 1]["firm_id"].unique()
        treat_year_map = {
            fid: np.random.choice(treat_years) for fid in treated_firms
        }
        all_firms = self.panel["firm_id"].unique()
        for fid in all_firms:
            if fid not in treat_year_map:
                treat_year_map[fid] = np.nan

        self.panel["first_treat"] = self.panel["firm_id"].map(treat_year_map)
        return self.panel
    
    def simulate_outcomes(self):
        panel = self.panel

        # Generate fixed effects
        firm_fe = {fid: np.random.normal(0, 1) for fid in panel["firm_id"].unique()}
        year_fe = {year: np.random.normal(0, 0.5) for year in panel["year"].unique()}

        panel["firm_fe"] = panel["firm_id"].map(firm_fe)
        panel["year_fe"] = panel["year"].map(year_fe)

        # Post-treatment dummy
        panel["post_treatment"] = (panel["treated"] == 1) & (panel["year"] >= panel["first_treat"])

        # Simulate outcomes
        noise1 = np.random.normal(0, 1, size=len(panel))
        noise2 = np.random.normal(0, 1, size=len(panel))
        noise3 = np.random.normal(0, 1, size=len(panel))

        panel["y1"] = (
            panel["firm_fe"] +
            panel["year_fe"] +
            2.0 * panel["post_treatment"].astype(int) +
            noise1
        )

        panel["y2"] = (
            panel["firm_fe"] +
            panel["year_fe"] + 
            3.0 * panel["post_treatment"].astype(int) +
            noise2
        )

        panel["y3"] = (
            panel["firm_fe"] +
            panel["year_fe"]  
            - 5.0 * panel["post_treatment"].astype(int) +
            noise3
        )

        return panel
    


def main():
    # Initialize simulator
    sim = FirmPanelSimulator()
    # Export to CSV
    sim.export_to_csv("simulated_panel.csv")

if __name__ == "__main__":
    main()
