import pandas as pd
import os
import glob

# Set input and output directories
input_folder = "CRAFTY-GLOBIO"
output_folder = "../data/forest/NUTS-2_averages/CRAFTY-GLOBIO_test"

# Ensure output dir exists
os.makedirs(output_folder, exist_ok=True)

# Filter files
input_files = glob.glob(os.path.join(input_folder, "NUTS2021*.csv"))

# SSPX to full scenario name
scenario_map = {
    "SSP1": "SSP1-RCP2.6",
    "SSP3": "SSP3-RCP7.0"
}

for file_path in input_files:
    # Extract exploratory case
    base_name = os.path.basename(file_path)
    #print(base_name)
    
    case_name = base_name.split("_")[-1].replace(".csv", "")

    # Read CSV
    df = pd.read_csv(file_path)
    if df.columns[0] == "":
        df = df.drop(columns=[df.columns[0]])

    # Process each scenario separately
    for ssp in ["SSP1", "SSP3"]:
        df_scenario = df[df["Scenario"] == ssp].copy()
        if df_scenario.empty:
            continue

        # Melt
        df_long = df_scenario.melt(
            id_vars=["Scenario", "Year", "NUTS_ID", "LEVL_CODE", "CNTR_CODE", "NUTS_NAME"],
            value_vars=["Mean", "Median", "Stdev"],
            var_name="variable",
            value_name="weighted_average_value"
        )

        # Rename variables
        df_long["variable"] = df_long["variable"].map({
            "Mean": "MSA_Mean",
            "Median": "MSA_Median",
            "Stdev": "MSA_Stdev"
        })

        # Add fixed metadata
        df_long["scenario"] = scenario_map[ssp]
        df_long["climate_model"] = "GLOBIO"
        df_long["forest_model"] = "GLOBIO"
        df_long["case"] = 0
        df_long["land_use_category"] = "all_forest"
        df_long["management_type"] = "baseline"
        df_long["unit"] = "index"
        df_long["surface_area"] = 0.0
        df_long["forest_surface_area"] = 0.0

        # Reorder and rename columns
        df_long = df_long[
            [
                "scenario", "climate_model", "forest_model", "case", "NUTS_ID", "Year",
                "land_use_category", "management_type", "variable", "unit",
                "weighted_average_value", "surface_area", "forest_surface_area",
                "LEVL_CODE", "CNTR_CODE", "NUTS_NAME"
            ]
        ]

        df_long.columns = [
            "scenario", "climate_model", "forest_model", "case", "NUTS_ID", "year",
            "land_use_category", "management_type", "variable", "unit",
            "weighted_average_value", "surface_area", "forest_surface_area",
            "LEVL_CODE", "CNTR_CODE", "NUTS_NAME"
        ]

        # Convert weighted averages to numeric
        df_long["weighted_average_value"] = pd.to_numeric(df_long["weighted_average_value"], errors="coerce")
        df_long = df_long.dropna(subset=["weighted_average_value"]).reset_index(drop=True)

        # Construct output filename
        output_filename = f"{scenario_map[ssp]}_{case_name}_CRAFTY-GLOBIO.csv"
        output_path = os.path.join(output_folder, output_filename)

        # Save the output for each SSP
        df_long.to_csv(output_path, index=False)
        print(f"Saved: {output_path}")
