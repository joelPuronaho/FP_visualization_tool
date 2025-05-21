import pandas as pd

# Read data - Change the input file accordingly. Also, remember to change the output file-name in the last row.
df = pd.read_csv("GLOBIO_hanneke_May17th/splitSSP_NUTS2021_SSP3_baseline.csv")
df = df.drop(columns=[df.columns[0]])

# Melt
df_long = df.melt(
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

# Add content to all columns, without input data
df_long["scenario"] = df_long["Scenario"].str.strip() + "-RCP2.6"
df_long["climate_model"] = "GLOBIO"
df_long["forest_model"] = "GLOBIO"
df_long["case"] = 0
df_long["land_use_category"] = "all_forest"
df_long["management_type"] = "baseline"
df_long["unit"] = "index"
df_long["surface_area"] = 0.0
df_long["forest_surface_area"] = 0.0

# Rename and reorder columns
df_long = df_long[
    [
        "scenario", "climate_model", "forest_model", "case", "NUTS_ID", "Year",
        "land_use_category", "management_type", "variable", "unit",
        "weighted_average_value", "surface_area", "forest_surface_area",
        "LEVL_CODE", "CNTR_CODE", "NUTS_NAME"
    ]
]


#df_long.columns = [col.lower() for col in df_long.columns]
df_long.columns = [
    "scenario", "climate_model", "forest_model", "case", "NUTS_ID", "year",
    "land_use_category", "management_type", "variable", "unit",
    "weighted_average_value", "surface_area", "forest_surface_area",
    "LEVL_CODE", "CNTR_CODE", "NUTS_NAME"
]

# Convert weighted averages to numeric
df_long["weighted_average_value"] = pd.to_numeric(df_long["weighted_average_value"], errors="coerce")
df_long = df_long.dropna(subset=["weighted_average_value"])

# OPTIONAL: reset index to keep the file clean
df_long = df_long.reset_index(drop=True)

# Save to csv - Rename the file according to the input file
df_long.to_csv("../data/forest/NUTS-2_averages/SSP3-RCP7.0_0_GLOBIO_TESTTEST.csv", index=False)
