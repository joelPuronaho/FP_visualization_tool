### Usage:
# - Change the input file and the final output name and location accordingly


import pandas as pd
import sys

# Load the data
infile = "EFISCEN-space/MPI26_bau_NUTS2_aggregated_FP.csv"
df = pd.read_csv(infile, sep=",", quotechar='"', skipinitialspace=True, encoding="utf-8")
#print(df.columns)

# Cols in lower case
df.columns = df.columns.str.lower().str.replace('"', '').str.strip()
#print(df.columns)

# Hard-coded variable columns from the input data. 
# *NEED TO BE CHANGED* if the input data has different variables
var_cols = [
    "area","nplots","shannon_trees","richness","gini",
    "veteran_trees_40","veteran_trees_50","veteran_stands_40","veteran_stands_50",
    "stand_c_ag","stand_c_bg","soil_c","gs","ba","n","nai","gai",
    "harv_tot","harv_tot_con","harv_tot_br"
]

# check if the cols are present
missing = [c for c in var_cols if c not in df.columns]
if missing:
    print("ERROR: These expected variable columns are missing:", missing, file=sys.stderr)
    print("Found columns are:", df.columns.tolist(), file=sys.stderr)
    sys.exit(1)

# Info cols
# *NEED TO BE CHANGED* if the input data has different columns
id_cols = ["climatescenario","climatemodel","forestmodel","case","nuts_id","year"]

# Lower case
id_cols = [c.lower() for c in id_cols]

missing_id = [c for c in id_cols if c not in df.columns]
if missing_id:
    print("ERROR: These ID columns are missing:", missing_id, file=sys.stderr)
    print("Found columns are:", df.columns.tolist(), file=sys.stderr)
    sys.exit(1)

# Melt data into long-format
df_long = df.melt(
    id_vars=id_cols,
    value_vars=var_cols,
    var_name="variable",
    value_name="weighted_average_value" # Currently app works with this, but should be changed to NUTS-2 specific name
)

# Rename cols back to original names
df_long = df_long.rename(columns={
    "climatescenario": "scenario",
    "climatemodel":    "climate_model",
    "forestmodel":     "forest_model",
    "nuts_id":         "NUTS_ID"
})

# Restore casing with variable names
orig_vars = {
    "area":"Area","nplots":"Nplots","shannon_trees":"Shannon_trees","richness":"Richness",
    "gini":"Gini","veteran_trees_40":"Veteran_trees_40","veteran_trees_50":"Veteran_trees_50",
    "veteran_stands_40":"Veteran_stands_40","veteran_stands_50":"Veteran_stands_50",
    "stand_c_ag":"Stand_C_AG","stand_c_bg":"Stand_C_BG","soil_c":"Soil_C",
    "gs":"GS","ba":"BA","n":"N","nai":"NAI","gai":"GAI",
    "harv_tot":"Harv_tot","harv_tot_con":"Harv_tot_con","harv_tot_br":"Harv_tot_br"
}
df_long["variable"] = df_long["variable"].map(orig_vars)

# Extra cols
df_long["land_use_category"]  = "all_forest"
df_long["management_type"]    = "probabilistic_harvest"

# *NEED TO BE CHANGED* if the input data has different variables / units
units = {
    "Area":"ha","Nplots":"count","Shannon_trees":"index","Richness":"species_count",
    "Gini":"index","Veteran_trees_40":"count_per_ha","Veteran_trees_50":"count_per_ha",
    "Veteran_stands_40":"count_per_ha","Veteran_stands_50":"count_per_ha",
    "Stand_C_AG":"tC/ha","Stand_C_BG":"tC/ha","Soil_C":"tC/ha",
    "GS":"growing_season_days","BA":"m2/ha","N":"trees_per_ha",
    "NAI":"m3/ha/yr","GAI":"m2/m2/yr","Harv_tot":"m3",
    "Harv_tot_con":"m3","Harv_tot_br":"m3"
}
df_long["unit"] = df_long["variable"].map(units)

# use the original "Area" for both surfaces
df_long["surface_area"]        = df_long["weighted_average_value"].where(df_long.variable=="Area").fillna(method="ffill")
# SELFNOTE: Need to change this
df_long["forest_surface_area"] = df_long["surface_area"]

# level & country codes
df_long["LEVL_CODE"] = df_long["NUTS_ID"].str.len()
df_long["CNTR_CODE"] = df_long["NUTS_ID"].str[:2]
# SELFNOTE: Need to change this. Can be added in the app.
df_long["NUTS_NAME"] = ""  # fill via lookup if you have one

# Order the cols
final_cols = [
    "scenario","climate_model","forest_model","case","NUTS_ID","year",
    "land_use_category","management_type",
    "variable","unit","weighted_average_value",
    "surface_area","forest_surface_area",
    "LEVL_CODE","CNTR_CODE","NUTS_NAME"
]

# Change the path and filename accordingly (depending on the input file)
df_long[final_cols].to_csv("SSP1-RCP2.6_0_EFISCEN-space.csv", index=False)
print(f"Written formatted_data.csv with {len(df_long)} rows.")
