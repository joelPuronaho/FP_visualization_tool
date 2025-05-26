import pandas as pd
import csv

# Input and output paths
input_file = "CRAFTY-GLOBIO/NUTS2021_SSP1_SSP3_baseline.csv"
output_file = "CRAFTY-GLOBIO/fixed_NUTS2021_SSP1_SSP3_baselineTEST.csv"

fixed_rows = []

with open(input_file, newline='', encoding='utf-8') as csvfile:
    reader = csv.reader(csvfile)
    headers = next(reader)

    for row in reader:
        try:
            scenario = row[1]
            year = row[2]
            nuts_id = row[6]
            levl_code = row[7]
            cntr_code = nuts_id[:2]
            name_latin = "NA"
            nuts_name = "NA"
            mean = row[8]
            median = row[9]
            stdev = row[10]

            fixed_row = [
                scenario, year, nuts_id, levl_code, cntr_code,
                name_latin, nuts_name, mean, median, stdev
            ]
            fixed_rows.append(fixed_row)
        except IndexError:
            print(f"Skipping malformed row: {row}")

# Define headers
fixed_headers = [
    "Scenario", "Year", "NUTS_ID", "LEVL_CODE", "CNTR_CODE",
    "NAME_LATIN", "NUTS_NAME", "Mean", "Median", "Stdev"
]

# Write output
with open(output_file, "w", newline='', encoding='utf-8') as f:
    writer = csv.writer(f)
    writer.writerow(fixed_headers)
    writer.writerows(fixed_rows)

print(f"Fixed data saved to: {output_file}")