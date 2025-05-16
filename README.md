# FP_visualization_tool

### Usage

Clone the repository and open the directory in R-Studio. Then:

**NOTE**: The real data has been removed from the repository. Add your own data, or ask sample data from the author. The path for the NUTS-2 forest data should be "data/forest/NUTS-2_averages/"

1. Run the app through terminal with "Rscript FP_Visualization_App.R"
2. Click the IP-address in "Listening on http://xxx.x.x.x:xxxx"
3. The app opens in browser
    - You can choose the variable and year
4. Choose input file (default LPJ-GUESS)
5. Enable comparison mode to compare two input files
    - You can choose the variable, year, and Absolute difference or Percentual change (%) shown on map
6. Click on NUTS-region to activate the Radar chart, Line chart and summary statistics on NUTS-region
    - For Radar chart, also choose the variables from bottom left corner
7. Enjoy the data! :)

### Data

The real forest data has been removed. Ask for sample files, or add your own data.

NUTS-data: Many alternative NUTS-shapefiles in data/nuts. If you have your own data, make sure it is in the same format as the initial files in data/forest/sample_data: "SSP1-RCP2.6_0_FOREST-MODEL-NAME", where:
- "SSP1-RCP2.6" is the scenario RCP combination
- "0": refers to the exploratory case
- "FOREST-MODEL-NAME": change to e.g. "LPJ-GUESS"

Example file:
"NUTS_RG_60M_2021_4326_LEVL_2.shp"
- "60M" = scale.
- "2021" = year
- "4326" = coordinate system
- "LEVL_2" = NUTS-region level

By changing these in the file name on row 41, you can change the input shapefile to e.g. be more accurate "60M" -> "01M". More accuracy == slower app.

Not all the possible scale, year, CRS-system, NUTS-level combinations are in the folder. All the cases are behind the link below.
NUTS-data source: https://gisco-services.ec.europa.eu/distribution/v2/nuts/download/#nuts16

### Issues / Contributions

There has been some issues when working with iOS on some app versions. Feel free to modify the code locally if you encounter any issues.

In case of any additional problems, you can contact the author (you should have the email-address, did not share here to reduce possible spam).
Also, if you  have some ideas of improvements or happen to implement them yourself, feel free to contact! :)
