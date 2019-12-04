# DP2010DemoDataReview_WA
ORIGINAL CODE BY EDDIE HUNSINGER SEE CA LINKS BELOW

Viewing US Census Bureau Differential Privacy 2010 Demonstration Data, using Shiny for R

Data downloaded November 8, 2019 from IPUMS NHGIS, University of Minnesota: https://www.nhgis.org/differentially-private-2010-census-data 

Viewer for CA data: https://edyhsgr.shinyapps.io/DP2010DemoData_CA/ 
(copy: https://shiny.demog.berkeley.edu/eddieh/DP2010DemoData_CA/) 

# To adapt for your own state
This assumes you're using RStudio projects. If not, then use full paths to files.
1. Download the "Long" data for the 050 and 160 summary levels.
2. Unzip and place the csv files where you want.
3. Open the state_data_050_160.R code and run after changing the statefips filter call to your state FIPS # and change _WA to _XX based on your state abbreviation.
4. Adjust file names to the _XX variants.
