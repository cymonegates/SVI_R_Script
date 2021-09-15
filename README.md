# Social Vulnerability Index (SVI) R Script

# Background
ATSDR’s Geospatial Research, Analysis &amp; Services Program (GRASP) created the Centers for Disease Control and Prevention Social Vulnerability Index (CDC SVI or simply SVI) to help public health officials and emergency response planners identify and map the communities that will most likely need support before, during, and after a hazardous event. The SVI uses the American Community Survey (ACS), 5-year data (https://www.census.gov/data/developers/data-sets/acs-5year.html) and was calculated using SQL programming language. The CDC releases updated SVI data every two years. 

In an effort to obtain updated SVI data each year, the Arizona Department of Health Services (ADHS) wrote an R script that reproduces the CDC's SVI calculations and can pull on newly released ACS 5-year data each year.

All methods and background information for SVI can be found at: https://www.atsdr.cdc.gov/placeandhealth/svi/documentation/SVI_documentation_2018.html

As the R script is merely a reproduction of the CDC's calculations, any questions about the CDC's SVI methodology should be directed to svi_coordinator@cdc.gov

If you find any discrepancies between the CDC's 2018 SVI values and the 2018 values generated from this R Script, please email Cymone.Gates@azdhs.gov.

Please note that the SVI data set generated from this R script does not include 2 variables that are present in the CDC's data set. Those are AREA_SQMI (Tract area in square miles) and E_DAYPOP (Adjunct variable - Estimated daytime population, LandScan 2018). As far as we could tell, these variables do not affect the SVI calculations but may be useful for mapping the data.
