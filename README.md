# Social Vulnerability Index (SVI) R Script

# Overview
ATSDR’s Geospatial Research, Analysis &amp; Services Program (GRASP) created the Centers for Disease Control and Prevention Social Vulnerability Index (CDC SVI or simply SVI) to help public health officials and emergency response planners identify and map the communities that will most likely need support before, during, and after a hazardous event. The SVI uses the U.S. Census Bureau's American Community Survey (ACS), 5-year data (https://www.census.gov/data/developers/data-sets/acs-5year.html) and was calculated using SQL programming language. The CDC releases updated, publically available SVI data every two years. 

In an effort to obtain updated SVI data each year, the Arizona Department of Health Services (ADHS) wrote an R script that reproduces the CDC's SVI calculations and can pull on newly released ACS 5-year data each year.

All methods and background information for SVI can be found at: https://www.atsdr.cdc.gov/placeandhealth/svi/documentation/SVI_documentation_2018.html

# Instructions
1. Download the CDC_SVI_2018_DD.xlsx
2. Download the R Script
3. Insert your census API key into line 27 (if you do not have one there is guidance in the R script)
4. Set your parameters in Section 2
5. Comment lines 27 and 30
6. Run the entire code

# Notes and Considerations
The SVI data set generated from this R script does not include 2 variables that are present in the CDC's data set. Those are AREA_SQMI (Tract area in square miles) and E_DAYPOP (Adjunct variable - Estimated daytime population, LandScan 2018). As far as we could tell, these variables do not affect the SVI calculations but may be useful for mapping the data.

This code was based on the CDC's 2018 SVI documentation, which is slightly different from prior years (2016 and earlier) and may be different from future years if the CDC makes any changes. 

# Issues or Questions
As the R script is merely a reproduction of the CDC's calculations, any questions about the CDC's SVI methodology should be directed to svi_coordinator@cdc.gov

If you find any discrepancies between the CDC's 2018 SVI values and the 2018 values generated from this R Script, please post an Issue in this repository.

# License Standard Notice
The repository utilizes code licensed under the terms of the Apache Software License and therefore is licensed under ASL v2 or later.

This source code in this repository is free: you can redistribute it and/or modify it under the terms of the Apache Software License version 2, or (at your option) any later version.

This source code in this repository is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the Apache Software License for more details.

You should have received a copy of the Apache Software License along with this program. If not, see http://www.apache.org/licenses/LICENSE-2.0.html

The source code forked from other open source projects will inherit its license.

# Privacy Standard Notice
This repository contains only non-sensitive, publicly available data and information.
