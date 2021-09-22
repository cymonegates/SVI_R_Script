
#Title:          CDC Social Vulnerability Index (SVI) Calculations

#Purpose:        This SVI model was created by ATSDR and the CDC using SQL programming language. 
#                This code is a reproduction of the CDC's SVI calculations in R so that the values can be updated yearly for the state of Arizona.
#                All methods and background information for SVI can be found at:
#                https://www.atsdr.cdc.gov/placeandhealth/svi/documentation/SVI_documentation_2018.html

#Data Source:    American Community Survey (ACS), 5-year data https://www.census.gov/data/developers/data-sets/acs-5year.html

#Script Created: Jun 2021  | Last Updated: SEP 2021

#Script Author:  Cymone Gates,MPH - Informatics Epidemiologist - Arizona Department of Health Services (cymone.gates@azdhs.gov)

#any questions about the CDC's SVI methodology should be directed to svi_coordinator@cdc.gov


############################################################################################################################################################
# CENSUS API Key
############################################################################################################################################################

#You need a census API key to pull in the survey data. If you do not have a Census API key,
#you can register for one at https://api.census.gov/data/key_signup.html

#This function will add your CENSUS API key to your .Renviron file so it can be called securely without being stored in your code. 

 #census_api_key("insert your API key here", install = TRUE)

#If this is your first time adding your API key, uncomment line 30 and reload your environment so you can use the key without restarting R.
#readRenviron("~/.Renviron")


############################################################################################################################################################
#SET YOUR PARAMETERS - REQUIRED FOR THIS TO WORK
############################################################################################################################################################

#-------------------------------------------------------------------
# Parameter 1
#-------------------------------------------------------------------
#geography - change 'tract' to 'county' if you want to look at SVI by county instead of census tract
geo <- "tract"


#-------------------------------------------------------------------
# Parameter 2
#-------------------------------------------------------------------
#change 'yes' to 'no' if you want to do an SVI comparison within one or more specified states (i.e. rank SVI between tracts/counties in the same state or a few states) instead of a national SVI comparison (rank SVI between tracts/counties across the US)

national_analysis <- "yes" 



#-------------------------------------------------------------------
# Parameter 3
#-------------------------------------------------------------------
#year of American Community Survey (ACS), 5-year data. As of Sep 2021, 2019 was the most recent ACS year
#change to desired year
yr <- 2019


#-------------------------------------------------------------------
# Parameter 4
#-------------------------------------------------------------------
#enter the file path where you stored the CDC_SVI_2018_DD.xlsx file from GitHub - e.g. //computerA/folderA/folderB/

filepath <- " "

#-------------------------------------------------------------------
# Parameter 5
# Only needed if you want an intrastate or select state comparison
#-------------------------------------------------------------------
#if you want the intrastate comparison, replace 'AZ' with your state of interest or leave null if you want the national comparison

st <- "AZ" 

#note: if you want to compare a select group of states to each other e.g. rank SVI between Arizona, California, and Nevada only, you can do so like so:
#st <- c("AZ","CA","NV")



############################################################################################################################################################
# AFTER YOU SET THE ABOVE PARAMETERS, RUN THE ENTIRE SCRIPT AND A FINAL DATASET CALLED CDC_SVI WILL BE CREATED WITH THE SVI SCORES 
############################################################################################################################################################


#load packages - install prior to loading if you do not have these packages downloaded
library(tidycensus)
library(tidyverse)
library(readxl)
library(stringr)
library(sf)


############################################################################################################################################################
# BRING IN METADATA
############################################################################################################################################################


#list of SVI vars with definitions for you to become familiar with the variables
datadictionary <- read_excel(paste0(filepath, "CDC_SVI_2018_DD.xlsx"),sheet = "DD", range = "A2:B126")

#list of vars that need to be renamed from the Census ACS survey names to the names the CDC chose for the SVI tool
rename_varlist <- read_excel(paste0(filepath, "CDC_SVI_2018_DD.xlsx"),sheet = "Rename_Var_List")

#list of variable names to bring in from the ACS survey
load_varlist <- read_excel(paste0(filepath, "CDC_SVI_2018_DD.xlsx"),sheet = "Vars_to_Load")

#list of US state abbreviation for national SVI comparison
load_states <- read_excel(paste0(filepath, "CDC_SVI_2018_DD.xlsx"),sheet = "US_States")

#vector of ACS vars needed for below step
varSVI <- load_varlist$ACS_Load_Vars

#list of US state abbreviations to be used later for national comparison
US <- load_states$STATE #list of US state abbreviations for national comparison


############################################################################################################################################################
#PULLING ACS DATA INTO R
############################################################################################################################################################

#create the acs_results data set based on parameters set above

if (national_analysis == "yes")  {
  acs_results <- get_acs(
    geography = geo, #find additional options here:https://walker-data.com/tidycensus/articles/basic-usage.html#geography-in-tidycensus
    state = US,
    survey = 'acs5',  
    year = yr,
    variables = varSVI, #using list of variables generated in prior step
    geometry = FALSE,
    output = 'wide'
  )

} else {
  
  if (national_analysis == "no")  {
    acs_results <- get_acs(
      geography = geo, #find additional options here:https://walker-data.com/tidycensus/articles/basic-usage.html#geography-in-tidycensus
      state = st,
      survey = 'acs5',  
      year = yr,
      variables = varSVI, #using list of variables generated in prior step
      geometry = FALSE,
      output = 'wide'
    )
  }
}


#reformat the acs_results data set to match CDC file

if (geo == "tract")  {
acs_results <- acs_results %>% 
                  separate(NAME, c("TRACT", "COUNTY","STATE"),sep = ",",remove=FALSE) %>% 
                  mutate(ST=substring(GEOID,1,2)) %>% 
                  mutate(STCNTY=substring(GEOID,3,5)) %>% 
                  mutate(FIPS=substring(GEOID,6,11)) %>% select(-GEOID) %>% rename(LOCATION=NAME) %>% 
                  relocate(STATE   ,.before = LOCATION) %>% 
                  relocate(ST      ,.before = STATE) %>% 
                  relocate(STCNTY  ,.before = COUNTY) %>% 
                  relocate(FIPS    ,.after  = COUNTY) %>% 
                  relocate(TRACT   ,.after  = FIPS) %>% 
                  relocate(LOCATION,.after  = TRACT) %>% mutate_if(is.character, str_trim)
} else {
  if (geo == "county" ) {
  acs_results <- acs_results %>% 
    separate(NAME, c("COUNTY","STATE"),sep = ",",remove=FALSE) %>% 
    mutate(ST=substring(GEOID,1,2)) %>% 
    mutate(STCNTY=substring(GEOID,3,5)) %>% 
    rename(LOCATION=NAME) %>% 
    relocate(STATE   ,.before = LOCATION) %>% 
    relocate(ST      ,.before = STATE) %>% 
    relocate(STCNTY  ,.before = COUNTY) %>% 
    relocate(LOCATION,.after  = STCNTY) %>% mutate_if(is.character, str_trim)
  }
}

#add state abbreviations for each tract/county to match CDC file
acs_results <- acs_results %>% left_join(load_states, by = "STATE") %>% relocate(ST_ABBR,.after  = STATE) 


############################################################################################################################################################
# CREATE CDC VARIABLES - VARIABLE DESCRIPTIONS CAN BE FOUND AT https://www.atsdr.cdc.gov/placeandhealth/svi/documentation/SVI_documentation_2018.html
############################################################################################################################################################

acs_results <- acs_results %>% 
  mutate(E_LIMENG=select(.,c("B16005_007E","B16005_008E","B16005_012E","B16005_013E","B16005_017E","B16005_018E","B16005_022E",
                             "B16005_023E","B16005_029E","B16005_030E","B16005_034E","B16005_035E","B16005_039E","B16005_040E",
                             "B16005_044E","B16005_045E")) %>% rowSums(na.rm=TRUE)) %>% 
  mutate(E_SNGPNT=select(.,c("DP02_0007E","DP02_0009E")) %>% rowSums(na.rm=TRUE)) %>% 
  mutate(E_MUNIT =select(.,c("DP04_0012E","DP04_0013E")) %>% rowSums(na.rm=TRUE)) %>%   
  mutate(E_CROWD =select(.,c("DP04_0078E","DP04_0079E")) %>% rowSums(na.rm=TRUE))  


#Minority (all persons except white, non-Hispanic) estimate, 2014-2018 ACS
acs_results$E_MINRTY  = acs_results$S0601_C01_001E - acs_results$B01001H_001E 


acs_results$M_SNGPNT  = sqrt((acs_results$DP02_0007M^2) + (acs_results$DP02_0009M^2)) #Single parent household with children under 18 estimate MOE, 2014-2018 ACS	
acs_results$M_MUNIT   = sqrt((acs_results$DP04_0012M^2) + (acs_results$DP04_0013M^2)) #Housing in structures with 10 or more units estimate MOE, 2014-2018 ACS	
acs_results$M_CROWD   = sqrt((acs_results$DP04_0078M^2) + (acs_results$DP04_0079M^2)) #At household level (occupied housing units), more people than rooms estimate MOE, 2014-2018 ACS
acs_results$M_MINRTY  = sqrt((acs_results$S0601_C01_001M^2) + (acs_results$B01001H_001M^2))#Minority (all persons except white, non-Hispanic) estimate MOE, 2014-2018 ACS	

#Persons (age 5+) who speak English "less than well" estimate MOE, 2014-2018 ACS	
acs_results$M_LIMENG  = sqrt(acs_results$B16005_007M^2 + acs_results$B16005_008M^2 + acs_results$B16005_012M^2 + acs_results$B16005_013M^2 + acs_results$B16005_017M^2 + acs_results$B16005_018M^2 + 
                             acs_results$B16005_022M^2 + acs_results$B16005_023M^2 + acs_results$B16005_029M^2 + acs_results$B16005_030M^2 + acs_results$B16005_034M^2 + acs_results$B16005_035M^2 + 
                             acs_results$B16005_039M^2 + acs_results$B16005_040M^2 + acs_results$B16005_044M^2 + acs_results$B16005_045M^2)


acs_results$EP_AGE17  =   (acs_results$B09001_001E/acs_results$S0601_C01_001E)*100 #Percentage of persons aged 17 and younger estimate, 2014-2018 ACS	
acs_results$EP_SNGPNT = 	(acs_results$E_SNGPNT   /  acs_results$DP02_0001E) * 100 #Percentage of single parent households with children under 18 estimate, 2014-2018 ACS	
acs_results$EP_MUNIT  = 	(acs_results$E_MUNIT    /  acs_results$DP04_0001E)*100 #Percentage of housing in structures with 10 or more units estimate	
acs_results$EP_CROWD  = 	(acs_results$E_CROWD    /  acs_results$DP04_0002E)*100 #Percentage of occupied housing units with more people than rooms estimate	
acs_results$EP_GROUPQ = 	(acs_results$B26001_001E / acs_results$S0601_C01_001E)*100 #Percentage of persons in group quarters estimate, 2014-2018 ACS	
acs_results$EP_MINRTY = 	(acs_results$E_MINRTY    / acs_results$S0601_C01_001E)*100 #Percentage minority (all persons except white, non-Hispanic) estimate, 2014-2018 ACS	
acs_results$EP_LIMENG = 	(acs_results$E_LIMENG    / acs_results$B16005_001E)*100 #Percentage of persons (age 5+) who speak English "less than well" estimate, 2014-2018 ACS	



#Some MOE calculations resulted in errors because the value under the square root was negative.
#For these rows, as the Census Bureau suggests, the documentation calls for use of the formula for derived ratios, as opposed to that for derived proportions.
#Instead of the subtraction in the standard formula, we add.

#Percentage of persons aged 17 and younger estimate MOE, 2014-2018 ACS	
acs_results$MP_AGE17  =   ifelse(is.nan(acs_results$EP_AGE17),NA,((sqrt(acs_results$B09001_001M^2 - ((acs_results$EP_AGE17/100)^2*acs_results$S0601_C01_001M^2)))/acs_results$S0601_C01_001E)*100)
acs_results$MP_AGE17  =   ifelse(is.nan(acs_results$MP_AGE17),((sqrt(acs_results$B09001_001M^2 + ((acs_results$EP_AGE17/100)^2*acs_results$S0601_C01_001M^2)))/acs_results$S0601_C01_001E)*100,acs_results$MP_AGE17)

#Percentage of single parent households with children under 18 estimate MOE, 2014-2018 ACS	
acs_results$MP_SNGPNT = 	ifelse(is.nan(acs_results$EP_SNGPNT),NA,((sqrt(acs_results$M_SNGPNT^2 - ((acs_results$EP_SNGPNT/100)^2*acs_results$DP02_0001M^2)))/acs_results$DP02_0001E)*100)
acs_results$MP_SNGPNT = 	ifelse(is.nan(acs_results$MP_SNGPNT),((sqrt(acs_results$M_SNGPNT^2 + ((acs_results$EP_SNGPNT/100)^2*acs_results$DP02_0001M^2)))/acs_results$DP02_0001E)*100,acs_results$MP_SNGPNT)

#Percentage of housing in structures with 10 or more units estimate MOE	
acs_results$MP_MUNIT  = 	ifelse(is.nan(acs_results$EP_MUNIT),NA,((sqrt(acs_results$M_MUNIT^2 - ((acs_results$EP_MUNIT/100)^2*acs_results$DP04_0001M^2)))/acs_results$DP04_0001E)*100)
acs_results$MP_MUNIT  = 	ifelse(is.nan(acs_results$MP_MUNIT),((sqrt(acs_results$M_MUNIT^2 + ((acs_results$EP_MUNIT/100)^2*acs_results$DP04_0001M^2)))/acs_results$DP04_0001E)*100,acs_results$MP_MUNIT)

#Percentage of occupied housing units with more people than rooms estimate MOE	
acs_results$MP_CROWD  = 	ifelse(is.nan(acs_results$EP_CROWD),NA,((sqrt(acs_results$M_CROWD^2 - ((acs_results$EP_CROWD/100)^2* acs_results$DP04_0002M^2)))/ acs_results$DP04_0002E)*100)
acs_results$MP_CROWD  = 	ifelse(is.nan(acs_results$MP_CROWD),((sqrt(acs_results$M_CROWD^2 + ((acs_results$EP_CROWD/100)^2* acs_results$DP04_0002M^2)))/ acs_results$DP04_0002E)*100,acs_results$MP_CROWD)

#Percentage of persons in group quarters estimate MOE, 2014-2018 ACS	
acs_results$MP_GROUPQ = 	ifelse(is.nan(acs_results$EP_GROUPQ),NA,((sqrt(acs_results$B26001_001M^2 - ((acs_results$EP_GROUPQ/100)^2*acs_results$S0601_C01_001M^2)))/acs_results$S0601_C01_001E)*100)
acs_results$MP_GROUPQ = 	ifelse(is.nan(acs_results$MP_GROUPQ),((sqrt(acs_results$B26001_001M^2 + ((acs_results$EP_GROUPQ/100)^2*acs_results$S0601_C01_001M^2)))/acs_results$S0601_C01_001E)*100,acs_results$MP_GROUPQ)

#Percentage minority (all persons except white, non-Hispanic) estimate MOE, 2014-2018 ACS	
acs_results$MP_MINRTY = 	ifelse(is.nan(acs_results$EP_MINRTY),NA,((sqrt(acs_results$M_MINRTY^2 - ((acs_results$EP_MINRTY/100)^2*acs_results$S0601_C01_001M^2)))/acs_results$S0601_C01_001E)*100)
acs_results$MP_MINRTY = 	ifelse(is.nan(acs_results$MP_MINRTY),((sqrt(acs_results$M_MINRTY^2 + ((acs_results$EP_MINRTY/100)^2*acs_results$S0601_C01_001M^2)))/acs_results$S0601_C01_001E)*100,acs_results$MP_MINRTY)

#Percentage of persons (age 5+) who speak English "less than well" estimate MOE, 2014-2018 ACS	
acs_results$MP_LIMENG = 	ifelse(is.nan(acs_results$EP_LIMENG),NA,((sqrt(acs_results$M_LIMENG^2 - ((acs_results$EP_LIMENG/100)^2*acs_results$B16005_001M^ 2)))/acs_results$B16005_001E)*100)
acs_results$MP_LIMENG = 	ifelse(is.nan(acs_results$MP_LIMENG),((sqrt(acs_results$M_LIMENG^2 + ((acs_results$EP_LIMENG/100)^2*acs_results$B16005_001M^ 2)))/acs_results$B16005_001E)*100,acs_results$MP_LIMENG)


#Some calculations resulted in some division by 0 errors in cases where E_TOTPOP equals 0 so we set these estimated proportions to 0
acs_results <- acs_results %>% 
  mutate_at(vars(EP_AGE17,EP_SNGPNT,EP_MUNIT,EP_CROWD,EP_GROUPQ,EP_LIMENG,EP_MINRTY), ~replace(., is.nan(.), 0))


############################################################################################################################################################
# REFORMAT DATA SET TO MATCH CDC NAMES AND PARAMETERS
############################################################################################################################################################

#reformat data to long format so we can join with rename_varlist
acs_long <- pivot_longer(acs_results,cols = B01001H_001E:MP_LIMENG, names_to = "ACS_VARNAME", values_to = "ESTIMATE")

#bring in CDC SVI var names
acs_long_newnames <- left_join(acs_long, rename_varlist, by =c("ACS_VARNAME"="ACS_VAR")) %>% select(-"DESCRIPTION")

#prefix for calculated variables
toMatch <- c("^EP_", "^MP_", "^E_", "^M_")

#keep the variable name for calculated vars and use var names from join for all remaining
acs_long_newnames$VAR_NAME <- ifelse(grepl(paste(toMatch,collapse = "|"), acs_long_newnames$ACS_VARNAME),acs_long_newnames$ACS_VARNAME , acs_long_newnames$VAR_NAME)

#create function to round like Microsoft Excel to stay consistent with CDC calculations
excel_round <- function(x, sigd){
  ifelse(substring(sub('.*\\.', '', x), 2, 2) == "5", round(x + .01,digits = sigd), round(x,digits = sigd))
}

#round estimates
acs_long_newnames$ESTIMATE <- excel_round(acs_long_newnames$ESTIMATE,sigd = 1)

#drop field that aren't needed any more - some were only needed for calculations, others were brought in by get_acs but they're not used in the CDC model
acs_long_newnames <- acs_long_newnames %>% drop_na(VAR_NAME) %>% select(-"ACS_VARNAME")
                                      
#reformat data back to wide with new var names  
CDC_SVI <- spread(acs_long_newnames,VAR_NAME, ESTIMATE) 

#remove EP values if population = 0 so they're not factored into the ranking
CDC_SVI <- CDC_SVI %>% 
  mutate(across(starts_with('EP_'), ~replace(., E_TOTPOP==0, NA)))

############################################################################################################################################################
# CREATE NEW SVI RANKING VARIABLES FOR EACH THEME
############################################################################################################################################################

#create new percentile ranking variables  
CDC_SVI <- CDC_SVI %>%
  mutate(round(across(starts_with('EP_'),percent_rank, .names = '{sub("EP", "EPL", .col)}'),digits=4))

#Per capita income has to be reversed as high income equates with low vulnerability and vice versa	
CDC_SVI$EPL_PCI <-    round(1- percent_rank(CDC_SVI$EP_PCI),digits = 4)#Percentile per capita income estimate

#Sum of series for Socioeconomic theme 
CDC_SVI <- CDC_SVI %>% 
 mutate(SPL_THEME1=round(select(.,c(EPL_UNEMP,EPL_PCI,EPL_NOHSDP,EPL_POV)) %>% rowSums(na.rm=FALSE),digits = 4))

#Sum of series for Household Composition & Disability theme
CDC_SVI <- CDC_SVI %>% 
  mutate(SPL_THEME2=round(select(.,c(EPL_AGE65,EPL_AGE17,EPL_DISABL,EPL_SNGPNT)) %>% rowSums(na.rm=FALSE),digits = 4))

#Sum of series for Minority Status/Language theme
CDC_SVI <- CDC_SVI %>% 
  mutate(SPL_THEME3=round(select(.,c(EPL_MINRTY,EPL_LIMENG)) %>% rowSums(na.rm=FALSE),digits = 4))

#Sum of series for Housing Type/Transportation
CDC_SVI <- CDC_SVI %>% 
  mutate(SPL_THEME4=round(select(.,c(EPL_MUNIT,EPL_MOBILE,EPL_CROWD,EPL_NOVEH,EPL_GROUPQ)) %>% rowSums(na.rm=FALSE),digits = 4))

#Sum of series themes	
CDC_SVI <- CDC_SVI %>% 
  mutate(SPL_THEMES=round(select(.,starts_with('SPL')) %>% rowSums(na.rm=FALSE),digits = 4))

#creates new RPL percentile ranking variables for each theme
CDC_SVI <- CDC_SVI %>%
  mutate(round(across(starts_with('SPL_'),percent_rank, .names = '{sub("SPL", "RPL", .col)}'),digits=4))

############################################################################################################################################################
# CREATE FLAG VARIABLES VARIABLES 
############################################################################################################################################################

# Tracts in the top 10%, are given a value of 1 for high vulnerability. Tracts below the 90th percentile are given a value of 0.
CDC_SVI <- CDC_SVI %>%
  mutate(across(starts_with('EPL_'),~ifelse(.x >=0.90,1,0), .names = '{sub("EPL", "F", .col)}'))

# For a theme, the flag value is the # of flags for variables comprising the theme.

#Sum of flags for Socioeconomic theme 
CDC_SVI <- CDC_SVI %>% 
  mutate(F_THEME1=round(select(.,c(F_UNEMP,F_PCI,F_NOHSDP,F_POV)) %>% rowSums(na.rm=FALSE),digits = 4))

#Sum of flags for Household Composition & Disability theme
CDC_SVI <- CDC_SVI %>% 
  mutate(F_THEME2=round(select(.,c(F_AGE65,F_AGE17,F_DISABL,F_SNGPNT)) %>% rowSums(na.rm=FALSE),digits = 4))

#Sum of flags for Minority Status/Language theme
CDC_SVI <- CDC_SVI %>% 
  mutate(F_THEME3=round(select(.,c(F_MINRTY,F_LIMENG)) %>% rowSums(na.rm=FALSE),digits = 4))

#Sum of flags for Housing Type/Transportation
CDC_SVI <- CDC_SVI %>% 
  mutate(F_THEME4=round(select(.,c(F_MUNIT,F_MOBILE,F_CROWD,F_NOVEH,F_GROUPQ)) %>% rowSums(na.rm=FALSE),digits = 4))

#Sum of flag themes	- The overall flag value for each tract is the number of all variable flags.
CDC_SVI <- CDC_SVI %>% 
  mutate(F_TOTAL=round(select(.,c(F_THEME1,F_THEME2,F_THEME3,F_THEME4)) %>% rowSums(na.rm=FALSE),digits = 4)) %>% 
  mutate_all(~replace(., is.na(.), -999))


############################################################################################################################################################
# END OF CODE
############################################################################################################################################################

#The final CDC_SVI data set has over 100 variables to match the CDC data set
#However if you only want the overall SVI scores, limit your data set to the following variables:

  #RPL_THEME1,RPL_THEME2,RPL_THEME3,RPL_THEME4, and RPL_THEMES

#Please note that the CDC_SVI data set does not include 2 variables in the original CDC data set:

  #AREA_SQMI (Tract area in square miles),and
  #E_DAYPOP (Adjunct variable - Estimated daytime population, LandScan 2018).

#As far as we could tell, these variables do not affect the SVI calculations but may be useful for
#mapping the data