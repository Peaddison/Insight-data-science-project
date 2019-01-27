## List of needed packages
packages.all <- c("Hmisc", "tidycensus", "tidyverse",
                  "corrplot", "plyr","dummies", "Amelia")

# Installing any missing packages
new.packages <- packages.all[!(packages.all %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

#Loading all packages
sapply(packages.all, require, character = TRUE)

#Loading Census data from API: American Community Survey (ACS) data
#census_api_key("40_digit_code_goes_here", install=TRUE)
readRenviron("~/.Renviron")

#Checking variable names for ACS data
v2017 <- load_variables(2017, "acs5", cache = TRUE)
View(v2017)

#ACS data for California
ACS_puma_California <- get_acs(geography = "public use microdata area",
                               variables = c(Population = "B00001_001",
                                             Income = "B19013_001",
                                             Female_adult = "B01001_026",
                                             Female_child = "B01001_030",
                                             Male_adult = "B01001_002",
                                             Male_child = "B01001_006",
                                             White = "C02003_003",
                                             Black = "C02003_004",
                                             NatAmer = "C02003_005",
                                             Asian = "C02003_006",
                                             Hawaiian = "C02003_007",
                                             Hispanic = "B03001_003",
                                             Bachelors = "B15012_011"),
                               state = "CA",
                               output = "wide")
#Saving for later easy access
#write.csv (ACS_puma_California, "ACS_puma_California.csv")

# Transform "GEOID" predictor type from character to integer
# This is to make later merge possible
ACS_puma_California <- transform(ACS_puma_California, GEOID = as.integer(GEOID))
#This data has no PUMA field to link other tables to

#Loading downloaded data that has "PUMA" with "GEOID" to link later
geoid_PUMA <- read.csv("CPUMA0010_PUMA2010_components.csv")
geoid_PUMA <- geoid_PUMA[c("State_Name", "PUMA", "GEOID")]       #subset of data

# Attaching PUMA code to ACS data using "GEOID" feild and summing gender fields
ACS_join <- join(ACS_puma_California, geoid_PUMA, by='GEOID')
ACS_join$Male <- ACS_join$Male_adultE + ACS_join$Male_childE  #summing "male" fields
ACS_join$Female <- ACS_join$Female_adultE + ACS_join$Female_childE #summing "female" fields

#Subsetting only needed data
ACS_final <- ACS_join[c("PUMA", "GEOID", "Male", "Female", "WhiteE",
                        "BlackE", "NatAmerE", "AsianE", "HawaiianE")]
colnames(ACS_final) <- c("PUMA", "GEOID", "Male_ACS", "Female_ACS", 
                         "White_ACS", "Black_ACS", "AmerIndian_ACS", "Asian_ACS", 
                         "Hawaiian_ACS")

################################################################################################
#Reading downloaded California PUMS microdata which consists of 10% population
CA_pums <- read.csv("psam_p06.csv")

#Saving a backup in case the original gets corrupted
#CA_pums -> CA_backup

# Subsetting needed predictors
CA_pums_subset <- CA_pums[c("PUMA", "SEX", "RAC1P")]
colnames(CA_pums_subset) <- c("PUMA", "Sex", "Race")

# Creating dummy variable for sex and race
temp_1 <- cbind(CA_pums_subset, dummy(CA_pums_subset$Sex, sep = "_"))
temp_2 <- cbind(temp_1, dummy(temp_1$Race, sep = "_"))
colnames(temp_2) <- c("PUMA", "Sex", "Race", "Male", "Female", "White", "Black",
                      "AmerIndian", "Alaskan", "IndianAlaskan", "Asian", "Hawaiian",
                      "Other", "Mixed")

## Investigating missing data
 missmap(temp_2, col = c("red","blue"), main="Missing Observations of CA")
 #no missing data

# Aggregating the sum for different PUMA subsets
CA_pums_sum <- aggregate(. ~ PUMA, temp_2, sum)

#Subsetting only needed predictors
CA_pums_final <- CA_pums_sum[c("PUMA", "Male", "Female", "White", "Black",
                            "AmerIndian",  "Asian", "Hawaiian")]

# Joining  ACS data and PUMS data
Census_FINAL <- join(ACS_final, CA_pums_final, by='PUMA')
colnames(Census_FINAL)

# Correlations plot
correlations<-cor(Census_FINAL[,3:16])
min(abs(correlations))  # 0.005044358
min (correlations)  # -0.3655173
corrplot(correlations,addCoef.col="red",type = "lower")
#Variables satisfactorily correlated