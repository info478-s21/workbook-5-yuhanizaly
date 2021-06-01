#Yuhaniz Aly
# Workbook 6: analyze NHANES data

# Set up
library(foreign)
library(survey)
library(Hmisc)
library(dplyr)

#load demographic data
demographics <- sasxport.get('demo_data.xpt')

#load alcohol data
alcohol <- sasxport.get('ALQ_I.xpt')

#merge data --what's the unique identifying characteristics? 

nhanes <- left_join(alcohol, demographics, by = 'seqn')

#sum of survey weights -- interpret the results
sum_survey_wt <- sum(demographics$wtint2yr) 
# The total US population at the time of the collection was 316481044.

#add more 
nhanes_updated <- nhanes %>%
  mutate(alq151 = replace(alq151, alq151 == 2, 0),
         alq151 = replace(alq151, alq151 == 7 | alq151 == 9, NA))

#create survey design

nhanes_design <-svydesign(
  id = ~sdmvpsu,
  nest = TRUE,
  strata = ~sdmvstra,
  weights = ~wtmec2yr,
  data = nhanes_updated 
)

#calc survey mean
survey_mean <- svymean(~alq151, nhanes_design, na.rm = T)

#mean by gender

mean_by_gender <- svyby(~alq151, ~riagendr, 
                        nhanes_design, svymean, na.rm = T)
