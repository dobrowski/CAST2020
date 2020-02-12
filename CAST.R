


library(tidyverse)
library(here)
library(janitor)


### Load files ----------





#  Need to see if I downloaded the wrong file.  Is this CAA or the CAST  

cast <- read_csv(here("data", "cast_ca2019_1_csv_v1.txt") ) %>% 
    clean_names(case = "upper_camel") 


entities <- read_csv(here("data", "cast_ca2019entities_csv.txt") ) %>% 
    clean_names(case = "upper_camel") %>%
    mutate(CountyCode = as.character(CountyCode))


cast.dis <- cast %>%
    filter(CountyCode == "27",
        SchoolCode == "0000000",
           DemographicId == "1",
           Grade %in% c(5,8)
           ) %>%
    mutate_at(vars(TotalNumberTestedAtEntityLevelAndDemographic:EarthAndSpaceSciencesDomainPercentAboveStandard), list(as.numeric)) %>%
    left_join(entities) %>%
    select(DistrictName, Grade, PercentageStandardMetAndAbove) %>%
    arrange(Grade, desc(PercentageStandardMetAndAbove))


write_csv(cast.dis, here("CAST Percentage Met or Exceededby Grade.csv"))
