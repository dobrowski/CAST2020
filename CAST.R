


library(tidyverse)
library(here)
library(janitor)
library(ggthemes)


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




codes <- c("66092", "6026256" ) # Crumpton 
codes <- c("66225","6026694" ) # Spreckles



cast.mry <- cast %>%
    filter(CountyCode %in% c("27","00"),
           DistrictCode %in% c("00000",codes) , #), ,"66225"
           SchoolCode %in% c("0000000",codes), #"6026694"),
#           DemographicId == "1",
           Grade %in% c(5)
    ) %>%
    mutate_at(vars(TotalNumberTestedAtEntityLevelAndDemographic:EarthAndSpaceSciencesDomainPercentAboveStandard), list(as.numeric)) %>%
    left_join(entities) %>%
    select(CountyCode,DistrictCode, SchoolCode, DistrictName, SchoolName , PercentageStandardMetAndAbove) %>%
    arrange(desc(PercentageStandardMetAndAbove)) %>%
    mutate(name = case_when(CountyCode == "00" ~ "California",
                            CountyCode == "27" & DistrictCode == "00000" ~ "Monterey County",
                            DistrictCode != "00000" & SchoolCode != "0000000" ~ SchoolName,
                            DistrictCode != "00000" & SchoolCode == "0000000" ~ DistrictName))
    


ggplot(cast.mry) +
    geom_col( aes(y = PercentageStandardMetAndAbove, x = fct_reorder( name,PercentageStandardMetAndAbove )) ) +
    theme_hc() +
    coord_flip() +
    labs(x = "",
         y = "Percentage Meeting or Exceeding Standard",
         title = "CAST 2019 Percentages")

ggsave("CAST Figure - Crumpton.png")
