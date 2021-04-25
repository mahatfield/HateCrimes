library(readr)
library(dplyr)
library(tidyr)
library(usmap)
library(maptools)
library(ggplot2)
library(stringr)

seg_loc <- read_csv("SegregationLocation.csv")
hcrimes <- read_csv("HateCrimes.csv")

hcrimes$State <- as.factor(hcrimes$State)
hcrimes$Agency <- as.factor(hcrimes$Agency)
hcrimes$Agency_Type <- as.factor(hcrimes$Agency_Type)


hcrimes <- hcrimes %>%
  mutate(City = as.character(Agency))

hcrimes <- hcrimes[,c(1:4,34,5:33)]

for (j in 1:length(hcrimes$Agency)) {
  if(hcrimes$Agency_Type[j] == "University or College" | hcrimes$Agency_Type[j] == "State Police") {
    hcrimes$City[j] = hcrimes$Unit[j]
  }
  else if(hcrimes$Agency_Type[j] == "Other" | hcrimes$Agency_Type[j] == "Federal" | hcrimes$Agency_Type[j] == "Other State Agency"){
    hcrimes$City[j] = str_trim(str_c(as.character(hcrimes$Agency[j]), hcrimes$Unit[j], sep = " "))
  }
}

hcnas <- hcrimes[which(is.na(hcrimes$City)),]

hcrimes$City <- as.factor(hcrimes$City)
sum_hc <- hcrimes[which(!is.na(hcrimes$City)),] %>%
  group_by(St, City, Year) %>%
  summarise(RaceCrimes = sum(RaceBias),
            ReligionCrimes = sum(ReligionBias),
            SexCrimes = sum(SexBias),
            GenderCrimes = sum(GenderBias),
            DisabilityCrimes = sum(DisBias),)

sum_hc <- sum_hc %>% 
  pivot_wider(names_from = Year, values_from = c(RaceCrimes, ReligionCrimes,
                                                 SexCrimes, GenderCrimes,
                                                 DisabilityCrimes), values_fill = 0)

sum_hc[is.na(sum_hc)] <- 0

sh <- seg_loc %>%
  left_join(sum_hc, by = c("City" = "City", "State" = "St")) %>%
  mutate(DisCrimesTotal = (`DisabilityCrimes_1991`+`DisabilityCrimes_1992`+`DisabilityCrimes_1993`+
                              `DisabilityCrimes_1994`+`DisabilityCrimes_1995`+`DisabilityCrimes_1996`+
                              `DisabilityCrimes_1997`+`DisabilityCrimes_1998`+`DisabilityCrimes_1999`+
                              `DisabilityCrimes_2000`+`DisabilityCrimes_2001`+`DisabilityCrimes_2002`+
                              `DisabilityCrimes_2003`+`DisabilityCrimes_2004`+`DisabilityCrimes_2005`+
                              `DisabilityCrimes_2006`+`DisabilityCrimes_2007`+`DisabilityCrimes_2008`+
                              `DisabilityCrimes_2009`+`DisabilityCrimes_2010`+`DisabilityCrimes_2011`+
                              `DisabilityCrimes_2012`+`DisabilityCrimes_2013`+`DisabilityCrimes_2014`+
                              `DisabilityCrimes_2015`+`DisabilityCrimes_2016`+`DisabilityCrimes_2017`+
                              `DisabilityCrimes_2018`+`DisabilityCrimes_2019`),
         GenderCrimesTotal = (`GenderCrimes_1991`+`GenderCrimes_1992`+`GenderCrimes_1993`+
                                `GenderCrimes_1994`+`GenderCrimes_1995`+`GenderCrimes_1996`+
                                `GenderCrimes_1997`+`GenderCrimes_1998`+`GenderCrimes_1999`+
                                `GenderCrimes_2000`+`GenderCrimes_2001`+`GenderCrimes_2002`+
                                `GenderCrimes_2003`+`GenderCrimes_2004`+`GenderCrimes_2005`+
                                `GenderCrimes_2006`+`GenderCrimes_2007`+`GenderCrimes_2008`+
                                `GenderCrimes_2009`+`GenderCrimes_2010`+`GenderCrimes_2011`+
                                `GenderCrimes_2012`+`GenderCrimes_2013`+`GenderCrimes_2014`+
                                `GenderCrimes_2015`+`GenderCrimes_2016`+`GenderCrimes_2017`+
                                `GenderCrimes_2018`+`GenderCrimes_2019`),
         SexCrimesTotal = (`SexCrimes_1991`+`SexCrimes_1992`+`SexCrimes_1993`+
                             `SexCrimes_1994`+`SexCrimes_1995`+`SexCrimes_1996`+
                             `SexCrimes_1997`+`SexCrimes_1998`+`SexCrimes_1999`+
                             `SexCrimes_2000`+`SexCrimes_2001`+`SexCrimes_2002`+
                             `SexCrimes_2003`+`SexCrimes_2004`+`SexCrimes_2005`+
                             `SexCrimes_2006`+`SexCrimes_2007`+`SexCrimes_2008`+
                             `SexCrimes_2009`+`SexCrimes_2010`+`SexCrimes_2011`+
                             `SexCrimes_2012`+`SexCrimes_2013`+`SexCrimes_2014`+
                             `SexCrimes_2015`+`SexCrimes_2016`+`SexCrimes_2017`+
                             `SexCrimes_2018`+`SexCrimes_2019`),
         ReligionCrimesTotal = (`ReligionCrimes_1991`+`ReligionCrimes_1992`+`ReligionCrimes_1993`+
                                  `ReligionCrimes_1994`+`ReligionCrimes_1995`+`ReligionCrimes_1996`+
                                  `ReligionCrimes_1997`+`ReligionCrimes_1998`+`ReligionCrimes_1999`+
                                  `ReligionCrimes_2000`+`ReligionCrimes_2001`+`ReligionCrimes_2002`+
                                  `ReligionCrimes_2003`+`ReligionCrimes_2004`+`ReligionCrimes_2005`+
                                  `ReligionCrimes_2006`+`ReligionCrimes_2007`+`ReligionCrimes_2008`+
                                  `ReligionCrimes_2009`+`ReligionCrimes_2010`+`ReligionCrimes_2011`+
                                  `ReligionCrimes_2012`+`ReligionCrimes_2013`+`ReligionCrimes_2014`+
                                  `ReligionCrimes_2015`+`ReligionCrimes_2016`+`ReligionCrimes_2017`+
                                  `ReligionCrimes_2018`+`ReligionCrimes_2019`),
         RaceCrimesTotal = (`RaceCrimes_1991`+`RaceCrimes_1992`+`RaceCrimes_1993`+
                              `RaceCrimes_1994`+`RaceCrimes_1995`+`RaceCrimes_1996`+
                              `RaceCrimes_1997`+`RaceCrimes_1998`+`RaceCrimes_1999`+
                              `RaceCrimes_2000`+`RaceCrimes_2001`+`RaceCrimes_2002`+
                              `RaceCrimes_2003`+`RaceCrimes_2004`+`RaceCrimes_2005`+
                              `RaceCrimes_2006`+`RaceCrimes_2007`+`RaceCrimes_2008`+
                              `RaceCrimes_2009`+`RaceCrimes_2010`+`RaceCrimes_2011`+
                              `RaceCrimes_2012`+`RaceCrimes_2013`+`RaceCrimes_2014`+
                              `RaceCrimes_2015`+`RaceCrimes_2016`+`RaceCrimes_2017`+
                              `RaceCrimes_2018`+`RaceCrimes_2019`))


write_csv(sh, "JoinedData.csv")

not_joined <- sum_hc %>%
  anti_join(seg_loc, by = c("City" = "City", "St" = "State"))

loc <- read_csv("uscities.csv")


hc_loc <- not_joined %>%
  mutate(DisCrimesTotal = (`DisabilityCrimes_1991`+`DisabilityCrimes_1992`+`DisabilityCrimes_1993`+
                             `DisabilityCrimes_1994`+`DisabilityCrimes_1995`+`DisabilityCrimes_1996`+
                             `DisabilityCrimes_1997`+`DisabilityCrimes_1998`+`DisabilityCrimes_1999`+
                             `DisabilityCrimes_2000`+`DisabilityCrimes_2001`+`DisabilityCrimes_2002`+
                             `DisabilityCrimes_2003`+`DisabilityCrimes_2004`+`DisabilityCrimes_2005`+
                             `DisabilityCrimes_2006`+`DisabilityCrimes_2007`+`DisabilityCrimes_2008`+
                             `DisabilityCrimes_2009`+`DisabilityCrimes_2010`+`DisabilityCrimes_2011`+
                             `DisabilityCrimes_2012`+`DisabilityCrimes_2013`+`DisabilityCrimes_2014`+
                             `DisabilityCrimes_2015`+`DisabilityCrimes_2016`+`DisabilityCrimes_2017`+
                             `DisabilityCrimes_2018`+`DisabilityCrimes_2019`),
         GenderCrimesTotal = (`GenderCrimes_1991`+`GenderCrimes_1992`+`GenderCrimes_1993`+
                                `GenderCrimes_1994`+`GenderCrimes_1995`+`GenderCrimes_1996`+
                                `GenderCrimes_1997`+`GenderCrimes_1998`+`GenderCrimes_1999`+
                                `GenderCrimes_2000`+`GenderCrimes_2001`+`GenderCrimes_2002`+
                                `GenderCrimes_2003`+`GenderCrimes_2004`+`GenderCrimes_2005`+
                                `GenderCrimes_2006`+`GenderCrimes_2007`+`GenderCrimes_2008`+
                                `GenderCrimes_2009`+`GenderCrimes_2010`+`GenderCrimes_2011`+
                                `GenderCrimes_2012`+`GenderCrimes_2013`+`GenderCrimes_2014`+
                                `GenderCrimes_2015`+`GenderCrimes_2016`+`GenderCrimes_2017`+
                                `GenderCrimes_2018`+`GenderCrimes_2019`),
         SexCrimesTotal = (`SexCrimes_1991`+`SexCrimes_1992`+`SexCrimes_1993`+
                             `SexCrimes_1994`+`SexCrimes_1995`+`SexCrimes_1996`+
                             `SexCrimes_1997`+`SexCrimes_1998`+`SexCrimes_1999`+
                             `SexCrimes_2000`+`SexCrimes_2001`+`SexCrimes_2002`+
                             `SexCrimes_2003`+`SexCrimes_2004`+`SexCrimes_2005`+
                             `SexCrimes_2006`+`SexCrimes_2007`+`SexCrimes_2008`+
                             `SexCrimes_2009`+`SexCrimes_2010`+`SexCrimes_2011`+
                             `SexCrimes_2012`+`SexCrimes_2013`+`SexCrimes_2014`+
                             `SexCrimes_2015`+`SexCrimes_2016`+`SexCrimes_2017`+
                             `SexCrimes_2018`+`SexCrimes_2019`),
         ReligionCrimesTotal = (`ReligionCrimes_1991`+`ReligionCrimes_1992`+`ReligionCrimes_1993`+
                                  `ReligionCrimes_1994`+`ReligionCrimes_1995`+`ReligionCrimes_1996`+
                                  `ReligionCrimes_1997`+`ReligionCrimes_1998`+`ReligionCrimes_1999`+
                                  `ReligionCrimes_2000`+`ReligionCrimes_2001`+`ReligionCrimes_2002`+
                                  `ReligionCrimes_2003`+`ReligionCrimes_2004`+`ReligionCrimes_2005`+
                                  `ReligionCrimes_2006`+`ReligionCrimes_2007`+`ReligionCrimes_2008`+
                                  `ReligionCrimes_2009`+`ReligionCrimes_2010`+`ReligionCrimes_2011`+
                                  `ReligionCrimes_2012`+`ReligionCrimes_2013`+`ReligionCrimes_2014`+
                                  `ReligionCrimes_2015`+`ReligionCrimes_2016`+`ReligionCrimes_2017`+
                                  `ReligionCrimes_2018`+`ReligionCrimes_2019`),
         RaceCrimesTotal = (`RaceCrimes_1991`+`RaceCrimes_1992`+`RaceCrimes_1993`+
                              `RaceCrimes_1994`+`RaceCrimes_1995`+`RaceCrimes_1996`+
                              `RaceCrimes_1997`+`RaceCrimes_1998`+`RaceCrimes_1999`+
                              `RaceCrimes_2000`+`RaceCrimes_2001`+`RaceCrimes_2002`+
                              `RaceCrimes_2003`+`RaceCrimes_2004`+`RaceCrimes_2005`+
                              `RaceCrimes_2006`+`RaceCrimes_2007`+`RaceCrimes_2008`+
                              `RaceCrimes_2009`+`RaceCrimes_2010`+`RaceCrimes_2011`+
                              `RaceCrimes_2012`+`RaceCrimes_2013`+`RaceCrimes_2014`+
                              `RaceCrimes_2015`+`RaceCrimes_2016`+`RaceCrimes_2017`+
                              `RaceCrimes_2018`+`RaceCrimes_2019`)) %>%
  left_join(loc, by = c("City" = "city_ascii", "St" = "state_id")) %>%
  select(c(1:152,157,158))

hc_loc_nas <- hc_loc[is.na(hc_loc$lat),]

hc_loc <- hc_loc[!is.na(hc_loc$lat),]
hc_map_points <- usmap_transform(data.frame(hc_loc$lng, hc_loc$lat))

hc_loc <- hc_loc %>%
  left_join(hc_map_points, by = c("lng" = "hc_loc.lng", "lat" = "hc_loc.lat"))

write_csv(hc_loc, "NotJoinedData.csv")


colleges <- hcrimes[which(hcrimes$Agency_Type == "University or College"),]
colleges$College <- as.character(colleges$City)
for (c in 1:length(colleges$Agency)) {
  if(!is.na(colleges$Unit[c])) {
    colleges$College[c] = str_c(as.character(colleges$Agency[c]), as.character(colleges$Unit[c]), sep = " ")
  }
  else {
    colleges$College[c] = as.character(colleges$Agency[c])
  }
}  

colleges$College <- as.factor(colleges$College)

colleges_sum <- colleges %>%
  group_by(St, College, Year) %>%
  summarise(RaceCrimes = sum(RaceBias),
            ReligionCrimes = sum(ReligionBias),
            SexCrimes = sum(SexBias),
            GenderCrimes = sum(GenderBias),
            DisabilityCrimes = sum(DisBias),) %>% 
  pivot_wider(names_from = Year, values_from = c(RaceCrimes, ReligionCrimes,
                                                 SexCrimes, GenderCrimes,
                                                 DisabilityCrimes), values_fill = 0) %>%
  mutate(DisCrimesTotal = (`DisabilityCrimes_1991`+`DisabilityCrimes_1992`+`DisabilityCrimes_1993`+
                             `DisabilityCrimes_1994`+`DisabilityCrimes_1995`+`DisabilityCrimes_1996`+
                             `DisabilityCrimes_1997`+`DisabilityCrimes_1998`+`DisabilityCrimes_1999`+
                             `DisabilityCrimes_2000`+`DisabilityCrimes_2001`+`DisabilityCrimes_2002`+
                             `DisabilityCrimes_2003`+`DisabilityCrimes_2004`+`DisabilityCrimes_2005`+
                             `DisabilityCrimes_2006`+`DisabilityCrimes_2007`+`DisabilityCrimes_2008`+
                             `DisabilityCrimes_2009`+`DisabilityCrimes_2010`+`DisabilityCrimes_2011`+
                             `DisabilityCrimes_2012`+`DisabilityCrimes_2013`+`DisabilityCrimes_2014`+
                             `DisabilityCrimes_2015`+`DisabilityCrimes_2016`+`DisabilityCrimes_2017`+
                             `DisabilityCrimes_2018`+`DisabilityCrimes_2019`),
         GenderCrimesTotal = (`GenderCrimes_1991`+`GenderCrimes_1992`+`GenderCrimes_1993`+
                                `GenderCrimes_1994`+`GenderCrimes_1995`+`GenderCrimes_1996`+
                                `GenderCrimes_1997`+`GenderCrimes_1998`+`GenderCrimes_1999`+
                                `GenderCrimes_2000`+`GenderCrimes_2001`+`GenderCrimes_2002`+
                                `GenderCrimes_2003`+`GenderCrimes_2004`+`GenderCrimes_2005`+
                                `GenderCrimes_2006`+`GenderCrimes_2007`+`GenderCrimes_2008`+
                                `GenderCrimes_2009`+`GenderCrimes_2010`+`GenderCrimes_2011`+
                                `GenderCrimes_2012`+`GenderCrimes_2013`+`GenderCrimes_2014`+
                                `GenderCrimes_2015`+`GenderCrimes_2016`+`GenderCrimes_2017`+
                                `GenderCrimes_2018`+`GenderCrimes_2019`),
         SexCrimesTotal = (`SexCrimes_1991`+`SexCrimes_1992`+`SexCrimes_1993`+
                             `SexCrimes_1994`+`SexCrimes_1995`+`SexCrimes_1996`+
                             `SexCrimes_1997`+`SexCrimes_1998`+`SexCrimes_1999`+
                             `SexCrimes_2000`+`SexCrimes_2001`+`SexCrimes_2002`+
                             `SexCrimes_2003`+`SexCrimes_2004`+`SexCrimes_2005`+
                             `SexCrimes_2006`+`SexCrimes_2007`+`SexCrimes_2008`+
                             `SexCrimes_2009`+`SexCrimes_2010`+`SexCrimes_2011`+
                             `SexCrimes_2012`+`SexCrimes_2013`+`SexCrimes_2014`+
                             `SexCrimes_2015`+`SexCrimes_2016`+`SexCrimes_2017`+
                             `SexCrimes_2018`+`SexCrimes_2019`),
         ReligionCrimesTotal = (`ReligionCrimes_1991`+`ReligionCrimes_1992`+`ReligionCrimes_1993`+
                                  `ReligionCrimes_1994`+`ReligionCrimes_1995`+`ReligionCrimes_1996`+
                                  `ReligionCrimes_1997`+`ReligionCrimes_1998`+`ReligionCrimes_1999`+
                                  `ReligionCrimes_2000`+`ReligionCrimes_2001`+`ReligionCrimes_2002`+
                                  `ReligionCrimes_2003`+`ReligionCrimes_2004`+`ReligionCrimes_2005`+
                                  `ReligionCrimes_2006`+`ReligionCrimes_2007`+`ReligionCrimes_2008`+
                                  `ReligionCrimes_2009`+`ReligionCrimes_2010`+`ReligionCrimes_2011`+
                                  `ReligionCrimes_2012`+`ReligionCrimes_2013`+`ReligionCrimes_2014`+
                                  `ReligionCrimes_2015`+`ReligionCrimes_2016`+`ReligionCrimes_2017`+
                                  `ReligionCrimes_2018`+`ReligionCrimes_2019`),
         RaceCrimesTotal = (`RaceCrimes_1991`+`RaceCrimes_1992`+`RaceCrimes_1993`+
                              `RaceCrimes_1994`+`RaceCrimes_1995`+`RaceCrimes_1996`+
                              `RaceCrimes_1997`+`RaceCrimes_1998`+`RaceCrimes_1999`+
                              `RaceCrimes_2000`+`RaceCrimes_2001`+`RaceCrimes_2002`+
                              `RaceCrimes_2003`+`RaceCrimes_2004`+`RaceCrimes_2005`+
                              `RaceCrimes_2006`+`RaceCrimes_2007`+`RaceCrimes_2008`+
                              `RaceCrimes_2009`+`RaceCrimes_2010`+`RaceCrimes_2011`+
                              `RaceCrimes_2012`+`RaceCrimes_2013`+`RaceCrimes_2014`+
                              `RaceCrimes_2015`+`RaceCrimes_2016`+`RaceCrimes_2017`+
                              `RaceCrimes_2018`+`RaceCrimes_2019`))

write_csv(colleges_sum, "CollegeCrimes.csv")
