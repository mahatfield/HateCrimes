library(readr)
library(tidyr)
library(dplyr)
library(usmap)
library(ggplot2)
library(maptools)

seg <- read_csv("CitySegregationSt.csv")
loc <- read_csv("uscities.csv")

seg$City <- as.factor(seg$City)
seg$State <- as.factor(seg$State)
loc$city_ascii <- as.factor(loc$city_ascii)
loc$state_id <- as.factor(loc$state_id)

seg_loc <- seg %>%
  left_join(loc, by = c("City" = "city_ascii", "State" = "state_id")) %>%
  select(c(1:72,75:78))

seg_loc_nas <- seg_loc[is.na(seg_loc$lat),]

#Removing unnecessary cities and counties
removals <- c("Clarke County", "Richmond County", "West Hartford", 
              "East Hartford", "Fayette", "Jefferson County", "Davidson", 
              "Suffolk", "Northern New Jersey", "Wayne", "Newburgh", "Arcade",
              "Saginaw Township North", "Paso Robles", "Barre", "Ewing", 
              "Ventura")
removals_states <- c("GA", "GA", "CT", 
                     "CT", "KY", "KY", "TN", 
                     "NY", "NJ", "NY", "NY", "CA",
                     "MI", "CA", "PA", "NJ",
                     "CA")

which_remove <- c()
for (i in 1:length(removals)) {
  which_remove <- append(which_remove, which(seg$City == removals[i] & seg$State == removals_states[i]))
}
seg <- seg[-which_remove,]


seg_loc <- seg %>%
  left_join(loc, by = c("City" = "city_ascii", "State" = "state_id")) %>%
  select(c(1:72,75:78))
seg_loc_nas <- seg_loc[is.na(seg_loc$lat),]


#Replacing names of cities and counties that don't fit
replacements <- c("Barnstable","Boise","Metuchen","Volo","Kenosha",
                  "Garden City", "Elizabeth", "Brentwood",
                  "Portsmouth","Rochester","Arden-Arcade",
                  "Wilkes-Barre","Winston-Salem")
to_replace <- as.character(seg_loc_nas$City)
to_replace_states <- as.character(seg_loc_nas$State)
seg$City <- as.character(seg$City)
seg$State <- as.character(seg$State)
for (r in 1:length(replacements)) {
  seg[which(seg$City == to_replace[r] & seg$State == to_replace_states[r]),3] <- replacements[r]
}
seg$City <- as.factor(seg$City)
seg$State <- as.factor(seg$State)

seg_loc <- seg %>%
  left_join(loc, by = c("City" = "city_ascii", "State" = "state_id")) %>%
  select(c(1:72,75:78))
seg_loc_nas <- seg_loc[is.na(seg_loc$lat),]

map_points <- usmap_transform(data.frame(seg_loc$lng, seg_loc$lat))


#Removing duplicates
duplicates <- seg_loc %>%
  select(City, State) %>%
  duplicated(nmax = 2)

duplicates_df <- seg_loc[duplicates,]
which_duplicates <- c()
for (d in 1:3) {
  which_duplicates <- append(which_duplicates,
                             which(seg_loc$City == duplicates_df$City[d] & seg_loc$State == duplicates_df$State[d]))
}
duplicates_full <- seg_loc[which_duplicates,]

seg$City <- as.character(seg$City)
duplicates_full$City <- as.character(duplicates_full$City)
which_dup_rem <- c()
for (f in c(1,4)) {
  which_dup_rem <- append(which_dup_rem,
                          which(seg$metroName == duplicates_full$metroName[f] &
                                  seg$City == duplicates_full$City[f]))
}
seg <- seg[-which_dup_rem,]


#FINAL segregation and locatoin dataframe
seg_loc <- seg %>%
  left_join(loc, by = c("City" = "city_ascii", "State" = "state_id")) %>%
  select(c(1:72,75:78))
seg_loc_nas <- seg_loc[is.na(seg_loc$lat),]

map_points <- usmap_transform(data.frame(seg_loc$lng, seg_loc$lat))

seg_loc <- seg_loc %>%
  left_join(map_points, by = c("lng" = "seg_loc.lng", "lat" = "seg_loc.lat"))

write_csv(seg_loc, "SegregationLocation.csv")
#testing map points
#plot_usmap(include = "KS") +
#  geom_point(data = seg_loc%>%filter(State == "KS"), aes(x = seg_loc.lng.1, 
#                                                         y = seg_loc.lat.1, 
#                                                         color = `09WhiteBlackDiss`,
#                                                         size = `09TotalCount`)) +
#  scale_color_gradientn(colors = c("green","blue","red"), 
#                        limits = c(20,100))

#JOINING HATE CRIMES
hcrimes <- read_csv("HateCrimes.csv")

hcrimes$State <- as.factor(hcrimes$State)
hcrimes$Agency <- as.factor(hcrimes$Agency)


sum_hc <- hcrimes %>%
  group_by(St, Agency, Year) %>%
  summarise(crimes = sum(RaceBias))

sum_hc <- sum_hc %>% 
  pivot_wider(names_from = Year, values_from = crimes)

sum_hc[is.na(sum_hc)] <- 0

seg_hc <- seg_loc %>%
  left_join(sum_hc, by = c("City" = "Agency", "State" = "St")) %>%
  mutate(TotalCrimes = (`1991`+`1992`+`1993`+`1994`+`1995`+`1996`+`1997`+`1998`+
                          `1999`+`2000`+`2001`+`2002`+`2003`+`2004`+`2005`+
                          `2006`+`2007`+`2008`+`2009`+`2010`+`2011`+`2012`+
                          `2013`+`2014`+`2015`+`2016`+`2017`+`2018`+`2019`))

plot_usmap() +
  geom_point(data = seg_hc, aes(x = seg_loc.lng.1, 
                                                         y = seg_loc.lat.1, 
                                                         color = `09WhiteBlackDiss`,
                                                         size = TotalCrimes)) +
  scale_color_gradientn(colors = c("green","blue","red"), 
                        limits = c(20,100))
