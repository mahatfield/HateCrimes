library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(splines)
library(npreg)

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

sh <- seg_loc %>%
  left_join(sum_hc, by = c("City" = "City", "State" = "St"))


joined <- read_csv("JoinedData.csv")
colnames(joined)[c(77,78)] <- c("usmaplon","usmaplat")
joined <- joined[,c(4,3,79:228,75:78,5:72)]
not_joined <- read_csv("NotJoinedData.csv")
colnames(not_joined)[1] <- "State"
colnames(not_joined)[c(155,156)] <- c("usmaplon","usmaplat")
not_joined[colnames(joined)[157:224]] <- NA

df <- rbind(not_joined,joined)


getData <- function(state_in, crime_in, year_in, census_in, dem_in) {
  segregation_var <- which(colnames(df) == str_c(census_in, dem_in, sep = ""))
  
  Crime_Years <- as.character(c(year_in[1]:year_in[2]))
  crime_vars <- which(colnames(df) %in% str_c(crime_in, Crime_Years))
  
  if(state_in == "US") {
    filtered_df <- df %>% 
      select(c(1,2, segregation_var, crime_vars, 153:156, 
               which(colnames(df) %in% str_c(census_in, "TotalCount")),
               which(colnames(df) %in% str_c(census_in, "WhitePerc"))))
    filtered_df$CrimeTotal <- rowSums(filtered_df[,4:(3+length(crime_vars))])
  }
  else {
    filtered_df <- df %>% 
      select(c(1,2, segregation_var, crime_vars, 153:156, 
               which(colnames(df) %in% str_c(census_in, "TotalCount")),
               which(colnames(df) %in% str_c(census_in, "WhitePerc")))) %>%
      filter(State == state_in)
    filtered_df$CrimeTotal <- rowSums(filtered_df[,4:(3+length(crime_vars))])
  }
  colnames(filtered_df)[3] <- "DemStat"
  colnames(filtered_df)[length(colnames(filtered_df))-2] <- "Population"
  colnames(filtered_df)[length(colnames(filtered_df))-1] <- "WhitePerc"
  filtered_df$Population <- as.numeric(filtered_df$Population)
  filtered_df$WhitePerc <- as.numeric(filtered_df$WhitePerc)
  return(filtered_df)
}
test <- getData("US", "SexCrimes_", c(2000,2005), "09", "BlackPerc")

first_model <- lm(DemStat ~ log(CrimeTotal) + lat + lng + lat*lng + Population,
                  data = getData("US", "RaceCrimes_", c(2000,2019), "09",
                                 "WhiteBlackDiss") %>% filter(CrimeTotal > 0))
summary(first_model)

second_model <- lm(log(CrimeTotal) ~ DemStat + lat + lng + lat*lng + Population,
                   data = getData("US", "RaceCrimes_", c(2000,2019), "09",
                                  "WhiteBlackDiss") %>% filter(CrimeTotal > 0))
summary(second_model)

third_model <- lm(log(CrimeTotal) ~ DemStat + lat + lng + lat*lng + Population + WhitePerc,
                  data = getData("US", "RaceCrimes_", c(2000,2019), "09",
                                 "WhiteBlackDiss") %>% filter(CrimeTotal > 0))
summary(third_model)

fourth_model <- lm(DemStat ~ log(CrimeTotal) + lat + lng + lat*lng + Population + WhitePerc,
                   data = getData("US", "RaceCrimes_", c(2000,2019), "09",
                                  "WhiteBlackDiss") %>% filter(CrimeTotal > 0))
summary(fourth_model)




year_sum <- sum_hc %>%
  group_by(Year) %>%
  summarise(raceCrimes = sum(RaceCrimes),
            religionCrimes = sum(ReligionCrimes),
            sexCrimes = sum(SexCrimes),
            genderCrimes = sum(GenderCrimes),
            disabilityCrimes = sum(DisabilityCrimes)) %>%
  mutate(TotalCrimes = raceCrimes + religionCrimes + sexCrimes + genderCrimes + disabilityCrimes)

ggplot(data = year_sum, aes(x = Year, y = raceCrimes)) +
  geom_line()
ggplot(data = year_sum, aes(x = Year, y = religionCrimes)) +
  geom_line()
ggplot(data = year_sum, aes(x = Year, y = sexCrimes)) +
  geom_line()
ggplot(data = year_sum, aes(x = Year, y = genderCrimes)) +
  geom_line()
ggplot(data = year_sum, aes(x = Year, y = disabilityCrimes)) +
  geom_line()
ggplot(data = year_sum, aes(x = Year, y = TotalCrimes)) +
  geom_line()
