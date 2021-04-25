library(readr)
library(usmap)
library(ggplot2)
library(plotly)

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
      select(c(1,2, segregation_var, crime_vars, 153:156))
    filtered_df$CrimeTotal <- 0
    for (f in 1:length(filtered_df[,1])) {
      c_sum = 0
      for (v in 4:(3+length(crime_vars))) {
        c_sum = c_sum + filtered_df[f,v]
      }
      filtered_df$CrimeTotal[f] <- c_sum
    }
  }
  else {
    filtered_df <- df %>% 
      select(c(1,2, segregation_var, crime_vars, 153:156)) %>%
      filter(State == state_in)
    filtered_df$CrimeTotal <- 0
    for (f in 1:length(filtered_df$CrimeTotal)) {
      c_sum = 0
      for (v in 4:(3+length(crime_vars))) {
        c_sum = c_sum + filtered_df[f,v]
      }
      filtered_df$CrimeTotal[f] <- c_sum
    }
  }
  colnames(filtered_df)[3] <- "DemStat"
  return(filtered_df)
}

crime_dict <- list("RaceCrimes_" = "Race Crimes: ", 
                   "ReligionCrimes_" = "Religion Crimes: ",
                   "SexCrimes_" = "Sex Crimes: ", 
                   "GenderCrimes_" = "Gender Crimes: ",
                   "DisabilityCrimes_" = "Disability Crimes: ")

dem_stat_dict <- list("WhiteBlackDiss" = "White/Black Dissimilarity: ",
                      "WhiteHispDiss" = "White/Hispanic Dissimilarity: ",
                      "WhiteAsianDiss" = "White/Asian Dissimilarity: ",
                      "BlackHispDiss" = "Black/Hispanic Dissimilarity: ",
                      "BlackAsianDiss" = "Black/Asian Dissimilarity: ",
                      "HispAsianDiss" = "Hispanic/Asian Dissimilarity: ",
                      "WhitePerc" = "Percent White: ",
                      "BlackPerc" = "Percent Black: ",
                      "HispPerc" = "Percent Hispanic: ",
                      "AsianPerc" = "Percent Asian: ",
                      "OtherPerc" = "Percent Other Race: ",
                      "TotalCount" = "Total Population: ",
                      "WhiteCount" = "Number of Whites: ",
                      "BlackCount" = "Number of Blacks: ",
                      "HispCount" = "Number of Hispanics: ",
                      "AsianCount" = "Number of Asians: ",
                      "OtherCount" = "Number of Other Race: ")

dem_type_dict <- list("WhiteBlackDiss" = "Index of Dissimilarity",
                      "WhiteHispDiss" = "Index of Dissimilarity",
                      "WhiteAsianDiss" = "Index of Dissimilarity",
                      "BlackHispDiss" = "Index of Dissimilarity",
                      "BlackAsianDiss" = "Index of Dissimilarity",
                      "HispAsianDiss" = "Index of Dissimilarity",
                      "WhitePerc" = "Percent (%)",
                      "BlackPerc" = "Percent (%)",
                      "HispPerc" = "Percent (%)",
                      "AsianPerc" = "Percent (%)",
                      "OtherPerc" = "Percent (%)",
                      "TotalCount" = "Count",
                      "WhiteCount" = "Count",
                      "BlackCount" = "Count",
                      "HispCount" = "Count",
                      "AsianCount" = "Count",
                      "OtherCount" = "Count")
#plot_usmap(include = "CA", regions = "counties") +
#  geom_point(data = df%>%filter(State == "CA"), aes(x = usmaplon, y = usmaplat, 
#                                color = `09WhiteBlackDiss`, 
#                                size = RaceCrimesTotal)) +
#  geom_text(data = joined%>%filter(State == "CA"), aes(x = seg_loc.lng.1, y = seg_loc.lat.1, label = City)) +
#  scale_color_gradientn(colors = c("green","blue","red"), 
#                        limits = c(20,100))

instate <- "WI"
incrime <- "RaceCrimes_"
inyear <- c(2000,2005)
incensus <- "09"
indem <- "BlackHispDiss"


data_df <- getData(instate, incrime, inyear, 
                   incensus, indem)

g <- list(
  domain = list(x = 1, y = 1),
  scope = 'usa',
  projection = list(type = 'albers usa'),
  showland = TRUE,
  showcounties = TRUE,
  showframe = TRUE,
  landcolor = toRGB("gray95"),
  subunitcolor = toRGB("#262626"),
  countrycolor = toRGB("gray85"),
  countrywidth = 0.5,
  subunitwidth = 0.5
)

fig <- NULL

fig <- plot_geo(data = data_df, lat = ~lat, lon = ~lng)

fig <- fig %>% add_markers(
  text = ~paste(City, State, paste(crime_dict[[incrime]][1], CrimeTotal, sep = ""),
                paste(dem_stat_dict[[indem]][1], DemStat, sep = ""), 
                sep = "<br />"),
  color = ~DemStat,
  size = ~CrimeTotal)

fig <- fig %>% colorbar(title = dem_type_dict[[indem]][1])
fig <- fig %>% layout(geo = g)

fig
