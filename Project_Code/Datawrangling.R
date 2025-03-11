########################
########################
########################
###Data wrangling tab###
########################
########################
########################

##########################################
##Creating plot theme for visualisations##
##########################################

plottheme <- 
  theme_bw()+
  theme(plot.title = element_text(size = 20),
        axis.text = element_text(size=15),
        axis.title = element_text(size=20),
        legend.text = element_text(size=15),
        legend.title = element_text(size=20),
        legend.position = "bottom",
        plot.caption = element_text(size=15),
        plot.subtitle = element_text(size=15),
        panel.grid.minor = element_blank()) 

####################################################################
##Creating blank plot and message prompting user to make selection##
####################################################################

promptmessage <- function() {
  
  set.seed(20)
  x <- rnorm(10)
  y <- rnorm(10, 1, .5)
  
  data.frame(x, y) %>%
    ggplot(aes(x, y)) +
    labs(x = "",
         y = "") +
    theme(
      panel.grid = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank()
    ) +
    annotate("text",
             x = 5,
             5,
             label = "Please select a pollutant and a station and a metric \nfrom the drop down menus above \n\n(Note that 'Raw data' selections may take some time to \nrender due to the size of the underlying dataset)",
             size = 11)
  
}

############################
##Creating dataset for map##
############################

map_basedata <- basedata_final %>% 
  select(10,6,8,9) %>% 
  unique() %>% 
  st_as_sf(coords=c("Longitude", "Latitude")) %>% 
  st_set_crs(4326)


###################################
##Creating dataset for second tab##
###################################

Tab2_Dataset <- basedata_final %>% 
  group_by(`Date`, StationName, AirPollutant) %>% 
  summarise(`Daily average` = mean(Concentration),
            `Daily max` = max(Concentration)) %>% 
  pivot_longer(4:5, names_to = "category", values_to="total") %>% 
  mutate(total = round(total, 1),
         `Threshold exceeded` = case_when(AirPollutant == "PM10" & category == "Daily average" & total >=50 ~ "Yes",
                                          AirPollutant == "PM10" & category == "Daily average" & total <50 ~ "No",
                                          AirPollutant == "SO2" & category == "Daily average" & total >=125 ~"Yes",
                                          AirPollutant == "SO2" & category == "Daily average" & total <125 ~"No",
                                          T~"Not applicable"))

##################################
##Creating dataset for third tab##
##################################

Tab3_Dataset <- basedata_final %>% 
  group_by(Year, Month, Day, AirPollutant, StationName) %>%
  summarise(
    `Daily average` = mean(Concentration),
    `Daily max` = max(Concentration)
  ) %>% 
  filter(!is.na(`Daily average`)) %>% 
  ungroup() %>% 
  select(-Year) %>% 
  group_by(Month, Day, AirPollutant, StationName) %>% 
  summarise(`Daily average` = mean(`Daily average`),
            `Daily max` = max(`Daily max`)) %>% 
  pivot_longer(5:6, names_to = "category", values_to="total") %>% 
  mutate(total = round(total, 1),
         Month = month.abb[as.numeric(Month)],
         `Threshold exceeded` = case_when(AirPollutant == "PM10" & category == "Daily average" & total >=50 ~ "Yes",
                                          AirPollutant == "PM10" & category == "Daily average" & total <50 ~ "No",
                                          AirPollutant == "SO2" & category == "Daily average" & total >=125 ~"Yes",
                                          AirPollutant == "SO2" & category == "Daily average" & total <125 ~"No",
                                          T~"Not applicable"))

Tab3_Dataset$`Date` <- lubridate::myd(paste(Tab3_Dataset$Month, 2000, Tab3_Dataset$Day))

###################################
##Creating dataset for fourth tab##
###################################

Tab4_Dataset_wrangle <- basedata_final

Tab4_Dataset_wrangle$dayofweek <- lubridate::wday(Tab4_Dataset_wrangle$Date, label = T, abbr = F)

tojoin <- Tab4_Dataset_wrangle %>% 
  select(Date, dayofweek) %>% 
  unique()

##Creating two datasets, one for day of week..........
Tab4_Dataset_dayofweek <- Tab4_Dataset_wrangle %>% 
  select(dayofweek, StationName, AirPollutant, Concentration) %>%
  group_by(dayofweek, StationName, AirPollutant) %>%
  summarise(`Daily average` = mean(Concentration, na.rm = T),
            `Daily max` = max(Concentration, na.rm = T)) %>% 
  pivot_longer(4:5, names_to = "category", values_to="total") %>% 
  mutate(total = round(total, 1),
         classification = dayofweek, 
         `Threshold exceeded` = case_when(AirPollutant == "PM10" & category == "Daily average" & total >=50 ~ "Yes",
                                          AirPollutant == "PM10" & category == "Daily average" & total <50 ~ "No",
                                          AirPollutant == "SO2" & category == "Daily average" & total >=125 ~"Yes",
                                          AirPollutant == "SO2" & category == "Daily average" & total <125 ~"No",
                                          T~"Not applicable")) %>% 
  ungroup() %>% 
  select(-dayofweek)

#.........and another for hour of week
Tab4_Dataset_hourofweek <- Tab4_Dataset_wrangle %>%
  select(Hour, dayofweek, StationName, AirPollutant, Concentration) %>%
  group_by(Hour,dayofweek, StationName, AirPollutant) %>%
  summarise(`Hourly average` = mean(Concentration, na.rm = T),
            `Hourly max` = max(Concentration, na.rm = T)) %>% 
  mutate(classification = case_when(dayofweek == "Sunday"~Hour,
                          dayofweek == "Monday"~(Hour+24),
                          dayofweek == "Tuesday"~(Hour+(24*2)),
                          dayofweek == "Wednesday"~(Hour+(24*3)),
                          dayofweek == "Thursday"~(Hour+(24*4)),
                          dayofweek == "Friday"~(Hour+(24*5)),
                          dayofweek == "Saturday"~(Hour+(24*6)),
                          T~(Hour+(24*7)))) %>% 
  pivot_longer(5:6, names_to = "category", values_to="total") %>% 
  mutate(total = round(total, 1)) %>% 
  ungroup() %>% 
  select(3,4,6,7,5)


###################################
##Creating dataset for fifth tab###
###################################

Tab4_Dataset <- basedata_final %>%
  select(Hour, StationName, AirPollutant, Concentration) %>%
  group_by(Hour, StationName, AirPollutant) %>%
  summarise(`Hourly average` = mean(Concentration, na.rm = T),
            `Hourly max` = max(Concentration, na.rm = T)) %>% 
  pivot_longer(4:5, names_to = "category", values_to="total") %>% 
  mutate(total = round(total, 1))

#################################
##creating drop down menu options
#################################

#Selection for year
yearselect <- unique(basedata_final$Year) %>% sort()

#Selction for pollutant name
pollutant_name <- unique(basedata_final$AirPollutant)

#Metrics
categories <- unique(Tab2_Dataset$category)

categories_hourly <- unique(Tab4_Dataset$category)

categories_raw <- "Raw data"

categories_raw_combined <- c(paste0(categories_raw, " (hourly)"), paste0(categories_raw, " (daily)"))

#combining metrics lists
categories_combined <- c(categories, categories_raw)

categories_combined_hourly <- c(categories_hourly, categories_raw)

categories_tab5 <- c(categories, categories_hourly)

categories_combined_tab5 <- c(categories_tab5, categories_raw_combined)


###Creating station options for individual pollutants, as some stations don't record everything - in other words, users can only select one pollutant at a time

#first the cleaning function
stationname_function <- function(df, pollutant) {
  df %>%
    filter(AirPollutant == pollutant) %>%
    select(StationName) %>%
    unique() %>%
    arrange(StationName)
}
#Now creating a dataframe for each pollutant with respective station names

NO2_stations <- stationname_function(basedata_final, "NO2")
PM10_stations <- stationname_function(basedata_final, "PM10")
PM2.5_stations <- stationname_function(basedata_final, "PM2.5")
SO2_stations <- stationname_function(basedata_final, "SO2")



