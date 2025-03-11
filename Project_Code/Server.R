####################
####################
####################
###Server###########
####################
####################
####################


shinyServer(function(input, output, session)
    {
  
  #Adding links on the data tabs back to the Introduction tab
  observeEvent(input$link_to_home,
               {
                 updateTabsetPanel(session, "Panels",
                                   selected = "Introduction")
               })
  
  observeEvent(input$link_to_home2,
               {
                 updateTabsetPanel(session, "Panels",
                                   selected = "Introduction")
               })
  
  observeEvent(input$link_to_home3,
               {
                 updateTabsetPanel(session, "Panels",
                                   selected = "Introduction")
               })
  
  observeEvent(input$link_to_home4,
               {
                 updateTabsetPanel(session, "Panels",
                                   selected = "Introduction")
               })
  
    ##########################
    ##TAB 2: Time series tab##
    ##########################

    #We need to include the input for station and pollutant type in the server section rather than the UI section because station is dependent on pollutant input
    
    output$Pollutant <- renderUI({
        pickerInput(
            inputId = "Pollutant",
            label = "Pollutant",
            selected = NULL,
            multiple = TRUE,
            choices = pollutant_name,
            options = list(size = 10,
                           "max-options" = 1))
    })
    
    output$Station_Name <- renderUI({
        pickerInput(
            inputId="Station_Name",
            label = "Station Name (select up to 3)",
            choices = (if(input$Pollutant == "NO2")
                NO2_stations
                else if(input$Pollutant == "PM10")
                    PM10_stations
                else if(input$Pollutant == "PM2.5")
                    PM2.5_stations
                else SO2_stations),
            multiple = TRUE,
            selected = NULL,
            options = list(
                size = 10,
                `live-search` = TRUE,
                `selected-text-format` = "count > 1",
                `count-selected-text` = "{0} locations chosen",
                "max-options" = 3,
                "max-options-text" = "Only 3 options can be chosen"
            )
        )
    })
    
    ######################################################################################
    
    #Plot graph based on the user input.
    #First we create a subset based on user input
    
    tab2_dataoutput <- reactive({
      
        #If user selects Raw data
          if (input$Category ==
            "Raw data")
            
        {
              datasets_wd %>%
                  mutate(AirQualityStationEoICode = str_sub(., 1, 7)) %>%
                  left_join(locations, by = "AirQualityStationEoICode") %>%
                  filter(str_detect(filepath, input$Pollutant),
                         StationName %in% input$Station_Name) %>%
                  rowwise() %>%
                  do(., read_csv(file = .$filepath)) %>%
                  mutate(
                      category = "Raw data",
                      Date = lubridate::make_date(Year, Month, Day),
                      total = round(Concentration,1)
                  ) %>%
                  left_join(locations, by = "AirQualityStationEoICode") %>%
                  select(Date, Hour, StationName, AirPollutant, category, total) %>%
                  unique() %>% 
                  filter(
                      StationName %in% input$Station_Name
                      & AirPollutant %in% input$Pollutant
                      & categories_raw %in% input$Category,
                      !is.na(total)
                  )
        }
        
        #If user selects an average
        else {
            Tab2_Dataset %>%
                filter(
                    StationName %in% input$Station_Name
                    & AirPollutant %in% input$Pollutant
                    & categories %in% input$Category
                )
        }
    })
    
    #############
    #Plot output#
    #############
    
    output$timeseries_plot <- renderPlot({
        
        ## Default output is message in a blank ggplot object telling users to make proper selections
        
        if (is.null(input$Station_Name) |
            is.null(input$Pollutant) |
            is.null(input$Category))
            
        {
            promptmessage()
        }
        
        
        
        # If conditions are met, chart is plotted
        
        ##FIRST IF USER SELECTS RAW DATA
        else if (input$Category ==
                       "Raw data") 
            {
            
            vis_tab2 <- ggplot(tab2_dataoutput(), aes(x=`Date`, y=total, group=StationName, color=StationName))+
                geom_point(aes(group = StationName, color = StationName))+
                labs(title = paste0(input$Pollutant, " concentration over the time series (raw data)"),
                     y= paste0(input$Pollutant, " concentration (µg/m3)"),
                     x="Date",
                     caption = "Source: European Environmental Agency",
                     color = "Station Name") +
                plottheme +
                theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
                scale_y_continuous(limits = c(0,max(tab2_dataoutput()$total))) +
                scale_x_date(date_breaks = "3 months", date_labels = "%b-%Y")
            
            #Adding horizontal line denoting threshold dependent on user selection
            if (input$Category == "Daily average"
                & input$Pollutant == "SO2")
                
            {
                vis_tab2 + geom_hline(yintercept = 125, linetype = "dashed") +
                    labs(subtitle = "Dashed line corresponds to daily average limit for SO2 (see Introduction page for more information)")
                
            }
            
            else {
                vis_tab2
            }
        }
        
        ##IF USER SELECTS AN AGGREGATION
        else {

           vis_tab2 <- ggplot(tab2_dataoutput(), aes(x=`Date`, y = total, group = StationName, color = StationName))+
            geom_line(aes(group = StationName, color = StationName)) +
            labs(title = paste0(input$Category, " ", input$Pollutant, " concentration over the time series"),
                 y= paste0(input$Pollutant, " concentration (µg/m3)"),
                 x="Date",
                 caption = "Source: European Environmental Agency",
                 color = "Station Name") +
            plottheme +
            theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
            scale_y_continuous(limits = c(0,max(tab2_dataoutput()$total))) +
            scale_x_date(date_breaks = "3 months", date_labels = "%b-%Y")

           #Adding horizontal line denoting threshold dependent on user selection
           if (input$Category == "Daily average"
               & input$Pollutant == "PM10")
           {
               vis_tab2 + geom_hline(yintercept = 50, linetype = "dashed") +
                   labs(subtitle = "Dashed line corresponds to daily average limit for PM10 (see Introduction page for more information)")
               
           }
           else {
               vis_tab2
               
           }
           
        }
            
    })
    
    ############
    #Map output#
    ############
    
    map_df_tab2 <- reactive({
        map_basedata %>%
            filter(StationName %in% input$Station_Name
                   & AirPollutant %in% input$Pollutant)
    })
    
    
    output$map_tab2 <- renderLeaflet({
        if (is.null(input$Station_Name) |
            is.null(input$Pollutant) |
            is.null(input$Category))
            
        {
            NULL
        }
        
        else {
            leaflet() %>%
                addTiles() %>%
                addMarkers(data = map_df_tab2(),
                           label = input$Station_Name)
            
        }
        
    })
    ##############
    #Table output#
    ##############
    
    output$timeseries_table <- renderDataTable({
        if (input$Category ==
            "Raw data")
            
            #There is a formatting issue with pivoting when there are multiple selections in a single day, so raw data will be presented in long format
        {
            tab2_dataoutput()
        }
        
        else {
            tab2_dataoutput() %>%
                pivot_wider(names_from = StationName, values_from = total)
        }
    })
    
    #################
    #Download button#
    #################
    
    output$download_timeseries <- downloadHandler(
                filename = "pollutant-aggregated-subset.csv",
                content = function(file) {
                    write.csv(tab2_dataoutput(), file, row.names=FALSE)
                }
            )
    
    ###############################
    ##End of time series data tab##
    ###############################
    
    ###################################
    ###################################
    ###################################
    ###################################
    ###################################
    ###################################
    ###################################
    ###################################
    
    ########################################
    ##TAB 3: Daily data by day of year tab##
    ########################################

    #We need to include the input for station and pollutant type in the server section rather than the UI section because station is dependent on pollutant input

    output$Pollutant_yearly <- renderUI({
        pickerInput(
            inputId = "Pollutant_yearly",
            label = "Pollutant",
            selected = NULL,
            multiple = TRUE,
            choices = pollutant_name,
            options = list(size = 10,
                           "max-options" = 1))
    })

    output$Station_Name_yearly <- renderUI({
        pickerInput(
            inputId="Station_Name_yearly",
            label = "Station Name (select up to 3)",
            choices = (if(input$Pollutant_yearly == "NO2")
                NO2_stations
                else if(input$Pollutant_yearly == "PM10")
                    PM10_stations
                else if(input$Pollutant_yearly == "PM2.5")
                    PM2.5_stations
                else SO2_stations),
            multiple = TRUE,
            selected = NULL,
            options = list(
                size = 10,
                `live-search` = TRUE,
                `selected-text-format` = "count > 1",
                `count-selected-text` = "{0} locations chosen",
                "max-options" = 3,
                "max-options-text" = "Only 3 options can be chosen"
            )
        )
    })

    #Plot graph based on the user input.
    #First we create a subset based on user input

    tab3_dataoutput <- reactive({
      
      #If user selects Raw data
      if (input$Category_yearly ==
          "Raw data")
        
      {
        datasets_wd %>%
          mutate(AirQualityStationEoICode = str_sub(., 1, 7)) %>%
          left_join(locations, by = "AirQualityStationEoICode") %>%
          filter(str_detect(filepath, input$Pollutant_yearly),
                 StationName %in% input$Station_Name_yearly) %>%
          rowwise() %>%
          do(., read_csv(file = .$filepath)) %>%
          mutate(
            category = "Raw data",
            Year2 = 2000,
            Date = lubridate::make_date(Year2, Month, Day),
            `Date (actual)` = lubridate::make_date(Year, Month, Day),
            total = round(Concentration,1),
            Month2 = month.abb[as.numeric(Month)],
            MonthDay = paste0(Month2," ",Day)
            ) %>%
          left_join(locations, by = "AirQualityStationEoICode") %>%
          select(MonthDay, Date, `Date (actual)`, Hour, StationName, AirPollutant, category, total) %>%
          unique() %>% 
          filter(
            StationName %in% input$Station_Name_yearly
            & AirPollutant %in% input$Pollutant_yearly
            & categories_raw %in% input$Category_yearly,
            !is.na(total)
          )
      }
      
      #If user selects an average
      
      else{
        Tab3_Dataset %>%
          filter(
            StationName %in% input$Station_Name_yearly
            & AirPollutant %in% input$Pollutant_yearly
            & categories %in% input$Category_yearly
          )
      }
    })

    #############
    #Plot output#
    #############

    output$yearly_plot <- renderPlot({

        ## Default output is message in a blank ggplot object telling users to make proper selections

      if (is.null(input$Station_Name_yearly) |
          is.null(input$Pollutant_yearly) |
          is.null(input$Category_yearly))
        
      {
        promptmessage()
      }

        # If conditions are met, chart is plotted
      
      ##FIRST IF USER SELECTS RAW DATA
      else if (input$Category_yearly ==
               "Raw data")
      {
        vis_tab3 <- ggplot(tab3_dataoutput(), aes(x=`Date`, y=total, group=StationName, color=StationName))+
          geom_point(aes(group = StationName, color = StationName))+
          labs(title = paste0(input$Pollutant_yearly, " concentration by day of the year (raw data), 2013-2018"),
               y= paste0(input$Pollutant_yearly, " concentration (µg/m3)"),
               x="Month",
               caption = "Source: European Environmental Agency",
               color = "Station Name") +
          plottheme +
          scale_y_continuous(limits = c(0,max(tab3_dataoutput()$total))) +
          scale_x_date(date_breaks = "month", date_labels = "%B")

          vis_tab3
      }
      

        else {

           vis_tab3 <-  ggplot(tab3_dataoutput(), aes(x=`Date`, y = total, group = StationName, color = StationName))+
                geom_point(aes(group = StationName, color = StationName))+
                labs(title = paste0(input$Category_yearly, " ", input$Pollutant_yearly, " concentration by day of the year, 2013-2018"),
                     y= paste0(input$Pollutant_yearly, " concentration (µg/m3)"),
                     x="Month",
                     caption = "Source: European Environmental Agency",
                     color = "Station Name") +
                plottheme +
                scale_y_continuous(limits = c(0,max(tab3_dataoutput()$total))) +
                scale_x_date(date_breaks = "month", date_labels = "%B")
           
           #Adding horizontal lines for daily limits
           
           if (input$Category_yearly == "Daily average"
               & input$Pollutant_yearly == "PM10")
           {
             vis_tab3 + geom_hline(yintercept = 50, linetype = "dashed") +
               labs(subtitle = "Dashed line corresponds to daily average limit for PM10 (see Introduction page for more information)")
             
           }

           else {
             vis_tab3
           }

        }

    })

    ############
    #Map output#
    ############
    
    map_df_tab3 <- reactive({
      map_basedata %>%
        filter(StationName %in% input$Station_Name_yearly
               & AirPollutant %in% input$Pollutant_yearly)
    })
    
    
    output$map_tab3 <- renderLeaflet({
      if (is.null(input$Station_Name_yearly) |
          is.null(input$Pollutant_yearly) |
          is.null(input$Category_yearly))
        
      {
        NULL
      }
      
      else {
        leaflet() %>%
          addTiles() %>%
          addMarkers(data = map_df_tab3(),
                     label = input$Station_Name_yearly)
        
      }
      
    })
    
    ##############
    #Table output#
    ##############

    output$yearlydata_table <- renderDataTable({
      
      #There is an issue with pivoting when there are multiple selections in a single day, so raw data will be presented in long format
      if (input$Category_yearly ==
          "Raw data")
        
      {
        tab3_dataoutput() %>% 
          select(`Date (actual)`, MonthDay, Hour, StationName, AirPollutant, category, total, -`Date`)
      }
      
      
      else {
        tab3_dataoutput() %>%
          select(-`Date`) %>% 
          pivot_wider(names_from = StationName, values_from = total)
      }
    })


    #################
    #Download button#
    #################

    output$download_yearly <- downloadHandler(
        filename = "pollutant-daily-subset.csv",
        content = function(file) {
          tab3_dataoutput() %>% 
            select(-`Date`) %>% 
            write.csv(file, row.names=FALSE)
        }
    )

    ########################################
    ##End of daily data by day of year tab##
    ########################################
    
    ###################################
    ###################################
    ###################################
    ###################################
    ###################################
    ###################################
    ###################################
    ###################################
    
    #############################################
    ##TAB 4: Daily data by day/hour of week tab##
    #############################################
    
    #We need to include the input for station and pollutant type in the server section rather than the UI section because station is dependent on pollutant input
    
    output$Pollutant_daily <- renderUI({
        pickerInput(
            inputId = "Pollutant_daily",
            label = "Pollutant",
            selected = NULL,
            multiple = TRUE,
            choices = pollutant_name,
            options = list(size = 10,
                           "max-options" = 1))
    })
    
    output$Station_Name_daily<- renderUI({
        pickerInput(
            inputId="Station_Name_daily",
            label = "Station Name (select up to 3)",
            choices = (if(input$Pollutant_daily == "NO2")
                NO2_stations
                else if(input$Pollutant_daily == "PM10")
                    PM10_stations
                else if(input$Pollutant_daily == "PM2.5")
                    PM2.5_stations
                else SO2_stations),
            multiple = TRUE,
            selected = NULL,
            options = list(
                size = 10,
                `live-search` = TRUE,
                `selected-text-format` = "count > 1",
                `count-selected-text` = "{0} locations chosen",
                "max-options" = 3,
                "max-options-text" = "Only 3 options can be chosen"
            )
        )
    })
    
    #Plot graph based on the user input.
    #This one is slightly different because we need to combine two different types of data - aggregated by day and hour of week
    
    #First reactive call is if someone selects raw data for either hour or day of the week

    tab5_dataoutput_RAW <- reactive({

      if (input$Category_daily ==
          "Raw data (daily)")

      {
        datasets_wd %>%
          mutate(AirQualityStationEoICode = str_sub(., 1, 7)) %>%
          left_join(locations, by = "AirQualityStationEoICode") %>%
          filter(str_detect(filepath, input$Pollutant_daily),
                 StationName %in% input$Station_Name_daily) %>%
          rowwise() %>%
          do(., read_csv(file = .$filepath)) %>%
          mutate(category = "Raw data (daily)",
                 total = round(Concentration,1),
                 Date = lubridate::make_date(Year, Month, Day)) %>%
          left_join(locations, by = "AirQualityStationEoICode") %>%
          left_join(tojoin, by = "Date") %>%
          mutate(classification = dayofweek) %>%
          select(Date, StationName, AirPollutant, category, total, classification) %>%
          filter(
            StationName %in% input$Station_Name_daily
            & AirPollutant %in% input$Pollutant_daily,
            !is.na(total)
          )

      }

     else if (input$Category_daily ==
          "Raw data (hourly)")

      {

        datasets_wd %>%
          mutate(AirQualityStationEoICode = str_sub(., 1, 7)) %>%
          left_join(locations, by = "AirQualityStationEoICode") %>%
          filter(str_detect(filepath, input$Pollutant_daily),
                 StationName %in% input$Station_Name_daily) %>%
          rowwise() %>%
          do(., read_csv(file = .$filepath)) %>%
          mutate(category = "Raw data (hourly)",
                 total = round(Concentration,1),
                 Date = lubridate::make_date(Year, Month, Day)) %>%
          left_join(locations, by = "AirQualityStationEoICode") %>%
          left_join(tojoin, by = "Date") %>%
          mutate(classification = case_when(dayofweek == "Sunday"~Hour,
                                            dayofweek == "Monday"~(Hour+24),
                                            dayofweek == "Tuesday"~(Hour+(24*2)),
                                            dayofweek == "Wednesday"~(Hour+(24*3)),
                                            dayofweek == "Thursday"~(Hour+(24*4)),
                                            dayofweek == "Friday"~(Hour+(24*5)),
                                            dayofweek == "Saturday"~(Hour+(24*6)),
                                            T~(Hour+(24*7))),
                 `Threshold exceeded` = case_when(AirPollutant == "SO2" & total >=350 ~ "Yes",
                                                  AirPollutant == "SO2" & total <350 ~ "No",
                                                  AirPollutant == "NO2" & total >=200 ~ "Yes",
                                                  AirPollutant == "NO2" & total <200 ~ "No",
                                                  T~"Not applicable")) %>%
          select(Date, StationName, AirPollutant, category, total, classification, `Threshold exceeded`) %>%
          filter(
            StationName %in% input$Station_Name_daily
            & AirPollutant %in% input$Pollutant_daily,
            !is.na(total)
          )

      }

    })
    
    #If someone selects aggregated data, first by hour of week

    tab5_dataoutput_HOURLY <- reactive({
        Tab4_Dataset_hourofweek %>%
            filter(
                StationName %in% input$Station_Name_daily
                & AirPollutant %in% input$Pollutant_daily
                & categories_hourly %in% input$Category_daily
            )
    })
    
    #Then day of the week
    
    tab5_dataoutput_DAILY <- reactive({
        Tab4_Dataset_dayofweek %>%
            filter(
                StationName %in% input$Station_Name_daily
                & AirPollutant %in% input$Pollutant_daily
                & categories %in% input$Category_daily
            )
        
    })
        

    
    #############
    #Plot output#
    #############
    
    output$daily_plot <- renderPlot({
        
        ## Default output is message in a blank ggplot object telling users to make proper selections
        
        if (is.null(input$Station_Name_daily) |
            is.null(input$Pollutant_daily) |
            is.null(input$Category_daily)
        )
            
        {
            promptmessage()
        }
        
        # If conditions are met, chart is plotted
      
      
        # First creating a plot for if someone selects hourly RAW data
      
      else if (input$Category_daily ==
               "Raw data (hourly)")
        
      {
        
        vis_tab4 <- ggplot(tab5_dataoutput_RAW(), aes(x = classification, y = total, group = StationName, color = StationName))+
          geom_point(aes(group = StationName, color = StationName)) +
          labs(title = paste0(input$Pollutant_daily," concentration by hour of week (raw data) 2013-2018"),
               y= paste0(input$Pollutant_daily, " concentration (µg/m3)"),
               x="Hour of week",
               caption = "Source: European Environmental Agency",
               color = "Station Name") +
          plottheme +
          scale_y_continuous(limits = c(0,max(tab5_dataoutput_RAW()$total)))+
          scale_x_continuous(breaks = seq(0,170,5))
        
        vis_tab4
      }
      
      # If someone selects daily RAW data
      
      else if (input$Category_daily ==
               "Raw data (daily)")

      {
        vis_tab4 <- ggplot(tab5_dataoutput_RAW(), aes(x = classification, y = total, group = StationName, color = StationName))+
          geom_jitter(aes(group = StationName, color = StationName)) +
          labs(title = paste0(input$Pollutant_daily," concentration by day of week of the week (raw data) 2013-2018"),
               y= paste0(input$Pollutant_daily, " concentration (µg/m3)"),
               x="Day of week",
               caption = "Source: European Environmental Agency",
               color = "Station Name") +
          plottheme +
          scale_y_continuous(limits = c(0,max(tab5_dataoutput_RAW()$total)))
        
        vis_tab4

      }
      
      #If someone selects hourly aggregated data
      
        else if (input$Category_daily ==
                "Hourly average" |
                input$Category_daily ==
                "Hourly max"

            )

         {
            vis_tab4 <-  ggplot(tab5_dataoutput_HOURLY(), aes(x=classification, y = total, group = StationName, color = StationName))+
                geom_point(aes(group = StationName, color = StationName))+
                labs(title = paste0(input$Category_daily, " ", input$Pollutant_daily, " concentration by hour of week 2013-2018"),
                     y= paste0(input$Pollutant_daily, " concentration (µg/m3)"),
                     x="Hour of week",
                     caption = "Source: European Environmental Agency",
                     color = "Station Name") +
                plottheme +
                scale_y_continuous(limits = c(0,max(tab5_dataoutput_HOURLY()$total)))+
                scale_x_continuous(breaks = seq(0,170,5))
            
            vis_tab4
            
        }
        
        #Then creating a plot for data by day of the week
        
        else {
            
            vis_tab4 <-  ggplot(tab5_dataoutput_DAILY(), aes(x=classification, y = total, group = StationName, color = StationName))+
                geom_point(aes(group = StationName, color = StationName))+
                labs(title = paste0(input$Category_daily, " ", input$Pollutant_daily, " concentration by day of the week, 2013-2018"),
                     y= paste0(input$Pollutant_daily, " concentration (µg/m3)"),
                     x="Day of week",
                     caption = "Source: European Environmental Agency",
                     color = "Station Name") +
                plottheme +
                theme(axis.text.x = element_text(angle = 0, hjust=.5, vjust=.5)) +
                scale_y_continuous(limits = c(0,max(tab5_dataoutput_DAILY()$total))) 
            
            vis_tab4
            
            
            
            
        }

        
    })
    
    ############
    #Map output#
    ############
    
    map_df_tab4 <- reactive({
      map_basedata %>%
        filter(
          StationName %in% input$Station_Name_daily
          & AirPollutant %in% input$Pollutant_daily
        )
    })
    
    
    output$map_tab4 <- renderLeaflet({
      if (is.null(input$Station_Name_daily) |
          is.null(input$Pollutant_daily) |
          is.null(input$Category_daily))
        
      {
        NULL
      }
      
      else {
        leaflet() %>%
          addTiles() %>%
          addMarkers(data = map_df_tab4(),
                     label = input$Station_Name_daily)
        
      }
      
    })
    
    ##############
    #Table output#
    ##############
    
    ##Have to create two tables
    
    output$dailydata_table <- renderDataTable({
      if (input$Category_daily ==
          "Raw data (hourly)")
        
      {
        tab5_dataoutput_RAW() %>%
          mutate(`Hour of week` = classification) %>%
          select(`Hour of week`, everything(),-classification)
      }
      
      else if (input$Category_daily ==
               "Raw data (daily)")
        
      {
        tab5_dataoutput_RAW() %>%
          mutate(`Day of week` = classification) %>%
          select(`Day of week`, everything(),-classification)
      }
      
      else if (input$Category_daily ==
               "Hourly average" |
               input$Category_daily ==
               "Hourly max")
        
      {
        tab5_dataoutput_HOURLY() %>%
          mutate(`Hour of week` = classification) %>%
          select(`Hour of week`, everything(),-classification) %>%
          pivot_wider(names_from = StationName, values_from = total)
        
        
      }
      
      else {
        tab5_dataoutput_DAILY() %>%
          mutate(`Day of week` = classification) %>%
          select(`Day of week`, everything(),-classification) %>%
          pivot_wider(names_from = StationName, values_from = total)
        
      }
    })

    #################
    #Download button#
    #################

    ##Have to create two download buttons
    
    output$download_daily <- downloadHandler(
      filename = "pollutant-daily-dayhour-subset.csv",
      content = function(file) {
        
        if (input$Category_daily ==
            "Raw data (daily)" )
          
        {
          tab5_dataoutput_RAW() %>% 
            mutate(`Day of week` = classification) %>%
            select(`Day of week`, everything(),-classification) %>% 
            write.csv(file, row.names = FALSE) 
          
        }
        
        
         else if (input$Category_daily ==
          "Raw data (hourly)")
         {
           tab5_dataoutput_RAW() %>% 
             mutate(`Hour of week` = classification) %>%
             select(`Hour of week`, everything(),-classification) %>% 
             write.csv(file, row.names = FALSE) 
         }
          
          
        else if (input$Category_daily ==
                 "Hourly average" |
                 input$Category_daily ==
                 "Hourly max")
        {
          
          tab5_dataoutput_HOURLY() %>%
            mutate(`Hour of week` = classification) %>%
            select(`Hour of week`, everything(),-classification) %>% 
            write.csv(file, row.names = FALSE)
          
        }
        
        else {
          tab5_dataoutput_DAILY() %>%
            mutate(`Day of week` = classification) %>%
            select(`Day of week`, everything(),-classification) %>% 
            write.csv(file, row.names = FALSE)
        }
      }
    )
    
    ##############################################
    ##End of daily data by day/hour of week tab###
    ##############################################
    
    ###################################
    ###################################
    ###################################
    ###################################
    ###################################
    ###################################
    ###################################
    ###################################
    
    ##########################
    ##TAB 5: Hourly data tab##
    ##########################

    #We need to include the input for station and pollutant type in the server section rather than the UI section because station is dependent on pollutant input

    output$Pollutant_hourly <- renderUI({
        pickerInput(
            inputId = "Pollutant_hourly",
            label = "Pollutant",
            selected = NULL,
            multiple = TRUE,
            choices = pollutant_name,
            options = list(size = 10,
                           "max-options" = 1))
    })

    output$Station_Name_hourly <- renderUI({
        pickerInput(
            inputId="Station_Name_hourly",
            label = "Station Name (select up to 3)",
            choices = (if(input$Pollutant_hourly == "NO2")
                NO2_stations
                else if(input$Pollutant_hourly == "PM10")
                    PM10_stations
                else if(input$Pollutant_hourly == "PM2.5")
                    PM2.5_stations
                else SO2_stations),
            multiple = TRUE,
            selected = NULL,
            options = list(
                size = 10,
                `live-search` = TRUE,
                `selected-text-format` = "count > 1",
                `count-selected-text` = "{0} locations chosen",
                "max-options" = 3,
                "max-options-text" = "Only 3 options can be chosen"
            )
        )
    })

    #Plot graph based on the user input.
    #First we create a subset based on user input

    tab5_dataoutput <- reactive({
      
      if (input$Category_hourly ==
          "Raw data")
        
      {
        datasets_wd %>%
          mutate(AirQualityStationEoICode = str_sub(., 1, 7)) %>%
          left_join(locations, by = "AirQualityStationEoICode") %>%
          filter(
            str_detect(filepath, input$Pollutant_hourly),
            StationName %in% input$Station_Name_hourly
          ) %>%
          rowwise() %>%
          do(., read_csv(file = .$filepath)) %>%
          mutate(
            category = "Raw data",
            Date = lubridate::make_date(Year, Month, Day),
            total = round(Concentration, 1),
            `Threshold exceeded` = case_when(AirPollutant == "SO2" & total >=350 ~ "Yes",
                                             AirPollutant == "SO2" & total <350 ~ "No",
                                             AirPollutant == "NO2" & total >=200 ~ "Yes",
                                             AirPollutant == "NO2" & total <200 ~ "No",
                                             T~"Not applicable")
          ) %>%
          left_join(locations, by = "AirQualityStationEoICode") %>%
          select(Date, Hour, StationName, AirPollutant, category, total, `Threshold exceeded`) %>%
          unique() %>%
          filter(
            StationName %in% input$Station_Name_hourly
            & AirPollutant %in% input$Pollutant_hourly
            & categories_raw %in% input$Category_hourly,
            !is.na(total)
          )
      }
      
      else {
        Tab4_Dataset %>%
          filter(
            StationName %in% input$Station_Name_hourly
            & AirPollutant %in% input$Pollutant_hourly
            & categories_hourly %in% input$Category_hourly
          )
      }
    })

    #############
    #Plot output#
    #############

    output$hourly_plot <- renderPlot({

        ## Default output is message in a blank ggplot object telling users to make proper selections

        if (is.null(input$Station_Name_hourly) |
            is.null(input$Pollutant_hourly) |
            is.null(input$Category_hourly)
            )

        {
            promptmessage()
        }

        # If conditions are met, chart is plotted

      
      else if (input$Category_hourly ==
               "Raw data") 
      {
      
        vis_tab5 <-  ggplot(tab5_dataoutput(), aes(x=Hour, y = total, group = StationName, color = StationName))+
          geom_jitter(aes(group = StationName, color = StationName))+
          labs(title = paste0(input$Pollutant_hourly, " concentration by hour of the day (raw data), 2013-2018"),
               y= paste0(input$Pollutant_hourly, " concentration (µg/m3)"),
               x="Time of day",
               caption = "Source: European Environmental Agency",
               color = "Station Name") +
          plottheme +
          theme(axis.text.x = element_text(angle = 0, hjust=.5, vjust=.5)) +
          scale_y_continuous(limits = c(0,max(tab5_dataoutput()$total))) +
          scale_x_continuous(breaks = seq(0,23,1))
        
        vis_tab5
        
        }
      
      
        else {

            vis_tab5 <-  ggplot(tab5_dataoutput(), aes(x=Hour, y = total, group = StationName, color = StationName))+
                geom_point(aes(group = StationName, color = StationName))+
                labs(title = paste0(input$Category_hourly, " ", input$Pollutant_hourly, " concentration by hour of the day, 2013-2018"),
                     y= paste0(input$Pollutant_hourly, " concentration (µg/m3)"),
                     x="Time of day",
                     caption = "Source: European Environmental Agency",
                     color = "Station Name") +
                plottheme +
                theme(axis.text.x = element_text(angle = 0, hjust=.5, vjust=.5)) +
                scale_y_continuous(limits = c(0,max(tab5_dataoutput()$total))) +
                scale_x_continuous(breaks = seq(0,23,1))
            
            vis_tab5

            #Adding a line for daily limit



        }

    })

    ############
    #Map output#
    ############
    
    map_df_tab5 <- reactive({
        map_basedata %>%
            filter(StationName %in% input$Station_Name_hourly
                   & AirPollutant %in% input$Pollutant_hourly)
    })
    
    
    output$map_tab5 <- renderLeaflet({
        if (is.null(input$Station_Name_hourly) |
            is.null(input$Pollutant_hourly) |
            is.null(input$Category_hourly))
            
        {
            NULL
        }
        
        else {
            leaflet() %>%
                addTiles() %>%
                addMarkers(data = map_df_tab5(),
                           label = input$Station_Name_hourly)
            
        }
        
    })
    
    ##############
    #Table output#
    ##############

    output$hourlydata_table <- renderDataTable({
      
      if (input$Category_hourly ==
          "Raw data")
        
        #There is an issue with pivoting when there are multiple selections in a single day, so raw data will be presented in long format
      {
        tab5_dataoutput()
      }
      
      else {
        tab5_dataoutput() %>%
          pivot_wider(names_from = StationName, values_from = total)
      }
      
    })

    #################
    #Download button#
    #################

    output$download_hourly <- downloadHandler(
        filename = "pollutant-hourly-subset.csv",
        content = function(file) {
            write.csv(tab5_dataoutput(), file, row.names=FALSE)
        }
    )

    ##########################
    ##End of hourly data tab##
    ##########################
    
})
