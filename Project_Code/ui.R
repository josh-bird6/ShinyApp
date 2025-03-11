####################
####################
####################
###User interface###
####################
####################
####################


######################
##Loading source files
######################

##First the data import and wrangling scripts
#NOTE: THESE TAKE A WHILE SO JUST RUN FOR THE ONE TIME

# source(here::here('StatisticalcomputingProject_Code/Setupscript.R'))
# source(here::here('Statisticalcomputingproject_Code/Datawrangling.R'))

#################
##Beginning of UI
#################

fluidPage(
    style = "width: 100%; height: 100%; max-width: 1200px;",
    tags$head(
        tags$style(
            type = "text/css",
            ".shiny-output-error { visibility: hidden; }",
            ".shiny-output-error:before { visibility: hidden; }"
        ),
        
        #Found a really neat widget which ensures that the iFrame is automatically resized to the content size
        tags$head(
            tags$script(src="https://cdnjs.cloudflare.com/ajax/libs/iframe-resizer/3.5.16/iframeResizer.contentWindow.min.js",
                        type="text/javascript")
        ),
        
       #The following chunk of code does two things:
       # 1. Paints the ribbon that contains the tab headers white
       # 2. Highlights the header of the active tab in blue
       
       tags$style(
           HTML("
                .tabbable > .nav > li > a {background-color: #def0ff; color: #000000;}
                .tabbable > .nav > li[class = active] > a {background-color: #0072B2;color: #FFFFFF;}
                ")
       )
        
    ),
    
    #Adding title panel title
    titlePanel(title=div(h1("Air pollution in Czechia"),
                         style = "height:96px;"),
               windowTitle="Air pollution in Czechia"),
    
    
##Separating into discrete sections

tabsetPanel(
    id = "Panels",
    
    ##########
    ##Home tab
    ##########
    
    tabPanel(
        "Introduction",
        icon = shiny::icon("info-circle"),
        style = "float: top; height: 95%; width: 95%;
        background-color: #FFFFFF; border: 0px solid #FFFFFF;",
        
        column(2),
        column(8,
               p(
                   br(),
                   "This dashboard provides a detailed breakdown of air pollution recorded in Czechia between 1 January 2013 and 31 December 2018. You can explore these data using the following pages:"
                   ),
               tags$ul(tags$li(HTML(paste0("<b>Time series data</b>:"),  
                               ("compare air pollution data over the entire time series and between stations.")))),
               tags$ul(tags$li(HTML(paste0("<b>Daily data by day of year</b>:"),
                               ("compare air pollution data by day of the year, and between stations.")))),
               tags$ul(tags$li(HTML(paste0("<b>Daily data by day and hour of week</b>:"),
                               ("compare air pollution data by day and hour of the week, and between stations.")))),
               tags$ul(tags$li(HTML(paste0("<b>Hourly data</b>:"),
                               ("compare air pollution data by hour of the day, and between stations.")))),
               bs_accordion(id = "dashboard_introductory_text") %>%
                   bs_set_opts(panel_type = "primary") %>%
                   bs_append(
                       title = tags$u("Technical information"),
                       content =
                           p(
                               "Data is sourced from the",
                               tags$a(href = "https://www.eea.europa.eu/en/topics/in-depth/air-pollution", "European Environmental Agency,"),
                               " and information on individual pollutants, as well as the legal limits under EU law, can be found below:",
                               tags$ul(
                                   tags$li(HTML(
                                       paste0("<b>Fine particulates (PM2.5)</b>")
                                   ), "Yearly average of at most 20", HTML(paste0(
                                       "<em>µg/m", tags$sup("3"), ".</em>"
                                   ))),
                                   tags$li(
                                       HTML(paste0("<b>Particulates (PM10)</b>")),
                                       "Daily average exceeding 50",
                                       HTML(paste0("<em>µg/m", tags$sup("3"), "</em>")),
                                       "observed on at most 35 days a year, and yearly average of at most 40",
                                       HTML(paste0("<em>µg/m", tags$sup("3"), ".</em>"))
                                   ),
                                   tags$li(
                                       HTML(paste0(
                                           "<b>Sulfur dioxide (SO", tags$sub("2"), ")</b>"
                                       )),
                                       "Hourly concentration exceeding 350",
                                       HTML(paste0("<em>µg/m", tags$sup("3"), "</em>")),
                                       "for at most 24 hours per year, and average daily concentration exceeding 125",
                                       HTML(paste0("<em>µg/m", tags$sup("3"), "</em>")),
                                       "on at most 3 days per year."
                                   ),
                                   tags$li(
                                       HTML(paste0(
                                           "<b>Nitrogen dioxide (NO", tags$sub("2"), ")</b>"
                                       )),
                                       "Hourly concentration exceeding 200",
                                       HTML(paste0("<em>µg/m", tags$sup("3"), "</em>")),
                                       "for at most 18 hours per year, and average yearly concentration of at most 40",
                                       HTML(paste0("<em>µg/m", tags$sup("3"), ".</em>"))
                                   )
                               ),
                               br(),
                               
                               HTML(paste0("<b>NOTE</b>", ": Data is not available for every single pollutant at every single station on every single date.")),
                               br(),
                               br(),
                               "For more information on EU air quality standards, plesae visit the relevant ",
                               tags$a(href = "https://environment.ec.europa.eu/topics/air/air-quality_en", "European Commission "),
                               "webpage."
                           )
                   ),
               p(
                   "If you experience any problems using this dashboard, please contact",
                   HTML(
                       paste0(
                           '<b> <a href="mailto:2874884b@student.gla.ac.uk">Joshua Bird</a></b>.'
                       )
                   )
               )
        )
    ), 
    
    ##########################
    ##TAB 2: Time series tab##
    ##########################
    
    tabPanel(
        "Time series data",
        icon = icon("line-chart"),
        style = "height: 95%; width: 95%; background-color: #FFFFFF;
        border: 0px solid #FFFFFF;",
        
        h3("Time series data"),
        
        p(
            h4(
                "Visualise air pollution data over the entire time series, and make comparisons between stations. "
            )
            
        ),
        bs_accordion(id = "timeseries_data_text") %>%
            bs_set_opts(panel_type = "primary") %>%
            bs_append(
                title = tags$u("Data selection"),
                content = p(
                    "The chart can be modified using the drop down boxes in the following order:",
                    tags$ul(
                        tags$li(HTML(
                            paste0(
                                "First select a Pollutant (1 selection max). Options include: Fine particulates (PM2.5); Particulates (PM10); Sulfur dioxide (SO",
                                tags$sub("2"),
                                "); Nitrogen dioxide (NO",
                                tags$sub("2"),
                                ")."
                            )
                        )),
                        tags$li(
                            "This selection will then produce a list of stations, arranged alphabetically, which have recorded data for this particular pollutant (3 selections max)."
                        ),
                        tags$li(HTML(
                            paste0(
                                "Finally, select the metric of interest: either the raw unaggregated data, or an aggregation (daily average or daily max concentration). All selections are measured in µg/m",
                                tags$sup("3"),
                                "."
                            )
                        )),
                    ),
                    "Making these selections will produce a plot visualising the options selected, as well as a map underneath showing where each of these stations is located. Where applicable, pollution thresholds are denoted by a horizontal line and in the table at the bottom (see Table functions below). More information on these thresholds can be found on the", actionLink("link_to_home", "introduction")," page.",
                    br(),
                    br(),
                    HTML(
                        paste0(
                            "<b>NOTE</b>",
                            ": Data is not available for every single pollutant at every single station on every single date. This means that each pollutant will produce a different list of stations to choose from."
                        )
                    ),
                    br(),
                    br(),
                    "To download your data selection as a CSV file, use the
                  'Download data' button under the drop down boxes."
                )
            ) %>%
            bs_append(
                title = tags$u("Table functions"),
                content = p(
                    HTML(
                        "To view your data selection in a table, use the
                            'Show/hide table'  button at the
                            bottom of the page."
                    ),
                    tags$ul(
                        tags$li(
                            tags$b("Show entries"),
                            " - change the number of rows shown
                            in the table using the drop-down box."
                        ),
                        tags$li(
                            tags$b("Search"),
                            " - enter text to search data for a specific word or
                            numerical value. Some selections meet the criteria for a given pollution threshold. To filter for these observations, type Yes in the search bar."
                        ),
                        tags$li(
                            icon("sort", lib = "glyphicon"),
                            tags$b("Sort"),
                            " - click to sort the table in ascending or
                            descending order based on the values in a column."
                        ),
                        tags$li(
                            tags$b("Page controls"),
                            " - switch to specific page of data
                            within the table."
                        )
                    )
                )
            ),
        p(""),
        
        
        wellPanel(
            tags$style(
                ".well { background-color: #FFFFFF;
        border: 0px solid #336699; }"
            ),

            #Insert the reactive filters.
            #We have three filters in this tab
            # 1 - Pollutant
            # 2 - Station name
            # 3 - Metric of interest
            
            #The station names are dependent on the pollutant input (see server tab)

           column(
                4,
                uiOutput("Pollutant")
            ),
            
            column(
                4,
                uiOutput("Station_Name")
            ),
            
            column(
                4,
                pickerInput(
                    inputId = "Category",
                    label = "Metric",
                    choices = categories_combined,
                    multiple = TRUE,
                    selected = NULL,
                    options = list(
                        size = 10,
                        "max-options" = 1
                    )
                )
            )
            
            
        ), 
        
        #Creating a download button
        
        downloadButton(outputId = "download_timeseries", 
                       label = "Download data", 
                       class = "aggregatedbutton"),
        
        tags$head(
            tags$style(".aggregatedbutton { background-color: 
                   #0072B2; } 
                   .aggregatedbutton { color: #FFFFFF; }")
        ),
        
        #In the main panel of the tab.... 
        
        mainPanel(
            width = 12,
            
            #Inserting time series plot
            
            plotOutput("timeseries_plot",
                       width = "1090px",
                       height = "500px") %>%
                
                #Adding a loading spinner to let a user know that computations are taking place
                
                shinycssloaders::withSpinner(),
            br(),
            
            #Inserting map
            
            leafletOutput(outputId = 'map_tab2'),
            br(),
            
            #Inserting table, with option to collapse
            
            HTML(
                "<button data-toggle = 'collapse' href = '#tab2'
                   class = 'btn btn-primary' id = 'tab2_link'>
                   <strong> Show/hide table </strong></button>"
            ),
            HTML("<div id = 'tab2' class = 'collapse'>"),
            br(),
            dataTableOutput("timeseries_table"),
            HTML("</div>"),
            br(),
            br()
        )
        ),
    
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

    tabPanel(
        "Daily data - by day of year",
        icon = icon("calendar"),
        style = "height: 95%; width: 95%; background-color: #FFFFFF;
        border: 0px solid #FFFFFF;",

        h3("Data by day of the year"),

        p(
            h4("Visualise air pollution data by day of the year, and make comparisons between stations. ")

        ),
        bs_accordion(id = "yearly_data_text") %>%
            bs_set_opts(panel_type = "primary") %>%
            bs_append(title = tags$u("Data selection"),
                      content = p(
                          "The chart can be modified using the drop down boxes in the following order:",
                          tags$ul(
                              tags$li(HTML(paste0("First select a Pollutant (1 selection max). Options include: Fine particulates (PM2.5); Particulates (PM10); Sulfur dioxide (SO", tags$sub("2"), "); Nitrogen dioxide (NO", tags$sub("2"),")."))),
                              tags$li("This selection will then produce a list of stations, arranged alphabetically, which have recorded data for this particular pollutant (3 selections max)."),
                              tags$li(HTML(
                                  paste0(
                                      "Finally, select the metric of interest: either the raw unaggregated data, or an aggregation (daily average or daily max concentration). All selections are measured in µg/m",
                                      tags$sup("3"),
                                      "."
                                  )
                              )),
                          ),
                          "Making these selections will produce a plot visualising the options selected, as well as a map underneath showing where each of these stations is located. Where applicable, pollution thresholds are denoted by a horizontal line and in the table at the bottom (see Table functions below). More information on these thresholds can be found on the", actionLink("link_to_home2", "introduction")," page.",
                          br(), br(),
                          HTML(paste0("<b>NOTE</b>", ": Data is not available for every single pollutant at every single station on every single date. This means that each pollutant will produce a different list of stations to choose from.")),
                          br(), br(),
                          "To download your data selection as a CSV file, use the
                  'Download data' button under the drop down boxes."
                      ))%>%
            bs_append(title = tags$u("Table functions"),
                      content = p(HTML("To view
        your data selection in a table, use the
                            'Show/hide table'  button at the
                            bottom of the page."),
                                  tags$ul(
                                      tags$li(tags$b("Show entries"), " - change the number of rows shown
                            in the table using the drop-down box."),
                                      tags$li(tags$b("Search"), " - enter text to search data for a specific word or
                            numerical value. Some selections meet the criteria for a given pollution threshold. To filter for these observations, type Yes in the search bar."),
                                      tags$li(icon("sort", lib = "glyphicon"),
                                              tags$b("Sort"), " - click to sort the table in ascending or
                            descending order based on the values in a column."),
                                      tags$li(tags$b("Page controls"), " - switch to specific page of data
                            within the table.")
                                  )
                      )),
        p(""),

        wellPanel(
            tags$style(
                ".well { background-color: #FFFFFF;
        border: 0px solid #336699; }"
            ),

            #Insert the reactive filters.
            #We have three filters in this tab
            # 1 - Pollutant
            # 2 - Station name
            # 3 - Category 

            #The station names are dependent on the pollutant input (see server tab)

            column(
                4,
                uiOutput("Pollutant_yearly")
            ),

            column(
                4,
                uiOutput("Station_Name_yearly")
            ),
            
            column(
                4,
                pickerInput(
                    inputId = "Category_yearly",
                    label = "Metric",
                    choices = categories_combined,
                    multiple = TRUE,
                    selected = NULL,
                    options = list(
                        size = 10,
                        "max-options" = 1
                    )
                )
            )


        ),


        downloadButton(outputId = "download_yearly",
                       label = "Download data",
                       class = "yearlydatabutton"),

        tags$head(
            tags$style(".yearlydatabutton { background-color:
                   #0072B2; }
                   .yearlydatabutton { color: #FFFFFF; }")
        ),

        #In the main panel of the tab, insert the plot

        mainPanel(
            width = 12,
            plotOutput("yearly_plot",
                       width = "1090px",
                       height = "500px") %>%

                #Adding a loading spinner to let a user know that computations are taking place

                shinycssloaders::withSpinner(),
            br(),
            
            #Inserting map
            
            leafletOutput(outputId = 'map_tab3'),
            br(),

            #Inserting table, with option to collapse

            HTML(
                "<button data-toggle = 'collapse' href = '#yearlydata'
                   class = 'btn btn-primary' id = 'yearlydata_link'>
                   <strong> Show/hide table </strong></button>"
            ),
            HTML("<div id = 'yearlydata' class = 'collapse'>"),
            br(),
            dataTableOutput("yearlydata_table"),
            HTML("</div>"),
            br(),
            br()
        )
    ),

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
    
    tabPanel(
        "Daily data - by day and hour of week",
        icon = icon("hourglass"),
        style = "height: 95%; width: 95%; background-color: #FFFFFF;
        border: 0px solid #FFFFFF;",
        
        h3("Data by day and hour of the week"),
        
        p(
            h4("Visualise air pollution data by day and hour of the week (hour 0 is midnight on Sunday), and make comparisons between stations. ")
            
        ),
        bs_accordion(id = "daily_data_text") %>%
            bs_set_opts(panel_type = "primary") %>%
            bs_append(title = tags$u("Data selection"),
                      content = p(
                          "The chart can be modified using the drop down boxes in the following order:",
                          tags$ul(
                              tags$li(HTML(paste0("First select a Pollutant (1 selection max). Options include: Fine particulates (PM2.5); Particulates (PM10); Sulfur dioxide (SO", tags$sub("2"), "); Nitrogen dioxide (NO", tags$sub("2"),")."))),
                              tags$li("This selection will then produce a list of stations, arranged alphabetically, which have recorded data for this particular pollutant (3 selections max)."),
                              tags$li(HTML(paste0("Finally, select the metric of interest: either the raw unaggregated data, or an aggregation (hourly/daily average or hourly/daily max concentration). All selections are measured in µg/m",
                                                  tags$sup("3"),
                                                  "."))),
                          ),
                          "Making these selections will produce a plot visualising the options selected, as well as a map underneath showing where each of these stations is located. Where applicable, pollution thresholds are denoted by a horizontal line and in the table at the bottom (see Table functions below). More information on these thresholds can be found on the", actionLink("link_to_home3", "introduction")," page.",
                          br(), br(),
                          HTML(paste0("<b>NOTE</b>", ": Data is not available for every single pollutant at every single station on every single date. This means that each pollutant will produce a different list of stations to choose from. Also, some stations only record certain pollutants once a day (usually 00.00). These selections will produce a plot with only one time of day on the x-axis.")),
                          br(), br(),
                          "To download your data selection as a CSV file, use the
                  'Download data' button under the drop down boxes."
                      ))%>%
            bs_append(title = tags$u("Table functions"),
                      content = p(HTML("To view
        your data selection in a table, use the
                            'Show/hide table' button at the
                            bottom of the page."),
                                  tags$ul(
                                      tags$li(tags$b("Show entries"), " - change the number of rows shown
                            in the table using the drop-down box."),
                                      tags$li(tags$b("Search"), " - enter text to search data for a specific word or
                            numerical value. Some selections meet the criteria for a given pollution threshold. To filter for these observations, type Yes in the search bar"),
                                      tags$li(icon("sort", lib = "glyphicon"),
                                              tags$b("Sort"), " - click to sort the table in ascending or
                            descending order based on the values in a column."),
                                      tags$li(tags$b("Page controls"), " - switch to specific page of data
                            within the table.")
                                  )
                      )),
        p(""),
        
        wellPanel(
            tags$style(
                ".well { background-color: #FFFFFF;
        border: 0px solid #336699; }"
            ),
            
            #Insert the reactive filters.
            #We have three filters in this tab
            # 1 - Pollutant
            # 2 - Station name
            # 3 - Category - NOTE THIS REQUIRES A DIFFERENT CATEGORY THAN THE PREVIOUS TWO TABS BECAUSE IT IS VISUALISING DATA BY WEEK AND HOUR
            
            #The station names are dependent on the pollutant input (see server tab)
            
            column(
                4,
                uiOutput("Pollutant_daily")
            ),
            
            column(
                4,
                uiOutput("Station_Name_daily")
            ),
            
            column(
                4,
                pickerInput(
                    inputId = "Category_daily",
                    label = "Metric",
                    choices = categories_combined_tab5,
                    multiple = TRUE,
                    selected = NULL,
                    options = list(
                        size = 10,
                        "max-options" = 1
                    )
                )
            )
            
            
        ),
        
        
        downloadButton(outputId = "download_daily",
                       label = "Download data",
                       class = "dailydatabutton"),
        
        tags$head(
            tags$style(".dailydatabutton { background-color:
                   #0072B2; }
                   .dailydatabutton { color: #FFFFFF; }")
        ),
        
        #In the main panel of the tab, insert the plot
        
        mainPanel(
            width = 12,
            plotOutput("daily_plot",
                       width = "1090px",
                       height = "500px") %>%
                
                #Adding a loading spinner to let a user know that computations are taking place
                
                shinycssloaders::withSpinner(),
            br(),
            
            #Inserting map
            
            leafletOutput(outputId = 'map_tab4'),
            br(),
            
            #Inserting table, with option to collapse
            
            HTML(
                "<button data-toggle = 'collapse' href = '#dailydata'
                   class = 'btn btn-primary' id = 'dailydata_link'>
                   <strong> Show/hide table </strong></button>"
            ),
            HTML("<div id = 'dailydata' class = 'collapse'>"),
            br(),
            dataTableOutput("dailydata_table"),
            HTML("</div>"),
            br(),
            br()
        )
    ),
    
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
    
    tabPanel(
        "Hourly data",
        icon = icon("clock"),
        style = "height: 95%; width: 95%; background-color: #FFFFFF;
        border: 0px solid #FFFFFF;",

        h3("Data by hour of the day"),

        p(
            h4("Visualise air pollution data by hour of the day, and make comparisons between stations. ")

        ),
        bs_accordion(id = "hourly_data_text") %>%
            bs_set_opts(panel_type = "primary") %>%
            bs_append(title = tags$u("Data selection"),
                      content = p(
                          "The chart can be modified using the drop down boxes in the following order:",
                          tags$ul(
                              tags$li(HTML(paste0("First select a Pollutant (1 selection max). Options include: Fine particulates (PM2.5); Particulates (PM10); Sulfur dioxide (SO", tags$sub("2"), "); Nitrogen dioxide (NO", tags$sub("2"),")."))),
                              tags$li("This selection will then produce a list of stations, arranged alphabetically, which have recorded data for this particular pollutant (3 selections max)."),
                              tags$li(HTML(paste0("Finally, select the metric of interest: either the raw unaggregated data, or an aggregation (hourly average or hourly max concentration). All selections are measured in µg/m",
                                                  tags$sup("3"),
                                                  "."))),
                          ),
                          "Making these selections will produce a plot visualising the options selected, as well as a map underneath showing where each of these stations is located. Where applicable, pollution thresholds are denoted by a horizontal line and in the table at the bottom (see Table functions below). More information on these thresholds can be found on the", actionLink("link_to_home4", "introduction")," page.",
                          br(),
                          br(),
                          HTML(paste0("<b>NOTE</b>", ": Data is not available for every single pollutant at every single station on every single date. This means that each pollutant will produce a different list of stations to choose from. Also, some stations only record certain pollutants once a day (usually 00.00). These selections will produce a plot with only one time of day on the x-axis.")),
                          br(), br(),
                          "To download your data selection as a CSV file, use the
                  'Download data' button under the drop down boxes."
                      ))%>%
            bs_append(title = tags$u("Table functions"),
                      content = p(HTML("To view
        your data selection in a table, use the
                            'Show/hide table' button at the
                            bottom of the page."),
                                  tags$ul(
                                      tags$li(tags$b("Show entries"), " - change the number of rows shown
                            in the table using the drop-down box."),
                                      tags$li(tags$b("Search"), " - enter text to search data for a specific word or
                            numerical value. Some selections meet the criteria for a given pollution threshold. To filter for these observations, type Yes in the search bar."),
                                      tags$li(icon("sort", lib = "glyphicon"),
                                              tags$b("Sort"), " - click to sort the table in ascending or
                            descending order based on the values in a column."),
                                      tags$li(tags$b("Page controls"), " - switch to specific page of data
                            within the table.")
                                  )
                      )),
        p(""),

        wellPanel(
            tags$style(
                ".well { background-color: #FFFFFF;
        border: 0px solid #336699; }"
            ),

            #Insert the reactive filters.
            #We have three filters in this tab
            # 1 - Pollutant
            # 2 - Station name
            # 3 - Category - NOTE THIS REQUIRES A DIFFERENT CATEGORY THAN THE PREVIOUS TABS BECAUSE IT IS VISUALISING DATA BY HOUR RATHER THAN DAY

            #The station names are dependent on the pollutant input (see server tab)

            column(
                4,
                uiOutput("Pollutant_hourly")
            ),

            column(
                4,
                uiOutput("Station_Name_hourly")
            ),

            column(
                4,
                pickerInput(
                    inputId = "Category_hourly",
                    label = "Metric",
                    choices = categories_combined_hourly,
                    multiple = TRUE,
                    selected = NULL,
                    options = list(
                        size = 10,
                        "max-options" = 1
                    )
                )
            )


        ),


        downloadButton(outputId = "download_hourly",
                       label = "Download data",
                       class = "hourlydatabutton"),

        tags$head(
            tags$style(".hourlydatabutton { background-color:
                   #0072B2; }
                   .hourlydatabutton { color: #FFFFFF; }")
        ),

        #In the main panel of the tab, insert the plot

        mainPanel(
            width = 12,
            plotOutput("hourly_plot",
                       width = "1090px",
                       height = "500px") %>%

                #Adding a loading spinner to let a user know that computations are taking place

                shinycssloaders::withSpinner(),
            br(),
            
            #Inserting map
            
            leafletOutput(outputId = 'map_tab5'),
            br(),

            #Inserting table, with option to collapse

            HTML(
                "<button data-toggle = 'collapse' href = '#hourlydata'
                   class = 'btn btn-primary' id = 'hourlydata_link'>
                   <strong> Show/hide table </strong></button>"
            ),
            HTML("<div id = 'hourlydata' class = 'collapse'>"),
            br(),
            dataTableOutput("hourlydata_table"),
            HTML("</div>"),
            br(),
            br()
        )
    )
    
    ##########################
    ##End of hourly data tab##
    ##########################
    
        )
)
