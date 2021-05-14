library(shiny)
library(DT)
library(tidyverse)
library(readr)
library(leaflet)

# Import data
df <- read_csv("full_dataset.csv")              # SST, air temperature, chlorophyll, upwelling
ibutton_df <- read_csv("ibutton_dataset.csv")   # iButton in situ temperature
coords_df <- read_csv("data_coords.csv")        # coordinates where data were obtained

# Round to 3 decimals
df$values <- round(df$values, 3)
ibutton_df$mean <- round(ibutton_df$mean, 3)
ibutton_df$sd <- round(ibutton_df$sd, 3)

# Define sites
sites <- c("L0 – Cariño", "L1 – A Coruña", "L2 – Camelle", "L3 – Bueu",
           "L4 – Cangas", "L5 – Baiona", "L6 – A Guarda")
df$site <- factor(df$site, levels = sites)

ibutton_sites <- c("L1 – A Coruña", "L4 – Cangas", "L5 – Baiona")
ibutton_df$site <- factor(ibutton_df$site, levels = ibutton_sites)

# Define panel width for UI and main panel
panel_width <- 3



# Define user interface

ui <- navbarPage("PERCEBES app",
                 
                 tabPanel("Satellite data",
                            sidebarLayout(
                                sidebarPanel(

                                    h3("Description"),

                                    helpText("SST, air temperature, chlorophyll, and upwelling index data are shown for six
                                    sites along the Galician coast (L1 to L6). For site L0 only SST data is available.
                                    Sites are displayed north to south, more information can be viewed in the", 
                                    em("Map of sites"), "tab.
                                    Data can be visualised as a time series, density plot, or boxplot."),

                                    checkboxGroupInput(inputId = "site",
                                                       label = "Select sites",
                                                       choices = sites,
                                                       selected = sites
                                    ),
                                    
                                    radioButtons(inputId = "dataset",
                                                 label = "Select data to plot",
                                                 choices = c("Raw data only", "Smoothing only", "Data with smoothing")),
                                    
                                    
                                    numericInput(inputId = "smoothingvalue",
                                                 label = "Select smoothing window",
                                                 value = 31,
                                                 min = 1,
                                                 max = 365,
                                                 step = 1),

                                    helpText("Note: number of days used to calculate an average sliding window
                                             (around the central value)."),

                                    dateRangeInput(inputId = "dates",
                                                   label = "Select date range",
                                                   start = min(df$date),
                                                   end = max(df$date)),

                                    width = panel_width),

                                mainPanel(
                                    tabsetPanel(tabPanel("Time series",
                                                         plotOutput("sstPlot"),
                                                         plotOutput("airtempPlot"),
                                                         plotOutput("sktPlot"),
                                                         plotOutput("chlPlot"),
                                                         plotOutput("upwPlot")),

                                                tabPanel("Density plots",
                                                         plotOutput("sstDensity"),
                                                         plotOutput("airtempDensity"),
                                                         plotOutput("sktDensity"),
                                                         plotOutput("chlDensity"),
                                                         plotOutput("upwDensity")),

                                                tabPanel("Boxplots",
                                                         radioButtons(inputId = "variable",
                                                                      label = "Select data to plot",
                                                                      choices = c("SST", "Air temperature at 2m", "SKT",
                                                                                  "Chlorophyll", "Upwelling Index"),
                                                                      inline = TRUE),
                                                         plotOutput("boxPlot",
                                                                    width = "100%",
                                                                    height = "800px"))),
                                    
                                    width = 12 - panel_width))),
                 
                 tabPanel("In situ data",

                           sidebarLayout(
                               sidebarPanel(

                                   h3("Description"),

                                   helpText(em("In situ"), "temperature data measured with iButtons. Water/air
                                   temperatures are based on the sensor being underwater/out of
                                   the water, respectively. Information about the sites can be viewed in the", 
                                   em("Map of sites"), "tab."),
                                   
                                   checkboxGroupInput(inputId = "ibutton_site",
                                                      label = "Select sites",
                                                      choices = ibutton_sites,
                                                      selected = ibutton_sites
                                   ),

                                   checkboxGroupInput(inputId = "ibutton_temp",
                                         label = "Temperature data: air and/or water",
                                         choices = unique(ibutton_df$temp),
                                         selected = unique(ibutton_df$temp)
                                         ),
                                   
                                   radioButtons(inputId = "ibutton_dataset",
                                                label = "Select data to plot",
                                                choices = c("Raw data only", "Smoothing only", "Data with smoothing")),
                                   
                                   numericInput(inputId = "smoothingvalue",
                                                label = "Select smoothing window",
                                                value = 31,
                                                min = 1,
                                                max = 365,
                                                step = 1),
                                   
                                   helpText("Note: number of days used to calculate an average sliding window
                                             (around the central value)."),

                                   dateRangeInput(inputId = "ibutton_dates",
                                     label = "Select date range",
                                     start = min(ibutton_df$date),
                                     end = max(ibutton_df$date)),

                                   width = panel_width),

                               mainPanel(
                                   plotOutput("ibuttonPlot",
                                              width = "100%",
                                              height = "700px")))),
                 
                 tabPanel("Tables",
                          
                          sidebarLayout(
                              sidebarPanel(
                                  
                                  h3("Description"),
                                  
                                  helpText("Tabular data presented in the graphs."),
                                  
                                  helpText("For the satellite data, the column,", em("Values"), 
                                  "shows the real values at each timestamp. The column", em("Average"),
                                  "represents the rolling average of multiple days as selected
                                  in the smoothing window (around the central value)."),
                                    
                                  helpText("For the", em("in situ"), "data, the average value of one or 
                                  two sensors is presented as", em("Mean"), "value with a 
                                  standard deviation (",em("SD"),"). The", em("Average"),
                                  "is calculated the same way as for the satellite data."),
                                  
                                  radioButtons(inputId = "data",
                                               label = "Select data to display",
                                               choices = c("Satellite", "In situ")),
                                  
                                  numericInput(inputId = "table_smoothing",
                                               label = "Select smoothing window",
                                               value = 31,
                                               min = 1,
                                               max = 365,
                                               step = 1),
                                  
                                  width = panel_width),
                              
                              mainPanel(
                                  dataTableOutput("dfTable")
                              ))),
                 
                 tabPanel("Map of sites",
                          
                          div(class="outer",
                              
                              tags$head(
                                  # Include custom CSS style
                                  includeCSS("styles.css")
                                  ),
                          
                          leafletOutput("map",
                                        width = "100%",
                                        height = "800px"),
                          
                          absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                        draggable = TRUE, top = 80, left = "auto", right = 20, bottom = "auto",
                                        width = 400, height = "auto",
                                        
                                        includeMarkdown("map-description.Rmd"),
                                        ))),
                 
                 tabPanel("About",
                          includeMarkdown("app-about.Rmd")))



# Define server


server <- function(input, output, session){

    # Satellite data: SST, air temperature, chl-a, upwelling
    
    rval_df <- reactive({
        df %>%
            arrange(variable, site, date) %>%
            group_by(variable, site) %>%
            mutate(average = zoo::rollmean(values, k = input$smoothingvalue,  fill = NA))
    })


    # Assign colors to sites:
    
    color_palette_7 <- c(c("L0 – Cariño" = "#702234"),
                         c("L1 – A Coruña" = "#EF476F"),
                         c("L2 – Camelle" = "#7209B7"),
                         c("L3 – Bueu" = "#FBAF69"),
                         c("L4 – Cangas" = "#06D6A0"),
                         c("L5 – Baiona" = "#118AB2"),
                         c("L6 – A Guarda" = "#073B4C"))
    
    color_palette_3 <- c(c("L1 – A Coruña" = "#7209B7"),
                         c("L4 – Cangas" = "#06D6A0"),
                         c("L5 – Baiona" = "#EF476F"))

    # Line plots ==============================================================
    
    # Basic plot design:

    base_plot <- ggplot() +
        scale_x_datetime(date_breaks = "2 month",
                         date_minor_breaks = "1 month",
                         date_labels = "%b %Y") +
        scale_colour_manual(values = color_palette_7) +
        labs(colour = "Site",
             x = NULL) +
        theme_bw()

    plot_filter <- function(df, name) {
        df %>%
            filter(variable == name,
                   site %in% input$site,
                   date >= input$dates[1],
                   date <= input$dates[2])
    }

    plot_select_data <- function(plot, df) {
        if(input$dataset == "Raw data only" | input$dataset == "Data with smoothing"){
            plot <- plot +
                geom_path(data = df,
                          aes(x = date, y = values, col = site),
                          size = 0.7,
                          alpha = 0.6)
        }
        if(input$dataset == "Smoothing only" | input$dataset == "Data with smoothing"){
            plot <- plot +
                geom_path(data = df,
                          aes(x = date, y = average, col = site),
                          size = 1,
                          alpha = 0.6)
        }
        plot
    }


    # Plot SST ----------------------------------------------------------------

    output$sstPlot <- renderPlot({

        sst_df <- rval_df() %>% plot_filter("sst")

        sst_plot <- base_plot +
            ggtitle("SST") +
            ylab("SST (ºC)") +
            ylim(11.5, 20.4)

        plot_select_data(sst_plot, sst_df)

    })



    # Plot air temperature ----------------------------------------------------

    output$airtempPlot <- renderPlot({

        airtemp_df <- rval_df() %>% plot_filter("airtemp")

        airtemp_plot <- base_plot +
            ggtitle("Air temperature at 2m") +
            ylab("Air temperature (ºC)") +
            ylim(3.5, 33.5)

        plot_select_data(airtemp_plot, airtemp_df)

    })



    # Plot SKT ----------------------------------------------------------------
    
    output$sktPlot <- renderPlot({
        
        skt_df <- rval_df() %>% plot_filter("skt")
        
        skt_plot <- base_plot +
            ggtitle("Air temperature (ground level)") +
            ylab("SKT (ºC)") +
            ylim(2.5, 45)
        
        plot_select_data(skt_plot, skt_df)
        
    })
    
    
    
    # Plot chlorophyll --------------------------------------------------------

    output$chlPlot <- renderPlot({

        chl_df <- rval_df() %>% plot_filter("chl")

        chl_plot <- base_plot +
            ggtitle("Chlorophyll") +
            ylab(expression(paste("Chlorophyll (microgram L" ^ -1, ")"))) +
            ylim(0, 30)

        plot_select_data(chl_plot, chl_df)

    })



    # Plot upwelling ----------------------------------------------------------

    output$upwPlot <- renderPlot({

        upw_df <- rval_df() %>% plot_filter("upw")

        upw_plot <- base_plot +
            
            #Line at 0
            geom_hline(aes(yintercept = 0), lty = "longdash", color = "grey60") +
            
            # Labels
            annotate("text", x = as.POSIXct("2017-07-01"), y = 0.5, label = "Upwelling") +
            annotate("text", x = as.POSIXct("2017-07-01"), y = -0.5, label = "Downwelling") +
                      
            ggtitle("Upwelling index") +
            ylab(expression(paste("UI"))) +
            ylim(-0.5, 0.5)

        plot_select_data(upw_plot, upw_df)

    })



    # Density plots ===========================================================


    # Basic plot design:

    base_density <- ggplot() +
        labs(fill = "Site", col = "Site") +
        scale_colour_manual(values = color_palette_7) +
        scale_fill_manual(values = color_palette_7) +
        theme_bw() +
        theme(legend.position = c(1,1),
              legend.justification = c(1,1),
              legend.background = element_rect(color = "black", linetype = "solid"))

    density_mapping <- aes(values, colour = site, fill = site)



    # SST density plot --------------------------------------------------------

    output$sstDensity <- renderPlot({

        sst_df <- rval_df() %>% plot_filter("sst")

        base_density +
            geom_density(data = sst_df, density_mapping, size = 1, alpha = 0.2) +
            ggtitle("SST (ºC)") +
            xlab("SST (ºC)")

    })



    # Air temperature density plot --------------------------------------------

    output$airtempDensity <- renderPlot({

        airtemp_df <- rval_df() %>% plot_filter("airtemp")

        base_density +
            geom_density(data = airtemp_df, density_mapping, size = 1, alpha = 0.2) +
            ggtitle("Air temperature at 2m") +
            xlab("Air temperature (ºC)")

    })

    
    
    # SKT density plot --------------------------------------------------------
    
    output$sktDensity <- renderPlot({
        
        skt_df <- rval_df() %>% plot_filter("skt")
        
        base_density +
            geom_density(data = skt_df, density_mapping, size = 1, alpha = 0.2) +
            ggtitle("Air temperature (ground level)") +
            xlab("SKT (ºC)")
        
    })
    


    # Chlorophyll density plot ------------------------------------------------

    output$chlDensity <- renderPlot({

        chl_df <- rval_df() %>% plot_filter("chl")

        base_density +
            geom_density(data = chl_df, density_mapping, size = 1, alpha = 0.2) +
            ggtitle("Chlorophyll") +
            xlab(expression(paste("Chlorophyll (microgram L" ^ -1, ")")))

    })


    # Upwelling density plot --------------------------------------------------

    output$upwDensity <- renderPlot({

        upw_df <- rval_df() %>% plot_filter("upw")

        base_density +
            geom_density(data = upw_df, density_mapping, size = 1, alpha = 0.2) +
            ggtitle("Upwelling index") +
            xlab("UI")

    })



    # Boxplots ================================================================


    # Basic plot design:

    base_boxplot <- ggplot() +
        facet_grid(site ~ .) +
        scale_fill_viridis_d() +
        theme_bw() +
        labs(fill = "Year",
             x = "Month of the year")

    boxplot_filter <- function(df, name) {
        rval_df() %>%
            filter(variable == name,
                   site %in% input$site) %>%
            mutate(month = format(date, "%m"), year = format(date, "%Y"))
    }

    # Plot SST, air temperature, chlorophyll, or upwelling data, based on option selected:

    output$boxPlot <- renderPlot({

        if(input$variable == "SST"){
            sst_box <-  rval_df() %>% boxplot_filter("sst")

            box_plot <- base_boxplot +
                geom_boxplot(data = sst_box, aes(x = month, y = values, fill = year)) +
                ylab("SST (ºC)")
        }

        if(input$variable == "Air temperature at 2m"){
            airtemp_box <-  rval_df() %>% boxplot_filter("airtemp")

            box_plot <- base_boxplot +
                geom_boxplot(data = airtemp_box, aes(x = month, y = values, fill = year)) +
                ylab("Air temperature (ºC)")
        }
        
        if(input$variable == "SKT"){
            skt_box <-  rval_df() %>% boxplot_filter("skt")
            
            box_plot <- base_boxplot +
                geom_boxplot(data = skt_box, aes(x = month, y = values, fill = year)) +
                ylab("SKT (ºC)")
        }

        if(input$variable == "Chlorophyll"){
            chl_box <-  rval_df() %>% boxplot_filter("chl")

            box_plot <- base_boxplot +
                geom_boxplot(data = chl_box, aes(x = month, y = values, fill = year)) +
                ylab(expression(paste("Chlorophyll (microgram L" ^ -1, ")")))
        }

        if(input$variable == "Upwelling Index"){
            upw_box <-  rval_df() %>% boxplot_filter("upw")

            box_plot <- base_boxplot +
                geom_boxplot(data = upw_box, aes(x = month, y = values, fill = year)) +
                ylab("UI")
        }

        box_plot
    })

    
    
    # iButton data ============================================================
    
    rval_ibutton <- reactive({
        ibutton_df %>%
            arrange(temp, site, date) %>%
            group_by(temp, site) %>%
            mutate(average = zoo::rollmean(mean, k = input$smoothingvalue,  fill = NA))
    })
    
    
    
    # Filter data by temperature measure, site, and date
    
    ibutton_filter <- function(df) {
        df %>%
            filter(temp %in% input$ibutton_temp,
                   site %in% input$ibutton_site,
                   date >= input$ibutton_dates[1],
                   date <= input$ibutton_dates[2])
    }


    # Basic plot design:
    
    ibutton_base_plot <- ggplot() +
        scale_x_datetime(date_breaks = "2 month",
                         date_minor_breaks = "1 month",
                         date_labels = "%b %Y") +
        scale_color_manual(values = color_palette_3) +
        labs(colour = "Site",
             linetype = "Temperature\nmeasure",
             x = NULL) +
        ggtitle("In situ temperature") +
        ylab("Temperature (ºC)") +
        ylim(9.4, 28.6) +
        theme_bw()
    

    plot_select_ibutton <- function(plot, df) {
        if(input$ibutton_dataset == "Raw data only" | input$ibutton_dataset == "Data with smoothing"){
            plot <- plot +
                geom_path(data = df,
                          aes(x = date, y = mean, col = site, linetype = temp),
                          size = 0.7,
                          alpha = 0.6)
        }
        if(input$ibutton_dataset == "Smoothing only" | input$ibutton_dataset == "Data with smoothing"){
            plot <- plot +
                geom_path(data = df,
                          aes(x = date, y = average, col = site, linetype = temp),
                          size = 1,
                          alpha = 0.6)
        }
        plot
    }
    
    output$ibuttonPlot <- renderPlot({
        
        ibutton_data <- rval_ibutton() %>% ibutton_filter()

        plot_select_ibutton(ibutton_base_plot, ibutton_data)
        
    })


    
    # Table ===================================================================
    
    # Function to select dataset to output as table
    
    table_selector <- function(df, variable, values){
        df %>%
            arrange({{variable}}, site, date) %>%
            group_by({{variable}}, site) %>%
            mutate(average = zoo::rollmean({{values}}, k = input$table_smoothing,  fill = NA))
    }

    
    output$dfTable <- renderDataTable({

        if(input$data == "Satellite"){
            
            # Rename variables measured
            
            var_levels <- c("sst" = "SST", "airtemp" = "Air temperature (2m)",
                            "chl" = "Chlorophyll", "upw" = "Upwelling", "skt" = "SKT")
            df$variable <- as.character(var_levels[df$variable])
            df$variable <- as.factor(df$variable)
            

            # Select data
            
            satellite_table <- eventReactive(input$table_smoothing, {
                table_selector(df, variable, values) %>%
                    rename(Date = date, Site = site, "Variable measured" = variable, Values = values, Average = average)
            })
            
            table <- satellite_table()
        }


        if(input$data == "In situ"){
            
            # Rename variables measured
            
            ibutton_var_levels <- c("air" = "Air temperature", "water" = "Water temperature")
            ibutton_df$temp <- as.character(ibutton_var_levels[ibutton_df$temp])
            ibutton_df$temp <- as.factor(ibutton_df$temp)
            ibutton_df$site <- as.factor(ibutton_df$site)
            
            # Re-order columns to match the satellite data
            
            ibutton_df <- ibutton_df[c("date", "site", "temp", "mean", "sd")]
            

            # Select data
            
            ibutton_table <- eventReactive(input$table_smoothing, {
                table_selector(ibutton_df, temp, mean) %>%
                            rename(Date = date, Site = site, "Variable measured" = temp, Mean = mean, SD = sd, Average = average)
            })
            
            table <- ibutton_table()
        }


        # Print table output to screen
        
        table
    },
    
    filter = "top")
    
    
    # Coordinates of sites and environmental data =============================
    
    coord_groups <- group_split(coords_df, variable)

    base_map <- leaflet() %>%
        addTiles() %>%
        setView(-10, 44, zoom = 7)

    map_selector <- function(map = base_map, df, color){
        map %>%
            addRectangles(
                lng1 = df$lng1, lat1 = df$lat1, lng2 = df$lng2, lat2 = df$lat2,
                group = df$site.code,
                label = paste0(df$site.code, " (", df$site, "), ", df$variable),
                labelOptions = labelOptions(textsize = "15px"),
                fillColor = "transparent",
                color = color
            )
    }
    
    
    output$map <- leaflet::renderLeaflet({
        
        map_selector(df = coord_groups[[4]], color = "blue") %>%
            map_selector(df = coord_groups[[1]], color = "red") %>%
            map_selector(df = coord_groups[[2]], color = "black") %>%
            addMarkers(
                lng = coord_groups[[3]]$lng1, lat = coord_groups[[3]]$lat1,
                label = paste0(coord_groups[[3]]$site.code, " (", coord_groups[[3]]$site, "), ", coord_groups[[3]]$variable),
                labelOptions = labelOptions(textsize = "15px")
                )
    })
}



# Run the app

shinyApp(ui = ui, server = server, options = list(port = 3000))
