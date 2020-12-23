library(shiny)
library(DT)
library(tidyverse)
library(readr)
library(leaflet)

# Import data
df <- read_csv("full_dataset.csv")  # SST, air temperature, chlorophyll, upwelling
ibutton_df <- read_csv("ibutton_dataset.csv")   # iButton in situ temperature

coords_df <- read_delim("map_coords.txt", "\t", escape_double = FALSE, trim_ws = TRUE)


# Round to 3 decimals
df$values <- round(df$values, 3)
ibutton_df$mean <- round(ibutton_df$mean, 3)
ibutton_df$sd <- round(ibutton_df$sd, 3)

panel_width <- 3

# sites <- c("A Coruña", "Camelle", "Bueu", "Cangas", "Baiona", "A Guarda")
sites <- c("L1", "L2", "L3", "L4", "L5", "L6")


# Define user interface

ui <- navbarPage("PERCEBES app",
                 
                 tabPanel("Satellite data",
                            sidebarLayout(
                                sidebarPanel(

                                    h3("Description"),

                                    helpText("SST, air temperature, and chlorophyll data are shown for six
                                    sites along the Galician coast (displayed north to south). Data
                                    can be visualised as a time series, density plot, or boxplot."),

                                    checkboxGroupInput(inputId = "site",
                                                       label = "Select sites",
                                                       choices = sites,
                                                       selected = unique(df$site)
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

                                    width = panel_width


                                ),

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

                                   helpText("In situ temperature data measured with iButtons. Water/air 
                                     temperatures are based on the sensor being underwater/out of
                                     the water respectively."),
                                   
                                   checkboxGroupInput(inputId = "ibutton_site",
                                                      label = "Select sites",
                                                      choices = unique(ibutton_df$site),
                                                      selected = unique(ibutton_df$site)
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

                                   width = panel_width
                               ),

                               mainPanel(
                                   plotOutput("ibuttonPlot",
                                              width = "100%",
                                              height = "700px")))),
                 
                 tabPanel("Tables",
                          
                          sidebarLayout(
                              sidebarPanel(
                                  
                                  h3("Description"),
                                  
                                  helpText("Tabular data presented in the graphs."),
                                  
                                  helpText("For the satellite data, the column 'Values' shows the real values at each timestamp.
                                    The column 'Average' represents the rolling average of multiple days as selected
                                    for the smoothing window (around the central value)."),
                                    
                                  helpText("For the in situ data, the average value of one or two sensors is presented as a 'Mean'
                                    value with a standard deviation ('SD'). The 'Average' is calculated the same way as
                                    the satellite data."),
                                  
                                  radioButtons(inputId = "data",
                                               label = "Select data to display",
                                               choices = c("Satellite", "In situ")),
                                  
                                  numericInput(inputId = "table_smoothing",
                                               label = "Select smoothing window",
                                               value = 31,
                                               min = 1,
                                               max = 365,
                                               step = 1),
                                  
                                  downloadButton(outputId = "downloadData",
                                                 label = "Download"),
                                  
                                  helpText("Note: the complete dataset is downloaded for the option selected.
                                           Smoothing window can be selected (values shown in column 'Average'."),
                                  
                                  width = panel_width),
                              
                              mainPanel(
                                  dataTableOutput("dfTable")
                              ))),
                 
                 tabPanel("About",
                          includeMarkdown("app-about.Rmd")))



# Define server


server <- function(input, output, session){

    # Data: SST, air temperature, chl-a, upwelling

    # Order sites geographically

    df$site <- as.factor(df$site)
    df$site <- factor(df$site, levels = sites)


    rval_df <- reactive({
        df %>%
            arrange(variable, site, date) %>%
            group_by(variable, site) %>%
            mutate(average = zoo::rollmean(values, k = input$smoothingvalue,  fill = NA))
    })



    # Line plots ==============================================================


    # Basic plot design:

    base_plot <- ggplot() +
        scale_x_datetime(date_breaks = "2 month",
                         date_minor_breaks = "1 month",
                         date_labels = "%b %Y") +
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
            ylim(11.5, 20)

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
            ggtitle("Upwelling index") +
            ylab(expression(paste("UI"))) +
            ylim(-0.5, 0.5)

        plot_select_data(upw_plot, upw_df)

    })



    # Density plots ===========================================================


    # Basic plot design:

    base_density <- ggplot() +
        labs(fill = "Site", col = "Site") +
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


    ibutton_base_plot <- ggplot() +
        scale_x_datetime(date_breaks = "2 month",
                         date_minor_breaks = "1 month",
                         date_labels = "%b %Y") +
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

        
        # Table downloadable as .csv
        
        output$downloadData <- downloadHandler(
            filename = function() {
                paste(input$data, ".csv", sep = "")
            },
            content = function(file) {
                write.csv(table, file, row.names = FALSE)
            }
        )
        

        # Print table output to screen
        
        table
    },
    
    filter = "top")
    
}



# Run the app

shinyApp(ui = ui, server = server, options = list(port = 3000))
