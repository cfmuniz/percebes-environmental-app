library(shiny)
library(tidyverse)
library(readr)

# Import data
df <- read_csv("full_dataset.csv")

panel_width = 3

sites <- c("A Coruña", "Camelle", "Bueu", "Cangas", "Baiona", "A Guarda")


# Define UI

ui <- fluidPage(
    
    sidebarLayout(
        sidebarPanel(
            
            h3("Description"),
            
            p("SST, air temperature, and chlorophyll data are shown for six
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
            
            helpText("Note: number of days used to calculate an average sliding window"),
            
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
                                 plotOutput("chlPlot")),
                        
                        tabPanel("Density plots",
                                 plotOutput("sstDensity"),
                                 plotOutput("airtempDensity"),
                                 plotOutput("chlDensity")),
                        
                        tabPanel("Boxplots",
                                 
                                 radioButtons(inputId = "variable",
                                              label = "Select data to plot",
                                              choices = c("SST", "Air temperature", "Chlorophyll"),
                                              inline = TRUE),
                                 
                                 p(em("Note: Boxplots include the entire period by default. Data
                                      for June 2017 is not presented for the entire month.")),
                                 
                                 plotOutput("boxPlot",
                                            width = "100%",
                                            height = "800px")),
                        tabPanel("About",
                                 h3("About"),
                                 p("Sea surface temperature (SST), air temperature, and chlorophyll
                                   data are available for 6 sites along the Galician coast (Spain)."),
                                 p("Data can be visualised as a time series, which allows to select
                                   daily values and/or smoothed values over an averaged time window."),
                                 p("The number of days to calculate the smoothing window can be selected
                                   by the user."),
                                 p("A date range can only be selected to visualise data as a time series
                                   or density plot."),
                                 p("Data were processed within the framework of project ",
                                   a("PERCEBES", href="https://www.unioviedo.es/percebes/")),
                                 br(),
                                 br(),
                                 h4("Data sources"),
                                 p("- Sea surface temperature (SST) data were obtained from L4 OSTIA
                                   model of the UK Met Office: ",
                                   a("https://coastwatch.pfeg.noaa.gov/erddap/griddap/jplUKMO_OSTIAv20.html",
                                     href = "https://coastwatch.pfeg.noaa.gov/erddap/griddap/jplUKMO_OSTIAv20.html")),
                                 p("- Air temperature data were obtained from the ERA5-Land dataset
                                   from Copernicus: ",
                                   a("https://cds.climate.copernicus.eu/cdsapp#!/dataset/reanalysis-era5-land?tab=form",
                                     href = "https://cds.climate.copernicus.eu/cdsapp#!/dataset/reanalysis-era5-land?tab=form")),
                                 p("- Chlorophyll data were obtained from Ifremer: ",
                                   a("http://tds1.ifremer.fr/thredds/dodsC/MARC-IBI-OC5_L4-OBS_FULL_TIME_SERIE.html",
                                     href = "http://tds1.ifremer.fr/thredds/dodsC/MARC-IBI-OC5_L4-OBS_FULL_TIME_SERIE.html")),
                                 br(),
                                 br(),
                                 h4("Contact"),
                                 p("App developed by Carlota Fernández Muñiz (carlota.fernandezmuniz [at] gmail.com)."),
                                 p("Code available on", a("GitHub", href = "https://github.com/cfmuniz/percebes-environmental-app"), ".")
                                 )),
            
            width = 12 - panel_width
            
            
                        )
        )
)




# Define server


server <- function(input, output, session){
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
            ggtitle("Air temperature (ºC)") +
            ylab("Air temperature (ºC)") +
            ylim(3.5, 33.5)
        
        plot_select_data(airtemp_plot, airtemp_df)
        
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
    
    
    
    
    # Density plots ===========================================================
    
    
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
    
    
    # Air temperature density plot --------------------------------------------------------
    
    output$airtempDensity <- renderPlot({
        
        airtemp_df <- rval_df() %>% plot_filter("airtemp")
        
        base_density +
            geom_density(data = airtemp_df, density_mapping, size = 1, alpha = 0.2) +
            ggtitle("Air temperature (ºC)") +
            xlab("Air temperature (ºC)")
        
    })
    
    
    # Chlorophyll density plot --------------------------------------------------------
    
    output$chlDensity <- renderPlot({
        
        chl_df <- rval_df() %>% plot_filter("chl")
        
        base_density +
            geom_density(data = chl_df, density_mapping, size = 1, alpha = 0.2) +
            ggtitle("Chlorophyll") +
            xlab(expression(paste("Chlorophyll (microgram L" ^ -1, ")")))
        
    })
    
    
    
    # Boxplots ================================================================
    
    
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
    
    
    output$boxPlot <- renderPlot({
        
        if(input$variable == "SST"){
            sst_box <-  rval_df() %>% boxplot_filter("sst")
            
            box_plot <- base_boxplot +
                geom_boxplot(data = sst_box, aes(x = month, y = values, fill = year)) +
                ylab("SST (ºC)")
        }
        
        if(input$variable == "Air temperature"){
            airtemp_box <-  rval_df() %>% boxplot_filter("airtemp")
            
            box_plot <- base_boxplot +
                geom_boxplot(data = airtemp_box, aes(x = month, y = values, fill = year)) +
                ylab("Air temperature (ºC)")
        }
        
        if(input$variable == "Chlorophyll"){
            chl_box <-  rval_df() %>% boxplot_filter("chl")
            
            box_plot <- base_boxplot +
                geom_boxplot(data = chl_box, aes(x = month, y = values, fill = year)) +
                ylab(expression(paste("Chlorophyll (microgram L" ^ -1, ")")))
        }
        
        box_plot
    })
}



# Run the app

shinyApp(ui = ui, server = server, options = list(port = 3000))
