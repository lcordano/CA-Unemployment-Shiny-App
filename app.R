##
# STA 141 B Final Project
# California Unemployment Statistics
#
#
#

library(shiny)
library(shinythemes)
library(sf)
library(widgetframe)
library(colormap)
library(plotly)
library(tidyverse)
library("RSocrata")

boundaries <- st_read(
    "CA_Counties/CA_Counties_TIGER2016.shp") %>% 
    mutate(area_name = as.character(NAMELSAD)) %>% select(area_name, geometry)

unemployment_db <- dbConnect(RSQLite::SQLite(), dbname = "CA_unemployment.sqlite")

county_data <- unemployment_db %>% tbl("unemployment") %>% collect() %>% 
    left_join(boundaries, by="area_name")

ui <- fluidPage(
    
    navbarPage("California Unemployment Statistics", theme=shinytheme("lumen"),
               tabPanel("Interactive Maps", fluid=TRUE,
                        sidebarLayout(
                            sidebarPanel(
                                selectInput("yearSelect", h4("Choose Year"), 
                                        choices = c(sort(unique(as.numeric(county_data$year)))), 
                                        selected = 2020),
                                selectInput("monthSelect", h4("Choose Month"),
                                        choices = c(unique(county_data$month)),
                                        selected = "January")
                            ),
                            mainPanel(
                                plotlyOutput(outputId = "state_plot_percentage", 
                                    width = "1000px",height = "650px"))
                            )
                        )),
               tabPanel("County Plots", fluid=TRUE,
                        sidebarLayout(
                            
                            #Sidebar Panel: the selected county 
                            sidebarPanel(
                                selectInput("county", h4("County"), choices 
                                            = c(sort(unique(county_data$area_name))))),
                            
                            #Main Panel: plot the selected values
                            mainPanel(
                                tabsetPanel(type="tabs",
                                    tabPanel("Annual Plot",
                                             plotOutput(outputId = "county_plot", 
                                                        width = "900px", 
                                                        height = "600px")),
                                    tabPanel("Monthly Plot",
                                             plotOutput(outputId = "county_plot_monthly",
                                                        width = "900px",
                                                        height = "600px"))
                            ))
                        ))
    )
#----------------------------------------------------------------------------------------------------------------------------------------------------------

server <- function(input, output){
    
    newdate_percent <- reactive({
        county_data %>% dplyr::filter(year == input$yearSelect, month == input$monthSelect) %>%
            mutate(unemployment_percent = as.numeric(unemployment_rate) * 100)
    })
    
    map_title <- reactive({
        paste("California Unemployment by County", input$monthSelect, input$yearSelect)
    })
    
    county_grp_year <- reactive({
        county_data %>% dplyr::filter(area_name == input$county) %>% group_by(year) %>%
            summarize(avg_unemployment_percent = mean(as.numeric(unemployment_rate))*100)
    })
    
    county_grp_month <- reactive({
        as.data.frame(county_data %>% 
            dplyr::filter(area_name == input$county) %>% 
            dplyr::filter(year %in% c("2016","2017","2018","2019","2020"))  %>%
            group_by(year, month) %>%
            summarize(unemployment_percent = as.numeric(unemployment_rate)*100))
    })
    
    plot_title_annual <- reactive({
        paste(input$county, "Average Annual Unemployment Percentage")
    })
    
    plot_title_by_month <- reactive({
        paste(input$county, "Unemployment Percentage by Month Over the Past 5 Years")
    })
    
    
    #Assign output$state_plot_percent
    output$state_plot_percentage <- renderPlotly({
        p <- ggplot(data=newdate_percent(), 
                    aes(fill = as.numeric(unemployment_percent),
                        text = paste0("<b>County: </b>", area_name,"<br>",
                                      "<b>Unemployment: </b>", unemployment_percent,"% <br>"), 
                        group = 1, lwd = 0.4)) + 
            geom_sf(size=0.05) + 
            scale_fill_colormap(colormap= colormaps$autumn, reverse=TRUE) +
            labs(title= map_title(), fill="Unemployment Percent") + 
            theme_minimal(base_size = 12, base_family = "Georgia") + 
            theme(axis.ticks = element_blank(), axis.text = element_blank(), 
                  plot.background = element_blank(),
                  panel.grid.major = element_line(color="transparent"))
        
        ggplotly(p, tooltip = "text") %>% 
            config(displayModeBar = FALSE) %>%
            layout(hoverlabel = list(bgcolor="white",
                                     font = list(family="Georgia")))
        
    })
    
    #Assign output$county_plot with renderPlot object
    output$county_plot <- renderPlot({
        
        #plot the graph
        
        ggplot(data=county_grp_year(), aes(x=as.numeric(year), y=avg_unemployment_percent)) + 
            geom_bar(stat="identity") + ggtitle(plot_title_annual()) + 
            xlab("Year") + ylab("Unemployment %") +
            theme_minimal(base_size = 14) +
            theme(axis.text.x = element_text(size=12), 
                  plot.background = element_blank())
        
        
    })
    
    output$county_plot_monthly <- renderCachedPlot({
        
        
        ggplot(data=county_grp_month(), 
               aes(x=factor(month, levels = month.name, ordered=TRUE), y=unemployment_percent, 
                   group=year, color=factor(year))) +
            geom_line(size=0.5) +
            ggtitle(plot_title_by_month()) + xlab("Months") + ylab("Unemployment %") +
            labs(fill="Year") +
            theme_minimal(base_size = 14) +
            theme(axis.text.x = element_text(angle = 45, size=12), 
                 plot.background = element_blank())
    }, cacheKeyExpr = { input$county })
    
} 


shinyApp(ui = ui, server = server)