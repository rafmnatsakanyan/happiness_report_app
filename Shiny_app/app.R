library(RColorBrewer)
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(plyr)
library(shinydashboard)
library(DT)
library(readxl)
library(MASS)
library(ggthemes)
library(gridExtra)
library(rgdal)
library(ggplot2)
library(knitr)
library(dplyr)
library(formatR)
library(sf)
library(raster)
library(jpeg)
library(raster)
library(maps)
library(rnaturalearth)
library(rgeos)
library(classInt)
library(tidyr)
library(plotly)

df <- read_excel("happiness.xls")
colnames(df) <- c('country','year','happiness','gdp', 'soc_support', 'hle',
                  'freedom', 'generosity', 'corruption', 'positive', 'negative', 'conf_gov')
df <- subset(df, year>=2010 & year<=2020)
df <- df %>% 
  group_by(country) %>% 
  mutate_if(is.numeric, ~replace_na(., mean(., na.rm = TRUE)))

world <- ne_countries(returnclass='sf')

df_mean <- df %>%
  group_by(country) %>%
  summarise_at(c('happiness','gdp', 'soc_support', 'hle',
                 'freedom', 'generosity', 'corruption', 'positive', 'negative',
                 'conf_gov'), mean, na.rm = TRUE)

df_mean <- na.omit(df_mean)

world_mean <- world %>%
  left_join(df_mean[,c('country', 'happiness','gdp', 'soc_support', 'hle',
                       'freedom', 'generosity', 'corruption', 'positive', 'negative',
                       'conf_gov')],by = c('name' = 'country'))

world_clean<-subset(world_mean, world_mean$continent!="Antarctica" & 
                      world_mean$continent!="Seven seas (open ocean)")

correlation_matrix <- round(cor(df_mean[2:11], use = "complete.obs"),3)
melted_cormat <- reshape2::melt(correlation_matrix)

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "metadata",
            imageOutput('image')),
                     
    tabItem(tabName = "scatter",
            fluidRow(
              column(width = 3,
                     box(width=35, selectInput(
                       inputId = "first",
                       label="X axis",
                       choices = c("Happiness" = "happiness",
                                   "GDP" = "gdp",
                                   "Social Support" = "soc_support",
                                   "Healthy life expectancy at brith" = "hle",
                                   "Freedom to make choices" = "freedom",
                                   "Generosity" = "generosity",
                                   "Corruption" = "corruption",
                                   "Positivie affect" = "positive",
                                   "Negative affect" = "negative",
                                   "Confidence in government" = "conf_gov"),
                       selected = "gdp")),
                     box(width=35, selectInput(
                       inputId = "second",
                       label="Y axis",
                       choices = c("Happiness" = "happiness",
                                   "GDP" = "gdp",
                                   "Social Support" = "soc_support",
                                   "Healthy life expectancy at brith" = "hle",
                                   "Freedom to make choices" = "freedom",
                                   "Generosity" = "generosity",
                                   "Corruption" = "corruption",
                                   "Positivie affect" = "positive",
                                   "Negative affect" = "negative",
                                   "Confidence in government" = "conf_gov"),
                       selected = "happiness"))),
              column(width = 8,
                      box(width=45, plotOutput("scatterplot"))))),
    tabItem(tabName = "hist",
            fluidRow(
              column(width = 3,
                     box(width=35, selectInput(
                       inputId = "third",
                       label="X axis",
                       choices = c("Happiness" = "happiness",
                                   "GDP" = "gdp",
                                   "Social Support" = "soc_support",
                                   "Healthy life expectancy at brith" = "hle",
                                   "Freedom to make choices" = "freedom",
                                   "Generosity" = "generosity",
                                   "Corruption" = "corruption",
                                   "Positivie affect" = "positive",
                                   "Negative affect" = "negative",
                                   "Confidence in government" = "conf_gov"),
                       selected = "happiness")),
                     sliderInput("range", "Bins:",
                                 min = 1, max = 100,
                                 value = 40)),
                     column(width = 8,
                            box(width=45, plotOutput("hist"))))),
    tabItem(tabName = "heatmap",
            fluidRow(
                     column(width = 8,
                            box(width="100%", plotlyOutput("heatmap"))))),
    tabItem(tabName = "map",
            fluidRow(
              column(width = 3,
                     box(width=35, selectInput(
                       inputId = "forth",
                       label="Indicator",
                       choices = c("Happiness" = "happiness",
                                   "GDP" = "gdp",
                                   "Social Support" = "soc_support",
                                   "Healthy life expectancy at brith" = "hle",
                                   "Freedom to make choices" = "freedom",
                                   "Generosity" = "generosity",
                                   "Corruption" = "corruption",
                                   "Positivie affect" = "positive",
                                   "Negative affect" = "negative",
                                   "Confidence in government" = "conf_gov"),
                       selected = "happiness")),
                     box(width=35, selectInput(
                       inputId = "fifth",
                       label="Year",
                       choices = c("2010" = 2010,
                                   "2011" = 2011,
                                   "2012" = 2012,
                                   "2013" = 2013,
                                   "2014" = 2014,
                                   "2015" = 2015,
                                   "2016" = 2016,
                                   "2017" = 2017,
                                   "2018" = 2018,
                                   "2019" = 2019,
                                   "2020" = 2020),
                       selected = 2014))),
                     column(width = 8,
                            box(width=45, plotOutput("map"))))),
    tabItem(tabName = "boxplot",
            fluidRow(
              column(width = 3,
                     box(width=35, selectInput(
                       inputId = "sixth",
                       label="Y axis",
                       choices = c("Happiness" = "happiness",
                                   "GDP" = "gdp",
                                   "Social Support" = "soc_support",
                                   "Healthy life expectancy at brith" = "hle",
                                   "Freedom to make choices" = "freedom",
                                   "Generosity" = "generosity",
                                   "Corruption" = "corruption",
                                   "Positivie affect" = "positive",
                                   "Negative affect" = "negative",
                                   "Confidence in government" = "conf_gov"),
                       selected = "happiness"))),
                column(width = 8,
                            box(width=45, plotOutput("boxplot")))))
))

ui <- dashboardPage(skin="green",
                    dashboardHeader(title = "World Happiness Report"),
                    dashboardSidebar(
                      sidebarMenu(
                        menuItem("Metadata", tabName = "metadata"),
                        menuItem("Scatter Plot", tabName = "scatter"),
                        menuItem("Histogram", tabName = "hist"),
                        menuItem("Correlation heatmap", tabName = "heatmap"),
                        menuItem("Map", tabName = "map"),
                        menuItem("Boxplot", tabName = "boxplot"),
                        valueBox(
                          value = tags$p("Data", style = "font-size: 50%;"),
                          subtitle =helpText(tags$div(class="header", checked=NA,
                                                      tags$p("The data was taken from"),
                                                      tags$a(href="https://worldhappiness.report/ed/2022/#appendices-and-data", "HERE"))), 
                          width = 12,
                          color="black")
                      )),
                    body
)

server<-function(input, output){
  output$scatterplot<-renderPlot({
    ggplot(data = world_clean, aes_string(x = input$first, y = input$second)) + 
      geom_point(aes(color = continent)) + 
      geom_smooth(aes(color = continent), method=lm, se=FALSE) + 
      labs(title=paste0("Relationship between ", input$first, " and ", input$second),
           x=as.character(input$first), y=as.character(input$second)) + theme_gdocs()
  })
    output$hist<-renderPlot({
      ggplot(world_clean, aes_string(x = input$third, fill="income_grp")) +
        geom_histogram(alpha = 0.3) + 
        facet_wrap(.~continent, scales = "free_y") + 
        stat_bin(bins = input$range) +  
        labs(title=paste0("Distribution of ", input$third, " for each continent by income group")) + 
        theme(legend.title = element_blank(), legend.position = "top",
              legend.direction = "horizontal", 
              panel.grid = element_line(linetype = "dotted", size = 0.7)) +
        scale_fill_brewer(palette = "Set2") + theme_bw()
  })
    output$heatmap<-renderPlotly({
      ggplotly(ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) +
                 geom_tile() + theme(axis.text.x = element_text(angle = 90)) +
                 labs(title = "Correlation heatmap", x="", y="") +
                 scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                                      midpoint = 0, limit = c(-1,1), space = "Lab", 
                                      name="Correlation")
               ) 
  })
    output$map<-renderPlot({
      
      df1 <- df %>% filter(year == input$fifth) 
      world1 <- world %>% 
        left_join(df1[,c('country','year','happiness','gdp', 'soc_support', 'hle',
                         'freedom', 'generosity', 'corruption', 'positive',
                         'negative', 'conf_gov')],by = c('name' = 'country'))
      ggplot() + geom_sf(data = world1, aes_string(fill=input$forth)) +
        labs(title=paste0(input$forth, " scale map")) +
        theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 12)) +
        scale_fill_viridis_c()
  })
    output$boxplot<-renderPlot({
    ggplot(data = world_clean, aes_string(y = input$sixth, x = "income_grp",
                                          fill = "income_grp", color = "income_grp")) + 
      geom_boxplot(alpha = 0.7) +  
        labs(title=paste0("Distribution of ", input$sixth, " by income groups"),
             x="Income Group", y=as.character(input$sixth))  + 
        theme(axis.text.x = element_blank())
  })
    output$image = renderImage(
      list(
        src = "metadata.jpg",
        contentType = "image/jpg",
        width = "100%", height = "500px"
      )
    )
}
  

shinyApp(ui=ui, server=server)
