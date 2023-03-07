#Libraries

library(shiny)
library(shinythemes)
library(ggplot2)
library(dplyr)
library(ggplot2)
library(dplyr)
library(leaflet)
library(sf)
library(mapview)
library(gganimate)
library(viridis)
library(ggrepel)
library(lubridate)
library(gifski)
library(forcats)
library(plotly)
library(stringr)
library(viridis)
library(tidyr)
library(scales)
library(easylabel)
library(ggExtra)
library(GGally)
library(ggpubr)

#Space with long lat
df_long_lat <- read.csv('data_with_longlat.csv')
#Corrected Missions
df = read.csv("Space_Corrected.csv")
#Tesla
tesla = read.csv('TSLA.csv')
#Astronauts
astronauts = read.csv("astronauts.csv")


#UI
ui <- fluidPage(theme = shinytheme("superhero"),
                titlePanel("Space Missions Analysis"),
                
                #Main Fluid Row 
                fluidRow( class= "R1",
                          # Start of tabset 
                          tabsetPanel(type= "tabs",
                                      #Team tab
                                      tabPanel("Team ",
                                               mainPanel(imageOutput(outputId = "img", width = "80%", height = "400px"))),
                                      #Countries Tab Panel     
                                      tabPanel("Countries",
                                               #Graph1 Map
                                               fluidRow(
                                                 column(4, width = 4.5,
                                                        sidebarPanel(
                                                          tags$h3("Mission Status"),
                                                          selectInput(
                                                            inputId = "status",
                                                            label = "Select status",
                                                            choices = c(
                                                              'Success' = 'Success',
                                                              'Failure' = 'Failure'
                                                            ))#Sidebar End
                                                        )),#Column End
                                                 column(8, width = 7.5, offset = 0, 
                                                        mainPanel(
                                                          leafletOutput(outputId = "map"),br(),br())
                                                 )#Column End
                                               ),#FluidRow Ends
                                               
                                               fluidRow(
                                                 mainPanel(
                                                   plotOutput(outputId = "cont1", width="100%"),br(),br())
                                               ),#FluidRow End
                                               #Graph 2 
                                               
                                               fluidRow(
                                                 sidebarLayout(position = "left",
                                                               sidebarPanel(h4("Years"),
                                                                            sliderInput("Year","Missions based on Years",
                                                                                        min=min(df_long_lat$Year),
                                                                                        max=max(df_long_lat$Year),
                                                                                        value =min(df_long_lat$Year),
                                                                                        step=1,
                                                                                        animate = TRUE)),
                                                               mainPanel(textOutput("out_year"),plotOutput("cont2"),br(),br()))),
                                               
                                               fluidRow(
                                                 column(4, width = 4.5,
                                                        sidebarPanel(
                                                          tags$h3("Missions by Country"),
                                                          selectInput(
                                                            inputId = "country",
                                                            label = "Select country",
                                                            choices = c(
                                                              "Russia" = "Russia",
                                                              "USA" = "USA",
                                                              "China" = "China",
                                                              "France" = "France",
                                                              "Japan" = "Japan",
                                                              "Italy" = "Italy",
                                                              "Australia" = "Australia",
                                                              "Ukraine" = "Ukraine",
                                                              "India" = "India",
                                                              "Israel" = "Israel",
                                                              "Brazil" = "Brazil",
                                                              "North Korea" = "North Korea",
                                                              "Norway, Ukraine, USA, Russia" = "Norway, Ukraine, USA, Russia",
                                                              "Iran" = "Iran",
                                                              "South Korea" = "South Korea"
                                                            ))#Sidebar End
                                                        )),#Column End
                                                 column(8, width = 7.5, offset = 0, 
                                                        mainPanel(
                                                          plotOutput(outputId = "cont3"),br(),br())
                                                 )#Column end
                                                 
                                               ),#FluidRow End
                                               
                                               fluidRow(
                                                 mainPanel(
                                                   plotlyOutput(outputId = "cont4", width="100%"),br(),br())
                                               ),#FluidRow End
                                               
                                               fluidRow(
                                                 mainPanel(
                                                   plotlyOutput(outputId = "cont5", width="100%"),br(),br())
                                               )#FluidRow End
                                               
                                      ),#Tab Panel Countries Ends
                                      tabPanel("Companies",
                                               fluidRow(
                                                 column(8, width = 12, align="center", offset = 2,
                                                        mainPanel(
                                                          plotOutput(outputId = "comp1"),br(),br())
                                                 )),#FluidRow End
                                               
                                               fluidRow(
                                                 column(8, width = 12, align="center", offset = 2,
                                                        mainPanel(
                                                          plotOutput(outputId = "comp2"),br(),br())
                                                 )),#FluidRow End
                                      ),#Tab Panel Companies Ends
                                      
                                      tabPanel("Launches",
                                               fluidRow(
                                                 column(8, width = 12, align="center", offset = 2,
                                                        mainPanel(
                                                          plotOutput(outputId = "launch1"),br(),br())
                                                 )),#FluidRow End
                                               
                                               fluidRow(
                                                 column(8, width = 12, align="center", offset = 2,
                                                        mainPanel(
                                                          plotOutput(outputId = "launch2"),br(),br())
                                                 )),#FluidRow End
                                      ),#Tab Panel Launches Ends
                                      
                                      #SpaceX tab
                                      tabPanel("SpaceX",
                                               titlePanel("Tesla Stocks Before and After SpaceX Launches"),
                                               fluidRow(
                                                 column(4,  offset = 0, width = 4.5,
                                                        sidebarPanel(
                                                          tags$h3("SpaceX and Tesla Stock"),
                                                          selectInput(
                                                            inputId = "choice",
                                                            label = "Select Date",
                                                            choices = c("2020-08-07 Success" = "2020-08-07",
                                                                        "2020-08-04 Success" = "2020-08-04",
                                                                        "2020-07-20 Success" = "2020-07-20",
                                                                        "2020-06-30 Success" = "2020-06-30",
                                                                        "2020-06-04 Success" = "2020-06-04",
                                                                        "2020-04-22 Success" = "2020-04-22",
                                                                        "2020-03-18 Success" = "2020-03-18",
                                                                        "2020-01-29 Success" = "2020-01-29",
                                                                        "2020-01-07 Success" = "2020-01-07",
                                                                        "2019-12-17 Success" = "2019-12-17",
                                                                        "2019-12-05 Success" = "2019-12-05",
                                                                        "2019-11-11 Success" = "2019-11-11",
                                                                        "2019-08-06 Success" = "2019-08-06",
                                                                        "2019-07-25 Success" = "2019-07-25",
                                                                        "2019-06-25 Success" = "2019-06-25",
                                                                        "2019-06-12 Success" = "2019-06-12",
                                                                        "2019-05-24 Success" = "2019-05-24",
                                                                        "2019-04-11 Success" = "2019-04-11",
                                                                        "2019-02-22 Success" = "2019-02-22",
                                                                        "2019-01-11 Success" = "2019-01-11",
                                                                        "2018-12-03 Success" = "2018-12-03",
                                                                        "2018-11-15 Success" = "2018-11-15",
                                                                        "2018-10-08 Success" = "2018-10-08",
                                                                        "2018-09-10 Success" = "2018-09-10",
                                                                        "2018-08-07 Success" = "2018-08-07",
                                                                        "2018-07-25 Success" = "2018-07-25",
                                                                        "2018-06-29 Success" = "2018-06-29",
                                                                        "2018-06-04 Success" = "2018-06-04",
                                                                        "2018-05-22 Success" = "2018-05-22",
                                                                        "2018-05-11 Success" = "2018-05-11",
                                                                        "2018-04-18 Success" = "2018-04-18",
                                                                        "2018-04-02 Success" = "2018-04-02",
                                                                        "2018-03-06 Success" = "2018-03-06",
                                                                        "2018-02-22 Success" = "2018-02-22",
                                                                        "2018-02-06 Success" = "2018-02-06",
                                                                        "2018-01-31 Success" = "2018-01-31",
                                                                        "2018-01-08 Success" = "2018-01-08",
                                                                        "2017-12-15 Success" = "2017-12-15",
                                                                        "2017-10-30 Success" = "2017-10-30",
                                                                        "2017-10-11 Success" = "2017-10-11",
                                                                        "2017-10-09 Success" = "2017-10-09",
                                                                        "2017-09-07 Success" = "2017-09-07",
                                                                        "2017-08-24 Success" = "2017-08-24",
                                                                        "2017-08-14 Success" = "2017-08-14",
                                                                        "2017-07-05 Success" = "2017-07-05",
                                                                        "2017-06-23 Success" = "2017-06-23",
                                                                        "2017-05-15 Success" = "2017-05-15",
                                                                        "2017-05-01 Success" = "2017-05-01",
                                                                        "2017-03-30 Success" = "2017-03-30",
                                                                        "2017-03-16 Success" = "2017-03-16",
                                                                        "2016-09-01 Failure" = "2016-09-01",
                                                                        "2016-08-14 Success" = "2016-08-14",
                                                                        "2016-06-15 Success" = "2016-06-15",
                                                                        "2016-05-27 Success" = "2016-05-27",
                                                                        "2016-05-06 Success" = "2016-05-06",
                                                                        "2016-04-08 Success" = "2016-04-08",
                                                                        "2016-03-04 Success" = "2016-03-04",
                                                                        "2016-01-17 Success" = "2016-01-17",
                                                                        "2015-12-22 Success" = "2015-12-22",
                                                                        "2015-04-27 Success" = "2015-04-27",
                                                                        "2015-04-15 Success" = "2015-04-15",
                                                                        "2015-03-02 Success" = "2015-03-02",
                                                                        "2015-02-11 Success" = "2015-02-11",
                                                                        "2014-08-05 Success" = "2014-08-05",
                                                                        "2014-07-14 Success" = "2014-07-14",
                                                                        "2014-01-06 Success" = "2014-01-06",
                                                                        "2013-12-03 Success" = "2013-12-03",
                                                                        "2013-03-01 Success" = "2013-03-01",
                                                                        "2012-10-08 Failure" = "2012-10-08",
                                                                        "2012-05-22 Success" = "2012-05-22",
                                                                        "2010-12-08 Success" = "2010-12-08")
                                                            
                                                          ))#Sidebar end
                                                 ), #Column End
                                                 column(8, width = 7.5, offset = 0, 
                                                        mainPanel(
                                                          plotOutput(outputId = "tsl"),br(),br())
                                                 )#Column End
                                               )),#tabpanel End
                                      
                                      tabPanel("Astronauts",
                                               fluidRow(
                                                 column(8, width = 12, align="center", offset = 2,
                                                        mainPanel(
                                                          plotlyOutput(outputId = "astr1"),br(),br())
                                                 )),#FluidRow End
                                               fluidRow(
                                                 column(8, width = 12, align="center", offset = 2,
                                                        mainPanel(
                                                          plotlyOutput(outputId = "astr2"),br(),br())
                                                 )),#FluidRow End
                                               fluidRow(
                                                 column(8, width = 12, align="center", offset = 2,
                                                        mainPanel(
                                                          plotlyOutput(outputId = "astr3"),br(),br())
                                                 )),#FluidRow End
                                               fluidRow(
                                                 column(8, width = 12, align="center", offset = 2,
                                                        mainPanel(
                                                          plotlyOutput(outputId = "astr4"),br(),br())
                                                 )),#FluidRow End
                                      )#TabPanel
                                      
                                      
                          )#Tabset Panel
                )#Main Fluid Row
)#UI End


#Server 
server <- function(input, output) {
  
  #Team Tab
  output$img <- renderImage( list(
    src = "space.jpeg",
    contentType = "image/png",
    height = 600,
    alt = "Face"
  ) )
  
  #Countries Tab
  #CT1 Map plot
  output$map <- renderLeaflet({
    
    grouped <- df_long_lat %>% group_by(Location, Longitude, Latitude, SuccessFailure) %>% summarise(Missions=n())
    data <- subset(grouped, SuccessFailure==input$status)[c(1,2,3,5)]
    
    mytext=paste("Location: ", data$Location, "<br/>Missions: ", data$Missions, sep=" ") %>%
      lapply(htmltools::HTML)
    
    leaflet(data) %>% 
      addTiles()  %>% 
      setView( lat=40.193317, lng=44.504462 , zoom=40) %>%
      addProviderTiles("Esri.WorldImagery") %>%
      addCircles(~Longitude, ~Latitude, 
                 fillColor='rgb(208, 88, 17)', fillOpacity = 0.7, radius=~sqrt(Missions)*40000, stroke=FALSE, weight = 1,
                 label = mytext,
                 labelOptions = labelOptions( style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "13px", direction = "auto")
      )
    
  })
  
  #CT Plot 1
  output$cont1 <- renderPlot({
    countries.succ.fail <- df_long_lat %>% group_by(CompanyCountry, SuccessFailure) %>% summarise(count=n())
    ggplot(countries.succ.fail, aes(x = CompanyCountry, y = count, fill=SuccessFailure)) +
      geom_bar(stat='identity',position = 'fill') +
      theme(axis.text.x = element_text(angle = 60,hjust=1)) + ylab('Missions proportion') +
      ggtitle('Countries with proportion of Success and Failure') + xlab('Country') +
      scale_fill_brewer(palette = 'Pastel1')
  })
  
  #CT Plot 2
  output$out_year<-renderText(input$Year)
  
  output$cont2<-renderPlot({
    
    df_long_lat %>% filter(Year == input$Year)  %>% ggplot(mapping = aes(x=CompanyCountry, fill=SuccessFailure)) +
      geom_bar(position = 'dodge') +
      labs(fill='Success vs Failure', title = 'Year') + ylab('Number of Missions') +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
      scale_fill_brewer(palette = 'Pastel1')
  })
  
  #CT Plot 3
  output$cont3<- renderPlot({
    ggplot(subset(df_long_lat,CompanyCountry==input$country),aes(x=Year,fill=SuccessFailure)) + geom_bar(position='dodge') +
      theme_bw() +
      scale_fill_brewer(palette = 'Pastel1') +
      ggtitle(paste('Space Missions of',input$country,sep=' ')) +
      labs(fill='Success vs Failure') +
      ylab('Number of Missions')
  })
  
  #CT Plot 4
  output$cont4 <- renderPlotly ({
    country_rocket_launches <-
      df_long_lat %>% dplyr::select(Date, Country) %>%
      mutate(country = fct_lump(Country, 5)) %>%
      mutate(launch_year = year(Date)) %>%
      group_by(country, launch_year) %>%
      summarise(Count = n()) %>%
      arrange(desc(Count)) %>%
      ggplot(aes(x = launch_year, y = Count, fill = country,
                 text = paste("Country: ", country,
                              "<br>Launch Year: ", launch_year,
                              "<br>Number of Launches: ", Count))) +
      geom_col(position = "fill") +
      scale_x_continuous(breaks = seq(from = 1956, to = 2020, by = 4)) +
      theme_bw() +
      scale_fill_brewer(palette = 'Pastel1') +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
      theme(axis.line = element_line(colour = "darkblue",
                                     size = 2)) +
      labs(
        title = "Launches by year by country (leading)",
        subtitle = "Column plot, launches by year by country (leading)",
        caption = "All Space Missions from 1957",
        x = "Year",
        y = "Count Scaled",
        fill = "Country"
      )
    
    ggplotly(country_rocket_launches, tooltip = "text")
    
  })
  
  #CT Plot 5
  output$cont5 <- renderPlotly({
    space_mission_loc <- df %>% mutate(Country =  word(Location,-1))
    
    country_spents <-
      space_mission_loc %>% dplyr::select(Country, Rocket) %>%
      mutate(country = fct_lump(Country, 25)) %>%
      filter(!(is.na(Rocket) | Rocket == "")) %>%
      group_by(country) %>%
      summarise(Total_spent = sum(as.numeric(gsub(",", "", Rocket)))/1000) %>%
      ungroup() %>%
      arrange(desc(Total_spent)) %>%
      ggplot(aes(x = fct_reorder(country, Total_spent), y = Total_spent, label = paste0(round(Total_spent, 1),"B"),
                 text = paste("Country: ", fct_reorder(country, Total_spent),
                              "<br>Total Spent: ", Total_spent,
                              "<br>Total Spent in B: ", paste0(round(Total_spent,1),"B")))) +
      geom_col(fill = '#91afc4') +
      geom_text(hjust = -0.8, color = '#a36b22') +
      theme_bw() +
      scale_fill_brewer(palette = 'Pastel1') +
      coord_flip() +
      scale_y_continuous(labels = unit_format(unit = "B"), expand = expansion(add = c(0, 13))) +
      labs(
        title = "Total Cost by Country",
        subtitle = "Pie Plot, Total Cost by Country",
        caption = "All Space Missions from 1957",
        fill = "",
        x = "Country Name",
        y = "Total Spending"
      )
    
    ggplotly(country_spents, tooltip = "text")
    
    
  })
  
  #Companies Tab
  #CPT Plot1
  output$comp1 <- renderPlot({
    top_companies <- df_long_lat %>% group_by(Company) %>% summarise(n=n())
    comps <- top_companies[order(top_companies$n,decreasing=T),][1:10,]$Company
    top_20_companies <- df_long_lat %>% group_by(Company, SuccessFailure) %>% summarise(n=n())
    top_20_companies <- subset(top_20_companies, Company %in% comps)
    ggplot(top_20_companies) + geom_col(aes(x=Company,y=n), fill="#B3CDE3") +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + ylab('Number of Missions') +
      ggtitle('Top 10 companies based on the number of missions') +
      scale_fill_brewer(palette = 'Pastel1')
  })
  
  #CPT Plot2
  output$comp2 <- renderPlot({
    corr <- df[3:9]
    corr$Rocket <- as.numeric(corr$Rocket)
    corr$Rocket[is.na(corr$Rocket)] <- 0
    corr$Status.Mission[corr$Status.Mission!='Success'] <- 'Failure'
    companies.price <- corr %>% group_by(Company.Name) %>% summarise(Price=sum(Rocket))
    comps <- companies.price[order(companies.price$Price,decreasing=T),][1:10,]$Company.Name
    companies.price <- corr %>% group_by(Company.Name, Status.Mission) %>% summarise(Price=sum(Rocket))
    companies.price <- subset(companies.price, Company.Name %in% comps)
    ggplot(companies.price) + geom_col(aes(x=Company.Name,y=Price), fill="#FBB4AE") +
      theme_bw() + ylab('Price (in $million)') + xlab('Company') +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
      ggtitle('Top 10 companies based on the money spent on rockets/missions') +
      scale_fill_brewer(palette = 'Pastel1')
  })
  
  #Launches Tab
  #LT Plot1
  output$launch1 <- renderPlot({
    success_grouped_weekdays <- subset(df_long_lat,SuccessFailure=='Success') %>% group_by(WeekdayN) %>% summarise(n=n())
    failure_grouped_weekdays <- subset(df_long_lat,SuccessFailure!='Success') %>% group_by(WeekdayN) %>% summarise(n=n())
    
    ggplot() + geom_line(success_grouped_weekdays,mapping=aes(x=WeekdayN+1,y=n),size=3,color='#FBB4AE') +
      geom_line(failure_grouped_weekdays,mapping=aes(x=WeekdayN+1,y=n),size=3,color='#B3CDE3') +
      scale_x_discrete(limit=c('Mon','Tue','Wed','Thu','Fri','Sat','Sun')) +
      ylab('Number of Missions') + xlab('Weekdays') + 
      theme_bw() +
      scale_fill_brewer(palette = 'Pastel1') +
      ggtitle('The number of missions per weekday') +
      annotate("text", x=7, y=260, label= "Success",color='#FBB4AE',size=6.4) +
      annotate("text", x=7, y=64, label= "Failure",color='#B3CDE3',size=6.4)
    
  })
  
  #LT Plot2
  output$launch2 <- renderPlot({
    launches_per_month_2022 <-
      df_long_lat %>% dplyr::select(Date, Country) %>%
      mutate(country = fct_lump(Country, 5)) %>%
      mutate(launch_month = month(Date, label = TRUE),
             launch_year = year(Date)) %>%
      filter(launch_year == 2022) %>%
      group_by(country, launch_month) %>%
      summarise(Count = n()) %>%
      arrange(desc(Count)) %>%
      ggplot(aes(x = launch_month, y = Count, label = Count)) +
      geom_col(aes(fill = launch_month)) +
      geom_text(vjust = 1.2, col = "#a36b22") +
      scale_fill_viridis(discrete = TRUE) +
      facet_grid(vars(country)) +
      theme_bw() +
      scale_fill_brewer(palette = 'Pastel1') +
      theme(legend.position = "none") +
      labs(
        title = "This year (2022) launches per month for each country",
        subtitle = "",
        caption = "All Space Missions from 1957",
        x = "Launch Month",
        y = "Number of launches"
      )
    
    launches_per_month_2022
    
  })
  
  #SpaceX Tab 
  #Plot
  output$tsl <- renderPlot({
    
    spacex <- df %>% filter(Company.Name == "SpaceX")
    spacex <- spacex[ -2 ]
    spacex$Date <- format(as.POSIXct(spacex$Datum, format = '%a %b %d, %Y %H:%M UTC'),'%Y-%m-%d')
    
    date = as.Date(input$choice)
    tsl_A20 <- tesla %>% filter((Date > (date - 15)) & (Date < (date+ 15)))
    ggplot(tsl_A20, aes(x = Date, y = Adj.Close, group = 1)) +
      geom_line(color = 'dark blue') +
      ggtitle(paste('Tesla Stock Prices and SpaceX Launch of the date of ',input$choice,sep=' ')) +
      geom_vline(xintercept = input$choice, color = 'black')+
      theme_bw() +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
    
  })
  
  #Astranauts Tab
  #AT Plot 1
  output$astr1 <- renderPlotly({
    grd <- astronauts %>% group_by(Year.of.mission,Sex) %>% summarise(count=n())
    grd <- grd[1:93,]
    grd %>% plot_ly(x = ~Year.of.mission, y = ~count, type = 'bar', 
                    color = ~Sex,
                    colors = c(M = '#B3CDE3', F = '#FBB4AE')) %>% layout(title = 'Missions done by Males and Females by Year',
                                                                         xaxis = list(title='Year'), yaxis = list(title='Number of Missions'))
  })
  
  #AT Plot2
  output$astr2 <- renderPlotly({
    grd <- subset(astronauts, Sex == 'F')
    grd <- grd %>% group_by(Nationality) %>% summarise(count=n())
    grd$Nationality <- factor(grd$Nationality, levels = unique(grd$Nationality)[order(grd$count, decreasing = TRUE)])
    top_countries <- grd[order(grd$count,decreasing=T),][1:8,]$Nationality
    grd <- subset(grd, Nationality %in% top_countries)
    # grd <- grd[1:93,]
    grd %>% plot_ly(x = ~Nationality, y = ~count, type = 'bar', 
                    #   color = ~Sex,
                    marker = list(color = '#FBB4AE',
                                  line = list(color = '#FBB4AE',
                                              width = 1.5))) %>% layout(title = 'Top 8 countries women austronauts come from',
                                                                        xaxis = list(title='Country'), yaxis = list(title='Number of Missions'))
    
  })
  
  #AT Plot3
  output$astr3 <- renderPlotly({
    hours_sorted <- astronauts[order(astronauts$Hours.mission,decreasing=T),]
    hours_sorted[is.na(hours_sorted$Hours.mission),]$Hours.mission <- 0
    density <- density(hours_sorted$Hours.mission/24)
    # density
    fig <- plot_ly(x = ~density$x, y = ~density$y, mode = 'lines')
    fig <- fig %>% layout(xaxis = list(title = 'Days'),
                          yaxis = list(title = 'Density'), title = 'Density of Days spent in Space')
    fig
    
  })

  #AT Plot4
  output$astr4<- renderPlotly({
    astronauts$Nationality = str_to_upper(astronauts$Nationality)
    astronauts$Occupation = str_to_upper(astronauts$Occupation)
    
    astr<-filter(astronauts, Nationality %in% c('U.S.','U.K./U.S.','U.S.S.R/RUSSIA','KAZAKHSTAN','FRANCE','U.K.','CHINA',
                                                'JAPAN'))
    p = ggballoonplot(as.data.frame(table(x = astr$Occupation, y = astr$Nationality)), fill = "value")+
      theme_bw() +
      scale_fill_gradientn(colours=RColorBrewer::brewer.pal(7,"Pastel1")) +
      theme(axis.text.x = element_text(angle = 60, vjust = 0.5, hjust=1)) +
      labs(fill = "Frequency",title="Occupation of Astronauts per Country", xlab("Occupation"),ylab("Countries"))
    
    
    ggplotly(p)
    
    
  })
  
}

shinyApp(ui=ui, server=server)

