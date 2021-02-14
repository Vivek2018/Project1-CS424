
#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#include necessary libraries
library(shiny)
library(readr)
library(ggplot2)
library(shinydashboard)
library(DT)
library(dplyr)
library(rlang)
library(maps)
library(mapdata)
library(maptools)
library(ggthemes)
library(rgeos)
library(ggmap)
library(usmap)
#data cleaning section 

#clean out all flaws in dataset and prepare it to be used for analysis
data <- read_csv("annual_generation_state.csv")

#Remove -watt hour data and incomplete/missing data 
cleaned_data <- annual_generation_state[annual_generation_state$`GENERATION (Megawatthours)` >= 0  & complete.cases(annual_generation_state), ]

#capitalize the US_Total format 
cleaned_data$STATE = toupper(cleaned_data$STATE)

#remove data related to other energy sources such as pumped storage and other storage 
cleaned_data <- cleaned_data[!grepl('Other', cleaned_data$`ENERGY SOURCE`),]
cleaned_data <- cleaned_data[!grepl('Pumped Storage', cleaned_data$`ENERGY SOURCE`),]

#final portions of data: replace long names with short names for certain energy and convert data types
finalized_data<-cleaned_data
finalized_data$`ENERGY SOURCE`[finalized_data$`ENERGY SOURCE`=="Hydroelectric Conventional"]  <- "Hydro"
finalized_data$`ENERGY SOURCE`[finalized_data$`ENERGY SOURCE`=="Wood and Wood Derived Fuels"]  <- "Wood"
finalized_data$`ENERGY SOURCE`[finalized_data$`ENERGY SOURCE`=="Solar Thermal and Photovoltaic"]  <- "Solar"
finalized_data$`GENERATION (Megawatthours)` <- as.numeric(finalized_data$`GENERATION (Megawatthours)`)
finalized_data$`STATE` <- as.factor(finalized_data$`STATE`)
finalized_data$`TYPE OF PRODUCER` <- as.factor(finalized_data$`TYPE OF PRODUCER`)
finalized_data <- finalized_data[finalized_data$`ENERGY SOURCE` != "Total",] 
finalized_data$`ENERGY SOURCE` <- as.factor(finalized_data$`ENERGY SOURCE`)

#create a state list to convert state names to abbrv
state_names <- state.name[unique(finalized_data$`STATE`)]
state_names <- state_names[complete.cases(state_names)]
state_names <- c(state_names, "Washington DC", "Total US")



# Define UI for application that draws a histogram
ui <- dashboardPage(
  
  #create dashboard and elements
  dashboardHeader(title = "CS 424 Project 1"),
  
  dashboardSidebar(disable = FALSE, collapsed = FALSE,
                   #menu bar with all 3 panels and about page
                   sidebarMenu(
                     menuItem("US Data Overall", tabName = "US_Data", icon = NULL),
                     menuItem("State to State Comparison", tabName = "State_Compare", icon = NULL),
                     menuItem("Resource Comparisons", tabName = "Resource_Compare", icon = NULL),
                     menuItem("About Page", tabName = "About", icon = NULL)
                   )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName="US_Data",
              #check box for options and all plots sorted into columns 
              column(width=10, offset=1, 
                     h2("US Data"),
              checkboxGroupInput("Energy_US", 
                                 h3("Select Energy:"), 
                                 choices = c("All" = "All",
                                             'Coal' = 'Coal', "Geothermal" = "Geothermal", "Hydro" = "Hydro",
                                             "Natural Gas"  ="Natural Gas" , "Nuclear" = "Nuclear", "Petroleum" = "Petroleum", "Solar" = "Solar", "Wind" = "Wind", "Wood" = "Wood"),
                                 selected = "All",
                                 inline = TRUE)),
              
              
              fluidRow(
                column(width=5, offset=1,
                       #Stacked box plot
                       fluidRow(
                         box(title = "Amount of Energy Provided Per Year as Stacked Box Plot for US", solidHeader = TRUE, status = "primary", width = 100,
                             plotOutput("US_Stacked", height = 300)
                         )
                       ),
                       
                       #Stacked box plot (Percentage)
                       fluidRow(
                         box(title = "% Breakdown of Energy as Box Plot for US", solidHeader = TRUE, status = "primary", width = 100,
                             plotOutput("US_Stacked_Percent", height = 300)
                         )
                       )
                ),
                
                
                column(5, offset=0,
                       #Line plot US 
                       fluidRow(
                         box(title = "Amount of Energy Provided Per Year as Line Plot for US", solidHeader = TRUE, status = "primary", width = 100,
                             plotOutput("US_Line", height = 300)
                         )
                       ),
                       
                       #Line plot US %
                       fluidRow(
                         box(title = "% Breakdown of Energy as Line Plot for US", solidHeader = TRUE, status = "primary", width = 100,
                             plotOutput("US_Line_Percent", height = 300)
                         )
                       )
                ),
                
                #RAW data
                column(8, offset=2,
                       fluidRow(
                         box(title = "Raw Data for Energy Production for US", solidHeader = TRUE, status = "primary", width = 100,
                             dataTableOutput("US_Raw")
                         )
                       ),

                       
                )
              )
              
              
      ), 
      #state comparison page
      tabItem(tabName="State_Compare",
              h2("State to State Comparison"),
              column(1, 
                     #Selectors come first
                     selectInput("State1", h5("Choose the first State:"),
                                 choices = state_names, 
                                 selected = "Total US",
                              
                                 
                     ),
                     
                     selectInput("Energy1", h5("Choose the first Energy Source:"),
                                 choices=c("All" = "All",
                                           'Coal' = 'Coal', "Geothermal" = "Geothermal", "Hydro" = "Hydro",
                                           "Natural Gas"  ="Natural Gas" , "Nuclear" = "Nuclear", "Petroleum" = "Petroleum", "Solar" = "Solar", "Wind" = "Wind", "Wood" = "Wood"), 
                                 selected = "All",
       
                     ),
                     
                     selectInput("Year1", h5("Choose the first Year:"),
                                 choices = c("All", unique(finalized_data$YEAR)), 
                                 selected = "All",
        
                     ),
                     
                     
                     selectInput("State2", h5("Choose the second State:"),
                                 choices = state_names, 
                                 selected = "Illinois",
        
                     ),
                     
                     selectInput("Energy2", h5("Choose the second Energy Source:"),
                                 choices=c("All" = "All",
                                           'Coal' = 'Coal', "Geothermal" = "Geothermal", "Hydro" = "Hydro",
                                           "Natural Gas"  ="Natural Gas" , "Nuclear" = "Nuclear", "Petroleum" = "Petroleum", "Solar" = "Solar", "Wind" = "Wind", "Wood" = "Wood"), 
                                 selected = "All",
   
                     ),
                     
                     selectInput("Year2", h5("Choose the second Year:"),
                                 choices = c("All", unique(finalized_data$YEAR)), 
                                 selected = "All",

                     )
              ),
              
              
              #State 1 data (Boxed plots)
              fluidRow(
                column(3,
                       fluidRow(
                         box(title = "Amount of Energy Provided Per Year as Stacked Box Plot for State 1", solidHeader = TRUE, status = "primary", width = 100,
                             plotOutput("State1_Stacked", height = 300)
                         )
                       ),
                       
                       
                       fluidRow(
                         box(title = "% Breakdown of Energy as Box Plot for State 1", solidHeader = TRUE, status = "primary", width = 100,
                             plotOutput("State1_Stacked_Percent", height = 300)
                         )
                       )
                       
                       
                ),
                
                #State 1 data (Line plots)
                column(2,
                       fluidRow(
                         box(title = "Amount of Energy Provided Per Year as Line Plot for State 1", solidHeader = TRUE, status = "primary", width = 100,
                             plotOutput("State1_Line", height = 300)
                         )
                       ),
                       
                       
                       fluidRow(
                         box(title = "% Breakdown of Energy as Line Plot for State 1", solidHeader = TRUE, status = "primary", width = 100,
                             plotOutput("State1_Line_Percent", height = 283)
                         )
                       )
                ),
                #State 2 data (Boxed plots)
                column(3,
                       fluidRow(
                         box(title = "Amount of Energy Provided Per Year as Stacked Box Plot for State 2", solidHeader = TRUE, status = "primary", width = "50%",
                             plotOutput("State2_Stacked", height = 300)
                         )
                       ),
                       
                       
                       fluidRow(
                         box(title = "% Breakdown of Energy as Box Plot for State 2", solidHeader = TRUE, status = "primary", width = "50%",
                             plotOutput("State2_Stacked_Percent", height = 300)
                         )
                       )
                ),
                
                #State 2 data (Line plots)
                column(2,
                       fluidRow(
                         box(title = "Amount of Energy Provided Per Year as Line Plot for State 2", solidHeader = TRUE, status = "primary", width = 100,
                             plotOutput("State2_Line", height = 300)
                         )
                       ),
                       
                       
                       fluidRow(
                         box(title = "% Breakdown of Energy as Line Plot for State 2", solidHeader = TRUE, status = "primary", width = 100,
                             plotOutput("State2_Line_Percent", height = 283)
                         )
                       )
                ),
                
                #State 1 and 2 RAW DATA
                column(5, offset=1 ,
                       fluidRow(
                         box(title = "Raw Data for Energy Production for State 1", solidHeader = TRUE, status = "primary", width = 200,
                             dataTableOutput("State1_Raw")
                         )
                       ),
                ),
                column(5,
                       fluidRow(
                         box(title = "Raw Data for Energy Production for State 2", solidHeader = TRUE, status = "primary", width = 200,
                             dataTableOutput("State2_Raw")
                         )
                       )
                )
              )
              
      ),
      
      #Create resource comparison per year tab
      tabItem(tabName="Resource_Compare",
              h2("Resource Comparisons"),
              
              
              #input selection for parameters
              column(1, 
                     
                     
                     selectInput("EnergySource1", h5("Choose the first Energy Source:"),
                                 choices=c('Coal' = 'Coal', "Geothermal" = "Geothermal", "Hydro" = "Hydro",
                                           "Natural Gas"  ="Natural Gas" , "Nuclear" = "Nuclear", "Petroleum" = "Petroleum", "Solar" = "Solar", "Wind" = "Wind", "Wood" = "Wood"), 
                                 selected = "Coal",
                        
                     ),
                     
                     selectInput("ComparisonYear1", h5("Choose the first Year:"),
                                 choices = unique(finalized_data$YEAR), 
                                 selected = 1990,
                        
                     ),
                     
                     
                     
                     selectInput("EnergySource2", h5("Choose the second Energy Source:"),
                                 choices=c('Coal' = 'Coal', "Geothermal" = "Geothermal", "Hydro" = "Hydro",
                                           "Natural Gas"  ="Natural Gas" , "Nuclear" = "Nuclear", "Petroleum" = "Petroleum", "Solar" = "Solar", "Wind" = "Wind", "Wood" = "Wood"), 
                                 selected = "Coal",
                        
                     ),
                     
                     selectInput("ComparisonYear2", h5("Choose the second Year:"),
                                 choices = unique(finalized_data$YEAR), 
                                 selected = 1990,
                             
                     )
              ),
              
              
              #Resource 1 plots
              column(5,
                     
                     fluidRow(
                       box(title = "Resource 1 Heat Map by MWH", solidHeader = TRUE, status = "primary", width = "50%",
                           plotOutput("heatmap1", height = 300)
                       )
                     ),
                     
                     
                     fluidRow(
                       box(title = "Resource 1 Heat Map by %", solidHeader = TRUE, status = "primary", width = "50%",
                           plotOutput("heatmap_percent1", height = 300)
                       )
                     )
                     
              ),
              
              #Resource 2 plots
              
              column(5,
                     
                     fluidRow(
                       box(title = "Resource 2 Heat Map by MWH", solidHeader = TRUE, status = "primary", width = "50%",
                           plotOutput("heatmap2", height = 300)
                       )
                     ),
                     
                     
                     fluidRow(
                       box(title = "Resource 2 Heat Map by %", solidHeader = TRUE, status = "primary", width = "50%",
                           plotOutput("heatmap_percent2", height = 300)
                       )
                     )
                     
              )
              
      ),
      #About page
      tabItem(tabName="About",
              h2("About Page"),
              verbatimTextOutput("AboutOut")
      )
      
    )
  )
)





# Define server logic required to draw a histogram
server <- function(input, output) {
  
  #initialize all data at the start
  
  
  #data for tab 1
  US_Total_data <- finalized_data[finalized_data$`STATE`=="US-TOTAL" & finalized_data$`TYPE OF PRODUCER`=="Total Electric Power Industry",]
  
  #colors for uniformity
  mappedColors <- c("Coal"="black", "Geothermal"="Green", "Hydro"="blue", "Natural Gas" = "#8B0CAC",
                    "Nuclear"="#0CA6AC", "Petroleum"="red", "Solar"="orange", "Wind"="pink", "Wood"="brown")
  
  #state data for tab 2 
  State_data <- finalized_data[finalized_data$`TYPE OF PRODUCER`=="Total Electric Power Industry" & finalized_data$`ENERGY SOURCE` != "Total",]
  
  
  
  #percent calculator by hand for the us data 
  #go through all elements (9 total energies) and calculate the denominator and numerator and divide each element as so
  splice_amount <- 8
  percent <- c()
  for (i in seq(from=1, to=nrow(US_Total_data), by=(splice_amount + 1))) {
    den <- sum(US_Total_data$`GENERATION (Megawatthours)`[i:(i+splice_amount)]) / 100
    percent <- c(percent, c(US_Total_data$`GENERATION (Megawatthours)`[i:(i+splice_amount)])/den)
    
  }
  US_Total_data$Percent <- percent
  
  #reactive value for US total
  selected_choices_US_Total <- reactiveValues(userResponse=c("Coal", "Geothermal", "Hydro", "Natural Gas", "Nuclear", "Petroleum", "Solar", "Wind", "Wood"), data=US_Total_data)
  
  observeEvent(input$Energy_US, {
    
    
    #parameters to filter out choice so there is no overlap between the all and normal categories
    selected_choices_US_Total$data <- US_Total_data
    
    if('All' %in% input$Energy_US) {
      userPicks <- c("Coal", "Geothermal", "Hydro", "Natural Gas", "Nuclear", "Petroleum", "Solar", "Wind", "Wood")
      selected_choices_US_Total$userResponse <- userPicks
    } else {
      userPicks <- duplicate(input$Energy_US, shallow = FALSE)
      selected_choices_US_Total$userResponse <- userPicks
    }
    
    #take the data and remove unwanted energies
    selected_choices_US_Total$data <- selected_choices_US_Total$data %>% filter_all(any_vars(. %in% selected_choices_US_Total$userResponse))
    
  })
  
  #print all sections from earlier for tab 1 with modified data
  
  #US stacked box plot
  output$US_Stacked <- renderPlot({
    ggplot(selected_choices_US_Total$data, aes(x = YEAR, y = `GENERATION (Megawatthours)`, fill = `ENERGY SOURCE`)) + 
      geom_bar(position="stack",
               stat = "identity") +
      xlab("\nYEAR") +
      ylab("GENERATION (Megawatthours)\n") + 
      labs(fill="ENERGY SOURCE") + scale_fill_manual(values=mappedColors, aesthetics = "fill", drop=FALSE) 
  })
  
  #US stacked box plot percentage
  output$US_Stacked_Percent <- renderPlot({
    ggplot(selected_choices_US_Total$data, aes(x = YEAR, y = `GENERATION (Megawatthours)`, fill = `ENERGY SOURCE`)) + 
      geom_bar(position="fill",
               stat = "identity") +
      xlab("\nYEAR") +
      ylab("GENERATION (Megawatthours)\n") + 
      labs(fill="ENERGY SOURCE") + scale_fill_manual(values=mappedColors, aesthetics = "fill", drop=FALSE) 
  })
  
  #US line plot
  output$US_Line <- renderPlot({
    ggplot(selected_choices_US_Total$data, aes(x = YEAR, y = (`GENERATION (Megawatthours)`), color = `ENERGY SOURCE`)) + 
      geom_line() +
      xlab("\nYEAR") +
      ylab("GENERATION (Megawatthours) \n") + 
      labs(fill="ENERGY SOURCE") + scale_fill_manual(values=mappedColors, aesthetics = "fill", drop=FALSE) 
  })
  
  #US line plot percentage
  output$US_Line_Percent <- renderPlot({
    ggplot(selected_choices_US_Total$data, aes(x = YEAR, y = Percent, color = `ENERGY SOURCE`)) + 
      geom_line() +
      xlab("\nYEAR") +
      ylab("GENERATION (Megawatthours) %\n") + 
      labs(fill="ENERGY SOURCE") + scale_fill_manual(values=mappedColors, aesthetics = "fill", drop=FALSE) 
    
  })
  
  #US line raw without states and energy producer details
  output$US_Raw <- renderDataTable({
    
    selected_choices_US_Total$data[c(-2, -3)]
    
  })
  
  
  
  #data setup for tab 2
  selected_choices_States <- reactiveValues(state1="US_TOTAL", energy1=c("Coal", "Geothermal", "Hydro", "Natural Gas", "Nuclear", "Petroleum", "Solar", "Wind", "Wood"), year1=1990, 
                                            state2="Illinois", energy2=c("Coal", "Geothermal", "Hydro", "Natural Gas", "Nuclear", "Petroleum", "Solar", "Wind", "Wood"), year2=1990, 
                                            data1=State_data, data2=State_data)
  
  #input receivers from front end 
  observeEvent({
    input$State1
    input$Energy1
    input$Year1
    input$State2
    input$Energy2
    input$Year2}, 
    
    {
      
      # all actions are duplicated for for both states
      selected_choices_States$data1 <- State_data
      selected_choices_States$data2 <- State_data
      
      
      #same as earlier, manipulate the all so that it doesn't overlap with the rest of the selection,
      if('All' == input$Energy1) {
        selected_choices_States$energy1 <- c("Coal", "Geothermal", "Hydro", "Natural Gas", "Nuclear", "Petroleum", "Solar", "Wind", "Wood")
      } else {
        selected_choices_States$energy1 <- duplicate(input$Energy1, shallow = FALSE)
      }
      
      if('All' == input$Energy2) {
        selected_choices_States$energy2 <- c("Coal", "Geothermal", "Hydro", "Natural Gas", "Nuclear", "Petroleum", "Solar", "Wind", "Wood")
      } else {
        selected_choices_States$energy2 <- duplicate(input$Energy2, shallow = FALSE)
      }
      
      #filter out sources
      selected_choices_States$data1 <- selected_choices_States$data1 %>% filter_all(any_vars(. %in% selected_choices_States$energy1))
      selected_choices_States$data2 <- selected_choices_States$data2 %>% filter_all(any_vars(. %in% selected_choices_States$energy2))
      
      
      #Convert to DC and US-TOTAL by hand
      if(input$State1 == "Washington DC") {
        selected_choices_States$state1 = "DC"
      } else if (input$State1 == "Total US") {
        selected_choices_States$data1 <- US_Total_data
        selected_choices_States$state1 = "US-TOTAL"
      } else {
        selected_choices_States$state1 = state.abb[grep(input$State1, state.name)]
      }
      
      
      
      if(input$State2 == "Washington DC") {
        selected_choices_States$state2 = "DC"
      } else if (input$State2 == "Total US") {
        selected_choices_States$data2 <- US_Total_data
        selected_choices_States$state2 = "US-TOTAL"
      } else {
        selected_choices_States$state2 = state.abb[grep(input$State2, state.name)]
      }
 
      
      #apply state filter
      selected_choices_States$data1 <- selected_choices_States$data1[selected_choices_States$data1$STATE == selected_choices_States$state1,]
      selected_choices_States$data2 <- selected_choices_States$data2[selected_choices_States$data2$STATE == selected_choices_States$state2,]

      
      #if the state selection is US-TOTAL then we already have the percentage so no need to calculate it again but if not then we can 
      if (selected_choices_States$state1 != "US-TOTAL") {
        copy_data <- selected_choices_States$data1
        percentage <- c()
        for (i in seq(from=1990, to=2019, by=1)) {
          temp<-selected_choices_States$data1[selected_choices_States$data1$YEAR==i,]
          den <- sum(temp$`GENERATION (Megawatthours)`) / 100
          for(j in seq(from=1, to=nrow(temp), by=1)) {
            percentage <- c(percentage, temp$`GENERATION (Megawatthours)`[j]/den)
          }
        }
        
        copy_data$Percent <- percentage
        selected_choices_States$data1 <- copy_data
      }
      
      
      if (selected_choices_States$state2 != "US-TOTAL") {
        copy_data <- selected_choices_States$data2
        percentage <- c()
        for (i in seq(from=1990, to=2019, by=1)) {
          temp<-selected_choices_States$data2[selected_choices_States$data2$YEAR==i,]
          den <- sum(temp$`GENERATION (Megawatthours)`) / 100
          for(j in seq(from=1, to=nrow(temp), by=1)) {
            percentage <- c(percentage, temp$`GENERATION (Megawatthours)`[j]/den)
          }
        }
        copy_data$Percent <- percentage
        selected_choices_States$data2 <- copy_data
      }
      
      
      
      #take year filter and distinguish long term all from individual year and apply filters as necessary
      if ('All' == input$Year1) {
        selected_choices_States$year1 = unique(finalized_data$YEAR)
        
      } else {
        selected_choices_States$year1 = as.numeric(input$Year1)    
        selected_choices_States$data1 <- selected_choices_States$data1[selected_choices_States$data1$YEAR == selected_choices_States$year1,]
      }
      
      if ('All' == input$Year2) {
        selected_choices_States$year2 = unique(finalized_data$YEAR)
        
      } else {
        selected_choices_States$year2 = as.numeric(input$Year2)    
        selected_choices_States$data2 <- selected_choices_States$data2[selected_choices_States$data2$YEAR == selected_choices_States$year2,]
      }
      
      
    })
  
  
  #plot outputs for 2nd tab 
  
  #state 1 stacked box
  output$State1_Stacked <- renderPlot({
    ggplot(selected_choices_States$data1, aes(x = YEAR, y = `GENERATION (Megawatthours)`, fill = `ENERGY SOURCE`)) + 
      geom_bar(position="stack",
               stat = "identity") +
      xlab("\nYEAR") +
      ylab("GENERATION (Megawatthours)\n") + 
      labs(fill="ENERGY SOURCE") + scale_fill_manual(values=mappedColors, aesthetics = "fill", drop=FALSE) 
  })
  
  #state 1 stacked box percent
  output$State1_Stacked_Percent <- renderPlot({
    ggplot(selected_choices_States$data1, aes(x = YEAR, y = `GENERATION (Megawatthours)`, fill = `ENERGY SOURCE`)) + 
      geom_bar(position="fill",
               stat = "identity") +
      xlab("\nYEAR") +
      ylab("GENERATION (Megawatthours)\n") + 
      labs(fill="ENERGY SOURCE") + scale_fill_manual(values=mappedColors, aesthetics = "fill", drop=FALSE) 
  })
  
  #state 1 line 
  output$State1_Line <- renderPlot({
    ggplot(selected_choices_States$data1, aes(x = YEAR, y = (`GENERATION (Megawatthours)`), color = `ENERGY SOURCE`)) + 
      geom_line() +
      xlab("\nYEAR") +
      ylab("GENERATION (Megawatthours) \n") + 
      labs(fill="ENERGY SOURCE") + scale_fill_manual(values=mappedColors, aesthetics = "fill", drop=FALSE) 
  })
  
  #state 1 line percent 
  output$State1_Line_Percent <- renderPlot({
    ggplot(selected_choices_States$data1, aes(x = YEAR, y = Percent, color = `ENERGY SOURCE`)) + 
      geom_line() +
      xlab("\nYEAR") +
      ylab("GENERATION (Megawatthours) %\n") + 
      labs(fill="ENERGY SOURCE") + scale_fill_manual(values=mappedColors, aesthetics = "fill", drop=FALSE) 
    
  })
  
  #state 1 raw
  output$State1_Raw <- renderDataTable({
    
    selected_choices_States$data1[c(-3)]
    
  })
  
  
  #state 2 plots
  
  #state 2 stacked box
  output$State2_Stacked <- renderPlot({
    ggplot(selected_choices_States$data2, aes(x = YEAR, y = `GENERATION (Megawatthours)`, fill = `ENERGY SOURCE`)) + 
      geom_bar(position="stack",
               stat = "identity") +
      xlab("\nYEAR") +
      ylab("GENERATION (Megawatthours)\n") + 
      labs(fill="ENERGY SOURCE") + scale_fill_manual(values=mappedColors, aesthetics = "fill", drop=FALSE) 
  })
  
  #state 2 stacked box percent
  output$State2_Stacked_Percent <- renderPlot({
    ggplot(selected_choices_States$data2, aes(x = YEAR, y = `GENERATION (Megawatthours)`, fill = `ENERGY SOURCE`)) + 
      geom_bar(position="fill",
               stat = "identity") +
      xlab("\nYEAR") +
      ylab("GENERATION (Megawatthours)\n") + 
      labs(fill="ENERGY SOURCE") + scale_fill_manual(values=mappedColors, aesthetics = "fill", drop=FALSE) 
  })
  
  #state 2 line plot
  output$State2_Line <- renderPlot({
    ggplot(selected_choices_States$data2, aes(x = YEAR, y = (`GENERATION (Megawatthours)`), color = `ENERGY SOURCE`)) + 
      geom_line() +
      xlab("\nYEAR") +
      ylab("GENERATION (Megawatthours) \n") + 
      labs(fill="ENERGY SOURCE") + scale_fill_manual(values=mappedColors, aesthetics = "fill", drop=FALSE) 
  })
  
  #state 2 line plot percent
  output$State2_Line_Percent <- renderPlot({
    ggplot(selected_choices_States$data2, aes(x = YEAR, y = Percent, color = `ENERGY SOURCE`)) + 
      geom_line() +
      xlab("\nYEAR") +
      ylab("GENERATION (Megawatthours) %\n") + 
      labs(fill="ENERGY SOURCE") + scale_fill_manual(values=mappedColors, aesthetics = "fill", drop=FALSE) 
    
  })
  
  #state 2 raw
  output$State2_Raw <- renderDataTable({
    
    selected_choices_States$data2[c(-3)]
    
  })
  
  
  
  #setup data for tab 3
  selected_choices_heatmap <- reactiveValues(energy1="Coal", energy2="Coal", year1=1990, year2=1990, data1=finalized_data, data2=finalized_data)
  
  #read in user input 
  observeEvent({
    input$EnergySource1
    input$ComparisonYear1
    input$EnergySource2
    input$ComparisonYear2}, 
    
    {
      
      #set up data and filter the incoming user input for energies, again everything codewise is duplicated but tweaked to read from the proper places
      selected_choices_heatmap$data1 <- finalized_data[finalized_data$`TYPE OF PRODUCER`=="Total Electric Power Industry",]
      selected_choices_heatmap$data2 <- finalized_data[finalized_data$`TYPE OF PRODUCER`=="Total Electric Power Industry",]
      
      selected_choices_heatmap$energy1 = duplicate(input$EnergySource1, shallow = FALSE) 
      selected_choices_heatmap$energy2 = duplicate(input$EnergySource2, shallow = FALSE) 
      
      
      selected_choices_heatmap$data1 <- selected_choices_heatmap$data1 %>% filter_all(any_vars(. %in% selected_choices_heatmap$energy1))
      selected_choices_heatmap$data2 <- selected_choices_heatmap$data2 %>% filter_all(any_vars(. %in% selected_choices_heatmap$energy2))
      
      #transfer data for to remove and add columns
      data1dup <- selected_choices_heatmap$data1
      data2dup <- selected_choices_heatmap$data2
      
      
      #remove dc and us-total for plotting 
      data1dup <- data1dup[data1dup$STATE != "US-TOTAL",]
      data1dup <- data1dup[data1dup$STATE != "DC",]
      
      data2dup <- data2dup[data2dup$STATE != "US-TOTAL",]
      data2dup <- data2dup[data2dup$STATE != "DC",]
      
      #read in year and retrieve data for the year
      data1dup <- data1dup[data1dup$YEAR == input$ComparisonYear1,]
      data2dup <- data2dup[data2dup$YEAR == input$ComparisonYear2,]
      
      #get state names for plotting 
      data1dup$state <- state.name[match(data1dup$STATE, state.abb)]
      data2dup$state <- state.name[match(data2dup$STATE, state.abb)]

      
      #calculate percentages for data1 and data2
      den <- sum(data1dup$`GENERATION (Megawatthours)`[1:(1+(nrow(data1dup) - 1))]) / 100
      percentage <- (data1dup$`GENERATION (Megawatthours)`[1:(1 +(nrow(data1dup) - 1))])/den
      data1dup$Percent <- percentage
      data1dup$region <- tolower(data1dup$state)

      
      den <- sum(data2dup$`GENERATION (Megawatthours)`[1:(1+(nrow(data2dup) - 1))]) / 100
      percentage <- (data2dup$`GENERATION (Megawatthours)`[1:(1 +(nrow(data2dup) - 1))])/den
      data2dup$Percent <- percentage
      data2dup$region <- tolower(data2dup$state)
    
      
      #transfer back and continue  
      selected_choices_heatmap$data1 <- data1dup 
      selected_choices_heatmap$data2 <- data2dup 
     
    })
  
  
  
  #heatmap for energy 1 regular 
  output$heatmap1 <- renderPlot({
    plot_usmap(regions="states", data=selected_choices_heatmap$data1, values="GENERATION (Megawatthours)", labels=T) +   labs(fill = 'GENERATION (Megawatthours)') + scale_fill_gradientn(colours=rev(heat.colors(15)), label = scales::comma, limits=c(0,local_max <- max(selected_choices_heatmap$data1$`GENERATION (Megawatthours)`)),
                                                                                                                                                                                          guide = guide_colourbar(barwidth = 25, rheight = 1,
                                                                                                                                                                                                                  title.position = "top")) + theme(legend.position = "bottom",
                                                                                                                                                                                                                                                   legend.title=element_text(size=12),
                                                                                                                                                                                                                                                   legend.text=element_text(size=10))
    
    
    
    
  })
  
  #heatmap for energy 1 percent
  output$heatmap_percent1 <- renderPlot({
    plot_usmap(regions="states", data=selected_choices_heatmap$data1, values="Percent", labels=T) +   labs(fill = 'GENERATION %') + scale_fill_gradientn(colours=rev(heat.colors(15)), label = scales::comma, limits=c(0,local_max <- max(selected_choices_heatmap$data1$`Percent`)),
                                                                                                                                                         guide = guide_colourbar(barwidth = 25, rheight = 1,
                                                                                                                                                                                 title.position = "top")) + theme(legend.position = "bottom",
                                                                                                                                                                                                                  legend.title=element_text(size=12),
                                                                                                                                                                                                                  legend.text=element_text(size=10))
    
   })
  
  
  #heatmap for energy 2 regular 
  output$heatmap2 <- renderPlot({
    plot_usmap(regions="states", data=selected_choices_heatmap$data2, values="GENERATION (Megawatthours)", labels=T) +   labs(fill = 'GENERATION (Megawatthours)') + scale_fill_gradientn(colours=rev(heat.colors(15)), label = scales::comma, limits=c(0,local_max <- max(selected_choices_heatmap$data2$`GENERATION (Megawatthours)`)),
                                                                                                                                                                                          guide = guide_colourbar(barwidth = 25, rheight = 1,
                                                                                                                                                                                                                  title.position = "top")) + theme(legend.position = "bottom",
                                                                                                                                                                                                                                                   legend.title=element_text(size=12),
                                                                                                                                                                                                                                                   legend.text=element_text(size=10))
    
    
    })
  
  #heatmap for energy 2 percent
  output$heatmap_percent2 <- renderPlot({
    plot_usmap(regions="states", data=selected_choices_heatmap$data2, values="Percent", labels=T) +   labs(fill = 'GENERATION %') + scale_fill_gradientn(colours=rev(heat.colors(15)), label = scales::comma, limits=c(0,local_max <- max(selected_choices_heatmap$data2$`Percent`)),
                                                                                                                                                         guide = guide_colourbar(barwidth = 25, rheight = 1, 
                                                                                                                                                                                 title.position = "top")) + theme(legend.position = "bottom",
                                                                                                                                                                                                                  legend.title=element_text(size=12),
                                                                                                                                                                                                                  legend.text=element_text(size=10))
    
    })
  
  #final print out for about page
  output$AboutOut <- renderText({
    "Created by: Vivek Bhatt\n
         Created: 2/13/2021\n
         Data Source: https://www.eia.gov/electricity/data/state/\n
         Intended for visualizing the change in America's energy source over time as to get a better idea of how energy production has grown and what energy sources have become important."   
  })
  
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
