#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(scales)
library(tidyverse)
library(usmap)
library(ggrepel)
library(rpart)
library(rpart.plot)
library(reticulate)
# Maptree library can change the font size in decision trees
#install.packages("maptree")
#library(maptree)

#setwd("~/PycharmProjects/***DirectedResearch/Directed_Research_R/directed_research_dashboard")
#setwd("~/Directed_Research_R")
#source_python('~/PycharmProjects/***DirectedResearch/Directed_Research_R/scraping.py')
#source_python('scraping.py')
#source_python('preprocessing.py')
#source('cleaning.R')
#source('inspection.R')
load("data/opps.RData")
state_count <- opps_inspect %>%
  count(locationState) %>%
  rename(CountOfStateProvide = n) %>%
  arrange(desc(CountOfStateProvide)) 

summary(opps_inspect$fromDate)
summary(opps_inspect$toDate)

glimpse(opps_for_ohe)
opps_tree <- opps_for_ohe %>%
  select(-OpportunityID, -title, -description, -fromDate, -toDate, -cost,
         -reach, -scholarship, -locationCity, -locationPostalCode, 
         -locationLatitude, -locationLongitude, -excerpt)

opps_tree$typeOfOpportunity <- as.factor(opps_tree$typeOfOpportunity)
opps_tree$locationState <- as.factor(opps_tree$locationState)


summary(opps_tree$typeOfOpportunity)

opps_tree <- cbind(opps_tree, cost = opps_inspect$cost)


save(opps_total, opps, opps_for_ohe, opps_inspect, opps_tree,  
     file = "data/opps.RData")
set.seed(203)
print(nrow(opps_inspect))

# Create a vector of 75% randomly sample rows from the orighinal dataset
sampleSet <- sample(nrow(opps_tree),
                    round(nrow(opps_tree) * 0.75),
                    replace = FALSE)
# Put the records from the 75% sample into mobilePhoneTraining
opps_tree_Training <- opps_tree[sampleSet, ]
# Put the records from the 25% sample into mobilePhoneTesting
opps_tree_Testing <- opps_tree[-sampleSet, ]

opps_inspect %>%
  filter(StartDate >= "01-08-2021" & EndDate <= "12-12-2022") %>%
  group_by(locationState) %>%
  count(locationState)

filePath <- './data/all_opportunities_df.pkl'
dirname(filePath)
basename(filePath)
print(file.info(filePath))
fetched_date <- file.info(filePath)$mtime
fetched_date <- as.Date(fetched_date)
fetched_date <- format(as.Date(fetched_date), "%d-%m-%Y")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Informal STEM Programs in the US"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          #h3("Information of Programs which are provided from 2019 to 2023"),
          p("The data fetched on", fetched_date, "from",
            HTML('<a href="https://theconnectory.org/find-opportunity/">TheConnectory</a>'),
            "with a total of",nrow(opps_inspect), "STEM programs"),
          p("Navigate through the different visualization using the tabs."),
          
        ),

        # Show a plot of the generated distribution
        mainPanel(
          tabsetPanel(
            tabPanel("US_Map",
                     dateRangeInput("dates",
                                    "Date range:",
                                    format = "dd-mm-yyyy",
                                    start = "2022-08-01",
                                    end = Sys.Date()),
                     helpText(em("The", strong("Date range"), "here will sort 
                                 the starting date and ending date of opportunities.")),
                     plotOutput("mapPlot")),
            tabPanel("Distribution of opportunity of states provide", 
                     sliderInput("bars",
                                 "Number of bars:",
                                 min = 1,
                                 max = 51,
                                 value = 15),
                     plotOutput("barPlot")),
            tabPanel("Distribution of opportunity within a state",
                     selectInput("state",
                                 "Select state:",
                                 choices = opps_inspect %>%  
                                   distinct(locationState) %>% 
                                   pull(locationState)
                     ),
                     sliderInput("bars_city",
                                 "Number of bars",
                                 min = 1,
                                 max = 20,
                                 value = 10),
                     plotOutput("barPlot2")),
            tabPanel("Distribution of cost within cities in a state",
                     selectInput("State",
                                 "Select state:",
                                 choices = opps_inspect %>%  
                                   distinct(locationState) %>% 
                                   pull(locationState)
                     ),
                     plotOutput("barPlot3")),
            tabPanel("Distribution of area of interest within a state",
                     selectInput("states",
                                 "Select state:",
                                 choices = opps_inspect %>%  
                                   distinct(locationState) %>% 
                                   pull(locationState)
                     ),
                     sliderInput("bars_interest",
                                 "Number of bars",
                                 min = 1, 
                                 max = 18, 
                                 value =9),
                     plotOutput("barPlot4")),
            tabPanel("Distribution of languages with in a state",
                     selectInput("SelectState",
                                 "Select state:",
                                 choices = opps_inspect %>%  
                                   distinct(locationState) %>% 
                                   pull(locationState),
                                 selected = "California"),
                     plotOutput("PieChart")),
            tabPanel("Predictive model: Decision Tree",
                     selectInput("attention",
                                 "Type of participant:",
                                 choice = opps_tree %>%
                                   select(-locationState, -typeOfOpportunity) %>%
                                   select(contains("Att_")) %>%
                                   colnames(),
                                 selected = "Att_GTS"),
                     helpText(em(" Note: choices in the type of participant means
                                 that the dependent variable which the predictive 
                                 model is going to predict.", br(),
                                 "Att_Boys means participants are boys.", br(),
                                 "Att_Girls means participants are girls.", br(),
                                 "Att_GTS means participants who are gifted and 
                                 talented students.", br(), 
                                 "Att_None means not mentioning the participant.",
                                 br(),
                                 "Att_SRDos means participants who are students 
                                 at risk of dropping out of school.", br(),
                                 "Att_SwDis means the participants are students 
                                 with disabilities.")),
                     selectInput("cp",
                                 "Complexity of decision tree:",
                                 choices = c("0.01", "0.0008"),
                                 selected = "0.01"),
                     h4("Predictive Accuracy:", strong(textOutput("prediction"))),
                     #strong(textOutput("prediction")),
                     plotOutput("DecisionTree")
                     )
            
            
        )
    )
)
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$barPlot <- renderPlot(width = 1000,
                               height = 500,{
    # generate bins based on input$bins from ui.R
    state_count %>%
      arrange(desc(CountOfStateProvide)) %>%
      head(input$bars) %>%
      ggplot(aes(y = reorder(locationState, CountOfStateProvide),
                 x = CountOfStateProvide))+ 
      geom_col(alpha = .5,
               fill="Red")+
      geom_text(aes(label = CountOfStateProvide),
                hjust = +1,
                fontface = "bold",
                color="Blue",
                size =5)+
      scale_x_continuous(limits = c(0,nrow(opps_inspect)/2),
                         breaks = seq(0, nrow(opps_inspect)/2, by = 20))+
      theme_classic(base_size = 20)+
      theme_linedraw()+
      labs(title = paste("The top",input$bars,"states provide opportunties"),
           y = "state", 
           x = "value")
  })
  output$barPlot2 <- renderPlot(width = 1000,
                                height = 500,{
    opps_inspect %>%
      filter(locationState == input$state) %>%
      group_by(locationCity)%>%
      count(locationCity) %>%
      rename(CountOfCityProvide = n) %>%
      arrange(desc(CountOfCityProvide)) %>%
      head(input$bars_city) %>%
      ggplot(aes(y = reorder(locationCity,CountOfCityProvide),
                 x = CountOfCityProvide)) +
      geom_col(fill = "#D55E00")+ 
      geom_text(aes(label = CountOfCityProvide),
                hjust = +1,
                fontface = "bold",
                color="Black",
                size =5)+
      scale_x_continuous(limits = c(0,nrow(opps_inspect)/5))+
      theme_linedraw()+
      labs(title = paste("Top ",input$bars_city,"Cities provide opportunties in",
                         input$state),
           y = "City", 
           x = "value")
    
    
  })
  output$barPlot3 <- renderPlot(width = 1000,
                                height = 500,{
    opps_inspect %>%
      filter(locationState == input$State) %>%
      group_by(locationCity, cost)%>%
      count(locationCity) %>%
      rename(count = n) %>%
      arrange(desc(count)) %>%
      ggplot(aes(y = reorder(locationCity,count),
                 x = count,
                 fill = cost)) +
      geom_col()+
      scale_x_continuous(limits = c(0,nrow(opps_inspect)/5),
                         breaks = seq(0, nrow(opps_inspect)/5, by = 10)) +
      theme_linedraw()+
      labs(y = paste("Cities of",input$State))
  })
  
  output$mapPlot <- renderPlot(width = 1200,
                               height = 600,{
    
    opps_inspect %>%
      filter(StartDate >= input$dates[1] & EndDate <= input$dates[2]) %>%
      group_by(locationState) %>%
      count(locationState) %>%
      rename(state = locationState) %>%
      plot_usmap(data = .,
                 values = "n", 
                 labels = TRUE,
                 label_color = "blue",
                 size=1,
                 color = "white") +
      theme(legend.position = "right") +
      scale_fill_continuous(name = "Opportunies",
                            low = "cornsilk", 
                            high = 'salmon4')+ 
      labs(caption = "Grey: when data is not available") +
      theme(plot.caption = element_text(size=18, face = "italic", 
                                        color = "gray44",hjust = 1),
            plot.caption.position = "plot") +
      theme(legend.text=element_text(size=18),
            legend.key.size = unit(1, 'cm'),
            legend.title=element_text(size=18, face="bold"),
            legend.direction = "horizontal")

  })

  output$barPlot4 <- renderPlot(width = 1000,
                                height = 500,{
    opps_for_ohe %>%
      filter(locationState == input$states) %>%
      select(contains("AoI_")) %>%
      colSums() %>%
      as.data.frame() %>%
      rownames_to_column() %>%
      rename('areaofinterest' = 'rowname', 'counts'='.') %>%
      arrange(desc(counts))%>%
      head(input$bars_interest) %>%
      ggplot(aes(y = reorder(areaofinterest,counts),
                 x = counts))+ 
      geom_col(alpha = .5,
               fill="Red") +
      geom_text(aes(label = counts),
                hjust = +1,
                fontface = "bold",
                color="Blue",
                size =5)+
      scale_x_continuous(limits = c(0,nrow(opps_inspect)/2),
                         breaks = seq(0, nrow(opps_inspect)/2, by = 20))+
      theme_linedraw()+
      labs(title =paste("The top",input$bars_interest,
                        "area of interest provided by", input$states),
           y = "Area of Interest")
      
  })
  f <- reactive({
    as.formula(paste(input$attention, "~."))
  })
  DecisionTree_Model <- reactive({
    rpart(f(),
          method = "class",
          cp = input$cp,
          data = opps_tree_Training)
  })
  output$DecisionTree <- renderPlot(width = 1000,
                                    height = 900,{
    rpart.plot(DecisionTree_Model())
  })
  Prediction <- reactive({
    predict(DecisionTree_Model(),
            opps_tree_Testing,
            type = "class")
  })
  variable <- reactive({
    getSymbols(input$attention)
  })
  ConfusionMatrix <- reactive({
    table(opps_tree_Testing$Att_Boys, 
          Prediction())
  })
  Accuracy <- reactive({
    sum(diag(ConfusionMatrix())) /
      nrow(opps_tree_Testing)
  })
  output$prediction <- renderText(
    Accuracy()
  )
  output$PieChart <- renderPlot(width = 1000,
                                height = 500,{
    language <- opps_tree %>%
      filter(locationState==input$SelectState) %>%
      select(locationState, contains("L_"))%>%
      summarize_if(is.numeric, sum, na.rm=TRUE) 
    
    new <- as.data.frame(t(language)) %>%
      rownames_to_column()
    
    colnames(new) <- c("language", 'value')
    
    new$language <- as.factor(new$language)
    
    sum <- sum(new$value)
    new <- new %>%
      mutate(percentage = round(value/ sum,2) * 100) %>%
      filter(new$value!=0) 
    
    new %>%
      ggplot(aes(x = "", 
                 y = value,
                 fill=reorder(language, value),
                 label=percentage)) +
      geom_bar(stat="identity", width=5, size=3, color = "white")+
      geom_label_repel(aes(label = paste0(percentage, "%")), 
                position = position_stack(vjust = .5), 
                size=8, 
                label.padding = unit(0.25, "lines"),
                show.legend = FALSE) +
      coord_polar("y") +
      scale_fill_brewer(name="Language", palette="Set3")+
      theme_void()+
      theme(legend.text=element_text(size=18))+
      guides(fill=guide_legend(title.theme = element_text(face="bold",size=18)))
  })
 
}

# Run the application 
shinyApp(ui = ui, server = server)
