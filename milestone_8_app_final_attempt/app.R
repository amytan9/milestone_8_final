
library(shiny)
library(shinyWidgets)
library(fs)
library(shinythemes)
library(moderndive)
library(gt)
library(plotly)
library(ggplot2)
library(tidyverse)

# reading in merged dataset from the prior data cleaning session in Rmd

data <- read_csv("joined_data2.csv")

# I made the county name column into a vector so that I can input all the names 
# at the same time into the drop-down bar later instead of manually writing a list

countiesVector <- data$countyname %>% unique()

# I needed to create a new mean of mean scores variable because the scores varied within county,
# but the predictor variables I am using later in the model does not

clean_data <- data %>% group_by(countyid) %>% mutate(mean_mn_all = mean(mn_all))

# Define UI for application that draws a histogram

ui <- fluidPage(
  
  # flatly is a nice theme to make the tabs look put together
  
  # Application title
  
  navbarPage("Academic Achievement In The U.S.", theme = shinytheme("flatly"),
             
             # remember that tab panel is for what shows up in the tab, and title panel is 
             # for what shows up at the top of the page within the tab                  
             
             tabPanel("Scores by County",
                      titlePanel("Test Scores by County"),
                      
                      # Sidebar with a slider input for number of bins 
                      
                      sidebarLayout(
                        sidebarPanel(
                          
                          # this is the actual inputID that you want to keep constant in the code
                          
                          pickerInput("selectCounty",
                                      
                                      # this is what would show up in the app
                                      
                                      "Select County",
                                      
                                      # this gives a list of all the choices that users can choose from in the drop-down menu
                                      
                                      choices = countiesVector,
                                      
                                      # this is just to define what things to list 
                                      
                                      options = list("actions-box" = TRUE),
                                      
                                      # this sets the default option that shows up when nothing is picked to the first
                                      # item on the countiesVector
                                      
                                      selected = countiesVector[1],
                                      
                                      # this allows users to choose multiple counties to compare
                                      
                                      multiple = TRUE
                          )
                        ),
                        
                        # Show a plot of the generated distribution
                        
                        mainPanel(
                          tabsetPanel(id = "tabsMain",
                                      tabPanel("Plot",
                                               
                                               # this tells the UI what plot to put 
                                               
                                               plotlyOutput("plot1")
                                      )
                          )
                        )
                      )
             ),
             
             # repeat the same tabPanel code for other tabs, changing the titles
             
             tabPanel("Gender Differences in Scores",
                      titlePanel("States with Highest Scores by Gender"),
                      
                      # Show a plot of the generated distribution
                      
                      tabsetPanel(id = "tabsMain",
                                  tabPanel("Plot",
                                           plotOutput("plot2"),
                                           p("This is a plot of the top 10 states with the highest mean female scores across the country."),
                                           br(),
                                           plotOutput("plot3"),
                                           p("This is a plot of the top 10 states with the highest mean male scores across the country."),
                                           br(),
                                           p("Comparing both of these plots, there are 8 states that have high scores for both females and males. Some differences 
                                             between the two plots are that Maryland and Pennsylvania appear in the top 10 states for females, and Kansas and Indiana
                                             appear in the top 10 states for males.")
                                           
                                           )
                                  )
                      
                      ),
             
             tabPanel("Model of Scores",
                      titlePanel("Model of Scores by Household Income and Single-Parent Household"),
                      
                      # Show a plot of the generated distribution
                      
                      tabsetPanel(id = "tabsMain",
                                  tabPanel("Model",
                                           
                                           # here, instead of calling plotlyOutput, we want to call tableOutput since the model
                                           # results are displayed in a summary statistics table
                                           
                                           tableOutput("model"),
                                           p("This is the summary statistics table for a linear regression of mean test scores based 
                                             on the mean household income in the county (hhinc_mean2000) and the proportion of single-parent households
                                             (singleparent_share2000) in the county. At first, it was a bit surprising that the coefficient for mean household
                                             income is 0 since it seems unlikely that household income does not have an effect on
                                             academic achievement. However, it makes sense because since household income is in terms
                                             of dollars, each additional dollar has a very small effect on mean scores, especially when
                                             the scores are standardized the way they are. For further analysis, I can make the units for 
                                             household income in terms of 10,000 dollars instead of one dollar. For the share of single-
                                             parent households, there is a coefficient for -1.88, meaning that for every .1 increase in
                                             the proportion of single-parent households, there is a -.188 decrease in the mean of mean
                                             scores in the county on average. Both variables have a very small standard error, which says
                                             that these variables model the scores very well. The p-values are also smaller than .05,
                                             which means that at the 95% significance level, the effects of both variables on mean scores
                                             are statistically significant and not due to random chance.")
                                           
                                           )
                                  )
                                  ),
             
             tabPanel("About",
                      titlePanel("About This Project"),
                      p("In this project, I used two datasets to figure out if there are any interesting correlations between economic variables and educational variables at the county level nationwide. 
                        How do students' broader social environments impact their academic achievement? I attempt to answer this question by getting the county-level covariates dataset from the Opportunity
                        Insights website (https://opportunityinsights.org/data/) and the stanforddata dataset from the Stanford Education Data Archive (https://exhibits.stanford.edu/data/catalog/db586ns4974).
                        The covariates dataset describes economic and social factors of each county, such as rates of intergenerational mobility and umemployment rates. The stanforddata dataset describes 
                        various academic variables for 3rd-8th graders across the nation by tract as well. It contains variables for academic achievement and student demographics. I merged these two datasets
                        together by their FIPS code. I also descriptively analyzed each dataset to find trends within the datasets themselves, such as the correlation between household income and test scores."),
                      br(),
                      p("You can find more of my work on Github: https://github.com/amytan9")
                      
                      )
             
             )
  
                                  )


# Define server logic required to draw a histogram

server <- function(input, output) {
  
  # use plotly to allow hover text to appear on plots, giving more info about the point
  
  output$plot1 <- renderPlotly({
    
    ggplotly(data %>%
               
               # first we want the ggplot to display data by county based on which
               # county(s) the user picks
               
               filter(countyname %in% input$selectCounty) %>% 
               
               # then, we group by countyname and year to set each data point as results
               # for each county by year
               
               group_by(countyname, year) %>% 
               
               # since there is variation within each county for scores, I just wanted 
               # to take the mean of each score within the county to set that as the
               # county score
               
               summarize(mean_score = mean(mn_all)) %>% 
               
               # i ungrouped to make sure that the data is back in its original form
               
               ungroup() %>% 
               
               # remember that group allows you to create graphs for each county separately
               
               # the color can also be varied to show each county's line on the same graph
               
               ggplot(aes(x = year, y = mean_score, group = countyname, color = countyname)) +
               geom_point() +
               
               # this connects all the points
               
               geom_line() +
               labs(title = "Mean Scores over Time",
                    y = "Mean Score",
                    x = "Year",
                    fill = "County Name",
                    subtitle = "Data from 3rd-8th graders in U.S. counties"
               )) %>% 
      
      # it's possible to add source notes to plotly using this annotations argument
      
      layout(annotations = list(text = "Data from the Stanford Education Data Archive, 
                                https://edopportunity.org/get-the-data/seda-archive-downloads/", 
                                showarrow = F, 
                                xref = 'paper', x = 1, 
                                
                                # for some reason, I can't change the position of y, or else it disappears
                                
                                yref = 'paper', y= -0.1,
                                
                                # xanchor sets which corner the text should be, but can't change
                                # yanchor to move the text down for some reason
                                
                                xanchor = 'right', yanchor = 'center', 
                                xshift = 0, yshift = -1,
                                font = list(size = 5, color = "black")))
    
  })
  
  output$plot2 <- renderPlot({
    data %>%
      
      # want to show mean scores for each gender by state
      
      group_by(stateabb) %>%
      
      #getting rid of all scores that are missing
      
      filter(!is.na(mn_fem)) %>%
      filter(!is.na(mn_mal)) %>%
      
      # want to take the mean of all female and male scores by state
      
      summarize(avg_female = mean(mn_fem), avg_male = mean(mn_mal)) %>%
      
      # want to show the states with the top scores
      
      arrange(desc(avg_female)) %>% 
      
      # to simply the graph, only show the first 10
      
      head(10) %>%
      
      # do a separate ggplot for avergae female scores by state
      
      ggplot(aes(x=reorder(stateabb, -avg_female), y=avg_female)) +
      
      # remember that the way to change the color of the bars in a 
      # geom_col is to put it directly here instead of aes()
      
      geom_col(fill= "pink") +
      labs(title = "States with Highest Mean Scores for Females",
           y = "Mean Scores",
           x = "States",
           caption = "Data from the Stanford Education Data Archive, 
           https://edopportunity.org/get-the-data/seda-archive-downloads/",
           subtitle = "Data from 3rd-8th graders in U.S. counties"
      )
    
  })
  
  #doing the same exact plotly code for the males
  
  output$plot3 <- renderPlot({
    data %>%
      group_by(stateabb) %>%
      filter(!is.na(mn_fem)) %>%
      filter(!is.na(mn_mal)) %>%
      summarize(avg_female = mean(mn_fem), avg_male = mean(mn_mal)) %>%
      arrange(desc(avg_male)) %>% 
      head(10) %>%
      ggplot(aes(x=reorder(stateabb, -avg_male), y=avg_male)) +
      geom_col(fill= "blue") +
      labs(title = "States with Highest Mean Scores for Males",
           y = "Mean Scores",
           x = "States",
           caption = "Data from the Stanford Education Data Archive, 
           https://edopportunity.org/get-the-data/seda-archive-downloads/",
           subtitle = "Data from 3rd-8th graders in U.S. counties"
      )
    
  })
  
  # for my model, I want to create a regression table that shows the results of the model
  
  output$model <- renderTable({
    
    # an easy way to get this table is to use this and directly put in the lm model
    
    # this is modeling mean scores using household income and proportion 
    # of single parent households via a linear regression
    
    get_regression_table(lm(data = clean_data, mean_mn_all ~ hhinc_mean2000 + singleparent_share2000)) 
  })
}

# Run the application 

shinyApp(ui = ui, server = server)

