

library(tidyr)
library(reshape2)
library(janitor)
library(gt)
library(googlesheets4)
library(infer)
library(fs)
library(rstanarm)
library(reprex)
library(cowplot)
library(stringr)
library(broom)
library(ggridges)
library(dplyr)
library(shiny)
library(tidyr)
library(shinyWidgets)
library(fs)
library(shinythemes)
library(moderndive)
library(gt)
library(plotly)
library(ggplot2)
library(maps)
library(stargazer)
library(tidyverse)



# reading in merged dataset from the prior data cleaning session in Rmd

data <- read_csv("joined_data2.csv")

# I made the county name column into a vector so that I can input all the names 
# at the same time into the drop-down bar later instead of manually writing a list

countiesVector <- data$countyname %>% unique()

# I needed to create a new mean of mean scores variable because the scores varied within county,
# but the predictor variables I am using later in the model does not

clean_data <- data %>% group_by(countyid) %>% mutate(mean_mn_all = mean(mn_all)) %>%
  mutate(hhinc_k = hhinc_mean2000 / 10000) %>% 
  mutate(county_state = paste0(countyname, ", ", stateabb))

# creating a vector for the drop-down menu, so you can search for a specific county within a state

countystateVector <- clean_data$county_state %>% unique()


# read in R vector that labels state and region, 
# for easy categorization later by region if needed

state.fips

# I only want to take the labels for the region from this vector

region_fips <- state.fips %>% select(fips, region)

# I am merging the two datasets by FIPS code

clean_data_merged <- left_join(clean_data, region_fips, by = "fips")

# moving the countyid column to the front for easy viewing

clean_data_merged <- clean_data_merged %>% select(countyid, everything())

# I created this new variable just to be able to see the proportion of white people easily

clean_data_merged <- clean_data_merged %>% mutate(white_share2010 = (1 - nonwhite_share2010))

# I wanted to make sure that the data analysis on mean test score gaps can be done on a numeric value

clean_data$mn_mfg <- as.numeric(clean_data$mn_mfg)

# this subsets all the data to just 2010 in order to compare the test score variation among different 
# counties in one particular year for one grade with the share of different racial groups later

temp_ela <- clean_data_merged %>% filter(year == 2010, subject == "ela", grade == 8) 
temp_math <- clean_data_merged %>% filter(year == 2010, subject == "math", grade == 8) 

# doing one temp aggregated across both subjects (ended up using this measure)

temp <- clean_data_merged %>% filter(year == 2010, grade == 8) 

# I wanted to run a linear regression on both the household income variables and single-parent households
# because I was curious which one had a larger effect, controlling for the other

hhinc_parent_model <- lm(data = clean_data, mean_mn_all ~ hhinc_k + singleparent_share2000)

# this tidy function is part of the broom package and packages all the regression outputs nicely

# you can also change the confidence interval levels here

hhinc_summary <- tidy(hhinc_parent_model, conf.int=TRUE, conf.level = .95)

# it is possible render gt tables in Shiny, using gt_output

hhinc_table <- gt(hhinc_summary) %>% 
  tab_header(title = "Model of Test Scores by Mean Household Income and Share of Single-Parent Households",
             subtitle = "Data from 3rd-8th graders in 2010") %>% 
  
  # shortening the decimal places so the numbers are more digestible
  
  fmt_number(columns = vars("estimate", "std.error", "statistic", "p.value", "conf.low", "conf.high"), decimals = 3) %>%
  
  # making the columns look nicer by relabelling them
  cols_label("estimate" = "Coefficient", "std.error" = "Standard Error", "statistic" = "Statistic", "p.value" = "P-value", "conf.low" = "5th Percentile",
             "conf.high" = "95th Percentile") %>%  
  
  # adding source info
  
  tab_source_note(html("Data from Opportunity Insights and Stanford Education Data Archive."))

# I am going to make a similar regression model as above for each racial group. Here I am making one for Asians
# I am interested in seeing how the proportions of each racial group affects their respective students' 
# test scores

asn_model <- lm(data = temp, mn_asn ~ share_asian2010)
asn_summary <- tidy(asn_model, conf.int=TRUE, conf.level = .95)
asn_table <- gt(asn_summary) %>% 
  tab_header(title = "Relationship Between Share of Asians in County vs. Asian Students' Test Scores",
             subtitle = "Data from 8th graders in 2010") %>% 
  
  # shortening the decimal places so the numbers are more digestible
  
  fmt_number(columns = vars("estimate", "std.error", "statistic", "p.value", "conf.low", "conf.high"), decimals = 3) %>%
  
  # making the columns look nicer by relabelling them
  cols_label("estimate" = "Coefficient", "std.error" = "Standard Error", "statistic" = "Statistic", "p.value" = "P-value", "conf.low" = "5th Percentile",
             "conf.high" = "95th Percentile") %>%  
  
  # adding source info
  
  tab_source_note(html("Data from Opportunity Insights and Stanford Education Data Archive."))

# making the same linear regression model for blacks

blk_model <- lm(data = temp, mn_blk ~ share_black2010)
blk_summary <- tidy(blk_model, conf.int=TRUE, conf.level = .95)
blk_table <- gt(blk_summary) %>% 
  tab_header(title = "Relationship Between Share of Blacks in County vs. Black Students' Test Scores",
             subtitle = "Test score data from 8th graders in 2010") %>% 
  
  # shortening the decimal places so the numbers are more digestible
  
  fmt_number(columns = vars("estimate", "std.error", "statistic", "p.value", "conf.low", "conf.high"), decimals = 3) %>%
  
  # making the columns look nicer by relabelling them
  cols_label("estimate" = "Coefficient", "std.error" = "Standard Error", "statistic" = "Statistic", "p.value" = "P-value", "conf.low" = "5th Percentile",
             "conf.high" = "95th Percentile") %>%  
  
  # adding source info
  
  tab_source_note(html("Data from Opportunity Insights and Stanford Education Data Archive."))

# here, I am doing yet another linear regression model for Hispanics

hsp_model <- lm(data = temp, mn_hsp ~ share_hisp2010)
hsp_summary <- tidy(hsp_model, conf.int=TRUE, conf.level = .95)
hsp_table <- gt(hsp_summary) %>% 
  tab_header(title = "Share of Hispanics in County vs. Hispanic Students' Test Scores",
             subtitle = "Data from 8th graders in 2010") %>% 
  
  # shortening the decimal places so the numbers are more digestible
  
  fmt_number(columns = vars("estimate", "std.error", "statistic", "p.value", "conf.low", "conf.high"), decimals = 3) %>%
  
  # making the columns look nicer by relabelling them
  cols_label("estimate" = "Coefficient", "std.error" = "Standard Error", "statistic" = "Statistic", "p.value" = "P-value", "conf.low" = "5th Percentile",
             "conf.high" = "95th Percentile") %>%  
  
  # adding source info
  
  tab_source_note(html("Data from Opportunity Insights and Stanford Education Data Archive."))

# this model is just analyzing the non-white share and its correlation with all students' test scores

minority_model <- lm(data = temp, mn_all ~ nonwhite_share2010 + poor_share2010)
minority_summary <- tidy(minority_model, conf.int=TRUE, conf.level = .95)
minority_table <- gt(minority_summary) %>% 
  tab_header(title = "Share of Non-White Minorities and Poor Residents in County vs. All Students' Test Scores",
             subtitle = "Test Score Data from 8th graders in 2010") %>% 
  
  # shortening the decimal places so the numbers are more digestible
  
  fmt_number(columns = vars("estimate", "std.error", "statistic", "p.value", "conf.low", "conf.high"), decimals = 3) %>%
  
  # making the columns look nicer by relabelling them
  cols_label("estimate" = "Coefficient", "std.error" = "Standard Error", "statistic" = "Statistic", "p.value" = "P-value", "conf.low" = "5th Percentile",
             "conf.high" = "95th Percentile") %>%  
  
  # adding source info
  
  tab_source_note(html("Data from Opportunity Insights and Stanford Education Data Archive."))

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
                          
                          selectizeInput("selectCounty",
                                      
                                      # this is what would show up in the app
                                      
                                      "Select County",
                                      
                                      # this gives a list of all the choices that users can choose from in the drop-down menu
                                      
                                      choices = countystateVector,
                                      
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
                                               
                                               plotOutput("plot1")
                                      ),
                                      tabPanel("Explanation",
                                               p("This plot shows the distribution of mean scores
                                                 over time for 3rd-8th graders across the U.S. by county.
                                                 Select two or more counties to see how the trends compare to
                                                 each other. For an explanation of the dataset, please see the
                                                 About tab."))
                          )
                        )
                      )
             ),
             
             # repeat the same tabPanel code for other tabs, changing the titles
             
             tabPanel("Gender Differences in Scores",
                      titlePanel("Exploring Gender Differences in Academic Achievement"),
                      
                      # Show a plot of the generated distribution
                      
                      tabsetPanel(id = "tabsMain",
                                  tabPanel("States with Best Test Scores by Gender",
                                           
                                           # it is easy to add text using p
                                           
                                           p("This section investigates the differences in scores between females and males. Which states have
                                             particularly good outcomes for females or males? In which counties is the gender gap the largest?"),
                                           
                                           # to put a different object right underneath, simply do a comma
                                           
                                           plotOutput("plot2"),
                                           p("This is a plot of the top 10 states with the highest mean female scores across the country."),
                                           
                                           #this provides spacing to make the text blocks look nice
                                           
                                           br(),
                                           plotOutput("plot3"),
                                           p("This is a plot of the top 10 states with the highest mean male scores across the country."),
                                           br(),
                                           p("Comparing both of these plots, there are 8 states that have high scores for both females and males. Some differences 
                                             between the two plots are that Maryland and Pennsylvania appear in the top 10 states for females, and Kansas and Indiana
                                             appear in the top 10 states for males.")
                                           
                                           ),
                                  tabPanel("Counties with Largest Gender Gaps",
                                           
                                           plotOutput("plot4"),
                                           p("There is a measure of the estimated mean female-male gap in the Stanford Education dataset. Grouping
                                             by county, this plot shows the counties with the largest female-male gaps across the country."),
                                           br(),
                                           p("All of these plots show interesting variations in academic performance by gender that should be further investigated
                                             in future research. What are the root causes of these gender differences? Could it be due to teacher bias, curriculum,
                                             culture, etc.? These would be very interesting questions to explore via a more in-depth study.")
                                           
                                  ))
                      
                      ),
             
             tabPanel("Model of Scores",
                      titlePanel("Models of Test Scores with Socioeconomic Covariates"),
                      
                      # Show a plot of the generated distribution
                      
                      tabsetPanel(id = "tabsMain",
                                  tabPanel("Household Income and Single-Parent Households",
                                           
                                           # here, instead of calling plotlyOutput, we want to call tableOutput since the model
                                           # results are displayed in a summary statistics table
                                           
                                           gt_output("model"),
                                           
                                           # I made sure that each line of code did not have too many characters
                                           # even though the text block was very long
                                           
                                           p("This is the summary statistics table for a linear regression of mean test scores based 
                                             on the mean household income in the county (hhinc_mean2000) and the proportion of single-parent households
                                             (singleparent_share2000) in the county. Since household income is in terms
                                             of dollars, each additional dollar has a very small effect on mean scores, especially when
                                             the scores are standardized the way they are. Thus, I made the units for 
                                             household income in terms of 10,000 dollars instead of one dollar by mutating a new column (hhinc_k) that
                                             describes household income in terms of 10,000 dollars. There is a coefficient of 0.06 for this variable, meaning that
                                             on average, for each 10,000 dollar increase in household income, there is a .06 increase in scores. For the share of single-
                                             parent households, there is a coefficient for -1.88, meaning that for every .1 increase in
                                             the proportion of single-parent households, there is a -.188 decrease in the mean of mean
                                             scores in the county on average. Both variables have a very small standard error as well as a high absolute T-statistic value, which says
                                             that these variables model the scores very well. The p-values are also smaller than .05,
                                             which means that at the 95% significance level, the effects of both variables on mean scores
                                             are statistically significant and not due to random chance. Furthermore, 0 is not in any of the confidence intervals,
                                             so this means that the estimates of the correlation coefficients are statistically significant.")
                                           
                                           ),
                                  tabPanel("Non-White Minorities and Poor Populations",
                                           
                                           # here, instead of calling plotlyOutput, we want to call gt_output since the model
                                           # results are displayed in a summary statistics table using the gt package
                                           
                                           gt_output("model5"),
                                           p("This is the summary statistics table for a linear regression of mean test scores for all 3rd-8th graders (mn_all)
                                              using the share of non-white residents, or minority residents, and share of poor residents as independent variables 
                                              (nonwhite_share2010 and poor_share2010, respectively). The coefficient 
                                             for nonwhite_share2010 says that on average, for every .1 increase in the share of non-white residents in a county,
                                             there is a .0431 decrease in the mean test scores for students in the county. I decided to interpret this at .1 increments because 
                                             the nonwhite_share2010 variable is a percentage share of the total population, not a binary variable. The p-value is 0, which means
                                             that at the 95% significance level, it is statistically significant. The coefficient on poor_share 2010 says that for every .1 increase
                                              in the share of poor people in the county, there is a decrease of .1715 in test scores on average. Interestingly, the share of poor people has a larger effect on 
                                             test scores than the share of non-white share does, which seems to indicate that the economic status of the neighborhood plays a larger
                                             effect on academic performance than the amount of minorities in the county."),
                                           br(),
                                          
                                           # I wanted to create space between plots and explanations
                                           
                                           plotOutput("plot8"),
                                           p("Here is a graph of the share of non-white residents in a county versus all students' test scores. I decided to specifically show data
                                             for one test grade (8th grade) and year (2010) because the demographic data about the county is from 2010, and it would inaccurate to 
                                             extrapolate demographic data from one year to correlate with test score data from all years. Also, selecting for one grade level makes it
                                             easier to see the linear trends with a fewer number of points plotted.")
                                           
                                           ),
                                  tabPanel("Asians",
                                           
                                           # here, instead of calling plotlyOutput, we want to call tableOutput since the model
                                           # results are displayed in a summary statistics table
                                           
                                           gt_output("model2"),
                                           p("This is the summary statistics table for a linear regression of mean test scores for Asian Americans (mn_asn)
                                              using the share of Asian residents as a independent variable (share_asian2010). The coefficient 
                                             for share_asian2010 says that on average, for every .1 increase in the share of Asian residents in a county,
                                             there is a .1021 increase in the mean test scores for Asian students in the county. I decided to interpret this at .1 increments because 
                                             the share_asian2010 variable is a percentage share of the total population, not a binary variable. The p-value is very close to 0, which means
                                             that at the 95% significance level, it is statistically significant. There seems to be a large effect of the share of Asians in the community on
                                              Asian students' achievement, so I wonder if there is a cultural, community-based component to students' academic performance. I decided to investigate this to answer the question
                                             of whether the share of a certain racial or ethnic group in the surrounding environment affects that group of students' performance.
                                             Perhaps there could be cultural or social effect on students' achievement if there is a large community of people from the same ethnic or racial group."),
                                           br(),
                                           plotOutput("plot5"),
                                           p("Here is a graph of the share of non-white residents in a county versus all students' test scores. I decided to specifically show data
                                             for one test grade (8th grade) and year (2010) because the demographic data about the county is from 2010, and it would inaccurate to 
                                             extrapolate demographic data from one year to correlate with test score data from all years. Also, selecting for one grade level makes it
                                             easier to see the linear trends with a fewer number of points plotted. I scaled this graph's x-axis by log10 because the share of Asian Americans
                                             in a county is usually small, so a lot of the data points were clustered toward 0 and this gives better visibility of the trend.")
                                           
                                  ),
                                  tabPanel("Blacks",
                                           
                                           # here, instead of calling plotlyOutput, we want to call tableOutput since the model
                                           # results are displayed in a summary statistics table
                                           
                                           gt_output("model3"),
                                           p("This is the summary statistics table for a linear regression of mean test scores for Black Americans (mn_blk)
                                              using the share of black residents as a independent variable (share_black2010). The coefficient 
                                             for share_black2010 says that on average, for every .1 increase in the share of Black residents in a county,
                                             there is a .05 decrease in the mean test scores for Black students in the county, meaning that a greater proportion of Black residents
                                              negatively affects Black students. While the root cause of this is uncertain, there have been studies done in which there are negative effects of 'apartheid'
                                              schools with predominantly Black or Hispanic kids on students' performance. I decided to interpret this at .1 increments because 
                                             the share_black2010 variable is a percentage share of the total population, not a binary variable. The p-value is very close to 0, which means
                                             that at the 95% significance level, it is statistically significant. I decided to investigate this to answer the question
                                             of whether the share of a certain racial or ethnic group in the surrounding environment affects that group of students' performance.
                                             Perhaps there could be cultural or social effect on students' achievement if there is a large community of people from the same ethnic or racial group.
                                             "),
                                           br(),
                                           plotOutput("plot6"),
                                           p("Here is a graph of the share of Black residents in a county versus Black students' test scores. I decided to specifically show data
                                             for one test grade (8th grade) and year (2010) because the demographic data about the county is from 2010, and it would inaccurate to 
                                             extrapolate demographic data from one year to correlate with test score data from all years. Also, selecting for one grade level makes it
                                             easier to see the linear trends with a fewer number of points plotted.")
                                           
                                  ),
                                  tabPanel("Hispanics",
                                           
                                           # here, instead of calling plotlyOutput, we want to call tableOutput since the model
                                           # results are displayed in a summary statistics table
                                           
                                           gt_output("model4"),
                                           p("This is the summary statistics table for a linear regression of mean test scores for Hispanic Americans (mn_hsp)
                                              using the share of Hispanic residents as a independent variable (share_hisp2010). The coefficient 
                                             for share_hisp2010 says that on average, for every .1 increase in the share of Hispanic residents in a county,
                                             there is a .015 decrease in the mean test scores for Hispanic students in the county, meaning that a greater proportion of Hispanic residents
                                             negatively affects Hispanic students, albeit to a small degree. While the root cause of this is uncertain, there have been studies done in which there are negative effects of 'apartheid'
                                             schools with predominantly Black or Hispanic kids on students' performance. I decided to interpret this at .1 increments because 
                                             the share_black2010 variable is a percentage share of the total population, not a binary variable. The p-value is very close to 0, which means
                                             that at the 95% significance level, it is statistically significant. I decided to investigate this to answer the question
                                             of whether the share of a certain racial or ethnic group in the surrounding environment affects that group of students' performance.
                                             Perhaps there could be cultural or social effect on students' achievement if there is a large community of people from the same ethnic or racial group.
                                             "),
                                           br(),
                                           plotOutput("plot7"),
                                           p("Here is a graph of the share of Hispanic residents in a county versus Hispanic students' test scores. I decided to specifically show data
                                             for one test grade (8th grade) and year (2010) because the demographic data about the county is from 2010, and it would inaccurate to 
                                             extrapolate demographic data from one year to correlate with test score data from all years. Also, selecting for one grade level makes it
                                             easier to see the linear trends with a fewer number of points plotted.")
                                           
                                  )
                                  )
                                  ),
             
             # remember the levels of tab panels. While there can be several subsets of panels, you want
             # to make sure that it is the right level by constantly re-running the app
             
             tabPanel("About",
                      titlePanel("About This Project"),
                      p("My name is Amy Tan. I am currently a senior at Harvard studying sociology and economics, and I am particularly interested in issues of educational inequality.
                        If you have any questions or comments, please email me at atan@college.harvard.edu."),
                      br(),
                      
                      # the Youtube tutorial on uploading a video was very helpful. Don't forget the '' to wrap around the embedding link
                      
                      HTML('<iframe width="560" height="315" src="https://www.youtube.com/embed/azJcJoRbhSA" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>'),
                      p("In this project, I used two datasets to figure out if there are any interesting correlations between economic variables and educational variables at the county level nationwide. 
                        How do students' broader social environments impact their academic achievement? I attempt to answer this question by getting the county-level covariates dataset from the Opportunity
                        Insights website (https://opportunityinsights.org/data/) and the dataset from the Stanford Education Data Archive (https://exhibits.stanford.edu/data/catalog/db586ns4974).
                        The covariates dataset describes economic and social factors of each county, such as rates of intergenerational mobility and umemployment rates. The stanforddata dataset describes 
                        various academic variables for 3rd-8th graders across the nation by tract as well. It contains variables for academic achievement and student demographics. The mn_all test score variable
                        is a standardized test score variable that standardizes scores across different states' tests. More information can be found on the technical codebook on the website. I merged these two datasets
                        together by their FIPS code. I also descriptively analyzed each dataset to find trends within the datasets themselves, such as the correlation between household income, share of single-parent households,
                        demographic composition, and test scores."),
                      br(),
                      p("For more information about how the mean test scores were standardized across the country,
                        please refer to the Stanford Education Data Archive's codebook below:"),
                      
                      # this allows you specify which words can correspond to the URL embedded, making it look neater
                      
                      p(a(href="https://cepa.stanford.edu/sites/default/files/SEDA%20Technical%20Documentation%20Version1_1.pdf", "SEDA Codebook")),
                      p("You can find more of my work on Github:"),
                      
                      # the only flaw is that it appears on a separate line
                      
                      p(a(href="https://github.com/amytan9", "Github"))
                      )
             
             )
  
                                  )


# Define server logic required to draw a histogram

server <- function(input, output) {
  
  # use plotly to allow hover text to appear on plots, giving more info about the point
  
  output$plot1 <- renderPlot({
    
     clean_data %>%
               
               # first we want the ggplot to display data by county based on which
               # county(s) the user picks
               
               filter(county_state %in% input$selectCounty) %>% 
               
               # then, we group by countyname and year to set each data point as results
               # for each county by year
               
               group_by(county_state, year) %>% 
               
               # since there is variation within each county for scores, I just wanted 
               # to take the mean of each score within the county to set that as the
               # county score
               
               summarize(mean_score = mean(mn_all)) %>% 
               
               # i ungrouped to make sure that the data is back in its original form
               
               ungroup() %>% 
               
               # remember that group allows you to create graphs for each county separately
               
               # the color can also be varied to show each county's line on the same graph
               
               ggplot(aes(x = year, y = mean_score, group = county_state, color = county_state)) +
               geom_point() +
               
               # this connects all the points
               
               geom_line() +
               labs(title = "Mean Scores over Time",
                    y = "Mean Score",
                    x = "Year",
                    color = "County Name",
                    subtitle = "Data from 3rd-8th graders in U.S. counties"
               )
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
  
  output$plot4 <- renderPlot({
   clean_data %>%
      
      # I am grouping by each distinct county
      
      group_by(county_state) %>%
      
      # don't want to include any NAs that might skew the analysis
      
      filter(!is.na(mn_mfg)) %>%
      
      # took me forever to realize that if I didn't summarize the mean value,
      # it would take several values for the same county, which is not the analysis I want
      # I want to aggregate by county, so it is necessary to summarize
      
      summarize(mean_mn_mfg = mean(mn_mfg)) %>% 
      
      # arranging this here still requires you to reorder the x-axis later because this is just
      # to select the top 10
      
      arrange(desc(mean_mn_mfg)) %>% 
      head(10) %>%
      
      # checking to make sure that counties are not counted twice
      
      select(county_state, mean_mn_mfg) %>%
      
      # putting a - in front of the variable would put it in descending order within reorder()
      
      ggplot(aes(x=reorder(county_state, -mean_mn_mfg), y=mean_mn_mfg)) +
      
      # you can change bar colors within geom_col
      
      geom_col(fill= "purple") +
      
      # here, just the standard labelling of plots
      
      labs(title = "Counties with Highest Female-Male Gap",
           y = "Mean Estimated Score Gap",
           x = "Counties",
           
           # don't forget the source!
           
           caption = "Data from the Stanford Education Data Archive, 
           https://edopportunity.org/get-the-data/seda-archive-downloads/",
           subtitle = "Data from 3rd-8th graders in U.S. counties"
      ) + 
      
      # the county names were really long, so they overlapped. I turned them so they're readable
      
      theme(axis.text.x = element_text(angle=45, hjust=1))
    
  })
  
  
  output$plot5 <- renderPlot({
    temp %>% 
      ggplot(aes(x = share_asian2010, y = mn_asn)) +
      geom_point() +
      geom_smooth(method ="lm") + 
      
      # since the share of Asians is usually very small, I wanted to make the datapoints
      # more visible by scaling the x-axis by log10
      
      scale_x_log10() +
      labs(title = "Share of Asians vs. Asian Students' Test Scores",
           subtitle = "Test score data from 8th graders in 2010",
           x = "Share of Asians",
           y = "Mean Test Scores",
           caption = "Data from the Stanford Education Data Archive, 
           https://edopportunity.org/get-the-data/seda-archive-downloads/")
    
  })
  
  output$plot6 <- renderPlot({
    temp %>% 
      
      # same code but for Blacks
      
      ggplot(aes(x = share_black2010, y = mn_blk)) +
      geom_point() +
      geom_smooth(method ="lm") + 
      labs(title = "Share of Blacks vs. Black Students' Test Scores",
           subtitle = "Test score data from 8th graders in 2010",
           x = "Share of Blacks",
           y = "Mean Test Scores",
           caption = "Data from the Stanford Education Data Archive, 
           https://edopportunity.org/get-the-data/seda-archive-downloads/")
    
  })
  
  output$plot7 <- renderPlot({
    temp %>% 
      
      # same code but for Hispanics
      
      ggplot(aes(x = share_hisp2010, y = mn_hsp)) +
      geom_point() +
      geom_smooth(method ="lm") + 
      labs(title = "Share of Hispanics vs. Hispanic Students' Test Scores",
           subtitle = "Test score data from 8th graders in 2010",
           x = "Share of Hispanics",
           y = "Mean Test Scores",
           caption = "Data from the Stanford Education Data Archive, 
           https://edopportunity.org/get-the-data/seda-archive-downloads/")
    
  })
  
  output$plot8 <- renderPlot({
    temp %>% 
      
      # same code but for non_white people, and also looking at all students' performance
      # instead of one racial group because it is unclear which group this would affect the most
      
      ggplot(aes(x = nonwhite_share2010, y = mn_all)) +
      geom_point() +
      geom_smooth(method ="lm") + 
      labs(title = "Share of Non-White People vs. Students' Test Scores",
           subtitle = "Test score data from 8th graders in 2010",
           x = "Share of Non-White People",
           y = "Mean Test Scores",
           caption = "Data from the Stanford Education Data Archive, 
           https://edopportunity.org/get-the-data/seda-archive-downloads/")
    
  })
  # for my model, I want to create a regression table that shows the results of the model
  
  # the way to render a gt table as opposed to a normal regression table is to do render_gt
  
  output$model <- render_gt({
    
    # this is modeling mean scores using household income and proportion 
    # of single parent households via a linear regression
    
    hhinc_table
    
  })
  
  # for my model, I want to create a regression table that shows the results of the model
  
  output$model2 <- render_gt({
    
    # I didn't realize how easy it was to put a table that's already made in the server
    # and it will show up once called in the UI
    
    asn_table
    
  })
  
  output$model3 <- render_gt({

    blk_table
    
  })
  
  output$model4 <- render_gt({
    
    hsp_table
    
  })
  output$model5 <- render_gt({
    
    minority_table
    
  })
}

# Run the application 

shinyApp(ui = ui, server = server)

