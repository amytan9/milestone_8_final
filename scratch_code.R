share_black2010, share_hisp2010, share_asian2010, 1-nonwhite_share2010
mn_wag, mn_wbg, mn_whg

clean_data_merged <- clean_data_merged %>% mutate(white_share2010 = (1 - nonwhite_share2010))

lm(data = clean_data_merged, mn_wag ~ share_black2010 + share_hisp2010 +  share_asian2010 + white_share2010)


lm(data = clean_data_merged, mn_wbg ~ share_black2010 + share_hisp2010 +  share_asian2010 + white_share2010)

lm(data = clean_data, mn_wbg ~ share_black2010 + share_hisp2010, share_asian2010, 1-nonwhite_share2010)

lm(data = clean_data, mn_whg ~ share_black2010 + share_hisp2010, share_asian2010, 1-nonwhite_share2010)

# include other variables as controls too? like hhminc2000?


# adding a PDF?

tags$iframe(style="height:600px; width:100%", src="http://localhost/ressources/pdf/R-Intro.pdf"))
#customized to my pdf
tags$iframe(style="height:600px; width:100%", src="milestone_8_app_final_attempt/www/gov_1005_pdf.pdf"))

# adding a video?
#mp4
tags$video(src = "dashboard.mp4", width = "500px", height = "350px", type = "video/mp4", controls = "controls")
#youtube
#use embed link from youtube video
HTML('youtubelink')

## plot code for looking at gender gap by county 

output$plot1 <- renderPlotly({
  
  ggplotly(clean_data_merged %>%
             
             # first we want the ggplot to display data by county based on which
             # county(s) the user picks
             
             filter(countyname %in% input$selectCounty) %>% 
             
             # then, we group by countyname and year to set each data point as results
             # for each county by year
             
             group_by(countyname, year) %>% 
             
             # since there is variation within each county for scores, I just wanted 
             # to take the mean of each score within the county to set that as the
             # county score
             
             summarize(mean_gender_gap = mean(mn_mfg_se)) %>% 
             
             # i ungrouped to make sure that the data is back in its original form
             
             ungroup() %>% 
             
             # remember that group allows you to create graphs for each county separately
             
             # the color can also be varied to show each county's line on the same graph
             
             ggplot(aes(x = year, y = mean_gender_gap, group = countyname, color = countyname)) +
             geom_point() +
             
             # this connects all the points
             
             geom_line() +
             labs(title = "Changes in Gender Gap Over Time",
                  y = "Mean Gender Gap",
                  x = "Year",
                  fill = "County Name",
                  subtitle = "Data from 3rd-8th graders in U.S. counties"
             )) %>% 
    
    
# different plots for scores and diversity
    temp <- clean_data_merged %>% filter(year == 2010, subject == "ela", grade == 8)
    
    temp %>% ggplot(aes(x = poor_share2010, y = mn_all)) + geom_point() + scale_x_log10() + geom_smooth(method ="lm", se = FALSE)
   
    temp %>% ggplot(aes(x = hhinc_mean2000, y = mn_all)) + geom_point() + scale_x_log10() + geom_smooth(method ="lm", se = FALSE)
    
    temp %>% ggplot(aes(x = singleparent_share2010, y = mn_all)) + geom_point() + geom_smooth(method ="lm", se = FALSE)
    
    lm(data = temp, mn_asn ~ share_asian2010)
    temp %>% ggplot(aes(x = share_asian2010, y = mn_asn)) + geom_point() + scale_x_log10() + geom_smooth(method ="lm", se = FALSE)
    
_____
  ggplotly(data %>%
             
             # first we want the ggplot to display data by county based on which
             # county(s) the user picks
             
             filter(countyid ==1001) %>% 
             
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