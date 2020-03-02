Discarded code

```{r}
# don't need this chunk now that I have discovered sub-groups in the data
# and facet wrap takes care of sub-categories

# plots of school segregation on test scores by racial group (use seda_oi)

# Variables:
# rswhtblk
# rswhthsp
# rsecdnec
# 
# mn_wht
# mn_asn
# mn_blk
# mn_hsp
# mn_ecd
# mn_nec

rswhtblk_blk <- seda_scores_cov %>% 
  ggplot(aes(x=mn_blk, y= rswhtblk)) +
  geom_point(alpha=.5) + 
  geom_smooth(method=lm, se= FALSE) +
  labs(title = "Black-White Segregation and Black Students' Scores",
       subtitle = "National Data on 3rd-8th graders, 2008-2016",
       caption = "Data from Opportunity Insights and Stanford Education Data Archive.",
       x = "Mean Standardized Score",
       y = "Relative Diversity Index") +
  theme_bw()  

rswhtblk_wht <- seda_scores_cov %>% 
  ggplot(aes(x=mn_wht, y= rswhtblk)) +
  geom_point(alpha=.5) + 
  geom_smooth(method=lm, se= FALSE) +
  labs(title = "Black-White Segregation and White Students' Scores",
       subtitle = "National Data on 3rd-8th graders, 2008-2016",
       caption = "Data from Opportunity Insights and Stanford Education Data Archive.",
       x = "Mean Standardized Score",
       y = "Relative Diversity Index") +
  theme_bw() 

rswhtblk_comb_plot <- plot_grid(rswhtblk_blk, rswhtblk_wht)

# rswhtblk_blk_1 <- seda_scores_cov %>%
#   ggplot(aes(x=mn_blk, y= rswhtblk)) +
#   geom_point(alpha=.5) +
#   geom_smooth(method=lm, se= FALSE)
#
# rswhtblk_wht_1 <- seda_scores_cov %>%
#   ggplot(aes(x=mn_wht, y= rswhtblk)) +
#   geom_point(alpha=.5) +
#   geom_smooth(method=lm, se= FALSE)

# # --- test---
# plot_row <- plot_grid(rswhtblk_blk_1, rswhtblk_wht_1)
# 
# # now add the title
# title <- ggdraw() + 
#   draw_label(
#     "Black-White Segregation and Test Scores by Race",
#     fontface = 'bold',
#     x = 0,
#     hjust = 0
#   ) +
#   theme(
#     # add margin on the left of the drawing canvas,
#     # so title is aligned with left edge of first plot
#     plot.margin = margin(0, 0, 0, 7)
#   )
# x <-plot_grid(
#   title, plot_row,
#   ncol = 1,
#   # rel_heights values control vertical title margins
#   rel_heights = c(0.1, 1),
#   labels = c("Black Students", "White Students")
# )
# 
# 
# 
# title_theme <- ggdraw() +
#   draw_label("Socio-economic measures", 
#              fontfamily = theme_georgia()$text$family, 
#              fontface = theme_georgia()$plot.title$face, x = 0.05, hjust = 0)
# plot_grid(title_theme, gridded, ncol = 1, rel_heights = c(0.2, 1))


rswhthsp_hsp <- seda_scores_cov %>% 
  ggplot(aes(x=mn_hsp, y= rswhthsp)) +
  geom_point(alpha=.5) + 
  geom_smooth(method=lm, se= FALSE) +
  labs(title = "Hispanic-White Segregation and Hispanic Students' Scores",
       subtitle = "National Data on 3rd-8th graders, 2008-2016",
       caption = "Data from Opportunity Insights and Stanford Education Data Archive.",
       x = "Mean Standardized Score",
       y = "Relative Diversity Index") + 
  theme_bw() 


rswhthsp_wht <- seda_scores_cov %>% 
  ggplot(aes(x=mn_wht, y= rswhthsp)) +
  geom_point(alpha=.5) + 
  geom_smooth(method=lm, se= FALSE) +
  labs(title = "Hispanic-White Segregation and White Students' Scores",
       subtitle = "National Data on 3rd-8th graders, 2008-2016",
       caption = "Data from Opportunity Insights and Stanford Education Data Archive.",
       x = "Mean Standardized Score",
       y = "Relative Diversity Index") +
  theme_bw()  

rswhthsp_wht <- seda_scores_cov %>% 
  ggplot(aes(x=mn_wht, y= rswhthsp)) +
  geom_point(alpha=.5) + 
  geom_smooth(method=lm, se= FALSE) +
  labs(title = "Hispanic-White Segregation and White Students' Scores",
       subtitle = "National Data on 3rd-8th graders, 2008-2016",
       caption = "Data from Opportunity Insights and Stanford Education Data Archive.",
       x = "Mean Standardized Score",
       y = "Relative Diversity Index") +
  theme_bw()  

rsecdnec_ecd <- seda_scores_cov %>% 
  ggplot(aes(x=mn_ecd, y= rsecdnec)) +
  geom_point(alpha=.5) + 
  geom_smooth(method=lm, se= FALSE) +
  labs(title = "ECD/Non-ECD Segregation and ECD Students' Scores",
       subtitle = "National Data on 3rd-8th graders, 2008-2016",
       caption = "Data from Opportunity Insights and Stanford Education Data Archive.",
       x = "Mean Standardized Score",
       y = "Relative Diversity Index") +
  theme_bw()  

rsecdnec_nec <- seda_scores_cov %>% 
  ggplot(aes(x=mn_nec, y= rsecdnec)) +
  geom_point(alpha=.5) + 
  geom_smooth(method=lm, se= FALSE) +
  labs(title = "ECD/Non-ECD Segregation and Non-ECD Students' Scores",
       subtitle = "National Data on 3rd-8th graders, 2008-2016",
       caption = "Data from Opportunity Insights and Stanford Education Data Archive.",
       x = "Mean Standardized Score",
       y = "Relative Diversity Index") +
  theme_bw()  

rsecdnec_comb_plot <- plot_grid(rsecdnec_ecd, rsecdnec_nec)

# maybe add in subsetting by income later?

```