#fig settings for de Souza paper
library(scico)

 fig_type= ".png"; save.width = 6; save.height =save.width;

close_theme <-  theme(panel.grid.major = element_blank(), plot.margin = margin(t = 2,  # Top margin
                                                                               r = 2,  # Right margin
                                                                               b = 2,  # Bottom margin
                                                                               l = 2),
                      #plot.title = element_text(size=22),
                      legend.title = element_text(size = 7), 
                      legend.text = element_text(size = 7))

n = 7
color_values <- c("white",scico(n*2+1, palette = "cork")[n:1])
# n = 8;color_values <- c(scico(n*2+1, palette = "cork")[n:1])
risk_values <- c("white", scico(11, palette = "vik" )[6:11])

blue_scale <-  binned_scale(aesthetics = "fill",
                            scale_name = "stepsn", 
                            palette = function(x) color_values,
                            breaks = c(0,1,10,20,30,40,50,75),
                            # breaks = c(0,1,10,20,30,40,60,80),
                            # breaks = c(0,1,5,15,25,50,75),
                            labels = ~paste0(., "%"),
                            # breaks = c(0,1,25,50,75),
                            limits = c(0, 100),
                            show.limits = TRUE, 
                            guide = "colorsteps")
blue_scale_binned = blue_scale
risk_values <- c("white", scico(11, palette = "vik" )[6:11])
red_scale <- binned_scale(aesthetics = "fill",
                          scale_name = "stepsn", 
                          palette = function(x) risk_values,
                          breaks = c(0,1,10,20,30,40,50),
                          # breaks = c(0,1,25,50,75),
                          limits = c(0, 60),
                          show.limits = TRUE,
                          guide = "colorsteps")
red_scale_binned = red_scale



shape_states <- read_state(year = 2020,simplified = TRUE)
shape_munis <- read_municipality(year = 2020,simplified = TRUE) 
shape_regions <- read_region(year = 2020,simplified = TRUE)
shape_brazil <- read_country(year=2020,simplified = TRUE)
