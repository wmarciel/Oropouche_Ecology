#Extended Data Figure 3

library(pacman)
pacman::p_load(sf, raster, tmap, terra, #handling spatial data
               dplyr,  tidyverse, readxl, openxlsx, ggplot2, ggpubr, hrbrthemes, 
               networkD3, ggbreak, ggridges, ggcorrplot, ggpattern, tidyr, lubridate,
               scales, biscale, corrplot, energy, magick, pdftools,
               RColorBrewer, EpiEstim, logistf, glmnet, plotly, cowplot, patchwork)

##### Extended Data Figure 3 ##### 

df_S3 <- read.csv("Figure S3.csv")

P_S3 <- ggplot(data = df_S3, aes(x = Epi_week, y = OROV_cases, fill = Category)) +
        geom_col(position = "identity", alpha = 0.6) + 
        labs(x = "Year (by epidemiological week)", 
             y = "Number of Oropouche fever cases", fill = NULL) +
        scale_x_discrete(breaks = breaks_year_start,
                         labels = sub("_01$", "", breaks_year_start)) +
        scale_fill_manual(values = c("Rural" = "#74a36f", "Urban" = "#4d78a2")) +
        theme_classic() +
        theme(legend.position = c(0.3, 0.98),   
        legend.direction = "horizontal") 

P_S3