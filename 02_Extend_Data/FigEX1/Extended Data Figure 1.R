#Extended Data Figure 1

library(pacman)
pacman::p_load(sf, raster, tmap, terra, #handling spatial data
               dplyr,  tidyverse, readxl, openxlsx, ggplot2, ggpubr, hrbrthemes, 
               networkD3, ggbreak, ggridges, ggcorrplot, ggpattern, tidyr, lubridate,
               scales, biscale, corrplot, energy, magick, pdftools,
               RColorBrewer, EpiEstim, logistf, glmnet, plotly, cowplot, patchwork)

##### Extended Data Figure 1. panel A ##### 

df_S1A <- read.csv("Figure S1A.csv")
df_S1A$Month <- as.Date(df_S1A$Month, format = "%m/%d/%y")

P_S1A <- ggplot(df_S1A, aes(x = Month, y = `Oropouche_Brazil`)) +
         geom_line(color = "black", size = 0.5) + 
         geom_area(fill = "#8ba6c2", alpha = 0.6) +  
         scale_y_continuous(breaks = seq(0, 100, by = 10), limits = c(0, 100)) +  
         scale_x_date(breaks = "1 year",  
                      labels = scales::date_format("%Y")) +
         labs(x = "Year", y = "Google Trends activity") + 
         geom_vline(xintercept = as.Date("2023-11-01"), color = "black", linetype = "dashed", size = 0.5) +
         theme_classic() 
P_S1A


##### Extended Data Figure 1. panel B ##### 

df_S1B <- read.csv("Figure S1B.csv")

P_S1B <-  ggplot(df_S1B, aes(x = log_OROV_cases, y = GT_value )) +
          geom_point(shape = 21, fill = "#73A36F", color = "black", size = 5, stroke = 0.4) +
          geom_smooth(method = "lm", formula = y ~ x, se = T, color = "black",
                      fill = "lightgray", alpha = 0.3) +  # Add regression line
          scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 10)) +
          labs(x = "log10 Laboratory-confirmed OROV cases (2014-2025)", y = "Google Trend activity", title = NULL) + 
          geom_text(aes(label = df_S1B$State), vjust = -1.5, size = 3) +
          theme_classic()  
P_S1B

# Statistical testing 
cor_test_result <- cor.test(df_S1B$log_OROV_cases, df_S1B$GT_value, method = "spearman")
cor_test_result 
# Spearman's rank correlation rho
# 
# data:  df_S1B$log_OROV_cases and df_S1B$GT_value
# S = 1157.4, p-value = 0.0002675
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
#       rho 
# 0.6466943 