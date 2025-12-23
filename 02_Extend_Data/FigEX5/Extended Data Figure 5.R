#Extended Data Figure 5

library(pacman)
pacman::p_load(sf, raster, tmap, terra, #handling spatial data
               dplyr,  tidyverse, readxl, openxlsx, ggplot2, ggpubr, hrbrthemes, 
               networkD3, ggbreak, ggridges, ggcorrplot, ggpattern, tidyr, lubridate,
               scales, biscale, corrplot, energy, magick, pdftools,
               RColorBrewer, EpiEstim, logistf, glmnet, plotly, cowplot, patchwork)

##### Extended Data Figure 5 ##### 

df_S5 <- read.csv("Figure S5.csv")

custom_order <- rev(c("AC", "AM", "PA", "RR", "RO", "AP", "TO",
                      "PI", "BA", "MA", "PE", "CE", "AL", "SE", "RN",
                      "PB", "MT", "GO", "MS", "DF", "MG", "ES", "RJ", "SP",
                      "PR", "SC", "RS")) #organizing as Figure 3A

df_S5$state_code <- factor(df_S5$state_code, levels = custom_order)

P_S5<- ggplot(df_S5, aes(x = Value, y = state_code, fill = Year_test)) +
       geom_col() +
       facet_wrap(~ Year_test, ncol = 2) +
       labs(title = NULL,
            x = "Oropouche virus testing positivity rate (%)", y = "States") +
       scale_fill_manual(values = c("Test_2024" = "#27408b", "Test_2025" = "#5ba061")) +
       scale_x_continuous(limits = c(0, 30), breaks = seq(0, 30, by = 10)) +
       theme_classic() +
       theme(legend.position = "none")
P_S5