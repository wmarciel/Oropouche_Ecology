#Figure 1

library(pacman)
pacman::p_load(sf, raster, tmap, terra, #handling spatial data
               dplyr,  tidyverse, readxl, openxlsx, ggplot2, ggpubr, hrbrthemes, 
               networkD3, ggbreak, ggridges, ggcorrplot, ggpattern, tidyr, lubridate,
               scales, biscale, corrplot, energy, magick, pdftools,
               RColorBrewer, EpiEstim, logistf, glmnet, plotly, cowplot, patchwork)

##### Figure 1. panel A - Number of lab-confirmed Oropouche fever cases (left) ######

df_1A <- read.csv("Figure 1A.csv")

#setting yearly break
breaks_year_start <- unique(df_1A$Epi_week[grep("_01$", df_1A$Epi_week)])

P_1A_case <- ggplot(data = df_1A, aes(x = Epi_week, y = OROV_cases, group = State, color = State)) +
             geom_line(size = 0.7) +
             labs(x = "Year", y = "Number of Oropouche fever cases", color = "State") + 
             scale_x_discrete(breaks = breaks_year_start,
                              labels = sub("_01$", "", breaks_year_start)) +
             scale_y_continuous(limits = c(0, 1000), breaks = seq(0, 1000, by = 200)) +
             scale_color_manual(values = c("AC" = "#104E8B", "AP" = "#16548C", "AM" = "#1C5A8E", "PA" = "#22608F", "RO" = "#296691",
                                           "RR" = "#2F6C92", "TO" = "#357294", #North Region
                                           "AL" = "#3C7996", "BA" = "#427F97", "CE" = "#488599", "MA" = "#4F8B9A", "PB" = "#55919C", 
                                           "PE" = "#5B979D", "PI" = "#629E9F", "RN" = "#68A4A1", "SE" = "#6EAAA2", #Northeast Region
                                           "DF" = "#74B0A4", "GO" = "#7BB6A5", "MT" = "#81BCA7", "MS" = "#87C2A8", #Central-West Region
                                           "ES" = "#8EC9AA", "MG" = "#94CFAC", "RJ" = "#9AD5AD", "SP" = "#A1DBAF", #Southeast Region
                                           "PR" = "#A7E1B0", "RS" = "#ADE7B2", "SC" = "#B4EEB4")) + #South Region
  
             theme_classic() +
             theme(legend.position = c(0.3, 0.98),   
                   legend.direction = "horizontal",  
                   legend.justification = c("center", "top"),  
                   legend.title = element_blank()) +   
             guides(color = guide_legend(nrow = 4, byrow = TRUE)) 
P_1A_case


##### Figure 1. panel A - Incidence of Oropouche fever cases (right) ######

#setting yearly break
breaks_year_start <- unique(df_1A$Epi_week[grep("_01$", df_1A$Epi_week)])

P_1A_incidence <- ggplot(data = df_1A, aes(x = Epi_week, y = INC, group = State, color = State)) +
                  geom_line(size = 0.7) +
                  labs(x = "Year", y = "Incidence of Oropouche fever cases", color = "State") + 
                  scale_x_discrete(breaks = breaks_year_start,
                                   labels = sub("_01$", "", breaks_year_start)) +
                  scale_y_continuous(limits = c(0, 25), breaks = seq(0, 25, by = 5)) +
                  scale_color_manual(values = c("AC" = "#104E8B", "AP" = "#16548C", "AM" = "#1C5A8E", "PA" = "#22608F", "RO" = "#296691",
                                                "RR" = "#2F6C92", "TO" = "#357294", #North Region
                                                "AL" = "#3C7996", "BA" = "#427F97", "CE" = "#488599", "MA" = "#4F8B9A", "PB" = "#55919C", 
                                                "PE" = "#5B979D", "PI" = "#629E9F", "RN" = "#68A4A1", "SE" = "#6EAAA2", #Northeast Region
                                                "DF" = "#74B0A4", "GO" = "#7BB6A5", "MT" = "#81BCA7", "MS" = "#87C2A8", #Central-West Region
                                                "ES" = "#8EC9AA", "MG" = "#94CFAC", "RJ" = "#9AD5AD", "SP" = "#A1DBAF", #Southeast Region
                                                "PR" = "#A7E1B0", "RS" = "#ADE7B2", "SC" = "#B4EEB4")) + #South Region
  
                  theme_classic() +
                  theme(legend.position = c(0.3, 0.98),   
                        legend.direction = "horizontal",  
                        legend.justification = c("center", "top"),  
                        legend.title = element_blank()) +   
                  guides(color = guide_legend(nrow = 4, byrow = TRUE)) 
P_1A_incidence


##### Figure 1. panel B  ######

load("Figure 1B.RData")

#Create a 2 by 2 legend 
bivariate_palette1 <- matrix(c("#e1f0eb", "#8dbbce", 
                               "#87c7a5", "#339388"), nrow = 2, ncol = 2, byrow = TRUE)

rownames(bivariate_palette1) <- c("0", ">0 - 50")
colnames(bivariate_palette1) <- c(">0 - 50", ">50")

legend_data1 <- expand.grid(OROV_INC_category = rownames(bivariate_palette1),
                            Class = colnames(bivariate_palette1))

legend_data1$color <- as.vector(bivariate_palette1)

legend <- ggplot(legend_data1, aes(x = Class, y = OROV_INC_category, fill = color)) +
          geom_tile() +
                scale_fill_identity() +
                labs(x = "OROV INC 2023-25", y = "OROV INC 2014-22", title = "Bivariate Legend") +
                theme_minimal() +
                theme(axis.text.x = element_text(angle = 45, hjust = 1))  
legend

#Create the bivariate map <- this step will take a few munites.
P_1B <- tm_shape(df_1B) +
        tm_polygons(col = "bivariate_color",  
                    title = "Combined Class & OROV INC") +
        tm_borders(lwd = 1) 
P_1B 


##### Figure 1. panel C  ######

load("Figure 1C.RData")

# Define the scaling factor
# 15000 (max OROV cases) / 70 (max positivity rate)
scale_factor <- (15000/70)  

P_1C <- ggplot() +
        geom_bar(data = df_1C_a, aes(x = Year, y = OROV_cases, fill = Region), 
                 stat = "identity") +
        scale_fill_manual(values = c("South" = "#485f30", 
                                   "Southeast" = "#74A36F", 
                                   "Central-West" = "#98be9b", 
                                   "Northeast" = "#849EB9", 
                                   "North" = "#46577B")) + 
        #Left Y-axis: Cases (0 to 15000)
        scale_y_continuous(name = "Number of Oropouche fever cases", 
                           limits = c(0, 15000), breaks = seq(0, 15000, by = 2500),
        #Right Y-axis: Positivity Rate (0 to 70)
                           sec.axis = sec_axis(~ . / scale_factor, 
                                            name = "Testing Positivity Rate (%)",
                                            breaks = seq(0, 70, by = 10))) + 
        #Testing positivity rate - line graph
        geom_line(data = df_1C_b, aes(x = Year, y = Positivity_Rate * scale_factor), 
                  color = "#a51e22", size = 1) +
        geom_point(data = df_1C_b, aes(x = Year, y = Positivity_Rate * scale_factor), 
                  color = "#a51e22", size = 2) +
        scale_x_continuous(breaks = seq(2014, 2025, 1)) +  
        theme_classic() +
        theme(legend.position = c(0.4, 0.98),          
              legend.justification = c(0.5, 1),       
              legend.direction = "horizontal",
              legend.title = element_blank(),
              legend.box.margin = margin(-10, 0, 0, 0))
P_1C


