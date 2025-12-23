#Extended Data Figure 2

library(pacman)
pacman::p_load(sf, raster, tmap, terra, #handling spatial data
               dplyr,  tidyverse, readxl, openxlsx, ggplot2, ggpubr, hrbrthemes, 
               networkD3, ggbreak, ggridges, ggcorrplot, ggpattern, tidyr, lubridate,
               scales, biscale, corrplot, energy, magick, pdftools,
               RColorBrewer, EpiEstim, logistf, glmnet, plotly, cowplot, patchwork)

##### Extended Data Figure 2. panel A ##### 

df_S2A <- read.csv("Figure S2A.csv")

custom_colors <- c("Serology" = "#849EB9", "RT-PCR" = "#98be9b")

df_S2A$Testing_method <- factor(df_S2A$Testing_method, 
                                levels = c("RT-PCR", "Serology")) #re-arrange the order

df_S2A$Years <- as.factor(df_S2A$Years)

P_S2A <- ggplot(df_S2A, aes(fill = Testing_method, y = percentage, x = Years)) +
         geom_bar(position = "stack", stat = "identity") +
         labs (x = "Year",
               y = "Percentage of OROV-positive per diagnosis methods",
               fill = NULL) +
         scale_fill_manual(values = custom_colors) +
         theme_classic() +
         theme(legend.position = c(0.2, 0.99)) +
         guides(fill = guide_legend(nrow = 1))
P_S2A


##### Extended Data Figure 2. panel B ##### 

df_S2B <- read.csv("Figure S2BC.csv")

custom_colors <- c("Serum/blood" = "#3c5d9f", "Others" = "#74a36f")

df_S2B$Biological_material <- factor(df_S2B$Biological_material, 
                                     levels = c("Others", "Serum/blood")) 

df_S2B$Years <- as.factor(df_S2B$Years)

P_S2B <- ggplot(df_S2B, aes(x = factor(Years), y = value, fill = Biological_material)) +
         geom_bar(stat = "identity") +
         scale_fill_manual(values = custom_colors) +
         labs(x = "Year", y = "Number of Oropouche fever cases per sample type", fill = NULL) +
         theme_classic() +
         theme(legend.position = "top") +
         scale_y_continuous(limits = c(0, 15000), 
                            breaks = c(0, 1000, 2000, 3000, 12000, 13000, 14000, 15000)) +
         ggbreak::scale_y_break(c(3000,12000)) +
         guides(fill = guide_legend())
P_S2B


##### Extended Data Figure 2. panel C #####

df_S2C <- read.csv("Figure S2BC.csv")

custom_colors <- c("Serum/blood" = "#849eb9", "Others" = "#98be9b")

df_S2C$Biological_material <- factor(df_S2C$Biological_material, 
                                     levels = c("Others", "Serum/blood")) 

df_S2C$Years <- as.factor(df_S2C$Years)

P_S2C <- ggplot(df_S2C, aes(fill = Biological_material, y = percentage, x = Years)) +
         geom_bar(position = "stack", stat = "identity") +
         labs (x = "Year", y = "Percentage of OROV-positive per sample type", fill = NULL) +
         scale_fill_manual(values = custom_colors) +
         theme_classic() +
         theme(legend.position = c(0.2, 0.99)) +
         guides(fill = guide_legend(nrow = 1))
P_S2C


##### Extended Data Figure 2. panel D #####

df_S2D <- read.csv("Figure S2D.csv")

custom_colors <- c("2025" = "#2a4779", "2024" = "#5b7bab", "2023" = "#A0C3D7", 
                   "2022" = "#74A36F", "2019" = "#417438", "2018" = "#485f30")

df_S2D$Biological_material <- factor(df_S2D$Biological_material, 
                                     levels = c("Urine", "Placenta fragment","Cerebrospinal fluid",
                                                "Other fluid samples", "Organ tissues"))
df_S2D$Years <- as.factor(df_S2D$Years)

P_S2D <- ggplot(df_S2D, aes(x = factor(Biological_material), y = value, fill = Years)) +
         geom_bar(stat = "identity") +
         scale_fill_manual(values = custom_colors) +
         labs(x = "Biological samples", y = "Number of Oropouche fever cases per sample type", fill = NULL) +
         scale_y_continuous(limits = c(0, 720), breaks = seq(0, 700, by = 100)) +
         theme_classic() +
         theme(legend.position = c(0.6, 0.8), 
               legend.direction = "horizontal",
               legend.background = element_rect(fill = "white", color = NA)) +
         theme(axis.text.x = element_text(angle = 15, vjust = 1, hjust = 1)) +
         guides(fill = guide_legend(nrow = 2, byrow = TRUE))
P_S2D
