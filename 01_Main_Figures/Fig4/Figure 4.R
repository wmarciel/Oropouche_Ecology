#Figure 4

library(pacman)
pacman::p_load(sf, raster, tmap, terra, #handling spatial data
               dplyr,  tidyverse, readxl, openxlsx, ggplot2, ggpubr, hrbrthemes, 
               networkD3, ggbreak, ggridges, ggcorrplot, ggpattern, tidyr, lubridate,
               scales, biscale, corrplot, energy, magick, pdftools,
               RColorBrewer, EpiEstim, logistf, glmnet, plotly, cowplot, patchwork)

##### Figure 4. panel A ##### 

df_4A <- read.csv("Figure 4A.csv")

# Ensure the correct order for y-axis
state_order <- rev(c("AC", "AM", "PA", "RR", "RO", "AP", "TO", #North region
                     "PI", "BA", "MA", "PE", "CE", "AL", "SE", "RN", "PB", #Northeast region
                     "MT", "GO", "MS", #Central-West region
                     "MG", "ES", "RJ", "SP", #Southeast region
                     "PR", "SC", "RS")) #South region

df_4A$State <- factor(df_4A$State , levels = state_order)

# Setting yearly break
df_4A$Epi_week <- factor(df_4A$Epi_week)
weeks_to_show <- levels(df_4A$Epi_week)[seq(1, length(levels(df_4A$Epi_week)), by = 4)]

# Ensure the correct order for the legend
df_4A$Percentage <- factor(df_4A$Percentage, levels = c("0", ">0-5", ">5-10", ">10-20", ">20-30", ">30"))

P_4A <- ggplot(df_4A, aes(x = Epi_week, y = State, fill = Percentage)) +
        geom_tile(color = "black") +
        scale_fill_manual(
          values = c("0" = "white",
                     ">0-5" = "#98be9b",
                     ">5-10" = "#5ca061",
                     ">10-20" = "#417438",
                     ">20-30" = "#485f30",
                     ">30" = "black"),
          name = NULL) +
        scale_x_discrete(breaks = weeks_to_show) +
        theme_minimal() +
        labs(x = "Epidemilogical week", y = "State") +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
P_4A


##### Figure 4. panel B ##### 

# Amazonas state
df_4B_Amazonas <- read_excel("Figure 4B.xlsx", sheet = "Amazonas") 
df_4B_Amazonas$dates <- as.Date(df_4B_Amazonas$dates, format = "%Y-%m-%d")

scale_factor1 <- (79/3.663760567) 
max(df_4B_Amazonas$I) #79
max(df_4B_Amazonas$`Quantile.0.975(R)`, na.rm = TRUE) #[1] 3.663760567

P_4B_AM <- ggplot() +
           geom_bar(data = df_4B_Amazonas, aes(x = dates, y = I / scale_factor1), stat = "identity", fill = "#98BE9B", alpha = 0.7) +
           geom_line(data = df_4B_Amazonas, aes(x = dates, y = `Median(R)`), color = "darkblue", size = 1) +
           geom_ribbon(data = df_4B_Amazonas, aes(x = dates, ymin = `Quantile.0.025(R)`, ymax = `Quantile.0.975(R)`), 
                       fill = "lightblue", alpha = 0.3) +
           scale_y_continuous(name = expression(EpiEstim ~ R[t] ~ "estimates"),
                              limits = c(0, 4), breaks = seq(0,4, by = 1),
                              sec.axis = sec_axis(~ . * scale_factor1,
                                  name = "Oropouche fever cases",
                                  breaks = seq (0, 80, by = 20)))+
           scale_x_date(breaks = seq(as.Date("2024-01-01"), as.Date("2025-10-01"), by = "1 months"),
                        labels = date_format("%Y-%b"),
                        limits = c(as.Date("2024-01-01"), as.Date("2025-10-01"))) +
           labs(title = "Amazonas") +
           geom_hline(yintercept = 1, color = "black", linetype = "dashed", size = 0.75) +
           theme_classic() +
           theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
P_4B_AM


# Bahia state
df_4B_Bahia <- read_excel("Figure 4B.xlsx", sheet = "Bahia") 
df_4B_Bahia$dates <- as.Date(df_4B_Bahia$dates, format = "%Y-%m-%d")

scale_factor2 <- (107/5.109745)  
max(df_4B_Bahia$I) #107
max(df_4B_Bahia$`Quantile.0.975(R)`, na.rm = TRUE) #[1] 5.109745

P_4B_BA <- ggplot() +
           geom_bar(data = df_4B_Bahia, aes(x = dates, y = I / scale_factor2), stat = "identity", fill = "#98be9b", alpha = 0.7) +
           geom_line(data = df_4B_Bahia, aes(x = dates, y = `Median(R)`), color = "darkblue", size = 1) +
           geom_ribbon(data = df_4B_Bahia, aes(x = dates, ymin = `Quantile.0.025(R)`, ymax = `Quantile.0.975(R)`), 
                       fill = "lightblue", alpha = 0.3) +
           scale_y_continuous(name = expression(EpiEstim ~ R[t] ~ "estimates"),
                              limits = c(0, 5.2), breaks = seq(0, 5, by = 1),
                              sec.axis = sec_axis(~ . * scale_factor2,
                                  name = "Oropouche fever cases",
                                  breaks = seq (0, 100, by = 20)))+
           scale_x_date(breaks = seq(as.Date("2024-01-01"), as.Date("2025-10-01"), by = "1 months"),
                        labels = date_format("%Y-%b"),
                        limits = c(as.Date("2024-01-01"), as.Date("2025-10-01"))) +
           labs(title = "Bahia") +
           geom_hline(yintercept = 1, color = "black", linetype = "dashed", size = 0.75) +
           theme_classic() +
           theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
P_4B_BA


# Espírito Santo state
df_4B_EspíritoSanto <- read_excel("Figure 4B.xlsx", sheet = "Espírito Santo") 
df_4B_EspíritoSanto$dates <- as.Date(df_4B_EspíritoSanto$dates, format = "%Y-%m-%d")

scale_factor3 <- (219/6.5163) 
max(df_4B_EspíritoSanto$I) #219
max(df_4B_EspíritoSanto$`Quantile.0.975(R)`, na.rm = TRUE) #[1] 6.516256

P_4B_ES <- ggplot() +
           geom_bar(data = df_4B_EspíritoSanto, aes(x = dates, y = I / scale_factor3), stat = "identity", fill = "#98be9b", alpha = 0.7) +
           geom_line(data = df_4B_EspíritoSanto, aes(x = dates, y = `Median(R)`), color = "darkblue", size = 1) +
           geom_ribbon(data = df_4B_EspíritoSanto, aes(x = dates, ymin = `Quantile.0.025(R)`, ymax = `Quantile.0.975(R)`), 
                       fill = "lightblue", alpha = 0.3) +
           scale_y_continuous(name = expression(EpiEstim ~ R[t] ~ "estimates"),
                              limits = c(0, 7), breaks = seq(0, 7, by = 1),
                              sec.axis = sec_axis(~ . * scale_factor3,
                                  name = "Oropouche fever cases",
                                  breaks = seq (0, 200, by = 50)))+
           scale_x_date(breaks = seq(as.Date("2024-01-01"), as.Date("2025-10-01"), by = "1 months"),
                        labels = date_format("%Y-%b"),
                        limits = c(as.Date("2024-01-01"), as.Date("2025-10-01"))) +
           labs(title = "Espírito Santo") +
           geom_hline(yintercept = 1, color = "black", linetype = "dashed", size = 0.75) +
           theme_classic() +
           theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
P_4B_ES

