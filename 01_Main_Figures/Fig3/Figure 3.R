#Figure 3

library(pacman)
pacman::p_load(sf, raster, tmap, terra, #handling spatial data
               dplyr,  tidyverse, readxl, openxlsx, ggplot2, ggpubr, hrbrthemes, 
               networkD3, ggbreak, ggridges, ggcorrplot, ggpattern, tidyr, lubridate,
               scales, biscale, corrplot, energy, magick, pdftools,
               RColorBrewer, EpiEstim, logistf, glmnet, plotly, cowplot, patchwork)

##### Figure 3. panel A ##### 

df_3A <- read.csv("Figure 3A.csv")

#setting yearly break
breaks_year_start <- unique(df_3A$Epi_week[grep("_01$", df_3A$Epi_week)])

P_3A <- ggplot(data = df_3A, aes(x = Epi_week, y = OROV_INC, group = Category, color = Category)) +
        geom_line(size = 0.7) +
        labs(x = "Year (by epidemiological week)", y = "Incidence of Oropouche fever cases", color = "State") + 
        scale_x_discrete(breaks = breaks_year_start,
                         labels = sub("_01$", "", breaks_year_start)) +
        scale_y_continuous(limits = c(0, 3.5), breaks = seq(0, 3.5, by = 0.5)) +
        scale_color_manual(values = c("Rural" = "#74a36f", "Urban" = "#4d78a2", 
                                      "National" = "#a41e22")) +
        theme_classic() +
        theme(legend.position = c(0.3, 0.98),   
              legend.direction = "horizontal",  
              legend.justification = c("center", "top"),  
              legend.title = element_blank()) +   
        guides(color = guide_legend(nrow = 1)) 
P_3A


##### Figure 3. panel B ##### 

df_3B <- read_excel("Figure 3B.xlsx", sheet = "df_3B")

# Manually creating a boxplot without extreme values
P_3B <- ggplot(df_3B, aes(x = Location, fill = Location)) +
        geom_boxplot(aes(ymin = Lower_whisker,     
                         lower = Q1,     
                         middle = Median, 
                         upper = Q3,     
                         ymax = Upper_whisker),      
                     stat = "identity")+
        scale_fill_manual(values = c("Rural" = "#73A36F", "Urban" = "#4d78a2")) +
        scale_y_continuous(limits = c(0, 311), breaks = seq(0, 300, by = 50)) + 
        labs(x = "Location", y = "Cumulative incidence of Oropouche fever cases") + 
        theme_classic() +
        theme(legend.position = c(0.7, 0.98),  
              legend.direction = "horizontal",
              legend.justification = "center",
              legend.title = element_blank())  
P_3B

#For statistical analysis 
df_3B_full <- read_excel("Figure 3B.xlsx", sheet = "df_3B_full")

wilcoxon_result <- wilcox.test(OROV_INC ~ Location, data = df_3B_full)
print(wilcoxon_result)
# The Wilcoxon rank sum test (also known as the Mann-Whitney U test) 
# 
# data:  OROV_INC by Location
# W = 149174, p-value < 2.2e-16
# alternative hypothesis: true location shift is not equal to 0

#The extremely small p-value (< 2.2e-16) tells us that there is a significant difference 
#between the Urban and Rural groups in terms of their OROV_INC values.


##### Figure 3. panel C ##### 

df_3C <- read.csv("Figure 3C.csv")

# Ensure the correct order for y-axis
df_3C$Variable <- factor(df_3C$Variable, levels = rev(c("OROV", "DENV", "CHIKV", "ZIKV")))

P_3C <- ggplot(df_3C, aes(y = Variable, x = Median)) +
        geom_point(color = "#74a36f", size = 3.5) +  
        geom_errorbarh(aes(xmin = lower_CI, xmax = upper_CI), height = 0.2, color = "#74a36f") + 
        scale_x_continuous(limits = c(0, 4), breaks = seq(0, 4, by = 1)) +
        geom_vline(xintercept = 1, linetype = "dashed", color = "black") +  # Reference line at OR = 1
        labs(title = NULL, x = "Urban-rural case ratio",y = "Arbovirus cases")+
        theme_classic()
P_3C

