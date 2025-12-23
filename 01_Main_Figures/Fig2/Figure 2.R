#Figure 2

library(pacman)
pacman::p_load(sf, raster, tmap, terra, #handling spatial data
               dplyr,  tidyverse, readxl, openxlsx, ggplot2, ggpubr, hrbrthemes, 
               networkD3, ggbreak, ggridges, ggcorrplot, ggpattern, tidyr, lubridate,
               scales, biscale, corrplot, energy, magick, pdftools,
               RColorBrewer, EpiEstim, logistf, glmnet, plotly, cowplot, patchwork)

##### Figure 2. panels A and B #####

df_2AB <- read_excel("Figure 2AB.xlsx")

colors <- c("Female" = "#98C49C",
            "Male" = "#8ba6C2")

# Ensure AgeGroup and Sex is ordered correctly
df_2AB$Age_group <- factor(df_2AB$Age_group, levels = rev(c("≥60", "50-59", "40-49", "30-39", "20-29", "10-19", "0-9")))
df_2AB$Group <- factor(df_2AB$Sex, levels = c("Female", "Male"))

P_2A <- ggplot(df_2AB, aes(x = Age_group, y = INC_before, fill = Sex)) +
        geom_bar(stat = "identity", position = "dodge") + 
        scale_fill_manual(values = colors) +
        scale_y_continuous(limits = c(0, 1.1), breaks = seq(0, 1, by = 0.2), expand = c(0, 0)) +
        labs(x = "Age Group",y = "Incidence of Oropouche fever cases (2014-22)") +
        theme_classic() +
        theme(legend.position = c(0.2, 0.98),
              legend.direction = "horizontal",
              legend.title = element_blank()) +
        geom_hline(yintercept = 0.582429, linetype = "dashed", color = "black", size = 0.4) 
        #0.5824 is the national-level incidence of Oropouche fever cases (2014-22)
P_2A

P_2B <- ggplot(df_2AB, aes(x = Age_group, y = INC_after, fill = Sex)) +
        geom_bar(stat = "identity", position = "dodge") + 
        scale_fill_manual(values = colors) +
        scale_y_continuous(limits = c(0, 20), breaks = seq(0, 20, by = 2), expand = c(0, 0)) +
        labs(x = "Age Group",y = "Incidence of Oropouche fever cases (2023-25)") +
        theme_classic() +
        theme(legend.position = c(0.2, 0.98),
              legend.direction = "horizontal",
              legend.title = element_blank()) +
        geom_hline(yintercept = 14.26772, linetype = "dashed", color = "black", size = 0.4) 
        #14.2677 is the national-level incidence of Oropouche fever cases (2023-25)
P_2B


# Two-way ANOVA for Age Groups and Genders
# Incidence (2014-22)
anova_age_gender1 <- aov(INC_before ~ Age_group + Sex, data = df_2AB) 
summary(anova_age_gender1)
# Df Sum Sq Mean Sq F value  Pr(>F)   
# Age_group    6 0.8514 0.14189   8.845 0.00895 **
# Sex          1 0.0079 0.00792   0.494 0.50853   
# Residuals    6 0.0963 0.01604                   
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
tukey_test1 <- TukeyHSD(anova_age_gender1)
print(tukey_test1)
# $Age_group
#                    diff         lwr       upr     p adj
# 10-19-0-9    0.14951378 -0.37846853 0.6774961 0.8794410
# 20-29-0-9    0.30729306 -0.22068926 0.8352754 0.3232355
# 30-39-0-9    0.49343543 -0.03454688 1.0214177 0.0663750
# 40-49-0-9    0.49099579 -0.03698652 1.0189781 0.0677330
# 50-59-0-9    0.79672873  0.26874641 1.3247110 0.0071094
# ≥60-0-9      0.54140309  0.01342078 1.0693854 0.0448727
# 20-29-10-19  0.15777928 -0.37020304 0.6857616 0.8541886
# 30-39-10-19  0.34392165 -0.18406066 0.8719040 0.2376517
# 40-49-10-19  0.34148201 -0.18650030 0.8694643 0.2426402
# 50-59-10-19  0.64721495  0.11923263 1.1751973 0.0198942
# ≥60-10-19    0.39188931 -0.13609300 0.9198716 0.1573697
# 30-39-20-29  0.18614238 -0.34183994 0.7141247 0.7538141
# 40-49-20-29  0.18370274 -0.34427958 0.7116850 0.7630760
# 50-59-20-29  0.48943567 -0.03854664 1.0174180 0.0686170
# ≥60-20-29    0.23411004 -0.29387228 0.7620923 0.5653704
# 40-49-30-39 -0.00243964 -0.53042195 0.5255427 1.0000000
# 50-59-30-39  0.30329329 -0.22468902 0.8312756 0.3340320
# ≥60-30-39    0.04796766 -0.48001465 0.5759500 0.9995325
# 50-59-40-49  0.30573293 -0.22224938 0.8337152 0.3274116
# ≥60-40-49    0.05040730 -0.47757501 0.5783896 0.9993818
# ≥60-50-59   -0.25532563 -0.78330795 0.2726567 0.4862466
# 
# $Sex
#                   diff        lwr       upr     p adj
# Male-Female -0.0475787 -0.2132366 0.1180792 0.5085288

# Incidence (2023-25)
anova_age_gender2 <- aov(INC_after ~ Age_group + Sex, data = df_2AB) # incidence after
summary(anova_age_gender2)
#             Df Sum Sq Mean Sq F value   Pr(>F)    
# Age_group    6  343.8   57.30   20.09 0.000992 ***
# Sex          1   11.5   11.49    4.03 0.091485 .  
# Residuals    6   17.1    2.85                     
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
tukey_test2 <- TukeyHSD(anova_age_gender2)
print(tukey_test2)
# $Age_group
#                    diff         lwr       upr     p adj
# 10-19-0-9    8.71068269   1.6702161 15.751149 0.0190355
# 20-29-0-9   14.28236570   7.2418991 21.322832 0.0014711
# 30-39-0-9   14.75273206   7.7122654 21.793199 0.0012307
# 40-49-0-9   14.68325539   7.6427888 21.723722 0.0012632
# 50-59-0-9   13.68953412   6.6490675 20.730001 0.0018551
# ≥60-0-9     10.89224229   3.8517757 17.932709 0.0062534
# 20-29-10-19  5.57168301  -1.4687836 12.612150 0.1258573
# 30-39-10-19  6.04204937  -0.9984173 13.082516 0.0931121
# 40-49-10-19  5.97257270  -1.0678939 13.013039 0.0973249
# 50-59-10-19  4.97885143  -2.0616152 12.019318 0.1846024
# ≥60-10-19    2.18155960  -4.8589070  9.222026 0.8351505
# 30-39-20-29  0.47036636  -6.5701003  7.510833 0.9999198
# 40-49-20-29  0.40088969  -6.6395769  7.441356 0.9999684
# 50-59-20-29 -0.59283158  -7.6332982  6.447635 0.9996962
# ≥60-20-29   -3.39012342 -10.4305901  3.650343 0.4901695
# 40-49-30-39 -0.06947667  -7.1099433  6.970990 1.0000000
# 50-59-30-39 -1.06319794  -8.1036646  5.977269 0.9926654
# ≥60-30-39   -3.86048978 -10.9009564  3.179977 0.3735161
# 50-59-40-49 -0.99372127  -8.0341879  6.046745 0.9948252
# ≥60-40-49   -3.79101311 -10.8314797  3.249454 0.3893571
# ≥60-50-59   -2.79729183  -9.8377585  4.243175 0.6611478
# 
# $Sex
#                 diff        lwr      upr     p adj
# Male-Female 1.812206 -0.3967863 4.021197 0.0914848


##### Figure 2. panel C #####

df_2C <- read.csv("Figure 2C.csv")

# Ensure the correct order for x-axis
df_2C <- df_2C %>%
         mutate(Time = factor(Time, levels = c("2014-2022", "2023-2025")))  

P_2C <- ggplot(df_2C, aes(x = Time, y = `Odds.Ratio`, color = Sex)) + 
        geom_point(size = 3.5, position = position_dodge(width = 0.5)) +  
        geom_errorbar(aes(ymin = Lower_CI, ymax = Upper_CI), width = 0.2, 
                      position = position_dodge(width = 0.5)) +  
        theme_classic() + 
        scale_color_manual(values = c("Female" = "#74a36f",   
                                      "Male"   = "#303b79")) +
        labs(x = NULL, y = "Age-adjusted odds ratio", color = NULL) +
        scale_y_continuous(breaks = c(0.8, 0.9, 1.0, 1.1, 1.2),
                           limits = c(0.8, 1.2)) +
        geom_hline(yintercept = 1, color = "darkgray", linetype = "dashed", size = 0.4) +
        theme_classic() +
        theme(legend.position = c(0.25, 0.95),
              legend.direction = "horizontal") +
        guides(fill = guide_legend(nrow = 1)) 
P_2C


##### Figure 2. panel D ##### 

df_2D <- read.csv("Figure 2D.csv")

# Ensure the correct order for x-axis
df_2D <- df_2D %>%
         mutate(Time = factor(Time, levels = c("2014-2022", "2023-2025")))  

P_2D <- ggplot(df_2D, aes(x = Time, y = OR, color = Variable)) + 
        geom_point(size = 3.5, position = position_dodge(width = 0.5)) +  
        geom_errorbar(aes(ymin = Lower_CI, ymax = Upper_CI), width = 0.2, 
                      position = position_dodge(width = 0.5)) +  
        theme_classic() +  
        labs(title = NULL, x = NULL, y = "Age-adjusted Odds Ratio", 
             color = "Race") +
        scale_y_continuous(breaks = c(0, 10, 40, 50, 60, 70, 75, 100, 140), 
                           limits = c(0, 141)) +
        scale_y_break(c(10,40)) +      
        scale_y_break(c(75,99)) +
        scale_y_break(c(105,138)) +
        scale_color_manual(values = c("Asian" = "#303b79",   
                                      "Black"   = "#849eb9",
                                      "Indigenous" = "#98be9b",   
                                      "Pardo"   = "#74a36f",
                                      "White" = "#485f30")) +
        geom_hline(yintercept = 1, color = "black", 
                   linetype = "dashed", size = 0.4) +
        theme_classic() +
        theme(legend.position = "top",
              axis.line.y.right = element_blank(),
              axis.ticks.y.right = element_blank(),
              axis.text.y.right = element_blank(),
              legend.title = element_blank()) +
        guides(fill = guide_legend(nrow = 1)) 
P_2D


##### Figure 2. panel E ##### 

df_2E <- read.csv("Figure 2E.csv")

# Ensure the correct order for x-axis
df_2E$Race <- factor(df_2E$Race, levels = c("Asian", "Black", "Indigenous", "Pardo", "White"))

P_2E <- ggplot(df_2E, aes(x = Race, y = Positivity_Rate, color = Race)) +
        geom_point(size = 3.5) +  
        geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper), width = 0.2) +  
        scale_color_manual(values = c("Asian" = "#303b79",   
                                      "Black"   = "#849eb9",
                                      "Indigenous" = "#98be9b",   
                                      "Pardo"   = "#74a36f",
                                      "White" = "#485f30")) + 
        labs(title = NULL,
             x = NULL, 
             y = "Positivity Rate (%)") +
        scale_y_continuous(limits = c(0, 10.6), breaks = seq(0, 10, by = 2)) +
        geom_hline(yintercept = 6.3197, linetype = "dashed", color = "black")+
        #6.3197 is the national-level testing positivity rate.
        theme_classic() +
        theme(legend.position = "none") 
P_2E

