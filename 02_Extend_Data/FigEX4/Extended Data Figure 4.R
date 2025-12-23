#Extended Data Figure 4

library(pacman)
pacman::p_load(sf, raster, tmap, terra, #handling spatial data
               dplyr,  tidyverse, readxl, openxlsx, ggplot2, ggpubr, hrbrthemes, 
               networkD3, ggbreak, ggridges, ggcorrplot, ggpattern, tidyr, lubridate,
               scales, biscale, corrplot, energy, magick, pdftools,
               RColorBrewer, EpiEstim, logistf, glmnet, plotly, cowplot, patchwork)

##### Extended Data Figure 4. panel A ##### 

df_S4A <- read.csv("Figure S4A.csv")

colors <- c("Female" = "#98C49C",
            "Male" = "#8ba6C2")

# Ensure AgeGroup is ordered correctly
df_S4A$AgeGroup <- factor(df_S4A$AgeGroup, levels = rev(c("≥60", "50-59", "40-49", "30-39", "20-29", "10-19", "0-9")))
df_S4A$Group <- factor(df_S4A$Sex, levels = c("Female", "Male"))

P_S4A <- ggplot(df_S4A, aes(x = AgeGroup, y = INC, fill = Sex)) +
         geom_bar(stat = "identity", position = "dodge") + 
         scale_fill_manual(values = colors) +
         labs(title = "Rural",
              x = "Age Group",y = "Incidence of Oropouche fever cases - Rural municipalities") +
         scale_y_continuous(limits = c(0, 60), breaks = seq(0, 60, by = 10), , expand = c(0, 0)) +
         theme_classic() +
         theme(legend.position = c(0.15, 0.98),
               legend.direction = "horizontal",
               legend.title = element_blank()) 
P_S4A


# Two-way ANOVA for Age Groups and Genders - using incidence - Rural
anova_age_gender3 <- aov(INC ~ AgeGroup + Sex, data = df_S4A) 
summary(anova_age_gender3)
#             Df Sum Sq Mean Sq F value   Pr(>F)    
# AgeGroup     6   3429   571.5  39.307 0.000147 ***
# Sex          1     13    12.6   0.865 0.388335    
# Residuals    6     87    14.5                     
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
tukey_test3 <- TukeyHSD(anova_age_gender3)
print(tukey_test3)
# $AgeGroup
#                  diff        lwr        upr     p adj
# 10-19-0-9    22.44480   6.549572 38.3400275 0.0099592
# 20-29-0-9    40.74305  24.847822 56.6382775 0.0003983
# 30-39-0-9    44.53955  28.644322 60.4347775 0.0002404
# 40-49-0-9    48.17840  32.283172 64.0736275 0.0001540
# 50-59-0-9    42.47025  26.575022 58.3654775 0.0003148
# ≥60-0-9      32.01150  16.116272 47.9067275 0.0015310
# 20-29-10-19  18.29825   2.403022 34.1934775 0.0267096
# 30-39-10-19  22.09475   6.199522 37.9899775 0.0107748
# 40-49-10-19  25.73360   9.838372 41.6288275 0.0049387
# 50-59-10-19  20.02545   4.130222 35.9206775 0.0174530
# ≥60-10-19     9.56670  -6.328528 25.4619275 0.2963411
# 30-39-20-29   3.79650 -12.098728 19.6917275 0.9377262
# 40-49-20-29   7.43535  -8.459878 23.3305775 0.5166916
# 50-59-20-29   1.72720 -14.168028 17.6224275 0.9987298
# ≥60-20-29    -8.73155 -24.626778  7.1636775 0.3719541
# 40-49-30-39   3.63885 -12.256378 19.5340775 0.9478541
# 50-59-30-39  -2.06930 -17.964528 13.8259275 0.9966188
# ≥60-30-39   -12.52805 -28.423278  3.3671775 0.1277071
# 50-59-40-49  -5.70815 -21.603378 10.1870775 0.7405338
# ≥60-40-49   -16.16690 -32.062128 -0.2716725 0.0464862
# ≥60-50-59   -10.45875 -26.353978  5.4364775 0.2307003
# 
# $Sex
#                 diff       lwr      upr     p adj
# Male-Female 1.895171 -3.092059 6.882402 0.3883348


##### Extended Data Figure 4. panel B ##### 

df_S4B <- read.csv("Figure S4B.csv")

colors <- c("Female" = "#98C49C",
            "Male" = "#8ba6C2")

# Ensure AgeGroup is ordered correctly
df_S4B$AgeGroup <- factor(df_S4B$AgeGroup, levels = rev(c("≥60", "50-59", "40-49", "30-39", "20-29", "10-19", "0-9")))
df_S4B$Group <- factor(df_S4B$Sex, levels = c("Female", "Male"))

P_S4B <- ggplot(df_S4B, aes(x = AgeGroup, y = INC, fill = Sex)) +
         geom_bar(stat = "identity", position = "dodge") + 
         scale_fill_manual(values = colors) +
         labs(title = "Urban",
              x = "Age Group",y = "Incidence of Oropouche fever cases - Urban municipalities") +
         scale_y_continuous(limits = c(0, 10), breaks = seq(0, 10, by = 2), , expand = c(0, 0)) +
         theme_classic() +
         theme(legend.position = c(0.15, 0.98),
               legend.direction = "horizontal",
               legend.title = element_blank()) 
P_S4B


# Two-way ANOVA for Age Groups and Genders - using incidence - Urban
anova_age_gender4 <- aov(INC ~ AgeGroup + Sex, data = df_S4B) 
summary(anova_age_gender4)
#             Df Sum Sq Mean Sq F value  Pr(>F)   
# AgeGroup     6   83.9  13.984  13.744 0.00281 **
# Sex          1    1.7   1.697   1.668 0.24405   
# Residuals    6    6.1   1.017                   
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
tukey_test4 <- TukeyHSD(anova_age_gender4)
print(tukey_test4)
# $AgeGroup
#                 diff          lwr       upr     p adj
# 10-19-0-9    4.20020 -0.004584845  8.404985 0.0502338
# 20-29-0-9    7.40640  3.201615155 11.611185 0.0031621
# 30-39-0-9    7.33860  3.133815155 11.543385 0.0033211
# 40-49-0-9    6.95010  2.745315155 11.154885 0.0044293
# 50-59-0-9    6.24990  2.045115155 10.454685 0.0076808
# ≥60-0-9      4.62965  0.424865155  8.834435 0.0327368
# 20-29-10-19  3.20620 -0.998584845  7.410985 0.1434975
# 30-39-10-19  3.13840 -1.066384845  7.343185 0.1544252
# 40-49-10-19  2.74990 -1.454884845  6.954685 0.2348821
# 50-59-10-19  2.04970 -2.155084845  6.254485 0.4789328
# ≥60-10-19    0.42945 -3.775334845  4.634235 0.9990987
# 30-39-20-29 -0.06780 -4.272584845  4.136985 1.0000000
# 40-49-20-29 -0.45630 -4.661084845  3.748485 0.9987390
# 50-59-20-29 -1.15650 -5.361284845  3.048285 0.8916621
# ≥60-20-29   -2.77675 -6.981534845  1.428035 0.2282190
# 40-49-30-39 -0.38850 -4.593284845  3.816285 0.9994859
# 50-59-30-39 -1.08870 -5.293484845  3.116085 0.9139102
# ≥60-30-39   -2.70895 -6.913734845  1.495835 0.2453947
# 50-59-40-49 -0.70020 -4.904984845  3.504585 0.9880047
# ≥60-40-49   -2.32045 -6.525234845  1.884335 0.3679704
# ≥60-50-59   -1.62025 -5.825034845  2.584535 0.6862689
# 
# $Sex
#                  diff        lwr      upr     p adj
# Male-Female 0.6963286 -0.6229498 2.015607 0.2440471
