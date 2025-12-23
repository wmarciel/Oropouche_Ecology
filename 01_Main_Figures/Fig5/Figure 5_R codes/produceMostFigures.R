library(tidyverse)
library(beepr); options(error = function() {beep(7)})
library(geobr)
library(cowplot)#plot_grid
library(scico)
require(broom) # for tidy()
require(knitr) # for kable()
library(sf) #st_jitter

setwd("Z:/laura/naturehealth")
source("code/settingsFigures.R")
fig_dir = ("results/figs/")

load(file="results/mappingfit_20251001.rda")

do_maps = TRUE
remove_maps = TRUE
remove_stuff = TRUE
make_pdfs = TRUE

#setup: load data -----
necessary_muni_info = read.csv("data/analysis_dfs/normed_municipal_info_compiled.csv")
unnormed_muni <- read.csv("data/analysis_dfs/municipal_info_compiled.csv")

monthly_normals <- read.csv("data/analysis_dfs/monthly_clim_normalized_as_priorsubsequent_weather2025-09-26.csv") |>  full_join(necessary_muni_info)

map_predictions <- full_join(monthly_normals, necessary_muni_info)

crop_info <- read.csv("data/analysis_dfs/cropland_normalized_2025-09-29.csv")
crop_info <- crop_info |> select(code_muni:name_region, all_of(contains(c("banana",
                                                              "cassava","rubber")
))) 

map_predictions <- full_join(map_predictions, crop_info)
map_predictions <- full_join(map_predictions, unnormed_muni |> select(code_muni, total_population))

## Make predictions -----

map_predictions$predicted <- predict(mappingfit, map_predictions, type = "response")

map_predictions |>  group_by(name_region,name_state,code_muni) |> summarise(
  mean_risk = mean(predicted),
  median_risk = median(predicted),
  max_risk = max(predicted),
  min_risk = min(predicted),
  risk_range = max_risk-min_risk
) -> for_mapping
  
map_predictions |>  group_by(name_region,name_state, code_muni, name_muni, total_population) |> summarise(
  mean_risk = mean(predicted),
  max_risk = max(predicted),
  max_month = which.max(predicted),#it's in order by month alreadu
  min_risk = min(predicted),
  min_month = which.min(predicted), #it's in order by month alreadu
  risk_range = max_risk-min_risk
) -> overall

#oddball seasonality -----
summary(factor(overall$max_month))
sum(overall$max_month %in% c(12,1:3))
sum(overall$max_month %in% c(12,1:3))/length(overall$max_month)

summary(factor(overall$max_month))
sum(overall$max_month %in% c(12,1:3))
sum(overall$max_month %in% c(12,1:3))/length(overall$max_month)

summary(factor(overall$min_month)) #!!
sum(overall$min_month %in% c(5:6))/length(overall$min_month)
sum(overall$min_month %in% c(5:8))
sum(overall$min_month %in% c(5:8))/length(overall$min_month)

oddball_idx <- which(overall$max_month %in% c(4:6))

oddball_munis <- overall$code_muni[oddball_idx]

summary(as.factor(overall$name_state[oddball_idx]))
summary(as.factor(overall$name_region[oddball_idx]))
summary(as.factor(overall$min_month[oddball_idx]))

map_predictions$oddballs <- as.character(map_predictions$name_region) 
map_predictions$oddballs[map_predictions$code_muni %in% overall$code_muni[oddball_idx]] <- "Out of Phase"
map_predictions$oddballs <- factor(map_predictions$oddballs, levels = c("Norte", "Nordeste",  "Centro Oeste", "Sudeste", "Sul", "Out of Phase"))

map_predictions |> group_by(oddballs, month) |> summarise(
  mean_risk = mean(predicted),
  poprisk = sum(total_population*mean_risk),
  weighted_mean_risk = weighted.mean(predicted, total_population),
  median_risk = median(predicted),
  q1 = quantile(predicted, .25),
  q3  = quantile(predicted, .75),
  q3  = quantile(predicted, .90),
  q3  = quantile(predicted, .95),
  max_risk = max(predicted),
  min_risk = min(predicted),
  risk_range = max_risk-min_risk,
  total_population = sum(total_population),
  mean = mean(predicted),
  sd = sd(predicted),
  se = sd / sqrt(n()),
  CI_lower = mean - (1.96 * se),
  CI_upper = mean + (1.96 * se),
  # mean_rain_flux = mean(rain_prior_30),
  # sd_rain = sd(rain_prior_30),
  # se_rain = sd_rain / sqrt(n()),
  # CI_lower_rain = mean_rain_flux - (1.96 * se_rain),
  # CI_upper_rain = mean_rain_flux + (1.96 * se_rain),
) -> oddball_by_month

oddball_by_month$month_name <- factor(month.name[oddball_by_month$month], levels=month.name[1:12])


group.colors <- c("Norte" = "#610061", "Nordeste" = "darkblue", "Centro Oeste" ="darkgreen", "Sul" = "goldenrod", "Sudeste" = "darkred", "Out of Phase" = "black")

reg_by_mon <- ggplot() + theme_cowplot() + close_theme+ #theme(legend.position = "bottom")+
  ylim(0, .5) +
  labs(y= "Mean Risk in Region", x = "Month", color = "Region") + 
  scale_x_continuous(breaks=1:12,labels=month.abb[1:12]) +
  # geom_point(data = oddball_by_month, aes(x = month, y = median_risk, color = oddballs)) +geom_smooth(data = oddball_by_month, aes(x = month, y = median_risk, color = oddballs), se = FALSE,linetype = 2) +
  geom_point(data = oddball_by_month, aes(x = month, y = mean_risk, color = oddballs)) +
  # geom_smooth(data = oddball_by_month, aes(x = month, y = mean_risk, color = oddballs), se = FALSE, alpha = .5) + 
  geom_line(stat="smooth",data = oddball_by_month, aes(x = month, y = mean_risk, color = oddballs), method = "loess",
            se = FALSE,size = 1.5,alpha = 0.25) +
  geom_errorbar(data = oddball_by_month, aes(color = oddballs, group = oddballs,x = month,ymin = CI_lower, ymax =CI_upper), width = 0) +
  scale_color_manual(values=group.colors); if(do_maps ==TRUE) reg_by_mon


state.colors <- c(#"Maranhão" = "#3333a2", 
                  "Pernambuco" = "#6666b9",   
                  "Amapá" = "#7d007d",      
                  "Amazônas" = "#a233a2",    
                  "Pará" = "#b966b9",         
                  "Roraima" = "#d199d1")

oddities <- subset(shape_munis, code_muni %in% overall$code_muni[oddball_idx])
oddities$name_state <- factor(oddities$name_state, levels = c("Amapá", "Amazônas", "Pará","Roraima",   "Pernambuco"))
# odd_muni_seats <- read_municipal_seat() |> mutate(across(contains("code"), as.factor)); odd_muni_seats <- left_join(odd_muni_seats, muni_info) |> subset(name_region == "Nordeste")


seasonal_oddballs_map <-   ggplot()   + xlim(-72.5, -34) +  #don't cut out Pernambuco island!
  geom_sf(data = shape_regions, col = "black", fill = NA) + geom_hline(yintercept = 0, color = "lightgray") +
  geom_sf(data = oddities,aes(fill = name_state, group = name_region), color = "white", linewidth = .1) +  scale_fill_manual(values=state.colors) +     labs(fill = "State")+
  theme(panel.background = element_rect(fill = 'transparent'),
        panel.grid.major.y = element_line(size = 0.5, color = "gray60"), 
        panel.grid.major.x = element_blank(),axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks=element_blank(),plot.margin = margin(t = 0,  # Top margin
                                                        r = 0,  # Right margin
                                                        b = 0,  # Bottom margin
                                                        l = 0)) + scale_y_continuous(breaks=c(-90,0,90), limits = c(-32,6)) + 
  # theme(legend.position="bottom", legend.margin=margin(c(0,1,3,1))) +
  geom_sf(data = shape_states, col = "gray50", fill = NA) +
  geom_sf(data = shape_regions, col = "black", linewidth = .5, fill = NA); if(do_maps ==TRUE) seasonal_oddballs_map
 
 ggsave(file=paste0(fig_dir ,"/SuppFig_seasonaloddballs", fig_type), 
        cowplot::plot_grid(reg_by_mon,
                  seasonal_oddballs_map,
                  nrow = 1,
                  labels = c('A', 'B'),
                  align="hv"), 
        width=save.width*2, height = save.height*.7)
 
 if(make_pdfs == TRUE) ggsave(file=paste0(fig_dir ,"/SuppFig_seasonaloddballs", ".pdf"), 
                              cowplot::plot_grid(reg_by_mon,
                                        seasonal_oddballs_map,
                                        nrow = 1,
                                        labels = c('A', 'B'),
                                        align="hv"), 
                              width=save.width*2, height = save.height*.8)
 if(remove_maps == TRUE) rm(seasonal_oddballs_map)

# map setup -----

# Main Paper Figure: mean, max, min, and range of risk -----
for_pred <- left_join(shape_munis, for_mapping)  %>% 
  mutate(across(
    .cols = mean_risk:risk_range,
    .fns = function(x) round(x*100),
    .names = "{.col}_pctround"
  ))
continouous = FALSE
if(continouous == TRUE) {blue_scale = blue_scale_continouous;red_scale=red_scale_continuous}
if(continouous == FALSE) {blue_scale = blue_scale_binned;red_scale=red_scale_binned}

map_mean_risk <- ggplot() + theme_void() + close_theme + xlim(-73, -36)  +  
  labs(fill = "Mean Risk") +
  geom_sf(data = for_pred, aes(fill = mean_risk_pctround),col = NA) + 
  blue_scale +  
  geom_sf(data = shape_states, col = "white", fill = NA);if(do_maps ==TRUE) map_mean_risk

map_range_risk <- ggplot() + theme_void() + close_theme + xlim(-73, -36)  + 
  labs(fill = "Range") +
  geom_sf(data = for_pred, aes(fill = risk_range_pctround),col = NA) + 
  red_scale +  
  geom_sf(data = shape_states, col = "white", fill = NA);if(do_maps ==TRUE) map_range_risk

map_max_risk <- ggplot() + theme_void() + close_theme + xlim(-73, -36)  + 
  labs(fill = "Max Risk") +
  geom_sf(data = for_pred, aes(fill = max_risk_pctround),col = NA) + 
  blue_scale +  
  geom_sf(data = shape_states, col = "white", fill = NA);if(do_maps ==TRUE) map_max_risk

map_min_risk <- ggplot() + theme_void() + close_theme + xlim(-73, -36)  + 
  labs(fill = "Min Risk") +
  geom_sf(data = for_pred, aes(fill = min_risk_pctround),col = NA) + 
  blue_scale +  
  geom_sf(data = shape_states, col = "white", fill = NA) + 
  geom_sf(data = shape_brazil, col = "gray70", fill = NA);if(do_maps ==TRUE) map_min_risk

fig5grid <- cowplot::plot_grid(map_mean_risk,
                      map_range_risk,
                      map_max_risk,
                      map_min_risk,
                      labels = c('A', 'B', 'C', 'D'),
                      align="hv"); beep()
if(continouous == TRUE) figname= "/continuousFig5_risk-mean-max-min-range"
if(continouous == FALSE) figname= "/Fig5_risk-mean-max-min-range"

ggsave(file=paste0(fig_dir ,figname, fig_type), 
       fig5grid, 
       width=save.width*2, height = save.height*2)

if(make_pdfs == TRUE) ggsave(file=paste0(fig_dir ,figname, ".pdf"), 
                             fig5grid, 
                             width=save.width*2, height = save.height*2)


if(remove_maps == TRUE) rm(map_mean_risk,
                           map_range_risk,
                           map_max_risk,
                           map_min_risk,
                           fig5grid)


#Supplemental Figure: Introductions and Incidence ----
weather_fit <- read.csv( "data/analysis_dfs/outbreaks_through_2025-07-31_with_weather.csv")

totals <- weather_fit |>  group_by(code_muni) |> summarise(total_cases_over_period = sum(sum_cases))

# shape_brazil <- read_country(); shape_states <- read_state()
# shape_munis <- read_municipality(year = 2020) |>  mutate(across(contains("code"), as.factor))

muni_seats <- read_municipal_seat() 
plot_intros <- right_join(muni_seats, weather_fit)
plot_intros <- left_join(plot_intros, totals)
# shape_munis <- left_join(shape_munis, sf::st_drop_geometry(dplyr::select(plot_intros, c("code_muni", "incper100k"))))

shape = 2
plot_isolated <- subset(plot_intros, case_outcome == "ISOLATED")
plot_outbreaks <- subset(plot_intros, case_outcome == "OUTBREAK")
# plot_intros <- subset(right_join(muni_seats, weather_fit), st_date <= as.Date("2023-01-01"))


map_introductions <- ggplot() + theme_void() + close_theme + xlim(-73, -36)  + 
  # labs(title=paste("Isolated Oropouche Cases")) +
  geom_sf(data = shape_munis, col = "gray90", fill = NA) +
  geom_sf(data = sf::st_jitter(plot_isolated, factor = 0.003), aes(size = sum_cases), shape = 18,  alpha = .25, color = "blue") + geom_sf(data = shape_states, col = "black", fill = NA)  + scale_size_area(max_size = 3,breaks = c(1,2), limits = c(1,2)) +   guides(size=guide_legend(title="Cases"))  ; if(do_maps ==TRUE) map_introductions

map_outbreaks<- ggplot() + theme_void() + close_theme + xlim(-73, -36)  + 
  # labs(title=paste("Oropouche Outbreaks")) +
  geom_sf(data = shape_munis, col = "gray90", fill = NA) +
  geom_sf(data = sf::st_jitter(plot_outbreaks, factor = 0.003), aes(size = sum_cases),color = "darkred", alpha = .1)+ scale_size_area(max_size = 30,breaks = c(1,5,10,50,100,500,1000,1500), limits = c(0,1500)) +
  # scale_size_continuous(breaks = c(10,100,1000), range = c(0,1300)) +
  guides(size=guide_legend(title="Cases in Outbreak")) +
  geom_sf(data = shape_states, col = "black", fill = NA) ;if(do_maps ==TRUE) map_outbreaks

introfiggrid_raw <- cowplot::plot_grid(map_introductions,
                              map_outbreaks,
                              nrow = 1,
                              labels = c('A', 'B'),
                              align="hv"); beep()


ggsave(file=paste0(fig_dir ,"/SuppFig_introsoutbreaks", fig_type), 
       introfiggrid_raw, 
       width=save.width*2, height = save.height*.8);beep()

if(make_pdfs == TRUE) ggsave(file=paste0(fig_dir ,"/SuppFig_introsoutbreaks", ".pdf"), 
                             introfiggrid_inc, 
                             width=save.width*2, height = save.height*.8)

if(remove_maps == TRUE) rm(map_introductions,
                           map_outbreaks_per100k,
                           introfiggrid_raw,
                           introfiggrid_inc)


# Prospective Supplemental: Banana Cultivation----

# banana_percentile <- quantile(necessary_muni_info$pct_crop_banana_mean , c(.5, .75, .9, .95, .99))
# many <- which(necessary_muni_info$pct_crop_banana_mean > 5)

signed_quart_trans <- function(){
  scales::trans_new(
    name = "signed_quart",
    transform = function(x) {
      sign(x) * pracma::nthroot(abs(x), n = 4)
    },
    inverse = function(x) {
      x ^4 * sign(x)
    }
  )
}
banana <- right_join(shape_munis, necessary_muni_info[, c("code_muni", 
                                                "pct_crop_banana_mean",
                                                # "banana_percentile",
                                                # "pct_crop_banana_mean_sqrt",
                                                "pct_crop_coffee_mean")])

nanners = c('white', "#ffff80","goldenrod","red","darkred")
banana_map <- ggplot() + theme_void() + close_theme + xlim(-73, -36)  +  
  labs(fill = "Mean Area (%)") +
  geom_sf(data = banana, aes(fill = pct_crop_banana_mean),col = NA) +
  # scale_fill_distiller(palette="YlOrRd", direction = 1 , trans = "signed_quart") +
  scale_fill_gradientn(colors = nanners, trans = "signed_quart")+
  geom_sf(data = shape_regions, col = "black", fill = NA)
# banana_map

java = c('white',"#fcf4e8","#f9e8d2","wheat","tan","#7e481c","#622a0f","#492200","#1d0200")
coffee_map <- ggplot() + theme_void() + close_theme + xlim(-73, -36)  +  
  labs(fill = "Mean Area (%)") +
  geom_sf(data = banana, aes(fill = pct_crop_coffee_mean),col = NA) +
  # scale_fill_distiller(palette="YlOrRd", direction = 1 ) +
  scale_fill_gradientn(colors = java, trans = "signed_quart") +
  geom_sf(data = shape_regions, col = "black", fill = NA);#coffee_map

ggsave(file=paste0(fig_dir ,"/SuppFig_crop_map", fig_type), 
       cowplot::plot_grid(banana_map,coffee_map,
                 nrow = 1,
                 labels = LETTERS[1:2],
                 align="hv"), 
       width=save.width*2, height = save.height*.8)

if(make_pdfs == TRUE) ggsave(file=paste0(fig_dir ,"/SuppFig_crop_map", ".pdf"), 
                             cowplot::plot_grid(banana_map,coffee_map,
                                       nrow = 1,
                                       labels = LETTERS[1:2],
                                       align="hv"), 
                             width=save.width*2, height = save.height*.8)


#end----
