library(tidyverse)
# library(geobr)
library(lubridate) # %m-% months(1)
# library(incidence)
library(zoo) #rolling means
library(readr)

setwd("Z:/laura/naturehealth")

analysis_dir = "data/analysis_dfs"
date_normed = "2025-09-26"

window = 30; lag1 = window; lag2 = window*2; lag3 = window*3
cases <- read.csv("data/additional_data_files/linelist_2025-09-23_positive.csv")
pop <- read.csv("data/additional_data_files/population_municipalities_2022.csv")

munis_with_cases <- unique(cases$six_digit_id)
pop$has_cases <- pop$six_digit_id %in% munis_with_cases
muni_code_with_cases <- pop$code_muni[which(pop$has_cases)]
rm(munis_with_cases)

#df for data normalization-----
keep_cols = c("name_muni","code_muni","abbrev_state", "date")

normalizingFunction <- function(unnormalized_data, keep_cols, return_norming_df = TRUE){
  cols_to_norm = names(unnormalized_data)[names(unnormalized_data) %in% keep_cols == FALSE]
  norming_df <- data.frame(pred_name = cols_to_norm, mean = NA, sd = NA);  

  normed_data <- unnormalized_data[, keep_cols]
  normed_data[,paste0(cols_to_norm,"_normalized")] = NA
  
  for(i in 1:nrow(norming_df)){
    ctn = norming_df$pred_name[i]
    norming_df$mean[i] = mean(unnormalized_data[, ctn], na.rm = TRUE)
    norming_df$sd[i] = sd(unnormalized_data[, ctn], na.rm = TRUE)
    
    normed_data[,paste0(ctn,"_normalized")] <- (unnormalized_data[,ctn] - norming_df$mean[i])/norming_df$sd[i] 
    
  }
  if(return_norming_df == TRUE) return(list(norming_df=norming_df,normed_data=normed_data))
  if(return_norming_df == FALSE) return(normed_data=normed_data)
  
}

#weather -------
daily_weather_dir = "data/extracted_weather/"
weather_withcases_dir = "data/extracted_weather/"

weather_types = c(
  "Temperature-Air-2m-Max-24h",
  "Temperature-Air-2m-Mean-24h",
  "Temperature-Air-2m-Min-24h",
  "Precipitation-Flux",
  "Vapour-Pressure-Mean"
)
##normalizing weather-----
for(w in 1:length(weather_types)){
  weath = read.csv(paste0(daily_weather_dir,weather_types[w],"_consolidated.csv"))
  norms <- normalizingFunction(weath, keep_cols)
  
  write.csv(norms$normed_data, paste0(daily_weather_dir, weather_types[w],"_normalized_", today(),".csv"), row.names = FALSE)
  write.csv(norms$norming_df, paste0(daily_weather_dir, weather_types[w],"_normingDF_", today(),".csv"), row.names = FALSE)
  
  write.csv(subset(norms$normed_data, code_muni %in% muni_code_with_cases)
, paste0(weather_withcases_dir, weather_types[w],"_withcases_normalizedtoall_", today(),".csv"), row.names = FALSE)
  write.csv(norms$norming_df, paste0(weather_withcases_dir, weather_types[w],"_normingDF_", today(),".csv"), row.names = FALSE)
  
  
  write.csv(norms$norming_df, paste0(weather_withcases_dir, weather_types[w],"_normingDF_", today(),".csv"))
  print(w)
  
  rm(weath, norms)
}

##consolidating weather------
keep_cols <- c("name_muni","code_muni","abbrev_state","date")
i = 1; t_max = read.csv(paste0(weather_withcases_dir,weather_types[i],"_withcases_normalizedtoall_", date_normed, ".csv"))

consol_weather <- t_max[,c(keep_cols, "temp_max_normalized")]
rm(t_max)

i = 2; t_mean = read.csv(paste0(weather_withcases_dir,weather_types[i],"_withcases_normalizedtoall_", date_normed, ".csv"))
consol_weather = full_join(consol_weather, t_mean[,c(keep_cols, "temp_mean_normalized")]);rm(t_mean)

i = 3; t_min = read.csv(paste0(weather_withcases_dir,weather_types[i],"_withcases_normalizedtoall_", date_normed, ".csv"))
consol_weather = full_join(consol_weather, t_min[,c(keep_cols, "temp_min_normalized")]);rm(t_min)

i = 4; rain = read.csv(paste0(weather_withcases_dir,weather_types[i],"_withcases_normalizedtoall_", date_normed, ".csv"))
consol_weather = full_join(consol_weather, rain[,c(keep_cols, "precip_normalized")]);rm(rain)

i = 5; vp = read.csv(paste0(weather_withcases_dir,weather_types[i],"_withcases_normalizedtoall_", date_normed, ".csv"))
consol_weather = full_join(consol_weather, vp[,c(keep_cols, "vapor_normalized")]);rm(vp)

write.csv(consol_weather, paste0(weather_withcases_dir,weather_types[i],"_withcases_normalizedtoall_", date_normed, ".csv"))

## get lagged weather -----
window = 30; lag1 = window; lag2 = window*2; lag3 = window*3
weath_col_names <- c( "temp_max_normalized","temp_mean_normalized","temp_min_normalized","precip_normalized","vapor_normalized")

weath_means <- consol_weather  %>% 
  arrange(abbrev_state,code_muni, date) %>% group_by(code_muni) %>% 
  mutate(
    "temp_max_normalized_mean_subsequent_{window}" := rollmean(temp_max_normalized, k = window, fill=NA, align= 'left'),
    "temp_max_normalized_mean_prior_{window}" := rollmean(temp_max_normalized, k = window, fill=NA, align= 'right'),
    "temp_max_normalized_mean_prior_{lag2}" := rollmean(temp_max_normalized, k = lag2, fill=NA, align= 'right'),
    "temp_max_normalized_mean_prior_{lag3}" := rollmean(temp_max_normalized, k = lag3, fill=NA, align= 'right'),
    
    "temp_mean_normalized_mean_subsequent_{window}" := rollmean(temp_mean_normalized, k = window, fill=NA, align= 'left'),
    "temp_mean_normalized_mean_prior_{window}" := rollmean(temp_mean_normalized, k = window, fill=NA, align= 'right'),
    "temp_mean_normalized_mean_prior_{lag2}" := rollmean(temp_mean_normalized, k = lag2, fill=NA, align= 'right'),
    "temp_mean_normalized_mean_prior_{lag3}" := rollmean(temp_mean_normalized, k = lag3, fill=NA, align= 'right'),
    
    "temp_min_normalized_mean_subsequent_{window}" := rollmean(temp_min_normalized, k = window, fill=NA, align= 'left'),
    "temp_min_normalized_mean_prior_{window}" := rollmean(temp_min_normalized, k = window, fill=NA, align= 'right'),
    "temp_min_normalized_mean_prior_{lag2}" := rollmean(temp_min_normalized, k = lag2, fill=NA, align= 'right'),
    "temp_min_normalized_mean_prior_{lag3}" := rollmean(temp_min_normalized, k = lag3, fill=NA, align= 'right'),
    
    "precip_normalized_mean_subsequent_{window}" := rollmean(precip_normalized, k = window, fill=NA, align= 'left'),
    "precip_normalized_mean_prior_{window}" := rollmean(precip_normalized, k = window, fill=NA, align= 'right'),
    "precip_normalized_mean_prior_{lag2}" := rollmean(precip_normalized, k = lag2, fill=NA, align= 'right'),
    "precip_normalized_mean_prior_{lag3}" := rollmean(precip_normalized, k = lag3, fill=NA, align= 'right'),
    
    "vapor_normalized_mean_subsequent_{window}" := rollmean(vapor_normalized, k = window, fill=NA, align= 'left'),
    "vapor_normalized_mean_prior_{window}" := rollmean(vapor_normalized, k = window, fill=NA, align= 'right'),
    "vapor_normalized_mean_prior_{lag2}" := rollmean(vapor_normalized, k = lag2, fill=NA, align= 'right'),
    "vapor_normalized_mean_prior_{lag3}" := rollmean(vapor_normalized, k = lag3, fill=NA, align= 'right')
    
  );

write.csv(weath_means, paste0(weather_withcases_dir,"consolidated_weather_and_rollmeans_withcases_normalizedtoall_", date_normed, ".csv"), row.names = FALSE)

## join weather means to outbreak info ----
outbreak_df <- read.csv("data/analysis_dfs/outbreaks_through_2025-07-31.csv")
weath <- read.csv(paste0(weather_withcases_dir,"consolidated_weather_and_rollmeans_withcases_normalizedtoall_", date_normed, ".csv"))
ob <- outbreak_df[, c("st_date","sum_cases","period","location","case_outcome")] |> rename(code_muni = location)

ob <- left_join(ob, weath, join_by(st_date == date, code_muni))
  
write.csv(ob,"data/analysis_dfs/outbreaks_through_2025-07-31_with_weather.csv", row.names = FALSE)

rm(ob, outbreak_df, weath_means, consol_weather)

#normalizing monthly climate as weather ----
month_clim_as_weather <- read.csv("extracted_landscape/climate_monthly.csv")
month_clim_as_weather$vapor <- month_clim_as_weather$vapr*10 #hPa vs kPa
month_clim_as_weather$precip <- month_clim_as_weather$prec/days_in_month(month_clim_as_weather$month) #per day instead of per month

normdf_precip <- read.csv("consolidated_weather/Precipitation-Flux_normingDF_", date_normed, ".csv")
normdf_vapor <- read.csv("consolidated_weather/Vapour-Pressure-Mean_normingDF_", date_normed, ".csv")
normdf_tempmin <- read.csv("consolidated_weather/Temperature-Air-2m-Min-24h_normingDF_", date_normed, ".csv")
normdf_tempmean <- read.csv("consolidated_weather/Temperature-Air-2m-Mean-24h_normingDF_", date_normed, ".csv")
normdf_tempmax <- read.csv("consolidated_weather/Temperature-Air-2m-Max-24h_normingDF_", date_normed, ".csv")

month_clim_as_weather$precip_normalized_mean <- (month_clim_as_weather$precip  - 
                                                   normdf_precip$mean[which(normdf_precip$pred_name == "precip")])/
  normdf_precip$sd[normdf_precip$pred_name  == "precip"]

month_clim_as_weather$vapor_normalized_mean <- (month_clim_as_weather$vapor  - 
                                                  normdf_vapor$mean[which(normdf_vapor$pred_name == "vapor")])/
  normdf_vapor$sd[normdf_vapor$pred_name == "vapor"]


month_clim_as_weather$temp_min_normalized_mean <-
  (month_clim_as_weather$tmin  - 
     normdf_tempmin$mean[which(normdf_tempmin$pred_name == "temp_min")])/
  normdf_tempmin$sd[which(normdf_tempmin$pred_name == "temp_min")]

month_clim_as_weather$temp_mean_normalized_mean <-
  (month_clim_as_weather$tavg - 
     normdf_tempmean$mean[which(normdf_tempmean$pred_name == "temp_mean")])/
  normdf_tempmean$sd[which(normdf_tempmean$pred_name == "temp_mean")]

month_clim_as_weather$temp_max_normalized_mean <-
  (month_clim_as_weather$tmax  - 
     normdf_tempmax$mean[which(normdf_tempmax$pred_name == "temp_max")])/
  normdf_tempmax$sd[which(normdf_tempmax$pred_name == "temp_max")]

month_clim_as_weather <- month_clim_as_weather |> select(code_muni:month, all_of(contains("normalized")))

## make prior/subsequent ------
weather_cols <- which(names(month_clim_as_weather) %in% names(month_clim_as_weather |> select(all_of(contains("normalized")))))

prior_month <- month_clim_as_weather; names(prior_month)[weather_cols] <- paste0(names(month_clim_as_weather)[weather_cols],"_prior_30")
subseq_month <- month_clim_as_weather;names(subseq_month)[weather_cols] <- paste0(names(month_clim_as_weather)[weather_cols],"_subsequent_30")
subseq_month$month <- month_clim_as_weather$month - 1; subseq_month$month[subseq_month$month == 0] <- 12

month_clim_as_weather_for_plotting <- full_join(prior_month, subseq_month)

write.csv(month_clim_as_weather_for_plotting,paste0(analysis_dir, "monthly_clim_normalized_as_priorsubsequent_weather",date_normed,".csv"), row.names = FALSE)
write.csv(month_clim_as_weather,paste0(analysis_dir, "monthly_clim_normalized_as_weather",date_normed,".csv"), row.names = FALSE)

rm(month_clim_as_weather_for_plotting, month_clim_as_weather, prior_month, subseq_month)

#normalize  mean monthly weather------
weather_types = c(
  "Temperature-Air-2m-Max-24h",
  "Temperature-Air-2m-Mean-24h",
  "Temperature-Air-2m-Min-24h",
  "Precipitation-Flux",
  "Vapour-Pressure-Mean"
)

### monthly tempmax mean norming -----
w <- weather_types[1] ;weath <- read.csv(paste0("consolidated_weather/monthly_", w, ".csv")) |> select(abbrev_state:mean_temp_max, plot_date); weath$plot_date <- as.Date(weath$plot_date)

weath$temp_max_normalized_mean_subsequent_30 <- 
  (weath$mean_temp_max  - 
     normdf_tempmax$mean[which(normdf_tempmax$pred_name == "temp_max")])/
  normdf_tempmax$sd[which(normdf_tempmax$pred_name == "temp_max")]

prior <- weath |> select(abbrev_state:code_muni, plot_date, temp_max_normalized_mean_subsequent_30) |> 
  rename(temp_max_normalized_mean_prior_30 = temp_max_normalized_mean_subsequent_30) |> 
  mutate(plot_date = plot_date %m+% months(1))

temp_max_normalized_mean <- full_join(weath, prior)

rm(weath, prior, w)

### monthly tempmean mean norming -----
w <- weather_types[2] ;weath <- read.csv(paste0("consolidated_weather/monthly_", w, ".csv")) |> select(abbrev_state:mean_temp_mean, plot_date); weath$plot_date <- as.Date(weath$plot_date)

weath$temp_mean_normalized_mean_subsequent_30 <- 
  (weath$mean_temp_mean  - 
     normdf_tempmean$mean[which(normdf_tempmean$pred_name == "temp_mean")])/
  normdf_tempmean$sd[which(normdf_tempmean$pred_name == "temp_mean")]

prior <- weath |> select(abbrev_state:code_muni, plot_date, temp_mean_normalized_mean_subsequent_30) |> 
  rename(temp_mean_normalized_mean_prior_30 = temp_mean_normalized_mean_subsequent_30) |> 
  mutate(plot_date = plot_date %m+% months(1))

temp_mean_normalized_mean <- full_join(weath, prior)

rm(weath, prior, w)

### monthly tempmin mean nomring -----
w <- weather_types[3] ;weath <- read.csv(paste0("consolidated_weather/monthly_", w, ".csv")) |> select(abbrev_state:mean_temp_min, plot_date); weath$plot_date <- as.Date(weath$plot_date)

weath$temp_min_normalized_mean_subsequent_30 <- 
  (weath$mean_temp_min  - 
     normdf_tempmin$mean[which(normdf_tempmin$pred_name == "temp_min")])/
  normdf_tempmin$sd[which(normdf_tempmin$pred_name == "temp_min")]

prior <- weath |> select(abbrev_state:code_muni, plot_date, temp_min_normalized_mean_subsequent_30) |> 
  rename(temp_min_normalized_mean_prior_30 = temp_min_normalized_mean_subsequent_30) |> 
  mutate(plot_date = plot_date %m+% months(1))

temp_min_normalized_mean <- full_join(weath, prior)

rm(weath, prior, w)

### monthly precip mean nomring -----
w <- weather_types[4] ;weath <- read.csv(paste0("consolidated_weather/monthly_", w, ".csv")) |> select(abbrev_state:mean_rain_mm, plot_date); weath$plot_date <- as.Date(weath$plot_date)

weath$precip_normalized_mean_subsequent_30 <-
  (weath$mean_rain_mm  -
     normdf_precip$mean[which(normdf_precip$pred_name == "precip")])/
  normdf_precip$sd[normdf_precip$pred_name  == "precip"]

prior <- weath |> select(abbrev_state:code_muni, plot_date, precip_normalized_mean_subsequent_30) |>
  rename(precip_normalized_mean_prior_30 = precip_normalized_mean_subsequent_30) |>
  mutate(plot_date = plot_date %m+% months(1))

precip_normalized_mean <- full_join(weath, prior)

rm(weath, prior, w)

### monthly tempmax mean norming-----
w <- weather_types[5] ;weath <- read.csv(paste0("consolidated_weather/monthly_", w, ".csv")) |> select(abbrev_state:mean_vapor, plot_date); weath$plot_date <- as.Date(weath$plot_date)

weath$vapor_normalized_mean_subsequent_30   <-
  (weath$mean_vapor  - normdf_vapor$mean[which(normdf_vapor$pred_name == "vapor")])/
  normdf_vapor$sd[normdf_vapor$pred_name == "vapor"]

prior <- weath |> select(abbrev_state:code_muni, plot_date, vapor_normalized_mean_subsequent_30) |>
  rename(vapor_normalized_mean_prior_30  = vapor_normalized_mean_subsequent_30) |>
  mutate(plot_date = plot_date %m+% months(1))

vapor_normalized_mean <- full_join(weath, prior) |>  subset(plot_date %in% precip_normalized_mean$plot_date)#vapor pressure was available for longer period, removing additional 

rm(weath, prior, w)

## make and write out overall monthly normalized weather -----
monthly_weath_normalized = temp_max_normalized_mean |>  
  full_join(temp_min_normalized_mean) |> 
  full_join(temp_mean_normalized_mean) |> 
  full_join(precip_normalized_mean) |> 
  full_join(vapor_normalized_mean)
  
# dim(monthly_weath_normalized)
write.csv(monthly_weath_normalized, paste0(analysis_dir, "normalized",date_normed,"_monthly_weather_for_prediction.csv"), row.names = FALSE)

#crop selection, tropical/temperate crops-----
landscape_dir = "data/extracted_landscape/"
crops <- read.csv(paste0(landscape_dir,"crops.csv"))

crop_norms <- normalizingFunction(crops, keep_cols= c("code_muni","name_muni","abbrev_state","name_region"))

write.csv(crop_norms$normed_data, paste0(landscape_dir,"cropland_normalized_", today(),".csv"))
write.csv(crop_norms$norming_df, paste0(landscape_dir,"cropland_normingDF_", today(),".csv"))

#df of additional municipal/landscape features ------

### population count/density ------
muni_den <- read.csv(paste0(landscape_dir,"pop_totals_density.csv"))
muni_den$log10_mean_den <- log10(muni_den$mean_density)
muni_den$log10_dwmd <- log10(muni_den$density_weighted_mean_density)
muni_den$log10_pop <- log10(muni_den$total_population)

normed_pop_info <- normalizingFunction(muni_den |> select(code_muni:name_region, urban_type2010, log10_mean_den:log10_pop), keep_cols = c("code_muni","name_muni","code_state","abbrev_state" ,"name_state","code_region","name_region","urban_type2010"), return_norming_df = FALSE)

rm(muni_den)

normed_muni_df <- normed_pop_info; rm(normed_pop_info)

### annual climate -----
clim <- read.csv(paste0(landscape_dir,"climate_annual.csv"))
 names(clim)[5:20] <- paste0("annual_",names(clim)[5:20])
 
normed_annual_clim <- normalizingFunction(clim, keep_cols = c("code_muni","name_muni","abbrev_state" ,"name_region"), return_norming_df = FALSE)
rm(clim)

normed_muni_df <- full_join(normed_muni_df,normed_annual_clim)

### landcover and footprint----
landcover = read.csv(paste0(landscape_dir,"landcover.csv"))
landcover <- landcover %>% select(1:4, ends_with("_mean"),-c(lc_snow_mean,lc_moss_mean)) %>% select(1:4, starts_with("lc_")) %>% mutate(across(lc_bare_mean:lc_wetland_mean, \(x) round(x, 4)))

normed_landcover <- normalizingFunction(landcover, keep_cols = c("code_muni","name_muni","abbrev_state" ,"name_region"), return_norming_df = FALSE)
rm(landcover)

normed_muni_df <- full_join(normed_muni_df,normed_landcover)

foot = read.csv(paste0(landscape_dir,"footprint.csv"))
normed_foot <- normalizingFunction(foot, keep_cols = c("code_muni","name_muni","abbrev_state" ,"name_region"), return_norming_df = FALSE)
rm(foot)

normed_muni_df <- full_join(normed_muni_df,normed_foot)

##soil ----
soil = read.csv(paste0(landscape_dir,"soil.csv"))
soil <- soil %>% select(2:4, ends_with("_mean"))

normed_soil <- normalizingFunction(soil, keep_cols = c("code_muni","name_muni","abbrev_state" ), return_norming_df = FALSE) 
rm(soil)

normed_muni_df <- full_join(normed_muni_df,normed_soil)


#write out normed municipality info df------
write.csv(normed_muni_df, paste0(landscape_dir,"normed_municipal_info_compiled.csv"), row.names = FALSE)
write.csv(normed_muni_df, paste0(analysis_dir,"normed_municipal_info_compiled.csv"), row.names = FALSE)


# bottom-----