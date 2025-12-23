library(raster)
library(geobr)
library(exactextractr)
library(ncdf4)
library(terra)
library(sf)
library(beepr); options(error = function() {beep(7)})
library(readr)

#setup ----
setwd("Z:/laura/naturehealth")
source("code/functionsWeatherExtraction.R")

raw_dir = "data/weather_rasters"
output_dir = "data/extracted_weather"
weather_types = c(
  "Temperature-Air-2m-Max-24h",
  "Temperature-Air-2m-Mean-24h",
  "Temperature-Air-2m-Min-24h",
  "Precipitation-Flux",
  "Vapour-Pressure-Mean"
)
#Get geographic info ------
bz <- read_country()
shape_munis <- read_municipality(year=2020);shape_munis <- st_transform(shape_munis, crs = 4326) 

#Extract weather----
for (i in weather_types){if (file.exists(paste(output_dir,i, sep ="/")) == FALSE) dir.create(file.path(output_dir, i))}; rm(i) # Verify/set up directories for extracted files to be sent to

date_range <- as.Date("2021-06-01"):as.Date("2025-08-31")
date_range <- date_range[1:10]
for (i in 1:length(weather_types)){
  print(weather_types[i])
  try(
  extractMeanWeathertoCSV(raw_dir, paste(output_dir,weather_types[i], sep = "/"),
                          input.type = weather_types[i],
                          shape_data = shape_munis,
                          date_range = date_range,
                          pop_weights = "nope")
  )
  beepr::beep(1)
};beepr::beep(4)


#Consolidate weather ------
cases <- read.csv("data/additional_data_files/linelist_2025-09-23_positive.csv")
pop <- read.csv("data/additional_data_files/population_municipalities_2022.csv")

munis_with_cases <- unique(cases$six_digit_id)
pop$has_cases <- pop$six_digit_id %in% munis_with_cases
muni_code_with_cases <- pop$code_muni[which(pop$has_cases)]
rm(cases, munis_with_cases, pop)

read_temp_csvs <- function(filename){
  read_csv(filename,
           col_types = c("c","c","c","n","D","n","n")
  )
}

read_nontemp_csvs <- function(filename){
  read_csv(filename,
           col_types = c("c","c","c","n","D","n")
  )
}

for (i in 1:length(weather_types)){
  tic = Sys.time()
  files <- list.files(paste(output_dir, weather_types[i], sep = "/"))
  if(length(grep("Temperature", weather_types[i])) == 1) consol <- purrr::map_dfr(paste(output_dir,weather_types[i], files, sep = "/"), read_temp_csvs)
  
  if(length(grep("Temperature", weather_types[i])) == 0) consol <- purrr::map_dfr(paste(output_dir,weather_types[i], files, sep = "/"), read_nontemp_csvs)
  
  write.csv(consol,
            paste0(output_dir, "/", weather_types[i],"_consolidated.csv"), row.names = FALSE)
  write.csv(subset(consol, code_muni %in% muni_code_with_cases),
            paste0(output_dir, "/", weather_types[i],"_withcases_consolidated.csv"), row.names = FALSE)
  rm(consol)
  toc = Sys.time()
  print(weather_types[i]); print(toc-tic)
  try(beep())
}; try(beep(4))



#get weekly, monthly averages -----
write_dir = output_dir

## temp max (i = 1)------
i = 1
tic = Sys.time()
files <- list.files(paste(output_dir, weather_types[i], sep = "/"))
if(length(grep("Temperature", weather_types[i])) == 1) consol <- purrr::map_dfr(paste(output_dir,weather_types[i], files, sep = "/"), read_temp_csvs)

if(length(grep("Temperature", weather_types[i])) == 0) consol <- purrr::map_dfr(paste(output_dir,weather_types[i], files, sep = "/"), read_nontemp_csvs)

consol$year <- year(consol$date)
consol$month <- month(consol$date)
consol$isoweek <- ISOweek(consol$date)

consol %>% group_by(abbrev_state, code_muni, year, month) %>% 
  summarise(mean_temp_max = mean(temp_max),
            median_temp_max = median(temp_max),
            min_temp_max = min(temp_max),
            max_temp_max = max(temp_max),
            mean_pop_weighted_temp_max = mean(pop_weighted_temp_max),
            median_pop_weighted_temp_max = median(pop_weighted_temp_max),
            min_pop_weighted_temp_max = min(pop_weighted_temp_max),
            max_pop_weighted_temp_max = max(pop_weighted_temp_max),
            mean_temp_max_F = mean(temp_max_F),
            median_temp_max_F = median(temp_max_F),
            min_temp_max_F = min(temp_max_F),
            max_temp_max_F = max(temp_max_F)
  ) -> monthly
monthly$plot_date <- paste(monthly$year, str_pad(monthly$month,2,"left",pad = "0"), "15", sep = "-")

write_csv(monthly,
          paste0(write_dir, "/monthly_", weather_types[i],".csv"))
rm(monthly)

consol %>% group_by(abbrev_state, code_muni, isoweek) %>% 
  summarise(mean_temp_max = mean(temp_max),
            median_temp_max = median(temp_max),
            min_temp_max = min(temp_max),
            max_temp_max = max(temp_max),
            mean_pop_weighted_temp_max = mean(pop_weighted_temp_max),
            median_pop_weighted_temp_max = median(pop_weighted_temp_max),
            min_pop_weighted_temp_max = min(pop_weighted_temp_max),
            max_pop_weighted_temp_max = max(pop_weighted_temp_max),
            mean_temp_max_F = mean(temp_max_F),
            median_temp_max_F = median(temp_max_F),
            min_temp_max_F = min(temp_max_F),
            max_temp_max_F = max(temp_max_F)
  ) -> weekly

weekly$plot_date <- ISOweek2date(paste0(weekly$isoweek[1],"-1"))

write_csv(weekly,
          paste0(write_dir, "/weekly_", weather_types[i],".csv"))
rm(weekly)

rm(consol)
toc = Sys.time()
print(weather_types[i]); print(toc-tic)

## temp mean (i = 2)------
i = 2
tic = Sys.time()
files <- list.files(paste(output_dir, weather_types[i], sep = "/"))
if(length(grep("Temperature", weather_types[i])) == 1) consol <- purrr::map_dfr(paste(output_dir,weather_types[i], files, sep = "/"), read_temp_csvs)

if(length(grep("Temperature", weather_types[i])) == 0) consol <- purrr::map_dfr(paste(output_dir,weather_types[i], files, sep = "/"), read_nontemp_csvs)
beep()
consol$year <- year(consol$date)
consol$month <- month(consol$date)
consol$isoweek <- ISOweek(consol$date)

consol %>% group_by(abbrev_state, code_muni, year, month) %>% 
  summarise(mean_temp_mean = mean(temp_mean),
            median_temp_mean = median(temp_mean),
            min_temp_mean = min(temp_mean),
            max_temp_mean = max(temp_mean),
            mean_pop_weighted_temp_mean = mean(pop_weighted_temp_mean),
            median_pop_weighted_temp_mean = median(pop_weighted_temp_mean),
            min_pop_weighted_temp_mean = min(pop_weighted_temp_mean),
            max_pop_weighted_temp_mean = max(pop_weighted_temp_mean),
            mean_temp_mean_F = mean(temp_mean_F),
            median_temp_mean_F = median(temp_mean_F),
            min_temp_mean_F = min(temp_mean_F),
            max_temp_mean_F = max(temp_mean_F)
  ) -> monthly
monthly$plot_date <- paste(monthly$year, str_pad(monthly$month,2,"left",pad = "0"), "15", sep = "-")

write_csv(monthly,
          paste0(write_dir, "/monthly_", weather_types[i],".csv"))
rm(monthly)

consol %>% group_by(abbrev_state, code_muni, isoweek) %>% 
  summarise(mean_temp_mean = mean(temp_mean),
            median_temp_mean = median(temp_mean),
            min_temp_mean = min(temp_mean),
            max_temp_mean = max(temp_mean),
            mean_pop_weighted_temp_mean = mean(pop_weighted_temp_mean),
            median_pop_weighted_temp_mean = median(pop_weighted_temp_mean),
            min_pop_weighted_temp_mean = min(pop_weighted_temp_mean),
            max_pop_weighted_temp_mean = max(pop_weighted_temp_mean),
            mean_temp_mean_F = mean(temp_mean_F),
            median_temp_mean_F = median(temp_mean_F),
            min_temp_mean_F = min(temp_mean_F),
            max_temp_mean_F = max(temp_mean_F)
  ) -> weekly

weekly$plot_date <- ISOweek2date(paste0(weekly$isoweek[1],"-1"))

write_csv(weekly,
          paste0(write_dir, "/weekly_", weather_types[i],".csv"))
rm(weekly)

rm(consol)
toc = Sys.time()
print(weather_types[i]); print(toc-tic); beep()

## temp min (i = 3)------
i = 3
tic = Sys.time()
files <- list.files(paste(output_dir, weather_types[i], sep = "/"))
if(length(grep("Temperature", weather_types[i])) == 1) consol <- purrr::map_dfr(paste(output_dir,weather_types[i], files, sep = "/"), read_temp_csvs)

if(length(grep("Temperature", weather_types[i])) == 0) consol <- purrr::map_dfr(paste(output_dir,weather_types[i], files, sep = "/"), read_nontemp_csvs)
beep()
consol$year <- year(consol$date)
consol$month <- month(consol$date)
consol$isoweek <- ISOweek(consol$date)

consol %>% group_by(abbrev_state, code_muni, year, month) %>% 
  summarise(mean_temp_min = mean(temp_min),
            median_temp_min = median(temp_min),
            min_temp_min = min(temp_min),
            max_temp_min = max(temp_min),
            mean_pop_weighted_temp_min = mean(pop_weighted_temp_min),
            median_pop_weighted_temp_min = median(pop_weighted_temp_min),
            min_pop_weighted_temp_min = min(pop_weighted_temp_min),
            max_pop_weighted_temp_min = max(pop_weighted_temp_min),
            mean_temp_min_F = mean(temp_min_F),
            median_temp_min_F = median(temp_min_F),
            min_temp_min_F = min(temp_min_F),
            max_temp_min_F = max(temp_min_F)
  ) -> monthly
monthly$plot_date <- paste(monthly$year, str_pad(monthly$month,2,"left",pad = "0"), "15", sep = "-")

write_csv(monthly,
          paste0(write_dir, "/monthly_", weather_types[i],".csv"))
rm(monthly)

consol %>% group_by(abbrev_state, code_muni, isoweek) %>% 
  summarise(mean_temp_min = mean(temp_min),
            median_temp_min = median(temp_min),
            min_temp_min = min(temp_min),
            max_temp_min = max(temp_min),
            mean_pop_weighted_temp_min = mean(pop_weighted_temp_min),
            median_pop_weighted_temp_min = median(pop_weighted_temp_min),
            min_pop_weighted_temp_min = min(pop_weighted_temp_min),
            max_pop_weighted_temp_min = max(pop_weighted_temp_min),
            mean_temp_min_F = mean(temp_min_F),
            median_temp_min_F = median(temp_min_F),
            min_temp_min_F = min(temp_min_F),
            max_temp_min_F = max(temp_min_F)
  ) -> weekly

weekly$plot_date <- ISOweek2date(paste0(weekly$isoweek[1],"-1"))

write_csv(weekly,
          paste0(write_dir, "/weekly_", weather_types[i],".csv"))
rm(weekly)

rm(consol)
toc = Sys.time()
print(weather_types[i]); print(toc-tic); beep()


## precip (i = 4)------
i = 4
tic = Sys.time()
files <- list.files(paste(output_dir, weather_types[i], sep = "/"))
if(length(grep("Temperature", weather_types[i])) == 1) consol <- purrr::map_dfr(paste(output_dir,weather_types[i], files, sep = "/"), read_temp_csvs)

if(length(grep("Temperature", weather_types[i])) == 0) consol <- purrr::map_dfr(paste(output_dir,weather_types[i], files, sep = "/"), read_nontemp_csvs)

consol$year <- year(consol$date)
consol$month <- month(consol$date)
consol$isoweek <- ISOweek(consol$date)
beep()

consol %>% group_by(abbrev_state, code_muni, year, month) %>% 
  summarise(mean_rain_mm = mean(precip),
            median_rain_mm = median(precip),
            min_rain_mm = min(precip),
            max_rain_mm = max(precip),
            sum_rain_mm = sum(precip),
            mean_pop_weighted_rain_mm = mean(pop_weighted_precip),
            median_pop_weighted_rain_mm = median(pop_weighted_precip),
            min_pop_weighted_rain_mm = min(pop_weighted_precip),
            max_pop_weighted_rain_mm = max(pop_weighted_precip)
  ) -> monthly
monthly$plot_date <- paste(monthly$year, str_pad(monthly$month,2,"left",pad = "0"), "15", sep = "-")

write_csv(monthly,
          paste0(write_dir, "/monthly_", weather_types[i],".csv"))
rm(monthly)

consol %>% group_by(abbrev_state, code_muni, isoweek) %>% 
  summarise(mean_rain_mm = mean(precip),
            median_rain_mm = median(precip),
            min_rain_mm = min(precip),
            max_rain_mm = max(precip),
            sum_rain_mm = sum(precip),
            mean_pop_weighted_rain_mm = mean(pop_weighted_precip),
            median_pop_weighted_rain_mm = median(pop_weighted_precip),
            min_pop_weighted_rain_mm = min(pop_weighted_precip),
            max_pop_weighted_rain_mm = max(pop_weighted_precip)
  ) -> weekly

weekly$plot_date <- ISOweek2date(paste0(weekly$isoweek[1],"-1"))

write_csv(weekly,
          paste0(write_dir, "/weekly_", weather_types[i],".csv"))
rm(weekly)

rm(consol)
toc = Sys.time()
print(weather_types[i]); print(toc-tic); beep()

##vapor pressure (i = 5)----
fil_dir <- "Vapour-Pressure-Mean"
weather_type = "vapor"

for(dp in weather_type){
  tic = Sys.time()
  files <- list.files(paste(output_dir,fil_dir, sep = "/"))
  consol <- purrr::map_dfr(paste(output_dir,fil_dir, files, sep = "/"), read_temp_csvs)
  
  consol$year <- year(consol$date)
  consol$month <- month(consol$date)
  consol$isoweek <- ISOweek(consol$date)
  
  write_csv(consol,
            paste0(output_dir, "/", fil_dir,"_consolidated.csv"))
  write_csv(subset(consol, code_muni %in% muni_code_with_cases),
            paste0(output_dir, "/", fil_dir,"_withcases_consolidated.csv"))
  write_csv(subset(consol, code_muni %in% muni_code_with_cases),
            paste0(write_dir, "/", fil_dir,"_withcases_consolidated.csv"))
  
  beep()
  consol %>% group_by(abbrev_state, code_muni, year, month) %>% 
    summarise("mean_{dp}" := mean(!!sym(dp)),
              "median_{dp}" := median(!!sym(dp)),
              "min_{dp}" := min(!!sym(dp)),
              "max_{dp}" := max(!!sym(dp))#,
              # "mean_pop_weighted_{dp}" := mean(!!sym(paste0("pop_weighted_",dp))),
              # "median_pop_weighted_{dp}" := median(!!sym(paste0("pop_weighted_",dp))),
              # "min_pop_weighted_{dp}" := min(!!sym(paste0("pop_weighted_",dp))),
              # "max_pop_weighted_{dp}" := max(!!sym(paste0("pop_weighted_",dp)))
    ) -> monthly
  monthly$plot_date <- paste(monthly$year, str_pad(monthly$month,2,"left",pad = "0"), "15", sep = "-")
  write_csv(monthly,
            paste0(write_dir, "/monthly_", fil_dir,".csv"))
  rm(monthly)
  
  consol %>% group_by(abbrev_state, code_muni, isoweek) %>% 
    summarise("mean_{dp}" := mean(!!sym(dp)),
              "median_{dp}" := median(!!sym(dp)),
              "min_{dp}" := min(!!sym(dp)),
              "max_{dp}" := max(!!sym(dp))#,
              # "mean_pop_weighted_{dp}" := mean(!!sym(paste0("pop_weighted_",dp))),
              # "median_pop_weighted_{dp}" := median(!!sym(paste0("pop_weighted_",dp))),
              # "min_pop_weighted_{dp}" := min(!!sym(paste0("pop_weighted_",dp))),
              # "max_pop_weighted_{dp}" := max(!!sym(paste0("pop_weighted_",dp)))
    ) -> weekly
  
  weekly$plot_date <- ISOweek2date(paste0(weekly$isoweek[1],"-1"))
  
  write_csv(weekly,
            paste0(write_dir, "/weekly_", fil_dir,".csv"))
  rm(weekly)
  
  rm(consol)
  toc = Sys.time()
  print(fil_dir); print(toc-tic); beep()
};beep(4)
#bottom----