# library(geodata)
library(raster)
library(geobr)
library(exactextractr)
library(terra)
library(tidyverse)
# library(sf)
library(beepr); options(error = function() {beep(7)})

#setup ----
setwd("Z:/laura/naturehealth")
rastdir = "data/landscape_rasters"
writedir = "data/extracted_landscape"

remove_stuff = TRUE

bz <- read_country(year=2020);
# bz_extent <- ext(bz)
shape_munis <- read_municipality(year=2020)
shape_munis <- st_transform(shape_munis, crs = 4326)
muni_df <- st_drop_geometry(shape_munis[, c("code_muni","name_muni","abbrev_state","name_region")])

#Human footprint-----
temp_muni_df <- muni_df
fxns <- c("mean", "median","max")
foot_rast = rast(paste0(rastdir, "/landuse/wildareas-v3-2009-human-footprint_geo.tif"))
for(fun in c(fxns)){
  temp_muni_df[,paste("foot", fun, sep = "_")] <- exact_extract(foot_rast, 
                                                                shape_munis$geom,
                                                                fun= fun,
                                                                progress=TRUE)
}; beep(4)

write.csv(temp_muni_df,
          paste0(writedir, "/footprint.csv"))

if(remove_stuff == TRUE) rm(temp_muni_df,foot_rast)

#Climate -----
climate_dir = paste0(rastdir,"/climate/wc2.1_30s")
temp_muni_df <- muni_df
vars = c("prec","tmin","tmax","tavg","vapr")

clim_list <- list()
for(input.type in vars){
  print(input.type)
  weather.files <- grep(input.type, list.files(climate_dir), value = TRUE)
  weath_df = NULL
  for(m in 1:12){
    month_df = temp_muni_df;month_df$month = m
    mon = str_pad(m,2,pad = 0, "left")
    filename = grep(mon,weather.files,value = TRUE)
    weather_rast <- rast(paste0(climate_dir,"/", filename))
    crs(weather_rast) <- "EPSG:4326" 
    month_df[,input.type]<- exact_extract(weather_rast, 
                                          shape_munis$geom,
                                          fun='mean',
                                          progress=TRUE) 
    weath_df <- rbind(weath_df, month_df)
    print(mon)
    
  }
  clim_list[[input.type]] <- weath_df
  
};beep(4)

clim_df <- left_join(clim_list$prec, clim_list$tmin)
clim_df <- left_join(clim_df, clim_list$tmax)
clim_df <- left_join(clim_df, clim_list$tavg)
clim_df <- left_join(clim_df, clim_list$vapr)


if(remove_stuff == TRUE) rm(weath_df, weather_rast, clim_list)

write.csv(clim_df,
          paste0(writedir, "/climate_monthly.csv"), row.names = FALSE)

clim_df %>% group_by(code_muni,name_muni,abbrev_state,name_region) %>% 
  summarise(
    mean_precip = mean(prec),
    min_precip = min(prec),
    max_precip = max(prec),
    sum_precip = sum(prec),
    
    mean_tempmin = mean(tmin),
    min_tempmin = min(tmin),
    max_tempmin = max(tmin),
    
    mean_tempmean = mean(tavg),
    min_tempmean = min(tavg),
    max_tempmean = max(tavg),
    
    mean_tempmax = mean(tmax),
    min_tempmax = min(tmax),
    max_tempmax = max(tmax),
    
    mean_vapr = mean(vapr),
    min_vapr = min(vapr),
    max_vapr = max(vapr)
  ) -> annual_climate

write.csv(annual_climate,
          paste0(writedir, "/climate_annual.csv"), row.names = FALSE)

#Population information-----
muni_pop <- read.csv("data/additional_data_files/population_municipalities_2022.csv")
pop_rast = rast(paste0(rastdir, "/population/pop/gpw_v4_population_density_rev11_2020_30s.tif"))

temp_muni_df <- muni_df

temp_muni_df$median_density <- exact_extract(pop_rast, 
                                        shape_munis$geom,
                                        fun='median',
                                        progress=TRUE)

temp_muni_df$mean_density <-exact_extract(pop_rast, 
                                     shape_munis$geom,
                                     fun='mean',
                                     progress=TRUE)

temp_muni_df$density_weighted_mean_density <-exact_extract(pop_rast, 
                                                      shape_munis$geom,
                                                      fun='weighted_mean',
                                                      weights = pop_rast,
                                                      default_weight = 0,
                                                      progress=TRUE)
pop_write <- full_join(muni_pop, temp_muni_df)

pop_write$log10_dwmd <- log10(pop_write$density_weighted_mean_density)
pop_write$log10_pop <- log10(pop_write$total_population)

write.csv(pop_write,
          paste0(writedir, "/pop_totals_density.csv", row.names = FALSE)
)
if(remove_stuff == TRUE) rm(pop_write, temp_muni_df, muni_pop)

#Land cover information ----
temp_muni_df <- muni_df

lc_files = grep("WorldCover", list.files(paste0(rastdir,"/landuse")), value = TRUE) 
  
fxns <- c("mean", "median","max")

wt_fxns = c("mean");other_fxns = c("median","max")

for(file in lc_files){
  lc_rast = rast(paste0(rastdir,"/landuse/", file))
  for(fun in c(fxns)){
    temp_muni_df[,paste("lc", strsplit(file, "_")[[1]][2],fun, sep = "_")] <- exact_extract(lc_rast, 
                                                                                       shape_munis$geom,
                                                                                       fun= fun,
                                                                                       progress=TRUE)
  }
  
}; beep(4)

write.csv(temp_muni_df,
          paste0(writedir, "/landcover.csv"), row.names= FALSE)

if(remove_stuff == TRUE) rm(temp_muni_df,lc_files,  file, fxns)


#crop information ------
temp_muni_df <- muni_df

crop_dir = paste0(rastdir,"/crops/CROPGRIDSv1.08_NC_maps")
all_crop_files =  grep("CROPGRIDSv1.08", list.files(crop_dir), value = TRUE)
crops_to_extract = c("banana","cassava","cocoa","coffee","rubber","soybean","sugarcane")
crop_files = grep(paste(crops_to_extract,collapse="|"),all_crop_files, value = TRUE)
  
  
wt_fxns = c("mean"); #other_fxns = c("median","max")

for(file in crop_files){
  crop_name = gsub(".nc","",gsub("CROPGRIDSv1.08_","",file)); print(crop_name)
  crop_rast_all = rast(paste0(crop_dir,"/", file)) #when loaded with raster, crs is WGS84 
  crop_rast = crop_rast_all$croparea
  crs(crop_rast) = "epsg:4326"
  
  for(fun in c("mean")){
    col_name = paste("pct_crop", crop_name,fun, sep = "_")
    temp_muni_df[,col_name] <- exact_extract(
      100*crop_rast/cellSize(crop_rast,unit = "ha"),                                                                                        shape_munis$geom,
      fun= fun,
      progress=TRUE)
    
    temp_muni_df[which(temp_muni_df[,col_name] < 0 & abs((temp_muni_df[,col_name])) < .1),col_name] <- 0 #rounding errors
    temp_muni_df[which(abs(temp_muni_df[,col_name]) < .01),col_name] <- 0#rounding errors
  }
  
}; beep(4); write.csv(temp_muni_df,
                      paste0(writedir, "/crops.csv"), row.names = FALSE)



if(remove_stuff == TRUE) rm(temp_muni_df,crop_files,all_crop_files,  file, fxns)


# soil health ----
soil_dir = paste0(rastdir,"/soil_health")
soil_list <- list()
sums = NULL
sh_metrics = c("LAC_SHI","LAC_fi","LAC_fii","LAC_fiii","LAC_fiv","LAC_fv")
for(input.type in sh_metrics){
  st.whole <- Sys.time()
  
  output.name = "unset"
  if(input.type == "LAC_SHI") output.name = "soil_healthidx"
  if(input.type == "LAC_fi") output.name = "soil_nutriflux"
  if(input.type == "LAC_fii") output.name = "soil_waterreg"
  if(input.type == "LAC_fiii") output.name = "soil_organic"
  if(input.type == "LAC_fiv") output.name = "soil_physsupport"
  if(input.type == "LAC_fv") output.name = "soil_eroresist"
  
  
  soil <- as.data.frame(cbind(shape_munis$name_muni, shape_munis$code_muni, shape_munis$abbrev_state));
  names(soil) <- c("name_muni","code_muni","abbrev_state")
  
  filename <- grep(paste0(input.type,".tif"), list.files(soil_dir), value = TRUE)
  if(length(filename) == 0) warning("Something is wrong with date or input type- there's more or less than one matching file!")
  soil_rast <- rast(paste0(soil_dir,"/", filename))
  # soil_rast <- terra::crop(soil_rast,bz, snap = "out")
  window(soil_rast) <- ext(shape_munis)
  soil_rast[soil_rast < 10] <- NA

  crs(soil_rast) <- "EPSG:4326" #same as what it's in
  extracted <- exact_extract(soil_rast,
                             shape_munis$geom,
                             fun=c(c("mean", "median", "mode","min","max")),
                             max_cells_in_memory = 3e+08,
                             progress=TRUE)
  
  if(ncol(extracted) != 5) error("Unexpected number of columns!")
  soil[,paste(output.name,names(extracted), sep = "_")] <- extracted
  soil_list[[input.type]] <- soil
  rm(soil)
  print(output.name)
  print(Sys.time()-st.whole)
  beep(2)
}; beep(4)

soil_df <- soil_list$LAC_SHI
soil_df <- left_join(soil_list$LAC_SHI, soil_list$LAC_fi)
soil_df <- left_join(soil_df, soil_list$LAC_fii)
soil_df <- left_join(soil_df, soil_list$LAC_fiii)
soil_df <- left_join(soil_df, soil_list$LAC_fiv)
soil_df <- left_join(soil_df, soil_list$LAC_fv)

if(remove_stuff == TRUE) rm(soil_rast, soil_list)

write.csv(soil_df,
          paste0(writedir, "/soil.csv"), row.names = FALSE)


#bottom----