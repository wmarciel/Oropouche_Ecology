
extractMeanWeathertoCSV <- function(raw.dir, output.dir, input.type, shape_data, date_range,pop_weights = "nope"){
  st.whole <- Sys.time()
  
  output.name = "unset"
  if(length(grep("Relative-Humidity-2m-",input.type)) ==  1) output.name = paste0("RH_at_", strsplit(input.type,"-")[[1]][4])
  if(input.type == "Temperature-Air-2m-Max-24h") output.name = "temp_max"
  if(input.type == "Temperature-Air-2m-Min-24h") output.name = "temp_min"
  if(input.type == "Temperature-Air-2m-Mean-24h") output.name = "temp_mean"
  if(input.type == "Precipitation-Flux") output.name = "precip"
  if(input.type == "SOILMOISTURE-L3S-SSMS-ACTIVE-DAILY") output.name = "soilmoist"
  if(input.type == "SOILMOISTURE-L3S-SSMS-ACTIVE-DEKADAL") output.name = "soilmoist10day"
  if(input.type == "SOILMOISTURE-L3S-SSMS-ACTIVE-MONTHLY") output.name = "soilmoistmonth"
  
  if(input.type == "SOILMOISTURE-L3S-SSMV-COMBINED-DAILY") output.name = "volsoilmoist"
  if(input.type == "SOILMOISTURE-L3S-SSMV-COMBINED-DEKADAL") output.name = "volsoilmoist10day"
  if(input.type == "SOILMOISTURE-L3S-SSMV-COMBINED-MONTHLY") output.name = "volsoilmoistmonth"
  
  if(input.type == "Dew-Point-Temperature-2m-Mean") output.name = "dewpoint"
  if(input.type == "Vapour-Pressure-Mean") output.name = "vapor"
  
  
  if(output.name == "unset") stop("There's a problem with input type!")
  
  weather <- st_drop_geometry(shape_data);
  # names(weather) <- c("name_muni","code_muni","abbrev_state")
  weather[,output.name]<- NA
  
  weather.files <- grep(input.type, list.files(raw.dir), value = TRUE)
  
  for(d in as.Date(date_range)){
    d = as.Date(d)
    today <- weather
    today$date <- d
    
    filename <- grep(format(d, "%Y%m%d"), weather.files, value = TRUE); if(length(filename) == 0) warning("Something is wrong with date or input type- there's more or less than one matching file!")
    if(length(filename) > 1){ filename = filename[1]; print(paste("More than one file for", output.name, as.Date(d)))}
    weather_rast <- rast(paste0(raw.dir,"/", filename))
    # weather_rast <- raster(paste0(raw.dir,"/", filename))
    
    if(input.type == "SOILMOISTURE-L3S-SSMS-ACTIVE-MONTHLY") weather_rast <- weather_rast$sm
    
    crs(weather_rast) <- "EPSG:4326" #the supplied one, WGS84, is depricated I guess? equivalentwith EPSG:4326
    #EPSG:4269 equivalent to GRS80
    today[,output.name]<- suppressWarnings(exact_extract(weather_rast, 
                                                         shape_data$geom,
                                                         fun='mean',
                                                         progress=FALSE)) 

    
    if(class(pop_weights) == "RasterLayer" | class(pop_weights) == "SpatRaster"){
      today[,paste0("pop_weighted_",output.name)]<- suppressWarnings(exact_extract(weather_rast, 
                                                                                   shape_data$geom,
                                                                                   fun='weighted_mean',
                                                                                   weights = pop_weights,
                                                                                   progress=FALSE))
    }
    
    
    if(grepl("temp", output.name) | output.name == "dewpoint") {
      today[,output.name] <- today[,output.name]- 273.15 #temperature is in kelvin
      today[,paste0(output.name,"_F")] <- today[,output.name]*(9/5) + 32
      
      if(class(pop_weights) == "RasterLayer"| class(pop_weights) == "SpatRaster"){
        today[,paste0("pop_weighted_",output.name)] <- today[,paste0("pop_weighted_",output.name)]- 273.15 #temperature is in kelvin
      }
      
    }
     rm(weather_rast)
    
    write.csv(today, 
              paste0(output.dir,"/",output.name,"_",format(d, "%Y%m%d"), ".csv"), row.names = FALSE)
    
  }
  end.whole <- Sys.time()
  
  print(paste0(output.name, 
               as.Date(min(date_range)), " to ", as.Date(max(date_range)),
               " completed in"))
  print(end.whole-st.whole)
  
}

extractDewpointtoCSV <- function(output.dir = "E:/extracted_weather/dewpoint", y, year.raster, shape_data,pop_weights = "nope",output.name="dewpoint"){
  st.whole <- Sys.time()
  
  weather <- as.data.frame(cbind(shape_data$name_muni, shape_data$code_muni, shape_data$abbrev_state));
  names(weather) <- c("name_muni","code_muni","abbrev_state")
  
  for(d in as.numeric(str_split_i(names(year.raster),"time=",2))){
    today <- weather
    today$date <- as.Date(d, origin = paste0(y,"-01-01"))
    today$year <- y
    
    
    weather_rast <- rotate(year.raster[[paste0("d2m_valid_time=",d)]])
    
    crs(weather_rast) <- "EPSG:4326" #the supplied one, WGS84, is depricated I guess? equivalentwith EPSG:4326
    #EPSG:4269 equivalent to GRS80
    # weather_rast <- terra::project(year.raster[paste0("d2m_valid_time=",d)], "+proj=longlat +datum=WGS84 +no_defs", "epsg:4326")
    
    today[,output.name]<- suppressWarnings(exact_extract(weather_rast, 
                                                         shape_data$geom,
                                                         fun='mean',
                                                         progress=FALSE)) #puts out a warning about the projection, but it seems like the projection is just fine??? https://epsg.io/4674 update it's that it's not attributed correctly, it's the same as EPSG:4326
    
    
    
    if(class(pop_weights) == "RasterLayer" | class(pop_weights) == "SpatRaster"){
      today[,paste0("pop_weighted_",output.name)]<- suppressWarnings(exact_extract(weather_rast, 
                                                                                   shape_data$geom,
                                                                                   fun='weighted_mean',
                                                                                   weights = pop_weights,
                                                                                   default_weight = 0,
                                                                                   progress=FALSE))
    }
    
    
    if(grepl("temp", output.name) | output.name == "dewpoint") {
      today[,output.name] <- today[,output.name]- 273.15 #temperature is in kelvin
      today[,paste0(output.name,"_F")] <- today[,output.name]*(9/5) + 32
      
      if(class(pop_weights) == "RasterLayer"| class(pop_weights) == "SpatRaster"){
        today[,paste0("pop_weighted_",output.name)] <- today[,paste0("pop_weighted_",output.name)]- 273.15 #temperature is in kelvin
      }
      
    }
    rm(weather_rast)
    
    write.csv(today, 
              paste0(output.dir,"/",output.name,"_",format(as.Date(d, origin = paste0(y,"-01-01")), "%Y%m%d"), ".csv"), row.names = FALSE)
    
  }
  end.whole <- Sys.time()
  
  print(paste0(output.name, 
               y,
               " completed in"))
  print(end.whole-st.whole)
  
}


