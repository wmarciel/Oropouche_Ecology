#script to download rasters from geodata
library(geodata)
# library(raster)
# library(geobr)
# library(exactextractr)
# library(terra)

setwd("Z:/laura/naturehealth")

rastdir = "data/landscape_rasters"

#Available via geodata: climate, human footprint, population density, landcover -----
vars = c("prec","tmin","tmax","tavg","vapr")
for(v in vars[1:5]){
  geodata::worldclim_global(var = v , res = .5, path = rastdir, version="2.1")
}
geodata::footprint(year=2009, rastdir)
geodata::population(year=2020,res=.5,path=rastdir)

vars = c( "trees", "grassland", "shrubs", "cropland", "built", "bare", "snow", "water", "wetland", "mangroves", "moss")
for(var in vars){
  geodata::landcover(var,path=rastdir)
}; beep(4)


#Links for not available via geodata: soil health, crops: -----

#soil health article: https://www.nature.com/articles/s43247-025-02021-w#Abs1	
#soil health data download link: https://zenodo.org/records/14285685

#Crops/cropgrids article: https://www.nature.com/articles/s41597-024-03247-7
#crops download link: https://figshare.com/articles/dataset/CROPGRIDS/22491997
