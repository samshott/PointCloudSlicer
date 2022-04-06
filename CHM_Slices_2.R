#set your working directory.  you will have to select a file within the working directory, 
#but it will
#remove the last file from the path and save it to the innermost folder
#my solution was to just create a blank text file in the desired directory
setwd(dirname(file.choose()))

library(lidR)
library(rlas)
library(sf)
library(raster)
library(dplyr)

#choose the las file you want to slice.  not setup to work with lists yet.
#if we cant get it to cover the entire dataset in ram I can modify this to work with lidar
#indecies in a list
LAS_file <- file.choose()


#Read in LAS_file as point cloud, 

LAS_PntCloud_NormHt_W_m <-  readLAS(LAS_file) 

#Pipe all functions to reduce ram usage
#Normalize the height using ground points and k nearest neighbor inverse distance weighting
# -- if ground points aren't available use grid_terrain() function
#Plot to check CHM if you like, careful - if your ram is already maxed out this can 
#take it over the edge - maybe better to plot outside of pipe

LAS_PntCloud_NormHt_W_m <- LAS_PntCloud_NormHt_W_m %>% 
  normalize_height(algorithm = knnidw())# %>%
  #plot()
  
#Add an attribute for z value in meters - z_met, for use in meter slicing later
  
LAS_PntCloud_NormHt_W_m <-  add_lasattribute(LAS_PntCloud_NormHt_W_m,
                                             x = (LAS_PntCloud_NormHt_W_m@data$Z*.3048),
                                             name = "z_met",
                                             desc = "Height above groudn in m")

#check that z and z_met values make sense 
mean(LAS_PntCloud_NormHt_W_m@data$Z)
mean(LAS_PntCloud_NormHt_W_m@data$z_met)

#preliminarily filter out unneccesary points to reduce ram load
LAS_PntCloud_NormHt_W_m <- LAS_PntCloud_NormHt_W_m %>% 
  filter_poi(LAS_PntCloud_NormHt_W_m@data$z_met >= .2 & 
               LAS_PntCloud_NormHt_W_m@data$z_met < 16, 
             LAS_PntCloud_NormHt_W_m@data$Classification != 2)

#function for creating rasters out of 
ClassifyByHeight_Raster <- function(HeightModelLas, scope = c(0:10), filePrefixStr, resolution = .5, replace = FALSE){
  #' note this will not work if filePrefixStr is already present in the folder 
  #' (ie. you have already run this and didn't change the filePrefixStr), delete or rename as necessary
  #'   HeightModelLas - LAS point cloud with z_met attribute
  #'   scope - a vector, start and end point for slices (in meters)
  #'   filePrefixStr - the prefix for the .tif to be written for each slice. see above note.
  #'   resolution - resolution of output rasters in feet
  
  crs_string <- paste("+init=epsg:6424")
  print(crs_string)
  
  for(base in scope){
    #create a prefix for each individual file to be written
    fileName <- paste(filePrefixStr, as.character(base), as.character(base+1), "m",".tif", sep = "_")
    
    lidar_slice <- filter_poi(HeightModelLas , HeightModelLas@data$z_met >= base & HeightModelLas@data$z_met < base + 1)
    
    raster_slice <- rasterize_canopy(lidar_slice, res = resolution )
   
    crs(raster_slice) <- crs_string
    
    writeRaster(raster_slice, fileName, overwrite = replace)
    
    print(paste(fileName, " written to WD"))
  }
  
}
  
  