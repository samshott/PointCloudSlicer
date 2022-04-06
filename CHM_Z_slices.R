setwd("~/R/AlthouseAndMeade")
library(lidR)
library(rlas)
library(rgdal)
library(sf)
#LASFile <- "EllwoodSec10.las"
LAS_file <- file.choose()

LAS_PntCloud <- readLAS(LAS_file)

DTM <- grid_terrain(LAS_PntCloud, res = .5, knnidw())

LAS_PntCloud_NormHt <- normalize_height(LAS_PntCloud, DTM, add_lasattribute = TRUE)

#writeLAS(NormHt_EllwoodSub, "EllwoodSec10NormHt.las")

# Create_Norm_Height <- function(LasWground, terrainRes = 1, DTM_algorithm = knnidw()){
#   tryCatch(require("lidR"), "lidR package not installed")
#   
#   DTM <- grid_terrain(LasWground, res = terrainRes, DTM_algorithm)
#   NormHtMod <- normalize_height(LasWground, DTM, add_lasattribute = TRUE)
# }

#filter_poi(EllwoodSubLASnorm , EllwoodSubLASnorm@data$Z >= 0 & EllwoodSubLASnorm@data$Z < 1)

if (min(LAS_PntCloud_NormHt@data$Z) < 0){
  print(c("minumum z value is less than 0: ", as.character( round(min(LAS_PntCloud_NormHt@data$Z), digits = 3))))
  normalization_coef <- min(LAS_PntCloud_NormHt@data$Z)
  
  LAS_PntCloud_NormHt@data$Z <- LAS_PntCloud_NormHt@data$Z - normalization_coef
  print("minumums normalized to 0")
}

LAS_PntCloud_NormHt_W_m <- add_lasattribute(LAS_PntCloud_NormHt, (LAS_PntCloud_NormHt@data$Z*.3048), name = "z_met", desc = "Height above groudn in m")

ClassifyByHeight_LAS <- function(HeightModelLas, scope = c(0:10), classes, filePrefixStr){
  
  for(base in scope){
    
    lidar_slice <- filter_poi(HeightModelLas , HeightModelLas@data$z_met >= base & HeightModelLas@data$z_met < base + 1)
    
    fileName <- paste(filePrefixStr, as.character(base), as.character(base+1), "m",".las", sep = "_")

    writeLAS(lidar_slice, fileName)

  }

}

ClassifyByHeight_Raster <- function(HeightModelLas, scope = c(0:10), classes, filePrefixStr){
  
  for(base in scope){
    fileName <- paste(filePrefixStr, as.character(base), as.character(base+1), "m",".tif", sep = "_")
    
    lidar_slice <- filter_poi(HeightModelLas , HeightModelLas@data$z_met >= base & HeightModelLas@data$z_met < base + 1)
    
    raster_slice <- grid_terrain(lidar_slice, res = .1 , )
    
    writeRaster(raster_slice, fileName, format = writeFormats()[24])
    
  }
  
}

test_lidar_slice <- filter_poi(LAS_PntCloud_NormHt_W_m , LAS_PntCloud_NormHt_W_m@data$Z >= 1 & LAS_PntCloud_NormHt_W_m@data$Z < 15 & LAS_PntCloud_NormHt_W_m@data$Classification != 2 )

#test_lidar_slice@data$Classification <- 2
test_raster_slice <- grid_terrain(test_lidar_slice, res = .1 , knnidw(), is_concave = TRUE)

metrics <- function(x){
  m <- list(
    z_max = max(x)
  )
  return()
}

test_voxel_slice <- voxel_metrics(test_lidar_slice, ~list(N = max(Z)), .2)

test_voxel_raster <- rasterFromXYZ(test_voxel_slice)

wrtieRaster