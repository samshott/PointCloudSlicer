#read in all necessary libraries (and some maybe unnecessary)
library(lidR)
library(rlas)
library(rgdal)
library(sf)
library(stringr)
library(mapview)

#choose the all of the LiDAR files to be analyzed 
#check the .las catalog for validity
#normalization and indexation will not check because catalog files do not get a deep inspection
lasCat <- readLAScatalog(choose.files())
las_check(lasCat)


#change the chunk size. larger numbers = bigger chunks read into memory
# probably set around 500-1000, but use the plot below to check how many sections 
#  it is being cut into
chunkSize <- 200
opt_chunk_size(lasCat) <- chunkSize
plot(lasCat, chunk = TRUE)


#just two different ways to plot bounding box over a map
#If these don't line up check that the epsg is set to 6264,  I had a couple sets that 
#were incorrectly set to 6263
mapView(st_bbox(lasCat), native.crs = TRUE, map.type = "OpenStreetMap.Mapnik")
plot(lasCat, mapview=TRUE, map.type = "Esri.WorldImagery", chunk = TRUE)

#choose a directory for the normalized output
directory <- choose.dir()
dir.create(paste0(directory, "\\tmp_slicer"))
opt_output_files(lasCat) <- (paste0(directory, "\\tmp_slicer\\Temp_Normalized_{ID}"))

directoryDel <- (paste0(directory, "\\tmp_slicer"))

lasCat@output_options$merge <- TRUE

#normalize the height of the lasCat
lasCat_Normalized <- normalize_height(lasCat, algorithm = tin())

#sanity check
mapView(st_bbox(lasCat_Normalized), native.crs = FALSE, map.type = "OpenStreetMap.Mapnik")

#check that z values are close to 0
plot(lasCat_Normalized["Min.Z"])

#filter out unneeded values
#this will occur the NEXT time the dataset is run through a process
#consider them instructions for future processing
opt_filter(lasCat_Normalized) <- "-drop_z_below .1 -drop_z_above 15 -drop_class 2"

opt_output_files(lasCat_Normalized) <- (paste0(directoryDel, "\\Temp_Normalized_{ID}"))
lasCat_Normalized@output_options$merge <- TRUE

#make a CHM - note: this will be writen in temp folder as chunks
#until the writeRaster function is explicitly called
raster_norm <- rasterize_canopy(lasCat_Normalized, res = 1)

#check the raster in a plot to confirm desired output
plot(raster_norm)

#writes the final combined raster to the desired output folder
terra::writeRaster(raster_norm, paste0(directory,"\\LasNorm_0-15ft_Raster.tif"))

#delete intermediate data
unlink(directoryDel, recursive = TRUE)

