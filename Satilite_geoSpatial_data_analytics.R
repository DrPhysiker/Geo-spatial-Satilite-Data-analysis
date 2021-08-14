# increase mimory used by R

# setup libraries
 
install.packages("devtools", repos="http://cran.rstudio.com/", dependencies=TRUE)
install.packages("dplyr", repos="http://cran.rstudio.com/", dependencies=TRUE)
install.packages("summarytools", repos="http://cran.rstudio.com/", dependencies=TRUE)
install.packages("ggplot2", repos="http://cran.rstudio.com/", dependencies=TRUE)
install.packages("tiff", repos="http://cran.rstudio.com/", dependencies=TRUE)
install.packages("ncdf4", repos="http://cran.rstudio.com/", dependencies=TRUE)
install.packages("raster", repos="http://cran.rstudio.com/", dependencies=TRUE)
install.packages("rasterVis", repos="http://cran.rstudio.com/", dependencies=TRUE)
install.packages("rgdal", repos="http://cran.rstudio.com/", dependencies=TRUE)
install.packages("gapfill", repos="http://cran.rstudio.com/", dependencies=TRUE)
install.packages("RColorBrewer", repos="http://cran.rstudio.com/", dependencies=TRUE)
install.packages("stars", repos="http://cran.rstudio.com/", dependencies=TRUE)

# 
library(devtools)
library(dplyr)

library(summarytools)
library(ggplot2)
library(tiff)
library(ncdf4) 
library(raster)
library(rasterVis)
library(rgdal)
library(gapfill)
library(RColorBrewer)
library(stars)

getwd()
# set working directory to ensure R can find the file we wish to import
# be sure that the downloaded file is in this directory
files<-list.files("C:/Users/user/Desktop/R_FILES/TRICHONIDA_DATA/Surf_Ref_8Days_250m_v6_full/")
#C:\Users\user\Desktop\R_FILES\TRICHONIDA_DATA\Surf_Ref_8Days_250m_v6_full


# Create list of NDVI file paths
path_tif_files <- paste0("C:/Users/user/Desktop/R_FILES/TRICHONIDA_DATA/Surf_Ref_8Days_250m_v6_full/",files)

# collecting Band1 to B7 
All_B1<-lapply(path_tif_files, raster, band = 1 )  # reading all files band1 together .
All_B2<-lapply(path_tif_files, raster, band = 2 ) 
All_B3<-lapply(path_tif_files, raster, band = 3 ) 
All_B4<-lapply(path_tif_files, raster, band = 4 ) 
All_B5<-lapply(path_tif_files, raster, band = 5 ) 
All_B6<-lapply(path_tif_files, raster, band = 6 ) 
All_B7<-lapply(path_tif_files, raster, band = 7 ) 


# apply scale factor
All_B1 <- All_B1/10000
All_B2 <- All_B2/10000
All_B3 <- All_B3/10000
All_B4 <- All_B4/10000
All_B5 <- All_B5/10000
All_B6 <- All_B6/10000
All_B7 <- All_B7/10000

# stacking all time series as layers 
All_B1 <- stack(All_B1) 
All_B2 <- stack(All_B2) 
All_B3 <- stack(All_B3) 
All_B4 <- stack(All_B4) 
All_B5 <- stack(All_B5) 
All_B6 <- stack(All_B6) 
All_B7 <- stack(All_B7) 

summary(All_B1)
e<-e/10000
# Create a time series raster stack

plot(All_B6, nc = 6)

levelplot(All_B6,
          layout=c(8, 4), # create a 4x4 layout for the data
          
          main="level plot for the year 2002")


names(All_B1) <-  substr(names(All_B1), 7,9)
names(All_B2) <-  substr(names(All_B2), 7,9)
names(All_B3) <-  substr(names(All_B3), 7,9)
names(All_B4) <-  substr(names(All_B4), 7,9)
names(All_B5) <-  substr(names(All_B5), 7,9)
names(All_B6) <-  substr(names(All_B6), 7,9)
names(All_B7) <-  substr(names(All_B7), 7,9)


levelplot(All_B6_v2,
          layout=c(5, 5), # create a 4x4 layout for the data
          main="Text ")
######
# gapfill demo : 
## create raster data for demo
## extent: x=10, y=10, month=3, years=2
data <- array(runif(600), c(10,10,6))
data[c(1,5,54,76,150,450,556)] <- NA
input_stack <- stack(brick(data))
plot(input_stack)
#View(data)
#View(input_array)
## create array and predict missing values with gapfill()
tmp <- array(input_stack, dim=c(10,10,3,2))
input_array <- aperm(tmp, c(2,1,3,4))
Image(input_array, col=rev(terrain.colors(100)), asRaster=TRUE)
output <- Gapfill(data=input_array)
output_array <- output$fill
Image(output_array, col=rev(terrain.colors(100)))



## convert back

output_stack <- stack(brick(array(output_array, c(10,10,6))))
plot(output_stack)
###############


# trying on B6

x2<-All_B6

image(x2)

# cuting the points from coordonites
#'+proj=longlat +datum=WGS84'

point_coord<- read.csv( "C:/Users/user/Desktop/UPWORKS/project_24_1_21/coords.csv")
lat<- point_coord$coords.x1
lon<- point_coord$coords.x2
point_coord<-tibble(lat, lon)
sppoints <- SpatialPoints(point_coord, proj4string=CRS("+proj=utm +zone=34 +datum=WGS84 +units=m +no_defs"))
tp <- spTransform(sppoints, crs(x2))
e <- extract(x2, tp)

tmp <- array(e, dim=c(13, 1,1, 817)) #pixel x, pixel y, point (1), dim time ##for 1 point 1,1,1,817 

table(is.na(tmp)) # verify Nas
dim(tmp)

Image(tmp, col=rev(terrain.colors(100)), asRaster=TRUE)
out<-Gapfill(data=tmp)

Image(out$fill, col=rev(terrain.colors(100)), asRaster=TRUE)

table(is.na(out$fill))
out<-Gapfill(data=out$fill)
plot(tmp)

output_stack <- stack(brick(array(out$fill, c(13, 1, 817))))
# save Data as dataset
result_dataset<-as_tibble( out$fill)
# save data as picture stack
