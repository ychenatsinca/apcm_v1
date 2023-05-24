# Title: Classification Code for Demo on Taoyuan Plateau
# Author: Meng-Hsuan Lin & Yi-Ying Chen
# Version: 1.0
# Date: 2023-05-24

# Step-1: SPOT classification for AOI - grid no.233_210 - northwest of Taoyuan Plateau ("classification" while loop)
# Step-2: Land-Use and Irrigation pond area detection ("detection" while loop)

##########

library(raster)
library(tidyverse)
library(rpart)
library(rgdal)

#Step1
classification <- TRUE
while (classification==TRUE){
  
  #assign grid no. of area of interest to go under land-use classification
  allaoi <- c("233_210")
  
  #import canopy height model raster for future usage
  canopy <- raster(x="./sourcefiles/canopyheight_233_210.tif") #data resolution: 6m
  #set crs as TWD97
  crs(canopy) <- "+proj=tmerc +lat_0=0 +lon_0=121 +k=0.9999 +x_0=250000 +y_0=0 +ellps=GRS80 +units=m +no_defs"
  
  #import Landsat classification raster for future usage
  landsatclass <- raster(x="./sourcefiles/LandsatNDBI_233_210.tif") #data resolution: 6m
  #set crs as TWD97
  crs(landsatclass) <- "+proj=tmerc +lat_0=0 +lon_0=121 +k=0.9999 +x_0=250000 +y_0=0 +ellps=GRS80 +units=m +no_defs"
  
  #classification loop of aoi grid for year 2015
  for (yr in 2015:2015){
    for (aoi in 1:length(allaoi)){ 
      print(allaoi[aoi]) #print allaoi[aoi] to see progress
      #direct to SPOT image directory
      directory <- paste("./spotimages/",yr,"/",sep="")
      #create list of SPOT image under directory that are from grid of AOI-
      #by filtering out images that satisfy condition matching grid number (ex.233_210)
      aoi_images <- dir(path = directory, pattern = "SPOT", all.files = FALSE,
                        full.names = FALSE, recursive = FALSE, ignore.case = FALSE, include.dirs = FALSE)
      aoi_images <- aoi_images[substr(aoi_images,start=23,stop=29)==allaoi[aoi]]
      
      #set default extent xmin xmax ymin ymax to 0
      extent_xmin <- 0
      extent_xmax <- 0
      extent_ymin <- 0
      extent_ymax <- 0
      row <- 0
      col <- 0
      
      #loop among all items to find min of xmin, max of xmax, min of ymin, and max of ymax among all images from grid and year of AOI-
      #to later use these extents to create universal raster extent for all SPOT images within this AOI to extend to
      for (item in 1:length(aoi_images)){
        testlayer <- raster(x=paste(directory,"/",aoi_images[item],"/",aoi_images[item],".1.bsq.ers",sep=""))
        if (item==1){
          extent_xmin <- extent(testlayer)[1]
          extent_xmax <- extent(testlayer)[2]
          extent_ymin <- extent(testlayer)[3]
          extent_ymax <- extent(testlayer)[4]
          row <- dim(testlayer)[1]
          col <- dim(testlayer)[2]
        }else{
          extent_xmin <- min(extent_xmin, extent(testlayer)[1])
          extent_xmax <- max(extent_xmax, extent(testlayer)[2])
          extent_ymin <- min(extent_ymin, extent(testlayer)[3])
          extent_ymax <- max(extent_ymax, extent(testlayer)[4])
          row <- max(row, dim(testlayer)[1])
          col <-max(col, dim(testlayer)[2])
        }
      } #end of item-loop
      
      #create extent object that holds xmin xmax ymin ymax values that you want to set future rasters to-
      #(this is to create an universal extent for all images under this AOI so later these rasters can be stacked)
      extent <- extent(extent_xmin,extent_xmax,extent_ymin,extent_ymax)
      
      #find rasters in the list aoi_images that when extended, does not match the majority's extent therefore cannot be stacked in the future
      t <- rep(NA,length(aoi_images))
      for (item in 1:length(aoi_images)){
        band <-  raster(x=paste(directory,"/",aoi_images[item],"/",aoi_images[item],".1.bsq.ers",sep=""))
        band <- extend(band, extent, value=NA)
        t[item] <- extent(band)[1]
      }
      xmin <- as.numeric(names(which.max(table(t)))) #find majority raster's extent after extending
      for (item in 1:length(aoi_images)){
        band <-  raster(x=paste(directory,"/",aoi_images[item],"/",aoi_images[item],".1.bsq.ers",sep=""))
        band <- extend(band, extent, value=NA)
        if (extent(band)[1]!=xmin){
          aoi_images[item] <- 0 #set as 0 first and not delete the item, so it won't mess up the index in the aoi_images list
        }
      }
      #remove images that are set as 0 (meaning these images within the AOI has different extent therefore cannot be stacked with other rasters)
      aoi_images <- aoi_images[aoi_images!=0]
      
      #create empty dataframe rasterallpoints to later store each pixels' monthly median NDVI & NIR value (_N stores NDVI values, _NIR stores NIR values)
      rasterallpoints <- data.frame("index"=1:(row*col),
                                    "January_N"=NA,
                                    "February_N"=NA,
                                    "March_N"=NA,
                                    "April_N"=NA,
                                    "May_N"=NA,
                                    "June_N"=NA,
                                    "July_N"=NA,
                                    "August_N"=NA,
                                    "September_N"=NA,
                                    "October_N"=NA,
                                    "November_N"=NA,
                                    "December_N"=NA,
                                    "January_NIR"=NA,
                                    "February_NIR"=NA,
                                    "March_NIR"=NA,
                                    "April_NIR"=NA,
                                    "May_NIR"=NA,
                                    "June_NIR"=NA,
                                    "July_NIR"=NA,
                                    "August_NIR"=NA,
                                    "September_NIR"=NA,
                                    "October_NIR"=NA,
                                    "November_NIR"=NA,
                                    "December_NIR"=NA)
      colnames(rasterallpoints) <- c("index","Jan_N","Feb_N","Mar_N","Apr_N","May_N","Jun_N","Jul_N","Aug_N","Sep_N","Oct_N","Nov_N","Dec_N",
                                     "Jan_NIR","Feb_NIR","Mar_NIR","Apr_NIR","May_NIR","Jun_NIR","Jul_NIR","Aug_NIR","Sep_NIR","Oct_NIR","Nov_NIR","Dec_NIR")
      
      #loop among 12 months to retrieve monthly median NDVI & NIR values for AOI
      for (month in 1:12){ #12 months
        #create strings ex. for 2015 january "201501" to search in aoi_images list of available images for aoi for the year+month
        print(month) #print month to see progress on looped months
        if (1<=month & month<=9){
          string_month <- paste("0",as.character(month),sep="")
        }else{
          string_month <- as.character(month)
        }
        #subset aoi_images to get those that satisfy month
        subset_aoiimages <- aoi_images[substr(aoi_images,start=7,stop=12)==paste(yr,string_month,sep="")]
        #if there are no available images for the month (aka length(subset_aoiimages)==0), set the points' median NDVI & NIR value for the month as NA
        if (length(subset_aoiimages)==0){
          rasterallpoints[,month+1] <- NA #ex. NDVI january column is the 1+1=2 2nd column
          rasterallpoints[,month+13] <- NA #ex. NIR january column is the 1+13=14 14th column
          next
        }else{
          #loop to stack images with same month and AOI to calculate median NDVI & NIR
          for (image in 1:length(subset_aoiimages)){
            print(subset_aoiimages[image]) #print subset_aoiimages[image] to see progress of raster stacking for the month
            #loop to retrieve rasters for all 4 bands
            for (band in 1:4){
              #assign SPOT image bands 1-4 to variables band1-4
              assign(paste("band",band,sep=""),  raster(x=paste(directory,"/",subset_aoiimages[image],"/",subset_aoiimages[image],".",band,".bsq.ers",sep="")))
            } #end of band-loop
            #create tempNDVIlayer to get NDVI raster created from band math
            tempNDVIlayer <- (band4-band3)/(band4+band3)
            #extend created NDVI raster and NIR raster to the universal extent for the images of the AOI with NA values
            tempNDVIlayer <- extend(tempNDVIlayer, extent, value=NA)
            band4 <- extend(band4, extent, value=NA)
            #stack all images under the month together-
            #resulting NDVIstack & NIRstack raster stack holds NDVI & NIR value of all the images available under the month of that aoi
            print(extent(tempNDVIlayer))
            if (image==1){
              NDVIstack <- tempNDVIlayer
              NIRstack <- band4
            }else{
              NDVIstack <- stack(NDVIstack, tempNDVIlayer)
              NIRstack <- stack(NIRstack, band4)
            }
          } #end of image-loop
          
          #if NDVIstack has more than one layer (aka more than one available image for the month), perform median among all layers
          if (dim(NDVIstack)[3]!=1){
            #convert from RasterLayer to RasterBrick
            NDVIstack <- brick(NDVIstack)
            #perform median among the raster stack of all images under the month together
            medianNDVI_ofmonth <- calc(NDVIstack, median,na.rm=T) #ignore error, still produces results
            #if NDVIstack only has one layer, meaning there is only one available image for the month, don't need to perform median;-
            #the single NDVI layer will be counted for the month
          }else if (dim(NDVIstack)[3]==1){ 
            medianNDVI_ofmonth <- NDVIstack
          }
          
          #if NIRstack has more than one layer (aka more than one available image for the month), perform median among all layers
          if (dim(NIRstack)[3]!=1){
            #convert from RasterLayer to RasterBrick
            NIRstack <- brick(NIRstack)
            #perform median among the raster stack of all images under the month together
            medianNIR_ofmonth <- calc(NIRstack, median,na.rm=T) #ignore error, still produces results
            #if NIRstack only has one layer, meaning there is only one available image for the month, don't need to perform median;-
            #the single NIR layer will be counted for the month
          }else if (dim(NIRstack)[3]==1){ 
            medianNIR_ofmonth <- NIRstack
          }
          
          #set crs of medianNDVI_ofmonth & medianNIR_ofmonth
          crs(medianNDVI_ofmonth) <- "+proj=tmerc +lat_0=0 +lon_0=121 +k=0.9999 +x_0=250000 +y_0=0 +ellps=GRS80 +units=m +no_defs"
          crs(medianNIR_ofmonth) <- "+proj=tmerc +lat_0=0 +lon_0=121 +k=0.9999 +x_0=250000 +y_0=0 +ellps=GRS80 +units=m +no_defs"
          
          #convert median NDVI & NIR value raster for the month to a dataframe
          medianNDVI <- as.data.frame(medianNDVI_ofmonth)
          medianNIR <- as.data.frame(medianNIR_ofmonth)
          #assign the points' median NDVI & NIR value for the month to rasterallpoints dataframe
          rasterallpoints[,month+1] <- medianNDVI #ex. NDVI january column is the 1+1=2 2nd column
          rasterallpoints[,month+13] <- medianNIR #ex. NIR january column is the 1+13=14 14th column
        }
      } #end of month-loop
      
      #calculate max, min of median NDVI from Jan to Dec for all points and store in new column "max_N", "min_N"
      rasterallpoints["max_N"] <-apply(X=rasterallpoints[2:13], MARGIN=1, FUN=max,na.rm=T) #margin=1 meaning retrieving max by row
      rasterallpoints["min_N"] <-apply(X=rasterallpoints[2:13], MARGIN=1, FUN=min,na.rm=T) #margin=1 meaning retrieving min by row
      #calculate variance of median NDVI from Jan to Dec for all points and store in new column "variance_N"
      rasterallpoints["variance_N"] <-apply(X=rasterallpoints[2:13], MARGIN=1, FUN=var,na.rm=T) #margin=1 meaning retrieving variance by row
      #calculate median of median NIR from Jan to Dec for all points and store in new column "median_NIR"
      rasterallpoints["median_NIR"] <-apply(X=rasterallpoints[14:25], MARGIN=1, FUN=median,na.rm=T)
      #calculate NDVI max/NDVI variance ratio from Jan to Dec for all points and store in new column "maxvar_ratio"
      rasterallpoints["maxvar_ratio"] <- rasterallpoints["max_N"]/rasterallpoints["variance_N"]
      
      #--------------------#classification based on decision tree#--------------------#
      #classify as forest=1, builtup=2, water=3, agri=4, unknown=5 (BA mix), unclassified=6
      #duplicate rasterallpoints dataframe for further usage
      allpoints <- rasterallpoints
      allpoints["class"] <- NA
      
      #thresholds of classification tree
      allpoints$class[allpoints$max_N<=0.03] <- 3
      
      allpoints$class[allpoints$max_N>0.03 & allpoints$max_N<=0.19 & allpoints$median_NIR<=400] <- 3
      allpoints$class[allpoints$max_N>0.03 & allpoints$max_N<=0.19 & allpoints$median_NIR>400] <- 2
      
      allpoints$class[allpoints$max_N>0.19 & allpoints$max_N<=0.39 & allpoints$variance_N<0.001] <- 2
      allpoints$class[allpoints$max_N>0.19 & allpoints$max_N<=0.39 & allpoints$variance_N>=0.001 & allpoints$variance_N<=0.005] <- 5
      allpoints$class[allpoints$max_N>0.19 & allpoints$max_N<=0.39 & allpoints$variance_N>0.005] <- 4
      
      allpoints$class[allpoints$max_N>0.39 & allpoints$maxvar_ratio<=19.19] <- 4
      allpoints$class[allpoints$max_N>0.39 & allpoints$maxvar_ratio>19.19] <- 1
      
      #classify points that are not classified from above decision tree as unclassified land type 6
      allpoints$class[is.na(allpoints$class)==TRUE] <- 6
      
      #output dataframe as raster
      #duplicate medianNDVI_ofmonth raster created from previous month for-loop to copy dimension, extent, and projection
      outputraster<- medianNDVI_ofmonth
      values(outputraster) <- NA
      #replace outputraster NA values with the classification results by index
      outputraster <- replace(outputraster,allpoints$index,allpoints$class)
      
      #set crs as TWD97
      crs(outputraster) <- "+proj=tmerc +lat_0=0 +lon_0=121 +k=0.9999 +x_0=250000 +y_0=0 +ellps=GRS80 +units=m +no_defs"
      
      #--------------------#canopy height model filters forest/agriculture mixups & Landsat classification filters built-up/water mixups#--------------------#
      #crop canopy raster based on the extent of AOI, therefore extent of outputraster (classified result) raster
      canopyraster <- crop(canopy, outputraster)
      #set extent of canopy raster just like outputraster raster
      extent(canopyraster) <- extent(outputraster)
      #crop landsatclass raster based on the extent of AOI, therefore extent of outputraster (classified result) raster
      landsatclassraster <- crop(landsatclass, outputraster)
      #set extent of landsatclassraster raster just like outputraster raster
      extent(landsatclassraster) <- extent(outputraster)
      
      #before doing raster stack function, need to  make sure the raster size is the same- 
      #by using the project function for the cropped rasters to  match the dimension of outputraster data
      canopyraster <- projectRaster(canopyraster,outputraster,method = 'bilinear')
      landsatclassraster <- projectRaster(landsatclassraster,outputraster,method = 'bilinear')
      
      #stack classified result, cropped canopy raster, and cropped landsat NDBI raster
      comparestack <- stack(outputraster, canopyraster)
      comparestack <- stack(comparestack, landsatclassraster)
      
      comparestack <- as.data.frame(comparestack)
      #rename comparestack df columns to class (classification land type), height (canopy height), and landsat classification NDBI
      colnames(comparestack) <- c("class", "height", "NDBI")
      #add index column to retain original index of each cell since default index may alter when subset
      comparestack["index"] <- 1:dim(comparestack)[1]
      #rearrange "comparestack" dataframe columns order to index, class, height, NDBI
      comparestack <- comparestack[,c(4,1,2,3)]
      
      #original classification classify as: forest=1, builtup=2, water=3, agri=4, unknown=5 (BA mix), unclassified=6
      #new classification classify as: forest=1, builtup=2, water=3, agri=4, unknown=5 (BA mix), grassland=6, unclassified=7
      comparestack["class"][comparestack["class"]==6] <- 7 #switch class index of unclassified from 6 to index 7
      #using canopy height data, substitute comparestack df cells that are classified as agri but have canopy height over 3 meters to forests
      comparestack["class"][(comparestack["class"]==4)&(comparestack["height"]>3)] <- 1
      #using canopy height data, substitute comparestack df cells that are classified as forest-
      #but have canopy height less than 3 meters to grassland (land type 6)
      comparestack["class"][(comparestack["class"]==1)&(comparestack["height"]<=3)] <- 6
      
      #using Landsat NDBI data, substitute comparestack df cells that are classified as water- 
      #but landsat classification has -0.04<NDBI or NDBI<-0.5 becomes builtup
      comparestack["class"][(comparestack["class"]==3)&((-0.04<comparestack["NDBI"])|(-0.5>comparestack["NDBI"]))] <- 2
      comparestack["class"][(comparestack["class"]==3)&(comparestack["NDBI"]<=-0.04)&(comparestack["NDBI"]>=-0.5)] <- 3
      #--------------------#
      
      #output dataframe as raster
      #duplicate outputraster raster to copy dimension and extent
      outputraster<- outputraster
      values(outputraster) <- NA
      #replace outputraster NA values with the classification results by index
      outputraster <- replace(outputraster,comparestack$index,comparestack$class)
      #set projection as TWD97
      crs(outputraster) <- "+proj=tmerc +lat_0=0 +lon_0=121 +k=0.9999 +x_0=250000 +y_0=0 +ellps=GRS80 +units=m +no_defs"
      
      #output classification result as tif raster
      writeRaster(outputraster,paste("./",yr,"_",allaoi[aoi],"_classification.tif",sep=""))
      
      #plot classification results to directly visualize results
      #colors:
      #dark green for forest(1), red for builtup(2), blue for water(3), orange for agri(4),
      #grey for unknown(builtup/agriculture mix)(5), yellow-green for grassland(6), black for unclassified(7)
      colors <- c("#439c6e","#e86d5f","#8bd2e8","#f0d86e","#999999","#99ad50","#383838")
      #subset color palette to the available classified values (ex. sometimes an image doesn't have unknown land type, so we omit grey from the color palette)
      colors <- colors[sort(unique(outputraster))]
      #plot classification results
      png(paste("./results/",yr,"_",allaoi[aoi],"_classification.png",sep=""),
          width = 1080, height = 1080, units = "px")
      plot(outputraster,
           col=colors)
      dev.off()
      
      #clear up variables "allpoints" and "rasterallpoints"
      rm(allpoints)
      rm(rasterallpoints)
    } #end of aoi for loop
    
  } #end of yr for loop
  classification <- FALSE
} #end of classification while loop

#Step2
detection <- FALSE
while (detection==TRUE){
  
  yr<-2015
  allaoi <- c("233_210")
  
  for (aoi in 1:length(allaoi)){
    
    #create dataframe df to store land area values for the land types under AOI
    df <- data.frame("yr" <- c(yr))
    colnames(df) <- "year"
    df["forest_m2"] <- NA
    df["builtup_m2"] <- NA
    df["water_m2"] <- NA
    df["agri_m2"] <- NA
    df["grass_m2"] <- NA
    df["pond_m2"] <- NA
    df["unknown_m2"] <- NA
    
    for (yr in 1:length(df$year)){
      #input classification results
      classificationraster <- raster(paste("./results/",df$year[yr],"_",allaoi[aoi],"_classification.tif",sep=""))
      #create a matrix of index to attach to classificationraster raster to give index value
      indexmatrix <- matrix(1:(dim(classificationraster)[1]*dim(classificationraster)[2]),nrow=dim(classificationraster)[1],ncol=dim(classificationraster)[2],byrow = TRUE)
      #convert matrix to raster
      indexraster <- raster(indexmatrix)
      #set coordinate system and extent of index raster just like classificationraster classification raster
      crs(indexraster) <- crs(classificationraster)
      extent(indexraster) <- extent(extent(classificationraster)[1],extent(classificationraster)[2],
                                    extent(classificationraster)[3],extent(classificationraster)[4])
      #stack classificationraster classification raster with index raster just created
      rasterstack <- stack(classificationraster,indexraster)
      #change raster layers' names
      names(rasterstack) <- c("ltype","index")
      
      #see occurrences/land size of the 6 types - forest, builtup, water, agri, grassland
      occurrence <- as.data.frame(classificationraster)
      colnames(occurrence) <- c("type")
      #occurrence is multipled by a factor of 36 to get area of each land type because each pixel is 6mx6m in spatial resolution
      df$forest_m2[yr] <- sum(occurrence$type == 1, na.rm=T)*6*6 #forest
      df$builtup_m2[yr] <- sum(occurrence$type == 2, na.rm=T)*6*6 #builtup
      df$water_m2[yr] <- sum(occurrence$type == 3, na.rm=T)*6*6 #water
      df$agri_m2[yr] <- sum(occurrence$type == 4, na.rm=T)*6*6 #agri
      df$grass_m2[yr] <- sum(occurrence$type == 6, na.rm=T)*6*6 #grassland
      df$unknown_m2[yr] <- sum(occurrence$type == 5, na.rm=T)*6*6 #unknown
      
      #----------#see size of irrigation ponds#----------#
      #import taoyuan pond centroids coordinates csv
      #this version of csv's pond name column is taken out and replaced as number because csv can't read in chinese
      centroidpoints <- read.csv("./sourcefiles/pondcentroid_coordinates.csv", header=T, encoding = "UTF-8",sep = ",")
      #set the point coordinates to TWD97
      coordinates(centroidpoints)= ~ TWD97X + TWD97Y
      crs(centroidpoints) <- crs(classificationraster)
      #extract value of classificationraster raster (land type and index) through overlapping point of centroid
      rasValue <- raster::extract(rasterstack, centroidpoints)
      #create combinePointValue dataframe that stores the centroid coordinates and extracted index+landtype value from classificationraster raster
      combinePointValue <- cbind(as.data.frame(centroidpoints),rasValue)
      #create new pondarea column to combinePointValue dataframe
      combinePointValue["pondarea"] <- NA
      
      #set box range of 75 to extract water pixels around pond centroid and detect as irrigationpond
      #use 75 because on Taoyuan gov website it says largest pond is 198900 m2, which is 198900/6/6=5525 pixels,-
      #which in a matrix square of 75 pixels per side (75*75=5625)
      boxrange<-75
      #calculate left,right,top,bottom distance from centroid within box
      box <- (75-1)/2
      #iterate among combinePointValue to see irrigation pond area detected in the classification result
      for (point in 1:length(combinePointValue$index)){
        print(point) #print point to see progress of iterated pond centroid
        #if the irrigation pond centroid is not within AOI, skip the pond
        if (is.na(combinePointValue$ltype[point])){
          next
        #if the irrigation pond centroid is within AOI and the extracted land type of classification result for the centroid is water,-
        #find amount of surrounding water pixels
        }else if (combinePointValue$ltype[point]==3){
          #convert index value to raster row and column value
          raster_row <- (combinePointValue$index[point]%/%(dim(classificationraster)[2]))+1
          raster_col <- combinePointValue$index[point]-((combinePointValue$index[point]%/%(dim(classificationraster)[2]))*dim(classificationraster)[2])
          #clip a 75x75 matrix with center at centroid
          #the min max functions in the extent function is to deal with situations where centroid is close to border therefore cannot extend 75x75 matrix outwards
          clipmatrix <- crop(rasterstack, extent(rasterstack, max(1,raster_row-box), min(dim(classificationraster)[1],raster_row+box), max(1,raster_col-box), min(dim(classificationraster)[2],raster_col+box))) #row min, row max, col min, col max
          #inspect within the clipped matrix box to see if there are water pixels surrounding the irrigation pond centroid
          for (row in 1:boxrange){
            for (col in 1:boxrange){
              #if the pixel is not classified as water, skip 
              if (is.na(clipmatrix[row,col][1])){
                next
              #inspect within the clipped matrix and detect water pixels (water pixels that stand alone will not be counted)
              }else if (clipmatrix[row,col][1]==3){
                #templist evaluating left right top bottom pixels of clipmatrix[row,col][1] to see if they are also water ltype
                #solve issues regarding border pixels - templist[1]=0 means that there is no left pixels, clipmatrix[row,col][1] is at left border of matrix
                #                                       templist[2]=boxrange+1 means that there is no right pixels, clipmatrix[row,col][1] is at right border of matrix
                #                                       templist[3]=0 means that there is no top pixels, clipmatrix[row,col][1] is at top border of matrix
                #                                       templist[4]=boxrange+1 means that there is no bottom pixels, clipmatrix[row,col][1] is at bottom border of matrix
                templist<-c(max(0,col-1),min(boxrange+1,col+1),max(0,row-1),min(boxrange+1,row+1)) #left right top bottom
                #if templist[1]!=0 that there is left neighboring pixel, get land type of the left pixel
                if (templist[1]!=0){ templist[1]<- clipmatrix[row,templist[1]][1] }
                #if templist[2]!=(boxrange+1) that there is right neighboring pixel, get land type of the right pixel
                if (templist[2]!=(boxrange+1)){ templist[2]<-clipmatrix[row,templist[2]][1] }
                #if templist[3]!=0 that there is top neighboring pixel, get land type of the top pixel
                if (templist[3]!=0){ templist[3]<-clipmatrix[templist[3],col][1] }
                #if templist[4]!=(boxrange+1) that there is bottom neighboring pixel, get land type of the bottom pixel
                if (templist[4]!=(boxrange+1)){ templist[4]<-clipmatrix[templist[4],col][1] }
                #set templist[1]/templist[3] as 0 is there are no left/top neighbor pixels
                templist[c(1,3)][templist[c(1,3)]==0]<- 0
                #set templist[2]/templist[4] as 0 is there are no right/bottom neighbor pixels
                templist[c(2,4)][templist[c(2,4)]==boxrange+1]<- 0
                #count the number water pixels surrounding the iterated pixel; if there is no surrounding water pixels, the center pixel will not be counted as pond
                if ((3 %in% templist)==FALSE){
                  clipmatrix[row,col][1]<-NA #set those pixels that don't contain neighboring water pixels as NA
                } #end of if statement
              } #end of else-if statement
            } #end of col-loop
          } #end of row-loop
          #create classifiedlandtype dataframe out of the land type column of clipmatrix dataframe
          classifiedlandtype <- as.data.frame(clipmatrix$ltype)
          #count the number of occurence of 3 within the clipped 75x75 matrix to see how many water pixels are counted as pond and *6*6 to get total area of the iterated pond (pixel resolution is 6x6m)
          combinePointValue$pondarea[point] <- (sum(classifiedlandtype==3,na.rm=TRUE))*6*6
        } #end of else-if statement
      } #end of point-loop
      
      #output area of each pond, /1000000 to convert from m2 to km2
      eachpond_area <- cbind(as.data.frame(centroidpoints),combinePointValue$pondarea/1000000)
      colnames(eachpond_area) <- c("ID", "TWD97X", "TWD97Y", "area_km2")
      #output csv of each pond's area
      write.csv(eachpond_area, paste("./results/", df$year[yr], "_eachpondarea.csv",sep=""), row.names = FALSE)
      
      #add all area of detected irrigation ponds together to get total area of all ponds
      sum(combinePointValue$pondarea, na.rm=T)
      df$pond_m2[yr] <- sum(combinePointValue$pondarea, na.rm=T) #pond
      
      #output csv of detected area of each land types
      write.csv(df, paste("./results/", df$year[yr], "_landtype_areachange.csv",sep=""), row.names = FALSE)
    } #end of yr for-loop
  } #end of aoi for-loop
  detection <- FALSE
} #end of detection while loop



