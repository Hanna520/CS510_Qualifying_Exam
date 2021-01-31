# About this script:
# This R script plots velocity vectors on the background of individual images from particle image velocimetry. The vectors fade from red to black based on the maximum velocity of the video clip (red) to zero (black). x and y are position data and u is the x-component of velocity and v is the y-component of velocity.

# Load Required Packages
library(grid) 
require(tiff)
library(foreach)
library(doParallel)
library(pryr)
library(raster)

# Clear Workspace
rm(list=ls())

# Move to working directory
# setwd("/Users/Spectre/Dropbox/microPIV/prelimdata/subset1")

scale <- 0.02   # Scale of the length of vector arrows
angle <- 25     # Angle between the arrow head and body
length <- 0.03  # Length of the arrow head sides

start <- 41       # Start number of image sequence
end <- 90        # End number of image sequence
imageseq<-seq(start,end) # Constructs vector with image sequence for later reference

# Construct a function for creating the color map for vectors
myColorRamp <- function(colors, values) {
  v <- (values - min(values))/diff(range(values))
  x <- colorRamp(colors)(v)
  rgb(x[,1], x[,2], x[,3], maxColorValue = 255)
}

# This section reads in files for pair 31, which had the highest magnitudes of any other pair, used to scale color on all pairs
u31.1 <- read.csv("cs510-qe-january2021-Hanna520/practical/u31.csv",header=FALSE)
u31 <- as.matrix(u31.1)
v31.1 <- read.csv("cs510-qe-january2021-Hanna520/practical/v31.csv",header=FALSE)
v31 <- as.matrix(v31.1)
it31.1 <- sqrt(u31^2+v31^2)
it31 <- replace(it31.1, is.na(it31.1), 0)
cols31 <- myColorRamp(c("black", "red"), it31)	#Constructs color map

rm(u31.1, v31.1, u31, v31, it31)

# The following code plots image and PIV data for all 50 data sets
# Since the plot of each dataset is independent, we can use parallelization to optimize the speed
filepath <- "cs510-qe-january2021-Hanna520/practical/"
x_files <- list()
y_files <- list() 
u_files <- list()
v_files <- list()
images <- list()
w_values <- list()

foreach(i = 1:50) %dopar%{
  # Constructs image file name
  nametiff <- ifelse(imageseq[i]<100, 
                     paste0(filepath,"tunicate_PIV_07_14_C001H001S0001_images0000",
                            imageseq[i],".tif"),
                     paste0(filepath,"tunicate_PIV_07_14_C001H001S0001_images000",
                            imageseq[i],".tif"))
  
  # Loads individual files
  xfile <- read.csv(paste0(filepath, "x", i, ".csv"),header=FALSE)
  x_files[[i]] <- as.matrix(xfile)
  yfile <- read.csv(paste0(filepath, "y", i, ".csv"),header=FALSE)
  y_files[[i]] <- as.matrix(yfile)
  ufile <- read.csv(paste0(filepath, "u", i, ".csv"),header=FALSE)
  u_files[[i]] <- as.matrix(ufile)
  vfile <- read.csv(paste0(filepath, "v", i, ".csv"),header=FALSE)
  v_files[[i]] <- as.matrix(vfile)
  images[[i]] <- readTIFF(nametiff)
  
  # Calculates velocity magnitudes
  w_values[[i]] <- sqrt(u_files[[i]]^2+v_files[[i]]^2)
  
  # Plots and save Image and PIV data
  new_tiff <- str_remove(nametiff, filepath)
  tiff(file=paste0("results/",new_tiff,".tiff"),res=100)
  par(mar=c(0,0,0,0)) # Sets margins of plot in frame
  plot(c(min(x_files[[i]]),max(x_files[[i]])),
       c(min(y_files[[i]]),max(y_files[[i]])),
       pch=".",xlim=c(0,10),ylim=c(-9,5),
       xlab=c(" "),ylab=c(" ")) # Sets initial plot
  plot(raster(images[[i]],xmn=min(x_files[[i]]),ymn=min(y_files[[i]]),
              xmx=max(x_files[[i]]),ymx=max(y_files[[i]])),
       col=cols31) # Draws background image
  arrows(x_files[[i]], y_files[[i]], 
         x_files[[i]]+scale*u_files[[i]], y_files[[i]]+scale*v_files[[i]], 
         angle=10, length=length, col=cols31, code=1)  #Draws PIV arrows
  dev.off()
}

