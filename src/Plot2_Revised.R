# About this script:
# This R script plots velocity vectors on the background of individual images from particle image velocimetry. The vectors fade from red to black based on the maximum velocity of the video clip (red) to zero (black). x and y are position data and u is the x-component of velocity and v is the y-component of velocity. Right now, the script will complete 5 plots out of 50.

# Load Required Packages
library(grid) 
require(tiff)

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


# Constructs image file name
filepath <- "cs510-qe-january2021-Hanna520/practical/"
if (imageseq[1]<100) {
  nametiff<-paste0(filepath,"tunicate_PIV_07_14_C001H001S0001_images0000",imageseq[1],".tif")
} else {
  nametiff<-paste0(filepath,"tunicate_PIV_07_14_C001H001S0001_images000",imageseq[1],".tif")
}

# Loads individual files
x1.1 <- read.csv("cs510-qe-january2021-Hanna520/practical/x1.csv",header=FALSE)
x1 <- as.matrix(x1.1)
y1.1 <- read.csv("cs510-qe-january2021-Hanna520/practical/y1.csv",header=FALSE)
y1 <- as.matrix(y1.1)
u1.1 <- read.csv("cs510-qe-january2021-Hanna520/practical/u1.csv",header=FALSE)
u1 <- as.matrix(u1.1)
v1.1 <- read.csv("cs510-qe-january2021-Hanna520/practical/v1.csv",header=FALSE)
v1 <- as.matrix(v1.1)
img1 <- readTIFF(nametiff)

# Calculates velocity magnitudes
w1 <- sqrt(u1^2+v1^2)

# Plots Image and PIV data
par(mar=c(0,0,0,0)) # Sets margins of plot in frame
plot(c(min(x1),max(x1)),c(min(y1),max(y1)),pch=".",xlim=c(0,10),ylim=c(-9,5),xlab=c(" "),ylab=c(" ")) # Sets initial plot
rasterImage(img1,min(x1),min(y1),max(x1),max(y1))  # Draws background image
arrows(x1, y1, x1+scale*u1, y1+scale*v1, angle=10, length=length, col=cols31, code=1)  #Draws PIV arrows


# Constructs image file name
if (imageseq[2]<100) {
  nametiff<-paste0(filepath, "tunicate_PIV_07_14_C001H001S0001_images0000",imageseq[2],".tif")
} else {
  nametiff<-paste0(filepath, "tunicate_PIV_07_14_C001H001S0001_images000",imageseq[2],".tif")
}

# Loads individual files
x2.1 <- read.csv("cs510-qe-january2021-Hanna520/practical/x2.csv",header=FALSE)
x2 <- as.matrix(x2.1)
y2.1 <- read.csv("cs510-qe-january2021-Hanna520/practical/y2.csv",header=FALSE)
y2 <- as.matrix(y2.1)
u2.1 <- read.csv("cs510-qe-january2021-Hanna520/practical/u2.csv",header=FALSE)
u2 <- as.matrix(u2.1)
v2.1 <- read.csv("cs510-qe-january2021-Hanna520/practical/v2.csv",header=FALSE)
v2 <- as.matrix(v2.1)
img2 <- readTIFF(nametiff)

# Calculates velocity magnitudes
w2 <- sqrt(u2^2+v2^2)

# Plots Image and PIV data
par(mar=c(0,0,0,0)) # Sets margins of plot in frame
plot(c(min(x2),max(x2)),c(min(y2),max(y2)),pch=".",xlim=c(0,10),ylim=c(-9,5),xlab=c(" "),ylab=c(" ")) # Sets initial plot
rasterImage(img2,min(x2),min(y2),max(x2),max(y2))  # Draws background image
arrows(x2, y2, x2+scale*u2, y2+scale*v2, angle=10, length=length, col=cols31, code=1)  #Draws PIV arrows



# Constructs image file name
if (imageseq[3]<100) {
  nametiff<-paste0(filepath,"tunicate_PIV_07_14_C001H001S0001_images0000",imageseq[3],".tif")
} else {
  nametiff<-paste0(filepath,"tunicate_PIV_07_14_C001H001S0001_images000",imageseq[3],".tif")
}

# Loads individual files
x3.1 <- read.csv("cs510-qe-january2021-Hanna520/practical/x3.csv",header=FALSE)
x3 <- as.matrix(x3.1)
y3.1 <- read.csv("cs510-qe-january2021-Hanna520/practical/y3.csv",header=FALSE)
y3 <- as.matrix(y3.1)
u3.1 <- read.csv("cs510-qe-january2021-Hanna520/practical/u3.csv",header=FALSE)
u3 <- as.matrix(u3.1)
v3.1 <- read.csv("cs510-qe-january2021-Hanna520/practical/v3.csv",header=FALSE)
v3 <- as.matrix(v3.1)
img3 <- readTIFF(nametiff)

# Calculates velocity magnitudes
w3 <- sqrt(u3^2+v3^2)

# Plots Image and PIV data
par(mar=c(0,0,0,0)) # Sets margins of plot in frame
plot(c(min(x3),max(x3)),c(min(y3),max(y3)),pch=".",xlim=c(0,10),ylim=c(-9,5),xlab=c(" "),ylab=c(" ")) # Sets initial plot
rasterImage(img3,min(x3),min(y3),max(x3),max(y3))  # Draws background image
arrows(x3, y3, x3+scale*u3, y3+scale*v3, angle=10, length=length, col=cols31, code=1)  #Draws PIV arrows



# Constructs image file name
if (imageseq[4]<100) {
  nametiff<-paste0(filepath,"tunicate_PIV_07_14_C001H001S0001_images0000",imageseq[4],".tif")
} else {
  nametiff<-paste0(filepath,"tunicate_PIV_07_14_C001H001S0001_images000",imageseq[4],".tif")
}

# Loads individual files
x4.1 <- read.csv("cs510-qe-january2021-Hanna520/practical/x4.csv",header=FALSE)
x4 <- as.matrix(x4.1)
y4.1 <- read.csv("cs510-qe-january2021-Hanna520/practical/y4.csv",header=FALSE)
y4 <- as.matrix(y4.1)
u4.1 <- read.csv("cs510-qe-january2021-Hanna520/practical/u4.csv",header=FALSE)
u4 <- as.matrix(u4.1)
v4.1 <- read.csv("cs510-qe-january2021-Hanna520/practical/v4.csv",header=FALSE)
v4 <- as.matrix(v4.1)
img4 <- readTIFF(nametiff)

# Calculates velocity magnitudes
w4 <- sqrt(u4^2+v4^2)

# Plots Image and PIV data
par(mar=c(0,0,0,0)) # Sets margins of plot in frame
plot(c(min(x4),max(x4)),c(min(y4),max(y4)),pch=".",xlim=c(0,10),ylim=c(-9,5),xlab=c(" "),ylab=c(" ")) # Sets initial plot
rasterImage(img4,min(x4),min(y4),max(x4),max(y4))  # Draws background image
arrows(x4, y4, x4+scale*u4, y4+scale*v4, angle=10, length=length, col=cols31, code=1)  #Draws PIV arrows

# Constructs image file name
if (imageseq[5]<100) {
  nametiff<-paste0(filepath,"tunicate_PIV_07_14_C001H001S0001_images0000",imageseq[5],".tif")
} else {
  nametiff<-paste0(filepath,"tunicate_PIV_07_14_C001H001S0001_images000",imageseq[5],".tif")
}

# Loads individual files
x5.1 <- read.csv("cs510-qe-january2021-Hanna520/practical/x5.csv",header=FALSE)
x5 <- as.matrix(x5.1)
y5.1 <- read.csv("cs510-qe-january2021-Hanna520/practical/y5.csv",header=FALSE)
y5 <- as.matrix(y5.1)
u5.1 <- read.csv("cs510-qe-january2021-Hanna520/practical/u5.csv",header=FALSE)
u5 <- as.matrix(u5.1)
v5.1 <- read.csv("cs510-qe-january2021-Hanna520/practical/v5.csv",header=FALSE)
v5 <- as.matrix(v5.1)
img5 <- readTIFF(nametiff)

# Calculates velocity magnitudes
w5 <- sqrt(u5^2+v5^2)

# Plots Image and PIV data
par(mar=c(0,0,0,0)) # Sets margins of plot in frame
plot(c(min(x5),max(x5)),c(min(y5),max(y5)),pch=".",xlim=c(0,10),ylim=c(-9,5),xlab=c(" "),ylab=c(" ")) # Sets initial plot
rasterImage(img5,min(x5),min(y5),max(x5),max(y5))  # Draws background image
arrows(x5, y5, x5+scale*u5, y5+scale*v5, angle=10, length=length, col=cols31, code=1)  #Draws PIV arrows


