## Main concepts to know before Diving into the tutorial
## Brick time series
library(rasta)
library(raster)
library(rgdal)
library(sp)
library(ggplot2)
library(rasterVis)
library(zoo)

## 1. The advantage of bricks is that they contain layer names
## number of layers?
## where do we get the dates from?

data(tura)
nlayers(tura)
names(tura)[1]
tura <- tura/10000

### 2. extracting the date from the character and create time series
substr(names(tura)[1], 10, 16)  ## date info a character
## character = year = julean date
?as.Date()

    ## example
    ## Group Session: let's have a look at another example
    ## read in date/time info in format 'm/d/y'
    dt <- c("02/27/92", "02/27/92", "01/14/92", "02/01/92")
    ts <- data.frame(dt, da = c(1, 4, 8, 2))
    ts$dt <- as.Date(ts$dt, format = "%m/%d/%y")
    class(ts$dt)
    ## plot
    plot(ts$dt, ts$da, type = "b", xlab = "Time", ylab = "data")
    format(ts$dt, "%m/%Y")
    ## check zoo object    
  z<- zoo(ts$da,ts$dt)
z
plot(z)
    ## Irregular (Landsat data) versus Regular time series (MODIS data)
    ?ts
    ## how would we define a regular MODIS time series?
    ts2 <- ts(1:20, frequency = 23, start = c(2000, 4)) # 2nd Quarter of 1959
    plot(ts2, type = "b")

dates <- substr(names(tura), 10, 16)  ## date info a character
dates <- as.Date(dates, format = "%Y%j")

# plot(dates, 1:length(dates))

## Try the tutorial
class(tura)
projection(tura)
res(tura)
extent(tura)
minValue(tura)
maxValue(tura)
names(tura)
sensor<-substr(names(tura),1,3)
print(sensor)
path<-as.numeric(substr(names(tura),4,6))
row<-as.numeric(substr(names(tura),7,9))
print(paste(path,row,sep=","))
dates<-substr(names(tura),10,16)
print(dates)
print(as.Date(dates,format="%Y%j"))
sceneID<-names(tura)
sceneinfo<-getSceneinfo(sceneID)
sceneinfo$date
sceneinfo$year<-factor(substr(sceneinfo$date,1,4),levels=c(1984:2013))
ggplot(data=sceneinfo,aes(x=year,fill=sensor))+
  geom_bar(width=0.7)+
  labs(y="number of scenes")+
  scale_y_continuous(limits=c(0,20))+
  theme_bw()+
  theme(axis.text.x=element_text(angle=45),legend.position=c(0.85,0.85))
plot(tura,c(1:9))
plot(tura[[1:9]],main=sceneinfo$date[c(1:9)])
bks<-seq(0,1,by=0.2)
cols<-rev(terrain.colors(length(bks)))
plot(tura[[1:9]],main=sceneinfo$date[1:9],breaks=bks,col=cols)

## have a look at the 
?getSceneinfo() ## function
## and create a graph Fig. 1

## 2. Plotting raster bricks
## 2a. Using the standard plotting techniques
## 2b. Using RasterVis package
library(rasterVis)
levelplot(tura[[1:6]])
levelplot(tura[[1:6]],names.attr=sceneinfo$date[1:6])
library(RColorBrewer)
display.brewer.all()
?brewer.pal()
cols<-brewer.pal(11,'Spectral')
?rasterTheme
rtheme<-rasterTheme(region=cols)
class(rtheme)
names(rtheme)
levelplot(tura[[1:6]],names.attr=sceneinfo$date[1:6],par.settings=rtheme)
histogram(tura[[1:6]])
bwplot(tura[[1:9]])

## 3. Calculating the data loss
## That should be possible with the knowledge from lesson 5 and 6 
## 3a. Optional - Visualise percentage NA's per date using ggplot

## 3b. now for all layers using calc()
# it's more readable to define the function first...

percNA <- function(x){
  y <- length(x[is.na(x)]) / length(x) * 100
  return(y)
}


# ...and then pass it to calc()
nodata <- calc(tura, fun = percNA)
plot(nodata)
plot(tura, 1)

mrb <- calc(tura, fun = mean, na.rm = TRUE)
plot(mrb)

sdrb <- calc(tura, fun = sd, na.rm = TRUE)
plot(sdrb)
click(tura,n=1)

## 4. Extracting a time series from the raster brick
plot(tura, 88)
x <- click(tura, n=1)
plot(dates, as.numeric(x), ylab= "NDVI", type = "b")
## zoo function
?zoo
z <- zoo(as.numeric(x), dates)
plot(z, type = "b")
y=freq(tura[[1]])
x=tura[53]
as.vector(x)
## 5. Zonal statistics i.e. extracting a time series per zone
# load in the data
data(lulcGewata)
# crop to the same extent as the tura rasterBrick
lulc <- crop(lulcGewata, extent(tura))
plot(lulc)
lulc
freq(lulc)
# load in the Look-up Table (LUT) to identify classes
data(LUTGewata)
print(LUTGewata)
forest <- matrix(data=c(819935, 832004), nrow=1, dimnames=list(NULL, c('x', 'y')))
## zonal
?zonal
cellFromXY(tura,forest)
data(lulcGewata)
lulc <- crop(lulcGewata, extent(tura))
zonestats <- zonal(x=tura, z=lulc, fun="mean")
head(zonestats)
# extract a layer from tura
x <- raster(tura, 88)
plot(x)
# distribution of values in this layer
histogram(x)
x
# mean NDVI per LULC class
meanNDVI <- zonal(x = x, z = lulc, fun = "mean")
print(meanNDVI)

zonestats <- zonal(x = tura, z = lulc, fun = "mean")

head(zonestats[,2])
class(zonestats)
dim(zonestats)
str(zonestats)

zonestats <- zonestats[,-1]

## time series
m <- as.matrix(t(zonestats))
z <- zoo(m, dates)
plot(z, type = "b", plot.type = "multiple")
plot(z, type = "b", plot.type = "single", col = c("orange", "darkgreen", "blue"), 
     lwd = c(1,2,1))

## exercise tips
plot(tura, 2)
if (FALSE) {
  e1 <- drawExtent()
  e2 <- drawExtent()
  print(e1)
  print(e2)
}

## (3) drawpoly and select average time series
e2 <- extent(c(822572.8, 822837.3, 831956.3, 832120.7))
## choose your own extent as this one it not immediately to right one.







