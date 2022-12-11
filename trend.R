library(raster)
library(rgdal)
library(rasterVis)
library(spatialEco)
library(Kendall)
library(RColorBrewer)
library(grDevices)
library(cowplot)
library(sp)
library(levelplot)
library(sign)
library(ggplot2)

#
################################ monthly_trend_calculation#################

AODrasterlsitmonthly <- list.files(path = "F:/ShareBoth/prewhitening/AOT/monthly/3rd_monthsum/raster/", full.names = TRUE)

length(AODrasterlsitmonthly)
AODrasterstackmonthly <- stack(AODrasterlsitmonthly)
AODrasterstackmonthly
plot (raster.kendall(AODrasterstackmonthly,p.value = TRUE))

fun_kendall <- function(x){ return(unlist(MannKendall(x)))}


kendall_resultAOTmonthly <- calc(AODrasterstackmonthly,fun_kendall)
plot(kendall_resultAOTmonthly)
writeRaster(x=kendall_resultAOTmonthly$S,filename = file.path("F:/ShareBoth/prewhitening/","trend_aot_monthlysum.tif"),format="GTiff")
writeRaster(x=kendall_resultAOTmonthly$sl,filename = file.path("F:/ShareBoth/prewhitening/","pvalue_aot_monthlysum.tif"),format="GTiff")

#---------------#


ndvirasterlsitmonthyl <- list.files(path = "F:/ShareBoth/prewhitening/NDVI/monthly/3rd_monthlysum_thesis/rasters/", full.names = TRUE)

length(ndvirasterlsitmonthyl)
ndvirasterstackmonthly <- stack(ndvirasterlsitmonthyl)
ndvirasterstackmonthly
plot (raster.kendall(ndvirasterstackmonthly,p.value = TRUE))

fun_kendall <- function(x){ return(unlist(MannKendall(x)))}
kendall_resultndvimonthly <- calc(ndvirasterstackmonthly,fun_kendall)
plot(kendall_resultndvimonthly)


writeRaster(x=kendall_resultndvimonthly$S,filename = file.path("F:/ShareBoth/prewhitening/","trend_ndvi_monthlysum.tif"),format="GTiff")
writeRaster(x=kendall_resultndvimonthly$sl,filename = file.path("F:/ShareBoth/prewhitening/","pvalue_ndvi_monthlysum.tif"),format="GTiff")


#---------------#


firerasterlsitmonthly <- list.files(path = "F:/ShareBoth/prewhitening/FIRES/mothly/ascii_fire_annual/rasters/", full.names = TRUE)
vector_path_boundary <- "F:/Thesis Work/VIIRS_DATA/RasterMaps/Maping/boundary/projected_mercator_external_boundary.shp"
vector_path_points <- "F:/Thesis Work/VIIRS_DATA/RasterMaps/Maping/boundary/City_points.shp"

Vector_boundary <-  readOGR(dsn = vector_path_boundary,layer = "projected_mercator_external_boundary")
vector_points <- readOGR(vector_path_points, layer = "City_points")

length(firerasterlsitmonthly)
firerasterstackmonthly <- stack(firerasterlsitmonthly)
firerasterstackmonthly
plot (raster.kendall(firerasterstackannual,p.value = TRUE))

fun_kendall <- function(x){ return(unlist(MannKendall(x)))}


kendall_resultfiremonthly <- calc(firerasterstackmonthly,fun_kendall)
plot(kendall_resultfiremonthly)

writeRaster(x=kendall_resultfiremonthly$S,filename = file.path("F:/ShareBoth/prewhitening/","trend_fire_rast_annualyg.tif"),format="GTiff")
writeRaster(x=kendall_resultfiremonthly$sl,filename = file.path("F:/ShareBoth/prewhitening/","pvalue_fire_monthly.tif"),format="GTiff")




################################# annual_trend_calcualtion################################

fire8dayannual <- list.files(path = "F:/ShareBoth/prewhitening/FIRES/annual/ascii_raster/rasters/", full.names = TRUE)
vector_path_boundary <- "F:/Thesis Work/VIIRS_DATA/RasterMaps/Maping/boundary/projected_mercator_external_boundary.shp"
vector_path_points <- "F:/Thesis Work/VIIRS_DATA/RasterMaps/Maping/boundary/City_points.shp"

Vector_boundary <-  readOGR(dsn = vector_path_boundary,layer = "projected_mercator_external_boundary")
vector_points <- readOGR(vector_path_points, layer = "City_points")

length(fire8dayannual)
fire8dayannualstack <- stack(fire8dayannual)
fire8dayannualstack

fireannualtrend <- raster.kendall(fire8dayannualstack, p.value = TRUE)
plot(fireannualtrend)


fun_kendall <- function(x){ return(unlist(MannKendall(x)))}
kendall_resultannualfire <- calc(fire8dayannualstack,fun_kendall)
plot(kendall_resultannualfire)

writeRaster(x=kendall_resultannualfire$S,filename = file.path("F:/ShareBoth/prewhitening/","trend_annual_fire_rast.tif"),format="GTiff")
writeRaster(x=kendall_resultannualfire$sl,filename = file.path("F:/ShareBoth/prewhitening/","pvalue_annual_fire_rast.tif"),format="GTiff")



#-----------------------#
  
aot8dayannual <- list.files(path = "F:/ShareBoth/prewhitening/AOT/annual/3rd_annualsum/rasters/", full.names = TRUE)

length(aot8dayannual)
aot8dayannualstack <- stack(aot8dayannual)
aot8dayannualstack
aotannualtrend <- raster.kendall(aot8dayannualstack, p.value = TRUE)
plot(aotannualtrend)


fun_kendall <- function(x){ return(unlist(MannKendall(x)))}
kendall_resultannualaot <- calc(aot8dayannualstack,fun_kendall)
plot(kendall_resultannualaot)

writeRaster(x=kendall_resultannualaot$S,filename = file.path("F:/ShareBoth/prewhitening/","trend_aot_annualsum.tif"),format="GTiff")
writeRaster(x=kendall_resultannualaot$sl,filename = file.path("F:/ShareBoth/prewhitening/","pvalue_aot_annualsum.tif"),format="GTiff")



#-----------------------#

ndvi8dayannual <- list.files(path = "F:/ShareBoth/prewhitening/NDVI/annual/3rd_annualsum/rasters/", full.names = TRUE)

length(ndvi8dayannual)
ndvi8dayannualstack <- stack(ndvi8dayannual)
ndvi8dayannualstack
ndviannualtrend <- raster.kendall(ndvi8dayannualstack, p.value = TRUE)
plot(ndviannualtrend)


fun_kendall <- function(x){ return(unlist(MannKendall(x)))}
kendall_resultannualndvi <- calc(ndvi8dayannualstack,fun_kendall)
plot(kendall_resultannualndvi)

writeRaster(x=kendall_resultannualndvi$S,filename = file.path("F:/ShareBoth/prewhitening/","trend_ndvi_annualsum.tif"),format="GTiff")
writeRaster(x=kendall_resultannualndvi$sl,filename = file.path("F:/ShareBoth/prewhitening/","pvalue_ndvi_annualsum.tif"),format="GTiff")


################### mosteff_trend_calculation###############

fire8daymosteff <- list.files(path = "F:/ShareBoth/prewhitening/FIRES/mosteffected/5th_thesis_monthsum/rasters/", full.names = TRUE)

length(fire8daymosteff)
fire8daymosteffstack <- stack(fire8daymosteff)
fire8daymosteffstack

firemostefftrend <- raster.kendall(fire8daymosteffstack, p.value = TRUE)
plot(firemostefftrend)


fun_kendall <- function(x){ return(unlist(MannKendall(x)))}
kendall_resultmostefffire <- calc(fire8daymosteffstack,fun_kendall)
plot(kendall_resultmostefffire)

writeRaster(x=kendall_resultmostefffire$S,filename = file.path("F:/ShareBoth/prewhitening/","trend_monthsum_fogtime_fire.tif"),format="GTiff")
writeRaster(x=kendall_resultmostefffire$sl,filename = file.path("F:/ShareBoth/prewhitening/","pvalue_monthsum_fogtime_fire.tif"),format="GTiff")



#-----------------------#

aot8daymosteff <- list.files(path = "F:/ShareBoth/prewhitening/AOT/mosteffected/6th_monthsum_thesis/raster/", full.names = TRUE)

length(aot8daymosteff)
aot8daymosteffstack <- stack(aot8daymosteff)
aot8daymosteffstack
plot(raster.kendall(aot8daymosteffstack, p.value = TRUE))




fun_kendall <- function(x){ return(unlist(MannKendall(x)))}
kendall_resultmosteffaot <- calc(aot8daymosteffstack,fun_kendall)
plot(kendall_resultmosteffaot)

writeRaster(x=kendall_resultmosteffaot$S,filename = file.path("F:/ShareBoth/prewhitening/","trend_aot_monthmosteffsum.tif"),format="GTiff")
writeRaster(x=kendall_resultmosteffaot$sl,filename = file.path("F:/ShareBoth/prewhitening/","pvalue_aot_monthmosteffsum.tif"),format="GTiff")



#-----------------------#

ndvi8daymosteff <- list.files(path = "F:/ShareBoth/prewhitening/NDVI/mosteffected/4th_most_monthsum/rasters/", full.names = TRUE)

length(ndvi8daymosteff)
ndvi8daymosteffstack <- stack(ndvi8daymosteff)
ndvi8daymosteffstack
plot(raster.kendall(ndvi8daymosteffstack, p.value = TRUE))



fun_kendall <- function(x){ return(unlist(MannKendall(x)))}
kendall_resultmosteffndvi <- calc(ndvi8daymosteffstack,fun_kendall)
plot(kendall_resultmosteffndvi)

writeRaster(x=kendall_resultmosteffndvi$S,filename = file.path("F:/ShareBoth/prewhitening/","trend_ndvi_mosteffsum.tif"),format="GTiff")
writeRaster(x=kendall_resultmosteffndvi$sl,filename = file.path("F:/ShareBoth/prewhitening/","pvalue_ndvi_mosteffsum.tif"),format="GTiff")


##################### mapping_mosteffected_tredns####################


trendraster <- list.files(path = "G:/ShareBoth/prewhitening/trendraster/mosteffect/for_paper/cliped/trend/", full.names = TRUE)
vector_path_boundary <- "G:/Thesis_Work/Boundary/Boundary/new/new_external_boundary.shp"
vector_path_points <- "G:/Thesis_Work/Boundary/Boundary/City_points.shp"

Vector_boundary <-  readOGR(dsn = vector_path_boundary,layer = "new_external_boundary")
vector_points <- readOGR(vector_path_points, layer = "City_points")

length(trendraster)
trendrasterstack <- stack(trendraster)
 

colrGP <- colorRampPalette(rev(brewer.pal(11, 'RdYlGn')))
display.brewer.all()

#______________________GGPLOT_____________________#
test_spdf <- as(trendrasterstack$clip_trend_monthmosteff_aot_wthoutlst, "SpatialPixelsDataFrame")
test_df <- as.data.frame(test_spdf)
colnames(test_df) <- c("value", "x", "y")

vector_points_df <- as.data.frame(vector_points)


ggplot() +  
  geom_tile(data=test_df, aes(x=x, y=y, fill=value)) + 
  geom_point(data = vector_points_df,aes(x=coords.x1, y=coords.x2), color="red", size=3)+
  geom_text(data = vector_points_df,aes(x=coords.x1, y=coords.x2), 
            label=vector_points_df$Names, size=5,nudge_y = 1 , vjust=1)+
  geom_polygon(data=Vector_boundary, aes(x=long, y=lat, group=group), 
           fill=NA, color="grey1", size=0.25) +
  scale_fill_gradient(high = "red", low = "blue", guide = "colorbar") +
  #scale_fill_gradient2( "AOT",low = ("blue"), mid = "white",
   #                     high = ("red"), midpoint = -60, space = "Lab",
    #                    guide = "colourbar", aesthetics = "fill")+
  coord_equal() +
  theme_map() #+theme(legend.)

#____________________________________LEVELPLOT________________#
 detach("package:ggplot2", unload=TRUE)

levelplot(trendrasterstack$clip_trend_monthmosteff_aot_wthoutlst)

levelplot(trendrasterstack$clip_trend_monthmosteff_aot_wthoutlst,
          margin=FALSE, 
          #main=list("Kendall Score for AOT (Oct 2012-Feb 2013 to Oct 2018-Feb 2020) ",col="Black",fontface='bold'),
          colorkey=list(
            space='right',                   
            labels=list(at=c(-150,-100,-50,0,50,100), font=5,fontface='bold',cex=1.9),
            axis.line=list(col='black'),
            width=1.9
          ),    
          par.settings=list(
            strip.border=list(col='transparent'),
            strip.background=list(col='transparent'),
            axis.line=list(col='transparent')
          ),
          scales=list(draw=FALSE),
          col.regions=colrGP,                   
          at=seq(-150,100, len=101))+
  layer(sp.polygons(Vector_boundary, lwd=0.8))+
  layer(sp.points(vector_points, pch=21,col="black", fill='red',cex=1.7, alpha=0.9))+
  layer(sp.text(coordinates(vector_points), txt = vector_points$Names, pos = 3,
                col="black",font=list(face="bold"),cex=2.8))

#width 1700 height 1300

levelplot(trendrasterstack$clip_trend_monthsum_mosteff_fire_rast2)
levelplot(trendrasterstack$clip_trend_monthsum_mosteff_fire_rast2,
          margin=FALSE, 
          #main=list("Kendall Score for Fire Events (Oct 2012-Jan 2013 to Oct 2018-Jan 2019) ",col="Black",fontface='bold'),
          colorkey=list(
            space='right',                   
            labels=list(at=c(-150,-100,-50,0,50,100,150,200,250), font=5,fontface='bold',cex=1.9),
            axis.line=list(col='black'),
            width=1.9
          ),    
          par.settings=list(
            strip.border=list(col='transparent'),
            strip.background=list(col='transparent'),
            axis.line=list(col='transparent')
          ),
          scales=list(draw=FALSE),
          col.regions=colrGP,                   
          at=seq(-170,250, len=101))+
  layer(sp.polygons(Vector_boundary, lwd=0.8))+
  layer(sp.points(vector_points, pch=21,col="black", fill='red',cex=1.7, alpha=0.9))+
  layer(sp.text(coordinates(vector_points), txt = vector_points$Names, 
                pos = 3,col="black",font=list(face="bold"),cex=1.8))

colrGP1 <- colorRampPalette((brewer.pal(11, 'RdYlGn')))

levelplot(trendrasterstack$trend_ndvi_mosteffsum)
levelplot(trendrasterstack$trend_ndvi_mosteffsum,
          margin=FALSE, 
          main=list("Kendall Score for Vegetation Index (Oct 2012-Jan 2013 to Oct 2018-Jan 2019) ",col="Black",fontface='bold'),
          colorkey=list(
            space='right',                   
            labels=list(at=c(-150,-100,-50,0,50,100,150,200), font=5,fontface='bold',cex=1.9),
            axis.line=list(col='black'),
            width=1.9
          ),    
          par.settings=list(
            strip.border=list(col='transparent'),
            strip.background=list(col='transparent'),
            axis.line=list(col='transparent')
          ),
          scales=list(draw=FALSE),
          col.regions=colrGP1,                   
          at=seq(-150,200, len=101))+
  layer(sp.polygons(Vector_boundary, lwd=0.8))+
  layer(sp.points(vector_points, pch=21,col="black", fill='red',cex=1.7, alpha=0.9))+
  layer(sp.text(coordinates(vector_points), txt = vector_points$Names, pos = 3,
                col="black",font=list(face="bold"),cex=1.8))


levelplot(trendrasterstack)


######################### maping_monthly_trend#########################

trendrastermonth <- list.files(path = "F:/ShareBoth/prewhitening/trendraster/monthly/monthlysum_ndvi_aot/", full.names = TRUE)

vector_path_boundary <- "F:/Thesis_Work/Boundary/Boundary/new/new_external_boundary.shp"
vector_path_points <- "F:/Thesis_Work/Boundary/Boundary/City_points.shp"

Vector_boundary <-  readOGR(dsn = vector_path_boundary,layer = "new_external_boundary")
vector_points <- readOGR(vector_path_points, layer = "City_points")

length(trendrastermonth)
trendrasterstackmonth <- stack(trendrastermonth)
trendrasterstackmonth
levelplot(trendrasterstackmonth$trend_aot_monthlysum)

colrGP <- colorRampPalette(rev(brewer.pal(11, 'RdYlGn')))


levelplot(trendrasterstackmonth$trend_aot_monthlysum,
          margin=FALSE, 
          main=list("Kendall Score for AOT (March-2012 to November-2019 Monthly)",col="Black",fontface='bold'),
          colorkey=list(
            space='right',                   
            labels=list(at=c(-100,0,100,200,300,400), font=5,fontface='bold',cex=1.9),
            axis.line=list(col='black'),
            width=1.9
          ),    
          par.settings=list(
            strip.border=list(col='transparent'),
            strip.background=list(col='transparent'),
            axis.line=list(col='transparent')
          ),
          scales=list(draw=FALSE),
          col.regions=colrGP,                   
          at=seq(-100,400, len=101))+
  layer(sp.polygons(Vector_boundary, lwd=0.8))+
  layer(sp.points(vector_points, pch=21,col="black", fill='red',cex=1.7, alpha=0.9))+
  layer(sp.text(coordinates(vector_points), txt = vector_points$Names, pos = 3,
                col="black",font=list(face="bold"),cex=1.8))

levelplot(trendrasterstackmonth$proj_trend_monthly_fire)
levelplot(trendrasterstackmonth$proj_trend_monthly_fire,
          margin=FALSE, 
          main=list("Kendall Score for Fire Events (March-2012 to November-2019 Monthly)",col="Black",fontface='bold'),
          colorkey=list(
            space='right',                   
            labels=list(at=c(-250,-200,-100,0,100,200,300,400,500), font=5,fontface='bold',cex=1.8),
            axis.line=list(col='black'),
            width=1.9
          ),    
          par.settings=list(
            strip.border=list(col='transparent'),
            strip.background=list(col='transparent'),
            axis.line=list(col='transparent')
          ),
          scales=list(draw=FALSE),
          col.regions=colrGP,                   
          at=seq(-250,500, len=101))+
  layer(sp.polygons(Vector_boundary, lwd=0.8))+
  layer(sp.points(vector_points, pch=21,col="black", fill='red',cex=1.5, alpha=0.9))+
  layer(sp.text(coordinates(vector_points), txt = vector_points$Names, pos = 3,col="black",font=list(face="bold"),cex=1.5))

colrGP1 <- colorRampPalette((brewer.pal(11, 'RdYlGn')))

levelplot(trendrasterstackmonth$trend_ndvi_monthlysum)
levelplot(trendrasterstackmonth$trend_ndvi_monthlysum,
          margin=FALSE, 
          main=list("Kendall Score for Vegetation Index (March-2012 to November-2019 Monthly)",col="Black",fontface='bold'),
          colorkey=list(
            space='right',                   
            labels=list(at=c(-300,-200,-100,0,100,200,300,400,500,600),font=5,fontface='bold',cex=1.9),
            axis.line=list(col='black'),
            width=1.9
          ),    
          par.settings=list(
            strip.border=list(col='transparent'),
            strip.background=list(col='transparent'),
            axis.line=list(col='transparent')
          ),
          scales=list(draw=FALSE),
          col.regions=colrGP1,                   
          at=seq(-350,600, len=101))+
  layer(sp.polygons(Vector_boundary, lwd=0.8))+
  layer(sp.points(vector_points, pch=21,col="black", fill='red',cex=1.7, alpha=0.9))+
  layer(sp.text(coordinates(vector_points), txt = vector_points$Names, pos = 3,
                col="black",font=list(face="bold"),cex=1.8))

################# mapping_annuaaltrend################

trendrasterannual <- list.files(path = "F:/ShareBoth/prewhitening/trendraster/annual1/annualsum_ndvi_aot/", full.names = TRUE)
vector_path_boundary <- "F:/Thesis_Work/Boundary/Boundary/new/new_external_boundary.shp"
vector_path_points <- "F:/Thesis_Work/Boundary/Boundary/City_points.shp"


Vector_boundary <-  readOGR(dsn = vector_path_boundary,layer = "new_external_boundary")
vector_points <- readOGR(vector_path_points, layer = "City_points")

length(trendrasterannual)
trendrasterstackannual <- stack(trendrasterannual)
trendrasterstackannual
levelplot(trendrasterstackannual$trend_aot_annualsum)

colrGP <- colorRampPalette(rev(brewer.pal(11, 'RdYlGn')))


levelplot(trendrasterstackannual$trend_aot_annualsum,
          margin=FALSE, 
          main=list("Kendall Score for AOT (2012-2019 Annualy)",col="Black",fontface='bold'),
          colorkey=list(
            space='right',                   
            labels=list(at=c(-15,-10,-5,0,5,10,15,20),font=5,fontface='bold',cex=1.9),
            axis.line=list(col='black'),
            width=1.9
          ),    
          par.settings=list(
            strip.border=list(col='transparent'),
            strip.background=list(col='transparent'),
            axis.line=list(col='transparent')
          ),
          scales=list(draw=FALSE),
          col.regions=colrGP,                   
          at=seq(-15,20, len=101))+
  layer(sp.polygons(Vector_boundary, lwd=0.8))+
  layer(sp.points(vector_points, pch=21,col="black", fill='red',cex=1.7, alpha=0.9))+
  layer(sp.text(coordinates(vector_points), txt = vector_points$Names, 
                pos = 3,col="black",font=list(face="bold"),cex=1.8))


levelplot(trendrasterstackannual$annual_fire)
levelplot(trendrasterstackannual$annual_fire,
          margin=FALSE, 
          main=list("Kendall Score for Fire Event (2012-2019 Annualy)",col="Black",fontface='bold'),
          colorkey=list(
            space='right',                   
            labels=list(at=c(-20,-15,-10,-5,0,5,10,15), font=5,fontface='bold',cex=1.8),
            axis.line=list(col='black'),
            width=1.9
          ),    
          par.settings=list(
            strip.border=list(col='transparent'),
            strip.background=list(col='transparent'),
            axis.line=list(col='transparent')
          ),
          scales=list(draw=FALSE),
          col.regions=colrGP,                   
          at=seq(-20,15, len=101))+
  layer(sp.polygons(Vector_boundary, lwd=0.8))+
  layer(sp.points(vector_points, pch=21,col="black", fill='red',cex=1.5, alpha=0.9))+
  layer(sp.text(coordinates(vector_points), txt = vector_points$Names, pos = 3,col="black",font=list(face="bold"),cex=1.5))

colrGP1 <- colorRampPalette((brewer.pal(11, 'RdYlGn')))
levelplot(trendrasterstackannual$trend_ndvi_annualsum)
levelplot(trendrasterstackannual$trend_ndvi_annualsum,
          margin=FALSE, 
          main=list("Kendall Score for Vegetation Index (EVI) (2012-2019 Annualy)",col="Black",fontface='bold'),
          colorkey=list(
            space='right',                   
            labels=list(at=c(-15,-10,-5,0,5,10,15), font=5,fontface='bold',cex=1.9),
            axis.line=list(col='black'),
            width=1.9
          ),    
          par.settings=list(
            strip.border=list(col='transparent'),
            strip.background=list(col='transparent'),
            axis.line=list(col='transparent')
          ),
          scales=list(draw=FALSE),
          col.regions=colrGP1,                   
          at=seq(-15,15, len=101))+
  layer(sp.polygons(Vector_boundary, lwd=0.8))+
  layer(sp.points(vector_points, pch=21,col="black", fill='red',cex=1.7, alpha=0.9))+
  layer(sp.text(coordinates(vector_points), txt = vector_points$Names, 
                pos = 3,col="black",font=list(face="bold"),cex=1.8))



############################ mosteffected corelation R##########################


corelrastermosteff <- list.files(path = "G:/ShareBoth/prewhitening/trendraster/mosteffect/mosteffsum_ndvi_aot/fire/", full.names = TRUE)
vector_path_boundary <- "G:/Thesis_Work/Boundary/Boundary/new/new_external_boundary.shp"
vector_path_points <- "G:/Thesis_Work/Boundary/Boundary/City_points.shp"


Vector_boundary <-  readOGR(dsn = vector_path_boundary,layer = "new_external_boundary")
vector_points <- readOGR(vector_path_points, layer = "City_points")

length(corelrastermosteff)
corelrastermosteff
corelrasterstackmosteff <- stack(corelrastermosteff)
corelrasterstackmosteff


colrGP <- colorRampPalette(rev(brewer.pal(11, 'RdYlGn')))

levelplot(corelrasterstackmosteff)
levelplot(corelrasterstackmosteff$r_aot_mosteffsum)

levelplot(corelrasterstackmosteff$r_aot_mosteffsum,
          margin=FALSE, 
          main=list("Lag1 Autocorrelation Coefficients(r1) for AOT (Oct-jan for 2012-2019) ",col="Black",fontface='bold'),
          colorkey=list(
            space='right',                   
            labels=list(at=c(-0.25,-0.20,-0.15,-0.10,-0.05,0.0,0.05,0.10,0.15,0.20,0.25,0.30,0.35), font=5,fontface='bold',cex=1.9),
            axis.line=list(col='black'),
            width=1.5
          ),    
          par.settings=list(
            strip.border=list(col='transparent'),
            strip.background=list(col='transparent'),
            axis.line=list(col='transparent')
          ),
          scales=list(draw=FALSE),
          col.regions=colrGP,                   
          at=seq(-0.25,0.35, len=101))+
  layer(sp.polygons(Vector_boundary, lwd=0.8))+
  layer(sp.points(vector_points, pch=21,col="black", fill='red',cex=1.7, alpha=0.9))+
  layer(sp.text(coordinates(vector_points), txt = vector_points$Names, pos = 3,
                col="black",font=list(face="bold"),cex=1.8))


levelplot(corelrasterstackmosteff$r_ndvi_mosteffsum)
levelplot(corelrasterstackmosteff$r_ndvi_mosteffsum,
          margin=FALSE, 
          main=list("Lag1 Correlation coefficient for Vegetation Index (Oct-jan for 2012-2019) ",col="Black",fontface='bold'),
          colorkey=list(
            space='right',                   
            labels=list(at=c(-0.40,-0.30,-0.20,-0.10,0.0,0.10,0.20,0.30), font=5,fontface='bold',cex=1.9),
            axis.line=list(col='black'),
            width=1.5
          ),    
          par.settings=list(
            strip.border=list(col='transparent'),
            strip.background=list(col='transparent'),
            axis.line=list(col='transparent')
          ),
          scales=list(draw=FALSE),
          col.regions=colrGP,                   
          at=seq(-0.40,0.30, len=101))+
  layer(sp.polygons(Vector_boundary, lwd=0.8))+
  layer(sp.points(vector_points, pch=21,col="black", fill='red',cex=1.7, alpha=0.9))+
  layer(sp.text(coordinates(vector_points), txt = vector_points$Names, pos = 3,
                col="black",font=list(face="bold"),cex=1.8))


levelplot(corelrasterstackmosteff$r_monthsum_fogtime)
levelplot(corelrasterstackmosteff$r_monthsum_fogtime,
          margin=FALSE, 
          main=list("Lag1 Correlation coefficient for Fire Events  (Oct-jan for 2012-2019)",col="Black",fontface='bold'),
          colorkey=list(
            space='right',                   
            labels=list(at=c(-0.2,-0.1,0.0,0.1,0.2,0.3,0.35), font=5,fontface='bold',cex=1.9),
            axis.line=list(col='black'),
            width=1.5
          ),    
          par.settings=list(
            strip.border=list(col='transparent'),
            strip.background=list(col='transparent'),
            axis.line=list(col='transparent')
          ),
          scales=list(draw=FALSE),
          col.regions=colrGP,                   
          at=seq(-0.2,0.35, len=101))+
  layer(sp.polygons(Vector_boundary, lwd=0.8))+
  layer(sp.points(vector_points, pch=21,col="black", fill='red',cex=1.7, alpha=0.9))+
  layer(sp.text(coordinates(vector_points), txt = vector_points$Names, 
                pos = 3,col="black",font=list(face="bold"),cex=1.8))



############################ monthly corelation R################################

corelrastermonthly <- list.files(path = "F:/ShareBoth/prewhitening/trendraster/monthly/monthlysum_ndvi_aot/", full.names = TRUE)
vector_path_boundary <- "F:/Thesis Work/Boundary/Boundary/new/new_external_boundary.shp"
vector_path_points <- "F:/Thesis Work/Boundary/Boundary/City_points.shp"


Vector_boundary <-  readOGR(dsn = vector_path_boundary,layer = "new_external_boundary")
vector_points <- readOGR(vector_path_points, layer = "City_points")

length(corelrastermonthly)
corelrasterstackmonthly <- stack(corelrastermonthly)
corelrasterstackmonthly


colrGP <- colorRampPalette(rev(brewer.pal(11, 'RdYlGn')))


levelplot(corelrasterstackmonthly$r_aot_monthsum)

levelplot(corelrasterstackmonthly$r_aot_monthsum,
          margin=FALSE, 
          main=list("Lag1 Autocorrelation Coefficients(r1) for AOT (March-2012 to November-2019, Monthly)",col="Black",fontface='bold'),
          colorkey=list(
            space='right',                   
            labels=list(at=c(0.0,0.1,0.2,0.3,0.4,0.5,0.6,0.7), font=5,fontface='bold',cex=1.9),
            axis.line=list(col='black'),
            width=1.5
          ),    
          par.settings=list(
            strip.border=list(col='transparent'),
            strip.background=list(col='transparent'),
            axis.line=list(col='transparent')
          ),
          scales=list(draw=FALSE),
          col.regions=colrGP,                   
          at=seq(0.0,0.65, len=101))+
  layer(sp.polygons(Vector_boundary, lwd=0.8))+
  layer(sp.points(vector_points, pch=21,col="black", fill='red',cex=1.7, alpha=0.9))+
  layer(sp.text(coordinates(vector_points), txt = vector_points$Names, 
                pos = 3,col="black",font=list(face="bold"),cex=1.8))


levelplot(corelrasterstackmonthly$r_ndvi_monthlysum)
levelplot(corelrasterstackmonthly$r_ndvi_monthlysum,
          margin=FALSE, 
          main=list("Lag1 Autocorrelation Coefficients(r1) for Vegatation Index (March-2012 to November-2019, Monthly)",col="Black",fontface='bold'),
          colorkey=list(
            space='right',                   
            labels=list(at=c(0.0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8), font=5,fontface='bold',cex=1.9),
            axis.line=list(col='black'),
            width=1.5
          ),    
          par.settings=list(
            strip.border=list(col='transparent'),
            strip.background=list(col='transparent'),
            axis.line=list(col='transparent')
          ),
          scales=list(draw=FALSE),
          col.regions=colrGP,                   
          at=seq(0.0,0.8, len=101))+
  layer(sp.polygons(Vector_boundary, lwd=0.8))+
  layer(sp.points(vector_points, pch=21,col="black", fill='red',cex=1.7, alpha=0.9))+
  layer(sp.text(coordinates(vector_points), txt = vector_points$Names, pos = 3,
                col="black",font=list(face="bold"),cex=1.8))


levelplot(corelrasterstackmonthly$r_monthly_fire)
levelplot(corelrasterstackmonthly$r_monthly_fire,
          margin=FALSE, 
          main=list("Lag1 Autocorrelation Coefficients(r1) for Fire Events (March-2012 to November-2019)",col="Black",fontface='bold'),
          colorkey=list(
            space='right',                   
            labels=list(at=c(-1.0,-0.8,-0.6,-0.4,-0.2,0.0,0.2,0.4,0.6), font=5,fontface='bold',cex=1.8),
            axis.line=list(col='black'),
            width=1.5
          ),    
          par.settings=list(
            strip.border=list(col='transparent'),
            strip.background=list(col='transparent'),
            axis.line=list(col='transparent')
          ),
          scales=list(draw=FALSE),
          col.regions=colrGP,                   
          at=seq(-1.0,0.6, len=101))+
  layer(sp.polygons(Vector_boundary, lwd=0.5))+
  layer(sp.points(vector_points, pch=21,col="black", fill='red',cex=1.5, alpha=0.9))+
  layer(sp.text(coordinates(vector_points), txt = vector_points$Names, pos = 3,col="black",font=list(face="bold"),cex=1.5))





################ annual corelation####################

corelrasterannual <- list.files(path = "F:/ShareBoth/prewhitening/trendraster/annual1/annualsum_ndvi_aot/", full.names = TRUE)
vector_path_boundary <- "F:/Thesis Work/Boundary/Boundary/new/new_external_boundary.shp"
vector_path_points <- "F:/Thesis Work/Boundary/Boundary/City_points.shp"


Vector_boundary <-  readOGR(dsn = vector_path_boundary,layer = "new_external_boundary")
vector_points <- readOGR(vector_path_points, layer = "City_points")

length(corelrasterannual)
corelrasterstackannual <- stack(corelrasterannual)
corelrasterstackannual


colrGP <- colorRampPalette(rev(brewer.pal(11, 'RdYlGn')))


levelplot(corelrasterstackannual$r_aot_annualsum)

levelplot(corelrasterstackannual$r_aot_annualsum,
          margin=FALSE, 
          main=list("Lag1 Autocorrelation Coefficients(r1) for AOT (2012-2019 Annualy)",col="Black",fontface='bold'),
          colorkey=list(
            space='right',                   
            labels=list(at=c(-0.4,-0.3,-0.2,-0.1,0.0,0.1,0.2,0.3,0.4,0.5,0.6), font=5,fontface='bold',cex=1.9),
            axis.line=list(col='black'),
            width=1.5
          ),    
          par.settings=list(
            strip.border=list(col='transparent'),
            strip.background=list(col='transparent'),
            axis.line=list(col='transparent')
          ),
          scales=list(draw=FALSE),
          col.regions=colrGP,                   
          at=seq(-0.4,0.6, len=101))+
  layer(sp.polygons(Vector_boundary, lwd=0.5))+
  layer(sp.points(vector_points, pch=21,col="black", fill='red',cex=1.7, alpha=0.9))+
  layer(sp.text(coordinates(vector_points), txt = vector_points$Names, pos = 3,
                col="black",font=list(face="bold"),cex=1.8))


levelplot(corelrasterstackannual$r_ndvi_annualsum)
levelplot(corelrasterstackannual$r_ndvi_annualsum,
          margin=FALSE, 
          main=list("Lag1 Autocorrelation Coefficients(r1) for Vegatation Index (2012-2019 Annualy)",col="Black",fontface='bold'),
          colorkey=list(
            space='right',              
            labels=list(at=c(-1.0,-0.8,-0.6,-0.4,-0.2,0.0,0.2,0.4,0.6,0.8), font=5,fontface='bold',cex=1.9),
            axis.line=list(col='black'),
            width=1.5
          ),    
          par.settings=list(
            strip.border=list(col='transparent'),
            strip.background=list(col='transparent'),
            axis.line=list(col='transparent')
          ),
          scales=list(draw=FALSE),
          col.regions=colrGP,                   
          at=seq(-1.0,0.8, len=101))+
  layer(sp.polygons(Vector_boundary, lwd=0.5))+
  layer(sp.points(vector_points, pch=21,col="black", fill='red',cex=1.7, alpha=0.9))+
  layer(sp.text(coordinates(vector_points), txt = vector_points$Names, 
                pos = 3,col="black",font=list(face="bold"),cex=1.8))


levelplot(corelrasterstackannual$R3_annual_fire)
levelplot(corelrasterstackannual$R3_annual_fire,
          margin=FALSE, 
          main=list("Lag1 Autocorrelation Coefficients(r1) for Fire Events (2012-2019 Annualy)",col="Black",fontface='bold'),
          colorkey=list(
            space='right',                   
            labels=list(at=c(-1.0,-0.8,-0.6,-0.4,-0.2,0.0,0.2,0.4,0.6,0.8,1.0), font=5,fontface='bold',cex=1.8),
            axis.line=list(col='black'),
            width=1.5
          ),    
          par.settings=list(
            strip.border=list(col='transparent'),
            strip.background=list(col='transparent'),
            axis.line=list(col='transparent')
          ),
          scales=list(draw=FALSE),
          col.regions=colrGP,                   
          at=seq(-1.0,1.0, len=101))+
  layer(sp.polygons(Vector_boundary, lwd=0.5))+
  layer(sp.points(vector_points, pch=21,col="black", fill='red',cex=1.5, alpha=0.9))+
  layer(sp.text(coordinates(vector_points), txt = vector_points$Names, pos = 3,col="black",font=list(face="bold"),cex=1.5))



################# mapping_annuaal_pvalue################

pvaluerasterannual <- list.files(path = "F:/ShareBoth/prewhitening/trendraster/annual1/annualsum_ndvi_aot/", full.names = TRUE)
vector_path_boundary <- "F:/Thesis_Work/Boundary/Boundary/new/new_external_boundary.shp"
vector_path_points <- "F:/Thesis_Work/Boundary/Boundary/City_points.shp"


Vector_boundary <-  readOGR(dsn = vector_path_boundary,layer = "new_external_boundary")
vector_points <- readOGR(vector_path_points, layer = "City_points")

length(pvaluerasterannual)
pvaluerasterstackannual <- stack(pvaluerasterannual)
pvaluerasterstackannual
levelplot(pvaluerasterstackannual)

colrGP <- colorRampPalette(rev(brewer.pal(11, 'RdYlGn')))

levelplot(pvaluerasterstackannual$pvalue_aot_annualsum)

levelplot(pvaluerasterstackannual$pvalue_aot_annualsum,
          margin=FALSE, 
          main=list("P-value for AOT (2012-2019 Annualy)",col="Black",fontface='bold'),
          colorkey=list(
            space='right',                   
            labels=list(at=c(0.0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8), font=5,fontface='bold',cex=1.9),
            axis.line=list(col='black'),
            width=1.5
          ),    
          par.settings=list(
            strip.border=list(col='transparent'),
            strip.background=list(col='transparent'),
            axis.line=list(col='transparent')
          ),
          scales=list(draw=FALSE),
          col.regions=colrGP,                   
          at=seq(0.0,0.8, len=101))+
  layer(sp.polygons(Vector_boundary, lwd=0.5))+
  layer(sp.points(vector_points, pch=21,col="black", fill='red',cex=1.7, alpha=0.9))+
  layer(sp.text(coordinates(vector_points), txt = vector_points$Names, 
                pos = 3,col="black",font=list(face="bold"),cex=1.8))


levelplot(pvaluerasterstackannual$proj_pvalue_annual_fire_rast)
levelplot(pvaluerasterstackannual$proj_pvalue_annual_fire_rast,
          margin=FALSE, 
          main=list("P-Value for Fire Event (2012-2019 Annualy)",col="Black",fontface='bold'),
          colorkey=list(
            space='right',                   
            labels=list(at=c(0.0,0.2,0.4,0.6,0.8,1.0), font=5,fontface='bold',cex=1.8),
            axis.line=list(col='black'),
            width=1.5
          ),    
          par.settings=list(
            strip.border=list(col='transparent'),
            strip.background=list(col='transparent'),
            axis.line=list(col='transparent')
          ),
          scales=list(draw=FALSE),
          col.regions=colrGP,                   
          at=seq(0.0,1.0, len=101))+
  layer(sp.polygons(Vector_boundary, lwd=0.5))+
  layer(sp.points(vector_points, pch=21,col="black", fill='red',cex=1.5, alpha=0.9))+
  layer(sp.text(coordinates(vector_points), txt = vector_points$Names, pos = 3,col="black",font=list(face="bold"),cex=1.5))


levelplot(pvaluerasterstackannual$pvalue_ndvi_annualsum)
levelplot(pvaluerasterstackannual$pvalue_ndvi_annualsum,
          margin=FALSE, 
          main=list("P-Vlaue for Vegetation Index (EVI) (2012-2019 Annualy)",col="Black",fontface='bold'),
          colorkey=list(
            space='right',                   
            labels=list(at=c(0.0,0.2,0.4,0.6,0.8,1.0),  font=5,fontface='bold',cex=1.9),
            axis.line=list(col='black'),
            width=1.5
          ),    
          par.settings=list(
            strip.border=list(col='transparent'),
            strip.background=list(col='transparent'),
            axis.line=list(col='transparent')
          ),
          scales=list(draw=FALSE),
          col.regions=colrGP,                   
          at=seq(0.0,1.0, len=101))+
  layer(sp.polygons(Vector_boundary, lwd=0.5))+
  layer(sp.points(vector_points, pch=21,col="black", fill='red',cex=1.7, alpha=0.9))+
  layer(sp.text(coordinates(vector_points), txt = vector_points$Names,
                pos = 3,col="black",font=list(face="bold"),cex=1.8))


################# mapping_monthly_pvalue################

pvaluerastermonthly <- list.files(path = "F:/ShareBoth/prewhitening/trendraster/monthly/monthlysum_ndvi_aot/", full.names = TRUE)
vector_path_boundary <- "F:/Thesis Work/Boundary/Boundary/new/new_external_boundary.shp"
vector_path_points <- "F:/Thesis Work/Boundary/Boundary/City_points.shp"


Vector_boundary <-  readOGR(dsn = vector_path_boundary,layer = "new_external_boundary")
vector_points <- readOGR(vector_path_points, layer = "City_points")

length(pvaluerastermonthly)
pvaluerasterstackmonthly <- stack(pvaluerastermonthly)
pvaluerasterstackmonthly
levelplot(pvaluerasterstackmonthly)

colrGP <- colorRampPalette(rev(brewer.pal(11, 'RdYlGn')))

levelplot(pvaluerasterstackmonthly$pvalue_aot_monthlysum)

levelplot(pvaluerasterstackmonthly$pvalue_aot_monthlysum,
          margin=FALSE, 
          main=list("P-value for AOT (March-2012 to November-2019 Monthly)",col="Black",fontface='bold'),
          colorkey=list(
            space='right',                   
            labels=list(at=c(0.0,0.2,0.4,0.6,0.8,1.0), font=5,fontface='bold',cex=1.9),
            axis.line=list(col='black'),
            width=1.5
          ),    
          par.settings=list(
            strip.border=list(col='transparent'),
            strip.background=list(col='transparent'),
            axis.line=list(col='transparent')
          ),
          scales=list(draw=FALSE),
          col.regions=colrGP,                   
          at=seq(0.0,1.0, len=101))+
  layer(sp.polygons(Vector_boundary, lwd=0.5))+
  layer(sp.points(vector_points, pch=21,col="black", fill='red',cex=1.7, alpha=0.9))+
  layer(sp.text(coordinates(vector_points), txt = vector_points$Names, 
                pos = 3,col="black",font=list(face="bold"),cex=1.8))

levelplot(pvaluerasterstackmonthly$pvalue_ndvi_monthlysum)

levelplot(pvaluerasterstackmonthly$pvalue_ndvi_monthlysum,
          margin=FALSE, 
          main=list("P-value for EVI (March-2012 to November-2019 Monthly)",col="Black",fontface='bold'),
          colorkey=list(
            space='right',                   
            labels=list(at=c(0.0,0.2,0.4,0.6,0.8,1.0), font=5,fontface='bold',cex=1.9),
            axis.line=list(col='black'),
            width=1.5
          ),    
          par.settings=list(
            strip.border=list(col='transparent'),
            strip.background=list(col='transparent'),
            axis.line=list(col='transparent')
          ),
          scales=list(draw=FALSE),
          col.regions=colrGP,                   
          at=seq(0.0,1.0, len=101))+
  layer(sp.polygons(Vector_boundary, lwd=0.5))+
  layer(sp.points(vector_points, pch=21,col="black", fill='red',cex=1.7, alpha=0.9))+
  layer(sp.text(coordinates(vector_points), txt = vector_points$Names, pos = 3,
                col="black",font=list(face="bold"),cex=1.8))


levelplot(pvaluerasterstackmonthly$proj_pvalue_fire_monthly,
          margin=FALSE, 
          main=list("P-value for Fire Events (March-2012 to November-2019 Monthly)",col="Black",fontface='bold'),
          colorkey=list(
            space='right',                   
            labels=list(at=c(0.0,0.2,0.4,0.6,0.8,1.0), font=5,fontface='bold',cex=1.8),
            axis.line=list(col='black'),
            width=1.5
          ),    
          par.settings=list(
            strip.border=list(col='transparent'),
            strip.background=list(col='transparent'),
            axis.line=list(col='transparent')
          ),
          scales=list(draw=FALSE),
          col.regions=colrGP,                   
          at=seq(0.0,1.0, len=101))+
  layer(sp.polygons(Vector_boundary, lwd=0.5))+
  layer(sp.points(vector_points, pch=21,col="black", fill='red',cex=1.5, alpha=0.9))+
  layer(sp.text(coordinates(vector_points), txt = vector_points$Names, pos = 3,col="black",font=list(face="bold"),cex=1.5))



################# mapping_mosteff_pvalue################

pvaluerastermosteff <- list.files(path = "F:/ShareBoth/prewhitening/trendraster/mosteffect/mosteffsum_ndvi_aot/fire/", full.names = TRUE)
vector_path_boundary <- "F:/Thesis_Work/Boundary/Boundary/new/new_external_boundary.shp"
vector_path_points <- "F:/Thesis_Work/Boundary/Boundary/City_points.shp"


Vector_boundary <-  readOGR(dsn = vector_path_boundary,layer = "new_external_boundary")
vector_points <- readOGR(vector_path_points, layer = "City_points")

length(pvaluerastermosteff)
pvaluerasterstackmosteff <- stack(pvaluerastermosteff)
pvaluerasterstackmosteff
levelplot(pvaluerasterstackmosteff)

colrGP <- colorRampPalette(rev(brewer.pal(11, 'RdYlGn')))

levelplot(pvaluerasterstackmosteff$pvalue_aot_monthmosteffsum)

levelplot(pvaluerasterstackmosteff$pvalue_aot_monthmosteffsum,
          margin=FALSE, 
          main=list("P-value for AOT (Oct-jan for 2012-2019)",col="Black",fontface='bold'),
          colorkey=list(
            space='right',                   
            labels=list(at=c(0.0,0.2,0.4,0.6,0.8,1.0), font=5,fontface='bold',cex=1.9),
            axis.line=list(col='black'),
            width=1.5
          ),    
          par.settings=list(
            strip.border=list(col='transparent'),
            strip.background=list(col='transparent'),
            axis.line=list(col='transparent')
          ),
          scales=list(draw=FALSE),
          col.regions=colrGP,                   
          at=seq(0.0,1.0, len=101))+
  layer(sp.polygons(Vector_boundary, lwd=0.5))+
  layer(sp.points(vector_points, pch=21,col="black", fill='red',cex=1.7, alpha=0.9))+
  layer(sp.text(coordinates(vector_points), txt = vector_points$Names, pos = 3,
                col="black",font=list(face="bold"),cex=1.8))


  levelplot(pvaluerasterstackmosteff$pvalue_monthsum_fogtime_fire)
levelplot(pvaluerasterstackmosteff$pvalue_monthsum_fogtime_fire,
          margin=FALSE, 
          main=list("P-Value for Fire Event (Oct-jan for 2012-2019)",col="Black",fontface='bold'),
          colorkey=list(
            space='right',                   
            labels=list(at=c(0.0,0.2,0.4,0.6,0.8,1.0), font=5,fontface='bold',cex=1.9),
            axis.line=list(col='black'),
            width=1.5
          ),    
          par.settings=list(
            strip.border=list(col='transparent'),
            strip.background=list(col='transparent'),
            axis.line=list(col='transparent')
          ),
          scales=list(draw=FALSE),
          col.regions=colrGP,                   
          at=seq(0.0,1.0, len=101))+
  layer(sp.polygons(Vector_boundary, lwd=0.5))+
  layer(sp.points(vector_points, pch=21,col="black", fill='red',cex=1.7, alpha=0.9))+
  layer(sp.text(coordinates(vector_points), txt = vector_points$Names, 
                pos = 3,col="black",font=list(face="bold"),cex=1.8))


levelplot(pvaluerasterstackmosteff$pvalue_ndvi_mosteffsum)
levelplot(pvaluerasterstackmosteff$pvalue_ndvi_mosteffsum,
          margin=FALSE, 
          main=list("P-Vlaue for Vegetation Index (EVI) (Oct-jan for 2012-2019)",col="Black",fontface='bold'),
          colorkey=list(
            space='right',                   
            labels=list(at=c(0.0,0.2,0.4,0.6,0.8,1.0), font=8,fontface='bold',cex=1.9),
            axis.line=list(col='black'),
            width=1.5
          ),    
          par.settings=list(
            strip.border=list(col='transparent'),
            strip.background=list(col='transparent'),
            axis.line=list(col='transparent')
          ),
          scales=list(draw=FALSE),
          col.regions=colrGP,                   
          at=seq(0.0,1.0, len=101))+
  layer(sp.polygons(Vector_boundary, lwd=0.5))+
  layer(sp.points(vector_points, pch=21,col="black", fill='red',cex=1.7, alpha=0.9))+
  layer(sp.text(coordinates(vector_points), txt = vector_points$Names, pos = 3,
                col="black",font=list(face="bold"),cex=1.8))



############################ overall_pvalue####
pvaluerasteroverall <- list.files(path = "F:/ShareBoth/prewhitening/trendraster/Pvalue/projected/", full.names = TRUE)
vector_path_boundary <- "F:/Thesis Work/Boundary/Boundary/new/new_external_boundary.shp"
vector_path_points <- "F:/Thesis Work/Boundary/Boundary/City_points.shp"

Vector_boundary <-  readOGR(dsn = vector_path_boundary,layer = "new_external_boundary")
vector_points <- readOGR(vector_path_points, layer = "City_points")

length(pvaluerasteroverall)
pvaluerasterstackoverall <- stack(pvaluerasteroverall)
pvaluerasterstackoverall
levelplot(pvaluerasterstackmosteff)

colrGP <- colorRampPalette(rev(brewer.pal(11, 'RdYlGn')))

levelplot(pvaluerasterstackoverall)
#main=list("P-value",col="Black",fontface='bold'),
levelplot(pvaluerasterstackoverall,
          margin=FALSE, 
          colorkey=list(
            space='right',                   
            labels=list(at=c(0.0,0.2,0.4,0.6,0.8,1.0), font=5,fontface='bold'),
            axis.line=list(col='black'),
            width=0.75
          ),    
          par.settings=list(
            strip.border=list(col='transparent'),
            strip.background=list(col='transparent'),
            axis.line=list(col='transparent')
          ),
          par.strip.text=list(cex=1, lines=2, fontface='bold'),
          scales=list(draw=FALSE),
          col.regions=colrGP,                   
          at=seq(0.0,1.0, len=101),
          names.attr=(c('AOD_Annual','EVI_Annual','FireEvent_Annual'
                        ,'AOD_Monthly','EVI_Monthly','FireEvent_Monthly'
                        ,'AOD_Mosteff','EVI_Mosteff','FireEvent_Mosteff')))+
  layer(sp.polygons(Vector_boundary, lwd=0.5))+
  layer(sp.points(vector_points, pch=21,col="black", fill='red',cex=0.95, alpha=0.7))+
  layer(sp.text(coordinates(vector_points), txt = vector_points$Names, pos = 1,col="black",font=list(face="bold"),cex=1))

  

########################## overall_oct_feb################

pvaluerasteroverall <- list.files(path = "F:/ShareBoth/prewhitening/trendraster/mosteffect/projected/pvalue/", full.names = TRUE)
vector_path_boundary <- "F:/Thesis Work/Boundary/Boundary/new/new_external_boundary.shp"
vector_path_points <- "F:/Thesis Work/Boundary/Boundary/City_points.shp"

Vector_boundary <-  readOGR(dsn = vector_path_boundary,layer = "new_external_boundary")
vector_points <- readOGR(vector_path_points, layer = "City_points")

length(pvaluerasteroverall)
pvaluerasterstackoverall <- stack(pvaluerasteroverall)
pvaluerasterstackoverall
levelplot(pvaluerasterstackmosteff)

colrGP <- colorRampPalette(rev(brewer.pal(11, 'RdYlGn')))

levelplot(pvaluerasterstackoverall)
#main=list("P-value",col="Black",fontface='bold'),
levelplot(pvaluerasterstackoverall,layout=c(3, 1),
          margin=FALSE, 
          colorkey=list(
            space='right',                   
            labels=list(at=c(0.0,0.2,0.4,0.6,0.8,1.0), font=5,fontface='bold'),
            axis.line=list(col='black'),
            width=0.75
          ),    
          par.settings=list(
            strip.border=list(col='transparent'),
            strip.background=list(col='transparent'),
            axis.line=list(col='transparent')
          ),
          par.strip.text=list(cex=1, lines=1, fontface='bold'),
          scales=list(draw=FALSE),
          col.regions=colrGP,                   
          at=seq(0.0,1.0, len=101),
          names.attr=(c('AOT','Fire Events','EVI')))+
  layer(sp.polygons(Vector_boundary, lwd=0.5))+
  layer(sp.points(vector_points, pch=21,col="black", fill='red',cex=0.95, alpha=0.7))+
  layer(sp.text(coordinates(vector_points), txt = vector_points$Names, pos = 1,col="black",font=list(face="bold"),cex=1))







####################NDVI pre and post mapping ############


NdviPrePost <- list.files(path = "G:/ShareBoth/1st_paper_edition/NDVI_proba/prepost/", full.names = TRUE)
Ndvicore <- list.files(path = "G:/ShareBoth/1st_paper_edition/NDVI_proba/corelation/corelation_raster/raster/", full.names = TRUE)
DiffMap <- list.files(path = "G:/ShareBoth/1st_paper_edition/NDVI_proba/Diff/",full.names = TRUE)

vector_path_boundary <- "G:/Thesis_Work/Boundary/Boundary/new/new_external_boundary.shp"
vector_path_points <- "G:/Thesis_Work/Boundary/Boundary/City_points.shp"
vector_corelation <- "G:/ShareBoth/1st_paper_edition/NDVI_proba/corelation/corelation_point_shapefile/coleration.shp"

Vector_boundary <-  readOGR(dsn = vector_path_boundary,layer = "new_external_boundary")
vector_points <- readOGR(vector_path_points, layer = "City_points")
vector_corelation_ <- readOGR(vector_corelation, layer = "coleration")


length(Ndvicore)
length(NdviPrePost)
length(DiffMap)

NdviPrePoststack <- stack(NdviPrePost)
NdviFireCorstack <- stack(Ndvicore)
DiffMapstack <- stack(DiffMap)

colrGP <- colorRampPalette((brewer.pal(7, 'RdYlBu')))


#levelplot(NdviPrePoststack$ All_proba_postharvest_clip)
levelplot(NdviPrePoststack$X1All_proba_preharvest_Clip_proj,
        margin=FALSE, 
        main=list('NDVI (Pre Harvest)',col="Black",fontface='bold'),
        colorkey=list(
        space='right',                   
        labels=list(at=c("%-0.1%",0.0,0.2,0.4,0.6,0.8),font=5,fontface='bold',cex=1.3),
        axis.line=list(col='black'),
        width=1.3
          ),    
        par.settings=list(
          strip.border=list(col='transparent'),
          strip.background=list(col='transparent'),
          axis.line=list(col='transparent')
          ),
          xlab=NULL,
          ylab=NULL,
          par.strip.text=list(cex=1, lines=1, fontface='bold'),
          scales=list(draw=FALSE),
          col.regions=colrGP,                   
          at=seq(-0.1,0.8, len=101))
          #names.attr=(c('NDVI (Pre Harvest)','NDVI (Post Harvest)')))+
    layer(sp.polygons(Vector_boundary, lwd=0.5))+
    layer(sp.points(vector_points, pch=21,col="black", fill='red',cex=0.95, alpha=0.7))+
    layer(sp.text(coordinates(vector_points), txt = vector_points$Names, pos = 1,col="black",font=list(face="bold"),cex=1))

levelplot(NdviPrePoststack$X2All_proba_postharvest_clip_proj,
               margin=FALSE, 
               main=list('NDVI (Post Harvest)',col="Black",fontface='bold'),
               colorkey=list(
                 space='right',                   
                 labels=list(at=signs_format((0.1),0.0,0.2,0.4,0.6,0.8),font=5,fontface='bold',cex=1.3),
                 axis.line=list(col='black'),
                 width=1.3
               ),    
               par.settings=list(
                 strip.border=list(col='transparent'),
                 strip.background=list(col='transparent'),
                 axis.line=list(col='transparent')
               ),
               xlab=NULL,
               ylab=NULL,
               par.strip.text=list(cex=1, lines=1, fontface='bold'),
               scales=list(draw=FALSE),
               col.regions=colrGP,                   
               at=seq(-0.1,0.8, len=101))+
               #names.attr=(c('NDVI (Pre Harvest)','NDVI (Post Harvest)')))+
  layer(sp.polygons(Vector_boundary, lwd=0.5))+
  layer(sp.points(vector_points, pch=21,col="black", fill='red',cex=0.95, alpha=0.7))+
  layer(sp.text(coordinates(vector_points), txt = vector_points$Names, pos = 1,col="black",font=list(face="bold"),cex=1))

#__________________Diff Map_____________#
display.brewer.all()
colrGPc <- colorRampPalette(rev(brewer.pal(11, 'RdYlBu')))
levelplot(DiffMapstack,
          main=list("Difference (Pre Harvest-Post Harvest)",col="Black",fontface='bold'),
          margin=FALSE, 
          colorkey=list(
            space='right',                   
            labels=list(at=c((-0.8),(-0.6),(-0.4),(-0.2),0.0,0.2,0.4,0.6),font=5,fontface='bold',cex=1.3),
            axis.line=list(col='black'),
            width=1.3
          ),    
          par.settings=list(
            strip.border=list(col='transparent'),
            strip.background=list(col='transparent'),
            axis.line=list(col='transparent')
          ),
          xlab=NULL,
          ylab=NULL,
          par.strip.text=list(cex=1, lines=1, fontface='bold'),
          scales=list(draw=FALSE),
          col.regions=colrGPc,                   
          at=seq(-0.8,0.6, len=101),
          )+
  layer(sp.polygons(Vector_boundary, lwd=0.5))+
  layer(sp.points(vector_points, pch=21,col="black", fill='red',cex=0.95, alpha=0.7))+
  layer(sp.text(coordinates(vector_points), txt = vector_points$Names, pos = 1,col="black",font=list(face="bold"),cex=1))
  
#______________________CORELATION___________#
colrGPd <- colorRampPalette(rev(brewer.pal(7, 'RdGn')))
#levelplot(NdviFireCor)
 rasterVis::levelplot(NdviFireCorstack,
                     margin=FALSE, 
                     main=list("Correlation (NDVI and Fire Event)",col="Black",fontface='bold'),
                     colorkey=list(space='right',                   
                    labels=list(at=c((-1.0),(-0.6),-0.2,0.2,0.6,1.0), font=5,fontface='bold',cex=1.3),
                    axis.line=list(col='black'),
                    width=1.3
                    ),    
                    par.settings=list(
                      strip.border=list(col='transparent'),
                      strip.background=list(col='transparent'),
                      axis.line=list(col='transparent')
                    ),
                    scales=list(draw=FALSE),
                      col.regions=colrGPc,                   
                      at=seq(-1.0,1.0, len=101))+
  latticeExtra::layer(sp.polygons(Vector_boundary, lwd=2, col = "grey40"))+
  latticeExtra::layer(sp.text(coordinates(vector_corelation_),txt = vector_corelation_$grid_code,col="black",font=list(face="bold"),lwd=4 ))



#plots <- align_plots(a, c, align = 'v', axis = 'l')
#bottom_row <- plot_grid(plots[[2]], d, labels = c('B', 'C'), label_size = 12)
#plot_grid(plots[[1]], bottom_row, labels = c('A', ''), label_size = 12, ncol = 1)


plot_grid(a,b,c,d, lebel_size=12, labels = "AUTO", ncol = 2, nrow = 2)

####################### P+R (for AOT trend paper) ###################

trendraster_P_R <- list.files(path = "G:/ShareBoth/prewhitening/trendraster/mosteffect/for_paper/cliped/P+R/AOT/", full.names = TRUE)
trendrasterFire_P_R <- list.files(path = "G:/ShareBoth/prewhitening/trendraster/mosteffect/for_paper/cliped/P+R/Fire/", full.names = TRUE)

vector_path_boundary <- "G:/Thesis_Work/Boundary/Boundary/new/new_external_boundary.shp"
vector_path_points <- "G:/Thesis_Work/Boundary/Boundary/City_points.shp"

Vector_boundary <-  readOGR(dsn = vector_path_boundary,layer = "new_external_boundary")
vector_points <- readOGR(vector_path_points, layer = "City_points")

length(trendraster_P_R)
length(trendrasterFire_P_R)

trendrasterstack_P_R <- stack(trendraster_P_R)
trendrasterstackFire_P_R <- stack(trendrasterFire_P_R)

trendrasterstack_P_R
trendrasterstackFire_P_R

colrGP <- colorRampPalette(rev(brewer.pal(11, 'RdYlGn')))
display.brewer.all()
levelplot(trendrasterstack_P_R$clip_R_aot_monthsum_corcoef,col.regions=colrGP)
levelplot(trendrasterstack_P_R$clip_R_aot_monthsum_corcoef,
          margin=FALSE, 
          #main=list("Auto-Correlation Coefficient (r) for AOT",cex=1.5,col="Black",fontface='bold'),
          colorkey=list(
            space='right',                   
            labels=list(at=c(-0.25,-0.20,-0.15,-0.10,-0.05,0.0,0.05,0.10,0.15,0.20,0.25), font=5,fontface='bold',cex=1.9),
            axis.line=list(col='black'),
            width=1.9
          ),    
          par.settings=list(
            strip.border=list(col='transparent'),
            strip.background=list(col='transparent'),
            axis.line=list(col='transparent')
          
          ),
          scales=list(draw=FALSE),
          col.regions=colrGP,                   
          at=seq(-0.25,0.25, len=101))+
  layer(sp.polygons(Vector_boundary, lwd=0.8))+
  layer(sp.points(vector_points, pch=21,col="black", fill='red',cex=1.7, alpha=0.9))+
  layer(sp.text(coordinates(vector_points), txt = vector_points$Names, pos = 3,
                col="black",font=list(face="bold"),cex=2.8))

levelplot(trendrasterstack_P_R$clip_pvalue_monthmosteff_aot_wthoutlst)
levelplot(trendrasterstack_P_R$clip_pvalue_monthmosteff_aot_wthoutlst,
          margin=FALSE, 
          #main=list("P-value for AOT",col="Black",cex=1.5,fontface='bold'),
          colorkey=list(
            space='right',                   
            labels=list(at=c(0.0,0.2,0.4,0.6,0.8,1.0), font=5,fontface='bold',cex=1.9),
            axis.line=list(col='black'),
            width=1.9
          ),    
          par.settings=list(
            strip.border=list(col='transparent'),
            strip.background=list(col='transparent'),
            axis.line=list(col='transparent')
            
          ),
          scales=list(draw=FALSE),
          col.regions=colrGP,                   
          at=seq(0.11,1.0, len=101))+
  layer(sp.polygons(Vector_boundary, lwd=0.8))+
  layer(sp.points(vector_points, pch=21,col="black", fill='red',cex=1.7, alpha=0.9))+
  layer(sp.text(coordinates(vector_points), txt = vector_points$Names, pos = 3,
                col="black",font=list(face="bold"),cex=2.8))
cowplot::plot_grid(a,b, labels = "AUTO")

#______________Fire _____________#


levelplot(trendrasterstackFire_P_R$clip_R_fire_monthsum_corcoef)
a <- levelplot(trendrasterstackFire_P_R$clip_R_fire_monthsum_corcoef,
               margin=FALSE, 
               main=list("Auto-Correlation Coefficient (r) for Fire",cex=1.5,col="Black",fontface='bold'),
               colorkey=list(
                 space='right',                   
                 labels=list(at=c(-1.,-0.8,-0.6,-0.4,-0.2,0.0,0.2,0.4,0.58), font=5,fontface='bold',cex=1.9),
                 axis.line=list(col='black'),
                 width=1.9
               ),    
               par.settings=list(
                 strip.border=list(col='transparent'),
                 strip.background=list(col='transparent'),
                 axis.line=list(col='transparent')
                 
               ),
               scales=list(draw=FALSE),
               col.regions=colrGP,                   
               at=seq(-1.2,0.7, len=101))+
  layer(sp.polygons(Vector_boundary, lwd=0.8))+
  layer(sp.points(vector_points, pch=21,col="black", fill='red',cex=1.7, alpha=0.9))+
  layer(sp.text(coordinates(vector_points), txt = vector_points$Names, pos = 3,
                col="black",font=list(face="bold"),cex=1.5))
a
levelplot(trendrasterstackFire_P_R$clip_pvalue_monthsum_mosteff_fire_rast)
b <- levelplot(trendrasterstackFire_P_R$clip_pvalue_monthsum_mosteff_fire_rast,
               margin=FALSE, 
               main=list("P-value for Fire",col="Black",cex=1.5,fontface='bold'),
               colorkey=list(
                 space='right',                   
                 labels=list(at=c(0.0,0.2,0.4,0.6,0.8,1.0), font=5,fontface='bold',cex=1.9),
                 axis.line=list(col='black'),
                 width=1.9
               ),    
               par.settings=list(
                 strip.border=list(col='transparent'),
                 strip.background=list(col='transparent'),
                 axis.line=list(col='transparent')
                 
               ),
               scales=list(draw=FALSE),
               col.regions=colrGP,                   
               at=seq(-0.1,1.0, len=101))+
  layer(sp.polygons(Vector_boundary, lwd=0.8))+
  layer(sp.points(vector_points, pch=21,col="black", fill='red',cex=1.7, alpha=0.9))+
  layer(sp.text(coordinates(vector_points), txt = vector_points$Names, pos = 3,
                col="black",font=list(face="bold"),cex=1.5))
b
cowplot::plot_grid(a,b, labels = "AUTO")

##############################Trend calculation MODIS NDVI Dr. Salman##################

KharifList <- list.files(path = "G:/ShareBoth/MODIS_PUNJAB/kharif/prewhiting/prewhitnend/", full.names = TRUE)

length(KharifList)
KharifListStack <- stack(KharifList)
KharifListStack
plot (raster.kendall(KharifListStack,p.value = TRUE))

fun_kendall <- function(x){ return(unlist(MannKendall(x)))}


kendall_resultAOTmonthly <- calc(KharifListStack,fun_kendall)
plot(kendall_resultAOTmonthly)
writeRaster(x=kendall_resultAOTmonthly$S,filename = file.path("G:/ShareBoth/MODIS_PUNJAB/kharif/prewhiting/","trend_Kharif_NDVI.tif"),format="GTiff")
writeRaster(x=kendall_resultAOTmonthly$sl,filename = file.path("G:/ShareBoth/MODIS_PUNJAB/kharif/prewhiting/","pvalue_Kharif_NDVI.tif"),format="GTiff")

#____________________________RABI_________________#

RabiList <- list.files(path = "G:/ShareBoth/MODIS_PUNJAB/Rabi/prewhiting/prewitnend/", full.names = TRUE)

length(RabiList)
RabbiListStack <- stack(RabiList)
RabbiListStack
plot (raster.kendall(KharifListStack,p.value = TRUE))

fun_kendall <- function(x){ return(unlist(MannKendall(x)))}


kendall_resultRabi <- calc(RabbiListStack,fun_kendall)
plot(kendall_resultRabi)
writeRaster(x=kendall_resultRabi$S,filename = file.path("G:/ShareBoth/MODIS_PUNJAB/Rabi/prewhiting/","trend_Rabi_NDVI.tif"),format="GTiff")
writeRaster(x=kendall_resultRabi$sl,filename = file.path("G:/ShareBoth/MODIS_PUNJAB/Rabi/prewhiting/","pvalue_Rabi_NDVI.tif"),format="GTiff")



######################### Mapping Trend MODIS NDVI Dr.Salman ###################


kharif_PRC <- list.files(path = "G:/ShareBoth/MODIS_PUNJAB/kharif_trend/New Folder/", full.names = TRUE)
rabi_PRC <- list.files(path = "G:/ShareBoth/MODIS_PUNJAB/rabi_trend/New Folder/", full.names = TRUE)

vector_path_boundary <- "G:/Other_scholar/Sir_salman/RIvver_Google_earth_engine/Punjab_project_.shp"
vector_path_points <- "G:/Other_scholar/Sir_salman/RIvver_Google_earth_engine/Punjab_point_city.shp"

Vector_boundary <-  readOGR(dsn = vector_path_boundary,layer = "Punjab_project_")
vector_points <- readOGR(vector_path_points, layer = "Punjab_point_city")

length(kharif_PRC)
length(rabi_PRC)

kharifStack_PRC <- stack(kharif_PRC)
rabiStack_PRC <- stack(rabi_PRC)

kharifStack_PRC
rabiStack_PRC

colrGP <- colorRampPalette(rev(brewer.pal(11, 'RdYlGn')))
display.brewer.all()
levelplot(kharifStack_PRC$trend_Kharif_NDVI,col.regions=colrGP)
levelplot(kharifStack_PRC$trend_kharif,
          margin=FALSE, 
          #main=list("Auto-Correlation Coefficient (r) for AOT",cex=1.5,col="Black",fontface='bold'),
          colorkey=list(
            space='right',                   
            labels=list(at=c(-80,-60,-40,-20,0,20,40,60,80,100), font=5,fontface='bold',cex=1),
            axis.line=list(col='black'),
            width=1.9
          ),    
          par.settings=list(
            strip.border=list(col='transparent'),
            strip.background=list(col='transparent'),
            axis.line=list(col='transparent')
            
          ),
          scales=list(draw=FALSE),
          col.regions=colrGP,                   
          at=seq(-80,100, len=101))+
  layer(sp.polygons(Vector_boundary, lwd=0.8))+
  layer(sp.points(vector_points, pch=21,col="black", fill='red',cex=1.7, alpha=0.9))+
  layer(sp.text(coordinates(vector_points), txt = vector_points$Name, pos = 3,
                col="black",font=list(face="bold"),cex=1))


levelplot(rabiStack_PRC$trend_Rabi_NDVI,col.regions=colrGP)
levelplot(rabiStack_PRC$trend_rabi,
          margin=FALSE, 
          #main=list("Auto-Correlation Coefficient (r) for AOT",cex=1.5,col="Black",fontface='bold'),
          colorkey=list(
            space='right',                   
            labels=list(at=c(-70,-60,-40,-20,0,20,40,60,80,90), font=5,fontface='bold',cex=1),
            axis.line=list(col='black'),
            width=1.9
          ),    
          par.settings=list(
            strip.border=list(col='transparent'),
            strip.background=list(col='transparent'),
            axis.line=list(col='transparent')
            
          ),
          scales=list(draw=FALSE),
          col.regions=colrGP,                   
          at=seq(-70,100, len=101))+
  layer(sp.polygons(Vector_boundary, lwd=0.8))+
  layer(sp.points(vector_points, pch=21,col="black", fill='red',cex=1.7, alpha=0.9))+
  layer(sp.text(coordinates(vector_points), txt = vector_points$Name, pos = 3,
                col="black",font=list(face="bold"),cex=1))


#______________________Pvaluemap______________#
levelplot(kharifStack_PRC$pvalue_Kharif_NDVI,col.regions=colrGP)
levelplot(kharifStack_PRC$p_kharif,
          margin=FALSE, 
          #main=list("Auto-Correlation Coefficient (r) for AOT",cex=1.5,col="Black",fontface='bold'),
          colorkey=list(
            space='right',                   
            labels=list(at=c(0.0,0.2,0.4,0.6,0.8,1.0), font=5,fontface='bold',cex=1),
            axis.line=list(col='black'),
            width=1.9
          ),    
          par.settings=list(
            strip.border=list(col='transparent'),
            strip.background=list(col='transparent'),
            axis.line=list(col='transparent')
            
          ),
          scales=list(draw=FALSE),
          col.regions=colrGP,                   
          at=seq(0.0,1.0, len=101))+
  layer(sp.polygons(Vector_boundary, lwd=0.8))+
  layer(sp.points(vector_points, pch=21,col="black", fill='red',cex=1.7, alpha=0.9))+
  layer(sp.text(coordinates(vector_points), txt = vector_points$Name, pos = 3,
                col="black",font=list(face="bold"),cex=1))


levelplot(rabiStack_PRC$trend_Rabi_NDVI,col.regions=colrGP)
levelplot(rabiStack_PRC$p_rabi,
          margin=FALSE, 
          #main=list("Auto-Correlation Coefficient (r) for AOT",cex=1.5,col="Black",fontface='bold'),
          colorkey=list(
            space='right',                   
            labels=list(at=c(0.0,0.2,0.4,0.6,0.8,1.0), font=5,fontface='bold',cex=1),
            axis.line=list(col='black'),
            width=1.9
          ),    
          par.settings=list(
            strip.border=list(col='transparent'),
            strip.background=list(col='transparent'),
            axis.line=list(col='transparent')
            
          ),
          scales=list(draw=FALSE),
          col.regions=colrGP,                   
          at=seq(0.0,1.0, len=101))+
  layer(sp.polygons(Vector_boundary, lwd=0.8))+
  layer(sp.points(vector_points, pch=21,col="black", fill='red',cex=1.7, alpha=0.9))+
  layer(sp.text(coordinates(vector_points), txt = vector_points$Name, pos = 3,
                col="black",font=list(face="bold"),cex=1))

#________________cocoref_______________#

levelplot(kharifStack_PRC$corocf_kharif,col.regions=colrGP)
levelplot(kharifStack_PRC$corocf_kharif,
          margin=FALSE, 
          #main=list("Auto-Correlation Coefficient (r) for AOT",cex=1.5,col="Black",fontface='bold'),
          colorkey=list(
            space='right',                   
            labels=list(at=c(-0.6,-0.4,-0.2,0.0,0.2,0.4,0.6,0.8,1.0), font=5,fontface='bold',cex=1),
            axis.line=list(col='black'),
            width=1.9
          ),    
          par.settings=list(
            strip.border=list(col='transparent'),
            strip.background=list(col='transparent'),
            axis.line=list(col='transparent')
            
          ),
          scales=list(draw=FALSE),
          col.regions=colrGP,                   
          at=seq(-0.6,1.0, len=101))+
  layer(sp.polygons(Vector_boundary, lwd=0.8))+
  layer(sp.points(vector_points, pch=21,col="black", fill='red',cex=1.7, alpha=0.9))+
  layer(sp.text(coordinates(vector_points), txt = vector_points$Name, pos = 3,
                col="black",font=list(face="bold"),cex=1))


levelplot(rabiStack_PRC$corceof_rabi,col.regions=colrGP)
levelplot(rabiStack_PRC$corceof_rabi,
          margin=FALSE, 
          #main=list("Auto-Correlation Coefficient (r) for AOT",cex=1.5,col="Black",fontface='bold'),
          colorkey=list(
            space='right',                   
            labels=list(at=c(-0.3,-0.2,-0.1,0.0,0.1,0.2,0.3), font=5,fontface='bold',cex=1),
            axis.line=list(col='black'),
            width=1.9
          ),    
          par.settings=list(
            strip.border=list(col='transparent'),
            strip.background=list(col='transparent'),
            axis.line=list(col='transparent')
            
          ),
          scales=list(draw=FALSE),
          col.regions=colrGP,                   
          at=seq(-0.3,0.3, len=101))+
  layer(sp.polygons(Vector_boundary, lwd=0.8))+
  layer(sp.points(vector_points, pch=21,col="black", fill='red',cex=1.7, alpha=0.9))+
  layer(sp.text(coordinates(vector_points), txt = vector_points$Name, pos = 3,
                col="black",font=list(face="bold"),cex=1))
