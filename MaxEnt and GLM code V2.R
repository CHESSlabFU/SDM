library(dismo)
library(maptools)
library(rasterVis)
library(viridis)
library(landscapemetrics)
data(wrld_simpl)
#https://rdrr.io/github/BAAQMD/geotools/src/R/mask_raster.R
mask_raster <- function(raster, shape){
  outr <- crop(raster, extent(shape))
  outr <- mask(outr, shape)
  return(outr)
}

######################
predictors2 <- stack(list.files(file.path(system.file(package="dismo"), 'ex'), pattern='grd$', full.names=TRUE ))

file <- file.path(system.file(package="dismo"), "ex/bradypus.csv")
bradypus <- read.table(file,  header=TRUE,  sep=',')
bradypus <- bradypus[,-1]
presvals <- extract(predictors, bradypus)
set.seed(0)
backgr <- randomPoints(predictors, 500)
absvals <- extract(predictors, backgr)
pb <- c(rep(1, nrow(presvals)), rep(0, nrow(absvals)))
sdmdata <- data.frame(cbind(pb, rbind(presvals, absvals)))
sdmdata[,'biome'] <- as.factor(sdmdata[,'biome'])
pred_nf <- dropLayer(predictors, 'biome')
set.seed(0)
group <- kfold(bradypus, 5)
pres_train <- bradypus[group != 1, ]
pres_test <- bradypus[group == 1, ]  #old stuff
#######################



##########
#boundaries
#watersheds
#https://nrcs.app.box.com/v/huc/folder/39640323180  / https://gdg.sc.egov.usda.gov/GDGHome_DirectDownLoad.aspx
BCR_HUC12watershed <- readOGR(dsn=path.expand("C:/Users/jquinn2/Desktop/GMI SDM/wbdhu12_a_us_september2020"), layer="HUC12_BCR")
GMI_BCR_AOI <- readOGR(dsn=path.expand("C:/Users/jquinn2/Desktop/GMI SDM"),layer="CMG_BCR")
#######
### predictors


#Baseline

names(CDL_2018_BCR_RC) <- c('CDL_mode')
predictors_BCR<-stack (CDL_2018_BCR_RC)
BCR18_CDL_freq<-freq(predictors_BCR)
write.csv(BCR18_CDL_freq,"BCR18_CDL_freq.csv", row.names = FALSE)



#2 million Regen
NtE25Per_nonGr<-raster("C:/Users/jquinn2/Desktop/GMI SDM/Cropscape/ScenariosR3/nearest_to_edge_Testing25Per_nonGr.tif")
names(NtE25Per_nonGr) <- c('CDL_mode')
NtE50Per_nonGr<-raster("C:/Users/jquinn2/Desktop/GMI SDM/Cropscape/ScenariosR3/nearest_to_edge_Testing50Per_nonGr.tif")
names(NtE50Per_nonGr) <- c('CDL_mode')
NtE75Per_nonGr<-raster("C:/Users/jquinn2/Desktop/GMI SDM/Cropscape/ScenariosR3/nearest_to_edge_Testing75Per_nonGr.tif")
names(NtE75Per_nonGr) <- c('CDL_mode')
#show_landscape(NtE75Per_nonGr, discrete = TRUE)
#BCR18_2mgr_freq2<-freq(CDL_2mRG_BCR)
predictors_BCR_NtE25Per_nonGr <-projectRaster(NtE25Per_nonGr,  crs=crs(CDL_2018_BCR_RC))
predictors_BCR_NtE25Per_nonGr<-stack (predictors_BCR_NtE25Per_nonGr)
predictors_BCR_NtE50Per_nonGr <-projectRaster(NtE50Per_nonGr,  crs=crs(CDL_2018_BCR_RC))
predictors_BCR_NtE50Per_nonGr<-stack (predictors_BCR_NtE50Per_nonGr)
names(predictors_BCR_NtE50Per_nonGr) <- c('CDL_mode')
predictors_BCR_NtE75Per_nonGr <-projectRaster(NtE75Per_nonGr,  crs=crs(CDL_2018_BCR_RC))
predictors_BCR_NtE75Per_nonGr<-stack (predictors_BCR_NtE75Per_nonGr)
names(predictors_BCR_NtE75Per_nonGr) <- c('CDL_mode')

#setwd("C:/Users/jquinn2/Desktop/GMI SDM/Cropscape/Scenarios/Reprojected")
#writeRaster(predictors_BCR_2mRG, filename="predictors_BCR_2mRG",format="GTiff")
#BCR18_2mgr_freq<-freq(predictors_BCR_2mRG)
#BCR18_2mRG_freq<-BCR18_2mgr_freq
#write.csv(BCR18_2mRG_freq,"BCR18_2mRG_freq.csv", row.names = FALSE)


########
#recordsSel_LLCDL<-dplyr::select(records2.KSCLO, c("CDL_mode","longitude","latitude" ))
#recordsSel_LL<-dplyr::select(records2.KSCLO, c("longitude","latitude" ))
#nrow(recordsSel_LL)
#dups <- duplicated(recordsSel_LL[c("latitude", "longitude")])
#recordsSel_LL <- recordsSel_LL[!dups, ]
#nrow(recordsSel_LL)
#head(recordsSel_LL)
#ras_recordsSpatial = raster(recordsSpatial)
#gridded(ras_recordsSpatial)
#head(recordsSpatial)
#https://rdrr.io/cran/sp/man/SpatialGridDataFrame.html
#https://gis.stackexchange.com/questions/79062/how-to-make-raster-from-irregular-point-data-without-interpolation
#recordsSel_LLspdf=SpatialPixelsDataFrame(points=recordsSpatial[c("latitude","longitude")], data=recordsSpatial, tolerance=2.23253e-07)
############

######## Maxent
maxent()
#https://stackoverflow.com/questions/34624002/r-error-java-lang-outofmemoryerror-java-heap-space
options(java.parameters = "-Xmx8000m")

#xm <- maxent(x=predictors, recordsSel_LL, factors='CDL_mode')
#2018
xmBCR_weme_2018 <- maxent(x=predictors_BCR, weme_gbif_LL, factors='CDL_mode')
xmBCR_eame_2018 <- maxent(x=predictors_BCR, eame_gbif_LL, factors='CDL_mode')
xmBCR_grsp_2018 <- maxent(x=predictors_BCR, grsp_gbif_LL, factors='CDL_mode')
xmBCR_dick_2018 <- maxent(x=predictors_BCR, dick_gbif_LL, factors='CDL_mode')
xmBCR_nobo_2018 <- maxent(x=predictors_BCR, nobo_gbif_LL, factors='CDL_mode')
xmBCR_bevi_2018 <- maxent(x=predictors_BCR, bevi_gbif_LL, factors='CDL_mode')
xmBCR_eaki_2018 <- maxent(x=predictors_BCR, eaki_gbif_LL, factors='CDL_mode')
xmBCR_witk_2018 <- maxent(x=predictors_BCR, witk_gbif_LL, factors='CDL_mode')  
xmBCR_ybcu_2018 <- maxent(x=predictors_BCR, ybcu_gbif_LL, factors='CDL_mode')

xmBCR_upsp_2018 <- maxent(x=predictors_BCR, upsp_gbif_LL, factors='CDL_mode')
xmBCR_bobo_2018 <- maxent(x=predictors_BCR, bobo_gbif_LL, factors='CDL_mode')  
xmBCR_labu_2018 <- maxent(x=predictors_BCR, labu_gbif_LL, factors='CDL_mode')

save.image("C:/Users/jquinn2/Desktop/GMI SDM/GMI SDM BCR18 V3.RData")
#choose the extent
#KSExtent <- extent(CDL_classified)
BCRExtent <- extent(GMI_BCR_AOI)

#predict occupancy - WEME
##Current
px_BCR_weme <- predict(predictors_BCR, xmBCR_weme_2018, ext=BCRExtent, progress='text')
px_BCR_weme <- mask_raster(raster=px_BCR_weme, shape=GMI_BCR_AOI)
#25% increase
px_BCR_weme_NtE25Per_nonGr <- predict(predictors_BCR_NtE25Per_nonGr, xmBCR_weme_2018, ext=BCRExtent, progress='text')
px_BCR_weme_NtE25Per_nonGr <- mask_raster(raster=px_BCR_weme_NtE25Per_nonGr, shape=GMI_BCR_AOI)
#50% increase
px_BCR_weme_NtE50Per_nonGr <- predict(predictors_BCR_NtE50Per_nonGr, xmBCR_weme_2018, ext=BCRExtent, progress='text')
px_BCR_weme_NtE50Per_nonGr <- mask_raster(raster=px_BCR_weme_NtE50Per_nonGr, shape=GMI_BCR_AOI)
#75% increase
px_BCR_weme_NtE75Per_nonGr <- predict(predictors_BCR_NtE75Per_nonGr, xmBCR_weme_2018, ext=BCRExtent, progress='text')
px_BCR_weme_NtE75Per_nonGr <- mask_raster(raster=px_BCR_weme_NtE75Per_nonGr, shape=GMI_BCR_AOI)



#writeRaster(px_BCR_weme_NtE25Per_nonGr, filename="px_BCR_weme_NtE25Per_nonGr",format="GTiff")

#predict occupancy - EAME
##Current
px_BCR_eame <- predict(predictors_BCR, xmBCR_eame_2018, ext=BCRExtent, progress='text')
px_BCR_eame <- mask_raster(raster=px_BCR_eame, shape=GMI_BCR_AOI)
#25% increase
px_BCR_eame_NtE25Per_nonGr <- predict(predictors_BCR_NtE25Per_nonGr, xmBCR_eame_2018, ext=BCRExtent, progress='text')
px_BCR_eame_NtE25Per_nonGr <- mask_raster(raster=px_BCR_eame_NtE25Per_nonGr, shape=GMI_BCR_AOI)
#50% increase
px_BCR_eame_NtE50Per_nonGr <- predict(predictors_BCR_NtE50Per_nonGr, xmBCR_eame_2018, ext=BCRExtent, progress='text')
px_BCR_eame_NtE50Per_nonGr <- mask_raster(raster=px_BCR_eame_NtE50Per_nonGr, shape=GMI_BCR_AOI)
#75% increase
px_BCR_eame_NtE75Per_nonGr <- predict(predictors_BCR_NtE75Per_nonGr, xmBCR_eame_2018, ext=BCRExtent, progress='text')
px_BCR_eame_NtE75Per_nonGr <- mask_raster(raster=px_BCR_eame_NtE75Per_nonGr, shape=GMI_BCR_AOI)

#writeRaster(px_BCR_eame_NtE25Per_nonGr, filename="px_BCR_eame_NtE25Per_nonGr",format="GTiff")

#predict occupancy - GRSP
##Current
px_BCR_grsp <- predict(predictors_BCR, xmBCR_grsp_2018, ext=BCRExtent, progress='text')
px_BCR_grsp <- mask_raster(raster=px_BCR_grsp, shape=GMI_BCR_AOI)
#25% increase
px_BCR_grsp_NtE25Per_nonGr <- predict(predictors_BCR_NtE25Per_nonGr, xmBCR_grsp_2018, ext=BCRExtent, progress='text')
px_BCR_grsp_NtE25Per_nonGr <- mask_raster(raster=px_BCR_grsp_NtE25Per_nonGr, shape=GMI_BCR_AOI)
#50% increase
px_BCR_grsp_NtE50Per_nonGr <- predict(predictors_BCR_NtE50Per_nonGr, xmBCR_grsp_2018, ext=BCRExtent, progress='text')
px_BCR_grsp_NtE50Per_nonGr <- mask_raster(raster=px_BCR_grsp_NtE50Per_nonGr, shape=GMI_BCR_AOI)
#75% increase
px_BCR_grsp_NtE75Per_nonGr <- predict(predictors_BCR_NtE75Per_nonGr, xmBCR_grsp_2018, ext=BCRExtent, progress='text')
px_BCR_grsp_NtE75Per_nonGr <- mask_raster(raster=px_BCR_grsp_NtE75Per_nonGr, shape=GMI_BCR_AOI)

#writeRaster(px_BCR_grsp_NtE25Per_nonGr, filename="px_BCR_grsp_NtE25Per_nonGr",format="GTiff")

#predict occupancy - DICK
##Current
px_BCR_dick <- predict(predictors_BCR, xmBCR_dick_2018, ext=BCRExtent, progress='text')
px_BCR_dick <- mask_raster(raster=px_BCR_dick, shape=GMI_BCR_AOI)
#25% increase
px_BCR_dick_NtE25Per_nonGr <- predict(predictors_BCR_NtE25Per_nonGr, xmBCR_dick_2018, ext=BCRExtent, progress='text')
px_BCR_dick_NtE25Per_nonGr <- mask_raster(raster=px_BCR_dick_NtE25Per_nonGr, shape=GMI_BCR_AOI)
#50% increase
px_BCR_dick_NtE50Per_nonGr <- predict(predictors_BCR_NtE50Per_nonGr, xmBCR_dick_2018, ext=BCRExtent, progress='text')
px_BCR_dick_NtE50Per_nonGr <- mask_raster(raster=px_BCR_dick_NtE50Per_nonGr, shape=GMI_BCR_AOI)
#75% increase
px_BCR_dick_NtE75Per_nonGr <- predict(predictors_BCR_NtE75Per_nonGr, xmBCR_dick_2018, ext=BCRExtent, progress='text')
px_BCR_dick_NtE75Per_nonGr <- mask_raster(raster=px_BCR_dick_NtE75Per_nonGr, shape=GMI_BCR_AOI)


#predict occupancy - NOBO
##Current
px_BCR_nobo <- predict(predictors_BCR, xmBCR_nobo_2018, ext=BCRExtent, progress='text')
px_BCR_nobo <- mask_raster(raster=px_BCR_nobo, shape=GMI_BCR_AOI)
#25% increase
px_BCR_nobo_NtE25Per_nonGr <- predict(predictors_BCR_NtE25Per_nonGr, xmBCR_nobo_2018, ext=BCRExtent, progress='text')
px_BCR_nobo_NtE25Per_nonGr <- mask_raster(raster=px_BCR_nobo_NtE25Per_nonGr, shape=GMI_BCR_AOI)
#50% increase
px_BCR_nobo_NtE50Per_nonGr <- predict(predictors_BCR_NtE50Per_nonGr, xmBCR_nobo_2018, ext=BCRExtent, progress='text')
px_BCR_nobo_NtE50Per_nonGr <- mask_raster(raster=px_BCR_nobo_NtE50Per_nonGr, shape=GMI_BCR_AOI)
#75% increase
px_BCR_nobo_NtE75Per_nonGr <- predict(predictors_BCR_NtE75Per_nonGr, xmBCR_nobo_2018, ext=BCRExtent, progress='text')
px_BCR_nobo_NtE75Per_nonGr <- mask_raster(raster=px_BCR_nobo_NtE75Per_nonGr, shape=GMI_BCR_AOI)


#predict occupancy - BEVI
##Current
px_BCR_bevi <- predict(predictors_BCR, xmBCR_bevi_2018, ext=BCRExtent, progress='text')
px_BCR_bevi <- mask_raster(raster=px_BCR_bevi, shape=GMI_BCR_AOI)
#25% increase
px_BCR_bevi_NtE25Per_nonGr <- predict(predictors_BCR_NtE25Per_nonGr, xmBCR_bevi_2018, ext=BCRExtent, progress='text')
px_BCR_bevi_NtE25Per_nonGr <- mask_raster(raster=px_BCR_bevi_NtE25Per_nonGr, shape=GMI_BCR_AOI)
#50% increase
px_BCR_bevi_NtE50Per_nonGr <- predict(predictors_BCR_NtE50Per_nonGr, xmBCR_bevi_2018, ext=BCRExtent, progress='text')
px_BCR_bevi_NtE50Per_nonGr <- mask_raster(raster=px_BCR_bevi_NtE50Per_nonGr, shape=GMI_BCR_AOI)
#75% increase
px_BCR_bevi_NtE75Per_nonGr <- predict(predictors_BCR_NtE75Per_nonGr, xmBCR_bevi_2018, ext=BCRExtent, progress='text')
px_BCR_bevi_NtE75Per_nonGr <- mask_raster(raster=px_BCR_bevi_NtE75Per_nonGr, shape=GMI_BCR_AOI)


#predict occupancy - EAKI
##Current
px_BCR_eaki <- predict(predictors_BCR, xmBCR_eaki_2018, ext=BCRExtent, progress='text')
px_BCR_eaki <- mask_raster(raster=px_BCR_eaki, shape=GMI_BCR_AOI)
#25% increase
px_BCR_eaki_NtE25Per_nonGr <- predict(predictors_BCR_NtE25Per_nonGr, xmBCR_eaki_2018, ext=BCRExtent, progress='text')
px_BCR_eaki_NtE25Per_nonGr <- mask_raster(raster=px_BCR_eaki_NtE25Per_nonGr, shape=GMI_BCR_AOI)
#50% increase
px_BCR_eaki_NtE50Per_nonGr <- predict(predictors_BCR_NtE50Per_nonGr, xmBCR_eaki_2018, ext=BCRExtent, progress='text')
px_BCR_eaki_NtE50Per_nonGr <- mask_raster(raster=px_BCR_eaki_NtE50Per_nonGr, shape=GMI_BCR_AOI)
#75% increase
px_BCR_eaki_NtE75Per_nonGr <- predict(predictors_BCR_NtE75Per_nonGr, xmBCR_eaki_2018, ext=BCRExtent, progress='text')
px_BCR_eaki_NtE75Per_nonGr <- mask_raster(raster=px_BCR_eaki_NtE75Per_nonGr, shape=GMI_BCR_AOI)


#predict occupancy - WITK
##Current
px_BCR_witk <- predict(predictors_BCR, xmBCR_witk_2018, ext=BCRExtent, progress='text')
px_BCR_witk <- mask_raster(raster=px_BCR_witk, shape=GMI_BCR_AOI)
#25% increase
px_BCR_witk_NtE25Per_nonGr <- predict(predictors_BCR_NtE25Per_nonGr, xmBCR_witk_2018, ext=BCRExtent, progress='text')
px_BCR_witk_NtE25Per_nonGr <- mask_raster(raster=px_BCR_witk_NtE25Per_nonGr, shape=GMI_BCR_AOI)
#50% increase
px_BCR_witk_NtE50Per_nonGr <- predict(predictors_BCR_NtE50Per_nonGr, xmBCR_witk_2018, ext=BCRExtent, progress='text')
px_BCR_witk_NtE50Per_nonGr <- mask_raster(raster=px_BCR_witk_NtE50Per_nonGr, shape=GMI_BCR_AOI)
#75% increase
px_BCR_witk_NtE75Per_nonGr <- predict(predictors_BCR_NtE75Per_nonGr, xmBCR_witk_2018, ext=BCRExtent, progress='text')
px_BCR_witk_NtE75Per_nonGr <- mask_raster(raster=px_BCR_witk_NtE75Per_nonGr, shape=GMI_BCR_AOI)



#predict occupancy - YBCU
##Current
px_BCR_ybcu <- predict(predictors_BCR, xmBCR_ybcu_2018, ext=BCRExtent, progress='text')
px_BCR_ybcu <- mask_raster(raster=px_BCR_ybcu, shape=GMI_BCR_AOI)
#25% increase
px_BCR_ybcu_NtE25Per_nonGr <- predict(predictors_BCR_NtE25Per_nonGr, xmBCR_ybcu_2018, ext=BCRExtent, progress='text')
px_BCR_ybcu_NtE25Per_nonGr <- mask_raster(raster=px_BCR_ybcu_NtE25Per_nonGr, shape=GMI_BCR_AOI)
#50% increase
px_BCR_ybcu_NtE50Per_nonGr <- predict(predictors_BCR_NtE50Per_nonGr, xmBCR_ybcu_2018, ext=BCRExtent, progress='text')
px_BCR_ybcu_NtE50Per_nonGr <- mask_raster(raster=px_BCR_ybcu_NtE50Per_nonGr, shape=GMI_BCR_AOI)
#75% increase
px_BCR_ybcu_NtE75Per_nonGr <- predict(predictors_BCR_NtE75Per_nonGr, xmBCR_ybcu_2018, ext=BCRExtent, progress='text')
px_BCR_ybcu_NtE75Per_nonGr <- mask_raster(raster=px_BCR_ybcu_NtE75Per_nonGr, shape=GMI_BCR_AOI)
save.image("C:/Users/jquinn2/Desktop/GMI SDM/GMI SDM BCR18 V3.RData")

# predict occupancy UPSP
##Current
px_BCR_upsp <- predict(predictors_BCR, xmBCR_upsp_2018, ext=BCRExtent, progress='text')
px_BCR_upsp <- mask_raster(raster=px_BCR_upsp, shape=GMI_BCR_AOI)
save.image("C:/Users/jquinn2/Desktop/GMI SDM/GMI SDM BCR18 V3.RData")
#25% increase
px_BCR_upsp_NtE25Per_nonGr <- predict(predictors_BCR_NtE25Per_nonGr, xmBCR_upsp_2018, ext=BCRExtent, progress='text')
px_BCR_upsp_NtE25Per_nonGr <- mask_raster(raster=px_BCR_upsp_NtE25Per_nonGr, shape=GMI_BCR_AOI)
save.image("C:/Users/jquinn2/Desktop/GMI SDM/GMI SDM BCR18 V3.RData")
#50% increase
px_BCR_upsp_NtE50Per_nonGr <- predict(predictors_BCR_NtE50Per_nonGr, xmBCR_upsp_2018, ext=BCRExtent, progress='text')
px_BCR_upsp_NtE50Per_nonGr <- mask_raster(raster=px_BCR_upsp_NtE50Per_nonGr, shape=GMI_BCR_AOI)
save.image("C:/Users/jquinn2/Desktop/GMI SDM/GMI SDM BCR18 V3.RData")
#75% increase
px_BCR_upsp_NtE75Per_nonGr <- predict(predictors_BCR_NtE75Per_nonGr, xmBCR_upsp_2018, ext=BCRExtent, progress='text')
px_BCR_upsp_NtE75Per_nonGr <- mask_raster(raster=px_BCR_upsp_NtE75Per_nonGr, shape=GMI_BCR_AOI)
save.image("C:/Users/jquinn2/Desktop/GMI SDM/GMI SDM BCR18 V3.RData")

# predict occupancy BOBO
##Current
px_BCR_bobo <- predict(predictors_BCR, xmBCR_bobo_2018, ext=BCRExtent, progress='text')
px_BCR_bobo <- mask_raster(raster=px_BCR_bobo, shape=GMI_BCR_AOI)
save.image("C:/Users/jquinn2/Desktop/GMI SDM/GMI SDM BCR18 V3.RData")
#25% increase
px_BCR_bobo_NtE25Per_nonGr <- predict(predictors_BCR_NtE25Per_nonGr, xmBCR_bobo_2018, ext=BCRExtent, progress='text')
px_BCR_bobo_NtE25Per_nonGr <- mask_raster(raster=px_BCR_bobo_NtE25Per_nonGr, shape=GMI_BCR_AOI)
save.image("C:/Users/jquinn2/Desktop/GMI SDM/GMI SDM BCR18 V3.RData")
#50% increase
px_BCR_bobo_NtE50Per_nonGr <- predict(predictors_BCR_NtE50Per_nonGr, xmBCR_bobo_2018, ext=BCRExtent, progress='text')
px_BCR_bobo_NtE50Per_nonGr <- mask_raster(raster=px_BCR_bobo_NtE50Per_nonGr, shape=GMI_BCR_AOI)
save.image("C:/Users/jquinn2/Desktop/GMI SDM/GMI SDM BCR18 V3.RData")
#75% increase
px_BCR_bobo_NtE75Per_nonGr <- predict(predictors_BCR_NtE75Per_nonGr, xmBCR_bobo_2018, ext=BCRExtent, progress='text')
px_BCR_bobo_NtE75Per_nonGr <- mask_raster(raster=px_BCR_bobo_NtE75Per_nonGr, shape=GMI_BCR_AOI)
save.image("C:/Users/jquinn2/Desktop/GMI SDM/GMI SDM BCR18 V3.RData")

# predict occupancy LABU
##Current
px_BCR_labu <- predict(predictors_BCR, xmBCR_labu_2018, ext=BCRExtent, progress='text')
px_BCR_labu <- mask_raster(raster=px_BCR_labu, shape=GMI_BCR_AOI)
save.image("C:/Users/jquinn2/Desktop/GMI SDM/GMI SDM BCR18 V3.RData")
#25% increase
px_BCR_labu_NtE25Per_nonGr <- predict(predictors_BCR_NtE25Per_nonGr, xmBCR_labu_2018, ext=BCRExtent, progress='text')
px_BCR_labu_NtE25Per_nonGr <- mask_raster(raster=px_BCR_labu_NtE25Per_nonGr, shape=GMI_BCR_AOI)
save.image("C:/Users/jquinn2/Desktop/GMI SDM/GMI SDM BCR18 V3.RData")
#50% increase
px_BCR_labu_NtE50Per_nonGr <- predict(predictors_BCR_NtE50Per_nonGr, xmBCR_labu_2018, ext=BCRExtent, progress='text')
px_BCR_labu_NtE50Per_nonGr <- mask_raster(raster=px_BCR_labu_NtE50Per_nonGr, shape=GMI_BCR_AOI)
save.image("C:/Users/jquinn2/Desktop/GMI SDM/GMI SDM BCR18 V3.RData")
#75% increase
px_BCR_labu_NtE75Per_nonGr <- predict(predictors_BCR_NtE75Per_nonGr, xmBCR_labu_2018, ext=BCRExtent, progress='text')
px_BCR_labu_NtE75Per_nonGr <- mask_raster(raster=px_BCR_labu_NtE75Per_nonGr, shape=GMI_BCR_AOI)
save.image("C:/Users/jquinn2/Desktop/GMI SDM/GMI SDM BCR18 V3.RData")


save.image("C:/Users/jquinn2/Desktop/GMI SDM/GMI SDM BCR18 V3.RData") 
#setwd("C:/Users/jquinn2/Desktop/GMI SDM/Maxent Outputs/25per_nonGrass")
    #writeRaster(px_BCR_ybcu_NtE25Per_nonGr, filename="px_BCR_ybcu_NtE25Per_nonGr",format="GTiff")
#setwd("C:/Users/jquinn2/Desktop/GMI SDM/Maxent Outputs/50per_nonGrass")
#setwd("C:/Users/jquinn2/Desktop/GMI SDM/Maxent Outputs/75per_nonGrass")


AllSp_pc<-bind_rows(weme_pc,eame_pc,dick_pc,grsp_pc,nobo_pc,upsp_pc,bobo_pc,labu_pc,
                     eaki_pc, ybcu_pc, bevi_pc,
                     .id = "Bird")
Species<-c(rep("WEME",4),rep("EAME",4),rep("DICK",4),
           rep("GRSP",4),rep("NOBO",4),rep("UPSP",4),
           rep("BOBO",4),rep("LABU",4),
           rep("EAKI",4),rep("YBCU",4),rep("BEVI",4)
            )
Guild<-c(rep("Grassland", 4*8), rep("Edge/Woodland",4*3))

bind_cols(AllSp_pc,Species,Guild)
install.packages("ggthemes")
library(ggthemes)
ggplot(AllSp_pc, aes(y=pct_change, x=Scenario, group=Species, fill=Species,color=Species))+#geom_rangeframe()+
  geom_point(aes())+stat_summary(geom="line") + ylab("% Increase")+theme_minimal()+theme(legend.position = "bottom")

########################
#OLD
############################3

#px <- predict(predictors, xm, ext=KSExtent, progress='text')
px_BCR_weme <- predict(predictors_BCR, xmBCR_weme_2018, ext=BCRExtent, progress='text')
  px_BCR_weme <- mask_raster(raster=px_BCR_weme, shape=GMI_BCR_AOI)
px_BCR_weme_2mRG <- predict(predictors_BCR_2mRG, xmBCR_weme_2018, ext=BCRExtent, progress='text')
  px_BCR_weme_2mRG <- mask_raster(raster=px_BCR_weme_2mRG, shape=GMI_BCR_AOI)
px_BCR_weme_2mGr <- predict(predictors_BCR_2mGr, xmBCR_weme_2018, ext=BCRExtent, progress='text')
  px_BCR_weme_2mGr <- mask_raster(raster=px_BCR_weme_2mGr, shape=GMI_BCR_AOI) 
px_BCR_weme_4mRG <- predict(predictors_BCR_4mRG, xmBCR_weme_2018, ext=BCRExtent, progress='text')
  px_BCR_weme_4mRG <- mask_raster(raster=px_BCR_weme_4mRG, shape=GMI_BCR_AOI)
px_BCR_weme_4mGr <- predict(predictors_BCR_4mGr, xmBCR_weme_2018, ext=BCRExtent, progress='text')
  px_BCR_weme_4mGr <- mask_raster(raster=px_BCR_weme_4mGr, shape=GMI_BCR_AOI) 

px_BCR_weme_2mGr2mRg <- predict(predictors_BCR_2mGr2mRg, xmBCR_weme_2018, ext=BCRExtent, progress='text')
  px_BCR_weme_2mGr2mRg <- mask_raster(raster=px_BCR_weme_2mGr2mRg, shape=GMI_BCR_AOI)
px_BCR_weme_8mRG <- predict(predictors_BCR_8mRG, xmBCR_weme_2018, ext=BCRExtent, progress='text')
  px_BCR_weme_8mRG <- mask_raster(raster=px_BCR_weme_8mRG, shape=GMI_BCR_AOI)
  save.image("C:/Users/jquinn2/Desktop/GMI SDM/GMI SDM BCR18 V3.RData") 


px_BCR_nobo <- predict(predictors_BCR, xmBCR_nobo_2018, ext=BCRExtent, progress='text')
  px_BCR_nobo <- mask_raster(raster=px_BCR_nobo, shape=GMI_BCR_AOI)
px_BCR_nobo_2mRG <- predict(predictors_BCR_2mRG, xmBCR_nobo_2018, ext=BCRExtent, progress='text')
  px_BCR_nobo_2mRG <- mask_raster(raster=px_BCR_nobo_2mRG, shape=GMI_BCR_AOI) 
px_BCR_nobo_2mGr <- predict(predictors_BCR_2mGr, xmBCR_nobo_2018, ext=BCRExtent, progress='text')
  px_BCR_nobo_2mGr <- mask_raster(raster=px_BCR_nobo_2mGr, shape=GMI_BCR_AOI)
px_BCR_nobo_4mRG <- predict(predictors_BCR_4mRG, xmBCR_nobo_2018, ext=BCRExtent, progress='text')
  px_BCR_nobo_4mRG <- mask_raster(raster=px_BCR_nobo_4mRG, shape=GMI_BCR_AOI) 

px_BCR_nobo_2mGr2mRg <- predict(predictors_BCR_2mGr2mRg, xmBCR_nobo_2018, ext=BCRExtent, progress='text')
  px_BCR_nobo_2mGr2mRg <- mask_raster(raster=px_BCR_nobo_2mGr2mRg, shape=GMI_BCR_AOI) 
px_BCR_nobo_8mRG <- predict(predictors_BCR_8mRG, xmBCR_nobo_2018, ext=BCRExtent, progress='text')
  px_BCR_nobo_8mRG <- mask_raster(raster=px_BCR_nobo_8mRG, shape=GMI_BCR_AOI) 
  save.image("C:/Users/jquinn2/Desktop/GMI SDM/GMI SDM BCR18 V2.RData") 

px_BCR_dick <- predict(predictors_BCR, xmBCR_dick_2018, ext=BCRExtent, progress='text')
  px_BCR_dick <- mask_raster(raster=px_BCR_dick, shape=GMI_BCR_AOI)
px_BCR_dick_2mRG <- predict(predictors_BCR_2mRG, xmBCR_dick_2018, ext=BCRExtent, progress='text')
  px_BCR_dick_2mRG <- mask_raster(raster=px_BCR_dick_2mRG, shape=GMI_BCR_AOI) 
px_BCR_dick_2mGr <- predict(predictors_BCR_2mGr, xmBCR_dick_2018, ext=BCRExtent, progress='text')
  px_BCR_dick_2mGr <- mask_raster(raster=px_BCR_dick_2mGr, shape=GMI_BCR_AOI)
px_BCR_dick_4mRG <- predict(predictors_BCR_4mRG, xmBCR_dick_2018, ext=BCRExtent, progress='text')
  px_BCR_dick_4mRG <- mask_raster(raster=px_BCR_dick_4mRG, shape=GMI_BCR_AOI) 
px_BCR_dick_4mGr <- predict(predictors_BCR_4mGr, xmBCR_dick_2018, ext=BCRExtent, progress='text')
  px_BCR_dick_4mGr <- mask_raster(raster=px_BCR_dick_4mGr, shape=GMI_BCR_AOI) 
  
px_BCR_dick_2mGr2mRg <- predict(predictors_BCR_2mGr2mRg, xmBCR_dick_2018, ext=BCRExtent, progress='text')
  px_BCR_dick_2mGr2mRg <- mask_raster(raster=px_BCR_dick_2mGr2mRg, shape=GMI_BCR_AOI) 
px_BCR_dick_8mRG <- predict(predictors_BCR_8mRG, xmBCR_dick_2018, ext=BCRExtent, progress='text')
  px_BCR_dick_8mRG <- mask_raster(raster=px_BCR_dick_8mRG, shape=GMI_BCR_AOI) 
  save.image("C:/Users/jquinn2/Desktop/GMI SDM/GMI SDM BCR18 V2.RData") 

px_BCR_grsp <- predict(predictors_BCR, xmBCR_grsp_2018, ext=BCRExtent, progress='text')
  px_BCR_grsp <- mask_raster(raster=px_BCR_grsp, shape=GMI_BCR_AOI)
px_BCR_grsp_2mRG <- predict(predictors_BCR_2mRG, xmBCR_grsp_2018, ext=BCRExtent, progress='text')
  px_BCR_grsp_2mRG <- mask_raster(raster=px_BCR_grsp_2mRG, shape=GMI_BCR_AOI)
px_BCR_grsp_2mGr <- predict(predictors_BCR_2mGr, xmBCR_grsp_2018, ext=BCRExtent, progress='text')
  px_BCR_grsp_2mGr <- mask_raster(raster=px_BCR_grsp_2mGr, shape=GMI_BCR_AOI) 
px_BCR_grsp_4mRG <- predict(predictors_BCR_4mRG, xmBCR_grsp_2018, ext=BCRExtent, progress='text')
  px_BCR_grsp_4mRG <- mask_raster(raster=px_BCR_grsp_4mRG, shape=GMI_BCR_AOI)
px_BCR_grsp_4mGr <- predict(predictors_BCR_4mGr, xmBCR_grsp_2018, ext=BCRExtent, progress='text')
  px_BCR_grsp_4mGr <- mask_raster(raster=px_BCR_grsp_4mGr, shape=GMI_BCR_AOI)
  
px_BCR_grsp_2mGr2mRg <- predict(predictors_BCR_2mGr2mRg, xmBCR_grsp_2018, ext=BCRExtent, progress='text')
  px_BCR_grsp_2mGr2mRg <- mask_raster(raster=px_BCR_grsp_2mGr2mRg, shape=GMI_BCR_AOI)
px_BCR_grsp_8mRG <- predict(predictors_BCR_8mRG, xmBCR_grsp_2018, ext=BCRExtent, progress='text')
  px_BCR_grsp_8mRG <- mask_raster(raster=px_BCR_grsp_8mRG, shape=GMI_BCR_AOI)
  save.image("C:/Users/jquinn2/Desktop/GMI SDM/GMI SDM BCR18 V2.RData") 
  
px_BCR_bevi <- predict(predictors_BCR, xmBCR_bevi_2018, ext=BCRExtent, progress='text')
  px_BCR_bevi <- mask_raster(raster=px_BCR_bevi, shape=GMI_BCR_AOI)
px_BCR_bevi_2mRG <- predict(predictors_BCR_2mRG, xmBCR_bevi_2018, ext=BCRExtent, progress='text')
  px_BCR_bevi_2mRG <- mask_raster(raster=px_BCR_bevi_2mRG, shape=GMI_BCR_AOI) 
px_BCR_bevi_2mGr <- predict(predictors_BCR_2mGr, xmBCR_bevi_2018, ext=BCRExtent, progress='text')
  px_BCR_bevi_2mGr <- mask_raster(raster=px_BCR_bevi_2mGr, shape=GMI_BCR_AOI) 
px_BCR_bevi_4mRG <- predict(predictors_BCR_4mRG, xmBCR_bevi_2018, ext=BCRExtent, progress='text')
  px_BCR_bevi_4mRG <- mask_raster(raster=px_BCR_bevi_4mRG, shape=GMI_BCR_AOI) 
px_BCR_bevi_4mGr <- predict(predictors_BCR_4mGr, xmBCR_bevi_2018, ext=BCRExtent, progress='text')
  px_BCR_bevi_4mGr <- mask_raster(raster=px_BCR_bevi_4mGr, shape=GMI_BCR_AOI) 
  
px_BCR_bevi_2mGr2mRg <- predict(predictors_BCR_2mGr2mRg, xmBCR_bevi_2018, ext=BCRExtent, progress='text')
  px_BCR_bevi_2mGr2mRg <- mask_raster(raster=px_BCR_bevi_2mGr2mRg, shape=GMI_BCR_AOI) 
px_BCR_bevi_8mRG <- predict(predictors_BCR_8mRG, xmBCR_bevi_2018, ext=BCRExtent, progress='text')
  px_BCR_bevi_8mRG <- mask_raster(raster=px_BCR_bevi_8mRG, shape=GMI_BCR_AOI) 
  save.image("C:/Users/jquinn2/Desktop/GMI SDM/GMI SDM BCR18 V2.RData") 

px_BCR_eame <- predict(predictors_BCR, xmBCR_eame_2018, ext=BCRExtent, progress='text')
  px_BCR_eame <- mask_raster(raster=px_BCR_eame, shape=GMI_BCR_AOI)
px_BCR_eame_2mRG <- predict(predictors_BCR_2mRG, xmBCR_eame_2018, ext=BCRExtent, progress='text')
  px_BCR_eame_2mRG <- mask_raster(raster=px_BCR_eame_2mRG, shape=GMI_BCR_AOI)
px_BCR_eame_2mGr <- predict(predictors_BCR_2mGr, xmBCR_eame_2018, ext=BCRExtent, progress='text')
  px_BCR_eame_2mGr <- mask_raster(raster=px_BCR_eame_2mGr, shape=GMI_BCR_AOI)
px_BCR_eame_4mRG <- predict(predictors_BCR_4mRG, xmBCR_eame_2018, ext=BCRExtent, progress='text')
  px_BCR_eame_4mRG <- mask_raster(raster=px_BCR_eame_4mRG, shape=GMI_BCR_AOI)
px_BCR_eame_4mGr <- predict(predictors_BCR_4mGr, xmBCR_eame_2018, ext=BCRExtent, progress='text')
  px_BCR_eame_4mGr <- mask_raster(raster=px_BCR_eame_4mGr, shape=GMI_BCR_AOI)
  
px_BCR_eame_2mGr2mRg <- predict(predictors_BCR_2mGr2mRg, xmBCR_eame_2018, ext=BCRExtent, progress='text')
  px_BCR_eame_2mGr2mRg <- mask_raster(raster=px_BCR_eame_2mGr2mRg, shape=GMI_BCR_AOI)
px_BCR_eame_8mRG <- predict(predictors_BCR_8mRG, xmBCR_eame_2018, ext=BCRExtent, progress='text')
  px_BCR_eame_8mRG <- mask_raster(raster=px_BCR_eame_8mRG, shape=GMI_BCR_AOI)
  save.image("C:/Users/jquinn2/Desktop/GMI SDM/GMI SDM BCR18 V2.RData") 
  
px_BCR_eaki <- predict(predictors_BCR, xmBCR_eaki_2018, ext=BCRExtent, progress='text')
  px_BCR_eaki <- mask_raster(raster=px_BCR_eaki, shape=GMI_BCR_AOI)
px_BCR_eaki_2mRG <- predict(predictors_BCR_2mRG, xmBCR_eaki_2018, ext=BCRExtent, progress='text')
  px_BCR_eaki_2mRG <- mask_raster(raster=px_BCR_eaki_2mRG, shape=GMI_BCR_AOI)
px_BCR_eaki_2mGr <- predict(predictors_BCR_2mGr, xmBCR_eaki_2018, ext=BCRExtent, progress='text')
  px_BCR_eaki_2mGr <- mask_raster(raster=px_BCR_eaki_2mGr, shape=GMI_BCR_AOI)
px_BCR_eaki_4mRG <- predict(predictors_BCR_4mRG, xmBCR_eaki_2018, ext=BCRExtent, progress='text')
  px_BCR_eaki_4mRG <- mask_raster(raster=px_BCR_eaki_4mRG, shape=GMI_BCR_AOI)
px_BCR_eaki_4mGr <- predict(predictors_BCR_4mGr, xmBCR_eaki_2018, ext=BCRExtent, progress='text')
  px_BCR_eaki_4mGr <- mask_raster(raster=px_BCR_eaki_4mGr, shape=GMI_BCR_AOI)
  
px_BCR_eaki_2mGr2mRg <- predict(predictors_BCR_2mGr2mRg, xmBCR_eaki_2018, ext=BCRExtent, progress='text')
  px_BCR_eaki_2mGr2mRg <- mask_raster(raster=px_BCR_eaki_2mGr2mRg, shape=GMI_BCR_AOI)
px_BCR_eaki_8mRG <- predict(predictors_BCR_8mRG, xmBCR_eaki_2018, ext=BCRExtent, progress='text')
  px_BCR_eaki_8mRG <- mask_raster(raster=px_BCR_eaki_8mRG, shape=GMI_BCR_AOI)
  save.image("C:/Users/jquinn2/Desktop/GMI SDM/GMI SDM BCR18 V2.RData") 

px_BCR_witk <- predict(predictors_BCR, xmBCR_witk_2018, ext=BCRExtent, progress='text')
  px_BCR_witk <- mask_raster(raster=px_BCR_witk, shape=GMI_BCR_AOI)
px_BCR_witk_2mRG <- predict(predictors_BCR_2mRG, xmBCR_witk_2018, ext=BCRExtent, progress='text')
  px_BCR_witk_2mRG <- mask_raster(raster=px_BCR_witk_2mRG, shape=GMI_BCR_AOI)
px_BCR_witk_2mGr <- predict(predictors_BCR_2mGr, xmBCR_witk_2018, ext=BCRExtent, progress='text')
  px_BCR_witk_2mGr <- mask_raster(raster=px_BCR_witk_2mGr, shape=GMI_BCR_AOI)
px_BCR_witk_4mRG <- predict(predictors_BCR_4mRG, xmBCR_witk_2018, ext=BCRExtent, progress='text')
  px_BCR_witk_4mRG <- mask_raster(raster=px_BCR_witk_4mRG, shape=GMI_BCR_AOI)
px_BCR_witk_4mGr <- predict(predictors_BCR_4mGr, xmBCR_witk_2018, ext=BCRExtent, progress='text')
  px_BCR_witk_4mGr <- mask_raster(raster=px_BCR_witk_4mGr, shape=GMI_BCR_AOI)
  
px_BCR_witk_2mGr2mRg <- predict(predictors_BCR_2mGr2mRg, xmBCR_witk_2018, ext=BCRExtent, progress='text')
  px_BCR_witk_2mGr2mRg <- mask_raster(raster=px_BCR_witk_2mGr2mRg, shape=GMI_BCR_AOI)
px_BCR_witk_8mRG <- predict(predictors_BCR_8mRG, xmBCR_witk_2018, ext=BCRExtent, progress='text')
  px_BCR_witk_8mRG <- mask_raster(raster=px_BCR_witk_8mRG, shape=GMI_BCR_AOI)
  save.image("C:/Users/jquinn2/Desktop/GMI SDM/GMI SDM BCR18 V2.RData") 

  
setwd("C:/Users/jquinn2/Desktop/GMI SDM/Maxent Outputs/2018")
writeRaster(px_BCR_weme, filename="px_BCR_weme",format="GTiff")
writeRaster(px_BCR_eame, filename="px_BCR_eame",format="GTiff")
writeRaster(px_BCR_grsp, filename="px_BCR_grsp",format="GTiff")
writeRaster(px_BCR_dick, filename="px_BCR_dick",format="GTiff")
writeRaster(px_BCR_nobo, filename="px_BCR_nobo",format="GTiff")
writeRaster(px_BCR_bevi, filename="px_BCR_bevi",format="GTiff") 
writeRaster(px_BCR_eaki, filename="px_BCR_eaki",format="GTiff")
writeRaster(px_BCR_witk, filename="px_BCR_witk",format="GTiff")

setwd("C:/Users/jquinn2/Desktop/GMI SDM/Maxent Outputs/2 mil Grass")
writeRaster(px_BCR_weme_2mGr, filename="px_BCR_weme_2mGr",format="GTiff")
writeRaster(px_BCR_eame_2mGr, filename="px_BCR_eame_2mGr",format="GTiff")
writeRaster(px_BCR_grsp_2mGr, filename="px_BCR_grsp_2mGr",format="GTiff")
writeRaster(px_BCR_dick_2mGr, filename="px_BCR_dick_2mGr",format="GTiff")
writeRaster(px_BCR_nobo_2mGr, filename="px_BCR_nobo_2mGr",format="GTiff")
writeRaster(px_BCR_bevi_2mGr, filename="px_BCR_bevi_2mGr",format="GTiff")
writeRaster(px_BCR_eaki_2mGr, filename="px_BCR_eaki_2mGr",format="GTiff")
writeRaster(px_BCR_witk_2mGr, filename="px_BCR_witk_2mGr",format="GTiff")

setwd("C:/Users/jquinn2/Desktop/GMI SDM/Maxent Outputs/2 mil RA")
writeRaster(px_BCR_weme_2mRG, filename="px_BCR_weme_2mRG",format="GTiff")
writeRaster(px_BCR_eame_2mRG, filename="px_BCR_eame_2mRG",format="GTiff")
writeRaster(px_BCR_grsp_2mRG, filename="px_BCR_grsp_2mRG",format="GTiff")
writeRaster(px_BCR_dick_2mRG, filename="px_BCR_dick_2mRG",format="GTiff")
writeRaster(px_BCR_nobo_2mRG, filename="px_BCR_nobo_2mRG",format="GTiff")
writeRaster(px_BCR_bevi_2mRG, filename="px_BCR_bevi_2mRG",format="GTiff")
writeRaster(px_BCR_eaki_2mRG, filename="px_BCR_eaki_2mRG",format="GTiff")
writeRaster(px_BCR_witk_2mRG, filename="px_BCR_witk_2mRG",format="GTiff")

setwd("C:/Users/jquinn2/Desktop/GMI SDM/Maxent Outputs/4 mil Grass")
writeRaster(px_BCR_weme_4mGr, filename="px_BCR_weme_4mGr",format="GTiff")
writeRaster(px_BCR_eame_4mGr, filename="px_BCR_eame_4mGr",format="GTiff")
writeRaster(px_BCR_grsp_4mGr, filename="px_BCR_grsp_4mGr",format="GTiff")
writeRaster(px_BCR_dick_4mGr, filename="px_BCR_dick_4mGr",format="GTiff")
writeRaster(px_BCR_nobo_4mGr, filename="px_BCR_nobo_4mGr",format="GTiff")
writeRaster(px_BCR_bevi_4mGr, filename="px_BCR_bevi_4mGr",format="GTiff")
writeRaster(px_BCR_eaki_4mGr, filename="px_BCR_eaki_4mGr",format="GTiff")
writeRaster(px_BCR_witk_4mGr, filename="px_BCR_witk_4mGr",format="GTiff")

setwd("C:/Users/jquinn2/Desktop/GMI SDM/Maxent Outputs/4 mil RA")
writeRaster(px_BCR_weme_4mRG, filename="px_BCR_weme_4mRG",format="GTiff")
writeRaster(px_BCR_eame_4mRG, filename="px_BCR_eame_4mRG",format="GTiff")
writeRaster(px_BCR_grsp_4mRG, filename="px_BCR_grsp_4mRG",format="GTiff")
writeRaster(px_BCR_dick_4mRG, filename="px_BCR_dick_4mRG",format="GTiff")
writeRaster(px_BCR_nobo_4mRG, filename="px_BCR_nobo_4mRG",format="GTiff")
writeRaster(px_BCR_bevi_4mRG, filename="px_BCR_bevi_4mRG",format="GTiff")
writeRaster(px_BCR_eaki_4mRG, filename="px_BCR_eaki_4mRG",format="GTiff")
writeRaster(px_BCR_witk_4mRG, filename="px_BCR_witk_4mRG",format="GTiff")

setwd("C:/Users/jquinn2/Desktop/GMI SDM/Maxent Outputs/2 mil RA 2 mil Grass")
writeRaster(px_BCR_weme_2mGr2mRg, filename="px_BCR_weme_2mGr2mRg",format="GTiff")
writeRaster(px_BCR_eame_2mGr2mRg, filename="px_BCR_eame_2mGr2mRg",format="GTiff")
writeRaster(px_BCR_grsp_2mGr2mRg, filename="px_BCR_grsp_2mGr2mRg",format="GTiff")
writeRaster(px_BCR_dick_2mGr2mRg, filename="px_BCR_dick_2mGr2mRg",format="GTiff")
writeRaster(px_BCR_nobo_2mGr2mRg, filename="px_BCR_nobo_2mGr2mRg",format="GTiff")
writeRaster(px_BCR_bevi_2mGr2mRg, filename="px_BCR_bevi_2mGr2mRg",format="GTiff")
writeRaster(px_BCR_eaki_2mGr2mRg, filename="px_BCR_eaki_2mGr2mRg",format="GTiff")
writeRaster(px_BCR_witk_2mGr2mRg, filename="px_BCR_witk_2mGr2mRg",format="GTiff")

setwd("C:/Users/jquinn2/Desktop/GMI SDM/Maxent Outputs/8 mil RA")
writeRaster(px_BCR_weme_8mRG, filename="px_BCR_weme_8mRG",format="GTiff")
writeRaster(px_BCR_eame_8mRG, filename="px_BCR_eame_8mRG",format="GTiff")
writeRaster(px_BCR_grsp_8mRG, filename="px_BCR_grsp_8mRG",format="GTiff")
writeRaster(px_BCR_dick_8mRG, filename="px_BCR_dick_8mRG",format="GTiff")
writeRaster(px_BCR_nobo_8mRG, filename="px_BCR_nobo_8mRG",format="GTiff")
writeRaster(px_BCR_bevi_8mRG, filename="px_BCR_bevi_8mRG",format="GTiff")
writeRaster(px_BCR_eaki_8mRG, filename="px_BCR_eaki_8mRG",format="GTiff")
writeRaster(px_BCR_witk_8mRG, filename="px_BCR_witk_8mRG",format="GTiff")












  
  ########
  #2 million Regen
  names(CDL_2mRG_BCR) <- c('CDL_mode')
  predictors_BCR_2mRG<-stack (CDL_2mRG_BCR)
  BCR18_2mgr_freq2<-freq(CDL_2mRG_BCR)
  predictors_BCR_2mRGV2 <-projectRaster(CDL_2mRG_BCR,  crs=crs(CDL_2018_BCR_RC), res=30)
  predictors_BCR_2mRG<-stack (predictors_BCR_2mRG)
  setwd("C:/Users/jquinn2/Desktop/GMI SDM/Cropscape/Scenarios/Reprojected")
  writeRaster(predictors_BCR_2mRG, filename="predictors_BCR_2mRG",format="GTiff")
  BCR18_2mgr_freq<-freq(predictors_BCR_2mRG)
  BCR18_2mRG_freq<-BCR18_2mgr_freq
  write.csv(BCR18_2mRG_freq,"BCR18_2mRG_freq.csv", row.names = FALSE)
  #2 million Grass
  names(CDL_2mGr_BCR) <- c('CDL_mode')
  predictors_BCR_2mRG <-projectRaster(CDL_2mGr_BCR,  crs=crs(CDL_2018_BCR_RC))
  predictors_BCR_2mGr<-stack (predictors_BCR_2mRG)
  setwd("C:/Users/jquinn2/Desktop/GMI SDM/Cropscape/Scenarios/Reprojected")
  writeRaster(predictors_BCR_2mGr, filename="predictors_BCR_2mGr",format="GTiff")
  BCR18_2mGR_freq<-freq(predictors_BCR_2mGr)
  write.csv(BCR18_2mGR_freq,"BCR18_2mGR_freq.csv", row.names = FALSE)
  
  

#reset projections
px_BCR <- setExtent(px_BCR, predictors_BCR)
projection(px_BCR) <-'+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0'
px_BCR <- projectRaster(px_BCR,  crs=crs(CDL_2018_BCR))

# 
setwd("C:/Users/jquinn2/Desktop/GMI SDM/Maxent Outputs/Testing")

raster::writeRaster(px_BCR, filename="DICK_SDM_MaxentV5", format="GTiff")









?writeRaster
# for GLM
#We also need a vector of 1's and 0's, one value per row in trainData to indicate if it's a presence or a background site.
presBg <- c(rep(1, nrow(records2.KSCLO)), rep(0, nrow(randomBgEnv_CDL)))
randomBgSites <- randomPoints(CDL, 5000)
randomBg <- cbind(randomBgSites, randomBgEnv_CDL)
names(randomBg)[1:2] <- c('longitude', 'latitude')
head(randomBg)


trainDataManual <- rbind(recordsSel, randomBg)
head(trainDataManual)
trainDataManual5<-cbind(trainDataManual,presBg)
head(trainDataManual5)
trainDataManual5$CDL_mode = as.factor(trainDataManual5$CDL_mode)
gm1<-glm(presBg~CDL_mode, data=trainDataManual5,family = binomial(link = "logit"))
summary(gm1)
pg <- predict(CDL_classified, gm1, ext=KSExtent)
hist(pg, breaks=9)
plot(pg,main="SDM, DICK")
plot(pg,
     legend=F,
     breaks = c(0, .1,.2,.3,.4,.5,.6,.7,.8,.9, 1), 
     col = rainbow(12),
     main="SDM, DICK")
plot(CDL)
plot(CDL_classified)



bc.model <- bioclim(x = predictors, p = recordsSel2)
str(bc.model)



pb <- predict(CDL_classified, bc.model, ext=KSExtent, progress='')
predict.presence <- dismo::predict(object = bc.model, 
                                   x = CDL_classified, 
                                   ext = KSExtent)
