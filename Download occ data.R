
#downloading and cleaning GBIF data

library(rgbif)
library(scrubr)
library(maps)
library(rgdal) 
library(dplyr)
library(rgeos)
GMI_BCR_AOI <- readOGR(dsn=path.expand("C:/Users/jquinn2/Desktop/GMI SDM"),
                            layer="CMG_BCR")

crs(GMI_BCR_AOI)
plot(GMI_BCR_AOI)
GMI_BCR_AOI
weme<-c("Sturnella neglecta") #14000   14615
eame<-c("Sturnella magna") #8372
grsp<-c("Ammodramus savannarum") #2632
dick<-c("Spiza americana") #7115
nobo<-c("Colinus virginianus") #5266
bevi<-c("Vireo bellii") #2253
eaki<-c("Tyrannus tyrannus") #7925
witk<-c("Meleagris gallopavo")
ybcu<-c("Coccyzus americanus") #3543
upsp<-c("Bartramia longicauda") #2043
bobo<-c("Dolichonyx oryzivorus") #476
labu<-c("Calamospiza melanocorys") #1707
sppi<-c("Anthus spragueii")#75
cclo<-c("Calcarius ornatus") #273
basp<-c("Centronyx bairdii") #1
hesp<-c("Centronyx henslowii") #75
lbcu<-c("Numenius americanus") #481

gbif_lbcu<- occ_data(scientificName = lbcu, 
                      hasCoordinate = TRUE, year=2018,
                      geometry= 'POLYGON((-103.2469 43.16474,-103.2469 30.76303, -96.54329 30.76303,-96.54329 43.16474, -103.2469 43.16474 ))',
                      #stateProvince = c("Nebraska", "Oklahoma","Kansas" ),
                      limit = 10000)


gbif_weme
gbif_eame
gbif_grsp
gbif_dick
gbif_nobo
gbif_bevi
gbif_eaki
gbif_witk
gbif_ybcu
gbif_upsp
gbif_bobo
gbif_labu
gbif_sppi
gbif_cclo
gbif_basp
gbif_hesp
gbif_lbcu
################
gbif<-gbif_lbcu
gbif_LL_S<-dplyr::select(gbif$data,scientificName,decimalLongitude,decimalLatitude,stateProvince,year, )


gbif_LL_S<-rename(gbif_LL_S, longitude=decimalLongitude )
gbif_LL_S<-rename(gbif_LL_S, latitude=decimalLatitude )
gbif_LL_S<-rename(gbif_LL_S, state=stateProvince )
#weme_gbif_LL_S<- filter(weme_gbif_LL_S, year==2018)
#weme_gbif_LL_S<-dplyr::filter(weme_gbif_LL_S, state=="Kansas" | state== "Nebraska"| state== "Oklahoma" )
nrow(gbif_LL_S)

##############
#############
#put back speceis name
gbif_LL_S<-dplyr::select(gbif_LL_S,longitude,latitude )
lbcu_gbif_LL<-as.data.frame(gbif_LL_S)
 

#########
weme_gbif_LL
eame_gbif_LL
grsp_gbif_LL
dick_gbif_LL
nobo_gbif_LL
bevi_gbif_LL
eaki_gbif_LL
witk_gbif_LL
ybcu_gbif_LL
upsp_gbif_LL
bobo_gbif_LL
labu_gbif_LL

sppi_gbif_LL
cclo_gbif_LL
hesp_gbif_LL
lbcu_gbif_LL
