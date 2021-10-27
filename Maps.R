library(maptools)
library(rasterVis)
library(viridis)

# different ways to plot
#plot(px_BCR_grsp, main='Grasshopper Sparrow 2018 Maxent')

levelplot(px_BCR_weme$layer, margin=FALSE, col.regions=viridis, at=seq(0, 1, len=100),main="WEME")
levelplot(px_BCR_weme_2mRG$layer, margin=FALSE, col.regions=viridis, at=seq(0, 1, len=100),main="WEME 2mRG")
levelplot(px_BCR_weme_2mGr$layer, margin=FALSE, col.regions=viridis, at=seq(0, 1, len=100),main="WEME 2mGrass")
levelplot(px_BCR_bevi$layer, margin=FALSE, col.regions=viridis, at=seq(0, 1, len=100),main="BEVI")
levelplot(px_BCR_bevi_2mRG$layer, margin=FALSE, col.regions=viridis, at=seq(0, 1, len=100),main="BEVI 2mRG")
levelplot(px_BCR_bevi_2mGr$layer, margin=FALSE, col.regions=viridis, at=seq(0, 1, len=100),main="BEVI 2mGrass")
levelplot(px_BCR_nobo$layer, margin=FALSE, col.regions=viridis, at=seq(0, 1, len=100),main="NOBO")
levelplot(px_BCR_nobo_2mRG$layer, margin=FALSE, col.regions=viridis, at=seq(0, 1, len=100),main="NOBO 2mRG")
levelplot(px_BCR_nobo_2mGr$layer, margin=FALSE, col.regions=viridis, at=seq(0, 1, len=100),main="NOBO 2mGrass")
levelplot(px_BCR_dick$layer, margin=FALSE, col.regions=viridis, at=seq(0, 1, len=100),main="DICK")
levelplot(px_BCR_dick_2mRG$layer, margin=FALSE, col.regions=viridis, at=seq(0, 1, len=100),main="DICK 2mRG")
levelplot(px_BCR_dick_2mGr$layer, margin=FALSE, col.regions=viridis, at=seq(0, 1, len=100),main="Dick 2mGrass")
levelplot(px_BCR_grsp$layer, margin=FALSE, col.regions=viridis, at=seq(0, 1, len=100),main="GRSP")
levelplot(px_BCR_grsp_2mRG$layer, margin=FALSE, col.regions=viridis, at=seq(0, 1, len=100),main="GRSP 2mRG")
levelplot(px_BCR_grsp_2mGr$layer, margin=FALSE, col.regions=viridis, at=seq(0, 1, len=100),main="GRSP 2mGrass")
levelplot(px_BCR_eame$layer, margin=FALSE, col.regions=viridis, at=seq(0, 1, len=100),main="EAME")
levelplot(px_BCR_eame_2mRG$layer, margin=FALSE, col.regions=viridis, at=seq(0, 1, len=100),main="EAME 2mRG")
levelplot(px_BCR_eame_2mGr$layer, margin=FALSE, col.regions=viridis, at=seq(0, 1, len=100),main="EAME 2mGrass")
levelplot(px_BCR_eaki$layer, margin=FALSE, col.regions=viridis, at=seq(0, 1, len=100),main="EAKI")
levelplot(px_BCR_eaki_2mRG$layer, margin=FALSE, col.regions=viridis, at=seq(0, 1, len=100),main="EAKI 2mRG")
levelplot(px_BCR_eaki_2mGr$layer, margin=FALSE, col.regions=viridis, at=seq(0, 1, len=100),main="EAKI 2mGrass")


rm(px_BCR_eame_2mRG)
rm(px_BCR_eame_2mGr)









