######Summerize by watershed

install.packages("raster")
library(exactextractr)
library(Rcpp)
library(rgdal) 
library(ggplot2)
library(tidyr)
library(pals)

BCR_HUC12watershed <- readOGR(dsn=path.expand("C:/Users/jquinn2/Desktop/GMI SDM/wbdhu12_a_us_september2020"), layer="HUC12_BCR")
plot(BCR_HUC12watershed)

## WEME
weme2018 <- exact_extract(px_BCR_weme, BCR_HUC12watershed, 'mean')
weme25Per_nonGr <- exact_extract(px_BCR_weme_NtE25Per_nonGr, BCR_HUC12watershed, 'mean')
weme50Per_nonGr <- exact_extract(px_BCR_weme_NtE50Per_nonGr, BCR_HUC12watershed, 'mean')
weme75Per_nonGr <- exact_extract(px_BCR_weme_NtE75Per_nonGr, BCR_HUC12watershed, 'mean')


weme_bWS<-bind_cols(weme2018,weme25Per_nonGr,weme50Per_nonGr,weme75Per_nonGr,.id = "id")
##rename for secnarios
weme_bWS<-rename(weme_bWS, Current=...1);weme_bWS<-rename(weme_bWS, NtE25Per=...2);
weme_bWS<-rename(weme_bWS, NtE50Per=...3);weme_bWS<-rename(weme_bWS, NtE75Per=...4)
#weme_bWS<-rename(weme_bWS, Grassland4mil=...5);weme_bWS<-rename(weme_bWS, RegenAg2milGrassland2mil=...6);
#weme_bWS<-rename(weme_bWS, RegenAg8mil=...7)
weme_bWS2<-weme_bWS %>% pivot_longer(!.id , names_to = "Scenario", values_to = "count")
weme_med<-weme_bWS2 %>% group_by(Scenario) %>% summarise(median(count))
weme_pc<-mutate(weme_med, pct_change=(`median(count)`/(`median(count)`[1]) - 1)*100)
weme_pc
p_weme<-ggplot(weme_bWS2, aes(y=count, x=Scenario, fill=Scenario))+ggtitle("Western Meadowlark")+
  geom_boxplot(width = 0.3)+#geom_line()+
  #geom_jitter(aes(color=Scenario))+
  theme_minimal()+ylab("Occupancy")+theme(legend.position = "none")+ylim(0,1)+coord_flip()
p_weme
## EAME
eame2018 <- exact_extract(px_BCR_eame, BCR_HUC12watershed, 'mean')
eame25Per_nonGr <- exact_extract(px_BCR_eame_NtE25Per_nonGr, BCR_HUC12watershed, 'mean')
eame50Per_nonGr <- exact_extract(px_BCR_eame_NtE50Per_nonGr, BCR_HUC12watershed, 'mean')
eame75Per_nonGr <- exact_extract(px_BCR_eame_NtE75Per_nonGr, BCR_HUC12watershed, 'mean')


eame_bWS<-bind_cols(eame2018,eame25Per_nonGr,eame50Per_nonGr,eame75Per_nonGr,.id = "id")
##rename for secnarios
eame_bWS<-rename(eame_bWS, Current=...1);eame_bWS<-rename(eame_bWS, NtE25Per=...2);
eame_bWS<-rename(eame_bWS, NtE50Per=...3);eame_bWS<-rename(eame_bWS, NtE75Per=...4);#eame_bWS<-rename(eame_bWS, Grassland4mil=...5);eame_bWS<-rename(eame_bWS, RegenAg2milGrassland2mil=...6);eame_bWS<-rename(eame_bWS, RegenAg8mil=...7)
eame_bWS<-eame_bWS %>% pivot_longer(!.id , names_to = "Scenario", values_to = "count")
eame_bWS %>% group_by(Scenario) %>% summarise(mean(count))
eame_med<-eame_bWS %>% group_by(Scenario) %>% summarise(median(count))
eame_pc<-mutate(eame_med, pct_change=(`median(count)`/(`median(count)`[1]) - 1)*100)
eame_pc
p_eame<-ggplot(eame_bWS, aes(y=count, x=Scenario, fill=Scenario))+ggtitle("Eastern Meadowlark")+
  geom_boxplot(width = 0.3)+
  theme_minimal()+ylab("Occupancy")+theme(legend.position = "none")+ylim(0,1)+coord_flip()

## GRSP
grsp2018 <- exact_extract(px_BCR_grsp, BCR_HUC12watershed, 'mean')
grsp25Per_nonGr <- exact_extract(px_BCR_grsp_NtE25Per_nonGr, BCR_HUC12watershed, 'mean')
grsp50Per_nonGr <- exact_extract(px_BCR_grsp_NtE50Per_nonGr, BCR_HUC12watershed, 'mean')
grsp75Per_nonGr <- exact_extract(px_BCR_grsp_NtE75Per_nonGr, BCR_HUC12watershed, 'mean')


grsp_bWS<-bind_cols(grsp2018,grsp25Per_nonGr,grsp50Per_nonGr,grsp75Per_nonGr,.id = "id")
##rename for secnarios
grsp_bWS<-rename(grsp_bWS, Current=...1);grsp_bWS<-rename(grsp_bWS, NtE25Per=...2);
grsp_bWS<-rename(grsp_bWS, NtE50Per=...3);grsp_bWS<-rename(grsp_bWS, NtE75Per=...4)#;grsp_bWS<-rename(grsp_bWS, Grassland4mil=...5);grsp_bWS<-rename(grsp_bWS, RegenAg2milGrassland2mil=...6);grsp_bWS<-rename(grsp_bWS, RegenAg8mil=...7)
grsp_bWS<-grsp_bWS %>% pivot_longer(!.id , names_to = "Scenario", values_to = "count")
grsp_bWS %>% group_by(Scenario) %>% summarise(mean(count))
grsp_med<-grsp_bWS %>% group_by(Scenario) %>% summarise(median(count))
grsp_pc<-mutate(grsp_med, pct_change=(`median(count)`/(`median(count)`[1]) - 1)*100)
grsp_pc
p_grsp<-ggplot(grsp_bWS, aes(y=count, x=Scenario, fill=Scenario))+ggtitle("Grasshopper Sparrow")+
  geom_boxplot(width = 0.3)+
  theme_minimal()+ylab("Occupancy")+theme(legend.position = "none")+ylim(0,1)+coord_flip()
p_grsp
## DICK
dick2018 <- exact_extract(px_BCR_dick, BCR_HUC12watershed, 'mean')
dick25Per_nonGr <- exact_extract(px_BCR_dick_NtE25Per_nonGr, BCR_HUC12watershed, 'mean')
dick50Per_nonGr <- exact_extract(px_BCR_dick_NtE50Per_nonGr, BCR_HUC12watershed, 'mean')
dick75Per_nonGr <- exact_extract(px_BCR_dick_NtE75Per_nonGr, BCR_HUC12watershed, 'mean')


dick_bWS<-bind_cols(dick2018,dick25Per_nonGr,dick50Per_nonGr,dick75Per_nonGr,.id = "id")
##rename for secnarios
dick_bWS<-rename(dick_bWS, Current=...1);dick_bWS<-rename(dick_bWS, NtE25Per=...2);
dick_bWS<-rename(dick_bWS, NtE50Per=...3);dick_bWS<-rename(dick_bWS, NtE75Per=...4);#dick_bWS<-rename(dick_bWS, Grassland4mil=...5);dick_bWS<-rename(dick_bWS, RegenAg2milGrassland2mil=...6);dick_bWS<-rename(dick_bWS, RegenAg8mil=...7)
dick_bWS<-dick_bWS %>% pivot_longer(!.id , names_to = "Scenario", values_to = "count")
dick_bWS %>% group_by(Scenario) %>% summarise(mean(count))
dick_med<-dick_bWS %>% group_by(Scenario) %>% summarise(median(count))
dick_pc<-mutate(dick_med, pct_change=(`median(count)`/(`median(count)`[1]) - 1)*100)
dick_pc
p_dick<-ggplot(dick_bWS, aes(y=count, x=Scenario, fill=Scenario))+ggtitle("Dickcissel")+
  geom_boxplot(width = 0.3)+
  theme_minimal()+ylab("Occupancy")+theme(legend.position = "none")+ylim(0,1)+coord_flip()
p_dick
## NOBO
nobo2018 <- exact_extract(px_BCR_nobo, BCR_HUC12watershed, 'mean')
nobo25Per_nonGr <- exact_extract(px_BCR_nobo_NtE25Per_nonGr, BCR_HUC12watershed, 'mean')
nobo50Per_nonGr <- exact_extract(px_BCR_nobo_NtE50Per_nonGr, BCR_HUC12watershed, 'mean')
nobo75Per_nonGr <- exact_extract(px_BCR_nobo_NtE75Per_nonGr, BCR_HUC12watershed, 'mean')


nobo_bWS<-bind_cols(nobo2018,nobo25Per_nonGr,nobo50Per_nonGr,nobo75Per_nonGr,.id = "id")
##rename for secnarios
nobo_bWS<-rename(nobo_bWS, Current=...1);nobo_bWS<-rename(nobo_bWS, NtE25Per=...2);
nobo_bWS<-rename(nobo_bWS, NtE50Per=...3);nobo_bWS<-rename(nobo_bWS, NtE75Per=...4);#nobo_bWS<-rename(nobo_bWS, Grassland4mil=...5);nobo_bWS<-rename(nobo_bWS, RegenAg2milGrassland2mil=...6);nobo_bWS<-rename(nobo_bWS, RegenAg8mil=...7)
nobo_bWS<-nobo_bWS %>% pivot_longer(!.id , names_to = "Scenario", values_to = "count")
nobo_bWS %>% group_by(Scenario) %>% summarise(mean(count))
nobo_med<-nobo_bWS %>% group_by(Scenario) %>% summarise(median(count))
nobo_pc<-mutate(nobo_med, pct_change=(`median(count)`/(`median(count)`[1]) - 1)*100)
nobo_pc
p_nobo<-ggplot(nobo_bWS, aes(y=count, x=Scenario, fill=Scenario))+ggtitle("Northern Bobwhite")+
  geom_boxplot(width = 0.3)+
  theme_minimal()+ylab("Occupancy")+theme(legend.position = "none")+ylim(0,1)+coord_flip()
p_nobo

## BEVI
bevi2018 <- exact_extract(px_BCR_bevi, BCR_HUC12watershed, 'mean')
bevi25Per_nonGr <- exact_extract(px_BCR_bevi_NtE25Per_nonGr, BCR_HUC12watershed, 'mean')
bevi50Per_nonGr <- exact_extract(px_BCR_bevi_NtE50Per_nonGr, BCR_HUC12watershed, 'mean')
bevi75Per_nonGr <- exact_extract(px_BCR_bevi_NtE75Per_nonGr, BCR_HUC12watershed, 'mean')


bevi_bWS<-bind_cols(bevi2018,bevi25Per_nonGr,bevi50Per_nonGr,bevi75Per_nonGr,.id = "id")
##rename for secnarios
bevi_bWS<-rename(bevi_bWS, Current=...1);bevi_bWS<-rename(bevi_bWS, NtE25Per=...2);
bevi_bWS<-rename(bevi_bWS, NtE50Per=...3);bevi_bWS<-rename(bevi_bWS, NtE75Per=...4);#bevi_bWS<-rename(bevi_bWS, Grassland4mil=...5);bevi_bWS<-rename(bevi_bWS, RegenAg2milGrassland2mil=...6);bevi_bWS<-rename(bevi_bWS, RegenAg8mil=...7)
bevi_bWS<-bevi_bWS %>% pivot_longer(!.id , names_to = "Scenario", values_to = "count")
bevi_bWS %>% group_by(Scenario) %>% summarise(mean(count))
bevi_med<-bevi_bWS %>% group_by(Scenario) %>% summarise(median(count))
bevi_pc<-mutate(bevi_med, pct_change=(`median(count)`/(`median(count)`[1]) - 1)*100)
bevi_pc
p_bevi<-ggplot(bevi_bWS, aes(y=count, x=Scenario, fill=Scenario))+ggtitle("Bell's Vireo")+
  geom_boxplot(width = 0.3)+
  theme_minimal()+ylab("Occupancy")+theme(legend.position = "none")+ylim(0,1)+coord_flip()
p_bevi


## EAKI
eaki2018 <- exact_extract(px_BCR_eaki, BCR_HUC12watershed, 'mean')
eaki25Per_nonGr <- exact_extract(px_BCR_eaki_NtE25Per_nonGr, BCR_HUC12watershed, 'mean')
eaki50Per_nonGr <- exact_extract(px_BCR_eaki_NtE50Per_nonGr, BCR_HUC12watershed, 'mean')
eaki75Per_nonGr <- exact_extract(px_BCR_eaki_NtE75Per_nonGr, BCR_HUC12watershed, 'mean')


eaki_bWS<-bind_cols(eaki2018,eaki25Per_nonGr,eaki50Per_nonGr,eaki75Per_nonGr,.id = "id")
##rename for secnarios
eaki_bWS<-rename(eaki_bWS, Current=...1);eaki_bWS<-rename(eaki_bWS, NtE25Per=...2);
eaki_bWS<-rename(eaki_bWS, NtE50Per=...3);eaki_bWS<-rename(eaki_bWS, NtE75Per=...4)#;eaki_bWS<-rename(eaki_bWS, Grassland4mil=...5);eaki_bWS<-rename(eaki_bWS, RegenAg2milGrassland2mil=...6);eaki_bWS<-rename(eaki_bWS, RegenAg8mil=...7)
eaki_bWS<-eaki_bWS %>% pivot_longer(!.id , names_to = "Scenario", values_to = "count")
eaki_bWS %>% group_by(Scenario) %>% summarise(mean(count))
eaki_med<-eaki_bWS %>% group_by(Scenario) %>% summarise(median(count))
eaki_pc<-mutate(eaki_med, pct_change=(`median(count)`/(`median(count)`[1]) - 1)*100)
eaki_pc
p_eaki<-ggplot(eaki_bWS, aes(y=count, x=Scenario, fill=Scenario))+ggtitle("Eastern Kingbird")+
  geom_boxplot(width = 0.3)+
  theme_minimal()+ylab("Occupancy")+theme(legend.position = "none")+ylim(0,1)+coord_flip()

## WITK
witk2018 <- exact_extract(px_BCR_witk, BCR_HUC12watershed, 'mean')
witk25Per_nonGr <- exact_extract(px_BCR_witk_NtE25Per_nonGr, BCR_HUC12watershed, 'mean')
witk50Per_nonGr <- exact_extract(px_BCR_witk_NtE50Per_nonGr, BCR_HUC12watershed, 'mean')
witk75Per_nonGr <- exact_extract(px_BCR_witk_NtE75Per_nonGr, BCR_HUC12watershed, 'mean')


witk_bWS<-bind_cols(witk2018,witk25Per_nonGr,witk50Per_nonGr,witk75Per_nonGr,.id = "id")
##rename for secnarios
witk_bWS<-rename(witk_bWS, Current=...1);witk_bWS<-rename(witk_bWS, NtE25Per=...2);
witk_bWS<-rename(witk_bWS, NtE50Per=...3);witk_bWS<-rename(witk_bWS, NtE75Per=...4);#witk_bWS<-rename(witk_bWS, Grassland4mil=...5);witk_bWS<-rename(witk_bWS, RegenAg2milGrassland2mil=...6);witk_bWS<-rename(witk_bWS, RegenAg8mil=...7)
witk_bWS<-witk_bWS %>% pivot_longer(!.id , names_to = "Scenario", values_to = "count")
witk_bWS %>% group_by(Scenario) %>% summarise(mean(count))
witk_med<-witk_bWS %>% group_by(Scenario) %>% summarise(median(count))
witk_pc<-mutate(witk_med, pct_change=(`median(count)`/(`median(count)`[1]) - 1)*100)
witk_pc
p_witk<-ggplot(witk_bWS, aes(y=count, x=Scenario, fill=Scenario))+ggtitle("Wild Turkey")+
  geom_boxplot(width = 0.3)+
  theme_minimal()+ylab("Occupancy")+theme(legend.position = "none")+ylim(0,1)+coord_flip()

## YBCU
ybcu2018 <- exact_extract(px_BCR_ybcu, BCR_HUC12watershed, 'mean')
ybcu25Per_nonGr <- exact_extract(px_BCR_ybcu_NtE25Per_nonGr, BCR_HUC12watershed, 'mean')
ybcu50Per_nonGr <- exact_extract(px_BCR_ybcu_NtE50Per_nonGr, BCR_HUC12watershed, 'mean')
ybcu75Per_nonGr <- exact_extract(px_BCR_ybcu_NtE75Per_nonGr, BCR_HUC12watershed, 'mean')


ybcu_bWS<-bind_cols(ybcu2018,ybcu25Per_nonGr,ybcu50Per_nonGr,ybcu75Per_nonGr,.id = "id")
##rename for secnarios
ybcu_bWS<-rename(ybcu_bWS, Current=...1);ybcu_bWS<-rename(ybcu_bWS, NtE25Per=...2);
ybcu_bWS<-rename(ybcu_bWS, NtE50Per=...3);ybcu_bWS<-rename(ybcu_bWS, NtE75Per=...4);#ybcu_bWS<-rename(ybcu_bWS, Grassland4mil=...5);ybcu_bWS<-rename(ybcu_bWS, RegenAg2milGrassland2mil=...6);ybcu_bWS<-rename(ybcu_bWS, RegenAg8mil=...7)
ybcu_bWS<-ybcu_bWS %>% pivot_longer(!.id , names_to = "Scenario", values_to = "count")
ybcu_bWS %>% group_by(Scenario) %>% summarise(mean(count))
ybcu_med<-ybcu_bWS %>% group_by(Scenario) %>% summarise(median(count))
ybcu_pc<-mutate(ybcu_med, pct_change=(`median(count)`/(`median(count)`[1]) - 1)*100)
ybcu_pc
p_ybcu<-ggplot(ybcu_bWS, aes(y=count, x=Scenario, fill=Scenario))+ggtitle("Yellow Billed Cuckcoo")+
  geom_boxplot(width = 0.3)+
  theme_minimal()+ylab("Occupancy")+theme(legend.position = "none")+ylim(0,1)+coord_flip()

## UPSP
upsp2018 <- exact_extract(px_BCR_upsp, BCR_HUC12watershed, 'mean')
upsp25Per_nonGr <- exact_extract(px_BCR_upsp_NtE25Per_nonGr, BCR_HUC12watershed, 'mean')
upsp50Per_nonGr <- exact_extract(px_BCR_upsp_NtE50Per_nonGr, BCR_HUC12watershed, 'mean')
upsp75Per_nonGr <- exact_extract(px_BCR_upsp_NtE75Per_nonGr, BCR_HUC12watershed, 'mean')


upsp_bWS<-bind_cols(upsp2018,upsp25Per_nonGr,upsp50Per_nonGr,upsp75Per_nonGr,.id = "id")
##rename for secnarios
upsp_bWS<-rename(upsp_bWS, Current=...1);upsp_bWS<-rename(upsp_bWS, NtE25Per=...2);
upsp_bWS<-rename(upsp_bWS, NtE50Per=...3);upsp_bWS<-rename(upsp_bWS, NtE75Per=...4);#upsp_bWS<-rename(upsp_bWS, Grassland4mil=...5);upsp_bWS<-rename(upsp_bWS, RegenAg2milGrassland2mil=...6);upsp_bWS<-rename(upsp_bWS, RegenAg8mil=...7)
upsp_bWS<-upsp_bWS %>% pivot_longer(!.id , names_to = "Scenario", values_to = "count")
upsp_bWS %>% group_by(Scenario) %>% summarise(mean(count))
upsp_med<-upsp_bWS %>% group_by(Scenario) %>% summarise(median(count))
upsp_pc<-mutate(upsp_med, pct_change=(`median(count)`/(`median(count)`[1]) - 1)*100)
upsp_pc
p_upsp<-ggplot(upsp_bWS, aes(y=count, x=Scenario, fill=Scenario))+ggtitle("Upland Sandpiper")+
  geom_boxplot(width = 0.3)+
  theme_minimal()+ylab("Occupancy")+theme(legend.position = "none")+ylim(0,1)+coord_flip()

## labu
labu2018 <- exact_extract(px_BCR_labu, BCR_HUC12watershed, 'mean')
labu25Per_nonGr <- exact_extract(px_BCR_labu_NtE25Per_nonGr, BCR_HUC12watershed, 'mean')
labu50Per_nonGr <- exact_extract(px_BCR_labu_NtE50Per_nonGr, BCR_HUC12watershed, 'mean')
labu75Per_nonGr <- exact_extract(px_BCR_labu_NtE75Per_nonGr, BCR_HUC12watershed, 'mean')


labu_bWS<-bind_cols(labu2018,labu25Per_nonGr,labu50Per_nonGr,labu75Per_nonGr,.id = "id")
##rename for secnarios
labu_bWS<-rename(labu_bWS, Current=...1);labu_bWS<-rename(labu_bWS, NtE25Per=...2);
labu_bWS<-rename(labu_bWS, NtE50Per=...3);labu_bWS<-rename(labu_bWS, NtE75Per=...4);#labu_bWS<-rename(labu_bWS, Grassland4mil=...5);labu_bWS<-rename(labu_bWS, RegenAg2milGrassland2mil=...6);labu_bWS<-rename(labu_bWS, RegenAg8mil=...7)
labu_bWS<-labu_bWS %>% pivot_longer(!.id , names_to = "Scenario", values_to = "count")
labu_bWS %>% group_by(Scenario) %>% summarise(mean(count))
labu_med<-labu_bWS %>% group_by(Scenario) %>% summarise(median(count))
labu_pc<-mutate(labu_med, pct_change=(`median(count)`/(`median(count)`[1]) - 1)*100)
labu_pc
p_labu<-ggplot(labu_bWS, aes(y=count, x=Scenario, fill=Scenario))+ggtitle("Lark Bunting")+
  geom_boxplot(width = 0.3)+
  theme_minimal()+ylab("Occupancy")+theme(legend.position = "none")+ylim(0,1)+coord_flip()

## bobo
bobo2018 <- exact_extract(px_BCR_bobo, BCR_HUC12watershed, 'mean')
bobo25Per_nonGr <- exact_extract(px_BCR_bobo_NtE25Per_nonGr, BCR_HUC12watershed, 'mean')
bobo50Per_nonGr <- exact_extract(px_BCR_bobo_NtE50Per_nonGr, BCR_HUC12watershed, 'mean')
bobo75Per_nonGr <- exact_extract(px_BCR_bobo_NtE75Per_nonGr, BCR_HUC12watershed, 'mean')


bobo_bWS<-bind_cols(bobo2018,bobo25Per_nonGr,bobo50Per_nonGr,.id = "id")
##rename for secnarios
bobo_bWS<-rename(bobo_bWS, Current=...1);bobo_bWS<-rename(bobo_bWS, NtE25Per=...2);
bobo_bWS<-rename(bobo_bWS, NtE50Per=...3);#bobo_bWS<-rename(bobo_bWS, NtE75Per=...4);#bobo_bWS<-rename(bobo_bWS, Grassland4mil=...5);bobo_bWS<-rename(bobo_bWS, RegenAg2milGrassland2mil=...6);bobo_bWS<-rename(bobo_bWS, RegenAg8mil=...7)
bobo_bWS<-bobo_bWS %>% pivot_longer(!.id , names_to = "Scenario", values_to = "count")
bobo_bWS %>% group_by(Scenario) %>% summarise(mean(count))
bobo_med<-bobo_bWS %>% group_by(Scenario) %>% summarise(median(count))
bobo_pc<-mutate(bobo_med, pct_change=(`median(count)`/(`median(count)`[1]) - 1)*100)
bobo_pc

p_bobo<-ggplot(bobo_bWS, aes(y=count, x=Scenario, fill=Scenario))+ggtitle("Bobolink")+
  geom_boxplot(width = 0.3)+
  theme_minimal()+ylab("Occupancy")+theme(legend.position = "none")+ylim(0,1)+coord_flip()


## sppi
sppi2018 <- exact_extract(px_BCR_sppi, BCR_HUC12watershed, 'mean')
sppi25Per_nonGr <- exact_extract(px_BCR_sppi_NtE25Per_nonGr, BCR_HUC12watershed, 'mean')
sppi50Per_nonGr <- exact_extract(px_BCR_sppi_NtE50Per_nonGr, BCR_HUC12watershed, 'mean')
sppi75Per_nonGr <- exact_extract(px_BCR_sppi_NtE75Per_nonGr, BCR_HUC12watershed, 'mean')


sppi_bWS<-bind_cols(sppi2018,sppi25Per_nonGr,sppi50Per_nonGr,sppi75Per_nonGr,.id = "id")
##rename for secnarios
sppi_bWS<-rename(sppi_bWS, Current=...1);sppi_bWS<-rename(sppi_bWS, NtE25Per=...2);
sppi_bWS<-rename(sppi_bWS, NtE50Per=...3);sppi_bWS<-rename(sppi_bWS, NtE75Per=...4);#sppi_bWS<-rename(sppi_bWS, Grassland4mil=...5);sppi_bWS<-rename(sppi_bWS, RegenAg2milGrassland2mil=...6);sppi_bWS<-rename(sppi_bWS, RegenAg8mil=...7)
sppi_bWS<-sppi_bWS %>% pivot_longer(!.id , names_to = "Scenario", values_to = "count")
sppi_bWS %>% group_by(Scenario) %>% summarise(mean(count))
sppi_med<-sppi_bWS %>% group_by(Scenario) %>% summarise(median(count))
sppi_pc<-mutate(sppi_med, pct_change=(`median(count)`/(`median(count)`[1]) - 1)*100)
sppi_pc

p_sppi<-ggplot(sppi_bWS, aes(y=count, x=Scenario, fill=Scenario))+ggtitle("sppilink")+
  geom_boxplot(width = 0.3)+
  theme_minimal()+ylab("Occupancy")+theme(legend.position = "none")+ylim(0,1)+coord_flip()

## cclo
cclo2018 <- exact_extract(px_BCR_cclo, BCR_HUC12watershed, 'mean')
cclo25Per_nonGr <- exact_extract(px_BCR_cclo_NtE25Per_nonGr, BCR_HUC12watershed, 'mean')
cclo50Per_nonGr <- exact_extract(px_BCR_cclo_NtE50Per_nonGr, BCR_HUC12watershed, 'mean')
cclo75Per_nonGr <- exact_extract(px_BCR_cclo_NtE75Per_nonGr, BCR_HUC12watershed, 'mean')


cclo_bWS<-bind_cols(cclo2018,cclo25Per_nonGr,cclo50Per_nonGr,cclo75Per_nonGr,.id = "id")
##rename for secnarios
cclo_bWS<-rename(cclo_bWS, Current=...1);cclo_bWS<-rename(cclo_bWS, NtE25Per=...2);
cclo_bWS<-rename(cclo_bWS, NtE50Per=...3);cclo_bWS<-rename(cclo_bWS, NtE75Per=...4);#cclo_bWS<-rename(cclo_bWS, Grassland4mil=...5);cclo_bWS<-rename(cclo_bWS, RegenAg2milGrassland2mil=...6);cclo_bWS<-rename(cclo_bWS, RegenAg8mil=...7)
cclo_bWS<-cclo_bWS %>% pivot_longer(!.id , names_to = "Scenario", values_to = "count")
cclo_bWS %>% group_by(Scenario) %>% summarise(mean(count))
cclo_med<-cclo_bWS %>% group_by(Scenario) %>% summarise(median(count))
cclo_pc<-mutate(cclo_med, pct_change=(`median(count)`/(`median(count)`[1]) - 1)*100)
cclo_pc

p_cclo<-ggplot(cclo_bWS, aes(y=count, x=Scenario, fill=Scenario))+ggtitle("cclolink")+
  geom_boxplot(width = 0.3)+
  theme_minimal()+ylab("Occupancy")+theme(legend.position = "none")+ylim(0,1)+coord_flip()


## hesp
hesp2018 <- exact_extract(px_BCR_hesp, BCR_HUC12watershed, 'mean')
hesp25Per_nonGr <- exact_extract(px_BCR_hesp_NtE25Per_nonGr, BCR_HUC12watershed, 'mean')
hesp50Per_nonGr <- exact_extract(px_BCR_hesp_NtE50Per_nonGr, BCR_HUC12watershed, 'mean')
hesp75Per_nonGr <- exact_extract(px_BCR_hesp_NtE75Per_nonGr, BCR_HUC12watershed, 'mean')


hesp_bWS<-bind_cols(hesp2018,hesp25Per_nonGr,hesp50Per_nonGr,hesp75Per_nonGr,.id = "id")
##rename for secnarios
hesp_bWS<-rename(hesp_bWS, Current=...1);hesp_bWS<-rename(hesp_bWS, NtE25Per=...2);
hesp_bWS<-rename(hesp_bWS, NtE50Per=...3);hesp_bWS<-rename(hesp_bWS, NtE75Per=...4);#hesp_bWS<-rename(hesp_bWS, Grassland4mil=...5);hesp_bWS<-rename(hesp_bWS, RegenAg2milGrassland2mil=...6);hesp_bWS<-rename(hesp_bWS, RegenAg8mil=...7)
hesp_bWS<-hesp_bWS %>% pivot_longer(!.id , names_to = "Scenario", values_to = "count")
hesp_bWS %>% group_by(Scenario) %>% summarise(mean(count))
hesp_med<-hesp_bWS %>% group_by(Scenario) %>% summarise(median(count))
hesp_pc<-mutate(hesp_med, pct_change=(`median(count)`/(`median(count)`[1]) - 1)*100)
hesp_pc

p_hesp<-ggplot(hesp_bWS, aes(y=count, x=Scenario, fill=Scenario))+ggtitle("hesplink")+
  geom_boxplot(width = 0.3)+
  theme_minimal()+ylab("Occupancy")+theme(legend.position = "none")+ylim(0,1)+coord_flip()

## lbcu
lbcu2018 <- exact_extract(px_BCR_lbcu, BCR_HUC12watershed, 'mean')
lbcu25Per_nonGr <- exact_extract(px_BCR_lbcu_NtE25Per_nonGr, BCR_HUC12watershed, 'mean')
lbcu50Per_nonGr <- exact_extract(px_BCR_lbcu_NtE50Per_nonGr, BCR_HUC12watershed, 'mean')
lbcu75Per_nonGr <- exact_extract(px_BCR_lbcu_NtE75Per_nonGr, BCR_HUC12watershed, 'mean')


lbcu_bWS<-bind_cols(lbcu2018,lbcu25Per_nonGr,lbcu50Per_nonGr,lbcu75Per_nonGr,.id = "id")
##rename for secnarios
lbcu_bWS<-rename(lbcu_bWS, Current=...1);lbcu_bWS<-rename(lbcu_bWS, NtE25Per=...2);
lbcu_bWS<-rename(lbcu_bWS, NtE50Per=...3);lbcu_bWS<-rename(lbcu_bWS, NtE75Per=...4);#lbcu_bWS<-rename(lbcu_bWS, Grassland4mil=...5);lbcu_bWS<-rename(lbcu_bWS, RegenAg2milGrassland2mil=...6);lbcu_bWS<-rename(lbcu_bWS, RegenAg8mil=...7)
lbcu_bWS<-lbcu_bWS %>% pivot_longer(!.id , names_to = "Scenario", values_to = "count")
lbcu_bWS %>% group_by(Scenario) %>% summarise(mean(count))
lbcu_med<-lbcu_bWS %>% group_by(Scenario) %>% summarise(median(count))
lbcu_pc<-mutate(lbcu_med, pct_change=(`median(count)`/(`median(count)`[1]) - 1)*100)
lbcu_pc

p_lbcu<-ggplot(lbcu_bWS, aes(y=count, x=Scenario, fill=Scenario))+ggtitle("lbculink")+
  geom_boxplot(width = 0.3)+
  theme_minimal()+ylab("Occupancy")+theme(legend.position = "none")+ylim(0,1)+coord_flip()



library(dplyr)
AllSp_pc<-bind_rows(weme_pc,eame_pc,dick_pc,
                    grsp_pc,nobo_pc,upsp_pc,
                    labu_pc,bobo_pc, sppi_pc,
                    cclo_pc, hesp_pc, lbcu_pc,
                    eaki_pc,  bevi_pc,
                    .id = "Bird")
nrow(Species)
Species<-c(rep("Western Meadowlark",4),rep("Eastern Meadowlark",4),rep("Dickecissel",4),
           rep("Grasshopper Sparrow",4),rep("Northern Bobowhite",4),rep("Upland Sandpiper",4),
           rep("Lark Bunting",4),rep("Bobolink",3),rep("Sp Pipit", 4), 
           rep("Chestnut Collard Longspur", 4), rep("Henslow Sparrow",4), rep("Long-billed Curlew", 4),
           rep("Eastern Kingbird",4),rep("Bells Vireo",4)
)
Guild<-c(rep("Grassland", 47), rep("Edge/Woodland",4*2))

AllSp_pc<-bind_cols(AllSp_pc,Species,Guild)
AllSp_pc<-rename(AllSp_pc, Species=...5);AllSp_pc<-rename(AllSp_pc, Guild=...6);

ggplot(AllSp_pc, aes(y=pct_change, x=Scenario, group=Species, color=Species))+facet_wrap(vars(Guild))+
  geom_point(aes(shape=Guild,size=1))+stat_summary(geom="line") + ylab("% Increase")+theme_minimal()+theme(legend.position = "left")+
  #scale_color_brewer(palette="Set3")
  scale_color_manual(values=as.vector(alphabet(26)))
  #scale_colour_viridis (discrete=TRUE)  ggtitle("Response to Regnerative") 


pal.bands(alphabet, alphabet2, cols25, glasbey, kelly, polychrome, 
          stepped, tol, watlington,
          show.names=FALSE)
#OLD
##########################



weme2018 <- exact_extract(px_BCR_weme, BCR_HUC12watershed, 'mean')
weme2mRG <- exact_extract(px_BCR_weme_2mRG, BCR_HUC12watershed, 'mean')
weme2mGr <- exact_extract(px_BCR_weme_2mGr, BCR_HUC12watershed, 'mean')
weme4mRG <- exact_extract(px_BCR_weme_4mRG, BCR_HUC12watershed, 'mean')
weme4mGr <- exact_extract(px_BCR_weme_4mGr, BCR_HUC12watershed, 'mean')
weme2mGr2mRG <- exact_extract(px_BCR_weme_2mGr2mRg, BCR_HUC12watershed, 'mean')
weme8mRG <- exact_extract(px_BCR_weme_8mRG, BCR_HUC12watershed, 'mean')

eame2018 <- exact_extract(px_BCR_eame, BCR_HUC12watershed, 'mean')
eame2mRG <- exact_extract(px_BCR_eame_2mRG, BCR_HUC12watershed, 'mean')
eame2mGr <- exact_extract(px_BCR_eame_2mGr, BCR_HUC12watershed, 'mean')
eame4mRG <- exact_extract(px_BCR_eame_4mRG, BCR_HUC12watershed, 'mean')
eame4mGr <- exact_extract(px_BCR_eame_4mGr, BCR_HUC12watershed, 'mean')
eame2mGr2mRG <- exact_extract(px_BCR_eame_2mGr2mRg, BCR_HUC12watershed, 'mean')
eame8mRG <- exact_extract(px_BCR_eame_8mRG, BCR_HUC12watershed, 'mean')

dick2018 <- exact_extract(px_BCR_dick, BCR_HUC12watershed, 'mean')
dick2mRG <- exact_extract(px_BCR_dick_2mRG, BCR_HUC12watershed, 'mean')
dick2mGr <- exact_extract(px_BCR_dick_2mGr, BCR_HUC12watershed, 'mean')
dick4mRG <- exact_extract(px_BCR_dick_4mRG, BCR_HUC12watershed, 'mean')
dick4mGr <- exact_extract(px_BCR_dick_4mGr, BCR_HUC12watershed, 'mean')
dick2mGr2mRG <- exact_extract(px_BCR_dick_2mGr2mRg, BCR_HUC12watershed, 'mean')
dick8mRG <- exact_extract(px_BCR_dick_8mRG, BCR_HUC12watershed, 'mean')


nobo2018 <- exact_extract(px_BCR_nobo, BCR_HUC12watershed, 'mean')
nobo2mRG <- exact_extract(px_BCR_nobo_2mRG, BCR_HUC12watershed, 'mean')
nobo2mGr <- exact_extract(px_BCR_nobo_2mGr, BCR_HUC12watershed, 'mean')
nobo4mRG <- exact_extract(px_BCR_nobo_4mRG, BCR_HUC12watershed, 'mean')
nobo4mGr <- exact_extract(px_BCR_nobo_4mGr, BCR_HUC12watershed, 'mean')
nobo2mGr2mRG <- exact_extract(px_BCR_nobo_2mGr2mRg, BCR_HUC12watershed, 'mean')
nobo8mRG <- exact_extract(px_BCR_nobo_8mRG, BCR_HUC12watershed, 'mean')

bevi2018 <- exact_extract(px_BCR_bevi, BCR_HUC12watershed, 'mean')
bevi2mRG <- exact_extract(px_BCR_bevi_2mRG, BCR_HUC12watershed, 'mean')
bevi2mGr <- exact_extract(px_BCR_bevi_2mGr, BCR_HUC12watershed, 'mean')
bevi4mRG <- exact_extract(px_BCR_bevi_4mRG, BCR_HUC12watershed, 'mean')
bevi4mGr <- exact_extract(px_BCR_bevi_4mGr, BCR_HUC12watershed, 'mean')
bevi2mGr2mRG <- exact_extract(px_BCR_bevi_2mGr2mRg, BCR_HUC12watershed, 'mean')
bevi8mRG <- exact_extract(px_BCR_bevi_8mRG, BCR_HUC12watershed, 'mean')

grsp2018 <- exact_extract(px_BCR_grsp, BCR_HUC12watershed, 'mean')
grsp2mRG <- exact_extract(px_BCR_grsp_2mRG, BCR_HUC12watershed, 'mean')
grsp2mGr <- exact_extract(px_BCR_grsp_2mGr, BCR_HUC12watershed, 'mean')
grsp4mRG <- exact_extract(px_BCR_grsp_4mRG, BCR_HUC12watershed, 'mean')
grsp4mGr <- exact_extract(px_BCR_grsp_4mGr, BCR_HUC12watershed, 'mean')
grsp2mGr2mRG <- exact_extract(px_BCR_grsp_2mGr2mRg, BCR_HUC12watershed, 'mean')
grsp8mRG <- exact_extract(px_BCR_grsp_8mRG, BCR_HUC12watershed, 'mean')

eaki2018 <- exact_extract(px_BCR_eaki, BCR_HUC12watershed, 'mean')
eaki2mRG <- exact_extract(px_BCR_eaki_2mRG, BCR_HUC12watershed, 'mean')
eaki2mGr <- exact_extract(px_BCR_eaki_2mGr, BCR_HUC12watershed, 'mean')
eaki4mRG <- exact_extract(px_BCR_eaki_4mRG, BCR_HUC12watershed, 'mean')
eaki4mGr <- exact_extract(px_BCR_eaki_4mGr, BCR_HUC12watershed, 'mean')
eaki2mGr2mRG <- exact_extract(px_BCR_eaki_2mGr2mRg, BCR_HUC12watershed, 'mean')
eaki8mRG <- exact_extract(px_BCR_eaki_8mRG, BCR_HUC12watershed, 'mean')

witk2018 <- exact_extract(px_BCR_witk, BCR_HUC12watershed, 'mean')
witk2mRG <- exact_extract(px_BCR_witk_2mRG, BCR_HUC12watershed, 'mean')
witk2mGr <- exact_extract(px_BCR_witk_2mGr, BCR_HUC12watershed, 'mean')
witk4mRG <- exact_extract(px_BCR_witk_4mRG, BCR_HUC12watershed, 'mean')
witk4mGr <- exact_extract(px_BCR_witk_4mGr, BCR_HUC12watershed, 'mean')
witk2mGr2mRG <- exact_extract(px_BCR_witk_2mGr2mRg, BCR_HUC12watershed, 'mean')
witk8mRG <- exact_extract(px_BCR_witk_8mRG, BCR_HUC12watershed, 'mean')


save.image("C:/Users/jquinn2/Desktop/GMI SDM/GMI SDM BCR18 V2.RData") 

nobo_bWS<-bind_cols(nobo2018,nobo2mRG,nobo2mGr,nobo4mRG,nobo4mGr,nobo2mGr2mRG,nobo8mRG,.id = "id")
nobo_bWS<-rename(nobo_bWS, Current=...1);nobo_bWS<-rename(nobo_bWS, RegenAg2mil=...2);nobo_bWS<-rename(nobo_bWS, Grassland2mil=...3);nobo_bWS<-rename(nobo_bWS, RegenAg4mil=...4);nobo_bWS<-rename(nobo_bWS, Grassland4mil=...5);nobo_bWS<-rename(nobo_bWS, RegenAg2milGrassland2mil=...6);nobo_bWS<-rename(nobo_bWS, RegenAg8mil=...7)
nobo_bWS<-nobo_bWS %>% pivot_longer(!.id , names_to = "Scenario", values_to = "count")
nobo_bWS %>% group_by(Scenario) %>% summarise(mean(count))
nobo_bWS$Scenario <- factor(nobo_bWS$Scenario , levels=c("Current", "RegenAg2mil", "RegenAg4mil", "RegenAg8mil", "Grassland2mil", "Grassland4mil", "RegenAg2milGrassland2mil"))
p_nobo<-ggplot(nobo_bWS, aes(y=count, x=Scenario, fill=Scenario))+ggtitle("Northern Bobwhite")+
  #geom_violin(trim = T) + 
  geom_boxplot(width = 0.3)+ theme_minimal()+ylab("Mean Occupancy by Watershed")+
  theme(legend.position = "none")+coord_flip()+ylim(0,1)
p_nobo+scale_fill_manual(values=c("gray70","green2","forestgreen","darkgreen","gold","yellow","darkolivegreen2"))

grsp<-bind_cols(grsp2018,grsp2mRG,grsp2mGr,grsp4mRG,grsp4mGr,grsp2mGr2mRG,grsp8mRG,.id = "id")
grsp<-rename(grsp, Current=...1);grsp<-rename(grsp, RegenAg2mil=...2);grsp<-rename(grsp, Grassland2mil=...3);grsp<-rename(grsp, RegenAg4mil=...4);grsp<-rename(grsp, Grassland4mil=...5);grsp<-rename(grsp, RegenAg2milGrassland2mil=...6);grsp<-rename(grsp, RegenAg8mil=...7)
grsp<-grsp %>% pivot_longer(!.id , names_to = "Scenario", values_to = "count")
p_grsp<-ggplot(grsp, aes(y=count, x=Scenario, fill=Scenario))+ggtitle("Grasshopper Sparrow")+
  geom_boxplot(width = 0.3)+
  theme_minimal()+ylab("Occupancy")+theme(legend.position = "none")+ylim(0,1)+coord_flip()

weme_bWS<-bind_cols(weme2018,weme2mRG,weme2mGr,weme4mRG,weme4mGr,weme2mGr2mRG,weme8mRG,.id = "id")
weme_bWS<-rename(weme_bWS, Current=...1);weme_bWS<-rename(weme_bWS, RegenAg2mil=...2);weme_bWS<-rename(weme_bWS, Grassland2mil=...3);weme_bWS<-rename(weme_bWS, RegenAg4mil=...4);weme_bWS<-rename(weme_bWS, Grassland4mil=...5);weme_bWS<-rename(weme_bWS, RegenAg2milGrassland2mil=...6);weme_bWS<-rename(weme_bWS, RegenAg8mil=...7)
weme_bWS<-weme_bWS %>% pivot_longer(!.id , names_to = "Scenario", values_to = "count")
weme_bWS %>% group_by(Scenario) %>% summarise(mean(count))
p_weme<-ggplot(weme_bWS, aes(y=count, x=Scenario, fill=Scenario))+ggtitle("Western Meadowlark")+
  geom_boxplot(width = 0.3)+
  theme_minimal()+ylab("Occupancy")+theme(legend.position = "none")+ylim(0,1)+coord_flip()

dick_bWS<-bind_cols(dick2018,dick2mRG,dick2mGr,dick4mRG,dick4mGr,dick2mGr2mRG,dick8mRG,.id = "id")
dick_bWS<-rename(dick_bWS, Current=...1);dick_bWS<-rename(dick_bWS, RegenAg2mil=...2);dick_bWS<-rename(dick_bWS, Grassland2mil=...3);dick_bWS<-rename(dick_bWS, RegenAg4mil=...4);dick_bWS<-rename(dick_bWS, Grassland4mil=...5);dick_bWS<-rename(dick_bWS, RegenAg2milGrassland2mil=...6);dick_bWS<-rename(dick_bWS, RegenAg8mil=...7)
dick_bWS<-dick_bWS %>% pivot_longer(!.id , names_to = "Scenario", values_to = "count")
dick_bWS %>% group_by(Scenario) %>% summarise(mean(count))
p_dick<-ggplot(dick_bWS, aes(y=count, x=Scenario,fill=Scenario))+ggtitle("Dickcissel")+
  geom_boxplot(width = 0.3)+
  theme_minimal()+ylab("Occupancy")+theme(legend.position = "none")+ylim(0,1)+coord_flip()

bevi_bWS<-bind_cols(bevi2018,bevi2mRG,bevi2mGr,bevi4mRG,bevi4mGr,bevi2mGr2mRG,bevi8mRG,.id = "id")
bevi_bWS<-rename(bevi_bWS, Current=...1);bevi_bWS<-rename(bevi_bWS, RegenAg2mil=...2);bevi_bWS<-rename(bevi_bWS, Grassland2mil=...3);bevi_bWS<-rename(bevi_bWS, RegenAg4mil=...4);bevi_bWS<-rename(bevi_bWS, Grassland4mil=...5);bevi_bWS<-rename(bevi_bWS, RegenAg2milGrassland2mil=...6);bevi_bWS<-rename(bevi_bWS, RegenAg8mil=...7)
bevi_bWS<-bevi_bWS %>% pivot_longer(!.id , names_to = "Scenario", values_to = "count")
bevi_bWS %>% group_by(Scenario) %>% summarise(mean(count))
p_bevi<-ggplot(bevi_bWS, aes(y=count, x=Scenario,fill=Scenario))+ggtitle("Bell's Vireo")+
  geom_boxplot(width = 0.3)+
  theme_minimal()+ylab("Occupancy")+theme(legend.position = "none")+ylim(0,1)+coord_flip()

eame_bWS<-bind_cols(eame2018,eame2mRG,eame2mGr,eame4mRG,eame4mGr,eame2mGr2mRG,eame8mRG,.id = "id")
eame_bWS<-rename(eame_bWS, Current=...1);eame_bWS<-rename(eame_bWS, RegenAg2mil=...2);eame_bWS<-rename(eame_bWS, Grassland2mil=...3);eame_bWS<-rename(eame_bWS, RegenAg4mil=...4);eame_bWS<-rename(eame_bWS, Grassland4mil=...5);eame_bWS<-rename(eame_bWS, RegenAg2milGrassland2mil=...6);eame_bWS<-rename(eame_bWS, RegenAg8mil=...7)
eame_bWS<-eame_bWS %>% pivot_longer(!.id , names_to = "Scenario", values_to = "count")
eame_bWS %>% group_by(Scenario) %>% summarise(mean(count))
p_eame<-ggplot(eame_bWS, aes(y=count, x=Scenario,fill=Scenario))+ggtitle("Eastern Meadowlark")+
  geom_boxplot(width = 0.3)+theme_minimal()+
  ylab("Occupancy")+theme(legend.position = "none")+ylim(0,1)+coord_flip()

eaki_bWS<-bind_cols(eaki2018,eaki2mRG,eaki2mGr,eaki4mRG,eaki4mGr,eaki2mGr2mRG,eaki8mRG,.id = "id")
eaki_bWS<-rename(eaki_bWS, Current=...1);eaki_bWS<-rename(eaki_bWS, RegenAg2mil=...2);eaki_bWS<-rename(eaki_bWS, Grassland2mil=...3);eaki_bWS<-rename(eaki_bWS, RegenAg4mil=...4);eaki_bWS<-rename(eaki_bWS, Grassland4mil=...5);eaki_bWS<-rename(eaki_bWS, RegenAg2milGrassland2mil=...6);eaki_bWS<-rename(eaki_bWS, RegenAg8mil=...7)
eaki_bWS<-eaki_bWS %>% pivot_longer(!.id , names_to = "Scenario", values_to = "count")
eaki_bWS %>% group_by(Scenario) %>% summarise(mean(count))
p_eaki<-ggplot(eaki_bWS, aes(y=count, x=Scenario,fill=Scenario))+ggtitle("Eastern Kingbird")+
  geom_boxplot(width = 0.3)+theme_minimal()+
  ylab("Occupancy")+theme(legend.position = "none")+ylim(0,1)+coord_flip()

witk_bWS<-bind_cols(witk2018,witk2mRG,witk2mGr,witk4mRG,witk4mGr,witk2mGr2mRG,witk8mRG,.id = "id")
witk_bWS<-rename(witk_bWS, Current=...1);witk_bWS<-rename(witk_bWS, RegenAg2mil=...2);witk_bWS<-rename(witk_bWS, Grassland2mil=...3);witk_bWS<-rename(witk_bWS, RegenAg4mil=...4);witk_bWS<-rename(witk_bWS, Grassland4mil=...5);witk_bWS<-rename(witk_bWS, RegenAg2milGrassland2mil=...6);witk_bWS<-rename(witk_bWS, RegenAg8mil=...7)
witk_bWS<-witk_bWS %>% pivot_longer(!.id , names_to = "Scenario", values_to = "count")
witk_bWS %>% group_by(Scenario) %>% summarise(mean(count))
p_witk<-ggplot(witk_bWS, aes(y=count, x=Scenario,fill=Scenario))+ggtitle("Wild Turkey")+
  geom_boxplot(width = 0.3)+theme_minimal()+
  ylab("Occupancy")+theme(legend.position = "none")+ylim(0,1)+coord_flip()


