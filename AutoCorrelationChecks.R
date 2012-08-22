# Script for EDA Descriptive Stats
# SVN: $Id$

# Export table to LaTeX support 
library(xtable)

INSTALL_DIR<-'/Users/rob'

event.horizon<-10
window.size<-5
sequence<-1

# BSG.results.dir<-paste(sep='',INSTALL_DIR,'/PhDStuff/ThesisSoftware/BaseSetGenerator/output_',event.horizon)
# 
# setwd(BSG.results.dir)
# 
# input.BDS<-paste(sep='','BDS_',event.horizon,'_',window.size,'.csv')
# all.cases<-read.table(input.BDS,header=T, sep=",", na.strings="NA", dec=".", strip.white=TRUE)



TTG_DIR<-paste(sep='',INSTALL_DIR,'/PhDStuff/ThesisSoftware/TrgTestGenerator/output_',event.horizon)

trg.filename<-paste(sep='',TTG_DIR,'/TTG_Training_',event.horizon,'_',window.size,'_seq_',sequence,'.csv')
all.trg<-read.table(trg.filename,header=T, sep=",", na.strings="NA", dec=".", strip.white=TRUE)

attach(all.trg)
spot.data<-data.frame(HRT_spot_10_5,BPs_spot_10_5,BPd_spot_10_5,BPm_spot_10_5,BPp_spot_10_5)
colnames(spot.data)<-c('HRT_spot','BPs_spot','BPd_spot','BPm_spot','BPp_spot')
mean.data<-data.frame(HRT_mean_10_5,BPs_mean_10_5,BPd_mean_10_5,BPm_mean_10_5,BPp_mean_10_5)
colnames(mean.data)<-c('HRT_mean','BPs_mean','BPd_mean','BPm_mean','BPp_mean')
sd.data<-data.frame(HRT_sd_10_5,BPs_sd_10_5,BPd_sd_10_5,BPm_sd_10_5,BPp_sd_10_5)
colnames(sd.data)<-c('HRT_sd','BPs_sd','BPd_sd','BPm_sd','BPp_sd')
slope.data<-data.frame(HRT_slope_10_5,BPs_slope_10_5,BPd_slope_10_5,BPm_slope_10_5,BPp_slope_10_5)
colnames(slope.data)<-c('HRT_slope','BPs_slope','BPd_slope','BPm_slope','BPp_slope')

# By type of stats process
xtable(cor(na.omit(spot.data)))
xtable(cor(na.omit(mean.data)))
xtable(cor(na.omit(sd.data)))
xtable(cor(na.omit(slope.data)))

# By type of physiological signal
HRT.data<-data.frame(HRT_spot_10_5,HRT_mean_10_5,HRT_sd_10_5,HRT_slope_10_5)
colnames(HRT.data)<-c('HRT_spot','HRT_mean','HRT_sd','HRT_slope')
BPs.data<-data.frame(BPs_spot_10_5,BPs_mean_10_5,BPs_sd_10_5,BPs_slope_10_5)
colnames(BPs.data)<-c('BPs_spot','BPs_mean','BPs_sd','BPs_slope')
BPd.data<-data.frame(BPd_spot_10_5,BPd_mean_10_5,BPd_sd_10_5,BPd_slope_10_5)
colnames(BPd.data)<-c('BPd_spot','BPd_mean','BPd_sd','BPd_slope')
BPm.data<-data.frame(BPm_spot_10_5,BPm_mean_10_5,BPm_sd_10_5,BPm_slope_10_5)
colnames(BPm.data)<-c('BPm_spot','BPm_mean','BPm_sd','BPm_slope')
BPp.data<-data.frame(BPp_spot_10_5,BPp_mean_10_5,BPp_sd_10_5,BPp_slope_10_5)
colnames(BPp.data)<-c('BPp_spot','BPp_mean','BPp_sd','BPp_slope')

xtable(cor(na.omit(HRT.data)))
xtable(cor(na.omit(BPs.data)))
xtable(cor(na.omit(BPd.data)))
xtable(cor(na.omit(BPm.data)))
xtable(cor(na.omit(BPp.data)))

min.data<-data.frame(HRT_sd_10_5,BPm_sd_10_5,BPm_mean_10_5)
colnames(min.data)<-c('HRT_sd','BPm_sd','BPm_mean')
xtable(cor(na.omit(min.data)))

detach(all.trg)

