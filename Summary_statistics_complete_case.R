
# SUMMARY STATISTICS

table(data_raw$sex)
round((table(data_raw$sex)/nrow(data_raw))*100,2)

summary(data_raw.ks2$AvTA)
round(sd(data_raw.ks2$AvTA),2)
summary(data_raw.ks3$AvTA)
round(sd(data_raw.ks3$AvTA),2)

mean(data_raw$mnth_dlvry)
sd(data_raw$mnth_dlvry)

length(which(data_raw$ks2_Av>0))
data_raw$ks2_Av=as.double(data_raw$ks2_Av)
round(mean(data_raw$ks2_Av[which(data_raw$ks2_Av>0)]),2)
round(sd(data_raw$ks2_Av[which(data_raw$ks2_Av>0)]),2)

length(which(data_raw$ks3_Av>0))
data_raw$ks3_Av=as.double(data_raw$ks3_Av)
round(mean(data_raw$ks3_Av[which(data_raw$ks3_Av>0)]),2)
round(sd(data_raw$ks3_Av[which(data_raw$ks3_Av>0)]),2)

table(data_raw$SEN.F)
round((table(data_raw$SEN.F)/nrow(data_raw))*100,2)

data_raw$mthr_HE=as.factor(data_raw$mthr_HE)
table(data_raw$mthr_HE)
round((table(data_raw$mthr_HE)/nrow(data_raw))*100,2)

data_raw$scl_clss=factor(data_raw$scl_clss)
table(data_raw$scl_clss)
round((table(data_raw$scl_clss)/nrow(data_raw))*100,2)

data_raw$incm_wk=as.factor(data_raw$incm_wk)
table(data_raw$incm_wk)
round((table(data_raw$incm_wk)/nrow(data_raw))*100,2)

length(data_raw$nmbr_ppls[which(data_raw$nmbr_ppls>0)])
round(mean(data_raw$nmbr_ppls[which(data_raw$nmbr_ppls>0)]),2)
round(sd(data_raw$nmbr_ppls[which(data_raw$nmbr_ppls>0)]),2)

data_raw$tchr_sx=as.factor(data_raw$tchr_sx)
length(which(data_raw$tchr_sx%in%levels(data_raw$tchr_sx)[c(1,2)]))
table(data_raw$tchr_sx)
round(((summary(data_raw$tchr_sx)[c(1,2)]*100)/length(which(data_raw$tchr_sx%in%levels(data_raw$tchr_sx)[c(1,2)]))),2)

data_raw$lngth_srv=as.factor(data_raw$lngth_srv)
length(which(data_raw$lngth_srv%in%levels(data_raw$lngth_srv)[c(1:4)]))
table(data_raw$lngth_srv)
round((summary(data_raw$lngth_srv)[c(1:4)]*100)/length(which(data_raw$lngth_srv%in%levels(data_raw$lngth_srv)[c(1:4)])),2)
