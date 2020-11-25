##Summary statistics for ALSPAC participants included in the study

setwd("M:/projects/ieu2/p1/015/working/data/CB")

library(readr)

'%!in%'=function(x,y)!('%in%'(x,y))

data_raw=read_csv("M:/projects/ieu2/p1/015/working/data/data_raw_190402.csv", 
                  col_types = cols(k2_maine = col_integer(), 
                                   k2_mainm = col_integer(), k2_mains = col_integer(), 
                                   k3_engta = col_integer(), k3_matta = col_integer(), 
                                   k3_scita = col_integer()))

colnames(data_raw)=c("preg_iden", "birth_ord", "sex","month_birth","ks2_main_test_E","ks2_main_test_M","ks2_main_test_S",
                     "ks2_TA_E","ks2_TA_M","ks2_TA_S","ks3_TA_E","ks3_TA_M","ks3_TA_S","ks2_Av","ks3_Av","ks4_Av",
                     "ks3_main_E","ks3_main_M","ks3_main_S","ks3_engtst","ks3_mattest","ks3_scitest","ks2_attain_eng",
                     "ks2_attain_mat","ks2_attain_sci","pgs_gwas","pgs_all","SEN","ks2_SDQ","nmbr_ppls","tchr_sx",
                     "lngth_srv","mnth_dlvry","mthr_age","parity","mthr_HE","mthr_scl_clss","fthr_scl_clss","ethncty","ptnrs_ethnc_grp",
                     "ethnc_bckgrnd","incm_wk","av_inc","ln(av_inc_)")


data_raw$ethncty=as.factor(data_raw$ethncty)
data_raw=data_raw[which(data_raw$ethncty==levels(data_raw$ethncty)[10]),]
data_raw=data_raw[which(data_raw$ethnc_bckgrnd=="White"),]


data_raw$sex=as.factor(data_raw$sex)
length(which(data_raw$sex==levels(data_raw$sex)[1]))/length(which(data_raw$sex%in%levels(data_raw$sex)[c(1,2)]))
length(which(data_raw$sex==levels(data_raw$sex)[2]))/length(which(data_raw$sex%in%levels(data_raw$sex)[c(1,2)]))
length(which(data_raw$sex==levels(data_raw$sex)[1]))
length(which(data_raw$sex==levels(data_raw$sex)[2]))


data_raw$mnth_dlvry=as.integer(data_raw$mnth_dlvry)
length(which(data_raw$mnth_dlvry%in%c(1:12)))

for (i in 1:nrow(data_raw))
{
  if (data_raw$mnth_dlvry[i]%in%c(1:8))
  {
    data_raw$mnth_dlvry[i]=data_raw$mnth_dlvry[i]+4
  }
  else if (data_raw$mnth_dlvry[i]%in%c(9:12))
  {
    data_raw$mnth_dlvry[i]=data_raw$mnth_dlvry[i]-8
  }
  else 
    data_raw$mnth_dlvry[i]=data_raw$mnth_dlvry[i]
}

mean(data_raw$mnth_dlvry[which(data_raw$mnth_dlvry%in%c(1:12))])
sd(data_raw$mnth_dlvry[which(data_raw$mnth_dlvry%in%c(1:12))])


length(which(data_raw$ks2_Av>0))
data_raw$ks2_Av=as.double(data_raw$ks2_Av)
round(mean(data_raw$ks2_Av[which(data_raw$ks2_Av>0)]),2)
round(sd(data_raw$ks2_Av[which(data_raw$ks2_Av>0)]),2)


length(which(data_raw$ks3_Av>0))
data_raw$ks3_Av=as.double(data_raw$ks3_Av)
round(mean(data_raw$ks3_Av[which(data_raw$ks3_Av>0)]),2)
round(sd(data_raw$ks3_Av[which(data_raw$ks3_Av>0)]),2)


data_raw$SEN.F=NA
data_raw$SEN=as.factor(data_raw$SEN)

for (i in 1:nrow(data_raw))
{
  if (data_raw$SEN[i]%!in%levels(data_raw$SEN))
  {
    data_raw$SEN.F[i]="Not statemented"
  }
  else if (data_raw$SEN[i]==levels(data_raw$SEN)[1])
  {
    data_raw$SEN.F[i]="Not statemented"
  }
  else if (data_raw$SEN[i]==levels(data_raw$SEN)[2])
  {
    data_raw$SEN.F[i]="Has a statement"
  }
  else if (data_raw$SEN[i]==levels(data_raw$SEN)[3])
  {
    data_raw$SEN.F[i]="Not statemented"
  }
  else if (data_raw$SEN[i]==levels(data_raw$SEN)[4])
  {
    data_raw$SEN.F[i]="Not statemented"
  }
  else if (data_raw$SEN[i]==levels(data_raw$SEN)[5])
  {
    data_raw$SEN.F[i]="Not statemented"
  }
  else if (data_raw$SEN[i]==levels(data_raw$SEN)[6])
  {
    data_raw$SEN.F[i]="Not statemented"
  }
  else if (data_raw$SEN[i]==levels(data_raw$SEN)[7])
  {
    data_raw$SEN.F[i]="Not statemented"
  }
  else if (data_raw$SEN[i]==levels(data_raw$SEN)[8])
  {
    data_raw$SEN.F[i]="Not statemented"
  }
}

data_raw$SEN.F=as.factor(data_raw$SEN.F)

length(which(data_raw$SEN.F%in%levels(data_raw$SEN.F)[c(1)]))/length(which(data_raw$SEN.F%in%levels(data_raw$SEN.F)))*100
length(which(data_raw$SEN.F%in%levels(data_raw$SEN.F)[c(1)]))
length(which(data_raw$SEN.F%in%levels(data_raw$SEN.F)[c(2)]))/length(which(data_raw$SEN.F%in%levels(data_raw$SEN.F)))*100
length(which(data_raw$SEN.F%in%levels(data_raw$SEN.F)[c(2)]))


data_raw$mthr_HE=as.factor(data_raw$mthr_HE)
a=length(which(data_raw$mthr_HE%in%levels(data_raw$mthr_HE)[c(4)]))                                         # Remove missing participants

length(which(data_raw$mthr_HE%in%levels(data_raw$mthr_HE)[c(1)]))/(length(which(data_raw$mthr_HE%in%levels(data_raw$mthr_HE)))-a)*100
length(which(data_raw$mthr_HE%in%levels(data_raw$mthr_HE)[c(1)]))
length(which(data_raw$mthr_HE%in%levels(data_raw$mthr_HE)[c(2)]))/(length(which(data_raw$mthr_HE%in%levels(data_raw$mthr_HE)))-a)*100
length(which(data_raw$mthr_HE%in%levels(data_raw$mthr_HE)[c(2)]))
length(which(data_raw$mthr_HE%in%levels(data_raw$mthr_HE)[c(3)]))/(length(which(data_raw$mthr_HE%in%levels(data_raw$mthr_HE)))-a)*100
length(which(data_raw$mthr_HE%in%levels(data_raw$mthr_HE)[c(3)]))
length(which(data_raw$mthr_HE%in%levels(data_raw$mthr_HE)[c(5)]))/(length(which(data_raw$mthr_HE%in%levels(data_raw$mthr_HE)))-a)*100
length(which(data_raw$mthr_HE%in%levels(data_raw$mthr_HE)[c(5)]))
length(which(data_raw$mthr_HE%in%levels(data_raw$mthr_HE)[c(6)]))/(length(which(data_raw$mthr_HE%in%levels(data_raw$mthr_HE)))-a)*100
length(which(data_raw$mthr_HE%in%levels(data_raw$mthr_HE)[c(6)]))


data_raw$scl_clss=0                                                                                        # Using highest parental social class
colnames(data_raw)[37:38]=c("mthr_scl_clss","fthr_scl_clss")
data_raw$mthr_scl_clss=as.factor(data_raw$mthr_scl_clss)
data_raw$fthr_scl_clss=as.factor(data_raw$fthr_scl_clss)
data_raw=data_raw[which(data_raw$fthr_scl_clss!=-1|data_raw$mthr_scl_clss!=-1),]

for (i in 1:nrow(data_raw))
{
  if (is.na(data_raw$mthr_scl_clss[i]))
  {
    data_raw$mthr_scl_clss[i]=NA
  }
  
  else if (data_raw$mthr_scl_clss[i]==-1)
  {
    data_raw$scl_clss[i]=data_raw$fthr_scl_clss[i]
  }
  else if ((data_raw$fthr_scl_clss[i]==-1)&(data_raw$mthr_scl_clss[i]>0))
  {
    data_raw$scl_clss[i]=data_raw$mthr_scl_clss[i]
  }
  else if ((data_raw$fthr_scl_clss[i]>0)&(data_raw$mthr_scl_clss[i]>0))
  {
    data_raw$scl_clss[i]=data_raw$fthr_scl_clss[i]
  }
}

data_raw=data_raw[which(data_raw$scl_clss%in%c("1","2","3","4","5","6","65")),]

data_raw$scl_clss=factor(data_raw$scl_clss,labels=c("I","II","III (manual)","III (non-manual)","IV","V","Armed forces"))
data_raw$scl_clss[which(data_raw$scl_clss=="Armed forces")]="III (non-manual)"
data_raw$scl_clss=factor(data_raw$scl_clss)
table(data_raw$scl_clss)
round((table(data_raw$scl_clss)/nrow(data_raw))*100,2)


data_raw$incm_wk=as.factor(data_raw$incm_wk)
data_raw$incm_wk[which(data_raw$incm_wk==-1)]=NA
(summary(data_raw$incm_wk)*100)/length(which(data_raw$incm_wk%in%levels(data_raw$incm_wk[c(2:6)]))) #can ignore NA


data_raw$nmbr_ppls=as.integer(data_raw$nmbr_ppls)
length(data_raw$nmbr_ppls[which(data_raw$nmbr_ppls>0)])
mean(data_raw$nmbr_ppls[which(data_raw$nmbr_ppls>0)])
sd(data_raw$nmbr_ppls[which(data_raw$nmbr_ppls>0)])


data_raw$tchr_sx=as.factor(data_raw$tchr_sx)
length(which(data_raw$tchr_sx%in%levels(data_raw$tchr_sx)[c(1,2)]))
(summary(data_raw$tchr_sx)[c(1,2)]*100)/length(which(data_raw$tchr_sx%in%levels(data_raw$tchr_sx)[c(1,2)]))


data_raw$lngth_srv=as.factor(data_raw$lngth_srv)
length(which(data_raw$lngth_srv%in%levels(data_raw$lngth_srv)[c(1:4)]))
(summary(data_raw$lngth_srv)[c(1:4)]*100)/length(which(data_raw$lngth_srv%in%levels(data_raw$lngth_srv)[c(1:4)]))

# Create subsets of ks2 and ks3 to derive assessment grades

data_raw.ks2=data_raw[which(data_raw$ks2_TA_E%in%c(1:6,"W")&data_raw$ks2_TA_S%in%c(1:6,"W")&
                              data_raw$ks2_TA_M%in%c(1:6,"W")),]
data_raw.ks2=data_raw.ks2[which(data_raw.ks2$ks2_Av>0),]

data_raw.ks3=data_raw[which(data_raw$ks3_TA_E%in%c(1:8,"W")&data_raw$ks3_TA_S%in%c(1:8,"W")&
                              data_raw$ks3_TA_M%in%c(1:8,"W")),]
data_raw.ks3=data_raw.ks3[which(data_raw.ks3$ks3_Av>0),]

# Creating TA variable, fine point score

data_raw$ks2_TA_E=as.numeric(data_raw$ks2_TA_E)
data_raw$ks2_TA_M=as.numeric(data_raw$ks2_TA_M)
data_raw$ks2_TA_S=as.numeric(data_raw$ks2_TA_S)

data_raw$ks3_TA_E=as.numeric(data_raw$ks3_TA_E)
data_raw$ks3_TA_M=as.numeric(data_raw$ks3_TA_M)
data_raw$ks3_TA_S=as.numeric(data_raw$ks3_TA_S)

data_raw.ks2[,8:10]=as.numeric(unlist(data_raw.ks2[,8:10]))
data_raw.ks2$AvTA=rowMeans(data_raw.ks2[,8:10],na.rm=TRUE)

data_raw.ks3[,11:13]=as.numeric(unlist(data_raw.ks3[,11:13]))
data_raw.ks3$AvTA=rowMeans(data_raw.ks3[,11:13],na.rm=TRUE)

data_raw.ks2$AvTA=as.numeric(factor(data_raw.ks2$AvTA))
data_raw.ks3$AvTA=as.numeric(factor(data_raw.ks3$AvTA))

a=c(3,4.7,6.4,8.27,10.13,12,14,16,18,20,22,24,26,28,30,32,34)
b=c(6.4,8.27,10.13,12,14,16,18,20,22,24,26,28,30,32,34,36,38,40,42,44,46,48)

for(i in 1:nrow(data_raw.ks2))
{
  c=data_raw.ks2$AvTA[i]
  data_raw.ks2$AvTA[i]=a[c]
}
for(i in 1:nrow(data_raw.ks3))
{
  c=data_raw.ks3$AvTA[i]
  data_raw.ks3$AvTA[i]=b[c]
}

mean(data_raw.ks2$AvTA)
sd(data_raw.ks2$AvTA)

mean(data_raw.ks3$AvTA)
sd(data_raw.ks3$AvTA)
