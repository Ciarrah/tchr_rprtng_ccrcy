##Multiple imputation: summary statistics and analyses

setwd("M:/projects/ieu2/p1/015/working/data/CB")

library(mice)  ; library(readr) ; library(VIM)

library(haven) ; library(RCurl) ; library(gdata) 

library(zoo)

#importing robust standard error function
url_robust="https://raw.githubusercontent.com/IsidoreBeautrelet/economictheoryblog/master/robust_summary.R"
eval(parse(text = getURL(url_robust, ssl.verifypeer = FALSE)),envir=.GlobalEnv)

'%!in%'=function(x,y)!('%in%'(x,y))

data_raw=read_csv("M:/projects/ieu2/p1/015/working/data/data_raw_190402.csv",col_types = cols(k2_maine = col_integer(), 
                                k2_mainm = col_integer(), k2_mains = col_integer(),k3_engta = col_integer(), k3_matta = col_integer(), 
                                k3_scita = col_integer()))

data_raw$mz024a=as.integer(data_raw$mz024a)
for (i in 1:nrow(data_raw))
{
  if (data_raw$mz024a[i]%in%c(1:8))
  {
    data_raw$mz024a[i]=data_raw$mz024a[i]+4
  }
  else 
  {
    data_raw$mz024a[i]=data_raw$mz024a[i]-8
  }
}

data_raw$kz021=as.factor(data_raw$kz021)
data_raw$b032=as.integer(data_raw$b032)
data_raw$k2_maine=as.numeric(data_raw$k2_maine)                                              ; data_raw$k2_mainm=as.numeric(data_raw$k2_mainm)                                              ; data_raw$k2_mains=as.numeric(data_raw$k2_mains)
data_raw$k2_engta=as.numeric(data_raw$k2_engta)                                              ; data_raw$k2_matta=as.numeric(data_raw$k2_matta)                                              ; data_raw$k2_scita=as.numeric(data_raw$k2_scita)
data_raw$k3_engta=as.numeric(data_raw$k3_engta)                                              ; data_raw$k3_matta=as.numeric(data_raw$k3_matta)                                              ; data_raw$k3_scita=as.numeric(data_raw$k3_scita)
data_raw$ks4_avptsent[which(data_raw$ks4_avptsent==0)]=NA                                    ; data_raw$ks4_cvap3aps[which(data_raw$ks4_cvap3aps==0)]=NA                                    ; data_raw$ks4_cvap2aps[which(data_raw$ks4_cvap2aps==0)]=NA
data_raw$k3_maine[which(data_raw$k3_maine=="Not collected"|data_raw$k3_maine=="No data")]=NA ; data_raw$k3_mainm[which(data_raw$k3_mainm=="Not collected"|data_raw$k3_mainm=="No data")]=NA ; data_raw$k3_mains[which(data_raw$k3_mains=="Not collected"|data_raw$k3_mains=="No data")]=NA
data_raw$se045=as.factor(data_raw$se045)
data_raw$se161b=as.numeric(data_raw$se161b)
data_raw$sf115=as.numeric(data_raw$sf115)
data_raw$sf351=as.factor(data_raw$sf351)
data_raw$sf352=as.factor(data_raw$sf352)
data_raw$mz028b=as.factor(data_raw$mz028b)
data_raw$c645a=as.factor(data_raw$c645a)
data_raw$c755=as.factor(data_raw$c755)
data_raw$c765=as.factor(data_raw$c765)
data_raw$c800=as.factor(data_raw$c800)
data_raw$c801=as.factor(data_raw$c801)
data_raw$c804=as.factor(data_raw$c804)
data_raw$j410=as.factor(data_raw$j410)


data_raw$kz021=factor(data_raw$kz021)
data_raw$kz021[which(data_raw$kz021%in%levels(data_raw$kz021)[3])]=NA
data_raw$kz021=factor(data_raw$kz021)


data_raw$k2_engta[which(data_raw$k2_engta=="W")]=0;data_raw$k2_matta[which(data_raw$k2_matta=="W")]=0;data_raw$k2_scita[which(data_raw$k2_scita=="W")]=0
data_raw$k2_engta[which(data_raw$k2_engta%in%levels(as.factor(data_raw$k2_engta))[c(1,8)])]=NA
data_raw$k2_matta[which(data_raw$k2_matta%in%levels(as.factor(data_raw$k2_matta))[c(1,8)])]=NA
data_raw$k2_scita[which(data_raw$k2_scita%in%levels(as.factor(data_raw$k2_scita))[c(1,7)])]=NA
data_raw$k3_engta[which(data_raw$k3_engta%in%levels(as.factor(data_raw$k3_engta))[c(1,10)])]=NA
data_raw$k3_matta[which(data_raw$k3_matta%in%levels(as.factor(data_raw$k3_matta))[c(1,10)])]=NA
data_raw$k3_scita[which(data_raw$k3_scita%in%levels(as.factor(data_raw$k3_scita))[c(1,10)])]=NA


data_raw$se045=as.factor(data_raw$se045)
data_raw$SEN=NA
data_raw$SEN[which(data_raw$se045%in%levels(data_raw$se045)[c(1,3,4,6)])]="Not statemented"
data_raw$SEN[which(data_raw$se045%in%levels(data_raw$se045)[2])]="Has a statement"
data_raw$SEN=as.factor(data_raw$SEN)
data_raw$se045=NULL


data_raw$se161b[which(data_raw$se161b%in%levels(as.factor(data_raw$se161b))[c(1,13)])]=NA


data_raw$sf351=factor(data_raw$sf351)
data_raw$sf351[which(data_raw$sf351%in%levels(data_raw$sf351)[c(3:6)])]=NA
data_raw$sf351=factor(data_raw$sf351)


data_raw$sf352=as.factor(data_raw$sf352)
data_raw$sf352[which(data_raw$sf352%in%levels(data_raw$sf352)[c(5:8)])]=NA
data_raw$sf352=factor(data_raw$sf352)


data_raw$mz028b[which(data_raw$mz028b%in%levels(data_raw$mz028b)[c(31:33)])]=NA
data_raw$mz028b[which(data_raw$mz028b%in%levels(data_raw$mz028b)[1])]=NA
data_raw$mz028b[which(data_raw$mz028b%in%levels(data_raw$mz028b)[2])]=NA
data_raw$mz028b=as.integer(data_raw$mz028b)


data_raw$c645a=factor(data_raw$c645a)
data_raw$c645a[which(data_raw$c645a%in%levels(data_raw$c645a)[4])]=NA
data_raw$c645a=factor(data_raw$c645a)


data_raw$c755=factor(data_raw$c755)
data_raw$c755[which(data_raw$c755%in%levels(data_raw$c755)[7])]=NA
data_raw$c755=factor(data_raw$c755)


data_raw$c765=factor(data_raw$c765)
data_raw$c765[which(data_raw$c765%in%levels(data_raw$c765)[7])]=NA
data_raw$c765=factor(data_raw$c765)


data_raw$c800=factor(data_raw$c800)
data_raw$c800[which(data_raw$c800%in%levels(data_raw$c800)[6])]=NA
data_raw$c800=factor(data_raw$c800)


data_raw$c801=factor(data_raw$c801)
data_raw$c801[which(data_raw$c801%in%levels(data_raw$c801)[6])]=NA
data_raw$c801=factor(data_raw$c801)


data_raw$c804=factor(data_raw$c804)
data_raw$c804[which(data_raw$c804%in%levels(data_raw$c804)[1])]=NA
data_raw$c804=factor(data_raw$c804)


data_raw$j410=factor(data_raw$j410)
data_raw$j410[which(data_raw$j410%in%levels(data_raw$j410)[1])]=NA
data_raw$j410=factor(data_raw$j410)


data_raw$k2_engta[which(data_raw$k2_engta%in%levels(as.factor(data_raw$k2_engta))[6])]=NA
data_raw$k2_matta[which(data_raw$k2_matta%in%levels(as.factor(data_raw$k2_matta))[6])]=NA
data_raw$k2_scita[which(data_raw$k2_scita%in%levels(as.factor(data_raw$k2_scita))[5])]=NA


data_raw[,20:25]=as.double(unlist(data_raw[,20:25]))        ;data_raw[,8:10]=as.numeric(unlist(data_raw[,8:10]));        data_raw[,11:13]=as.numeric(unlist(data_raw[,11:13]))
data_raw$AvTA_KS2=apply(data_raw[,8:10],1,mean,na.rm=TRUE)  ;data_raw$AvTA_KS3=apply(data_raw[,11:13],1,mean,na.rm=TRUE)
data_raw$AvTA_KS2=as.numeric(factor(data_raw$AvTA_KS2))     ;data_raw$AvTA_KS3=as.numeric(factor(data_raw$AvTA_KS3))

a=c(3,4.7,6.4,8.27,10.13,12,14,16,18,20,22,24,26,28,30,32,34)                            # Dummy variables
b=c(6.4,8.27,10.13,12,14,16,18,20,22,24,26,28,30,32,34,36,38,40,42,44,46,48)


for(i in 1:nrow(data_raw))
{
  c=data_raw$AvTA_KS2[i]
  data_raw$AvTA_KS2[i]=a[c]
}

for(i in 1:nrow(data_raw))
{
  c=data_raw$AvTA_KS3[i]
  data_raw$AvTA_KS3[i]=b[c]
}

data_raw$Bias_KS2=data_raw$Bias_KS3=NA
data_raw$Bias_KS2[which(data_raw$AvTA_KS2>0&data_raw$ks4_cvap2aps>0)]=data_raw$AvTA_KS2-data_raw$ks4_cvap2aps
data_raw$Bias_KS3[which(data_raw$AvTA_KS3>0&data_raw$ks4_cvap3aps>0)]=data_raw$AvTA_KS3-data_raw$ks4_cvap3aps
data_raw$Bias_KS2=round(data_raw$Bias_KS2,0);data_raw$Bias_KS3=round(data_raw$Bias_KS3,0)


e=mean(na.omit(data_raw$Bias_KS2));f=sd(na.omit(data_raw$Bias_KS2))
g=mean(na.omit(data_raw$Bias_KS3));h=sd(na.omit(data_raw$Bias_KS3))

for (i in 1:nrow(data_raw))
{
  if (is.na(data_raw$Bias_KS2[i])==F)
  {
    data_raw$Bias_KS2.S[i]=(data_raw$Bias_KS2[i]-e)/f
  }
  else if (is.na(data_raw$Bias_KS2[i]))
  {
    data_raw$Bias_KS2.S[i]=NA
  }
}
for (i in 1:nrow(data_raw))
{
  if (is.na(data_raw$Bias_KS3[i])==F)
  {
    data_raw$Bias_KS3.S[i]=(data_raw$Bias_KS3[i]-g)/h
  }
  else if (is.na(data_raw$Bias_KS3[i]))
  {
    data_raw$Bias_KS3.S[i]=NA
  }
}


data_raw=data_raw[-which(data_raw$c800!="White"),]                                                            # Remove non-white from the dataset
data_raw=data_raw[-which(data_raw$c801!="White"),] 
data_raw=data_raw[which(data_raw$c804=="White"),]


data_raw$scl_clss=NA                                                                                          # Create social class
data_raw$scl_clss[which(data_raw$c755%!in%levels(data_raw$c755)&data_raw$c765%!in%levels(data_raw$c755))]=NA
data_raw$c755[which(data_raw$c755=="Armed forces")]="III (non-manual)"                                        # Regroup armed forces
data_raw$c765[which(data_raw$c765=="Armed forces")]="III (non-manual)"
data_raw$c755=factor(data_raw$c755);data_raw$c765=factor(data_raw$c765)
data_raw$c755=as.numeric(data_raw$c755)
data_raw$c765=as.numeric(data_raw$c765)

for (i in 1:nrow(data_raw))
{
  data_raw$scl_clss[i]=min(data_raw$c755[i],data_raw$c765[i])
}

data_raw$scl_clss=factor(data_raw$scl_clss)
levels(data_raw$scl_clss)=c("I", "II", "III (manual)", "III (non-manual)", "IV", "V")

data_raw$SEN=relevel(data_raw$SEN,ref="Not statemented")
data_raw$j410=relevel(data_raw$j410,ref=">UKP400")
data_raw$sf352=relevel(data_raw$sf352,ref="10+ years")
data_raw$c645a=relevel(data_raw$c645a,ref="Degree")
data_raw$scl_clss=relevel(data_raw$scl_clss,ref="I")

data_raw[,c(4,8:13,17:19,38:40,47,48)]=NULL                                                   # Remove colinear variables and ethnicity

write.csv(data_raw,"data_raw_MI")

######################################################################################
# Imputation                                                                         #
######################################################################################

set.seed(1997)
imputed_dataset_1=mice(data_raw, m=10, maxit = 50)

densityplot(imputed_dataset_1,data=~Bias_KS2.S+Bias_KS3.S+mz024a+se161b+sf115+
            sf351+sf352+c645a+j410+SEN+scl_clss)

## Under MAR assumptions we expect the blue and purple plot distributions to be similar


######################################################################################
# Association estimates, table 1                                                     #
######################################################################################

Modelfit=with(imputed_dataset_1,lm(Bias_KS2.S~c645a))
summary(pool(Modelfit))[c(1,2)]
round(summary(pool(Modelfit),robust=TRUE),2)

Modelfit=with(imputed_dataset_1,lm(Bias_KS2.S~kz021))
summary(pool(Modelfit))[c(1,2)]
round(summary(pool(Modelfit),robust=TRUE),2)

Modelfit=with(imputed_dataset_1,lm(Bias_KS2.S~scl_clss))
summary(pool(Modelfit))[c(1,2)]
round(summary(pool(Modelfit),robust=TRUE),2)

Modelfit=with(imputed_dataset_1,lm(Bias_KS2.S~SEN))
summary(pool(Modelfit))[c(1,2)]
round(summary(pool(Modelfit),robust=TRUE),2)

Modelfit=with(imputed_dataset_1,lm(Bias_KS2.S~mz024a))
summary(pool(Modelfit))[c(1,2)]
round(summary(pool(Modelfit),robust=TRUE),2)

Modelfit=with(imputed_dataset_1,lm(Bias_KS2.S~j410))
summary(pool(Modelfit))[c(1,2)]
round(summary(pool(Modelfit),robust=TRUE),2)

Modelfit=with(imputed_dataset_1,lm(Bias_KS2.S~sf351))
summary(pool(Modelfit))[c(1,2)]
round(summary(pool(Modelfit),robust=TRUE),2)

Modelfit=with(imputed_dataset_1,lm(Bias_KS2.S~sf352))
summary(pool(Modelfit))[c(1,2)]
round(summary(pool(Modelfit),robust=TRUE),2)

Modelfit=with(imputed_dataset_1,lm(Bias_KS2.S~sf115))
summary(pool(Modelfit))[c(1,2)]
round(summary(pool(Modelfit),robust=TRUE),2)

######################################################################################
# Multivariable model, KS2                                                           #
######################################################################################

Modelfit=with(imputed_dataset_1,lm(Bias_KS2.S~c645a+kz021+scl_clss+SEN+mz024a+j410+sf351+sf352+sf115))
d=as.data.frame((summary(pool(Modelfit),robust=TRUE)))
write.csv(d,"KS2_rgrssn_ll.csv")

imputed_dataset_1$SEN=as.factor(imputed_dataset_1$SEN)
imputed_dataset_1$j410=as.factor(imputed_dataset_1$j410)
imputed_dataset_1$sf352=as.factor(imputed_dataset_1$sf352)
imputed_dataset_1$c645a=as.factor(imputed_dataset_1$c645a)
imputed_dataset_1$scl_clss=as.factor(imputed_dataset_1$scl_clss)
imputed_dataset_1$SEN=relevel(imputed_dataset_1$SEN,ref="Not statemented")
imputed_dataset_1$j410=relevel(imputed_dataset_1$j410,ref=">UKP400")
imputed_dataset_1$sf352=relevel(imputed_dataset_1$sf352,ref="10+ years")
imputed_dataset_1$c645a=relevel(imputed_dataset_1$c645a,ref="Degree")
imputed_dataset_1$scl_clss=relevel(imputed_dataset_1$scl_clss,ref="I")

Modelfit=with(imputed_dataset_1,lm(Bias_KS3.S~c645a))
summary(pool(Modelfit))[c(1,2)]
round(summary(pool(Modelfit),robust=TRUE),2)

Modelfit=with(imputed_dataset_1,lm(Bias_KS3.S~kz021))
summary(pool(Modelfit))[c(1,2)]
round(summary(pool(Modelfit),robust=TRUE),2)

Modelfit=with(imputed_dataset_1,lm(Bias_KS3.S~scl_clss))
summary(pool(Modelfit))[c(1,2)]
round(summary(pool(Modelfit),robust=TRUE),2)

Modelfit=with(imputed_dataset_1,lm(Bias_KS3.S~SEN))
summary(pool(Modelfit))[c(1,2)]
round(summary(pool(Modelfit),robust=TRUE),2)

Modelfit=with(imputed_dataset_1,lm(Bias_KS3.S~mz024a))
summary(pool(Modelfit))[c(1,2)]
round(summary(pool(Modelfit),robust=TRUE),2)

Modelfit=with(imputed_dataset_1,lm(Bias_KS3.S~j410))
summary(pool(Modelfit))[c(1,2)]
round(summary(pool(Modelfit),robust=TRUE),2)

######################################################################################
# Multivariable model, KS3                                                           #
######################################################################################

Modelfit=with(imputed_dataset_1,lm(Bias_KS3.S~c645a+kz021+scl_clss+SEN+mz024a+j410))
d=as.data.frame((summary(pool(Modelfit),robust=TRUE)))
write.csv(d,"KS3_rgrssn_ll.csv")

######################################################################################
# Association between teacher rating and achievement, table 2                        #
######################################################################################

Modelfit=with(imputed_dataset_1,lm(AvTA_KS2~ks4_cvap2aps))
pool.r.squared(Modelfit,adjusted=T)
round(summary(pool(Modelfit), conf.int = TRUE), 2)

Modelfit=with(imputed_dataset_1,lm(AvTA_KS3~ks4_cvap3aps))
pool.r.squared(Modelfit,adjusted=T)
round(summary(pool(Modelfit), conf.int = TRUE), 2)

