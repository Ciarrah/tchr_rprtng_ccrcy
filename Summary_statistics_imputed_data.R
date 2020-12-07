setwd("//rdsfcifs.acrc.bris.ac.uk/MRC-IEU-research/projects/ieu2/p1/015/working/data/CB")

library(mice)  ; library(readr) ; library(VIM)

library(haven) ; library(RCurl) ; library(gdata) 

library(zoo)

'%!in%'=function(x,y)!('%in%'(x,y))

data_raw=read_csv("data_raw_MI")

set.seed(1997)

imputed_dataset_1=mice(data_raw, m=10, maxit = 50)

completeddata=complete(imputed_dataset_1,action="all")


colnames(completeddata)=c("ID","birthorder","sex","KS2_Eng_lvl","KS2_Mat_lvl","KS2_Sci_lvl","KS2_av_pt","KS3_av_pt","KS4_av_pt","KS3_Eng_lvl",
                       "KS3_Mat_lvl","KS3_Sci_lvl","KS3_eng.2","KS3_mat.2", "KS3_sci.2","GWAS","PGS","SDQ_score","Nmbr_ppls","Tchr_sx","Lngth_srv","Mnth_brth",
                       "Mthr_age","Parity","Mthr_HE", "Scl_clss_mat","Scl_clss_pat","incm_wk","Av_inc","Log_av_inc","SEN","KS2_Av_TA",
                       "KS3_Av_TA","Bias_KS2.S","Bias_KS3.S","Scl_clss")

######################################################################################
## Summary statistics from one of the imputed datasets                               #
######################################################################################

library(readr)
completeddata=read_csv("//rdsfcifs.acrc.bris.ac.uk/MRC-IEU-research/projects/ieu2/p1/015/working/data/CB/completedexampledataset.csv")
completeddata[,1]=NULL

colnames(completeddata)=c("ID","birthorder","sex","KS2_Eng_lvl","KS2_Mat_lvl","KS2_Sci_lvl","KS2_av_pt","KS3_av_pt","KS4_av_pt","KS3_Eng_lvl",
                       "KS3_Mat_lvl","KS3_Sci_lvl","KS3_eng.2","KS3_mat.2", "KS3_sci.2","GWAS","PGS","SDQ_score","Nmbr_ppls","Tchr_sx","Lngth_srv","Mnth_brth",
                       "Mthr_age","Parity","Mthr_HE", "Scl_clss_mat","Scl_clss_pat","incm_wk","Av_inc","Log_av_inc","SEN","KS2_Av_TA",
                       "KS3_Av_TA","Bias_KS2.S","Bias_KS3.S","Scl_clss")

completeddata$sex=as.factor(completeddata$sex)

length(which(completeddata$sex==levels(completeddata$sex)[1]))/nrow(completeddata)
length(which(completeddata$sex==levels(completeddata$sex)[2]))/nrow(completeddata)
length(which(completeddata$sex==levels(completeddata$sex)[1]))
length(which(completeddata$sex==levels(completeddata$sex)[2]))

length(which(completeddata$Mnth_brth%in%c(1:12)))
mean(completeddata$Mnth_brth);sd(completeddata$Mnth_brth)

length(which(completeddata$KS2_Av_TA>0))
mean(completeddata$KS2_Av_TA);sd(completeddata$KS2_Av_TA)

length(which(completeddata$KS3_Av_TA>0))
mean(completeddata$KS3_Av_TA);sd(completeddata$KS3_Av_TA)

length(which(completeddata$KS2_av_pt>0))
mean(completeddata$KS2_av_pt);sd(completeddata$KS2_av_pt)

length(which(completeddata$KS3_av_pt>0))
mean(completeddata$KS3_av_pt);sd(completeddata$KS3_av_pt)

completeddata$SEN=as.factor(completeddata$SEN)
length(which(completeddata$SEN=="Has a statement"))/nrow(completeddata)
length(which(completeddata$SEN=="Not statemented"))/nrow(completeddata)

completeddata$Mthr_HE=as.factor(completeddata$Mthr_HE)
length(which(completeddata$Mthr_HE==levels(completeddata$Mthr_HE)[1]))/length(which(completeddata$Mthr_HE%in%levels(completeddata$Mthr_HE)))
length(which(completeddata$Mthr_HE==levels(completeddata$Mthr_HE)[2]))/length(which(completeddata$Mthr_HE%in%levels(completeddata$Mthr_HE)))
length(which(completeddata$Mthr_HE==levels(completeddata$Mthr_HE)[3]))/length(which(completeddata$Mthr_HE%in%levels(completeddata$Mthr_HE)))
length(which(completeddata$Mthr_HE==levels(completeddata$Mthr_HE)[4]))/length(which(completeddata$Mthr_HE%in%levels(completeddata$Mthr_HE)))
length(which(completeddata$Mthr_HE==levels(completeddata$Mthr_HE)[5]))/length(which(completeddata$Mthr_HE%in%levels(completeddata$Mthr_HE)))
length(which(completeddata$Mthr_HE==levels(completeddata$Mthr_HE)[1]))
length(which(completeddata$Mthr_HE==levels(completeddata$Mthr_HE)[2]))
length(which(completeddata$Mthr_HE==levels(completeddata$Mthr_HE)[3]))
length(which(completeddata$Mthr_HE==levels(completeddata$Mthr_HE)[4]))
length(which(completeddata$Mthr_HE==levels(completeddata$Mthr_HE)[5]))

completeddata$Scl_clss=as.factor(completeddata$Scl_clss)
length(which(completeddata$Scl_clss==levels(completeddata$Scl_clss)[1]))/length(which(completeddata$Scl_clss%in%levels(completeddata$Scl_clss)))
length(which(completeddata$Scl_clss==levels(completeddata$Scl_clss)[2]))/length(which(completeddata$Scl_clss%in%levels(completeddata$Scl_clss)))
length(which(completeddata$Scl_clss==levels(completeddata$Scl_clss)[3]))/length(which(completeddata$Scl_clss%in%levels(completeddata$Scl_clss)))
length(which(completeddata$Scl_clss==levels(completeddata$Scl_clss)[4]))/length(which(completeddata$Scl_clss%in%levels(completeddata$Scl_clss)))
length(which(completeddata$Scl_clss==levels(completeddata$Scl_clss)[5]))/length(which(completeddata$Scl_clss%in%levels(completeddata$Scl_clss)))
length(which(completeddata$Scl_clss==levels(completeddata$Scl_clss)[6]))/length(which(completeddata$Scl_clss%in%levels(completeddata$Scl_clss)))
length(which(completeddata$Scl_clss==levels(completeddata$Scl_clss)[1]))
length(which(completeddata$Scl_clss==levels(completeddata$Scl_clss)[2]))
length(which(completeddata$Scl_clss==levels(completeddata$Scl_clss)[3]))
length(which(completeddata$Scl_clss==levels(completeddata$Scl_clss)[4]))
length(which(completeddata$Scl_clss==levels(completeddata$Scl_clss)[5]))
length(which(completeddata$Scl_clss==levels(completeddata$Scl_clss)[6]))

completeddata$incm_wk=as.factor(completeddata$incm_wk)
length(which(completeddata$incm_wk==levels(completeddata$incm_wk)[1]))/length(which(completeddata$incm_wk%in%levels(completeddata$incm_wk)))
length(which(completeddata$incm_wk==levels(completeddata$incm_wk)[2]))/length(which(completeddata$incm_wk%in%levels(completeddata$incm_wk)))
length(which(completeddata$incm_wk==levels(completeddata$incm_wk)[3]))/length(which(completeddata$incm_wk%in%levels(completeddata$incm_wk)))
length(which(completeddata$incm_wk==levels(completeddata$incm_wk)[4]))/length(which(completeddata$incm_wk%in%levels(completeddata$incm_wk)))
length(which(completeddata$incm_wk==levels(completeddata$incm_wk)[5]))/length(which(completeddata$incm_wk%in%levels(completeddata$incm_wk)))
length(which(completeddata$incm_wk==levels(completeddata$incm_wk)[1]))
length(which(completeddata$incm_wk==levels(completeddata$incm_wk)[2]))
length(which(completeddata$incm_wk==levels(completeddata$incm_wk)[3]))
length(which(completeddata$incm_wk==levels(completeddata$incm_wk)[4]))
length(which(completeddata$incm_wk==levels(completeddata$incm_wk)[5]))

length(completeddata$Nmbr_ppls>0)
mean(completeddata$Nmbr_ppls);sd(completeddata$Nmbr_ppls)

completeddata$Tchr_sx=as.factor(completeddata$Tchr_sx)
length(which(completeddata$Tchr_sx==levels(completeddata$Tchr_sx)[1]))/length(which(completeddata$Tchr_sx%in%levels(completeddata$Tchr_sx)))
length(which(completeddata$Tchr_sx==levels(completeddata$Tchr_sx)[2]))/length(which(completeddata$Tchr_sx%in%levels(completeddata$Tchr_sx)))
length(which(completeddata$Tchr_sx==levels(completeddata$Tchr_sx)[1]))
length(which(completeddata$Tchr_sx==levels(completeddata$Tchr_sx)[2]))

completeddata$Lngth_srv=as.factor(completeddata$Lngth_srv)
length(which(completeddata$Lngth_srv==levels(completeddata$Lngth_srv)[1]))/length(which(completeddata$Lngth_srv%in%levels(completeddata$Lngth_srv)))
length(which(completeddata$Lngth_srv==levels(completeddata$Lngth_srv)[2]))/length(which(completeddata$Lngth_srv%in%levels(completeddata$Lngth_srv)))
length(which(completeddata$Lngth_srv==levels(completeddata$Lngth_srv)[3]))/length(which(completeddata$Lngth_srv%in%levels(completeddata$Lngth_srv)))
length(which(completeddata$Lngth_srv==levels(completeddata$Lngth_srv)[4]))/length(which(completeddata$Lngth_srv%in%levels(completeddata$Lngth_srv)))
length(which(completeddata$Lngth_srv==levels(completeddata$Lngth_srv)[1]))
length(which(completeddata$Lngth_srv==levels(completeddata$Lngth_srv)[2]))
length(which(completeddata$Lngth_srv==levels(completeddata$Lngth_srv)[3]))
length(which(completeddata$Lngth_srv==levels(completeddata$Lngth_srv)[4]))




