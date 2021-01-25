setwd("/Volumes/MRC-IEU-research/projects/ieu2/p1/015/working/data/CB_2021/")

library(readxl);library(forestplot);library(checkmate)

KS2=read_excel("KS2_forrest_plot_input.xlsx")
KS3=read_excel("KS3_forrest_plot_input.xlsx")

## KS2

KS2=as.data.frame(KS2)
labels=as.vector(KS2$Variable)[c(T,F)]
KS2.CC=KS2$Mean[seq(1,56,2)]
a=as.vector(paste0(KS2.CC))
KS2.IMP=KS2$Mean[seq(0,56,2)]  #IMP
b=as.vector(paste0(KS2.IMP))
KS2=transform(KS2, "95% CI"=paste0("(",paste(Lower, Upper, sep=", "),paste0(")")))
e=KS2$X95..CI
e.C=e[seq(1,56,2)]
e.I=e[seq(0,56,2)]

tabletext=cbind(c("Variable",labels),c("Imputed est.",b),c("95% CI",e.I),c("C c est.",a),c("95% CI",e.C))

png("KS2_FP_2021.png",width=5000,height=2500,res=300)
forestplot(tabletext,
           hrzl_lines = list("2"=gpar(lty=1),"7"=gpar(lty=2,columns=1:5),"9"=gpar(lty=2,columns=1:5),
                             "15"=gpar(lty=2,columns=1:5),"17"=gpar(lty=2,columns=1:5),"18"=gpar(lty=2,columns=1:5),
                             "23"=gpar(lty=2,columns=1:5),"25"=gpar(lty=2,columns=1:5),"29"=gpar(lty=2,columns=1:5),
                            "30"=gpar(lty=1,columns=1:5)),
           fn.ci_norm = c(fpDrawNormalCI, fpDrawCircleCI),
           legend = c("Complete case","Imputed"),
           mean = cbind(c(NA,KS2$Mean[KS2$Analyses == "Complete case"]), 
                        c(NA,KS2$Mean[KS2$Analyses == "Imputed"])),
           lower = cbind(c(NA,KS2$Lower[KS2$Analyses == "Complete case"]), 
                         c(NA,KS2$Lower[KS2$Analyses == "Imputed"])),
           upper = cbind(c(NA,KS2$Upper[KS2$Analyses == "Complete case"]), 
                         c(NA,KS2$Upper[KS2$Analyses == "Imputed"])),
           col=fpColors(box=c("#4393C3", "red2"),line=c("#4393C3", "red2"),
                        zero=c("black")),cex = 1,
           boxsize = 0.25,
           line.margin = 0.25,
           xlab = "Standardised effect estimate (95% CI)")
dev.off()

## KS3

KS3=as.data.frame(KS3)
labels=as.vector(KS3$Variable)[c(T,F)]
KS3.CC=KS3$Mean[seq(1,42,2)]
a=as.vector(paste0(KS3.CC))
KS3.IMP=KS3$Mean[seq(0,42,2)]  #IMP
b=as.vector(paste0(KS3.IMP))
KS3=transform(KS3, "95% CI"=paste0("(",paste(Lower, Upper, sep=", "),paste0(")")))
e=KS3$X95..CI
e.C=e[seq(1,42,2)]
e.I=e[seq(0,42,2)]

tabletext=cbind(c("Variable",labels),c("Imputed est.",b),c("95% CI",e.I),c("C c est.",a),c("95% CI",e.C))

png("KS3_FP_2021.png",width=5000,height=2500,res=300)
forestplot(tabletext, 
           hrzl_lines = list("2"=gpar(lty=1),"7"=gpar(lty=2,columns=1:5),"9"=gpar(lty=2,columns=1:5),
                             "15"=gpar(lty=2,columns=1:5),"17"=gpar(lty=2,columns=1:5),"18"=gpar(lty=2,columns=1:5),
                             "23"=gpar(lty=1,columns=1:5)),
           fn.ci_norm = c(fpDrawNormalCI, fpDrawCircleCI),
           legend = c("Complete case","Imputed"),
           mean = cbind(c(NA,KS3$Mean[KS3$Analyses == "Complete case"]), 
                        c(NA,KS3$Mean[KS3$Analyses == "Imputed"])),
           lower = cbind(c(NA,KS3$Lower[KS3$Analyses == "Complete case"]), 
                         c(NA,KS3$Lower[KS3$Analyses == "Imputed"])),
           upper = cbind(c(NA,KS3$Upper[KS3$Analyses == "Complete case"]), 
                         c(NA,KS3$Upper[KS3$Analyses == "Imputed"])),
           col=fpColors(box=c("#4393C3", "red2"),line=c("#4393C3", "red2"),
                        zero=c("black")),cex = 1,
           boxsize = 0.25,
           line.margin = 0.25,
           xlab = "Standardised effect estimate (95% CI)")
dev.off()

## Heritability

heritability=read_excel("/Volumes/MRC-IEU-research/projects/ieu2/p1/015/working/data/CB_2021/heritability_forest_plot_input.xlsx")
heritability=as.data.frame(heritability)
heritability[,c("Mean","SE","Lower","Upper")]=round(heritability[,c("Mean","SE","Lower","Upper")],3)
lbl=c("KS2","KS3")
heritability.adj=round(heritability$Mean[seq(0,4,2)],3) #adjusted
a=paste0(heritability.adj)
heritability.unadj=round(heritability$Mean[seq(1,4,2)],3) #unadjusted
b=paste0(heritability.unadj)
heritability=transform(heritability, "95% CI"=paste0("(",paste(Lower, Upper, sep=", "),paste0(")")))
e=paste0(heritability$X95..CI)
e.U=e[seq(1,4,2)]
e.A=e[seq(0,4,2)]
tabletext=cbind(c("Key stage",lbl),c("Unadjusted est.",b),c("95% CI",e.U),c("Adjusted est.",a),c("95% CI",e.A))

png("HERIT_FP_2021.png",width=5000,height=2000,res=300)
forestplot(tabletext,
           hrzl_lines = list("1"=gpar(lty=1),"2"=gpar(lty=1)),
           
           legend = c("Adjusted","Unadjusted"),fn.ci_norm = c(fpDrawNormalCI, fpDrawCircleCI),
           title = "Heritability plot",
           mean = cbind(c(NA,heritability$Mean[heritability$`Analyses` == "Adjusted"]), 
                        c(NA,heritability$Mean[heritability$`Analyses` == "Unadjusted"])),
           lower = cbind(c(NA,heritability$Lower[heritability$`Analyses` == "Adjusted"]), 
                         c(NA,heritability$Lower[heritability$`Analyses` == "Unadjusted"])),
           upper = cbind(c(NA,heritability$Upper[heritability$`Analyses` == "Adjusted"]), 
                         c(NA,heritability$Upper[heritability$`Analyses` == "Unadjusted"])),
           col=fpColors(box=c("#4393C3", "red2"),line=c("#4393C3", "red2"),
                        zero=c("black")),cex = 1,
           boxsize = 0.25,
           line.margin = 0.25,
           xlab = "Standardised effect estimate (95% CI)")
dev.off()


