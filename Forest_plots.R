## Forest plots

setwd("//rdsfcifs.acrc.bris.ac.uk/MRC-IEU-research/projects/ieu2/p1/015/working/data/CB")

install.packages("forestplot")
library(forestplot) ; library(readxl) ; library(checkmate)

## ks2

Mdata=read_excel("C:/Users/cb18669/OneDrive - University of Bristol/MyFiles-Migrated/Documents/Mini-project week 26+/coefficients ks2.xlsx")
Mdata=as.data.frame(Mdata)
labels=c("Degree","Degree", "Alevel","Alevel","CSE","CSE","O-level","O-level","Vocational","Vocational","Female","Female","Male","Male","I","I","II","II","III manual","III manual","III non-manual","III non-manual","IV","IV","V","V","Not statemented","Not statemented","Has a statement","Has a statement","Month of birth","Month of birth","Over £400","Over £400", "Less than £100", "Less than £100","£100-199","£100-199","£200-299","£200-299","£300-399","£300-399","Female (teacher)","Male (teacher)","Over 10 years exp.","Less than 1 year exp.","1-2 years exp.","3-9 years exp.","Class size")
lbl=as.vector(c("Degree", "Alevel","CSE","O-level","Vocational","Female","Male","I","II","III manual","III non-manual","IV","V","Not statemented","Has a statement","Month of birth","Over £400", "Less than £100","£100-199","£200-299","£300-399","Female (teacher)", "Male (teacher)","Over 10 years exp.","Less than 1 year exp.","1-2 years exp.","3-9 years exp.","Class size"))
Mdata.CC=Mdata$Mean[seq(1,56,2)] #CC
a=paste0(Mdata.CC)
a=c("0.00","0.00","0.01","-0.05","0.04","0.00","-0.06","0.00","0.02","0.00","0.04","0.05","-0.42","0.00","-0.69","-0.01","0.00","-0.11","0.00","-0.02","0.01","0.00","0.08","0.00","-0.11","-0.16","-0.06","0.01" )
Mdata.IMP=Mdata$Mean[seq(0,56,2)]  #IMP
b=paste0(Mdata.IMP)
b=c("0.00","-0.06","-0.03","-0.03","0.00","0.00","0.03","0.00","-0.02","0.04","-0.02","-0.13","-0.07","0.00","0.00","0.00","0.00","0.04","0.04","-0.02","0.06","0.00","0.02","0.00","0.02","0.02","0.02","0.00")
Mdata=transform(Mdata, "95% CI"=paste(Lower, Upper, sep="-"))
e=paste0(Mdata$X95..CI)
e=c("(0.00,0.00)","(0.00,0.00)","(-0.10,0.11)","(-0.16,0.04)","(-0.11,0.14)","(-0.13,0.06)","(-0.15,0.05)","(-0.16,0.09)",
"(-0.10,0.17)", "(-0.11,0.09)","(0.00,0.00)","(0.00,0.00)","(-0.12,0.00)", "(-0.02,0.09)","(0.00,0.00)","(0.00,0.00)",
"(-0.08,0.12)","(-0.12,0.07)","(-0.10,0.11)","(-0.06,0.13)","(-0.08,0.16)","(-0.15,0.11)","(-0.17,0.28)","(-0.33,0.06)", 
"(-1.30,0.46)","(-0.52,0.38)","(0.00,0.00)","(0.00,0.00)","(-1.00,0.38)","(-0.20,0.20)","(-0.02,0.00)","(0.00,0.00)",
"(0.00-0.00)","(0.00,0.00)","(-0.28,0.05)","(-0.04,0.12)","(-0.11,0.10)","(-0.03,0.10)","(-0.11,0.06)","(-0.09,0.06)",
"(-0.07,0.10)","(-0.06,0.18)","(0.00,0.00)","(0.00,0.00)","(-0.02,0.18)","(-0.05,0.09)","(0.00,0.00)","(0.00,0.00)",
"(-0.44,0.22)","(-0.01,0.04)","(-0.39,0.07)","(-0.02,0.06)","(-0.15,0.03)","(-0.02,0.06)","(0.00,0.02)","(-0.00,0.01)")
e.I=e[seq(0,56,2)]
e.C=e[seq(1,56,2)]

tabletext=cbind(c("Variable",lbl),c("Imputed est.",b),c("95% CI",e.I),c("C c est.",a),c("95% CI",e.C))
png("high_res_ks2.png",width=5000,height=2500,res=300)
forestplot(tabletext,hrzl_lines = gpar(col="#444444"), fn.ci_norm = c(fpDrawNormalCI, fpDrawCircleCI),
           legend = c("Complete case","Imputed"),
           mean = cbind(c(NA,Mdata$Mean[Mdata$Analyses == "Complete case"]), 
                        c(NA,Mdata$Mean[Mdata$Analyses == "Imputed"])),
           lower = cbind(c(NA,Mdata$Lower[Mdata$Analyses == "Complete case"]), 
                         c(NA,Mdata$Lower[Mdata$Analyses == "Imputed"])),
           upper = cbind(c(NA,Mdata$Upper[Mdata$Analyses == "Complete case"]), 
                         c(NA,Mdata$Upper[Mdata$Analyses == "Imputed"])),
           col=fpColors(box=c("turquoise2", "red2"),line=c("turquoise2", "red2"),
                        zero=c("black")),cex = 1,
           boxsize = 0.25,
           line.margin = 0.25,
           xlab = "Standardised effect estimate (95% CI)")
dev.off()


##ks3

Mdata=read_excel("O:/Documents/Mini-project week 26+/coefficients_ks3.xlsx")
Mdata=as.data.frame(Mdata)
labels=c("Degree","Degree", "Alevel","Alevel","CSE","CSE","O-level","O-level","Vocational","Vocational","Female","Female","Male","Male","I","I","II","II","III manual","III manual","III non-manual","III non-manual","IV","IV","V","V","Not statemented","Not statemented","Has a statement","Has a statement","Month of birth","Month of birth","Over £400","Over £400", "Less than £100", "Less than £100","£100-199","£100-199","£200-299","£200-299","£300-399","£300-399")
lbl=as.vector(c("Degree", "Alevel","CSE","O-level","Vocational","Female","Male","I","II","III manual","III non-manual","IV","V","Not statemented","Has a statement","Month of birth","Over £400", "Less than £100","£100-199","£200-299","£300-399"))
Mdata.CC=Mdata$Mean[seq(1,42,2)] #CC
a=paste0(Mdata.CC)
a=c("0.00","-0.06","-0.01","0.00","0.03","0.00","0.04","0.00","-0.07","-0.06","-0.02","-0.01","0.07","0.00","0.06","0.01","0.00","-0.06","0.01","0.06","0.10")
Mdata.IMP=Mdata$Mean[seq(0,42,2)] #imp
b=paste0(Mdata.IMP)
b=c("0.00","0.16","0.10","0.08","0.09","0.00","-0.06","0.00","-0.02","-0.07","-0.02","0.12","0.10","0.00","0.17","0.00","0.00","-0.10","0.04","0.03","-0.01")
Mdata=transform(Mdata, "95% CI"=paste(Lower, Upper, sep="-"))
e=paste0(Mdata$X95..CI)
e=c("(0.00,0.00)","(0.00,0.00)","(-0.19,0.07)","(0.01,0.33)","(-0.13,0.11)","(-0.04,0.25)","(-0.16,0.17)","(-0.05,0.21)","(-0.12,0.18)",
  "(-0.08,0.27)","(0.00,0.00)","(0.00,0.00)","(-0.04,0.13)","(-0.14,0.01)","(0.00,0.00)","(0.00,0.00)","(-0.19,0.05)","(-0.12,0.09)",
  "(-0.19,0.07)","(-0.19,0.06)","(-0.17,0.13)","(-0.18,0.14)","(-0.27,0.25)","(-0.35,0.10)","(-1.32,1.46)","(-0.40,0.61)","(0.00,0.00)",       
   "(0.00,0.00)","(-0.31,0.42)","(-0.06,0.40)","(0.00,0.02)","(-0.01,0.01)","(0.00,0.00)","(0.00,0.00)","(-0.17,0.04)","(-0.30,0.09)", 
   "(-0.10,0.11)","(-0.07,0.15)","(-0.07,0.18)","(-0.16,0.22)","(-0.09,0.29)","(-0.19,0.18)")
e.I=e[seq(0,42,2)]
e.C=e[seq(1,42,2)]

tabletext=cbind(c("Variable",lbl),c("Imputed est.",b),c("95% CI",e.I),c("C c est.",a),c("95% CI",e.C))

png("high_res_ks3.png",width=5000,height=2500,res=300)
forestplot(tabletext,hrzl_lines = gpar(col="#444444"), fn.ci_norm = c(fpDrawNormalCI, fpDrawCircleCI),
           legend = c("Complete case","Imputed"),
           mean = cbind(c(NA,Mdata$Mean[Mdata$Analyses == "Complete case"]), 
                        c(NA,Mdata$Mean[Mdata$Analyses == "Imputed"])),
           lower = cbind(c(NA,Mdata$Lower[Mdata$Analyses == "Complete case"]), 
                         c(NA,Mdata$Lower[Mdata$Analyses == "Imputed"])),
           upper = cbind(c(NA,Mdata$Upper[Mdata$Analyses == "Complete case"]), 
                         c(NA,Mdata$Upper[Mdata$Analyses == "Imputed"])),
           col=fpColors(box=c("turquoise2", "red2"),line=c("turquoise2", "red2"),
                        zero=c("black")),cex = 1,
           boxsize = 0.25,
           line.margin = 0.25,
           xlab = "Standardised effect estimate (95% CI)")
dev.off()

##heritability

Mdata=read_excel("O:/Documents/Mini-project week 26+/heritability_FP.xlsx")
Mdata=hrtblty
lbl=c("KS2","KS3")
Mdata.adj=round(Mdata$Mean[seq(0,4,2)],3) #adjusted
a=paste0(Mdata.adj)
Mdata.unadj=round(Mdata$Mean[seq(1,4,2)],3) #unadjusted
b=paste0(Mdata.unadj)

Mdata=transform(Mdata, "95% CI"=paste(Lower, Upper, sep="-"))
e=paste0(Mdata$X95..CI)
e=c("(-0.023,0.277)","(0.006,0.345)","(-0.053,0.361)","(-0.150,0.315)")
e.U=e[seq(1,4,2)]
e.A=e[seq(0,4,2)]

tabletext=cbind(c("Key stage",lbl),c("Unadjusted est.",b),c("95% CI",e.U),c("Adjusted est.",a),c("95% CI",e.A))

png("high_res_herit_2.png",width=5000,height=2000,res=300)
forestplot(tabletext,
           legend = c("Adjusted","Unadjusted"),fn.ci_norm = c(fpDrawNormalCI, fpDrawCircleCI),
           title = "Heritability plot",
           mean = cbind(c(NA,Mdata$Mean[Mdata$`Analyses` == "Adjusted"]), 
                        c(NA,Mdata$Mean[Mdata$`Analyses` == "Unadjusted"])),
           lower = cbind(c(NA,Mdata$Lower[Mdata$`Analyses` == "Adjusted"]), 
                         c(NA,Mdata$Lower[Mdata$`Analyses` == "Unadjusted"])),
           upper = cbind(c(NA,Mdata$Upper[Mdata$`Analyses` == "Adjusted"]), 
                        c(NA,Mdata$Upper[Mdata$`Analyses` == "Unadjusted"])),
           col=fpColors(box=c("turquoise", "red2"),line=c("turquoise2", "red2"),
                        zero=c("black")),
           boxsize = 0.1,           line.margin = 0.25,
           xlab = "Standardised effect estimate")
dev.off()

##heritability2

Mdata=read_excel("O:/Documents/Mini-project week 26+/heritability_FP.xlsx")
Mdata=as.data.frame(Mdata)
lbl=c("KS2","KS3")
Mdata=Mdata[-c(1,3),]
Mdata$Mean=round(Mdata$Mean,3)
Mdata=transform(Mdata, "95% CI"=paste(Lower, Upper, sep="-"))
e=paste0(Mdata$X95..CI)
e=c("(0.006,0.345)","(-0.150,0.315)")

tabletext=cbind(c("Key stage",lbl),c("Adjusted est.",Mdata$Mean),c("95% CI",e))

png("high_res_herit_2.png",width=5000,height=2000,res=300)
forestplot(tabletext,title = "Heritability plot",
           mean = cbind(c(NA,Mdata$Mean)),
           lower = cbind(c(NA,Mdata$Lower)),
           upper = cbind(c(NA,Mdata$Upper)),
           col=fpColors(box="red",line="red",
                        zero=c("black")),
           boxsize = 0.1,           line.margin = 0.25,
           xlab = "Standardised effect estimate",txt_gp=fpTxtGp(cex=1.9))
dev.off()
