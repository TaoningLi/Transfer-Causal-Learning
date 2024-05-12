
#replicate under RCT
repnum <- 100
colnum <- 5
prob <- 0.4
ATEmat.RR <- matrix(rep(0, repnum*colnum), nrow=repnum)
colnames(ATEmat.RR)<-c("ATE.TO", "ATE.TCL",  "ATE.TO.full", "ATE.TCL.full","ATE.true")
for (kk in seq(1,repnum)){
  cat('currently ', kk, 'round in RR.\n')
  ps.source <- rbinom(n = dim(twin.source)[1], prob = prob, size = 1)
  ps.target <- rbinom(n = dim(twin.target)[1], prob = prob, size = 1)
  source("D:/A_Total_phd/1AAphd/phd grade 1/advanced statistical application/proposal/TCL/twins-TCL.R")
  ATE.TO -> ATEmat.RR[kk,1]
  ATE.TCL -> ATEmat.RR[kk,2]
  ATE.TO.full -> ATEmat.RR[kk,3]
  ATE.TCL.full -> ATEmat.RR[kk,4]
  ATE.true.target -> ATEmat.RR[kk,5]
}

repnum <- 10
colnum <- 10
prob <- 0.3
ATEmat.RO <- matrix(rep(0, repnum*colnum), nrow=repnum)
colnames(ATEmat.RO)<-c("ATE.true.source", "ATE.true.target", "ATE.TO", "ATE.TO.full", "ATE.TCL", "ATE.TCL.full", "ATE.TO.IPW","ATE.TCL.IPW","ATE.TO.DR","ATE.TCL.DR")
for (kk in seq(4,repnum)){
  cat('currently ', kk, 'round in RO.\n')
  ps.source <- rbinom(n = dim(twin.source)[1], prob = as.matrix(twin.source["gestat10"]/10-0.1, ncol=1), size = 1)
  ps.target <- rbinom(n = dim(twin.target)[1], prob = prob, size = 1)
  source("D:/A_Total_phd/1AAphd/phd grade 1/advanced statistical application/proposal/TCL/twins-TCL.R")
 for(kkk in 1:colnum){
   vecATE[kkk]->ATEmat.RO[kk,kkk]
 }
}

repnum <- 10
colnum <- 10
prob <- 0.3
ATEmat.OO <- matrix(rep(0, repnum*colnum), nrow=repnum)
colnames(ATEmat.OO)<-c("ATE.true.source", "ATE.true.target", "ATE.TO", "ATE.TO.full", "ATE.TCL", "ATE.TCL.full", "ATE.TO.IPW","ATE.TCL.IPW","ATE.TO.DR","ATE.TCL.DR")
for (kk in seq(1,repnum)){
  cat('currently ', kk, 'round in OO.\n')
  ps.source <- rbinom(n = dim(twin.source)[1], prob = as.matrix(twin.source["gestat10"]/10-0.1, ncol=1), size = 1)
  ps.target <- rbinom(n = dim(twin.target)[1], prob = as.matrix(twin.target["gestat10"]/10-0.1, ncol=1), size = 1)
  source("D:/A_Total_phd/1AAphd/phd grade 1/advanced statistical application/proposal/TCL/twins-TCL.R")
  for(kkk in 1:colnum){
    vecATE[kkk]->ATEmat.OO[kk,kkk]
  }
}

