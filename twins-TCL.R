library(dplyr)
library(ggplot2)
library(patchwork)
library(glmnet)
library(tidyverse)
#------------data processing-----------
#use read.csv to read files
twin.Y <- read.csv("data_twins/twin_pairs_Y_3years_samesex.csv", header=TRUE, sep=",", stringsAsFactors = FALSE)
twin.X <- read.csv("data_twins/twin_pairs_X_3years_samesex.csv", header=TRUE, sep=",", stringsAsFactors = FALSE)
twin.X <- twin.X[,-which(colnames(twin.X) %in% c("hemo"))]
twin.T <- read.csv("data_twins/twin_pairs_T_3years_samesex.csv", header=TRUE, sep=",", stringsAsFactors = FALSE)
#twin.X <- twin.X[ , -which(colnames(twin.X) %in% c("tobacco","alcohol"))]

#create ID for each pair of twins
colnames(twin.X)[1]<-"ID"
colnames(twin.T)[1]<-"ID"
colnames(twin.Y)[1]<-"ID"

twin.X <- twin.X[,-2]
twin.total <- merge(twin.X, twin.Y, by="ID")
twin.total <- merge(twin.total, twin.T, by="ID")

#.clean means we delete the sample with missing covariates.
twin.X.clean<-na.omit(twin.X)
twin.Y.clean<-twin.Y[which(twin.Y$ID %in% twin.X.clean$ID),]
twin.T.clean<-twin.T[which(twin.T$ID %in% twin.X.clean$ID),]
twin.total.clean <- merge(twin.X.clean, twin.Y.clean, by="ID")
twin.total.clean <- merge(twin.total.clean, twin.T.clean, by="ID")

#target: twins either weigh <= 2000g
#source: twins both weigh >2000g
# twin.target <- twin.total.clean  %>% 
#   filter(dbirwt_0 <= 2000 & dbirwt_1 <= 2000)#4846 samples
# twin.source <- twin.total.clean  %>% 
#   filter(dbirwt_0 > 2000 | dbirwt_1 > 2000)#27274 samples
# 
# 
# #true ATE under target domain and source domain
# twin.total.target <- twin.total %>% 
#        filter(dbirwt_0 <= 2000 & dbirwt_1 <= 2000)
# twin.total.source <- twin.total %>% 
#        filter(dbirwt_0 > 2000 | dbirwt_1 > 2000)

twin.target <- twin.total.clean  %>% 
  filter(brstate < 10)#3261 samples
twin.source <- twin.total.clean  %>% 
  filter(brstate >= 10)#28859 samples
twin.total.target <- twin.total %>% 
  filter(brstate < 10)
twin.total.source <- twin.total %>% 
  filter(brstate >= 10)
ATE.true.target <- as.numeric(colMeans(twin.total.target)['mort_1']-colMeans(twin.total.target)['mort_0'])
ATE.true.source <- as.numeric(colMeans(twin.total.source)['mort_1']-colMeans(twin.total.source)['mort_0'])

#RCT with propensity 0.4
#prob <- 0.4
#ps.source <- rbinom(n = dim(twin.source)[1], prob = prob, size = 1)
#ps.target <- rbinom(n = dim(twin.target)[1], prob = prob, size = 1)

index.T.source <- which(ps.source==1)
index.C.source <- which(ps.source==0)
index.T.target <- which(ps.target==1)
index.C.target <- which(ps.target==0)

twin.source.T <- twin.source[index.T.source,]
twin.source.C <- twin.source[index.C.source,]
twin.target.T <- twin.target[index.T.target,]
twin.target.C <- twin.target[index.C.target,]

twin.X.target.T <- twin.target.T[,-which(colnames(twin.target) %in% 
                                           c("mort_0","mort_1","bord_0","tobacco","ID","alcohol","dbirwt_0","dbirwt_1","infant_id_0","infant_id_1","data_year"))]
twin.Y.target.T <- twin.target.T['mort_1']
twin.X.target.C <- twin.target.C[,-which(colnames(twin.target) %in% 
                                           c("mort_0","mort_1","bord_1","tobacco","ID","alcohol","dbirwt_0","dbirwt_1","infant_id_0","infant_id_1","data_year"))]
twin.Y.target.C <- twin.target.C['mort_0']
twin.X.target_forT   <- twin.target[,-which(colnames(twin.target) %in% 
                                              c("mort_0","mort_1","bord_0","tobacco","ID","alcohol","dbirwt_0","dbirwt_1","infant_id_0","infant_id_1","data_year"))]
twin.X.target_forC   <- twin.target[,-which(colnames(twin.target) %in% 
                                         c("mort_0","mort_1","bord_1","tobacco","ID","alcohol","dbirwt_0","dbirwt_1","infant_id_0","infant_id_1","data_year"))]
#-----------Outcome Regression----------
#Classical Method
#estimating causal effects using samples in 'T'arget domain 'O'nly (TO)
#mean function estimation under treatment group
twin.target.T.reg <- (twin.target.T[,-which(colnames(twin.target.T) %in% 
                                                       c("mort_0","bord_0","tobacco","ID","alcohol","dbirwt_0","dbirwt_1","infant_id_0","infant_id_1","data_year"))])
glm.TO.T <- glm(mort_1 ~. ,   data = twin.target.T.reg, family = binomial(link = "logit"))
alpha.TO.T <- glm.TO.T$coefficients

#mean function estimation under control group
twin.target.C.reg <- (twin.target.C[,-which(colnames(twin.target.C) %in% 
c("mort_1","bord_1","tobacco","ID","alcohol","dbirwt_0","dbirwt_1","infant_id_0","infant_id_1","data_year"))])
glm.TO.C <- glm(mort_0 ~. ,   data = twin.target.C.reg, family = binomial(link = "logit"))
alpha.TO.C <- glm.TO.C$coefficients

alpha.TO.T[which(is.na(alpha.TO.T)==TRUE)]<-0
alpha.TO.C[which(is.na(alpha.TO.C)==TRUE)]<-0

That.TO <- predict.glm(glm.TO.T, twin.X.target.T, type = "response")
Chat.TO <- predict.glm(glm.TO.C, twin.X.target.C, type = "response")

That.TO.full <- predict.glm(glm.TO.T, twin.X.target_forT, type = "response")
Chat.TO.full <- predict.glm(glm.TO.C, twin.X.target_forC, type = "response")
#ATE using samples in Target domain Only.
ATE.TO <- mean(That.TO)-mean(Chat.TO)
ATE.TO.full <- mean(That.TO.full)-mean(Chat.TO.full)

#Transfer Causal Learning with l1-penalty
#######treatment
twin.source.T.reg <- (twin.source.T[,-which(colnames(twin.source.T) %in% 
c("mort_0","bord_0","tobacco","ID","alcohol","dbirwt_0","dbirwt_1","infant_id_0","infant_id_1","data_year"))])
glm.TCLS.T <- glm(mort_1 ~., data = twin.source.T.reg, family = binomial(link = "logit"))
alpha.TCLS.T <- glm.TCLS.T$coefficients
#use samples in twin.target.T to fit alpha hat.

cv.TCL.T <- cv.glmnet(as.matrix(twin.X.target.T), as.matrix(twin.Y.target.T,ncol=1), intercept = T, alpha = 1, nfolds = 5, lambda.min.ratio = 1e-05, nlambda = 10)
opt.TCL.T <- fun.cvmin(alpha.init = alpha.TCLS.T,
                     twin.y = as.matrix(twin.Y.target.T,ncol=1),
                     twin.x = as.matrix(twin.X.target.T),
                     lambda.seq = cv.TCL.T$lambda,
                     folds = 5)
glm.TCLS.T$coefficients <- opt.TCL.T$alpha
That.TCL <- predict.glm(glm.TCLS.T, twin.X.target.T, type = "response")
That.TCL.full <- predict.glm(glm.TCLS.T, twin.X.target_forT, type = "response")

#######control
twin.source.C.reg <- (twin.source.C[,-which(colnames(twin.source.C) %in% 
c("mort_1","bord_1","tobacco","ID","alcohol","dbirwt_0","dbirwt_1","infant_id_0","infant_id_1","data_year"))])
glm.TCLS.C <- glm(mort_0 ~. ,   data = twin.source.C.reg, family = binomial(link = "logit"))
alpha.TCLS.C <- glm.TCLS.C$coefficients
#use samples in twin.target.C to fit alpha hat.
cv.TCL.C <- cv.glmnet(as.matrix(twin.X.target.C), as.matrix(twin.Y.target.C,ncol=1), intercept = T, alpha = 1, nfolds = 5, lambda.min.ratio = 1e-05, nlambda = 10)
opt.TCL.C <- fun.cvmin(alpha.init = alpha.TCLS.C,
                     twin.y = as.matrix(twin.Y.target.C,ncol=1),
                     twin.x = as.matrix(twin.X.target.C),
                     lambda.seq = cv.TCL.C$lambda,
                     folds = 5
)
glm.TCLS.C$coefficients <- opt.TCL.C$alpha
Chat.TCL <- fun.predict(alpha = opt.TCL.C$alpha, twin.x = as.matrix(twin.X.target.C))
Chat.TCL.full <- fun.predict(alpha = opt.TCL.C$alpha, twin.x = as.matrix(twin.X.target_forC))

#ATE use TCL.
ATE.TCL <- mean(That.TCL)-mean(Chat.TCL)
ATE.TCL.full <- mean(That.TCL.full)-mean(Chat.TCL.full)

vecATE<-as.matrix(c(ATE.true.source, ATE.true.target, ATE.TO, ATE.TO.full, ATE.TCL, ATE.TCL.full), nrow=1)
rownames(vecATE)<-c("ATE.true.source", "ATE.true.target", "ATE.TO", "ATE.TO.full", "ATE.TCL", "ATE.TCL.full")

#PS Model
#probability estimation via logistic regression
deletecol <- c(dim(twin.source.T.reg)[2]-1,dim(twin.source.T.reg)[2])
twin.source.T.reg.prob <- cbind(twin.source.T.reg, as.matrix(rep(1,dim(twin.source.T.reg)[1]),ncol=1) )
colnames(twin.source.T.reg.prob)[dim(twin.source.T.reg.prob)[2]] <- c('treatment')
twin.source.C.reg.prob <- cbind(twin.source.C.reg, as.matrix(rep(0,dim(twin.source.C.reg)[1]),ncol=1) )
colnames(twin.source.C.reg.prob)[dim(twin.source.C.reg.prob)[2]] <- c('treatment')
twin.source.reg <- data.frame(rbind(as.matrix(twin.source.T.reg.prob), as.matrix(twin.source.C.reg.prob)))
glm.TCLS.T.prob <- glm(treatment ~., data = twin.source.reg[,-(dim(twin.source.reg)[2]-1)], family = binomial(link = "logit"))
beta.TCLS.T.prob <- glm.TCLS.T.prob$coefficients

#use samples in twin.target.T to fit beta hat.
twin.target.T.reg.prob <- cbind(twin.target.T.reg, as.matrix(rep(1,dim(twin.target.T.reg)[1]),ncol=1) )
colnames(twin.target.T.reg.prob)[dim(twin.target.T.reg.prob)[2]] <- c('treatment')
twin.target.C.reg.prob <- cbind(twin.target.C.reg, as.matrix(rep(0,dim(twin.target.C.reg)[1]),ncol=1) )
colnames(twin.target.C.reg.prob)[dim(twin.target.C.reg.prob)[2]] <- c('treatment')
twin.target.reg <- data.frame(rbind(as.matrix(twin.target.T.reg.prob), as.matrix(twin.target.C.reg.prob)))

glm.TO.T.prob <- glm(treatment ~., data = twin.target.reg[,-(dim(twin.target.reg)[2]-1)], family = binomial(link = "logit"))
beta.TO.T <- glm.TCLS.T.prob$coefficients
prob.TO <- fun.predict(alpha = beta.TO.T, 
                        twin.x = as.matrix(twin.target.reg[,-c(dim(twin.source.reg)[2]-1,dim(twin.source.reg)[2])]))

cv.TCL.T.prob <- cv.glmnet(as.matrix(twin.target.reg[,-c(dim(twin.source.reg)[2]-1,dim(twin.source.reg)[2])]),
                           as.matrix(twin.target.reg[,dim(twin.target.reg)[2]],ncol=1), 
                           intercept = T, alpha = 1, nfolds = 5, lambda.min.ratio = 1e-05, nlambda = 10)
opt.TCL.T.prob <- fun.cvmin(alpha.init = beta.TCLS.T.prob,
                       twin.y = as.matrix(twin.target.reg[,dim(twin.target.reg)[2]],ncol=1),
                       twin.x = as.matrix(twin.target.reg[,-c(dim(twin.source.reg)[2]-1,dim(twin.source.reg)[2])]),
                       lambda.seq = cv.TCL.T.prob$lambda,
                       folds = 5)
beta.TCL.T <- opt.TCL.T.prob$alpha
prob.TCL <- fun.predict(alpha = beta.TCL.T, 
                        twin.x = as.matrix(twin.target.reg[,-c(dim(twin.source.reg)[2]-1,dim(twin.source.reg)[2])]))
t <- as.matrix(twin.target.reg[,dim(twin.target.reg)[2]],ncol=1)
y <- rbind(as.matrix(twin.target.T.reg[,dim(twin.target.T.reg)[2]],ncol=1),as.matrix(twin.target.C.reg[,dim(twin.target.C.reg)[2]],ncol=1))
That.TCL.IPW <- mean(t*y/prob.TCL)
Chat.TCL.IPW <- mean(((1-t)*(y))/(1-prob.TCL))
ATE.TCL.IPW <- That.TCL.IPW-Chat.TCL.IPW

#Doubly Robust
yhatOR.T <- fun.predict(alpha = opt.TCL.T$alpha, twin.x = as.matrix(twin.target.reg[,-c(dim(twin.source.reg)[2]-1,dim(twin.source.reg)[2])]))
yhatOR.C <- fun.predict(alpha = opt.TCL.C$alpha, twin.x = as.matrix(twin.target.reg[,-c(dim(twin.source.reg)[2]-1,dim(twin.source.reg)[2])]))

That.TCL.DR <- mean((t*y- yhatOR.T*(t-prob.TCL))/(prob.TCL))
Chat.TCL.DR <- mean(((1-t)*y+ yhatOR.T*(t-prob.TCL))/(1-prob.TCL))
ATE.TCL.DR <- That.TCL.DR-Chat.TCL.DR

#Target Only
That.TO.IPW <- mean(t*y/prob.TO)
Chat.TO.IPW <- mean(((1-t)*(y))/(1-prob.TO))
ATE.TO.IPW <- That.TO.IPW-Chat.TO.IPW

#Doubly Robust
yhatOR.T <- fun.predict(alpha = alpha.TO.T, twin.x = as.matrix(twin.target.reg[,-c(dim(twin.source.reg)[2]-1,dim(twin.source.reg)[2])]))
yhatOR.C <- fun.predict(alpha = alpha.TO.C, twin.x = as.matrix(twin.target.reg[,-c(dim(twin.source.reg)[2]-1,dim(twin.source.reg)[2])]))


That.TO.DR <- mean((t*y- yhatOR.T*(t-prob.TO))/(prob.TO))
Chat.TO.DR <- mean(((1-t)*y+ yhatOR.C*(t-prob.TO))/(1-prob.TO))
ATE.TO.DR <- That.TO.DR-Chat.TO.DR


  vecATE<-as.matrix(c(ATE.true.source, ATE.true.target, ATE.TO, ATE.TO.full, ATE.TCL, ATE.TCL.full, ATE.TO.IPW, ATE.TCL.IPW, ATE.TO.DR, ATE.TCL.DR), nrow=1)
rownames(vecATE)<-c("ATE.true.source", "ATE.true.target", "ATE.TO", "ATE.TO.full", "ATE.TCL", "ATE.TCL.full", "ATE.TO.IPW","ATE.TCL.IPW","ATE.TO.DR","ATE.TCL.DR")
