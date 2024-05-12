fun.lik<- function(alpha, alpha.init, twin.y, twin.x, lambda){
  twin.x <- cbind(rep(1,dim(twin.x)[1]), twin.x)
  val <- (-t(twin.y)%*%twin.x %*% alpha+sum(log(1+exp(twin.x %*% alpha))))/dim(twin.x)[1]+lambda*sum(abs(alpha-alpha.init))
  return(as.numeric(val))
}


fun.min <- function(alpha.init, twin.y, twin.x, lambda.seq){
  MSE <- Inf
  for (lambda in lambda.seq){
    fun.temp <- function(alpha){
      fun.lik(alpha, alpha.init, twin.y, twin.x, lambda)
    }
    opt <- optim(rep(0.1,length(alpha.init)), fun.temp, lower = -Inf, upper = Inf, method = "BFGS")
    MSE.predict <- sum(-twin.y*log(fun.predict(opt$par, twin.x))-(1-twin.y)*log(1-fun.predict(opt$par, twin.x)))
    #MSE.predict <- sum((fun.predict(opt$par, twin.x)-twin.y)^2)
    if (MSE.predict <= MSE){
      MSE <- MSE.predict
      lambda.opt <- lambda
      alpha.opt <- opt$par
    }
  }
  return(list(alpha = alpha.opt, lambda = lambda.opt))
}

fun.cvmin <- function(alpha.init, twin.y, twin.x, lambda.seq, folds){
  MSE <- Inf
  vecsample <- sample(folds,dim(twin.x)[1],replace = TRUE)
  list.twin.test <- split(cbind(twin.y,twin.x),sample(folds,dim(twin.x)[1],replace = TRUE))
  list.twin.test.x <- list()
  list.twin.train.x <- list()
  list.twin.test.y <- list()
  list.twin.train.y <- list()
  for (f in seq(1,folds)){
    list.twin.test.x[[f]] <- twin.x[which(vecsample==f),]
    list.twin.test.y[[f]] <- twin.y[which(vecsample==f),]
  }
  for (f in seq(1,folds)){
    list.twin.train.x[[f]] <- twin.x[which(vecsample!=f),]
    list.twin.train.y[[f]] <- twin.y[which(vecsample!=f),]
  }
  for (lambda in lambda.seq){
     MSE.predict<-0
    for (ff in seq(1,folds)){
      fun.temp <- function(alpha){
        fun.lik(alpha, alpha.init, as.matrix(list.twin.train.y[[ff]],ncol=1), as.matrix(list.twin.train.x[[ff]]), lambda)
      }
      opt <- optim(rep(0.1,length(alpha.init)), fun.temp, lower = -Inf, upper = Inf, method = "BFGS")
      MSE.predict <- MSE.predict+sum(-as.matrix(list.twin.test.y[[ff]],ncol=1)*log(fun.predict(opt$par, as.matrix(list.twin.test.x[[ff]])))-(1-as.matrix(list.twin.test.y[[ff]],ncol=1))*log(1-fun.predict(opt$par, as.matrix(list.twin.test.x[[ff]]))))/folds
    }
    #MSE.predict <- sum((fun.predict(opt$par, twin.x)-twin.y)^2)
    if (MSE.predict <= MSE){
      MSE <- MSE.predict
      lambda.opt <- lambda
    }
  }
  
  fun.temp <- function(alpha){
    fun.lik(alpha, alpha.init, twin.y, twin.x, lambda.opt)
  }
  opt <- optim(rep(0.1,length(alpha.init)), fun.temp, lower = -Inf, upper = Inf, method = "BFGS")
  alpha.opt <- opt$par
  return(list(alpha = alpha.opt, lambda = lambda.opt))
}


fun.predict <- function(alpha, twin.x){
  twin.x <- cbind(rep(1,dim(twin.x)[1]), twin.x)
  return(1/(1+exp((-1)*twin.x %*% alpha)))
}

#pgd functions
fun.grad <- function(alpha, twin.y, twin.x){
  twin.x <- cbind(rep(1,dim(twin.x)[1]), twin.x)
  val <- (-t(twin.y)%*%twin.x+sum(log(1+exp(twin.x %*% alpha))))/dim(twin.x)[1]
  return(as.numeric(val))
}

fun.pgd.cvmin <- function(alpha.init, twin.y, twin.x, lambda.seq, folds, stepsize = 0.1){
  MSE <- Inf
  vecsample <- sample(folds,dim(twin.x)[1],replace = TRUE)
  list.twin.test <- split(cbind(twin.y,twin.x),sample(folds,dim(twin.x)[1],replace = TRUE))
  list.twin.test.x <- list()
  list.twin.train.x <- list()
  list.twin.test.y <- list()
  list.twin.train.y <- list()
  for (f in seq(1,folds)){
    list.twin.test.x[[f]] <- twin.x[which(vecsample==f),]
    list.twin.test.y[[f]] <- twin.y[which(vecsample==f),]
  }
  for (f in seq(1,folds)){
    list.twin.train.x[[f]] <- twin.x[which(vecsample!=f),]
    list.twin.train.y[[f]] <- twin.y[which(vecsample!=f),]
  }
  for (lambda in lambda.seq){
    MSE.predict<-0
    for (ff in seq(1,folds)){
      fun.temp <- function(alpha){
        fun.lik(alpha, alpha.init, as.matrix(list.twin.train.y[[ff]],ncol=1), as.matrix(list.twin.train.x[[ff]]), lambda)
      }
      opt <- optim(rep(0.1,length(alpha.init)), fun.temp, lower = -Inf, upper = Inf, method = "BFGS")
      MSE.predict <- MSE.predict+sum(-as.matrix(list.twin.test.y[[ff]],ncol=1)*log(fun.predict(opt$par, as.matrix(list.twin.test.x[[ff]])))-(1-as.matrix(list.twin.test.y[[ff]],ncol=1))*log(1-fun.predict(opt$par, as.matrix(list.twin.test.x[[ff]]))))/folds
    }
    #MSE.predict <- sum((fun.predict(opt$par, twin.x)-twin.y)^2)
    if (MSE.predict <= MSE){
      MSE <- MSE.predict
      lambda.opt <- lambda
    }
  }
  
  fun.temp <- function(alpha){
    fun.lik(alpha, alpha.init, twin.y, twin.x, lambda.opt)
  }
  opt <- optim(rep(0.1,length(alpha.init)), fun.temp, lower = -Inf, upper = Inf, method = "BFGS")
  alpha.opt <- opt$par
  return(list(alpha = alpha.opt, lambda = lambda.opt))
}


