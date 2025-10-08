install.packages("ggfortify", repos="http://cran.us.r-project.org")
install.packages("mvnormtest", repos="http://cran.us.r-project.org")
install.packages("datarium", repos="http://cran.us.r-project.org")
install.packages("ggplot2", repos="http://cran.us.r-project.org")
install.packages("car", repos="http://cran.us.r-project.org")

library(MASS)
library(car) 
library(datarium)
library(ggplot2)
library(broom) 
library(ggfortify)
library(tidyverse)
library(mvnormtest)

library(readxl)
X2025_food_environment_atlas_data <- read_excel("2025-food-environment-atlas-data.xlsx", 
                                                sheet = "ACCESS", skip = 1)
access <- read_excel("2025-food-environment-atlas-data.xlsx", 
                     sheet = "ACCESS", skip = 1)
access 

library(readxl)
X2025_food_environment_atlas_data <- read_excel("2025-food-environment-atlas-data.xlsx", 
                                                sheet = "STORES", skip = 1)
stores <- read_excel("2025-food-environment-atlas-data.xlsx", 
                     sheet = "STORES", skip = 1)
stores 

library(readxl)
X2025_food_environment_atlas_data <- read_excel("2025-food-environment-atlas-data.xlsx", 
                                                sheet = "RESTAURANTS", skip = 1)
restaurants <- read_excel("2025-food-environment-atlas-data.xlsx", 
                          sheet = "RESTAURANTS", skip = 1)
restaurants 

library(readxl)
X2025_food_environment_atlas_data <- read_excel("2025-food-environment-atlas-data.xlsx", 
                                                sheet = "LOCAL", skip = 1)
local <- read_excel("2025-food-environment-atlas-data.xlsx", 
                    sheet = "LOCAL", skip = 1)
local 

atlasog = merge(access, stores, by.x = "FIPS", by.y = "FIPS")
atlasog = merge(atlasog, restaurants, by.x = "FIPS", by.y = "FIPS")
atlasog = merge(atlasog, local, by.x = "FIPS", by.y = "FIPS")


atlas = select(atlasog, PCH_LACCESS_POP_15_19, PCH_GROC_16_20, PCH_SUPERC_16_20, PCH_CONVS_16_20, PCH_SPECS_16_20, PCH_SNAPS_17_23, PCH_WICS_16_22, PCH_FFR_16_20, PCH_FSR_16_20, PCH_DIRSALES_12_17)
atlas = filter(atlas, PCH_LACCESS_POP_15_19 != -9999, PCH_GROC_16_20 != -9999, PCH_SUPERC_16_20 != -9999, PCH_CONVS_16_20 != -9999, PCH_SPECS_16_20 != -9999, PCH_SNAPS_17_23 != -9999, PCH_WICS_16_22 != -9999, PCH_FFR_16_20 != -9999, PCH_FSR_16_20 != -9999, PCH_DIRSALES_12_17 != -9999)
atlas = filter(atlas, PCH_LACCESS_POP_15_19 != -8888, PCH_GROC_16_20 != -8888, PCH_SUPERC_16_20 != -8888, PCH_CONVS_16_20 != -8888, PCH_SPECS_16_20 != -8888, PCH_SNAPS_17_23 != -8888, PCH_WICS_16_22 != -8888, PCH_FFR_16_20 != -8888, PCH_FSR_16_20 != -8888, PCH_DIRSALES_12_17 != -8888)
atlas2 = filter(atlas, PCH_LACCESS_POP_15_19 <5000, PCH_GROC_16_20 <5000, PCH_SUPERC_16_20 <5000, PCH_CONVS_16_20 <5000, PCH_SPECS_16_20 <5000, PCH_SNAPS_17_23 <5000, PCH_WICS_16_22 <5000, PCH_FFR_16_20 <5000, PCH_FSR_16_20 <5000, PCH_DIRSALES_12_17 <5000)

colMeans(atlas2) 

install.packages("leaps",repos = "http://cran.us.r-project.org")
install.packages("glmnet",repos = "http://cran.us.r-project.org")

library(MASS)
library(leaps)
library(glmnet)

Y <- as.numeric(atlas2[,1])
X <- as.matrix(atlas2[,-1])

Y <- Y- mean(Y)
Y

X <- t(t(X) -colMeans(X))

cor(atlas2)

plot(atlas2,pch=16,cex=.5)

fit <- lm(PCH_LACCESS_POP_15_19~.,data=atlas2)
summary(fit)

fit0 <- lm(PCH_LACCESS_POP_15_19~1,data=atlas2)
summary(fit0)

fit_bsub <- regsubsets(x=atlas2[,2:10],y=atlas2[,1])
summary(fit_bsub)

fit_bsub$rss

# forward step-wise via BIC 
fit_forw <- stepAIC(fit0,scope=PCH_LACCESS_POP_15_19~PCH_GROC_16_20+PCH_SUPERC_16_20+PCH_CONVS_16_20+PCH_SPECS_16_20+PCH_SNAPS_17_23+PCH_WICS_16_22+PCH_FFR_16_20+PCH_FSR_16_20+PCH_DIRSALES_12_17,
                    direction="forward",data=atlas2,k=log(nrow(atlas2)))
summary(fit_forw)

# backward step-wise via BIC 
fit_back <- stepAIC(fit,direction="backward",data=atlas2,k=log(nrow(atlas2)))
summary(fit_back)

lam <- 1*nrow(atlas2)

beta_ls <- solve(t(X)%*%X)%*%t(X)%*%Y
beta_r <- solve(t(X)%*%X + diag(rep(lam,9)))%*%t(X)%*%Y
cbind(beta_ls,beta_r)

# coefficient paths 

lambdas <- exp(seq(log(.01),log(100*nrow(atlas2)),l=100))
betasr <- matrix(0,length(lambdas),9)
for(i in 1:length(lambdas))
{
  betasr[i,] = solve(t(X)%*%X + diag(rep(lambdas[i],9)))%*%t(X)%*%Y
}

betasr

plot(c(1,length(lambdas)),range(betasr),type="n",ylab="Coefficients",xlab="Lambda Index")
for(j in 1:9)
{
  lines(betasr[length(lambdas):1,j],col=j)
}
legend(0,20,legend=names(atlas2)[2:10],col=1:9,lty=rep(1,9))

plot(c(1,length(lambdas)),range(betasr),type="n",ylab="Coefficients",xlab="Lambda Index")
for(j in 1:9)
{
  lines(betasr[length(lambdas):1,j],col=j)
}
legend("center",legend=names(atlas2)[2:10],col=1:9,lty=rep(1,9))

install.packages("glmnet",repos = "http://cran.us.r-project.org")

library(glmnet)

fitl <- glmnet(x=X,y=Y,family="gaussian",alpha=1)
plot(fitl,col=1:9)
legend("center",legend=names(atlas2)[2:10],col=1:9,lty=rep(1,9),cex=.8)

fitl <- glmnet(x=X,y=Y,family="gaussian",alpha=1)
plot(fitl,col=1:9)

plot(fitl, xvar = "lambda", label = TRUE)
plot(fitl, xvar = "dev", label = TRUE)

cvfit <- cv.glmnet(X, Y)
print(cvfit)
plot(cvfit)

cvfit$lambda.min
coef(cvfit, s = "lambda.min")
predict(cvfit, newx = X[1:5,], s = "lambda.min")
Y[1:5]

cvfit2 <- cv.glmnet(X, Y, type.measure = "mse", nfolds = 5)
print(cvfit2)
cvfit2$lambda.min
plot(cvfit2)

svdx = svd(X)
svdx$d
svdx$v

par(mar=c(1,1,1,1))
layout(matrix(1:81,9,9))
mycols = rainbow(length(Y))
orY = order(Y)
for(i in 1:9)
{
  for(j in 1:9)
  {
    plot(svdx$u[,i],svdx$u[,j],type="p",pch=16,col=mycols[orY])
  }
}

#amount of variance explained 
varex = 0; cumvarex = 0;
for(i in 1:9)
{
  varex[i] = svdx$d[i]^2/sum(svdx$d^2)
  cumvarex[i] = sum(varex)
}
par(mfrow=c(1,2))
par(mar=c(5,4,4,2))
barplot(varex,ylab="Amount of Var Explained",xlab="PCs")
barplot(cumvarex,ylab="Cummulative Var Explained",xlab="PCs")

# ridge paths again 

plot(c(1,length(lambdas)),range(betasr),type="n",ylab="Coefficients",xlab="Lambda Index")
for(j in 1:9)
{
  lines(betasr[length(lambdas):1,j],col=j)
}
legend(0,20,legend=names(atlas2)[2:10],col=1:10,lty=rep(1,9))

# principal components regression 

betapcr <- diag(svdx$d)%*%t(svdx$u)%*%Y
ypcr <- svdx$u[,1:2]%*%t(svdx$u[,1:2])%*%Y
mean((predict(fit)-atlas2[,1])^2)
mean((ypcr-Y)^2)
mse_pcr <- NULL
for(i in 1:9){
  ypcr <- svdx$u[,1:i]%*%t(svdx$u[,1:i])%*%Y
  mse_pcr <- c(mse_pcr, mean((ypcr-Y)^2))
}
mse_pcr

plsfunc <- function(x,y)
{
  p <- ncol(x)
  n <- nrow(x)
  M <- t(x)%*%y
  Z <- NULL; V <- NULL; P <- NULL;
  for(k in 1:p){
    svdm <- svd(M)
    z <- x%*%svdm$u
    z <- z*as.numeric(1/sqrt(t(z)%*%z))
    V <- cbind(V,svdm$u)
    p <- t(x)%*%z/as.numeric(t(z)%*%z)
    P <- cbind(P,p);
    Z <- cbind(Z,z);
    M <- M - P%*%solve(t(P)%*%P)%*%t(P)%*%M;
  }
  return(list(Z=Z,V=V))
}

plsx <- plsfunc(X,Y)

# scatterplots of PLS components 

par(mar=c(1,1,1,1))
layout(matrix(1:81,9,9))
mycols <- rainbow(length(Y))
orY <- order(Y)
for(i in 1:9)
{
  for(j in 1:9)
  {
    plot(plsx$Z[,i],plsx$Z[,j],type="p",pch=16,col=mycols[orY])
  }
}

betapls = t(plsx$Z)%*%Y

cbind(betapcr,betapls)


label.splitting <- sample(1:5, nrow(atlas2), replace = TRUE)
label.splitting 

for (k in 1:5) {
  dat.valid <- atlas2[label.splitting == k, ]
  dat.train <- atlas2[label.splitting != k, ]
}

fitdeg_1 <- lm(PCH_LACCESS_POP_15_19 ~ ., data = dat.train)
predictions_1 <- predict(fitdeg_1, newdata = dat.valid)
mean((dat.valid$PCH_LACCESS_POP_15_19 - predictions_1)^2)
AIC(fitdeg_1)
BIC(fitdeg_1)
summary(fitdeg_1) 

fitdeg_2 <- lm(PCH_LACCESS_POP_15_19 ~ poly(PCH_GROC_16_20, 2, raw=TRUE) + poly(PCH_SUPERC_16_20, 2, raw=TRUE) + poly(PCH_CONVS_16_20, 2, raw=TRUE) + poly(PCH_SPECS_16_20, 2, raw=TRUE) + poly(PCH_SNAPS_17_23, 2, raw=TRUE) + poly(PCH_WICS_16_22, 2, raw=TRUE) + poly(PCH_FFR_16_20, 2, raw=TRUE) + poly(PCH_FSR_16_20, 2, raw=TRUE) + poly(PCH_DIRSALES_12_17, 2, raw=TRUE), data = dat.train)
predictions_2 <- predict(fitdeg_2, newdata = dat.valid)
mean((dat.valid$PCH_LACCESS_POP_15_19 - predictions_2)^2)
AIC(fitdeg_2)
BIC(fitdeg_2)
summary(fitdeg_2)

fitdeg_3 <- lm(PCH_LACCESS_POP_15_19 ~ poly(PCH_GROC_16_20, 3, raw=TRUE) + poly(PCH_SUPERC_16_20, 3, raw=TRUE) + poly(PCH_CONVS_16_20, 3, raw=TRUE) + poly(PCH_SPECS_16_20, 3, raw=TRUE) + poly(PCH_SNAPS_17_23, 3, raw=TRUE) + poly(PCH_WICS_16_22, 3, raw=TRUE) + poly(PCH_FFR_16_20, 3, raw=TRUE) + poly(PCH_FSR_16_20, 3, raw=TRUE) + poly(PCH_DIRSALES_12_17, 3, raw=TRUE), data = dat.train)
predictions_3 <- predict(fitdeg_3, newdata = dat.valid)
mean((dat.valid$PCH_LACCESS_POP_15_19 - predictions_3)^2)
AIC(fitdeg_3)
BIC(fitdeg_3)
summary(fitdeg_3)

fitdeg_4 <- lm(PCH_LACCESS_POP_15_19 ~ poly(PCH_GROC_16_20, 4, raw=TRUE) + poly(PCH_SUPERC_16_20, 4, raw=TRUE) + poly(PCH_CONVS_16_20, 4, raw=TRUE) + poly(PCH_SPECS_16_20, 4, raw=TRUE) + poly(PCH_SNAPS_17_23, 4, raw=TRUE) + poly(PCH_WICS_16_22, 4, raw=TRUE) + poly(PCH_FFR_16_20, 4, raw=TRUE) + poly(PCH_FSR_16_20, 4, raw=TRUE) + poly(PCH_DIRSALES_12_17, 4, raw=TRUE), data = dat.train)
predictions_4 <- predict(fitdeg_4, newdata = dat.valid)
mean((dat.valid$PCH_LACCESS_POP_15_19 - predictions_4)^2)
AIC(fitdeg_4)
BIC(fitdeg_4)
summary(fitdeg_4)

fitdeg_5 <- lm(PCH_LACCESS_POP_15_19 ~ poly(PCH_GROC_16_20, 5, raw=TRUE) + poly(PCH_SUPERC_16_20, 5, raw=TRUE) + poly(PCH_CONVS_16_20, 5, raw=TRUE) + poly(PCH_SPECS_16_20, 5, raw=TRUE) + poly(PCH_SNAPS_17_23, 5, raw=TRUE) + poly(PCH_WICS_16_22, 5, raw=TRUE) + poly(PCH_FFR_16_20, 5, raw=TRUE) + poly(PCH_FSR_16_20, 5, raw=TRUE) + poly(PCH_DIRSALES_12_17, 5, raw=TRUE), data = dat.train)
predictions_5 <- predict(fitdeg_5, newdata = dat.valid)
mean((dat.valid$PCH_LACCESS_POP_15_19 - predictions_5)^2)
AIC(fitdeg_5)
BIC(fitdeg_5)
summary(fitdeg_5)

fitdeg_6 <- lm(PCH_LACCESS_POP_15_19 ~ poly(PCH_GROC_16_20, 6, raw=TRUE) + poly(PCH_SUPERC_16_20, 6, raw=TRUE) + poly(PCH_CONVS_16_20, 6, raw=TRUE) + poly(PCH_SPECS_16_20, 6, raw=TRUE) + poly(PCH_SNAPS_17_23, 6, raw=TRUE) + poly(PCH_WICS_16_22, 6, raw=TRUE) + poly(PCH_FFR_16_20, 6, raw=TRUE) + poly(PCH_FSR_16_20, 6, raw=TRUE) + poly(PCH_DIRSALES_12_17, 6, raw=TRUE), data = dat.train)
predictions_6 <- predict(fitdeg_6, newdata = dat.valid)
mean((dat.valid$PCH_LACCESS_POP_15_19 - predictions_6)^2)
AIC(fitdeg_6)
BIC(fitdeg_6)
summary(fitdeg_6)

fitdeg_7 <- lm(PCH_LACCESS_POP_15_19 ~ poly(PCH_GROC_16_20, 7, raw=TRUE) + poly(PCH_SUPERC_16_20, 7, raw=TRUE) + poly(PCH_CONVS_16_20, 7, raw=TRUE) + poly(PCH_SPECS_16_20, 7, raw=TRUE) + poly(PCH_SNAPS_17_23, 7, raw=TRUE) + poly(PCH_WICS_16_22, 7, raw=TRUE) + poly(PCH_FFR_16_20, 7, raw=TRUE) + poly(PCH_FSR_16_20, 7, raw=TRUE) + poly(PCH_DIRSALES_12_17, 7, raw=TRUE), data = dat.train)
predictions_7 <- predict(fitdeg_7, newdata = dat.valid)
mean((dat.valid$PCH_LACCESS_POP_15_19 - predictions_7)^2)
AIC(fitdeg_7)
BIC(fitdeg_7)
summary(fitdeg_7)

fitdeg_8 <- lm(PCH_LACCESS_POP_15_19 ~ poly(PCH_GROC_16_20, 8, raw=TRUE) + poly(PCH_SUPERC_16_20, 8, raw=TRUE) + poly(PCH_CONVS_16_20, 8, raw=TRUE) + poly(PCH_SPECS_16_20, 8, raw=TRUE) + poly(PCH_SNAPS_17_23, 8, raw=TRUE) + poly(PCH_WICS_16_22, 8, raw=TRUE) + poly(PCH_FFR_16_20, 8, raw=TRUE) + poly(PCH_FSR_16_20, 8, raw=TRUE) + poly(PCH_DIRSALES_12_17, 8, raw=TRUE), data = dat.train)
predictions_8 <- predict(fitdeg_8, newdata = dat.valid)
mean((dat.valid$PCH_LACCESS_POP_15_19 - predictions_8)^2)
AIC(fitdeg_8)
BIC(fitdeg_8)
summary(fitdeg_8)

fitdeg_9 <- lm(PCH_LACCESS_POP_15_19 ~ poly(PCH_GROC_16_20, 9, raw=TRUE) + poly(PCH_SUPERC_16_20, 9, raw=TRUE) + poly(PCH_CONVS_16_20, 9, raw=TRUE) + poly(PCH_SPECS_16_20, 9, raw=TRUE) + poly(PCH_SNAPS_17_23, 9, raw=TRUE) + poly(PCH_WICS_16_22, 9, raw=TRUE) + poly(PCH_FFR_16_20, 9, raw=TRUE) + poly(PCH_FSR_16_20, 9, raw=TRUE) + poly(PCH_DIRSALES_12_17, 9, raw=TRUE), data = dat.train)
predictions_9 <- predict(fitdeg_9, newdata = dat.valid)
mean((dat.valid$PCH_LACCESS_POP_15_19 - predictions_9)^2)
AIC(fitdeg_9)
BIC(fitdeg_9)
summary(fitdeg_9)

fitdeg_10 <- lm(PCH_LACCESS_POP_15_19 ~ poly(PCH_GROC_16_20, 10, raw=TRUE) + poly(PCH_SUPERC_16_20, 10, raw=TRUE) + poly(PCH_CONVS_16_20, 10, raw=TRUE) + poly(PCH_SPECS_16_20, 10, raw=TRUE) + poly(PCH_SNAPS_17_23, 10, raw=TRUE) + poly(PCH_WICS_16_22, 10, raw=TRUE) + poly(PCH_FFR_16_20, 10, raw=TRUE) + poly(PCH_FSR_16_20, 10, raw=TRUE) + poly(PCH_DIRSALES_12_17, 10, raw=TRUE), data = dat.train)
predictions_10 <- predict(fitdeg_10, newdata = dat.valid)
mean((dat.valid$PCH_LACCESS_POP_15_19 - predictions_10)^2)
AIC(fitdeg_10)
BIC(fitdeg_10)
summary(fitdeg_10)

# L1 regularization 

fit1 <- lm(Y~X-1,data=atlas2)
lam <- .01
fitl <- glmnet(x=X,y=Y,family="gaussian",lambda=lam,alpha=1)
cbind(fit1$coef,as.matrix(fitl$beta))


# model complexity and prediction error 

n = 100
p = 50
Btrue = matrix(0,p,1)
Btrue[1:10] = rnorm(10)*.75
Btrue[11:20] = rnorm(10)*.5

Xtr = scale(matrix(rnorm(n*p),n,p))
Ytr = Xtr%*%Btrue + matrix(rnorm(n),n,1)
Xts = scale(matrix(rnorm(n*p),n,p))
Yts = Xts%*%Btrue + matrix(rnorm(n),n,1)

fit = glmnet(x=Xtr,y=Ytr,family="gaussian",standardize=FALSE,nlambda=50,lambda.min.ratio=.001)
Yhtr = predict(fit,newx=Xtr)
MSEtr = apply((Yhtr-Ytr%*%matrix(1,1,length(fit$lambda)))^2,2,mean)
Yhts = predict(fit,newx=Xts)
MSEts = apply((Yhts-Yts%*%matrix(1,1,length(fit$lambda)))^2,2,mean)

plot(1:length(fit$lambda),MSEtr,type="l",col=4,xlab="Lambda Index",ylab="Error")
lines(1:length(fit$lambda),MSEtr,col=4)
lines(1:length(fit$lambda),MSEts,col=2)
legend(30,5,legend=c("Training Error","Test Error"),col=c(4,2),lty=c(1,1),cex=.75)

install.packages("leaps",repos = "http://cran.us.r-project.org")
install.packages("glmnet",repos = "http://cran.us.r-project.org")

library(MASS)
library(leaps)
library(glmnet)

fold = 5
sam = sample(1:n,n)
CVerrs = NULL
for(i in 1:fold)
{
  ind = sam[((i-1)*n/fold + 1):(i*n/fold)]
  Xin = Xtr[-ind,]; Yin = Ytr[-ind]
  Xout = Xtr[ind,]; Yout = Ytr[ind]
  fit = glmnet(x=Xin,y=Yin,family="gaussian",standardize=FALSE,nlambda=50,lambda.min.ratio=.001)
  Yh = predict(fit,newx=Xout)
  CVerrs = cbind(CVerrs,apply((Yh-Yout%*%matrix(1,1,length(fit$lambda)))^2,2,mean))
}
CVerr = apply(CVerrs,1,mean)

#getting an error message here for xycoords not matching when I try to knit to pdf, but the code runs in the block 
#plot(1:length(fit$lambda),MSEtr,type="l",col=4,xlab="Lambda Index",ylab="Error")
#lines(1:length(fit$lambda),MSEtr,col=4)
#lines(1:length(fit$lambda),MSEts,col=2)
#legend(30,5,legend=c("Training Error","Test Error"),col=c(4,2),lty=c(1,1),cex=.75)
#lines(1:length(fit$lambda),CVerr,col=1)

# test error 
fit = glmnet(x=Xtr,y=Ytr,family="gaussian",standardize=FALSE,nlambda=50,lambda.min.ratio=.001)
optlam = fit$lambda[which.min(CVerr)]
optlam

fit = glmnet(x=Xtr,y=Ytr,family="gaussian",standardize=FALSE,lambda=optlam)
Yhtr = predict(fit,newx=Xtr)
TRerr = mean( (Yhtr - Ytr)^2 )
Yhts = predict(fit,newx=Xts)
TSerr = mean( (Yhts - Yts)^2 )
sum(fit$beta!=0)

TRerr
TSerr

#now LOO 
fold = n
sam = sample(1:n,n)
CVerrs = NULL
for(i in 1:fold)
{
  ind = sam[((i-1)*n/fold + 1):(i*n/fold)]
  Xin = Xtr[-ind,]; Yin = Ytr[-ind]
  Xout = Xtr[ind,]; Yout = Ytr[ind]
  fit = glmnet(x=Xin,y=Yin,family="gaussian",standardize=FALSE,nlambda=50,lambda.min.ratio=.001)
  Yh = predict(fit,newx=Xout)
  CVerrs = cbind(CVerrs,apply((Yh-Yout%*%matrix(1,1,length(fit$lambda)))^2,2,mean))
}
CVerr = apply(CVerrs,1,mean)

plot(1:length(fit$lambda),MSEtr,type="l",col=4,xlab="Lambda Index",ylab="Error")
lines(1:length(fit$lambda),MSEtr,col=4)
lines(1:length(fit$lambda),MSEts,col=2)
legend(30,5,legend=c("Training Error","Test Error"),col=c(4,2),lty=c(1,1),cex=.75)
lines(1:length(fit$lambda),CVerr,col=1)

fit = glmnet(x=Xtr,y=Ytr,family="gaussian",standardize=FALSE,nlambda=50,lambda.min.ratio=.001)
optlam = fit$lambda[which.min(CVerr)]
optlam

fit = glmnet(x=Xtr,y=Ytr,family="gaussian",standardize=FALSE,lambda=optlam)
Yhtr = predict(fit,newx=Xtr)
TRerr = mean( (Yhtr - Ytr)^2 )
Yhts = predict(fit,newx=Xts)
TSerr = mean( (Yhts - Yts)^2 )
sum(fit$beta!=0)

TRerr
TSerr
