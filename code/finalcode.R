rm(list = ls())
bodyfat<-read.csv("BodyFat.csv")
summary(bodyfat)
library(tidyverse)
library(caret)
library(glmnet)
library(MVA)
library(car)

###########data preprocessing#################
plot(bodyfat$DENSITY,bodyfat$BODYFAT,pch=19,xlab="density",ylab = "bodyfat",main = "density vs. bodyfat")
dif<-bodyfat[2]-(495/bodyfat[3]-450)
diff<-as.numeric(unlist(dif))
Q1<-quantile(diff)[2];Q3<-quantile(diff)[4]
IQR<-Q3-Q1
out<-which(dif<Q1-1.5*IQR | dif>Q3+1.5*IQR) ####48,76,96,182,216

par(mfrow=c(3,5),mar=c(4,2,2,2))
#vs. bodyfat
for (i in 3:17) {
  bvbox(cbind(bodyfat[,i],bodyfat[,2]),col="white",pch=ifelse(bodyfat[,1]%in%out,19,16),xlab = colnames(bodyfat)[i],ylab="BODYFAT")
  text(bodyfat[out,i],bodyfat[out,2],label=out,col="red")
  text(bodyfat[-out,i],bodyfat[-out,2],label=bodyfat[-out,1],col="darkblue")
}
#vs. density
for (i in 4:17) {
  bvbox(cbind(bodyfat[,i],bodyfat[,3]),col="white",pch=ifelse(bodyfat[,1]%in%out,19,16),xlab = colnames(bodyfat)[i],ylab="DENSITY")
  text(bodyfat[out,i],bodyfat[out,3],label=out,col="red")
  text(bodyfat[-out,i],bodyfat[-out,3],label=bodyfat[-out,1],col="darkblue")
}
#density outlier +96

#delete 216,182
#keep bodyfat of 48,76
#keep 96
###one outlier in columns——39 41 delete 
#one more outlier in HEIGHT——42 ###correct
bodyfat$HEIGHT[42]<-sqrt(bodyfat$WEIGHT[42]/bodyfat$ADIPOSITY[42]*703)

newbf<-bodyfat[-c(39,41,182,216),-3]#delete them
n<-dim(newbf)[1]

##########MLR model###########################
###all possible subsets
if (!require("leaps")) { 
  install.packages("leaps") 
  stopifnot(require("leaps"))
}
options(width = 90) # just for better views (not required)
myleaps <- regsubsets(BODYFAT~., data=newbf[,-1], nbest=8) 
(myleaps.summary <- summary(myleaps)) 

bettertable <- cbind(myleaps.summary$which, myleaps.summary$rsq, myleaps.summary$rss,
                     myleaps.summary$adjr2, myleaps.summary$cp, myleaps.summary$bic) 
dimnames(bettertable)[[2]] <- c(dimnames(myleaps.summary$which)[[2]],"rsq", "rss", "adjr2", "cp", "bic")
show(bettertable)
par(mfrow=c(1,3), pty="s")
plot(myleaps, scale = "adjr2"); plot(myleaps, scale = "Cp"); plot(myleaps, scale = "bic");                                                                                          show(bettertable)
#r2: BODYFAT~AGE+HEIGHT+NECK+CHEST+ABDOMEN+BICEPS+FOREARM+WRIST
#CP: BODYFAT~AGE+HEIGHT+NECK+CHEST+ABDOMEN+FOREARM+WRIST
#BIC: BODYFAT~HEIGHT+ABDOMEN+WRIST
model_r2<-lm(BODYFAT~AGE+HEIGHT+NECK+CHEST+ABDOMEN+BICEPS+FOREARM+WRIST,data=newbf[,-1])
model_cp<-lm(BODYFAT~AGE+HEIGHT+NECK+CHEST+ABDOMEN+FOREARM+WRIST,data = newbf[,-1])
model_bic<-lm(BODYFAT~HEIGHT+ABDOMEN+WRIST,data = newbf[,-1])

###stepwise selection
fitall<-lm(BODYFAT~.*.,data = newbf[,-1])
fitmain<-lm(BODYFAT~.,data = newbf[,-1])
summary(fitall)
m1<-step(fitall,direction = "both",k=log(n))
#m2<-step(fitall,direction = "backward",k=log(n)) same as m1
m4<-step(fitmain,direction = "both",k=log(n))
#m4<-lm(BODYFAT~AGE+WEIGHT+HEIGHT+ADIPOSITY+NECK+ABDOMEN+WRIST,data=newbf[,-1])
summary(m1)
anova(m4,m1)
anova(m1)



vif(m1)
vif(m4)#multicollinearity

######cross validation
set.seed(100)
training.samples <- newbf$BODYFAT %>%
  createDataPartition(p = 0.8, list = FALSE)
train.data  <- newbf[training.samples, ]
test.data <- newbf[-training.samples, ]
# Build the model
model1 <- lm(formula = BODYFAT ~ AGE + ADIPOSITY + CHEST + ABDOMEN + WRIST + AGE:WRIST + 
              ADIPOSITY:CHEST, data = train.data)
model4<-lm(BODYFAT~AGE+WEIGHT+HEIGHT+ADIPOSITY+NECK+ABDOMEN+WRIST,data = train.data)
model_r2<-lm(BODYFAT~AGE+HEIGHT+NECK+CHEST+ABDOMEN+BICEPS+FOREARM+WRIST,data = train.data)
model_cp<-lm(BODYFAT~AGE+HEIGHT+NECK+CHEST+ABDOMEN+FOREARM+WRIST,data = train.data)
model_bic<-lm(BODYFAT~HEIGHT+ABDOMEN+WRIST,data = train.data)
# Make predictions and compute the R2, RMSE and MAE
predictions <- model1 %>% predict(test.data)
results1<-data.frame( R2 = R2(predictions, test.data$BODYFAT),
            RMSE = RMSE(predictions, test.data$BODYFAT),
            MAE = MAE(predictions, test.data$BODYFAT))
predictions <- model4 %>% predict(test.data)
results2<-data.frame( R2 = R2(predictions, test.data$BODYFAT),
                      RMSE = RMSE(predictions, test.data$BODYFAT),
                      MAE = MAE(predictions, test.data$BODYFAT))
predictions <- model_r2 %>% predict(test.data)
results3<-data.frame( R2 = R2(predictions, test.data$BODYFAT),
                      RMSE = RMSE(predictions, test.data$BODYFAT),
                      MAE = MAE(predictions, test.data$BODYFAT))
predictions <- model_cp %>% predict(test.data)
results4<-data.frame( R2 = R2(predictions, test.data$BODYFAT),
                      RMSE = RMSE(predictions, test.data$BODYFAT),
                      MAE = MAE(predictions, test.data$BODYFAT))
predictions <- model_bic %>% predict(test.data)
results5<-data.frame( R2 = R2(predictions, test.data$BODYFAT),
                      RMSE = RMSE(predictions, test.data$BODYFAT),
                      MAE = MAE(predictions, test.data$BODYFAT))
results<-rbind(results1,results2,results3,results4,results5)

#lasso
x_var<-as.matrix(train.data[,-c(1,2)])
y_var<-train.data$BODYFAT
lambda_seq <- 10^seq(2, -2, by = -.1)
c<-glmnet(x_var, y_var , standardize=TRUE, alpha=1)
plot(c)
cv_output <- cv.glmnet(x_var, y_var,
                       alpha = 1, lambda = lambda_seq) 
best_lam <- cv_output$lambda.min
best_lam
lasso_best <- glmnet(x_var, y_var, alpha = 1, lambda = best_lam)
pred <- predict(lasso_best, s = best_lam, newx = as.matrix(test.data[,-c(1,2)]))
final <- cbind(test.data$BODYFAT, pred)
coef(lasso_best)
rel<-data.frame( R2 = R2(pred, test.data$BODYFAT),
            RMSE = RMSE(pred, test.data$BODYFAT),
            MAE = MAE(pred, test.data$BODYFAT))
colnames(rel)<-c("R2","RMSE","MAE")

(results<-rbind(results,rel))
####By comparison, we chose our final model: BODYFAT ~ AGE + ADIPOSITY + CHEST + ABDOMEN + WRIST + AGE:WRIST + ADIPOSITY:CHEST

myfit<-lm(BODYFAT~AGE + ADIPOSITY + CHEST + ABDOMEN + WRIST + AGE:WRIST + ADIPOSITY:CHEST,data=newbf[,-1])
anova(m4,m1)
anova(m1)
summary(myfit)
car::Anova(myfit)
#ci
confint(myfit)



##########Diagnostics#########################
par(mfrow=c(1,1))
##check normality
qqnorm(rstandard(myfit),pch=19,cex=1.2,cex.lab=1.5,cex.main=1.5,
       main="Normal Q-Q Plot of the Residuals")
abline(a=0,b=1,col="black",lwd=3)

# 1. Types of Residuals and Identifying Outliers
# compare residuals (raw, standardized, studentized)
cbind(resid(myfit), rstandard(myfit), rstudent(myfit))
# manually compute and compare
X <- model.matrix(myfit)
H <- X %*% solve(t(X)%*%X) %*% t(X)
# or,
myhat <- hatvalues(myfit)
cbind(myhat, diag(H))
mysigma <- sigma(myfit)
myrstandard <- myfit$residuals/sqrt(mysigma^2 * (1-myhat)) # standardized
cbind(myrstandard, rstandard(myfit))
# can extract sigma_{(i)}
myinfluence <- influence(myfit)
myinfluence$sigma
# studentized
myrstudent <- myfit$residuals/sqrt(myinfluence$sigma^2 * (1-myinfluence$hat)) 
cbind(myrstudent, rstudent(myfit))
par(mfrow=c(1,3))
# Outlying in Y
# check standardized residuals
plot(myfit, which=3)
plot(myfit$fitted.values, sqrt(abs(rstandard(myfit))))
plot(myfit$fitted.values, rstandard(myfit))
# check studentized residuals
plot(myfit$fitted.values, rstudent(myfit))
plot(myfit$fitted.values, rstudent(myfit), ylim=c(-4,4)) 
abline(h=c(-3,3), col="red") # rule of thumb
p <- dim(model.matrix(myfit))[2] 
alpha <- 0.05
t.critical <- qt(1-alpha/(2*n), n-p-1) # Bonferroni correction 
abline(h=c(-t.critical, t.critical), col="green")
# Outlying in X
n <- dim(model.matrix(myfit))[1] 
p <- dim(model.matrix(myfit))[2] 
plot(myinfluence$hat,type = "n") 
abline(h=2*p/n, col="red")
text(myinfluence$hat,label=newbf[,1],col="darkblue")
# 2. Identifying Influential Observations
# DFFITS
dffits(myfit)
plot(dffits(myfit),type = "n")
abline(h=1, col="red") 
abline(h=2*sqrt(p/n), col="green")
text(dffits(myfit),label=newbf[,1],col="darkblue")
# Cook's distance
cooks.distance(myfit)
plot(myfit, which = 4) 
plot(cooks.distance(myfit),type = "n") 
abline(h=1, col="red")
abline(h=qf(0.5, p, n-p), col="green") 
abline(h=4/n, col="blue") 
abline(h=4/(n-p-1-1), col="orange")
text(cooks.distance(myfit),label=newbf[,1],col="darkblue")
# DFBETAS
dfbetas(myfit)
plot(dfbetas(myfit)[,2],type = "n") # DFBETAS_{1(i)} 
abline(h=1, col="red")
abline(h=2/sqrt(n), col="blue")
text(dfbetas(myfit)[,2],label=newbf[,1],col="darkblue")

par(mfrow=c(2,2),mar=c(4,1,1,1))
qqnorm(rstandard(myfit),pch=19,cex=1.2,cex.lab=1.2,cex.main=1.2,
       main="Normal Q-Q Plot of the Residuals")
abline(a=0,b=1,col="black",lwd=3)
plot(myfit$fitted.values, rstandard(myfit),ylab = "standardized residual",xlab = "predicted body fat %",main = "standardized residual plot")
plot(myinfluence$hat,type = "n",ylab = "leverage",xlab = "index",main = "leverage plot") 
abline(h=2*p/n, col="red")
text(myinfluence$hat,label=newbf[,1],col="darkblue")
plot(myfit, which = 4) 



############# Members' contribution to R code #############

# Jonquil Liao: create the model diagnostics code, revised data cleaning code, reviewed MLR part code

# Runze You: create the data cleaning code, revised and maintained MLR part code and model diagnostics code

# Yike Wang: create the MLR part code, reviewed data cleaning and diagnostics code, ultimately responsible for all portions of code


