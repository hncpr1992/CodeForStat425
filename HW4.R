# HW4 STAT425

##############################################################
# Problem 1
# a
library(faraway)
year = aatemp$year
year[year<=1930] = 0
year[year>1930] =  year[year>1930] - 1930
temp = aatemp$temp
myfit1 = lm(temp~year)
summary(myfit)
 
# b
myfit2 = lm(aatemp$temp~aatemp$year)
plot(aatemp$year,aatemp$temp,xlab = "year", ylab = "temp")
lines(aatemp$year, fitted(myfit), col="red")
lines(aatemp$year,fitted(myfit2), col="blue")
 
# c
summary(myfit1)
summary(myfit2)


####################################################################
 
# Problem 2
 
# a
# backward
temp = aatemp$temp
year = aatemp$year

fit_poly = list()
length(fit_poly) = 10
for (i in 1:10){
        fit_poly[[i]] = lm(temp~poly(year,11-i))
}
lapply(fit_poly, summary)

newdata = data.frame(year = 2015)
fit_poly_1 = lm(temp~poly(year,5))
pred_2015_1 = predict(fit_poly, newdata = newdata,
                      interval = "confidence")
pred_2015_2 = predict(fit_poly, newdata = newdata,
                      interval = "prediction") 
 
# b
#forward
fit_poly_a = lm(temp~year, data = aatemp)
fit_poly_b = lm(temp~year+I(year^2), data = aatemp)
fit_poly_2 = lm(temp~year, data = aatemp)
newdata = data.frame(year = 2015)
pred_2015_1 = predict.lm(fit_poly_2, newdata = newdata,interval
                         = "confidence")
pred_2015_2 = predict.lm(fit_poly_2, newdata = newdata,interval
                         = "prediction")

# c
library(splines)
knots_amt = 1:11

# Generate 10 folds
library(caret)
flds = createFolds(temp, k = 10, list = TRUE, returnTrain = FALSE)
train_data = lapply(flds,function(x){aatemp[-x,]})
test_data = lapply(flds,function(x){aatemp[x,]})

# Cubic Spline Regression with train data
err_all = c()
for (i in knots_amt+3){
        err_each = c()
        for (j in 1:10){
                x = train_data[[j]][,1]
                y = train_data[[j]][,2]
                fit = lm(y ~ bs(x, df=i))
                x.new = test_data[[j]][,1]
                pred = predict.lm(fit, newdata = data.frame(x = x.new))
                err_each = c(err_each,sum(abs(pred-test_data[[j]][,2]))/length(test_data[[j]][,1]))
        }
err_all = c(err_all, mean(err_each))                 
}

d = which(err_all == min(err_all))
plot.ts(1:11,err_all,main = "CV error vs Knots Number",lty = 1)

fit_cub_3 = lm(temp ~ bs(year, df=2+3))
x.new = 2015
pred_2015_1 = predict.lm(fit_cub_3, newdata = list(year = x.new),
                         interval = "prediction")
pred_2015_2 = predict.lm(fit_cub_3, newdata = list(year = x.new),
                         interval = "confidence") 

# d
plot(year,temp,main = "model comparison") 
lines(year,fitted(fit_poly_a),col = "red", lty = 1)
lines(year,fitted(fit_poly_b), col = "blue",lty = 40)
lines(year,fitted(fit_cub_c),col = "green", lty = 20)
legend("topleft",c("forward","backward","cubic spline"),
       lty = c(1,40,20), col = c("red","blue","green"))
 

##############################################################

# Problem 3
 
# a
boxplot(weight~feed, data = chickwts)
stripchart(weight~feed, data=chickwts, method="jitter",
            col="blue", vertical=TRUE, add=TRUE)
 
# b
fit_ch = lm(weight~feed, data = chickwts)
anova(fit_ch)
 
# c
qqnorm(fit_ch$residuals)
qqline(fit_ch$residuals)
 
library(Rcmdr)
attach(chickwts)
leveneTest(weight,feed)
 
 
 
 
 
 
 