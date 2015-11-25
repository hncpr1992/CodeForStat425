# Problem #1
data("iris")
# Testing the changes of coefficients and parameters with the data 
# set of iris
lm_data = iris[,c(1,2,3,4)]
# name the four variables as a, b, c and d 
names(lm_data) <- c('a','b','c','d')
# fit models with different changes
# fit1 is the original model
fit1 = lm(a~.,data = lm_data)
summary(fit1)

# (a)
fit2 = lm(a~I(2*b)+c+d,data = lm_data)
summary(fit2)

# (b)
fit3 = lm(I(a+2)~b+c+d,data = lm_data)
summary(fit3)

# (c)
fit4 = lm(a~I(b+2*c)+c+d,data = lm_data)
summary(fit4)

# Problem #3
# (a)
teengamb = read.table("F:/graduate study/Applied Regression/HW2/teengamb.txt")
fitTeen = lm(gamble ~., data = teengamb)
summary(fitTeen)

# (b)
maxindex = which(fitTeen$residuals == max(fitTeen$residuals))
minindex = which(fitTeen$residuals == min(fitTeen$residuals))

# (c)
mean_r = mean(fitTeen$residuals)
median_r = median(fitTeen$residuals)

# (d)
cor_r_pred = cor(fitTeen$residuals, fitted(fitTeen))
cor_r_income = cor(fitTeen$residuals, teengamb$income)

#（f）
ave = apply(FUN = mean,teengamb[,c(2,3,4)], MARGIN = 2)
ave_male = data.frame(sex = 0, status = ave[1], income = ave[2], verbal = ave[3])
row.names(ave_male) = ""
pred1 = predict.lm(fitTeen, ave_male,interval = "prediction")

ave = apply(FUN = max,teengamb[,c(2,3,4)], MARGIN = 2)
ave_male = data.frame(sex = 0, status = ave[1], income = ave[2], verbal = ave[3])
row.names(ave_male) = ""
pred2 = predict.lm(fitTeen, ave_male,interval = "prediction")

# (g)
fitTeen2 = lm(gamble~ income + sex, data = teengamb)
summary(fitTeen2)
anova(fitTeen2,fitTeen)


# Problem #4
# (a)
fit_single = list()
for(i in 1:4){
        formu = as.formula(paste("gamble~", names(teengamb)[i]))
        fit_single[[i]] = lm(formu, data = teengamb)
}
sapply(fit_single, summary)["r.squared",]
anova(fitTeen, fit_single[[3]])

# (b)
fit_single2 = list()
for(i in  1:4){
        formu = as.formula(paste("gamble ~ income + ",
                                 names(teengamb)[i]))
        fit_single2[[i]] = lm(formu, data = teengamb)
}
sapply(fit_single2, summary)["r.squared",]
anova(fitTeen, fit_single2[[1]])

# (c)
fit_single3 = list()
for(i in  1:4){
        formu = as.formula(paste("gamble ~ income + sex +",
                                 names(teengamb)[i]))
        fit_single3[[i]] = lm(formu, data = teengamb)
}
sapply(fit_single3, summary)["r.squared",]
anova(fitTeen, fit_single3[[4]])

# (d)
r_table = numeric()
r_table[1] = sapply(fit_single, summary)["r.squared",][3]
r_table[2] = sapply(fit_single2, summary)["r.squared",][1]
r_table[3] = sapply(fit_single3, summary)["r.squared",][4]
r_table[4] = summary(fitTeen)["r.squared"]

adjr_table = numeric()
adjr_table[1] = sapply(fit_single, summary)["adj.r.squared",][3]
adjr_table[2] = sapply(fit_single2, summary)["adj.r.squared",][1]
adjr_table[3] = sapply(fit_single3, summary)["adj.r.squared",][4]
adjr_table[4] = summary(fitTeen)["adj.r.squared"]

plot(1:4,r_table,type = "o", ylab = "R-squared")
plot(1:4,adjr_table,type = "o", ylab = "Adjusted R-squared")

# Problem 5
iter = 50000
t_temp = vector(length = iter)
for(i in seq(1,iter)){
        temp = teengamb
        temp[,3] = temp[sample(47),3]
        fitTeenTemp = lm(gamble ~., data = temp)
        t_temp[i] = coef(summary(fitTeenTemp))[, "t value"]["income"]
}
p_value = (sum(abs(t_temp) > abs(coef(summary(fitTeen))[, "t value"]["income"])))/iter
















































