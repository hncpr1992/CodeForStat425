#For STAT425, Professor: Feng 
#Homework #1
#Student Name: Peiran Cao
#NETID: pcao6

#############################################################

# Problem #3
#read in the data(the address can be modified for TA's testing)
load("F:/graduate study/Applied Regression/HW1/coffee.Rdata")

#data transformation
coffee$temp <- log(coffee$temp)
names(coffee)[2] <- "temp_log"


#(a) 
#Conduncting LS estimation
attach(coffee)
fit <- lm(temp_log~time,data = coffee)
summary(fit)
LnA <- fit$coefficients[1]
LnB <- fit$coefficients[2]
A <- exp(LnA)
B <- exp(LnB)
#Obtain the confidence interval of parameters LnA and LnB
CI_log <- confint(fit)
#Obtain the confidence interval of parameters A and B
CI <- exp(CI_log)
detach(coffee)



#(b)
#read in raw data and plot(the address can be modified for TA's testing)
load("F:/graduate study/Applied Regression/HW1/coffee.Rdata")
plot(coffee$temp~coffee$time, xlab = "time", ylab = "temperature",
     main = "Coffee")
#plot the line of exponantial model
x <- coffee$time
y <- A*B^x
lines(y~x, xlab = "time", ylab = "temperature", main = "coffee")



#(f)
required_temp = 68.33
#Break the time into pieces
time_for_pred <- data.frame(time = seq(0, 30, by = 0.01))
#Get the prediction intervel for each time point
lm_pred <- predict(fit, time_for_pred, level = 0.95, 
                   interval = "prediction")
#Find the time point whose upper bound of its prediction intervel is
#below 68.33 for the first time
lm_pred <- as.data.frame(lm_pred)
lm_pred$upr <- exp(lm_pred$upr)
tm_for_serve <- time_for_pred[which(lm_pred$upr <= required_temp)[1],]

############################################
# Problem #4
library(ggplot2)
library(reshape2)

#read in, clean and transform the raw data
#(the address can be modified for TA's testing)
gift_data <- read.csv("F:/graduate study/Applied Regression/HW1/gift.csv")
names(gift_data) <- c("week", "GF", "BF")
gift_data <- gift_data[-(1:4),]
gift_data$GF <- as.numeric(as.character(gift_data$GF))
gift_data$BF <- as.numeric(as.character(gift_data$BF))

#Create a new variable "week_order" to replace the "week" variable to
#simplify the analysis
gift_data$week_order <- seq(1,length(gift_data$week),by = 1)

#Plot the GF and BF on the same canvas
gift_data2 <- gift_data[,c(2,3,4)]
gift_data_long <- melt(gift_data2, id = "week_order")
p <- ggplot(gift_data_long, aes(x = week_order, y = value, colour = variable)) + 
        geom_line() + geom_point()
plot(p)

#plot the scatterplot of BF and GF
qplot(BF, GF, data = gift_data, geom = c("point"))



#(b)
#LS without transformation
fit1 <- lm(BF~GF, data = gift_data)
R_squqre1 <- summary(fit1)$r.squared

#LS with transformation
fit2 <- lm(log(BF)~log(GF), data = gift_data)
R_squqre2 <- summary(fit2)$r.squared



#(c)
#Hypothesis #1
boxplot(gift_data$BF,gift_data$GF, names = c("BF", "GF"))
# ANOVA
fit.anova <- aov(GF~BF, data = gift_data)
summary(fit.anova)

#1. test of normality
attach(gift_data)
shapiro.test(GF)
shapiro.test(BF)

#2. Bartlett test
test_obj <- list(GF = GF, BF = BF)
bartlett.test(test_obj)

#Wilcox rank sum test to test the difference between two samples
wilcox.test(GF,BF)



#Hypothesis 2
library(tseries)

#E-G two steps test
fit_GB <- lm(GF~BF, data = gift_data)
summary(fit_GB)
adf.test(residuals(fit_GB))

















































