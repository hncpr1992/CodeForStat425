# HW3 STAT425

# Problem 1
# 1
elec2000 = read.csv("F:/graduate study/Applied Regression/HW3/elec2000.csv",
                    header=T)
elec = elec2000[elec2000$COUNTY !="PALM BEACH",]
attach(elec)
y = BUCHANAN
x = BUSH
fit.elec = lm(y ~ x, data = elec)
plot(x,y, ylab = "BUCHANAN", xlab = "BUSH")
lines(predict(fit.elec)~x, col = "red")

# 2
library(lmtest)
res.elec = fit.elec$residuals
plot(x, res.elec,ylab="residuals")
bptest(fit.elec)

# 3
library(alr3)
tranxy = powerTransform(cbind(y, x) ~ 1)
summary(tranxy)

# 4
x.log = log(x)
y.log = log(y)
fit.elec.log = lm(y.log~x.log)
plot(y.log~x.log, ylab="log(BUCHANAN)", xlab="log(BUSH)")
lines(fitted(fit.elec.log)~x.log,col="red")
 # R-squre
r1 = summary(fit.elec)$r.squared
r2 = summary(fit.elec.log)$r.squared
#bptest
bptest(fit.elec.log)

# 5
new = data.frame(x.log=log(elec2000[elec2000$COUNTY=="PALM BEACH",]$BUSH))
pred.PB.log = predict(fit.elec.log,new, interval = "prediction")
pred.PB = exp(pred.PB.log)
elec2000[elec2000$COUNTY =="PALM BEACH",]$BUCHANAN

# 6
attach(elec2000)
y = BUCHANAN
x = BUSH
detach(elec2000)
# Model with all data
fit.all = lm(y~x)
jack=rstudent(fit.all)
n = length(y)
p = 2
ber.t = qt(.05/(2*n), n-p)
out = elec2000[which(abs(jack) > abs(ber.t)),]$COUNTY

###########################################################

# Problem 2

# 1
library(faraway)
mydata = infmort
mydata$income = log(mydata$income)

myfit = lm(mortality~.,data = mydata)
summary(myfit)

# with oil
data.oil = mydata[mydata$oil=="oil exports",]
attach(data.oil)
plot(income, mortality,xlab="income",ylab="mortality"
        ,main="region with orili export",xlim = c(0,10),ylim=c(0,800),type="n")
text(income[region=='Africa'],mortality[region=='Africa'], 'AF', col="red")
text(income[region=='Asia'],mortality[region=='Asia'], 'AS', col="green")
text(income[region=='Americas'],mortality[region=='Americas'], 'AM', col="black")
abline(b=myfit$coef[5], a=sum(myfit$coef[1]), col="red", lty=1, lwd=1.5)
abline(b=myfit$coef[5], a=sum(myfit$coef[1],myfit$coef[3]), col="green", lty=1, lwd=1.5)
abline(b=myfit$coef[5], a=sum(myfit$coef[1],myfit$coef[4]), col="black", lty=1, lwd=1.5)
detach(data.oil)

# no oil
data.nooil = mydata[mydata$oil!="oil exports",]
attach(data.nooil)
plot(income, mortality,xlab="income",ylab="mortality"
     ,main="region without oil export",xlim = c(0,10),ylim=c(0,500),type="n")

text(income[region=='Africa'],mortality[region=='Africa'], 'AF', col="red")
text(income[region=='Europe'],mortality[region=='Europe'], 'AF', col="blue")
text(income[region=='Asia'],mortality[region=='Asia'], 'AS', col="green")
text(income[region=='Americas'],mortality[region=='Americas'], 'AM', col="black")
abline(b=myfit$coef[5], a=sum(myfit$coef[1],myfit$coef[6]), col="red", lty=1, lwd=1.5)
abline(b=myfit$coef[5], a=sum(myfit$coef[1],myfit$coef[6],myfit$coef[2]), col="blue", lty=1, lwd=1.5)
abline(b=myfit$coef[5], a=sum(myfit$coef[1],myfit$coef[6],myfit$coef[3]), col="green", lty=1, lwd=1.5)
abline(b=myfit$coef[5], a=sum(myfit$coef[1],myfit$coef[6],myfit$coef[4]), col="black", lty=1, lwd=1.5)
detach(data.nooil)


# 2
n=length(mydata$mortality)
p=6
lev = influence(myfit)$hat
names(lev[lev>2*p/n]) 
halfnorm(lev, 10, labs=row.names(mydata), ylab="Leverages")

# 3
jack = rstudent(myfit)
n=length(mydata$mortality)
p=6
ber.t = qt(0.05/(2*n),n-p)
out = row.names(mydata[which(abs(jack) > abs(ber.t)),])

# 4
cook_dist = cooks.distance(myfit)
high_inf = row.names(mydata[which(cook_dist>1),])
halfnorm(cook_dist, labs=row.names(mydata), ylab="Cook's distances")

# 5
anova(lm(mortality ~ income+region+oil+ income:oil+income:region, mydata))
anova(lm(mortality ~ income+region+oil+ income:region+ income:oil, mydata))

# Problem 3
# Preparation
g=lm(sr~pop15+pop75+dpi+ddpi, data = savings)
newsavings = savings
newsavings$sr = g$fitted
sigma.hat = summary(g)$sigma
remove(g)

# 1
n = length(newsavings$sr)
p = 5
rss = sigma.hat^2 * (n-p)
ess = sum((newsavings$sr-mean(newsavings$sr))^2)
tss = rss+ess
adj_r = 1-((rss)/(n-p))/(tss/(n-1))

# 2
newfit = lm(sr~pop15+pop75+dpi+ddpi, data = newsavings)
summary(newfit)

# 3
pop75.beta = newfit$coefficients[3]
h = model.matrix(newfit)
pop75.sigma2 = solve(t(h)%*%h)[3,3]*sigma.hat^2
t_value = pop75.beta/sqrt(pop75.sigma2)
p_value = pt(t_value,n-p)*2


# 4
g1=lm(sr~dpi+ddpi, data = newsavings)
g2=lm(sr~dpi+ddpi+pop15+pop75, data = newsavings)
anova(g1,g2)
f = ((174-0)/2)/(650.713/45)
p = pf(f,2,45,lower.tail = F)


# Problem 4
# 1
galton = read.table("F:/graduate study/Applied Regression/galton.txt",
                    header = F)
names(galton) = c("parent","offspring","std")
gfit = lm(offspring~parent, data = galton, weights = 1/std^2)
fit =lm(offspring~parent, data = galton)
attach(galton)
plot(parent, offspring, xlab = "parent", ylab = "offspring")
abline(a = gfit$coefficients[1],b = gfit$coefficients[2], col = "red", lty=1)
abline(a = fit$coefficients[1],b = fit$coefficients[2], col = "blue", lty=2)
legend("topleft", inset = .05, c("WLS","OLS"), lty = c(1,2),col = c("red","blue"))

# 2
newdata = data.frame(parent = 20)
pred = predict(gfit, newdata)
q_3rd = qnorm(3/4, mean=pred, sd=1.938)

# 3
n = 7
p = 2
beta = gfit$coefficients[2]
beta_std = 0.03815
t_value = (beta-1)/beta_std
p_value = pt(t_value, df = n-p)

# 4
sigma = (summary(gfit)$sigma)
chi_sq = (n-p)*sigma^2/1
pchisq(chi_sq,df = 5,lower.tail = F)
