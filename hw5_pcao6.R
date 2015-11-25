# Porblem 1
library(MASS)
library(faraway) 

# (a)
myfit = lm(breaks~wool*tension-1,data = warpbreaks)
boxcox(myfit)

# (b)
myfit_log = lm(log(breaks)~wool*tension,data = warpbreaks)
anova(myfit_log)

# (c)
attach(warpbreaks)
TukeyHSD(aov(log(breaks) ~ wool:tension))
detach(warpbreaks)

# Problem 2

# (a)
library(lattice) 

newbarley=barley
newbarley$year[newbarley$site=="Morris"]=
        ifelse(newbarley$year[newbarley$site=="Morris"]==1931, 1932, 1931)

attach(newbarley)
dotplot(variety ~ yield | site, data = newbarley, groups = year,
        key = simpleKey(levels(barley$year), space = "right"),
        xlab = "New Barley Yield (bushels/acre) ", aspect=0.5,
        layout = c(2,3), ylab=NULL)
detach(newbarley)

# (b)
myfit = lm(yield~variety+year+site+variety:year+year:site+variety:site
           ,data = newbarley)

anova(myfit)
plot(myfit)

# (c)
newbarley[23,]
newbarley[83,]

# (d)
newbarley[c(23,83),]$year = ifelse(newbarley[c(23,83),]$year==
                                           1931,1932,1931)

myfit2 = lm(yield~variety+year+site+variety:year+year:site+variety:site
           ,data = newbarley)

anova(myfit2)


# Problem 3

# (b)
attach(eggprod)
g=lm(eggs~., eggprod)
anova(g)

# (c)
g2=lm(eggs~treat,eggprod)
anova(g2)

# (e)
TukeyHSD(aov(g))


# Problem 4

load("F:/graduate study/Applied Regression/HW5/BostonHousing.Rdata")
bdata = BostonHousing
n=dim(bdata)[1]; p=dim(bdata)[2]-1;


fullModel = lm(Y~., data = bdata)
summary(fullModel)

stepAIC = step(fullModel, trace=0, direction="backward")     
summary(stepAIC)

stepwiseAIC = step(fullModel, trace=0, direction="both")     
summary(stepwiseAIC)

stepBIC = step(fullModel, trace=0, direction="backward", k=log(n))     
summary(stepBIC)

stepwiseBIC = step(fullModel, trace=0, direction="both")     
summary(stepwiseBIC)









