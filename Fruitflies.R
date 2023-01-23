#Fruit flies
#An experiment to test whether increased in sexual activity decreases the life spans of male fruit flies
#125 male fruit flies randomly assigned to 5 groups (Treatments)

flies=read.csv(file.choose(), header=T)
attach(flies)

#Look at dotplot
par(mfrow=c(1, 2))
stripchart(Longevity,method="stack", pch=20, cex=1.5)

#What do groups look like?
#produce comparative dotplots
stripchart(Longevity~Treatment, method="stack", pch=20, cex=1.5)
#If you want to see the dotplots vertically, use vertical=TRUE)

#produce comparative boxplots
boxplot(Longevity)
boxplot(Longevity~Treatment)

#Do residuals look better for null model or ANOVA model?
#Null model
flies.null=lm(Longevity~1)
#ANOVA model
flies.aov=aov(Longevity~Treatment)
summary(flies.aov)
hist(flies.null$residuals, breaks=8)
hist(flies.aov$residuals, breaks=8)

#re-format plots
par(mfrow=c(1, 1))

#Make table including n, mean, sd
#For groups
n=tapply(Longevity, Treatment, length)
mean=tapply(Longevity, Treatment, mean)
SD=round(tapply(Longevity, Treatment, sd),2)
groupdata=cbind(n, mean, SD)
groupdata

#Add row for totals
totaln=length(Longevity)
grandmean=mean(Longevity)
grandsd=sd(Longevity)
totaldata=cbind(totaln, grandmean, grandsd)

rbind(groupdata, totaldata)

#Check conditions
# Equal variance 
# Normal error distribution
# Independent errors with mean=0: Based on how data are collected.
#   If there is randomization, we typically assume independence

#Equal variance condition
# Take ratio of largest groupSD and smallest groupSD
# It should be less than or equal to 2
16.45/12.1
# 1.3595
# Resids vs Fits allows us to visually check similarity of spreads
plot(flies.aov$fitted.values, flies.aov$residuals)
abline(h=0)

#Normal probability plot of residuals
qqnorm(flies.aov$residuals)
qqline(flies.aov$residuals)

#Once conditions are met, we can use model for inference

