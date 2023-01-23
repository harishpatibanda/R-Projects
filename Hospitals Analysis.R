hospitals=read.csv(file.choose(),header=T)

View(hospitals)
attach(hospitals)

install.packages("car")
library(car)
leveneTest(Rating.Overall~Facility.Type)

hospitals.aov=aov(Rating.Overall~ Facility.Type)
summary(hospitals.aov)
hospitals$Rating.Overall=as.factor(hospitals$Rating.Overall)

hospitals.lm=lm(Rating.Overall~Facility.State)
summary(hospitals.lm)
plot(hospitals.lm,1)
plot(hospitals.lm,2)



par(mfrow=c(2,2))
Rating.Overall1=(Rating.Overall[Facility.Type="1" ])
Facility.State1=(Facility.State[Facility.Type="1"])
Rating.Overall2=(Rating.Overall[Facility.Type="2" ])
Facility.State2=(Facility.State[Facility.Type="2"])
Rating.Overall3=(Rating.Overall[Facility.Type="3" ])
Facility.State3=(Facility.State[Facility.Type="3"])
Rating.Overall4=(Rating.Overall[Facility.Type="4" ])
Facility.State4=(Facility.State[Facility.Type="4"])

points(Facility.State1,Rating.Overall1,pch=1,col="red")
points(Facility.State2,Rating.Overall2,pch=0,col="blue")
points(Facility.State3,Rating.Overall3,pch=16,col="orange")

abline(lm(Facility.State1~Rating.Overall1),pch=1,col="red")


model=lm(hospital$Rating.Overall)~(hospital$Facility.Type)
anova(model)
summary(model)
