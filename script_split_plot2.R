interaction.plot(oats$Variety,oats$Nitrogen,
                 oats$Yield,fun=mean)
boxplot(oats$Yield~oats$Variety)
boxplot(oats$Yield~oats$Nitrogen)

splitplot=aov(Yield~factor(Field)*factor(Variety)*factor(Nitrogen)+
      Error(factor(Field)/factor(Variety)),data=oats)
summary(splitplot)


splitplot1=aov(Yield~factor(Field)*factor(Variety)*factor(Nitrogen),data=oats)
summary(splitplot1)
