################Data136

setwd("C:/Users/stone/Dropbox/stoneDS1817/김현정통계/MBCT")

###data.csv 1차  dat2.csv 2ㅊ

## 데이터 입력
CTdata <- read.csv("data2.csv", header = F)
ptdata <- read.csv("pt.csv", header = F)

CTdata<- as.matrix(CTdata)
ptdata<- as.matrix(ptdata)

nptnum <-ptdata[,4]
nref <-c(1,2,2)
ngroup <-c(1,2)

Ndata<- expand.grid(NRef=nref, NptNum =nptnum)
Ndata

CTdata = cbind(Ndata[,2],Ndata[,1],CTdata,CTdata[,6]-CTdata[,3])
colnames(CTdata) =c("No","Ref","CBCTmean", "CBCTsd","CBCTd","MDCTmean", "MDCTsd","MDCTd","diffdens")
CTdata

CTdata <-data.frame(CTdata)
CTdata   ### 통계의 기본 데이터


attach(CTdata)

##### 자료 전체 그래프 

x<-c(0.8,1,1.2)
for (i in 2:15)
  x= c(x,c(-0.2, 0, 0.2)+i)

colx <-as.numeric(diffdens>0)

plot(x,CBCTd, pch=19, xlim=c(1, 15.5),ylim =c(1.0,1.4), col="blue", axes=F, xlab = "Patient ID (ref1, ref2, ref3)", 
     ylab="CT density", main ="환자별 CBCT와 MBCT의 density 그래프 (각 환자별 ref 1,2,3)")

for(i in 1:45)
  rect(x[i]-0.02,CBCTd[i],x[i]+0.02,MDCTd[i], lty =1, col = colx[i]+3)

points(x,CBCTd, pch=19, col="blue")
points(x,MDCTd, pch=19, col="red")

axis(side = 1, at= 1:15, labels=LETTERS[1:15])
axis(side = 2, at= seq(1.0,1.4,0.1))
grid(NA,NULL)
for(i in 1:15)
  abline(v=i+0.5)
legend("topright", legend = c("CBCTdensity", "MDCTdensity"), pch= 19, col=c("blue","red"))

options(scipen = 100)
options(digits=5)
summary(CTdata[,c(5,8,9)])

############# 오차 분석

hist(diffdens, breaks =10, main = "MBCT-CBCT density 오차", xlab ="density difference")

summary(diffdens)
mean(diffdens)
median(diffdens)
var(diffdens)
sd(diffdens)

absdiff =abs(diffdens)
hist(absdiff, breaks =10, main = "MBCT-CBCT density 오차 절대값", xlab ="density difference")
summary(absdiff)
mean(absdiff)
median(absdiff)
var(absdiff)
sd(absdiff)
############### 상관분석
plot(CBCTd, MDCTd)



CTcor= cor.test(CBCTd, MDCTd, type="pearson") # type can be pearson or spearman
CTcor


library(pwr)
pwr.r.test(n = NULL, r = 0.924, sig.level = 0.05, power = 0.9,
           alternative = c("two.sided", "less","greater"))

pwr.r.test(n = NULL, r = 0.924, sig.level = 0.01, power = 0.9,
           alternative = c("two.sided", "less","greater"))

############### 상관분석 그래프


library("ggpubr")
ggscatter(CTdata, x = "CBCTd", y = "MDCTd", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "CBCTdensity", ylab = "MDCTdensity")




############### 회귀분석


MDCT_lm =lm(MDCTd ~ CBCTd, data=CTdata)

MDCT_lm
summary(MDCT_lm)


pred.frame =data.frame(CBCTd=seq(1,1.44,0.01))

pc=predict(MDCT_lm, int="c", newdata = pred.frame)
pp=predict(MDCT_lm, int="p", newdata = pred.frame)

pred.X=pred.frame$CBCTd
plot(CTdata$CBCTd , CTdata$MDCTd, ylim=range(CTdata$MDCTd,pp),xlab = "CBCTdensity", ylab="MDCTdensity", col=CTdata$Ref, pch=19)
text(CTdata$CBCTd+0.005 , CTdata$MDCTd-0.005,  labels=LETTERS[CTdata$No])

matlines(pred.X, pc, lty=c(1,2,2), col="BLUE")
matlines(pred.X, pp, lty=c(1,3,3), col="RED")
legend("topleft", legend = c("Ref 1", "Ref 2.3"), pch= 19, col=CTdata$Ref)




plot(CBCTd, MDCT_lm$residuals, pch=19)
abline(h=0, lty=2)

anova(MDCT_lm)
qqnorm(MDCTd)
qqline(MDCTd)
shapiro.test(MDCTd)
boxplot(MDCTd)

AIC(MDCT_lm)  # AIC => 419.1569
BIC(MDCT_lm)



modelSummary <- summary(MDCT_lm)  # capture model summary as an object
modelCoeffs <- modelSummary$coefficients  # model coefficients
beta.estimate <- modelCoeffs["CBCTd", "Estimate"]  # get beta estimate 
std.error <- modelCoeffs["CBCTd", "Std. Error"]  # get std.error 
t_value <- beta.estimate/std.error  # calc t statistic
p_value <- 2*pt(-abs(t_value), df=nrow(cars)-ncol(cars))  # calc p Value
f_statistic <- MDCT_lm$fstatistic[1]  # fstatistic
f <- summary(MDCT_lm)$fstatistic  # parameters for model p-value calc
model_p <- pf(f[1], f[2], f[3], lower=FALSE)




MDCT_lmzero =lm(MDCTd ~ CBCTd+ 0, data=CTdata)

MDCT_lmzero
summary(MDCT_lmzero)


pred.frame =data.frame(CBCTd=seq(1,1.44,0.01))

pc=predict(MDCT_lmzero, int="c", newdata = pred.frame)
pp=predict(MDCT_lmzero, int="p", newdata = pred.frame)

pred.X=pred.frame$CBCTd
plot(CTdata$CBCTd , CTdata$MDCTd, ylim=range(CTdata$MDCTd,pp),xlab = "CBCTdensity", ylab="MDCTdensity", col=CTdata$No, pch=19)

matlines(pred.X, pc, lty=c(1,2,2), col="BLUE")
matlines(pred.X, pp, lty=c(1,3,3), col="RED")



############### Bland-Altman plot

library(BlandAltmanLeh)
library(ggplot2)
ba1.stats <- bland.altman.stats(MDCTd,CBCTd)
ba1.stats
summary(ba1.stats)
bland.altman.plot(MDCTd, CBCTd, graph.sys = "ggplot2", conf.int=.95, col=C, geom_count = TRUE)



library(blandr)
library(shiny)
# Generates two random measurements

# Generates a plot, with no optional arguments
blandr.display.and.draw( MDCTd,CBCTd )
# Generates a plot, with title
blandr.display.and.draw( MDCTd,CBCTd , plotTitle = 'Bland-Altman  plot' )
blandr.method.comparison( MDCTd,CBCTd )
blandr.output.report(MDCTd,CBCTd)

data(giavarina.2015)







library(pwr)
delta <- 20
sigma <- 60
d <- delta/sigma
pwr.t.test(d=d, sig.level=.05, power = .90, type = 'two.sample')

library(pwr)

# For a one-way ANOVA comparing 5 groups, calculate the
# sample size needed in each group to obtain a power of
# 0.80, when the effect size is moderate (0.25) and a
# significance level of 0.05 is employed.

pwr.anova.test(k=5,f=.25,sig.level=.05,power=.8)

# What is the power of a one-tailed t-test, with a
# significance level of 0.01, 25 people in each group, 
# and an effect size equal to 0.75?

pwr.t.test(n=25,d=0.75,sig.level=.01,alternative="greater")

# Using a two-tailed test proportions, and assuming a
# significance level of 0.01 and a common sample size of 
# 30 for each proportion, what effect size can be detected 
# with a power of .75? 

pwr.2p.test(n=30,sig.level=0.01,power=0.75)

# Plot sample size curves for detecting correlations of
# various sizes.

library(pwr)

# range of correlations
r <- seq(.5,.85,.01)
nr <- length(r)

# power values
p <- seq(.4,.9,.1)
np <- length(p)

# obtain sample sizes
samsize <- array(numeric(nr*np), dim=c(nr,np))
for (i in 1:np){
  for (j in 1:nr){
    result <- pwr.r.test(n = NULL, r = r[j],
                         sig.level = .05, power = p[i],
                         alternative = "two.sided")
    samsize[j,i] <- ceiling(result$n)
  }
}

# set up graph
xrange <- range(r)
yrange <- round(range(samsize))
colors <- rainbow(length(p))
plot(xrange, yrange, type="n",
     xlab="Correlation Coefficient (r)",
     ylab="Sample Size (n)" )

# add power curves
for (i in 1:np){
  lines(r, samsize[,i], type="l", lwd=2, col=colors[i])
}

# add annotation (grid lines, title, legend) 
abline(v=0, h=seq(0,yrange[2],50), lty=2, col="grey89")
abline(h=0, v=seq(xrange[1],xrange[2],.02), lty=2,
       col="grey89")
title("Sample Size Estimation for Correlation Studies\n
      Sig=0.05 (Two-tailed)")
legend("topright", title="Power", as.character(p),
       fill=colors)


