################Data136

#####setwd("C:/Users/stone/Dropbox/stoneDS1817/김현정통계/MBCT")

#setwd("C:/Users/stone/OneDrive/바탕 화면/CBCTdata")

setwd("D:/Dropbox/stoneDS1817/김현정통계/MBCT")

###data.csv 1차  dat2.csv 2ㅊ

## 데이터 입력############################################
CTdata <- read.csv("data3.csv", header = F)
ptdata <- read.csv("pt.csv", header = F)

CTdata<- as.matrix(CTdata)
ptdata<- as.matrix(ptdata)

nptnum <-ptdata[,4]
nref <-c(1,2,2)
ngroup <-c(1,2)

Ndata<- expand.grid(NRef=nref, NptNum =nptnum)
Ndata

CTdata = cbind(Ndata[,2],Ndata[,1],CTdata,CTdata[,3]-CTdata[,6])
colnames(CTdata) =c("No","Ref","CBCTmean", "CBCTsd","CBCTd","MDCTmean", "MDCTsd","MDCTd","diffdens")
CTdata

CTdata <-data.frame(CTdata)
CTdata   ### 통계의 기본 데이터


attach(CTdata)

##### 자료 전체 그래프 ####################################################

x<-c(0.8,1,1.2)
for (i in 2:15)
  x= c(x,c(-0.2, 0, 0.2)+i)

colx <-as.numeric(diffdens>0)

plot(x,CBCTd, pch=19, xlim=c(1, 15.5),ylim =c(0.96,1.4), col="blue", axes=F, xlab = "Patient ID (ref1, ref2, ref3)", 
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

##########################################################




options(scipen = 100)
options(digits=5)
summary(CTdata[,c(5,8,9)])

############# 오차 분석

hist(diffdens, breaks =10, main = "CBCT-MBCT density 오차", xlab ="density difference")

summary(diffdens)
mean(diffdens, na.rm = TRUE)
median(diffdens, na.rm = TRUE)
var(diffdens, na.rm = TRUE)
sd(diffdens, na.rm = TRUE)

var(CBCTd, na.rm=TRUE)
sd(CBCTd, na.rm=TRUE)
var(MDCTd, na.rm=TRUE)
sd(MDCTd, na.rm=TRUE)


###################################  절대값 오차

absdiff =abs(diffdens)
hist(absdiff, breaks =10, main = "CBCT-MBCT density 오차 절대값", xlab ="density difference")
summary(absdiff,na.rm = TRUE)
mean(absdiff, na.rm = TRUE)
median(absdiff, na.rm = TRUE)
var(absdiff, na.rm = TRUE)
sd(absdiff, na.rm = TRUE)


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

############### 회귀분석 그래프

nomit= length(na.omit(diffdens))


pred.frame =data.frame(CBCTd=seq(1,1.4,length.out =nomit ))

pc=predict(MDCT_lm, int="c", newdata = pred.frame)
pp=predict(MDCT_lm, int="p", newdata = pred.frame)

pred.X=pred.frame$CBCTd
plot(CTdata$CBCTd , CTdata$MDCTd, ylim=c(0.9,1.5), xlab = "CBCTdensity", ylab="MDCTdensity", col=CTdata$Ref, pch=19)
text(CTdata$CBCTd+0.005 , CTdata$MDCTd-0.005,  labels=LETTERS[CTdata$No])

matlines(pred.X, pc, lty=c(1,2,2), col="BLUE")
matlines(pred.X, pp, lty=c(1,3,3), col="RED")
legend("topleft", legend = c("Ref 1", "Ref 2.3"), pch= 19, col=CTdata$Ref)

프###################################################################

### 아래는 무시
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
#bland.altman.plot(MDCTd, CBCTd, graph.sys = "ggplot2", conf.int=.95, col=C, geom_count = TRUE)

bland.altman.plot(CBCTd, MDCTd, graph.sys = "ggplot2", conf.int=.95, col=C, geom_count = TRUE)



############### Bland-Altman plot 2
library(blandr)

blandr.display.and.draw( CBCTd, MDCTd, , plotTitle = 'Bland-Altman  plot' )




