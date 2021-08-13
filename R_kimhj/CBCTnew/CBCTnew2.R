################Data136

setwd("E:/Dropbox/stoneDS1817/CBCTnew")


library(dplyr)
library(reshape2)

## 데이터 입력
CTdata <- read.csv("CBCTdata.csv", header = T)
CTdata



nptnum <-CTdata$NO
nref <-c(1,2,2)
ngroup <-c(1,2)

Ndata<- expand.grid(NRef=nref, NptNum =nptnum)
Ndata


agedata <-CTdata$AGE
nref <-c(1,2,2)

AGEdata<- expand.grid(NRef=nref, AGE =agedata)
AGEdata

sexdata <-CTdata$SEX
nref <-c(1,2,2)

SEXdata<- expand.grid(NRef=nref, sexdata =sexdata)
SEXdata



E1DCTHU = select(CTdata, NO, E1DCTHU1, E1DCTHU2, E1DCTHU3)
E1DCTHU

df_melt= melt(E1DCTHU, id.vars = "NO" , variable.name = "group", value.name = "E1DCTHU")
df_melt

E1DCTHU =arrange(df_melt, NO)
E1DCTHU

E2DCTHU = select(CTdata, NO, E2DCTHU1, E2DCTHU2, E2DCTHU3)
E2DCTHU

df_melt= melt(E2DCTHU, id.vars = "NO" , variable.name = "group", value.name = "E2DCTHU")
df_melt

E2DCTHU =arrange(df_melt, NO)
E2DCTHU

E1QCTHU = select(CTdata, NO, E1QCTHU1, E1QCTHU2, E1QCTHU3)
E1QCTHU

df_melt= melt(E1QCTHU, id.vars = "NO" , variable.name = "group", value.name = "E1QCTHU")
df_melt

E1QCTHU =arrange(df_melt, NO)
E1QCTHU

E2QCTHU = select(CTdata, NO, E2QCTHU1, E2QCTHU2, E2QCTHU3)
E2QCTHU

df_melt= melt(E2QCTHU, id.vars = "NO" , variable.name = "group", value.name = "E2QCTHU")
df_melt

E2QCTHU =arrange(df_melt, NO)
E2QCTHU


E1DCTBD = select(CTdata, NO, E1DCTBD1, E1DCTBD2, E1DCTBD3)
E1DCTBD

df_melt= melt(E1DCTBD, id.vars = "NO" , variable.name = "group", value.name = "E1DCTBD")
df_melt

E1DCTBD =arrange(df_melt, NO)
E1DCTBD

E2DCTBD = select(CTdata, NO, E2DCTBD1, E2DCTBD2, E2DCTBD3)
E2DCTBD

df_melt= melt(E2DCTBD, id.vars = "NO" , variable.name = "group", value.name = "E2DCTBD")
df_melt

E2DCTBD =arrange(df_melt, NO)
E2DCTBD

E1QCTBD = select(CTdata, NO, E1QCTBD1, E1QCTBD2, E1QCTBD3)
E1QCTBD

df_melt= melt(E1QCTBD, id.vars = "NO" , variable.name = "group", value.name = "E1QCTBD")
df_melt

E1QCTBD =arrange(df_melt, NO)
E1QCTBD

E2QCTBD = select(CTdata, NO, E2QCTBD1, E2QCTBD2, E2QCTBD3)
E2QCTBD

df_melt= melt(E2QCTBD, id.vars = "NO" , variable.name = "group", value.name = "E2QCTBD")
df_melt

E2QCTBD =arrange(df_melt, NO)
E2QCTBD



CTdata2 = cbind(Ndata[,2],Ndata[,1], AGEdata[2], SEXdata[2],E1DCTHU[3], E2DCTHU[3],E1QCTHU[3],  E2QCTHU[3] , E1DCTBD[3],  E2DCTBD[3] ,E1QCTBD[3],E2QCTBD[3])
CTdata2


colnames(CTdata2) =c("No","Ref","Age", "Sex", "E1DCTHU", "E2DCTHU",  "E1QCTHU",  "E2QCTHU", "E1DCTBD", "E2DCTBD", "E1QCTBD", "E2QCTBD")
CTdata2





CTdata2$MDCTHU =(CTdata2$E1DCTHU + CTdata2$E2DCTHU)/2 
CTdata2$MQCTHU =(CTdata2$E1QCTHU + CTdata2$E2QCTHU)/2
CTdata2$MDCTBD =(CTdata2$E1DCTBD + CTdata2$E2DCTBD)/2
CTdata2$MQCTBD =(CTdata2$E1QCTBD + CTdata2$E2QCTBD)/2

CTdata2$LMDCTHU =log(CTdata2$MDCTHU)
CTdata2$LMQCTHU =log(CTdata2$MQCTHU)

CTdata2$agegroup =ifelse(CTdata2$Age < 45, 1, ifelse(CTdata2$Age <65, 2,3) )

rm(list = c("Ndata", "CTdata","df_melt", "E1DCTHU", "E2DCTHU",  "E1QCTHU",  "E2QCTHU", "E1DCTBD", "E2DCTBD", "E1QCTBD", "E2QCTBD"))


attach(CTdata2)
CTdata2












##### E1와 E2 DE-CBCT 전체 그래프 

x<-c(0.8,1,1.2)
for (i in 2:34)
  x= c(x,c(-0.2, 0, 0.2)+i)

colx <-as.numeric((E1DCTHU-E2DCTHU)>0)

plot(x,E1DCTHU, pch=19, xlim=c(1, 34.5), col="blue", axes=F, xlab = "Patient ID (ref1, ref2, ref3)", 
     ylab="CT 영상값", main ="환자별 E1와 E2 DE-CBCT 영상값 (HU) 그래프 (각 환자별 ref 1,2,3)")

for(i in 1:102)
  rect(x[i]-0.02,E1DCTHU[i],x[i]+0.02,E2DCTHU[i], lty =1, col = colx[i]+3)

points(x,E1DCTHU, pch=19, col="blue")
points(x,E2DCTHU, pch=19, col="red")

axis(side = 1, at= 1:34, labels=1:34)
axis(side = 2, at= seq(floor( min(E2DCTHU, na.rm = TRUE)/10)*10,ceiling(max(E2DCTHU, na.rm = TRUE)/10)*10,100))

grid(NA,NULL)
for(i in 1:34)
  abline(v=i+0.5)
legend("topright", legend = c("E1DCTHU", "E2DCTHU"), pch= 19, col=c("blue","red"))




##### E1와 E2 QCT 전체 그래프 

x<-c(0.8,1,1.2)
for (i in 2:34)
  x= c(x,c(-0.2, 0, 0.2)+i)

colx <-as.numeric((E1QCTHU-E2QCTHU)>0)

plot(x,E1QCTHU, pch=19, xlim=c(1, 34.5), col="blue", axes=F, xlab = "Patient ID (ref1, ref2, ref3)", 
     ylab="CT 영상값", main ="환자별 E1와 E2 QCT 영상값 (HU) 그래프 (각 환자별 ref 1,2,3)")

for(i in 1:102)
  rect(x[i]-0.02,E1QCTHU[i],x[i]+0.02,E2QCTHU[i], lty =1, col = colx[i]+3)

points(x,E1QCTHU, pch=19, col="blue")
points(x,E2QCTHU, pch=19, col="red")

axis(side = 1, at= 1:34, labels=1:34)
axis(side = 2, at= seq(floor( min(E2QCTHU, na.rm = TRUE)/10)*10,ceiling(max(E2QCTHU, na.rm = TRUE)/10)*10,100))

grid(NA,NULL)
for(i in 1:34)
  abline(v=i+0.5)
legend("topright", legend = c("E1QCTHU", "E2QCTHU"), pch= 19, col=c("blue","red"))



##### E1와 E2 DE-CBCT 전체 골밀도그래프 

x<-c(0.8,1,1.2)
for (i in 2:34)
  x= c(x,c(-0.2, 0, 0.2)+i)

colx <-as.numeric((E1DCTBD-E2DCTBD)>0)

plot(x,E1DCTBD, pch=19, xlim=c(1, 34.5), col="blue", axes=F, xlab = "Patient ID (ref1, ref2, ref3)", 
     ylab="CT 영상값", main ="환자별 E1와 E2 DE-CBCT 영상값 (BD) 그래프 (각 환자별 ref 1,2,3)")

for(i in 1:102)
  rect(x[i]-0.02,E1DCTBD[i],x[i]+0.02,E2DCTBD[i], lty =1, col = colx[i]+3)

points(x,E1DCTBD, pch=19, col="blue")
points(x,E2DCTBD, pch=19, col="red")

axis(side = 1, at= 1:34, labels=1:34)
axis(side = 2, at= seq(floor( min(E2DCTBD, na.rm = TRUE)*10)/10,ceiling(max(E2DCTBD, na.rm = TRUE)*10)/10,0.1))

grid(NA,NULL)
for(i in 1:34)
  abline(v=i+0.5)
legend("topright", legend = c("E1DCTBD", "E2DCTBD"), pch= 19, col=c("blue","red"))




##### E1와 E2 QCT 전체 골밀도 그래프 

x<-c(0.8,1,1.2)
for (i in 2:34)
  x= c(x,c(-0.2, 0, 0.2)+i)

colx <-as.numeric((E1QCTBD-E2QCTBD)>0)

plot(x,E1QCTBD, pch=19, xlim=c(1, 34.5), col="blue", axes=F, xlab = "Patient ID (ref1, ref2, ref3)", 
     ylab="CT 영상값", main ="환자별 E1와 E2 QCT 영상값 (BD) 그래프 (각 환자별 ref 1,2,3)")

for(i in 1:102)
  rect(x[i]-0.02,E1QCTBD[i],x[i]+0.02,E2QCTBD[i], lty =1, col = colx[i]+3)

points(x,E1QCTBD, pch=19, col="blue")
points(x,E2QCTBD, pch=19, col="red")

axis(side = 1, at= 1:34, labels=1:34)
axis(side = 2, at= seq(floor( min(E2QCTBD, na.rm = TRUE)*10)/10,ceiling(max(E2QCTBD, na.rm = TRUE)*10)/10,0.1))


grid(NA,NULL)
for(i in 1:34)
  abline(v=i+0.5)
legend("topright", legend = c("E1QCTBD", "E2QCTBD"), pch= 19, col=c("blue","red"))



##### DE-CBCT와 QCT 전체 HU 그래프 

x<-c(0.8,1,1.2)
for (i in 2:34)
  x= c(x,c(-0.2, 0, 0.2)+i)

colx <-as.numeric((MDCTHU-MQCTHU)>0)

plot(x,MDCTHU, pch=19, xlim=c(1, 34.5), col="blue", axes=F, xlab = "Patient ID (ref1, ref2, ref3)", 
     ylab="CT 영상값", main ="환자별 DE-CBCT와 QCT 전체 HU 그래프 (각 환자별 ref 1,2,3)")

for(i in 1:102)
  rect(x[i]-0.02,MDCTHU[i],x[i]+0.02,MQCTHU[i], lty =1, col = colx[i]+3)

points(x,MDCTHU, pch=19, col="blue")
points(x,MQCTHU, pch=19, col="red")

axis(side = 1, at= 1:34, labels=1:34)
axis(side = 2, at= seq(floor( min(MQCTHU, na.rm = TRUE)/10)*10,ceiling(max(MQCTHU, na.rm = TRUE)/10)*10,100))

grid(NA,NULL)
for(i in 1:34)
  abline(v=i+0.5)
legend("topright", legend = c("MDCTHU", "MQCTHU"), pch= 19, col=c("blue","red"))




##### DE-CBCT와 QCT 전체 전체 골밀도 그래프 

x<-c(0.8,1,1.2)
for (i in 2:34)
  x= c(x,c(-0.2, 0, 0.2)+i)

colx <-as.numeric((MDCTBD-MQCTBD)>0)

plot(x,MDCTBD, pch=19, xlim=c(1, 34.5), col="blue", axes=F, xlab = "Patient ID (ref1, ref2, ref3)", 
     ylab="CT 영상값", main ="DE-CBCT와 QCT 전체 골밀도  (BD) 그래프 (각 환자별 ref 1,2,3)")

for(i in 1:102)
  rect(x[i]-0.02,MDCTBD[i],x[i]+0.02,MQCTBD[i], lty =1, col = colx[i]+3)

points(x,MDCTBD, pch=19, col="blue")
points(x,MQCTBD, pch=19, col="red")

axis(side = 1, at= 1:34, labels=1:34)
axis(side = 2, at= seq(floor( min(MQCTBD, na.rm = TRUE)*10)/10,ceiling(max(MQCTBD, na.rm = TRUE)*10)/10,0.1))


grid(NA,NULL)
for(i in 1:34)
  abline(v=i+0.5)
legend("topright", legend = c("MDCTBD", "MQCTBD"), pch= 19, col=c("blue","red"))




options(scipen = 100)
options(digits=5)
summary(CTdata2)













############# 오차 분석

diffdens = MDCTBD-MQCTBD

hist(diffdens, breaks =10, main = "MBCT-CBCT density 오차", xlab ="density difference")

summary(diffdens,na.rm = TRUE)
mean(diffdens, na.rm = TRUE)
median(diffdens, na.rm = TRUE)
var(diffdens, na.rm = TRUE)
sd(diffdens, na.rm = TRUE)

absdiff =abs(diffdens)
hist(absdiff, breaks =10, main = "MBCT-CBCT density 오차 절대값", xlab ="density difference")
summary(absdiff, na.rm = TRUE)
mean(absdiff, na.rm = TRUE)
median(absdiff, na.rm = TRUE)
var(absdiff, na.rm = TRUE)
sd(absdiff, na.rm = TRUE)







############### 상관분석 그래프


library("ggpubr")
ggscatter(CTdata2, x = "MQCTBD", y = "MDCTBD", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "QCTdensity", ylab = "DCTdensity")




############### 회귀분석


MDCT_lm =lm(MDCTBD ~ MQCTBD, data=CTdata2)

MDCT_lm
summary(MDCT_lm)


MDCT_lmHU =lm(MDCTHU ~ MQCTHU, data=CTdata2)

MDCT_lmHU
summary(MDCT_lmHU)






############### Bland-Altman plot 2
library(blandr)

blandr.display.and.draw(CTdata2$MDCTBD, CTdata2$MQCTBD, , plotTitle = 'Bland-Altman  plot' )


blandr.display.and.draw(CTdata2$MDCTHU, CTdata2$MQCTHU, , plotTitle = 'Bland-Altman  plot' )



blandr.display.and.draw( CTdata2$LMDCTHU,CTdata2$LMQCTHU, , plotTitle = 'Bland-Altman  plot' )


#################### t-test

shapiro.test(CTdata2$MQCTHU -CTdata2$MDCTHU)
t.test(CTdata2$MQCTHU,CTdata2$MDCTHU, paired =T, alt='two.sided')

shapiro.test(CTdata2$MQCTBD -CTdata2$MDCTBD)
t.test(CTdata2$MQCTBD,CTdata2$MDCTBD, paired =T,alt='two.sided')

summary(CTdata2)

plot(density(CTdata2$MQCTBD , na.rm = TRUE))

summary(CTdata2[13:16])



##################  2차 유효성 ############


CTdata_ds = CTdata2 %>%
  filter(Ref ==1)


CTdata_nl = CTdata2 %>%
  filter(Ref ==2)




############### 회귀분석 시술부위


MDCT_lm =lm(MDCTBD ~ MQCTBD, data=CTdata_ds)

MDCT_lm
summary(MDCT_lm)


MDCT_lmHU =lm(MDCTHU ~ MQCTHU, data=CTdata_ds)

MDCT_lmHU
summary(MDCT_lmHU)

summary(CTdata_ds[13:16])


############### 회귀분석 정상술부위


MDCT_lm =lm(MDCTBD ~ MQCTBD, data=CTdata_nl)

MDCT_lm
summary(MDCT_lm)


MDCT_lmHU =lm(MDCTHU ~ MQCTHU, data=CTdata_nl)

MDCT_lmHU
summary(MDCT_lmHU)



summary(CTdata_nl[13:16])

############### 평가자 간 변동 회귀분석 


MDCT_lmHU1 =lm(E1QCTHU ~ E2QCTHU, data=CTdata2)

MDCT_lmHU1
summary(MDCT_lmHU1)

blandr.display.and.draw(CTdata2$E1QCTHU, CTdata2$E2QCTHU, , plotTitle = 'Bland-Altman  plot' )


MDCT_lmHU2 =lm(E1DCTHU ~ E2DCTHU, data=CTdata2)

MDCT_lmHU2
summary(MDCT_lmHU2)

blandr.display.and.draw(CTdata2$E1DCTHU, CTdata2$E2DCTHU, , plotTitle = 'Bland-Altman  plot' )

summary(CTdata2[5:8])


MDCT_lm1 =lm(E1DCTBD  ~ E2DCTBD , data=CTdata2)

MDCT_lm1
summary(MDCT_lm1)

blandr.display.and.draw(CTdata2$E1QCTBD, CTdata2$E2QCTBD, , plotTitle = 'Bland-Altman  plot' )

MDCT_lm2 =lm(E1QCTBD ~ E2QCTBD, data=CTdata2)

MDCT_lm2
summary(MDCT_lm2)

blandr.display.and.draw(CTdata2$E1DCTBD, CTdata2$E2DCTBD, , plotTitle = 'Bland-Altman  plot' )

summary(CTdata2[9:12])

############### 성별간 차이

#################### t-test




t.test(CTdata2$MDCTHU ~ factor(CTdata2$Sex)  , na.rm = TRUE  )

t.test(CTdata2$MQCTHU ~ factor(CTdata2$Sex)  , na.rm = TRUE  )

t.test(CTdata2$MDCTBD ~ factor(CTdata2$Sex)  , na.rm = TRUE  )

t.test(CTdata2$MQCTBD ~ factor(CTdata2$Sex)  , na.rm = TRUE  )

####################  Age 그룹

library(knitr)

Agegroup1 =   aov(CTdata2$MDCTHU ~ factor(CTdata2$agegroup))
summary(Agegroup1)

tapply(CTdata2$MDCTHU, factor(CTdata2$agegroup), summary)


Agegroup2 =   aov(CTdata2$MQCTHU ~ factor(CTdata2$agegroup))
summary(Agegroup2)

tapply(CTdata2$MQCTHU, factor(CTdata2$agegroup), summary)

Agegroup3 =   aov(CTdata2$MDCTBD ~ factor(CTdata2$agegroup))
summary(Agegroup3)

tapply(CTdata2$MDCTBD, factor(CTdata2$agegroup), summary)

Agegroup4 =   aov(CTdata2$MQCTBD ~ factor(CTdata2$agegroup))
summary(Agegroup4)

tapply(CTdata2$MQCTBD, factor(CTdata2$agegroup), summary)