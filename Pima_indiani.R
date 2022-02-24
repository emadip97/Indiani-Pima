setwd("C:\\Users\\emanu\\Desktop\\Ema\\Unisi\\Laurea Magistrale\\1° Anno 2° Semestre\\Inferenza Statistica\\Dati tesine")
Data<-read.table("pima-indians.txt", header = T, sep = "", dec = ".")


library(car)
attach(Data)
Data$Age_Class<-recode(Age, "21:25=1; 26:31=2; 32:41=3; 42:50=4; 51:hi=5")
Data$Diabetes<-recode(Diabetes, "0='No'; 1='Si'")
attach(Data)

Data$Age_Class<-as.factor(Age_Class)
Data$Diabetes<-as.factor(Diabetes)
attach(Data)

#Togliere imc=0

Data <- Data[-c(10,50,61,82,146,372,427,495,523,685,707),]



#Analisi esplorativa univariata

#quantitative

setwd("C:\\Users\\emanu\\Desktop\\Emanuele\\Pima")


png("boxplot_BMI2.png")
boxplot(BMI, boxwex = 0.2, main = "Box-and-whiskers plot", 
        ylab = "Indice di massa corporea (IMC)")
rug(BMI, side = 2)
dev.off()

png("boxplot_età.png")
boxplot(Age, boxwex = 0.2, main = "Box-and-whiskers plot", 
        ylab = "Età")
rug(BMI, side = 2)
dev.off()

summary(BMI)
summary(Age)


#commento


#funzioni

variance <- function(x){
  m2 <- sum((x - mean(x))^2)/length(x)
  m2}
skewness <- function(x){
  s3 <- sum((x - mean(x))^3)/length(x)/sqrt(variance(x))^3
  s3}
kurtosis <- function(x){
  s4 <- sum((x - mean(x))^4)/length(x)/variance(x)^2
  s4}


#DHR
variance(BMI)
skewness(BMI)
kurtosis(BMI)

#Istogrammi

#1

png("isto_bmi.png")
hist(BMI)
dens<-density(BMI)
hist(BMI,probability = T, ylim = c(0, 0.07), main = "Istogramma", 
     xlab = "Indice di massa corporea (IMC)", ylab = "Densità")
lines(dens)
dev.off()


#qualitative

library(lattice)

png("barchart_diab.png")
barchart(table(Diabetes), xlab = "Frequenza Diabete", main = "Barplot")
dev.off()

table(Diabetes)

png("barchart_age.png")
barchart(table(Age_Class), xlab = "Frequenza Classi età", main = "Barplot")
dev.off()

table(Age_Class)

#Analisi bivariata

pairs(Data[, 1:3], main = "Scatter-plot matrix")

plot(Data[, 1:3])
cor(Data[, 2:3])

#...

#1

png("plot_imc_age.png")
plot(BMI, Age, xlab = "Indice di massa corporea (IMC)",
     ylab = "Età", main = "Scatter plot")
dev.off()

cor(BMI, Age)

#2

png("barchart_biv.png")
barchart(table(Data$Diabetes, Data$Age_Class), ylab = "Diabete", xlab = "Frequenza",
         auto.key = list(title = "Classi età", cex = 0.8))
dev.off()
w<-table(Diabetes, Data$Age_Class)
prop.table(w)
#Analisi condizionata

#1

png("boxplot_cond_diab.png")
boxplot(BMI ~ Diabetes, boxwex = 0.2, xlab = "Diabete", ylab = "Indice di massa corporea (IMC)",
        names=c("No", "Si"),
        main = "Box-and-whiskers plot")
dev.off()

by(BMI, Diabetes, median)

#2

png("boxplot_cond_age.png")
boxplot(BMI ~ Age_Class, boxwex = 0.2, xlab = "Classi d'età", ylab = "Indice di massa corporea (IMC)",
        names=c("21-25", "26-31", "32-41", "42-50", "Over 50"),
        main = "Box-and-whiskers plot")
dev.off()

library(lattice)

xyplot(BMI ~ Age | Diabetes , xlab = "Età",   
       strip = strip.custom(strip.names = F, strip.levels = T), 
       ylab = "Indice di massa corporea (IMC)",
       main = "Scatter plot condizionato a Diabete")



xyplot(BMI ~ Age_Class | Diabetes , xlab = "Età",   
       strip = strip.custom(strip.names = F, strip.levels = T),
       ylab = "Indice di massa corporea (IMC)",
       main = "Scatter plot condizionato a Diabete")



#funzione di ripartizione empirica

#1

png("ecdf.png")
plot(ecdf(BMI), do.points = F, verticals = T,
     xlab = "Indice di massa corporea (IMC)", ylab = "Probabilità",
     main = "Funzione di ripartizione empirica")
rug(BMI)
dev.off()


#verosimiglianza

mean(BMI)
var(BMI)

png("qqplot.png")
qqnorm(BMI)
qqline(BMI)
dev.off()




#stimatori di nucleo univariati
#Parametro di smorzamento automatico

library(sm)

#BMI_Plugin

png("dens_cv.png")
sm.density(BMI, hcv(BMI, hstart = 1, hend = 4.00),
           yht = 0.08, xlim = c(15, 70), xlab = "Indice di massa corporea (IMC)", ylab = "Probabilità")
title(main = "Stima densità ('CV h=1.95')")
dev.off()

png("dens_plug.png")
sm.density(BMI, hsj(BMI), yht= 0.08, xlim = c(15, 70),
           xlab = "Indice di massa corporea (IMC)", ylab = "Probabilità")
title(main = "Stima densità ('Plug-in h=1.81')")
dev.off()

#stimatori nucleo bivariati

#bmi, age

hsj<-c(hsj(BMI), hsj(Age))

png("sm_biv.png")
plot(BMI, Age, xlim = c(0, 70), ylim = c(20, 85),
     xlab = "Indice di massa corporea (IMC)", ylab = "Età")
sm.density(Data[, c(2, 3)], hsj, display = "slice",
           props = c(75, 50, 25, 2), add = T)
title(main = "Stima densità ('Plug-in' h1=1.8 h2=1.04)")
dev.off()

png("sm_biv2.png")
plot(BMI, Age, xlim = c(0, 70), ylim = c(20, 85),
     xlab = "Indice di massa corporea (IMC)", ylab = "Età")
sm.density(Data[, c(2, 3)], hsj, display = "image", xlim = c(0, 70), ylim = c(20, 85),
           xlab = "Indice di massa corporea (IMC)", ylab = "Età")
title(main = "Stima densità ('Plug-in' h1=1.8 h2=1.04)")
dev.off()

#Analisi inferenziale

#kolmogorov

par(mfrow=c(1, 1))

png("ecdf_kolm.png")
plot(ecdf(BMI), do.points = F, verticals = T,
     xlab = "Indice di massa corporea (IMC)", ylab = "Probabilità",
     main = "Funzione di ripartizione")
rug(BMI)
curve(pnorm(x, mean(BMI), sd(BMI)), xlab = "x", ylab = "Probabilità", type = "s", col = "red",add = T, 
      main = "Theorical distribution function")
legend(55, 0.3, c("Empirica", "Teorica"), lty = c(1, 1), col = c("black", "red"))
dev.off()

sd(BMI)
ks.test(BMI, "pnorm", 32.46, 6.92)







#Test T a due campioni

BMI_NO <- split(BMI, Diabetes)[["No"]]
BMI_SI <- split(BMI, Diabetes)[["Si"]]


library(sm)
par(mfrow=c(1, 1))

png("dens_no.png")
sm.density(BMI_NO, hsj(BMI_NO),
            yht = 0.12, xlim = c(0, 70),
            xlab = "IMC donne senza diabete")
title(main = "Stima densità ('Plug-in' h = 1.86)")
dev.off()

png("dens_si.png")
sm.density(BMI_SI, hsj(BMI_SI),
             yht = 0.12, xlim = c(0, 70),
             xlab = "IMC donne con diabete")
title(main = "Stima densità ('Plug-in' h = 1.75)")
dev.off()

var.test(BMI_NO, BMI_SI, paired = F, alternative = "two.sided")
t.test(BMI_NO, BMI_SI, var.equal = T, alternative = "greater")


#test di mann wthitney

wilcox.test(BMI ~ Diabetes, alternative = "two.sided")

#Test di Kolmogorov

par(mfrow=c(1,1))
png("ecdf_kol-smi.png")
plot(ecdf(BMI_NO), do.points = F, verticals = T, xlim = c(0, 70),
     lty = 1, xlab = "Indice di massa corporea (IMC)", ylab = "Probabilità",
     main = "Funzione di ripartizione empirica")
plot(ecdf(BMI_SI), do.points = F, verticals = T, lty = 3, add = T, col = "red")
legend(50, 0.3, c("Diabete No", "Diabete Si"), lty = c(1, 3),col = c("black", "red"))
dev.off()

ks.test(BMI_NO, BMI_SI)

#analisi della varianza

Data_Diab_Si<-subset(Data, Diabetes == "Si")
Data_Diab_No<-subset(Data, Diabetes == "No")
attach(Data_Diab_No)
attach(Data_Diab_Si)

Diab_Si_1 <- split(Data_Diab_Si$BMI, Data_Diab_Si$Age_Class)[[1]]
Diab_Si_2 <- split(Data_Diab_Si$BMI, Data_Diab_Si$Age_Class)[[2]]
Diab_Si_3 <- split(Data_Diab_Si$BMI, Data_Diab_Si$Age_Class)[[3]]
Diab_Si_4 <- split(Data_Diab_Si$BMI, Data_Diab_Si$Age_Class)[[4]]
Diab_Si_5 <- split(Data_Diab_Si$BMI, Data_Diab_Si$Age_Class)[[5]]



library(sm)
par(mfrow = c(3, 2))
sm.density(Diab_Si_1, hnorm(Diab_Si_1), yht = 0.08, xlim = c(0, 70), ylab = "Probabilità",
             xlab = "IMC donne 21-25 anni")
title(main = "Stima densità")
sm.density(Diab_Si_2, hnorm(Diab_Si_2), yht = 0.08, xlim = c(0, 70), ylab = "Probabilità",
             xlab = "IMC donne 26-31 anni")
title(main = "Stima densità")
sm.density(Diab_Si_3, hnorm(Diab_Si_3), yht = 0.08, xlim = c(0, 70), ylab = "Probabilità",
             xlab = "IMC donne 32-41 anni")
title(main = "Stima densità")
sm.density(Diab_Si_4, hnorm(Diab_Si_4), yht = 0.08, xlim = c(0, 70), ylab = "Probabilità",
             xlab = "IMC donne 42-50 anni")
title(main = "Stima densità")
sm.density(Diab_Si_5, hnorm(Diab_Si_5), yht = 0.08, xlim = c(0, 70), ylab = "Probabilità",
             xlab = "IMC donne 50+ anni")
title(main = "Stima densità")
par(mfrow = c(1, 1))

boxplot(Data_Diab_Si$BMI ~ Data_Diab_Si$Age_Class ,boxwex = 0.2, xlab = "Classi d'età", ylab = "IMC donne diabete Si",
        names=c("21-25", "26-31", "32-41", "42-50", "Over 50"),
        main = "Box-and-whiskers plot")

summary(aov(Data_Diab_Si$BMI ~ Data_Diab_Si$Age_Class))
TukeyHSD(aov(Data_Diab_Si$BMI ~ Data_Diab_Si$Age_Class), "Data_Diab_Si$Age_Class")


boxplot(Data_Diab_No$BMI ~ Data_Diab_No$Age_Class ,boxwex = 0.2, xlab = "Classi d'età", ylab = "IMC donne diabete No",
        names=c("21-25", "26-31", "32-41", "42-50", "Over 50"),
        main = "Box-and-whiskers plot")

summary(aov(Data_Diab_No$BMI ~ Data_Diab_No$Age_Class))
TukeyHSD(aov(Data_Diab_No$BMI ~ Data_Diab_No$Age_Class), "Data_Diab_No$Age_Class")

#Test di Kruskal Wallis

kruskal.test(Data_Diab_Si$BMI ~ Data_Diab_Si$Age_Class)
kruskal.test(Data_Diab_No$BMI ~ Data_Diab_No$Age_Class)



#regressione logistica1

Data$Diabetes<-factor(Data$Diabetes, c(0,1), c("No", "Si"))
Data$Age_Class<-factor(Data$Age_Class, c(1,2,3,4,5), c("21-25", "26-31", "32-41", "42-50", "51+"))

table(Data$Age_Class, Data$Diabetes)

names(Data)[1]<-"Diabete"
names(Data)[2]<-"IMC"
names(Data)[3]<-"Età"
names(Data)[4]<-"Classi_età"

glm.fit<-glm(Diabete ~ IMC + Classi_età,data=Data, family=binomial)
summary(glm.fit)

glm.probs<-predict(glm.fit,type="response")
probabib<-round(glm.probs,digits=4)
dffit<-cbind(Data,"Pr. Diabete"=probabib)
head(dffit)

glm.pred=rep("No",nrow(Data))
glm.pred[glm.probs>0.5]="Si"
table(glm.pred,Data$Diabete)


corretti<-(425+119)
pc_corretti<-corretti/nrow(Data)
senza_diabete<-(425+75)
pc_senzdiab_corr<-425/senza_diabete
con_diabete<-(149+119)
pc_condiab_corr<-119/con_diabete


#2


glm.fit2<-glm(Diabete ~ IMC + Data$Età ,data=Data, family=binomial)
summary(glm.fit2)

glm.probs2<-predict(glm.fit2,type="response")
probabib2<-round(glm.probs2,digits=4)
dffit2<-cbind(Data,"Pr. Diabete"=probabib2)
head(dffit2)

glm.pred2=rep("No",nrow(Data))
glm.pred2[glm.probs2>0.5]="Si"
table(glm.pred2,Data$Diabete)

corretti2<-(434+93)
pc_corretti2<-corretti2/nrow(Data)
senza_diabete2<-(434+66)
pc_senzdiab_corr2<-434/senza_diabete2
con_diabete2<-(175+93)
pc_condiab_corr2<-93/con_diabete2

#3 classe età nuova

Data$Age_Class1<-factor(Data$Age_Class1, c(1,2,3,4,5), c("21-24", "25-29", "30-40", "41-50", "51+"))

table(Data$Age_Class, Data$Diabetes)

names(Data)[1]<-"Diabete"
names(Data)[2]<-"IMC"
names(Data)[3]<-"Età"
names(Data)[5]<-"Classi"
names(Data)[6]<-"Classi_età"

glm.fit<-glm(Diabete ~ IMC,data=Data, family=binomial)
summary(glm.fit)

glm.probs<-predict(glm.fit,type="response")
probabib<-round(glm.probs,digits=4)
dffit<-cbind(Data,"Pr. Diabete"=probabib)
head(dffit)

glm.pred=rep("No",nrow(Data))
glm.pred[glm.probs>0.5]="Si"
table(glm.pred,Data$Diabete)

corretti<-(425+119)
pc_corretti<-corretti/nrow(Data)
senza_diabete<-(425+75)
pc_senzdiab_corr<-425/senza_diabete
con_diabete<-(149+119)
pc_condiab_corr<-119/con_diabete