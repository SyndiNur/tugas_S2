#Mengakses dataset ke dalam global environment
#Mengaktifkan package data.table
library(data.table)

#Mengimport data stroke dan menjadikannya objek baru
stroke <- fread("http://www.statsci.org/data/oz/stroke.txt")

#Mengaktifkan package dplyr
library(dplyr)

#Memilih kolom yang akan dipergunakan
stroke <- stroke %>% select(c(1:6,39:46))

#1. Menganalisis perbedaan functional ability minggu 1 (Bart1) berdasar grup 
#intervensi (Group) dengan boxplot beserta interpretasi
#1.1. Membuat boxplot antara Bart1 dan Group
boxplot(stroke$Bart1~stroke$Group)

#1.2. Mengubah data wide menjadi long
boxplot(stroke$Bart1~stroke$Group, xlab = "Group", ylab = "Functional Ability", 
        col = c("purple", "yellow", "red"), main = "Functional Ability Week 1 based on Group")

#2. Melakukan uji normalitas pada data Bart1
shapiro.test(stroke$Bart1)

#3. Menghitung perubahan functional ability dari Bart1 hingga Bart8 dan membuat menjadi variabel baru
stroke$Bart_diff <- stroke$Bart8-stroke$Bart1

#4. Melakukan uji normalitas pada data Bart_diff
shapiro.test(stroke$Bart_diff)

#5. Melakukan uji kesamaan variance dari Bart_diff antara grup intervensi
bartlett.test(stroke$Bart_diff,stroke$Group)

#6. Membuat plot mean dan 95% confidence interval dari Bart_diff berdasarkan grup
#6.1. Mengaktifkan package gplots
library(gplots)

#6.2. Membuat plot mean dan 95% confidence interval dari Bart_diff berdasarkan grup
plotmeans(Bart_diff~Group, data = stroke, frame = FALSE, mean.labels = TRUE, 
          connect = FALSE, xlab = "Group", ylab = "Functional Ability Difference", 
          main = "Functional Ability Difference based on Group")

#7. Melakukan uji ANOVA untuk membandingkan mean Bart_diff antara 3 Group
#7.1. Mengaktifkan package lmerTest
library(lmerTest)

#7.2. Melakukan test one-way ANOVA
res.aov <- aov(Bart_diff ~ Group, data = stroke)

#7.3. Melihat summary dari res.aov
summary(res.aov)

#8. Analisis model linear regresi antara Functional Ability dan explanatory 
#variables (Waktu, grup, dan interaksi waktu serta grup)
#8.1. Mengaktifkan package tidyr
library(tidyr)

#8.2. Mengubah stroke (dalam bentuk wide) menjadi bentuk long
stroke_long = stroke %>% pivot_longer(cols=Bart1:Bart8, names_to = "Time", 
                                      names_prefix = "Bart", values_to = "Ability")

#8.3. Melakukan analisis model linear regresi
model_1 <- lm(Ability~as.numeric(Time) + as.factor(Group) + 
                as.numeric(Time)*as.factor(Group),data=stroke_long)

#8.4. Melihat summary model linear regresi
summary(model_1)

#9.Analisis model linear regresi antara Functional Ability dan explanatory 
#variables (Waktu dan grup)
#9.1. Melakukan analisis model linear regresi
model_2 <- lm(Ability~as.numeric(Time) + as.factor(Group),data=stroke_long)

#9.2. Melihat summary model linear regresi
summary(model_2)

#10. AIC model #8 dan #9
#10.1. Mengaktifkan package broom
library(broom)

#10.2. Melihat AIC model 1
glance(model_1)

#10.3. Melihat AIC model 2
glance(model_2)

#10.4. Interpretasi
#AIC model_1 lebih tinggi dibandingkan model_2 sehingga model_2 lebih baik.

#11. Interpretasi dari salah satu model
#Berdasarkan AIC, model_2 lebih baik. Pada model_2 didapatkan estimate (???1) dari 
#Time (x1) adalah 4,76. Oleh karena itu dapat dikatakan bahwa setiap penambahan 
#Time sebesar 1 kali (setiap pemberian intervensi yang lebih banyak selama 1 minggu) 
#akan menyebabkan peningkatan Functional Ability Score (Barthel Index Score) 
#sebesar 4,76 poin.

#12. Analisis mixed model (random intercept) dengan package nlme antara Bartlet 
#dan explanatory (waktu, grup, dan random intercept)
#12.1. Mengaktifkan package nlme
library(nlme)

#12.2. Melakukan analisis mixed model
rndeff<-lme(Ability~as.factor(Group) + as.numeric(Time), data=stroke_long,
            random=~1|Subject)

#12.3. Melihat summary rndeff
summary(rndeff)

#13. Analisis Bartlet dan explanatory (waktu, grup) dengan GEE dengan correlation structure:
#13.1. Mengaktifkan package geepack
library(geepack)

#13.1. Exchangeable
gee.exch <- geeglm(Ability~as.factor(Group)+as.numeric(Time),family = gaussian, 
                   data=stroke_long,id=as.factor(Subject),wave=as.numeric(Time),
                   corst="exchangeable")

#13.2. Melihat hasil analisis dengan correlation structure Exchangeable
summary(gee.exch)

#13.3. Autoregressive
gee.ar1 <- geeglm(Ability~as.numeric(Time)+as.factor(Group),family = gaussian, 
                  data=stroke_long,id=as.factor(Subject),wave=as.numeric(Time),
                  corst="ar1")

#13.4. Melihat hasil analisis dengan correlation structure Autoregressive
summary(gee.ar1)

#13.5. Unstructured
gee.un <- geeglm(Ability~as.numeric(Time)+as.factor(Group),family = gaussian, 
                 data=stroke_long,id=as.factor(Subject),wave=as.numeric(Time),
                 corst="unstructured")

#13.4. Melihat hasil analisis dengan correlation structure Unstructured
summary(gee.un)

#14. Menghitung AIC dari model GLS dengan ketiga struktur korelasi
#14.1. Exchangeable
exch<-corCompSymm(form = ~ 1 | Subject)
gls.exch<-gls(Ability~as.factor(Group)+as.numeric(Time), data=stroke_long,
              correlation=exch)

#14.2. Melihat AIC model GLS dengan struktur korelasi exchangeable
summary(gls.exch)

#14.3. Autoregressive
ar1<-corAR1(form = ~ 1 | Subject)
gls.ar1<-gls(Ability~as.factor(Group)+as.numeric(Time), data=stroke_long, 
             correlation=ar1)

#14.4. Melihat AIC model GLS dengan struktur korelasi autoregressive
summary(gls.ar1)

#14.5. Unstructured
un<-corSymm(form = ~ 1 | Subject)
gls.un<-gls(Ability~as.factor(Group)+as.numeric(Time), data=stroke_long, 
            correlation=un)

#14.6. Melihat AIC model GLS dengan struktur korelasi unstructured
summary(gls.un)

#15. Tabel membandingkan AIC model korelasi struktur dan model AIC linear regresi model
aic = AIC(model_2,gls.exch,gls.ar1,gls.un)
