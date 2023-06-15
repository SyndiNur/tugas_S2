#1. Mengakses dataset ke dalam global environment
#1.1. Mengaktifkan package data.table
library(data.table)

#1.2. Mengimport data stroke dan menjadikannya objek baru
stroke <- fread("http://www.statsci.org/data/oz/stroke.txt")

#1.3. Melihat tabel stroke2
View(stroke)

#2. Membuat visualisasi grafik garis dari bart terhadap waktu
#2.1. Mengaktifkan package tidyr
library(tidyr)

#2.2. Melihat dataset stroke
names(stroke)

#2.3. Mengubah data wide menjadi long
stroke_long = stroke %>% select(c(1:6,39:46)) %>% 
  pivot_longer(cols=Bart1:Bart8, names_to = "Time", names_prefix = "Bart", 
               values_to = "Ability")

#2.4. Mengaktifkan package ggplot2
library(ggplot2)

#2.5. Membuat grafik per individu untuk data longitudinal 
ggplot(data = stroke_long, aes(x = Time, y = Ability, group = Subject, color = Subject)) + 
  geom_line()

#2.6. Menambahkan label
ggplot(data = stroke_long, aes(x = Time, y = Ability, group = Subject, color = Subject)) + 
  geom_line() + labs(list, title = "Recovery from stroke", y = "Functional ability score", 
                     x = "Week")

#3. membuat grafik per grup
#3.1. mengaktifkan package tidyverse
library(tidyverse)

#3.2. membuat mean dari grup berdasarkan time
stroke_mean <- stroke_long %>% group_by (Group, Time) %>% summarise(mean_val = mean(Ability))

#3.3. membuat grafik per grup
ggplot(data = stroke_mean, aes(x = Time, y = mean_val, group = Group, color = Group)) + 
  geom_line()

#3.4. menambahkan label dan judul pada grafik
ggplot(data = stroke_mean, aes(x = Time, y = mean_val, group = Group, color = Group)) + 
  geom_line() + labs(title = "Average stroke recovery score based on groups", 
                     x = "Week", y = "Functional ability score")

#4. Membuat scatterplot matrix
#4.1.Membuat objek baru, mutasi integer menjadi numerik, melakukan renaming
stroke_wide = stroke %>% select(c(39:46)) %>% 
  mutate_at(c("Bart1", "Bart2", "Bart3", "Bart4", "Bart5", "Bart6", "Bart7", "Bart8"), 
            as.numeric) %>% rename(Week1 = Bart1, Week2 = Bart2, Week3 = Bart3, 
                                   Week4 = Bart4, Week5 = Bart5, Week6 = Bart6, 
                                   Week7 = Bart7, Week8 = Bart8)

#4.2.membuat scatterplot matrix dari objek stroke
pairs(stroke_wide[,Week1:Week8],pch = 19)

#5.membuat analisis korelasi
#5.1.membuat correlation matrix dari objek stroke_wide
res <- cor(stroke_wide)

#5.3.melihat hasil correlation matrix
view(res)

#6.membuat interpretasi scatterplot dan correlation matrix
#Scatterplot matrix dan correlation matrix menampilkan hubungan antara beberapa 
#variabel. Pada scatterplot matrix kotak kedua baris pertama menunjukkan hubungan 
#antara Week1 (x) dan Week2 (y) dimana diduga terdapat hubungan karena plot 
#membentuk garis lurus. Hubungan tersebut dikonfirmasi dengan hasil perhitungan
#koefisien korelasi pada correlation matrix kotak paling kiri kedua dari atas 
#dimana didapatkan koefisien korelasi 0,93. Hasil ini berbeda dengan kotak pojok
#kanan atas pada scatterplot matrix yang menunjukkan hubungan antara Week1 (x) 
#dan Week8 (y) dimana plot tidak membentuk garis lurus. Hasil correlation matrix
#menunjukkan bahwa didapatkan koefisien korelasi sebesar 0,55 (kotak pojok kiri 
#bawah), lebih kecil dibandingkan koefisien korelasi antara Week1 dan Week2

#7.membuat intercept dan slopes dari masing-masing subject
#7.1.mengaktifkan package lme4
library(lme4)

#7.2.mengubah variabel Time menjadi numerik (Time1)
stroke_long$Time1 <- as.numeric(stroke_long$Time)

#7.3. membuat intercept, slopes, dan standard error untuk setiap subject
fm1 <- lmList(Ability ~ Time1 | Subject, data = stroke_long)
summary(fm1)$coef
