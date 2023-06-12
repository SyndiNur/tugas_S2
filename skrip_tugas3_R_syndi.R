#objek yang dipergunakan adalah w5_ahsp yang telah dibuat pada tugas sebelumnya

#1. Mengidentifikasi outlier berdasarkan kriteria dan visualisasi grafik boxplot
#1.1.Membuat boxplot dengan mempergunakan variabel age dan height
boxplot(w5_ahsp$pef)

#1.2. Menambahkan label dan judul pada boxplot
boxplot(w5_ahsp$pef,xlab = "Peak Expiratory Flow", ylab = "Frekuensi", col = c("purple"), 
        main = "Grafik boxplot berdasar PEF")

#2. Menentukan cut-off outlier (batas atas dan bawah) dari grafik boxplot tersebut
#2.1.Menentukan cut-off outlier dengan menggunakan mean dan standar deviasi
#2.1.1. Menentukan mean
mean = mean(w5_ahsp$pef)
#2.1.2. Menentukan standar deviasi
std = sd(w5_ahsp$pef)
#2.1.3. Menentukan batas bawah
Tmin1 = mean-(3*std)
#2.1.4. Menentukan batas atas
Tmax1 = mean+(3*std)

#2.2. Menentukan cut-off outlier dengan menggunakan IQR
#2.2.1. Menentukan Q1 dan Q3
Q1 <- quantile(w5_ahsp$pef, .25)
Q3 <- quantile(w5_ahsp$pef, .75)
#2.2.2. Menentukan IQR
IQR <- IQR(w5_ahsp$pef)
#2.2.3. Menentukan batas bawah
Lower <- Q1 - 1.5*IQR
#2.2.4. Menentukan batas atas
Upper <- Q3 + 1.5*IQR

#3. Membuat dataset yang tidak berisi outlier sesuai cut-off no 2
#3.1. Membuat dataset baru berdasarkan cut-off IQR
pef_no2 <- subset(w5_ahsp, w5_ahsp$pef > Lower & w5_ahsp$pef < Upper)

#3.2. Melakukan pengecekan pada boxplot
boxplot(pef_no2$pef,xlab = "Peak Expiratory Flow", ylab = "Frekuensi", col = c("purple"), 
        main = "Grafik boxplot berdasar PEF")

#4. Melakukan tes normalitas pada dataset dengan outlier dan tanpa outlier
#4.1. Mengaktifkan package nortest
library(nortest)

#4.2.Tes normalitas pada dataset dengan outlier. Uji normalitas dilakukan dengan liliefors
lillie.test(w5_ahsp$pef)

#4.2. Tes normalitas pada dataset tanpa outlier
lillie.test(pef_no2$pef)

#5. Membuat grafik QQ line untuk membandingkan visualisasi nilai pef pada dataset
#dengan outlier dan tanpa outlier
#5.1. Membuat grafik QQ line pada dataset dengan outlier
qqnorm(w5_ahsp$pef); qqline(w5_ahsp$pef)

#5.2. Membuat grafik QQ line pada dataset tanpa outlier
qqnorm(pef_no2$pef); qqline(pef_no2$pef)

#6. Membuat scatterplot yang memperlihatkan hubungan antara pef dengan height,
#dengan penambahan garis linear/regresi dan smoothed dengan loess (local 
#regression smoothing)
#6.1. Mengaktifkan package ggplot2
library(ggplot2)

#6.2. Pada data dengan outlier
#6.2.1.Membuat scatterplot pef dengan height
plot(w5_ahsp$height~w5_ahsp$pef, xlab = "Peak Expiratory Flow",
     ylab = "Tinggi badan (cm)", 
     main = "Sebaran PEF berdasarkan Tinggi Badan")
#6.2.2. Menambahkan garis linear
abline(lm(w5_ahsp$height~w5_ahsp$pef, data = w5_ahsp), col = "blue")

#6.2.3. Menambahkan loess
lines(lowess(w5_ahsp$pef, w5_ahsp$height), col = "red")

#6.3. Pada data tanpa outlier
#6.3.1.Membuat scatterplot pef dengan height
plot(pef_no2$height~pef_no2$pef, xlab = "Peak Expiratory Flow",
     ylab = "Tinggi badan (cm)", 
     main = "Sebaran PEF berdasarkan Tinggi Badan")
#6.3.2. Menambahkan garis linear
abline(lm(pef_no2$height~pef_no2$pef, data = pef_no2), col = "blue")
#6.3.3. Menambahkan loess
lines(lowess(pef_no2$pef, pef_no2$height), col = "red")

#7.Membuat scatterplot yang memperlihatkan hubungan antara pef dengan umur,
#dengan penambahan garis linear/regresi dan smoothed dengan loes (local 
#regression smoothing)
#note: ggplot2 sudah aktif
#7.1. Pada dataset dengan outlier
#7.1.1.Membuat scatterplot pef dengan age
plot(w5_ahsp$age~w5_ahsp$pef, xlab = "Peak Expiratory Flow",
     ylab = "Usia (tahun)", main = "Sebaran PEF berdasarkan Usia")

#7.1.2. Menambahkan garis linear
abline(lm(w5_ahsp$age~w5_ahsp$pef, data = w5_ahsp), col = "blue")

#7.1.3. Menambahkan loess
lines(lowess(w5_ahsp$pef, w5_ahsp$age), col = "red")

#7.2. Pada dataset tanpa outlier
#7.2.1.Membuat scatterplot pef dengan age
plot(pef_no2$age~pef_no2$pef, xlab = "Peak Expiratory Flow",
     ylab = "Usia (tahun)", main = "Sebaran PEF berdasarkan Usia")

#7.2.2. Menambahkan garis linear
abline(lm(pef_no2$age~pef_no2$pef, data = pef_no2), col = "blue")

#7.2.3. Menambahkan loess
lines(lowess(pef_no2$pef, pef_no2$age), col = "red")
