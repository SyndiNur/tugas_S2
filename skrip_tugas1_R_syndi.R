#membuat skrip baru untuk tugas
#1. mengaktifkan paket readr
library(readr)

#2. mengaktifkan paket dplyr
library(dplyr)

#3. install paket janitor
#4. aktifkan paket janitor
library(janitor)

#5. membaca data pef dan diganti nama menjadi ds_tugas
ds_tugas = read.csv("https://raw.githubusercontent.com/dwi-agustian/biostat/main/pef.csv")

#6. identifikasi duplikasi pada ds_tugas
get_dupes(ds_tugas, pidlink)

#7. menghapus duplikasi pada ds_tugas
ds_tugasnd = ds_tugas[!duplicated(ds_tugas$pidlink),]
