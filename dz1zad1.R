#brisanje svih podataka iz okruženja
rm(list = ls())

#provjera i priprema radnog direktorija
getwd()
setwd("~/Desktop/UznL")

#ucitavanje csv datoteke kao podatkovni okvir
dz1data <- read.csv("lab3.csv", header = TRUE, sep = ",")

#provjera jesu li se podatci pravilno ucitali
View(dz1data)

#pronalazak svih celija u okviru s vrijednoscu -1 te zamijena s kodiranjem pod NA
dz1data[dz1data==-1] <- NA

#zapis uredenog podatkovnog okvira u novu csv datoteku
write.csv(dz1data, "~/Desktop/UznL/lab3_izlaz.csv", sep=",")