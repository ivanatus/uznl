#brisanje prethodnih varijabli u okruzenju
rm(list = ls())

#pregled koji je radni direktorij i priprema zeljenog radnog direktorija
getwd()
setwd("~/Desktop/UznL")

#postavljanje knjizice koja se koristi te varijable koja se koristi za crtanje histograma na vrijednost knjizice
library(stats)
dz1data <- sunspot.month

#crtanje histograma
h <- hist(dz1data, breaks = 20, xlab = "Broj opazanja pjega", ylab = "Frekvencija opazanja", main ="Histogram pojave suncanih pjega od 1749. godine s normalnom razdiobom")

#crtanje dijagrama odgovarajuce normalne razdiobe
xfit<-seq(min(dz1data),max(dz1data),length=40)
yfit<-dnorm(xfit,mean=mean(dz1data),sd=sd(dz1data))
yfit <- yfit*diff(h$mids[1:2])*length(dz1data)
lines(xfit, yfit, col="black", lwd=2)