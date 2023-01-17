rm(list = ls())
setwd("~/Desktop/UZnL")

#učitavanje podataka u programski okvir
dz4_data <- read.csv("rec.csv", header = TRUE)

#prikaz prave razdiobe podataka u varijablama nerr i eerr
plot(dz4_data$nerr, xlab = 'x', ylab ='y', main = 'Razdioba podataka u varijabli nerr')
plot(dz4_data$eerr, xlab = 'x', ylab ='y', main = 'Razdioba podataka u varijabli eerr')

#prikaz kvantila pomoću kutijastog dijagrama
boxplot(dz4_data$nerr, main='Kutijasti dijagram za nerr', xlab='x', ylab='y')
boxplot(dz4_data$eerr, main='Kutijasti dijagram za eer', xlab='x', ylab='y')

#histogram razdiobe podataka
hist(dz4_data$nerr, xlab = 'x', ylab = 'y', main = 'Histogram za nerr')
hist(dz4_data$eerr, xlab = 'x', ylab = 'y', main = 'Histogram za eer')

#prikaz eksperimentalne funkcije gustoće vjerojatnosti
plot(density(dz4_data$nerr), main = '', xlab = 'x', ylab = 'y')
plot(density(dz4_data$eerr), main = '', xlab = 'x', ylab = 'y')

#standardna devijacija
print(sd(dz4_data$nerr))
print(sd(dz4_data$eerr))

#eksplorativna statistička analiza
print(summary(dz4_data$nerr))
print(summary(dz4_data$eerr))

#Shapiro-Wilk test
print(shapiro.test(dz4_data$nerr))
print(shapiro.test(dz4_data$eerr))

#Kolmogorov-Smirnov test
print(ks.test(dz4_data$nerr, 'pnorm'))
print(ks.test(dz4_data$eerr, 'pnorm'))

#t test
print(t.test(dz4_data$nerr, dz4_data$eerr))

#var test
print(var.test(dz4_data$nerr, dz4_data$eerr))