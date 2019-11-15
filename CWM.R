##Cargamos los datos
library(readxl)
trait <- read.csv2(file = "Traits.csv", header = TRUE)
vegetacion <- read_excel("vegetacion.xlsx")
dta_par <- read_excel("DATOS POR PARCELA.xlsx",sheet=2)

#calcular el cwm por comunidad como:
#Suma(pi*trait)


#Evaluar si existen cambios en el CWM debidos a la perturbación.
#Se usa la cantidad de fecas y la distancia al pueblo como una medida
#de disturbio (dta_par)

library(FD)
rownames(trait)<- trait$X
trait <- trait[,-1]
rownames(vegetacion) <- vegetacion$X
vegetacion <- vegetacion[,-1]

xgow <- gowdis(trait[,c(1,2,10,11)])
str(xgow)

FD.t <- dbFD(xgow,vegetacion,calc.CWM = T, corr = "sqrt")
FD.t$FRic

plot(dta_par$Distancia, FD.t$FRic)
plot(dta_par$Distancia, FD.t$FDis)

modtc <- lm(FD.t$FDis~dta_par$Distancia)
summary(modtc)
abline(modtc)

long <- trait$lon_fr
names(long) <- rownames(trait)
FD.t <- dbFD(long,vegetacion,calc.CWM = T, corr = "sqrt")
