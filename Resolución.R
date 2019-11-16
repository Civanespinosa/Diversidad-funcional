library(readxl)

##Leemos los datos 

par <-  read_excel("parcelas.xlsx")
vege <- read_excel("vegetacion.xlsx")
trait <- read_excel("trait.xlsx")

#damos la vuelta a la vegetación

vegeT <- data.frame(Especies = colnames(vege[,-1]) ,
                    t(vege[,-1]))

colnames(vegeT)[-1] <- vege$COD.PAR


cwm <- function(x, trait, sppV, sppT, Ntrait){
  #x= matriz de datos de abundancia
  #trait = matriz de datos de caracteres por especie
  #sppV = el nombre de la columna especies en vegetación
  #sppT = el nombre de la columna especies en trait
  #Ntrait = el nombre del trait a analizar
  
  #Convertimos en abundancia relativa por sitio
  
  xAR <- data.frame(Especies=x[,sppV],x[,colnames(x)!=sppV]/
                      colSums(x[,colnames(x)!=sppV]))
  colnames(trait)[which(colnames(trait)==sppT)] <- "Especies"
  
  xT <- merge(xAR, trait[, c("Especies",Ntrait)], by="Especies")
  
  #Calculamos el cwm
  cwm <- colSums(xT[,2:(ncol(xT)-1)]*xT[,ncol(xT)])
  return(cwm)
}

##Calculamos CWM usando la función

CWM.Alt <- cwm(x=vegeT, trait = trait, sppV = "Especies", 
                sppT = "especies", Ntrait = "Altura")

cwmTP <- data.frame(COD.PAR=names(CWM.Alt), cwmAlt=CWM.Alt)

parF <- par[,c(3,12,23)]
parF <- aggregate(parF[,2:3], by=list("COD.PAR"=parF$COD.PAR),
                  min)

cwmTP <- merge(cwmTP, parF, by="COD.PAR")

modAl <- lm(cwmAlt~Caprino, data=cwmTP)
summary(modAl)


plot(cwmTP$cwmAlt~cwmTP$Caprino, pch=19, col=rgb(0,0,0,0.5))
abline(modAl, col="red", lwd=1.5)

##Densidad ----

CWM.Dens <- cwm(x=vegeT, trait = trait, sppV = "Especies", 
               sppT = "especies", Ntrait = "Densidad.madera")

cwmTPD <- data.frame(COD.PAR=names(CWM.Dens), cwmDen=CWM.Dens)

cwmTPD <- merge(cwmTPD, parF, by="COD.PAR")

modDen <- lm(cwmDen~Caprino, data=cwmTPD)
summary(modDen)

plot(cwmTPD$cwmDen~cwmTPD$Caprino, pch=19, col=rgb(0,0,0,0.5))
abline(modDen, col="red", lwd=1.5)

modDenD <- lm(cwmDen~DISTANCIA, data=cwmTPD)
summary(modDenD)

plot(cwmTPD$cwmDen~cwmTPD$DISTANCIA, pch=19, col=rgb(0,0,0,0.5))
abline(modDenD, col="red", lwd=1.5)

##SLA ---