##############################################################
#######ANÀLISI ESTADÍSTIC DE LA MESURA DELS HIPOCOTILS########
##############################################################
##########################Ruth Domínguez i Hanan El Hajjioui##
##############################################################


#Carreguem les librerias necessàries
library(readxl)
library(tidyverse) 
library(ggplot2)

#Carreguem l'excel amb les dades recollides  
datos <- as.data.frame(read_excel("C:/Users/rdr21/Desktop/datos/AllData.xlsx"))

#Creem els vectors amb els gens i les condicions  
condicions = c("W", "WFR")
gens = c("col","hfr1","phyB","pif7")

#Taula amb el número de dades de cada subgrup:
cantidades <- table(datos$condicions, datos$gen)


#Creem el gràfic de caixes
Boxplot <- ggplot(data = datos, mapping = aes(x = gen, y= mida)) +
  geom_boxplot(aes(fill=condicions)) + 
  labs(title="Distribució de la llargada dels hipocotils", x="Gen", y="Llargada", colour="")+ 
  stat_summary(fun.y = mean, geom="point", aes(colour="Mitjana", group=condicions), position=position_dodge(0.8))+ 
  theme_bw() + scale_fill_manual(values=c("#76D7C4", "#85C1E9"))+
  stat_summary(aes(label=round(..y..,2), group=condicions), fun.y=mean, geom="text", size=3,
vjust = -1.1, hjust = 0.6)

Boxplot


#Creem una taula que recull les mitjanes
mitjanes <- data.frame(Condicio=character(),
                       Gen=character(), 
                       Mitjana=integer(), 
                       stringsAsFactors=FALSE) 

for (condicio in condicions){
  for (gen in gens){
    mides1 <- c(datos$mida[which(datos$condicions==condicio & datos$gen==gen)])
    mitjana <- mean(mides1)
    fila <- t(as.matrix(condicio, gen, mitjana))
    mitjanes[nrow(mitjanes) + 1,] = c(condicio, gen,mitjana)
  }
}

#Creem un data frame que contingui els resultats dels pvalors dels test estadístics emprats
Resultats <- data.frame(Condicio=character(),
                       Gen=character(), 
                       Test_Shapiro=integer(),
                       Test_Variancia= integer(),
                       Test_TStudent=integer(),
                       stringsAsFactors=FALSE)


for (condi in condicions){
  mides1 <- c(datos$mida[which(datos$condicions==condi & datos$gen=="col")])
  media <- mean(mides1)
  
  for (mutacion in gens[2:4]){
    mides2 <- c(datos$mida[which(datos$condicions==condi & datos$gen==mutacion)])
    media <- mean(mides2)
    
    shapiro <- shapiro.test(mides2)
    shapiro <- shapiro[["p.value"]]
    
    varianza <- var.test(mides1, mides2)
    varianza <- varianza[["p.value"]]
    
    if(varianza>=0.05){
      bool= T
    } else{
      bool=F
    }
    
    ttest <- t.test(mides1, mides2, var.equal = bool)
    ttest <- ttest[["p.value"]]
    
    #Guardamos los resultados
    Resultats[nrow(Resultats) + 1,] = c(condi, mutacion, shapiro, varianza,ttest)
    
    
  }
}
