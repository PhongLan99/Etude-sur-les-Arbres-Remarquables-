library(FactoMineR)
library(ggplot2)
library(factoextra)
library(Factoshiny)
library(questionr)
library(dplyr)

rm(list=ls())
D<-read.csv('D:\\Cours\\Stage\\The Quantic Factory\\Test technique\\Arbres_Remarquables_Modif3.csv',header=TRUE,sep=";")
D$DATEPLANTATION<-as.Date(D$DATEPLANTATION)

#Sélection des variables qui nous semblent pertinentes pour l'analyse
Dselect<-subset(D, select=c(CIRCONFERENCE.EN.CM,HAUTEUR.EN.M,STADEDEVELOPPEMENT,ESPECE,GENRE,DOMANIALITE,ARRONDISSEMENT,ADRESSE))

#Création de la variable mesurant la qualité de l'air LE 19/11/2021 à 10H d'après le site
#https://www.paris.fr/pages/etat-des-lieux-de-la-qualite-de-l-air-a-paris-7101

QUALITE.AIR<-rep("NA",178) #Création nouvelle variable pour mesurer la qualité de l'air

QUALITE.AIR[Dselect$ARRONDISSEMENT=="BOIS DE VINCENNES"]="Mauvaise"
QUALITE.AIR[Dselect$ARRONDISSEMENT=="BOIS DE BOULOGNE"]="Dégradée"
QUALITE.AIR[Dselect$ARRONDISSEMENT=="BOIS DE BOULOGNE"]="Dégradée"
QUALITE.AIR[Dselect$ARRONDISSEMENT=="PARIS 1ER ARRDT"]="Mauvaise"
QUALITE.AIR[Dselect$ARRONDISSEMENT=="PARIS 3E ARRDT"]="Mauvaise"
QUALITE.AIR[Dselect$ARRONDISSEMENT=="PARIS 4E ARRDT"]="Mauvaise"
QUALITE.AIR[Dselect$ARRONDISSEMENT=="PARIS 5E ARRDT"]="Dégradée"
QUALITE.AIR[Dselect$ARRONDISSEMENT=="PARIS 6E ARRDT"]="Dégradée"
QUALITE.AIR[Dselect$ARRONDISSEMENT=="PARIS 7E ARRDT"]="Dégradée"
QUALITE.AIR[Dselect$ARRONDISSEMENT=="PARIS 8E ARRDT"]="Mauvaise"
QUALITE.AIR[Dselect$ARRONDISSEMENT=="PARIS 9E ARRDT"]="Mauvaise"
QUALITE.AIR[Dselect$ARRONDISSEMENT=="PARIS 10E ARRDT"]="Mauvaise"
QUALITE.AIR[Dselect$ARRONDISSEMENT=="PARIS 11E ARRDT"]="Mauvaise"
QUALITE.AIR[Dselect$ARRONDISSEMENT=="PARIS 12E ARRDT"]="Mauvaise"
QUALITE.AIR[Dselect$ARRONDISSEMENT=="PARIS 13E ARRDT"]="Mauvaise"
QUALITE.AIR[Dselect$ARRONDISSEMENT=="PARIS 14E ARRDT"]="Dégradée"
QUALITE.AIR[Dselect$ARRONDISSEMENT=="PARIS 15E ARRDT"]="Dégradée"
QUALITE.AIR[Dselect$ARRONDISSEMENT=="PARIS 16E ARRDT"]="Dégradée"
QUALITE.AIR[Dselect$ARRONDISSEMENT=="PARIS 17E ARRDT"]="Mauvaise"
QUALITE.AIR[Dselect$ARRONDISSEMENT=="PARIS 18E ARRDT"]="Mauvaise"
QUALITE.AIR[Dselect$ARRONDISSEMENT=="PARIS 19E ARRDT"]="Mauvaise"
QUALITE.AIR[Dselect$ARRONDISSEMENT=="PARIS 20E ARRDT"]="Mauvaise"

QUALITE.AIR

Dselect$QUALITE.AIR<-as.factor(QUALITE.AIR)

#Création de la variable longévité en années
LONGEVITE.EN.ANNEES<-rep("NA",178) #Création nouvelle variable pour mesurer la longévité
dt<-Sys.Date()

n<-178
for(i in 1:n)
{
  LONGEVITE.EN.ANNEES[i]<-floor(difftime(dt, D$DATEPLANTATION[i], units = "weeks")/52.25)
}
LONGEVITE.EN.ANNEES

Dselect$LONGEVITE.EN.ANNEES<-as.numeric(LONGEVITE.EN.ANNEES)

#Création des caractéristiques de la longévité
CARACT.LONGEVITE<-rep("NA",178)

CARACT.LONGEVITE[Dselect$LONGEVITE.EN.ANNEES<"100"]="Moins de cent ans"
CARACT.LONGEVITE[(Dselect$LONGEVITE.EN.ANNEES>="100")&(Dselect$LONGEVITE.EN.ANNEES<"200")]="Entre 100 et 200 ans"
CARACT.LONGEVITE[(Dselect$LONGEVITE.EN.ANNEES>="200")&(Dselect$LONGEVITE.EN.ANNEES<"300")]="Entre 200 et 300 ans"
CARACT.LONGEVITE[(Dselect$LONGEVITE.EN.ANNEES>="300")&(Dselect$LONGEVITE.EN.ANNEES<"400")]="Entre 300 et 400 ans"
CARACT.LONGEVITE[Dselect$LONGEVITE.EN.ANNEES>="400"]="Plus de 400 ans"
CARACT.LONGEVITE

Dselect$CARACT.LONGEVITE<-as.factor(CARACT.LONGEVITE)

#Quel est l'impact des arbres remarquables sur la qualité de l'air?

#I.Analyse factorielle de données mixtes
Dfamd1<-Dselect[,c(1,2,9)]

res1.FAMD<-FAMD(Dfamd1,graph=T)

plot.FAMD(res1.FAMD,habillage=3,title="Graphe des individus et des modalités",cex=0.7,cex.main=0.7,cex.axis=0.7)

#Valeurs propres 
#Le critère des valeurs propres (valeur propre>1) impose de garder au moins 2 composantes.
#De plus les 2 composantes expliquent 92% de l'inertie totale, cad la quasi-totalité totale de l'inertie totale
#On garde donc les 2ères composantes.
(Dfamd1.eig<-round(res1.FAMD$eig,4))

#Corrélations
cor.test(Dselect$CIRCONFERENCE.EN.CM,Dselect$HAUTEUR.EN.M)

Dselect$LONGEVITE.EN.ANNEES<-as.numeric(Dselect$LONGEVITE.EN.ANNEES)
Dfamd1<-Dselect[,c(1,2,9)]
cor.test(Dselect$CIRCONFERENCE.EN.CM,Dselect$LONGEVITE.EN.ANNEES)
cor.test(Dselect$HAUTEUR.EN.M,Dselect$LONGEVITE.EN.ANNEES)

chisq.test(Dselect$CIRCONFERENCE.EN.CM,Dselect$LONGEVITE.EN.ANNEES)
chisq.test(Dselect$HAUTEUR.EN.M,Dselect$LONGEVITE.EN.ANNEES)

#2.Influence des individus
Infl<- res1.FAMD$ind$contrib[, c("Dim.1", "Dim.2")]
Infl
Infl<-as.data.frame(Infl)

#Individus les mieux représentés pour une qualité de l'air dégradée sur l'axe 2 
InflOrder1<-Infl[order(Infl$Dim.2, decreasing = T),]["Dim.2"] 
head(InflOrder1)

Individu34<-Dselect[34,]
Individu34
Individu133<-Dselect[133,]
Individu133
Individu104<-Dselect[104,]
Individu104
Individu86<-Dselect[86,]
Individu86
Individu61<-Dselect[61,]
Individu61
Individu72<-Dselect[72,]
Individu72

x<-c(Individu34$CIRCONFERENCE.EN.CM,Individu133$CIRCONFERENCE.EN.CM,
     Individu104$CIRCONFERENCE.EN.CM,Individu86$CIRCONFERENCE.EN.CM,
     Individu61$CIRCONFERENCE.EN.CM,Individu72$CIRCONFERENCE.EN.CM)
mc<-mean(x)

y<-c(Individu34$HAUTEUR.EN.M,Individu133$HAUTEUR.EN.M,
     Individu104$HAUTEUR.EN.M,Individu86$HAUTEUR.EN.M,
     Individu61$HAUTEUR.EN.M,Individu72$HAUTEUR.EN.M)
mh<-mean(y)

l<-c(Individu34$LONGEVITE.EN.ANNEES,Individu133$LONGEVITE.EN.ANNEES,
     Individu104$LONGEVITE.EN.ANNEES,Individu86$LONGEVITE.EN.ANNEES,
     Individu61$LONGEVITE.EN.ANNEES,Individu72$LONGEVITE.EN.ANNEES)
ml<-mean(l)

mc
mh
ml

mcDselect<-mean(Dselect[,1])
mhDselect<-mean(Dselect[,2])
mlDselect<-floor(mean(Dselect[,10]))

mcDselect
mhDselect
mlDselect

#Contributions des dimensions
group <- get_famd_var(res1.FAMD, "var")
group$contrib
fviz_contrib (res1.FAMD, "var", axes = 1)
fviz_contrib (res1.FAMD, "var", axes = 2)

#Contribution des individus les plus représentatifs pour une qualité de l'air dégradée sur l'axe 2
sum(head(InflOrder1))

#Individus les mieux représentés pour une qualité de l'air mauvaise sur l'axe 2 
InflOrder2<-Infl[order(Infl$Dim.2, decreasing = F),]["Dim.2"] 
head(InflOrder2)

#Contribution des individus les plus représentatifs pour une qualité de l'air dégradée sur l'axe 2
sum(head(InflOrder2))

Individu121<-Dselect[121,]
Individu121
Individu141<-Dselect[141,]
Individu141
Individu136<-Dselect[136,]
Individu136
Individu138<-Dselect[138,]
Individu138
Individu97<-Dselect[97,]
Individu97
Individu55<-Dselect[55,]
Individu55


x<-c(Individu121$CIRCONFERENCE.EN.CM,Individu141$CIRCONFERENCE.EN.CM,
     Individu136$CIRCONFERENCE.EN.CM,Individu138$CIRCONFERENCE.EN.CM,
     Individu97$CIRCONFERENCE.EN.CM,Individu55$CIRCONFERENCE.EN.CM)
mc<-mean(x)

y<-c(Individu121$HAUTEUR.EN.M,Individu141$HAUTEUR.EN.M,
      Individu136$HAUTEUR.EN.M,Individu138$HAUTEUR.EN.M,
      Individu97$HAUTEUR.EN.M,Individu55$HAUTEUR.EN.M)
mh<-mean(y)

z<-c(Individu121$LONGEVITE.EN.ANNEES,Individu141$LONGEVITE.EN.ANNEES,
     Individu136$LONGEVITE.EN.ANNEES,Individu138$LONGEVITE.EN.ANNEES,
     Individu97$LONGEVITE.EN.ANNEES,Individu55$LONGEVITE.EN.ANNEES)
mz<-floor(mean(z))

mc
mh
mz


#II.Analyses bivariées

######################1ère analyse######################
#Sélection valeurs qualité de l'air
Dselectm<-filter(Dselect, QUALITE.AIR == "Mauvaise")
Dselectd<-filter(Dselect, QUALITE.AIR == "Dégradée")

  #Mauvaise qualité de l'air
cep<-table(Dselectm$ESPECE, Dselectm$QUALITE.AIR)
cep<-cprop(cep)
cep<-as.data.frame(cep)
cep[order(cep$Freq, decreasing = T),][c("Var1","Var2","Freq")]

cgp<-table(Dselectm$GENRE, Dselectm$QUALITE.AIR)
cgp<-cprop(cgp)
cgp<-as.data.frame(cgp)
cgp[order(cgp$Freq, decreasing = T),][c("Var1","Var2","Freq")]


  #Qualité de l'air dégradée
cep<-table(Dselectd$ESPECE, Dselectd$QUALITE.AIR)
cep<-cprop(cep)
cep<-as.data.frame(cep)
cep[order(cep$Freq, decreasing = T),][c("Var1","Var2","Freq")]

cgp<-table(Dselectd$GENRE, Dselectd$QUALITE.AIR)
cgp<-cprop(cgp)
cgp<-as.data.frame(cgp)
cgp[order(cgp$Freq, decreasing = T),][c("Var1","Var2","Freq")]

#Platanus, Aesculus, Quercus
platanus<-(Dselect$GENRE=="Platanus")
Dselect$platanus<-(Dselect$GENRE=="Platanus")

aesculus<-(Dselect$GENRE=="Aesculus")
Dselect$aesculus<-(Dselect$GENRE=="Aesculus")

Quercus<-(Dselect$GENRE=="Quercus")
Dselect$Quercus<-(Dselect$GENRE=="Quercus")

  #Platanus
Dselect1<-filter(Dselect, GENRE != "Corylus"&GENRE != "Quercus"&
                 GENRE != "Aesculus"&GENRE != "Cedrus"&GENRE != "Ginkgo"&
                 GENRE != "Pterocarya"&GENRE != "Acer")
nbp<-filter(Dselect, GENRE=="Platanus")
n<-nrow(nbp)

plot(factor(Dselect1$platanus), factor(Dselect1$QUALITE.AIR), xlab="Platanus",
     ylab="Qualité de l'air", main="Proportions de pollution sachant les platanus")

#Aesculus
Dselect1<-filter(Dselect, GENRE != "Corylus"&GENRE != "Quercus"&
                   GENRE != "Platanus"&GENRE != "Cedrus"&GENRE != "Ginkgo"&
                   GENRE != "Pterocarya"&GENRE != "Acer")


plot(factor(Dselect1$aesculus), factor(Dselect1$QUALITE.AIR), xlab="Aesculus",
     ylab="Qualité de l'air", main="Proportions de pollution sachant les aesculus")

  #Quercus
Dselect1<-filter(Dselect, GENRE != "Corylus"&GENRE != "Aesculus"&
                   GENRE != "Platanus"&GENRE != "Cedrus"&GENRE != "Ginkgo"&
                   GENRE != "Pterocarya"&GENRE != "Acer")

plot(factor(Dselect1$Quercus), factor(Dselect1$QUALITE.AIR), xlab="Quercus",
     ylab="Qualité de l'air", main="Proportions de pollution sachant les quercus")


Dselect2<-filter(Dselect, GENRE=="Platanus"|GENRE == "Aesculus"|GENRE=="Quercus")
Dselect2$ESPECE

#Test d'indépendance variables genre et qualité de l'air
chisq.test(Dselect2$GENRE,Dselect2$QUALITE.AIR)

Dselect2<-filter(Dselect, GENRE=="Platanus"|GENRE == "Fagus"|GENRE == "Quercus"|
                   GENRE == "Fagus"|GENRE == "Cedrus"|GENRE == "Ginkgo"&
                   GENRE == "Aesculus"|GENRE=="Acer"|GENRE=="Acer"|GENRE=="Corylus"|
                   GENRE=="Pterocarya")

Dselect2<-filter(Dselect, GENRE=="Platanus"|GENRE == "Aesculus"|GENRE=="Quercus")

#Test d'indépendance variables genre et qualité de l'air
chisq.test(Dselect2$GENRE,Dselect2$QUALITE.AIR)
chisq.test(Dselect2$STADEDEVELOPPEMENT,Dselect2$QUALITE.AIR)

#Platanus, Fagus, Quercus
platanus<-(Dselect$GENRE=="Platanus")
Dselect$platanus<-(Dselect$GENRE=="Platanus")

fagus<-(Dselect$GENRE=="Fagus")
Dselect$fagus<-(Dselect$GENRE=="Fagus")

Quercus<-(Dselect$GENRE=="Quercus")
Dselect$Quercus<-(Dselect$GENRE=="Quercus")

#Platanus
Dselect1<-filter(Dselect, GENRE != "Quercus"&GENRE != "Cedrus"&GENRE != "Ginkgo"
                 &GENRE != "Fagus")

plot(factor(Dselect1$platanus), factor(Dselect1$QUALITE.AIR), xlab="Platanus",
     ylab="Qualité de l'air", main="Proportions de pollution sachant les platanus")

#Fagus
Dselect1<-filter(Dselect, GENRE != "Quercus"&GENRE != "Cedrus"&GENRE != "Ginkgo"
                 &GENRE != "Platanus")


plot(factor(Dselect1$fagus), factor(Dselect1$QUALITE.AIR), xlab="Fagus",
     ylab="Qualité de l'air", main="Proportions de pollution sachant les Fagus")

#Quercus
Dselect1<-filter(Dselect, GENRE != "Platanus"&GENRE != "Cedrus"&GENRE != "Ginkgo"
                 &GENRE != "Fagus")

plot(factor(Dselect1$Quercus), factor(Dselect1$QUALITE.AIR), xlab="Quercus",
     ylab="Qualité de l'air", main="Proportions de pollution sachant les quercus")


Dselect2<-filter(Dselect, GENRE=="Platanus"|GENRE == "Aesculus"|GENRE=="Quercus")
Dselect2$ESPECE

#Test d'indépendance variables genre et qualité de l'air
Dselect2<-filter(Dselect, GENRE=="Platanus"|GENRE == "Fagus"|GENRE=="Quercus")

chisq.test(Dselect2$GENRE,Dselect2$QUALITE.AIR)
chisq.test(Dselect2$STADEDEVELOPPEMENT,Dselect2$QUALITE.AIR)

######################2ère analyse######################
#Sélection valeurs stade de développement
Dselectm<-filter(Dselect, QUALITE.AIR == "Mauvaise")
Dselectd<-filter(Dselect, QUALITE.AIR == "Dégradée")

#SD qualité de l'air mauvaise
csp<-table(Dselectm$STADEDEVELOPPEMENT, Dselectm$QUALITE.AIR)
csp<-cprop(csp)
csp<-as.data.frame(csp)
csp[order(csp$Freq, decreasing = T),][c("Var1","Var2","Freq")]

#SD qualité de l'air dégradée
csp<-table(Dselectd$STADEDEVELOPPEMENT, Dselectd$QUALITE.AIR)
csp<-cprop(csp)
csp<-as.data.frame(csp)
csp[order(csp$Freq, decreasing = T),][c("Var1","Var2","Freq")]

#Test d'indépendance variables stade de développement et qualité de l'air

chisq.test(Dselect$STADEDEVELOPPEMENT,Dselect2$QUALITE.AIR)

Dselectmj<-filter(Dselect, STADEDEVELOPPEMENT == "Mature"|
                    STADEDEVELOPPEMENT == "Jeune (arbre)")
chisq.test(Dselectmj$STADEDEVELOPPEMENT,Dselectmj$QUALITE.AIR)

######################Exporter le dataframe et nouvelles mesures pour la dataviz######################
Dselect3<-Dselect
Dselect3$Latitude<-as.factor(D$Latitude)
Dselect3$Longitude<-as.factor(D$Longitude)

write.csv2(Dselect3,"D:\\Cours\\Stage\\The Quantic Factory\\Test technique\\DataFrameExport.csv",row.names = FALSE)

n<-178
Nombre_platanus<-0
Nombre_aesculus<-0
Nombre_quercus<-0
Nombre_autres<-0

for(i in 1:n)
{
  if(Dselect$platanus[i]=="TRUE")
  {
    Nombre_platanus<-Nombre_platanus+1
  }
  
  if(Dselect$aesculus[i]=="TRUE")
    Nombre_aesculus<-Nombre_aesculus+1

  
  if(Dselect$Quercus[i]=="TRUE")
    Nombre_quercus<-Nombre_quercus+1
  
  
  if(Dselect$GENRE[i]!="Platanus"&Dselect$GENRE[i]!="Aesculus"
     &Dselect$GENRE[i]!="Quercus")
  {
    Nombre_autres<-Nombre_autres+1
  }
}

Nombre_platanus
Nombre_aesculus
Nombre_quercus
Nombre_autres


######################Test Classification ascendante hiérarchique######################
res1.hcpc <- HCPC(res1.FAMD, graph = FALSE)
fviz_cluster(res1.hcpc,
             repel = TRUE,            # Evite le chevauchement des textes
             show.clust.cent = TRUE, # Montre le centre des clusters
             palette = "jco",         # Palette de couleurs
             ggtheme = theme_minimal(),
             main = "Factor map"
)

Dfamd1$cluster<-res1.hcpc$data.clust$clust
head(res1.hcpc$data.clust)

res1.hcpc$desc.var$quanti$"1"
res1.hcpc$desc.var$category$"3"

m.c<-tapply(Dfamd1$CIRCONFERENCE.EN.CM, Dfamd1$cluster, mean)
m.c
m.h<-tapply(Dfamd1$HAUTEUR.EN.M, Dfamd1$cluster, mean)
m.h
  
boxplot(Dfamd1$CIRCONFERENCE.EN.CM ~ Dfamd1$cluster, 
        xlab='Cluster', ylab='Circonférence en cm', 
        main=' Circonférence en cm par Cluster')

boxplot(Dfamd1$HAUTEUR.EN.M ~ Dfamd1$cluster, 
        xlab='Cluster', ylab='Hauteur en m', 
        main='Hauteur en m par Cluster')

Dselect$cluster<-res1.hcpc$data.clust$clust

Dcluster1<-filter(Dselect, cluster == "1")
Dcluster1
Dcluster2<-filter(Dselect, cluster == "2")
Dcluster2
Dcluster3<-filter(Dselect, cluster == "3")
Dcluster3




