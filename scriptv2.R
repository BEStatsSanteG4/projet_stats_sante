attach(couples)

summary(couples)

summary(enfant)
pie(enfant)

#nettoyage des donn�es
#(conversion donn�es)
couples2$age_f<-as.numeric(age_f)
couples2$bmi_h<- as.numeric(bmi_h)
enfant <- factor(enfant)

  #(suppression des NA)

  #c(conversion des dates)
couples2$dconsultation <- as.Date(dconsultation, "%d/%m/%Y")
couples2$dconception <- as.Date(dconception, "%d/%m/%Y")
couples2 = couples2[c(1:4, 6:19)]

 #(uniformisation des donn�es manquantes)

couples2<-couples
couples2[couples2=='']<-NA
couples2[couples2=='.']<-NA

couples3<-couples
couples3[couples3=='']<-'NA'
couples3[couples3=='.']<-'NA'


couples3$patho_f[couples3$patho_f == 'NA']<-'non' # change les 'NA' en 'non' pour la variable patho
couples3$ct_f[couples3$bh_f == 'normal']<- 'ovulation' #change les 'NA' en 'ovulation' de la varibale ct_f si la variable bh_f = normal 
couples3$dconception[couples3$dconception == 'NA']<- '01/06/2009' # concat�ne les variables ddn et dconception (variable ddn_dconception)
couples3$dconception_ddn<-couples3$dconception #on rajoute la variable ddn_dconception

couples3$diplome_f[couples3$diplome_f=='NA']<-'Bac+'
couples3$diplome_h[couples3$diplome_h=='NA']<-'Bac+'

couples3$age_f[couples3$age_f=='NA'] <- '31'


#(d�coupage des variables en classes)
  #age homme 
classe_age_h<-cut(age_h,breaks=c(16,26,30,35,40,99))
levels(age_h)<-c("De 18 � 26 ans ","De 27 � 30 ans","De 31 � 35 ans","De 36 � 40 ans",
                 "Plus de 40 ans")
table(classe_age_h)

  #age femme
classe_age_f<-cut(age_f_num,breaks=c(16,26,30,35,40,99))
levels(age_f_num)<-c("De 18 � 26 ans ","De 27 � 30 ans","De 31 � 35 ans","De 36 � 40 ans",
                 "Plus de 40 ans")
table(classe_age_f)

  #IMC
  #classe_bmi_h
classe_bmi_h <- cut(bmi_h_num,breaks=c(0,19,21,25,28,30,35,99))
table(classe_bmi_h)

  #Dur�e infirtilit�
classe_duree_infertilite<-cut(duree_infertilite,breaks=c(0,10,15,25,35,45,65,170))
levels(duree_infertilite)<-c("De 18 � 26 ans ","De 27 � 30 ans","De 31 � 35 ans","De 36 � 40 ans",
                     "Plus de 40 ans")

table(classe_duree_infertilite)

#univari�

barplot(table(diplome_h))
barplot(table(diplome_f))

table(spermo,patho_f)
table(patho_f)
table(bh_f)

table(cryptorchidie)
couples <- cbind(couples,age_f_num)

#camemberts
pie(table(ct_f),col = 2:4, main = "R�partiton de la pr�sence de l'ovulation")
par(mfrow=c(1,2))
pie(table(diplome_f),main="Niveau de dipl�me pour les femmes",col=4:6)
pie(table(diplome_h), main="Niveau de dipl�me pour les hommes",col=4:6)
pie(table(fecondite),col = 2:3, main = "R�partition du type de f�condit�")

#graphique en barres classes

par(mfrow=c(1,2))
barplot(table(classe_age_h), main = "R�partition de l'�ge chez les hommes",col=2:7)
barplot(table(classe_age_f), main = "R�partition de l'�ge chez les femmes",col=2:7)
par(mfrow=c(1,1))
barplot(table(classe_bmi_h), main= "R�partition de l'IMC chez les femmes" )
barplot(table(classe_duree_infertilite), main = "R�partition de la dur�e d'infertilit� (en mois)")

#bivari�e
  #(�ge/dipl�me")
table(classe_age_h,diplome_h)
table(classe_age_f,diplome_f)
  #(�ge/IMC Homme")
table(classe_age_f,classe_bmi_h)
  #(�ge/patho")
table(classe_age_h,patho_h)
table(classe_age_f,patho_f)
  #(patho/ovulation)
table(patho_f,ct_f)
  #(�ge/grade d'infitilit�)
table(classe_age_h,spermo)


#ACM
install.packages("ade4", dep = TRUE)
library(ade4)
d<- couples3
varqual<-d[, c("enfant","classe_age_f", "classe_age_h", "diplome_h", "diplome_f", 
      "spermo", "bh_f", "ct_f", "patho_f", "fecondite","traitement")]
acm <- dudi.acm(varqual)
