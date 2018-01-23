attach(couples)

couples
couples[couples == ''] <- NA
couples[couples == '.'] <- NA

couples$dconsultation <- as.Date(dconsultation, "%d/%m/%Y")
couples$dconception <- as.Date(dconception, "%d/%m/%Y")
couples = couples[c(1:4, 6:19)]

summary(couples)
summary(enfant)

summary(dconsultation)

#age_f_num<-as.numeric(age_f)
age_f_num <- as.numeric(levels(age_f))[age_f]
summary(age_f_num)
summary(age_h)

summary(diplome_f)
summary(diplome_h)

bmi_h_num <- as.numeric(levels(bmi_h))[bmi_h]
summary(bmi_h_num)

summary(patho_f)
summary(patho_h)

summary(cryptorchidie)
summary(spermo)

summary(bh_f)
summary(ct_f)

summary(fecondite)
summary(duree_infertilite)

summary(traitement)
