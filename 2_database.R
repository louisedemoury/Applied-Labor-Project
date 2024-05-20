################################################################################
#2                                                                             #
#2                 Applied Labor - It's raining women                          #
#2                  Louise Demoury & Gabrielle Sagot                           #
#2                                                                             #
################################################################################

library(dplyr)

#we load yearly SIASP datasets
#they were filtered on SAS to keep observations where individuals work in Fonction publique d'Etat
siasp14 <- read.csv("C:/Users/Public/Documents/Demoury_Sagot/Data/siasp2014fpe.csv", sep=",")
siasp13 <- read.csv("C:/Users/Public/Documents/Demoury_Sagot/Data/siasp2013fpe.csv", sep=",")
siasp12 <- read.csv("C:/Users/Public/Documents/Demoury_Sagot/Data/siasp2012fpe.csv", sep=",")

#for each year of interest, we keep the ministries we need, and create the var treatment (quota binding)
siasp12b <- siasp12 %>% mutate(MIN= case_when(EMP_MIN==1~"AffairesEt",
                                              EMP_MIN==2~"Culture",
                                              EMP_MIN==3~"Agriculture",
                                              EMP_MIN==6~"EducNat",
                                              EMP_MIN==7~"EcoFin",
                                              EMP_MIN==9~"Interieur",
                                              EMP_MIN==10~"Justice",
                                              EMP_MIN==12~"PremierMin",
                                              EMP_MIN==23~"Env",
                                              EMP_MIN==36~"Travail",
                                              EMP_MIN==70~"Défense"))%>% 
  filter(MIN %in% c("Travail", "Culture", "EducNat", "Agriculture", "EcoFin", "AffairesEt", "Interieur")) %>% 
  mutate(quota=if_else(EMP_MIN %in% c(1,3,7,9),1,0),
         year="2012")

siasp13b <- siasp13 %>% mutate(MIN= case_when(EMP_MIN==1~"AffairesEt",
                                              EMP_MIN==2~"Culture",
                                              EMP_MIN==3~"Agriculture",
                                              EMP_MIN==6~"EducNat",
                                              EMP_MIN==7~"EcoFin",
                                              EMP_MIN==9~"Interieur",
                                              EMP_MIN==10~"Justice",
                                              EMP_MIN==12~"PremierMin",
                                              EMP_MIN==23~"Env",
                                              EMP_MIN==36~"Travail",
                                              EMP_MIN==70~"Défense"))%>% 
  filter(MIN %in% c("Travail", "Culture", "EducNat", "Agriculture", "EcoFin", "AffairesEt", "Interieur")) %>% 
  mutate(quota=if_else(EMP_MIN %in% c(1,3,7,9),1,0),
         year="2013")

siasp14b <- siasp14 %>% mutate(MIN= case_when(EMP_MIN==1~"AffairesEt",
                                              EMP_MIN==2~"Culture",
                                              EMP_MIN==3~"Agriculture",
                                              EMP_MIN==6~"EducNat",
                                              EMP_MIN==7~"EcoFin",
                                              EMP_MIN==9~"Interieur",
                                              EMP_MIN==10~"Justice",
                                              EMP_MIN==12~"PremierMin",
                                              EMP_MIN==23~"Env",
                                              EMP_MIN==36~"Travail",
                                              EMP_MIN==70~"Défense"))%>% 
  filter(MIN %in% c("Travail", "Culture", "EducNat", "Agriculture", "EcoFin", "AffairesEt", "Interieur")) %>% 
  mutate(quota=if_else(EMP_MIN %in% c(1,3,7,9),1,0),
         year="2014",
         id_nir=ID_NIR)

#To stack the 3 datasets the variables need to be all the same
#We decided to remove what was different

vardiff1 <- setdiff(names(siasp12b),names(siasp13b))
siasp12b <- siasp12b %>% 
  select(-vardiff1)

vardiff2 <- setdiff(names(siasp13b),names(siasp12b))
siasp13b <- siasp13b %>% 
  select(-vardiff2)

vardiff3 <- setdiff(names(siasp14b),names(siasp12b))
siasp14b <- siasp14b %>% 
  select(-vardiff3)

vardiff4 <- setdiff(names(siasp12b),names(siasp14b))
siasp12b <- siasp12b %>% 
  select(-vardiff4)
siasp13b <- siasp13b %>% 
  select(-vardiff4)

data <- rbind(siasp12b,siasp13b, siasp14b) %>% 
  mutate(post=if_else(year %in% c("2013","2014"),1,0),
         GENRE=if_else(SEXE=="F",1,0),
         TRANS10=if_else(NH=="10"& !(NH_1 %in% c("10",NA)),1,0),
         TRANS11=if_else(NH=="11"& !(NH_1 %in% c("11","10",NA)),1,0),
         TRANS12=if_else(NH=="12"& !(NH_1 %in% c("12","11","10",NA)),1,0),
         recrupriv=if_else(EMP_CHAMP_FINAL_1=="",1,0),
         TIT=if_else(ST=="T",1,0))

#we creates a dataset with the yearly quota compliance (data retried from some public reports)
#this would be used in an IV approach 
quota <- data.frame(
  MIN = c("AffairesEt","Culture","Agriculture","EducNat","EcoFin","Interieur","Justice","PremierMin","Env","Défense"),
  quota13 = c(0.29,0.48,0.37,0.35,0.27,0.37,0.47,0.29,0.36,0.26),
  quota14 = c(0.29,0.28,0.35,0.24,0.25,0.35,0.38,0.30,0.26,0.25),
  EMP_MIN = c(1,2,3,6,7,9,10,12,23,70)
) 

data1 <- merge(data, quota, by = c("MIN", "EMP_MIN"), all.x=TRUE)

#the share of women in such positions would be the instrument
min12 <- siasp12b %>% filter(PCS_FINAL=="331a" & NH=="10" & statut=="TITU")%>% group_by(EMP_MIN) %>% summarise(partf12=mean(SEXE=="F"))

#this is the final dataset!
data2 <- merge(data1, min12, by = c("EMP_MIN"), all.x=TRUE)

save(data2, file="C:/Users/Public/Documents/Demoury_Sagot/Data/data2.RData")
