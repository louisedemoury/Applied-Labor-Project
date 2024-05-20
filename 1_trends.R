################################################################################
#1                                                                             #
#1                 Applied Labor - It's raining women                          #
#1                  Louise Demoury & Gabrielle Sagot                           #
#1                                                                             #
################################################################################

library(dplyr)
library(ggplot2)
library(writexl)

#we load yearly SIASP datasets
#they were filtered on SAS to keep observations where individuals work in Fonction publique d'Etat
siasp17 <- read.csv("C:/Users/Public/Documents/Demoury_Sagot/Data/siasp2017fpe.csv", sep=",")
siasp16 <- read.csv("C:/Users/Public/Documents/Demoury_Sagot/Data/siasp2016fpe.csv", sep=",")
siasp15 <- read.csv("C:/Users/Public/Documents/Demoury_Sagot/Data/siasp2015fpe.csv", sep=",")
siasp14 <- read.csv("C:/Users/Public/Documents/Demoury_Sagot/Data/siasp2014fpe.csv", sep=",")
siasp13 <- read.csv("C:/Users/Public/Documents/Demoury_Sagot/Data/siasp2013fpe.csv", sep=",")
siasp12 <- read.csv("C:/Users/Public/Documents/Demoury_Sagot/Data/siasp2012fpe.csv", sep=",")
siasp11 <- read.csv("C:/Users/Public/Documents/Demoury_Sagot/Data/siasp2011fpe.csv", sep=",")
siasp10 <- read.csv("C:/Users/Public/Documents/Demoury_Sagot/Data/siasp2010fpe.csv", sep=",")

#PRE-TRENDS
#we compute the share of women in our proxy for the share of women appointed in roles subject to the quota
min17 <- siasp17 %>%
  filter(PCS_FINAL=="331A" & NH=="10" & STATUT=="TITU") %>%
  group_by(EMP_MIN) %>% summarise(partf17=mean(SEXE=="F"))

min16 <- siasp16 %>%
  filter(PCS_FINAL=="331a" & NH=="10" & STATUT=="TITU") %>% 
  group_by(EMP_MIN) %>% summarise(partf16=mean(SEXE=="F"))


min15 <- siasp15 %>%
  filter(PCS_FINAL=="331a" & NH=="10" & STATUT=="TITU") %>%
  group_by(EMP_MIN) %>% summarise(partf15=mean(SEXE=="F"))


min14 <- siasp14 %>%
  filter(PCS_FINAL=="331a" & NH=="10" & statut=="TITU") %>%
  group_by(EMP_MIN) %>% summarise(partf14=mean(SEXE=="F"))

min13 <- siasp13 %>%
  filter(PCS_FINAL=="331a" & NH=="10" & statut=="TITU") %>% 
  group_by(EMP_MIN) %>% summarise(partf13=mean(SEXE=="F"))


min12 <- siasp12 %>%
  filter(PCS_FINAL=="331a" & NH=="10" & statut=="TITU") %>% 
  group_by(EMP_MIN) %>% summarise(partf12=mean(SEXE=="F"))

min11 <- siasp11 %>%
  filter(PCS_FINAL=="331a" & NH=="10" & statut=="TITU") %>%
  group_by(EMP_MIN) %>% summarise(partf11=mean(SEXE=="F"))

#dataframe with the share of women in executive position per ministry and year
minevo <- siasp10 %>%
  filter(PCS_FINAL=="331a" & NH=="10" & ST=="T")%>%
  group_by(EMP_MIN) %>% 
  summarise(partf10=mean(SEXE=="F")) %>% 
  merge(.,min11,"EMP_MIN") %>% merge(.,min12,"EMP_MIN") %>% merge(.,min13,"EMP_MIN") %>% 
  merge(.,min14,"EMP_MIN") %>% merge(.,min15,"EMP_MIN") %>% merge(.,min16,"EMP_MIN") %>%
  merge(.,min17,"EMP_MIN") %>% 
  t(.) %>% as.data.frame(.) %>% slice(-1) %>% 
  rename(AffairesEt=V1,
         Culture=V2,
         Agriculture=V3,
         EducNat=V4,
         EcoFin=V5,
         Interieur=V6,
         Justice=V7,
         PremierMin=V8,
         Env=V9,
         Travail=V10,
         Défense=V11) %>% 
  mutate(observation=1:nrow(.)) %>% 
  gather(.,key="min", value="value", -observation)

#we plot the evolution to look at pre-trends
ggplot(minevo, aes(x=observation, y=value, color=min))+
  geom_point()+
  labs(x="Year",
       y="Share of women in executive positions",
       color="Ministries")+
  scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8),
                     labels=c("2010", "2011", "2012", "2013","2014","2015","2016","2017"))+
  geom_vline(xintercept=4, linetype="dashed")+
  annotate("text", x=4, y=0.2,label="20%")+
  geom_vline(xintercept=6, linetype="dashed")+
  annotate("text", x=6, y=0.3,label="30%")+
  geom_vline(xintercept=8, linetype="dashed")+
  annotate("text", x=8, y=0.4,label="40%")+
  theme_minimal()

#we select here the minisitries we will keep for the analysis
minevo2 <-  siasp10 %>%
  filter(PCS_FINAL=="331a" & NH=="10" & ST=="T")%>%
  group_by(EMP_MIN) %>% 
  summarise(partf10=mean(SEXE=="F")) %>% 
  merge(.,min11,"EMP_MIN") %>% merge(.,min12,"EMP_MIN") %>% merge(.,min13,"EMP_MIN") %>% 
  merge(.,min14,"EMP_MIN") %>% merge(.,min15,"EMP_MIN") %>% merge(.,min16,"EMP_MIN") %>%
  merge(.,min17,"EMP_MIN") %>% 
  t(.) %>% as.data.frame(.) %>% slice(-1) %>% 
  rename(AffairesEt=V1,
         Culture=V2,
         Agriculture=V3,
         EducNat=V4,
         EcoFin=V5,
         Interieur=V6,
         Justice=V7,
         PremierMin=V8,
         Env=V9,
         Travail=V10,
         Défense=V11) %>% 
  select(Travail, Culture, EducNat, Agriculture, EcoFin, AffairesEt, Interieur) %>% 
  mutate(observation=1:nrow(.)) %>% 
  gather(.,key="min", value="value", -observation)

ggplot(minevo2, aes(x=observation, y=value, color=min))+
  geom_point()+
  geom_line()+
  labs(x="Year",
       y="Share of women in executive positions",
       color="Ministries")+
  scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8),
                     labels=c("2010", "2011", "2012", "2013","2014","2015","2016","2017"))+
  scale_y_continuous(limits=c(0,0.5))+
  geom_vline(xintercept=4, linetype="dashed")+
  annotate("text", x=4, y=0.2,label="20%")+
  geom_vline(xintercept=6, linetype="dashed")+
  annotate("text", x=6, y=0.3,label="30%")+
  geom_vline(xintercept=8, linetype="dashed")+
  annotate("text", x=8, y=0.4,label="40%")+
  theme_minimal()

#some table we had to produce for the CASD data export of the graph
minevocasd <- dir10 %>%
  group_by(EMP_MIN) %>% 
  summarise(partf10=mean(SEXE=="F"), nobs10=n()) %>% 
  merge(.,min11,"EMP_MIN") %>% merge(.,min12,"EMP_MIN") %>% merge(.,min13,"EMP_MIN") %>% 
  merge(.,min14,"EMP_MIN") %>% merge(.,min15,"EMP_MIN") %>% merge(.,min16,"EMP_MIN") %>%
  merge(.,min17,"EMP_MIN") %>% 
  mutate(EMP_MIN= case_when(EMP_MIN==1~"AffairesEt",
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
  filter(EMP_MIN %in% c("Travail", "Culture", "EducNat", "Agriculture", "EcoFin", "AffairesEt", "Interieur")) %>% 
  write_xlsx(.,"C:\\Users\\Public\\Documents\\Demoury_Sagot\\Output\\datagraph.xlsx")
