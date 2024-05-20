################################################################################
#4                                                                             #
#4                 Applied Labor - It's raining women                          #
#4                  Louise Demoury & Gabrielle Sagot                           #
#4                                                                             #
################################################################################

library(dplyr)
library(ggplot2)
library(xtable)

#we use our constructed data 
load("C:/Users/Public/Documents/Demoury_Sagot/Data/data2.RData")

#we compute the number of employees per ministry
eff_min <-data2%>% 
  filter(year=="2012") %>% 
  group_by(MIN) %>% 
  summarise(eff = n())

#and the share of employees in our NH of interest
eff_nh <-data2%>% 
  filter(year=="2012") %>% 
  group_by(MIN,NH) %>% 
  summarise(effNH = n()) %>% 
  filter(NH %in% c("10","11","12"))%>% 
  left_join(., eff_min, by="MIN") %>% 
  mutate(shareNH=effNH/eff)


eff_nhaut <-eff_nh %>%
  group_by(MIN) %>% 
  summarize(tot=sum(effNH)) %>% 
  left_join(., eff_min, by="MIN") %>% 
  mutate(NH="AUT",
         effNH=eff-tot,
         shareNH=effNH/eff) %>% 
  select(MIN,NH,effNH,shareNH,eff) 

nh_min <- rbind(eff_nhaut,eff_nh)

#share of NH of interest per ministry in 2012
colors <- c("AUT"="grey", "10"="#F8766D", "11"="lightgreen", "12"="blue")
ggplot(nh_min, aes(x= MIN, y = shareNH, fill=factor(NH)))+
  geom_bar(stat="identity", position = "stack")+
  scale_fill_manual(values = colors)+
  theme_minimal()+
  labs("Hierarchy within ministries",
       x="Ministry",
       y="Share of hierarchical level", 
       fill =" NH")

#now we show the relative difficulty to get promoted to high hierarchical categories by computing the share of hires in each category
#we have 2 types of recruitment : internal, externaland the incumbents

data2<-data2%>%
  mutate(internal=if_else((NH_1!=NH & !is.na(NH_1)),1,0),
         stable=if_else((NH_1==NH & !is.na(NH_1)),1,0),
         external=if_else(is.na(NH_1),1,0))

table_recruitmentinterne<-data2%>%
   filter((NH=="10" | NH=="11" | NH=="12") & year=="2012")%>%
  group_by(MIN,NH, internal) %>% 
  summarise(count = n())%>%
  mutate(relative_size = count / sum(count)*100)%>%
  filter(internal==1)

table_recruitmentexternal<-data2%>%
  filter((NH=="10" | NH=="11" | NH=="12") & year=="2012")%>%
  group_by(MIN,NH, external) %>% 
  summarise(count = n())%>%
  mutate(relative_size = count / sum(count)*100)%>%
  filter(external==1)

table_recruitmentstable<-data2%>%
  filter((NH=="10" | NH=="11" | NH=="12") & year=="2012")%>%
  group_by(MIN,NH, stable) %>% 
  summarise(count = n())%>%
  mutate(relative_size = count / sum(count)*100)%>%
  filter(stable==1)

table_gender<-data2%>%
  filter((NH=="10" | NH=="11" | NH=="12") & year=="2012")%>%
  group_by(MIN,NH, GENRE) %>% 
  summarise(count = n())%>%
  mutate(relative_size = count / sum(count)*100)%>%
  filter(GENRE==1) %>% 
  select(MIN, NH,relative_size)

#we merge everything to have one big table
final_table<-merge(table_recruitmentinterne, table_recruitmentexternal, by=c("MIN","NH"))
final_table<-subset(final_table, select = -c(internal, external, count.x, count.y))
colnames(final_table)[3]<-"relative_size_internal"
colnames(final_table)[4]<-"relative_size_external"
final_table<-merge(final_table, table_recruitmentstable, by=c("MIN","NH"))
final_table<-subset(final_table, select = -c(stable,  count))
colnames(final_table)[5]<-"relative_size_stable"
final_table<-merge(final_table, eff_nh, by=c("MIN","NH"))
final_table<-merge(final_table, table_gender, by=c("MIN","NH"))
colnames(final_table)[7]<-"eff_tot_min"
colnames(final_table)[8]<-"relative_size_nh"
colnames(final_table)[9]<-"womenshare"
colnum <- c("relative_size_internal","relative_size_external","relative_size_stable","relative_size_nh","womenshare")
final_table <- final_table[,c("MIN", "eff_tot_min", "NH","effNH","relative_size_nh","womenshare","relative_size_internal","relative_size_external","relative_size_stable" )] %>%
  mutate(across(all_of(colnum), ~format(round(.,2),nsmall=2)))

 
tex_table <- xtable(final_table, digits = 2)
write.table(tex_table, "C:/Users/Public/Documents/Demoury_Sagot/Output/summarystats.tex", sep="&", row.names=FALSE, col.names=TRUE)  
