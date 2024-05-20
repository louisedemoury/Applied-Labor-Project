################################################################################
#3                                                                             #
#3                 Applied Labor - It's raining women                          #
#3                  Louise Demoury & Gabrielle Sagot                           #
#3                                                                             #
################################################################################

library(dplyr)
library(fixest)
library(purrr)

starmaker <- function(estim_res){
  pval <- estim_res$p.value
  est <- estim_res$estimate
  std <- estim_res$std.error
  stars <- case_when(
    pval > .1 ~ "",
    pval > .05 ~ "*",
    pval > .01 ~ "**",
    pval < .01 ~ "***"
  )
  res <- paste0("$\\underset{(") %>% 
    paste0(as.character(formatC(std, digit=5, format="f"))) %>% 
    paste0(")}{") %>% 
    paste0(formatC(est, digits=5, format="f")) %>% 
    paste0("^{") %>% paste0(stars) %>% paste0("}}$")
  return(res)
}

load("C:/Users/Public/Documents/Demoury_Sagot/Data/data2.RData")

##Regressions for 2013##
#gender wage gap
feols1_13 <-  data2 %>% filter(year!="2014") %>% 
  feols(log(S_NET)~ GENRE + quota + quota*GENRE + post + post*GENRE + quota*post + quota*post*GENRE + MIN, cluster=.$MIN)
summary(feols1_13)

#probability that a new hire is a woman
feols2_13 <- data2%>% filter(year!="2014" & NH=="10" & (NH_1!="10" | is.na(NH_1))& TIT==1)  %>%
  feols(GENRE~ quota  + post+ quota*post  + MIN, cluster=.$MIN)
summary(feols2_13)

feols3_13 <- data2%>% filter(year!="2014" & NH=="11" & (NH_1!="11" | is.na(NH_1))& TIT==1)  %>%
  feols(GENRE~ quota  + post+ quota*post  + MIN, cluster=.$MIN)
summary(feols3_13)

feols4_13 <- data2%>% filter(year!="2014" & NH=="12" & (NH_1!="12" | is.na(NH_1))& TIT==1)  %>%
  feols(GENRE~ quota  + post+ quota*post  + MIN, cluster=.$MIN)
summary(feols4_13)

#proba of transition for women in category 
feols5_13 <-  data2 %>% filter(year!="2014"& NH_1=="11" & GENRE==1 & TIT==1) %>%
  feols(TRANS10~ quota  + post+ quota*post  + MIN, cluster=.$MIN)
summary(feols5_13)

feols6_13 <-  data2%>% filter(year!="2014"& NH_1=="12" & GENRE==1 & TIT==1)  %>%
  feols(TRANS11~ quota  + post+ quota*post  + MIN, cluster=.$MIN)
summary(feols6_13)

feols7_13 <-  data2%>% filter(year!="2014"& NH_1=="13"& GENRE==1 & TIT==1)  %>%
  feols(TRANS12~ quota  + post+ quota*post  + MIN, cluster=.$MIN)
summary(feols7_13)

#proba of being recruited in the private sector (versus internally promoted) 
feols8_13 <-  data2%>% filter(year!="2014" & (NH_1!="10" | is.na(NH_1)) & NH=="10" & GENRE==1 & TIT==1)  %>%
  feols(recrupriv~  quota  + post+ quota*post  + MIN, cluster=.$MIN)
summary(feols6_13)

feols9_13 <-  data2%>% filter(year!="2014" & (NH_1!="11" | is.na(NH_1))& NH=="11" & GENRE==1)  %>%
  feols(recrupriv~  quota  + post+ quota*post  + MIN, cluster=.$MIN)
summary(feols9_13)

feols10_13 <-  data2%>% filter(year!="2014" & (NH_1!="12" | is.na(NH_1)) & NH=="12" & GENRE==1)  %>%
  feols(recrupriv~  quota  + post+ quota*post  + MIN, cluster=.$MIN)
summary(feols10_13)

#proba of being "titulaire" for women who were recruited from the private sector 
feols11_13 <- data2%>% filter(year!="2014" & ((recrupriv==1 & NH=="10")|TRANS10==1) & GENRE==1)  %>%
  feols(TIT ~  quota  + post+ quota*post  + MIN, cluster=.$MIN)
summary(feols11_13)

feols12_13 <- data2%>% filter(year!="2014" & ((recrupriv==1 & NH=="10")|TRANS10==1) & GENRE==1)  %>%
  feols(TIT ~  quota  + post+ quota*post  + MIN, cluster=.$MIN)
summary(feols12_13)

feols13_13 <- data2%>% filter(year!="2014" & ((recrupriv==1 & NH=="10")|TRANS10==1) & GENRE==1)  %>%
  feols(TIT ~  quota  + post+ quota*post  + MIN, cluster=.$MIN)
summary(feols13_13)

#age de recrutement privé
feols14_13 <-  data2 %>% filter(year!="2014" & NH=="10"& recrupriv==1  & GENRE==1) %>% 
  feols(AGE ~ quota  + post+ quota*post  + MIN, cluster=.$MIN)
summary(feols14_13)

feols15_13 <-  data2 %>% filter(year!="2014" & NH=="11"& recrupriv==1  & GENRE==1) %>% 
  feols(AGE ~ quota  + post+ quota*post  + MIN, cluster=.$MIN)
summary(feols15_13)

feols16_13 <-  data2 %>% filter(year!="2014" & NH=="12"& recrupriv==1  & GENRE==1) %>% 
  feols(AGE ~ quota  + post+ quota*post  + MIN, cluster=.$MIN)
summary(feols16_13)

##Regressions for 2014##
#gender wage gap
feols1_feol14 <-  data2 %>% filter(year!="2013") %>% 
  feols(log(S_NET)~ GENRE + quota + quota*GENRE + post + post*GENRE + quota*post + quota*post*GENRE + MIN, cluster=.$MIN)
summary(feols1_14)

#probability that a new hire is a woman
feols2_14 <- data2%>% filter(year!="2013" & NH=="10" & (NH_1!="10" | is.na(NH_1))& TIT==1)  %>%
  feols(GENRE~ quota  + post+ quota*post  + MIN, cluster=.$MIN)
summary(feols2_14)

feols3_14 <- data2%>% filter(year!="2013" & NH=="11" & (NH_1!="11" | is.na(NH_1))& TIT==1)  %>%
  feols(GENRE~ quota  + post+ quota*post  + MIN, cluster=.$MIN)
summary(feols3_14)

feols4_14 <- data2%>% filter(year!="2013" & NH=="12" & (NH_1!="12" | is.na(NH_1))& TIT==1)  %>%
  feols(GENRE~ quota  + post+ quota*post  + MIN, cluster=.$MIN)
summary(feols4_14)

#proba of transition for women in category 
feols5_14 <-  data2 %>% filter(year!="2013"& NH_1=="11" & GENRE==1 & TIT==1) %>%
  feols(TRANS10~ quota  + post+ quota*post  + MIN, cluster=.$MIN)
summary(feols5_14)

feols6_14 <-  data2%>% filter(year!="2013"& NH_1=="12" & GENRE==1 & TIT==1)  %>%
  feols(TRANS11~ quota  + post+ quota*post  + MIN, cluster=.$MIN)
summary(feols6_14)

feols7_14 <-  data2%>% filter(year!="2013"& NH_1=="13"& GENRE==1 & TIT==1)  %>%
  feols(TRANS12~ quota  + post+ quota*post  + MIN, cluster=.$MIN)
summary(feols7_14)

#proba of being recruited in the private sector (versus internally promoted) 
feols8_14 <-  data2%>% filter(year!="2013" & (NH_1!="10" | is.na(NH_1)) & NH=="10" & GENRE==1 & TIT==1)  %>%
  feols(recrupriv~  quota  + post+ quota*post  + MIN, cluster=.$MIN)
summary(feols8_14)

feols9_14 <-  data2%>% filter(year!="2013" & (NH_1!="11" | is.na(NH_1))& NH=="11" & GENRE==1)  %>%
  feols(recrupriv~  quota  + post+ quota*post  + MIN, cluster=.$MIN)
summary(feols9_14)

feols10_14 <-  data2%>% filter(year!="2013" & (NH_1!="12" | is.na(NH_1)) & NH=="12" & GENRE==1)  %>%
  feols(recrupriv~  quota  + post+ quota*post  + MIN, cluster=.$MIN)
summary(feols10_14)

#proba of being "titulaire" for women who were recruited from the private sector 
feols11_14 <- data2%>% filter(year!="2013" & ((recrupriv==1 & NH=="10")|TRANS10==1) & GENRE==1)  %>%
  feols(TIT ~  quota  + post+ quota*post  + MIN, cluster=.$MIN)
summary(feols11_14)

feols12_14 <- data2%>% filter(year!="2013" & ((recrupriv==1 & NH=="10")|TRANS10==1) & GENRE==1)  %>%
  feols(TIT ~  quota  + post+ quota*post  + MIN, cluster=.$MIN)
summary(feols12_14)

feols13_14 <- data2%>% filter(year!="2013" & ((recrupriv==1 & NH=="10")|TRANS10==1) & GENRE==1)  %>%
  feols(TIT ~  quota  + post+ quota*post  + MIN, cluster=.$MIN)
summary(feols13_14)

#age de recrutement privé
feols14_14 <-  data2 %>% filter(year!="2013" & NH=="10"& recrupriv==1  & GENRE==1) %>% 
  feols(AGE ~ quota  + post+ quota*post  + MIN, cluster=.$MIN)
summary(feols14_14)

feols15_14 <-  data2 %>% filter(year!="2013" & NH=="11"& recrupriv==1  & GENRE==1) %>% 
  feols(AGE ~ quota  + post+ quota*post  + MIN, cluster=.$MIN)
summary(feols15_14)

feols16_14 <-  data2 %>% filter(year!="2013" & NH=="12"& recrupriv==1  & GENRE==1) %>% 
  feols(AGE ~ quota  + post+ quota*post  + MIN, cluster=.$MIN)
summary(feols16_14)




colnam <- c("\\beta_{F2013}$","Observations", "\\beta_{F2014}$","Observations")

cbind(colnam,
      rbind(
        list(feols2_13,feols5_13, feols8_13, feols11_13, feols14_13) %>% map(
          function(estim_res){
            estim_res %>% broom::tidy() %>% 
              filter(term %in% c("quota:post")) %>% 
              select(estimate, std.error, p.value) %>% 
              starmaker()} %>% as.matrix(n.col=1)) %>% 
          do.call("cbind",.),
        purrr::map(list(feols2_13,feols5_13, feols8_13, feols11_13, feols14_13),nobs) %>% format(big.mark = ",") %>% 
          purrr::map(~matrix(.)) %>% do.call("cbind",.),
        list(feols2_14,feols5_14, feols8_14, feols11_14, feols14_14) %>% map(
          function(estim_res){
            estim_res %>% broom::tidy() %>% 
              filter(term %in% c("quota:post")) %>% 
              select(estimate, std.error, p.value) %>% 
              starmaker()} %>% as.matrix(n.col=1)) %>% 
          do.call("cbind",.),
        purrr::map(list(feols2_14,feols5_14, feols8_14, feols11_14, feols14_14),nobs) %>% format(big.mark = ",") %>% 
          purrr::map(~matrix(.)) %>% do.call("cbind",.)
      )) %>%
  apply(MARGIN = 1, function(ligne){paste(ligne, collapse= " & ")}) %>%
  paste(collapse = "\\\\\\\\ \r") %>%  
  cat(file="C:/Users/Public/Documents/Demoury_Sagot/Output/tabNH10.tex")

cbind(colnam,
      rbind(
        list(feols3_13,feols6_13, feols9_13, feols12_13, feols15_13) %>% map(
          function(estim_res){
            estim_res %>% broom::tidy() %>% 
              filter(term %in% c("quota:post")) %>% 
              select(estimate, std.error, p.value) %>% 
              starmaker()} %>% as.matrix(n.col=1)) %>% 
          do.call("cbind",.),
        purrr::map(list(feols3_13,feols6_13, feols9_13, feols12_13, feols15_13),nobs) %>% format(big.mark = ",") %>% 
          purrr::map(~matrix(.)) %>% do.call("cbind",.),
        list(feols3_14,feols6_14, feols9_14, feols12_14, feols15_14) %>% map(
          function(estim_res){
            estim_res %>% broom::tidy() %>% 
              filter(term %in% c("quota:post")) %>% 
              select(estimate, std.error, p.value) %>% 
              starmaker()} %>% as.matrix(n.col=1)) %>% 
          do.call("cbind",.),
        purrr::map(list(feols3_14,feols6_14, feols9_14, feols12_14, feols15_14),nobs) %>% format(big.mark = ",") %>% 
          purrr::map(~matrix(.)) %>% do.call("cbind",.)
      )) %>%
  apply(MARGIN = 1, function(ligne){paste(ligne, collapse= " & ")}) %>%
  paste(collapse = "\\\\\\\\ \r") %>%  
  cat(file="C:/Users/Public/Documents/Demoury_Sagot/Output/tabNH11.tex")

cbind(colnam,
      rbind(
        list(feols4_13,feols7_13, feols10_13, feols13_13, feols16_13) %>% map(
          function(estim_res){
            estim_res %>% broom::tidy() %>% 
              filter(term %in% c("quota:post")) %>% 
              select(estimate, std.error, p.value) %>% 
              starmaker()} %>% as.matrix(n.col=1)) %>% 
          do.call("cbind",.),
        purrr::map(list(feols4_13,feols7_13, feols10_13, feols13_13, feols16_13),nobs) %>% format(big.mark = ",") %>% 
          purrr::map(~matrix(.)) %>% do.call("cbind",.),
        list(feols4_14,feols7_14, feols10_14, feols13_14, feols16_14) %>% map(
          function(estim_res){
            estim_res %>% broom::tidy() %>% 
              filter(term %in% c("quota:post")) %>% 
              select(estimate, std.error, p.value) %>% 
              starmaker()} %>% as.matrix(n.col=1)) %>% 
          do.call("cbind",.),
        purrr::map(list(feols4_14,feols7_14, feols10_14, feols13_14, feols16_14),nobs) %>% format(big.mark = ",") %>% 
          purrr::map(~matrix(.)) %>% do.call("cbind",.)
      )) %>%
  apply(MARGIN = 1, function(ligne){paste(ligne, collapse= " & ")}) %>%
  paste(collapse = "\\\\\\\\ \r") %>%  
  cat(file="C:/Users/Public/Documents/Demoury_Sagot/Output/tabNH12.tex")

colnam2 <- c("$\\beta_M$","$\\beta_{M-F}$","Observations")

cbind(colnam2,
      rbind(
        list(feols1_13, feols1_14) %>% map(
          function(estim_res){
            estim_res %>% broom::tidy() %>% 
              filter(term %in% c("quota:post","GENRE:quota:post")) %>% 
              select(estimate, std.error, p.value) %>% 
              starmaker()} %>% as.matrix(n.col=1)) %>% 
          do.call("cbind",.),
        purrr::map(list(feols1_13, feols1_14),nobs) %>% format(big.mark = ",") %>% 
          purrr::map(~matrix(.)) %>% do.call("cbind",.)
      )) %>%
  apply(MARGIN = 1, function(ligne){paste(ligne, collapse= " & ")}) %>%
  paste(collapse = "\\\\\\\\ \r") %>%  
  cat(file="C:/Users/Public/Documents/Demoury_Sagot/Output/tabwagegap.tex")

