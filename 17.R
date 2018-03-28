rm(list=ls()) 
setwd("D:/Group_125/Vasilkov")
getwd()
library("dplyr")
library("tidyverse")
library("nycflights13")
library("tidyr")
library("stringr")
library("dplyr")
library("tibble")
library("readr")
library(readr)
tbl = read_csv("eddypro.csv",skip = 1, na =c("","NA","-9999","-9999.0"), comment=c("[")) 
tbl = tbl[-1,] 
tbl 
tbl = select(tbl, -(roll)) 
tbl<-tbl[,c(-1,-3,-9,-12,-15,-18,-21,-30,-35,-70,-88:-99)] 
names(tbl) 
tbl <- tbl[tbl$DOY>244 & tbl$DOY<334, c(1:ncol(tbl))]
tbl$daytime=as.logical(tbl$daytime)
filter(tbl,daytime == TRUE) 
tbl = tbl %>% mutate_if(is.character, factor) 
names(tbl) = str_replace_all(names(tbl), "[!]","_emph_") 
names(tbl) = names(tbl) %>% 
  str_replace_all("[!]","_emph_") %>% 
  str_replace_all("[?]","_quest_") %>% 
  str_replace_all("[*]","_star_") %>% 
  str_replace_all("[+]","_plus_") %>% 
  str_replace_all("[-]","_minus_") %>% 
  str_replace_all("[@]","_at_") %>% 
  str_replace_all("[$]","_dollar_") %>% 
  str_replace_all("[#]","_hash_") %>% 
  str_replace_all("[/]","_div_") %>% 
  str_replace_all("[%]","_perc_") %>% 
  str_replace_all("[&]","_amp_") %>% 
  str_replace_all("[\\^]","_power_") %>% 
  str_replace_all("[()]","_") 
glimpse(tbl) 
sapply(tbl,is.numeric) 
tbl_numeric = tbl[,sapply(tbl,is.numeric) ] 
tbl_non_numeric = tbl[,!sapply(tbl,is.numeric) ] 
cor_td = cor(tbl_numeric) 
cor_td 
cor_td = cor(drop_na(tbl_numeric)) %>% as.data.frame %>% select(co2_flux) 
vars = row.names(cor_td)[cor_td$co2_flux^2 > .1] %>% na.exclude 
formula = as.formula(paste("co2_flux~", paste(vars,collapse = "+"), sep="")) 
formula 
row_numbers = 1:length(tbl$date) 
teach = sample(row_numbers, floor(length(tbl$date)*.7)) 
test = row_numbers[-teach] 
teaching_tbl_unq = tbl[teach,] 
testing_tbl_unq = tbl[test,] 
mod = lm(formula, data=tbl) 
mod 
coef(mod) 
resid(mod) 
confint(mod) 
summary(mod) 
anova(mod) 

mod1 = lm(co2_flux~(DOY+Tau+rand_err_Tau + H+LE+ rand_err_LE +h2o_flux+ rand_err_h2o_flux + H_strg + h2o_time_lag + sonic_temperature + 
                      air_temperature+air_density+air_molar_volume+RH+T_star_+un_H+un_LE+un_co2_flux+un_h2o_flux+ 
                      w_div_ts_cov+w_div_h2o_cov)^2,data=tbl) 
mod1 
coef(mod1) 
resid(mod1) 
confint(mod1) 
summary(mod1) 
anova(mod1) 
mod2=lm(co2_flux~(DOY+Tau+H+LE+ rand_err_LE +h2o_flux+ rand_err_h2o_flux + H_strg + h2o_time_lag + sonic_temperature + 
                    air_temperature+air_density+air_molar_volume+RH+T_star_+un_H+un_LE+un_co2_flux+un_h2o_flux+ 
                    w_div_ts_cov+w_div_h2o_cov)^2-DOY:LE-DOY:h2o_flux-DOY:H_strg-DOY:sonic_temperature,data=tbl) 
mod2 
coef(mod2) 
resid(mod2) 
confint(mod2) 
summary(mod2) 
anova(mod2) 
