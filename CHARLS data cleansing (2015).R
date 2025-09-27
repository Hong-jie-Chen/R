
setwd("D:\\R\\R_data")
library(haven)     
library(tidyverse) 
library(dplyr)
library(janitor)   

#1.2015 data----
demographic <- read_dta('test_CHARLS/wave3 2015/demographic_background.dta')
health <- read_dta('test_CHARLS/wave3 2015/health_status_and_functioning.dta')
biomarkers <- read_dta('test_CHARLS/wave3 2015/Biomarker.dta')
home <- read_dta('test_CHARLS/wave3 2015/family_information.dta')

cd_2015 <- left_join(demographic,health,by = "ID")
cd_2015 <- left_join(cd_2015,tf2013,by = "ID")
cd_2015 <- left_join(cd_2015,biomarkers,by = "ID")
cd_2015 <- left_join(cd_2015,home,by = "ID")

#2.Cognitive function----
cd_2015 <- mutate(cd_2015,
                  ymd_1 = dc001s1,
                  ymd_2 = ifelse(dc001s2==2,1,0),
                  ymd_3 = ifelse(dc001s3==3,1,0),
                  dc002,
                  dc003,
                  math_1 = ifelse(dc019==93,1,0),
                  math_2 = ifelse(dc020==86,1,0),
                  math_3 = ifelse(dc021==79,1,0),
                  math_4 = ifelse(dc022==72,1,0),
                  math_5 = ifelse(dc023==65,1,0),
                  draw = ifelse(dc025==2,0,dc025),
                  dc006_1 = dc006s1,
                  dc006_2 = ifelse(dc006s2==2,1,0),
                  dc006_3 = ifelse(dc006s3==3,1,0),
                  dc006_4 = ifelse(dc006s4==4,1,0),
                  dc006_5 = ifelse(dc006s5==5,1,0),
                  dc006_6 = ifelse(dc006s6==6,1,0),
                  dc006_7 = ifelse(dc006s7==7,1,0),
                  dc006_8 = ifelse(dc006s8==8,1,0),
                  dc006_9 = ifelse(dc006s9==9,1,0),
                  dc006_10 = ifelse(dc006s10==10,1,0),
                  dc027_1 = dc027s1,
                  dc027_2 = ifelse(dc027s2==2,1,0),
                  dc027_3 = ifelse(dc027s3==3,1,0),
                  dc027_4 = ifelse(dc027s4==4,1,0),
                  dc027_5 = ifelse(dc027s5==5,1,0),
                  dc027_6 = ifelse(dc027s6==6,1,0),
                  dc027_7 = ifelse(dc027s7==7,1,0),
                  dc027_8 = ifelse(dc027s8==8,1,0),
                  dc027_9 = ifelse(dc027s9==9,1,0),
                  dc027_10 = ifelse(dc027s10==10,1,0),
                  (across(c(starts_with("da042_"),
                            starts_with("ymd_"), 
                            starts_with("math_"),
                            starts_with("dc006_"), 
                            starts_with("dc027_"), 
                            "draw"),
                          ~ replace(., is.na(.), 0))),
                  cognition1 = ymd_1+ymd_2+ymd_3+dc002+dc003+math_1+math_2+math_3+math_4+math_5+ draw,
                  cognition2_1 = dc006_1+dc006_2+dc006_3+dc006_4+dc006_5+dc006_6+dc006_7+dc006_8+dc006_9+dc006_10,
                  cognition2_2 = dc027_1+dc027_2+dc027_3+dc027_4+dc027_5+dc027_6+dc027_7+dc027_8+dc027_9+dc027_10,
                  cognition2 = rowMeans(across(starts_with("cognition2_"))),
                  cognition = rowSums(across(c(cognition1, cognition2))))

#3.Depression----
reverse_vars <- c("dc013", "dc016")
cd_2015[reverse_vars] <- 5 - cd_2015[reverse_vars]
cd_2015 <- mutate(cd_2015,
                  depression = (dc009+dc010+dc011+dc012+dc013+dc014+dc015+dc016+dc017+dc018)-10)

#4.Frailty----
cd_2015 <- mutate(cd_2015,
                  fra1 = ifelse(da007_w2_1_1_ == 1, fra1,                  
                                ifelse(da007_w2_1_1_ == 2, 1 - fra1, NA)), 
                  fra2 = ifelse(da007_w2_1_2_ == 1, fra2,                 
                                ifelse(da007_w2_1_2_ == 2, 1 - fra2, NA)),
                  fra3 = ifelse(da007_w2_1_3_ == 1, fra3,                 
                                ifelse(da007_w2_1_3_ == 2, 1 - fra3, NA)),
                  fra4 = ifelse(da007_w2_1_4_ == 1, fra4,                 
                                ifelse(da007_w2_1_4_ == 2, 1 - fra4, NA)),
                  fra5 = ifelse(da007_w2_1_5_ == 1, fra5,                 
                                ifelse(da007_w2_1_5_ == 2, 1 - fra5, NA)),
                  fra6 = ifelse(da007_w2_1_6_ == 1, fra6,                 
                                ifelse(da007_w2_1_6_ == 2, 1 - fra6, NA)),
                  fra7 = ifelse(da007_w2_1_7_ == 1, fra7,                 
                                ifelse(da007_w2_1_7_ == 2, 1 - fra7, NA)),
                  fra8 = ifelse(da007_w2_1_8_ == 1, fra8,                 
                                ifelse(da007_w2_1_8_ == 2, 1 - fra8, NA)),
                  fra9 = ifelse(da007_w2_1_9_ == 1, fra9,                 
                                ifelse(da007_w2_1_9_ == 2, 1 - fra9, NA)),
                  fra10 = ifelse(da007_w2_1_10_ == 1, fra10,                 
                                 ifelse(da007_w2_1_10_ == 2, 1 - fra10, NA)),
                  fra11 = ifelse(da007_w2_1_11_ == 1, fra11,                 
                                 ifelse(da007_w2_1_11_ == 2, 1 - fra11, NA)),
                  fra12 = ifelse(da007_w2_1_12_ == 1, fra12,                 
                                 ifelse(da007_w2_1_12_ == 2, 1 - fra12, NA)),
                  fra13 = ifelse(da007_w2_1_13_ == 1, fra13,                 
                                 ifelse(da007_w2_1_13_ == 2, 1 - fra13, NA)),
                  fra14 = ifelse(da007_w2_1_14_ == 1, fra14,                 
                                 ifelse(da007_w2_1_14_ == 2, 1 - fra14, NA)),
                  fra15 = ifelse(da005_1_ == 1, 1, 0),
                  fra16 = ifelse(da005_2_ == 1, 1, 0),
                  fra17 = ifelse(da005_3_ == 1, 1, 0),
                  fra18 = ifelse(da005_4_ == 1, 1, 0),
                  fra19 = ifelse(da005_5_ == 1, 1, 0),
                  fra20 = ifelse(db010 %in% c(3, 4), 1, 0),
                  fra21 = ifelse(db011 %in% c(3, 4), 1, 0),
                  fra22 = ifelse(db012 %in% c(3, 4), 1, 0),
                  fra23 = ifelse(db013 %in% c(3, 4), 1, 0),
                  fra24 = ifelse(db014 %in% c(3, 4), 1, 0),
                  fra25 = ifelse(db015 %in% c(3, 4), 1, 0),
                  fra26 = ifelse(db016 %in% c(3, 4), 1, 0),
                  fra27 = ifelse(db017 %in% c(3, 4), 1, 0),
                  fra28 = ifelse(db018 %in% c(3, 4), 1, 0),
                  fra29 = ifelse(db019 %in% c(3, 4), 1, 0),
                  fra30 = ifelse(db020 %in% c(3, 4), 1, 0),
                  fra31 = ifelse(db004 %in% c(3, 4), 1, 0),
                  fra32 = ifelse(db005 %in% c(3, 4), 1, 0),
                  fra33 = ifelse(db006 %in% c(3, 4), 1, 0),
                  fra34 = ifelse(db007 %in% c(3, 4), 1, 0),
                  fra35 = ifelse(db008 %in% c(3, 4), 1, 0),
                  fra36 = ifelse(db009 %in% c(3, 4), 1, 0))

fra_vars <- paste0("fra", 1:36)                          
cd_2015$na_count <- rowSums(is.na(cd_2015[, fra_vars])) 
cd_2015$na_ratio <- cd_2015$na_count / 36

cd_2015$median_value <- apply(cd_2015[, fra_vars], 1, median, na.rm = TRUE)
for (var in fra_vars) { 
  cd_2015[[var]] <- ifelse(
    cd_2015$na_ratio < 0.1 & is.na(cd_2015[[var]]),
    cd_2015$median_value,
    cd_2015[[var]])}

cd_2015$frailty_index <- ifelse(cd_2015$na_ratio > 0.1,NA,rowMeans(cd_2015[, fra_vars], na.rm = TRUE))

cd_2015 <- cd_2015[, !names(cd_2015) %in% c("na_count", "na_ratio", "median_value")]

cd_2015 <- mutate(cd_2015,
                  frailty_class = case_when(
                    frailty_index <= 0.1 ~ 0,
                    frailty_index > 0.1 & frailty_index < 0.25 ~ 1,
                    frailty_index >= 0.25 ~ 2,
                    TRUE ~ NA)) 

cd_2015 <- mutate(cd_2015,
                  event_fra = case_when(
                    frailty_index < 0.25 ~ 0,
                    frailty_index >= 0.25 ~ 1,
                    TRUE ~ NA_real_)) 

#5.Sleep duration----
cd_2015 <- mutate(cd_2015,
                  sleep = da049,
                  sleep_c = ifelse(da049 < 6 , 1,
                                   ifelse(da049 >= 6 & da049< 9, 0,
                                          ifelse(da049 >= 9 , 2,NA_real_))),
                  napping = da050,
                  napping_c = ifelse(da050 == 0 , 0,
                                     ifelse(da050 > 0 & da050 <= 30, 1,
                                            ifelse(da050 > 30 & da050 <= 60, 2,
                                                   ifelse(da050 > 60 & da050 <=90 ,3,
                                                          ifelse(da050 >90 ,4, NA_real_))))))

#6.Data extraction----
cd2015 <- transmute(cd_2015,ID, 
                    age = age + 2,
                    gender,
                    sleep, sleep_c, napping, napping_c, cognition, social_isolation, si_class, depression, frailty_index, frailty_class, event_fra)
cd2015_w <- transmute(cd2015,ID, 
                      age_15 = age, 
                      sleep_15 = sleep, sleep_c_15 = sleep_c,
                      napping_15 = napping, napping_c_15 = napping_c,
                      cog_15 = cognition, 
                      si_15 = social_isolation, si_c_15 = si_class, 
                      dep_15 = depression, 
                      fra_i_15 = frailty_index, fra_c_15 = frailty_class, fra_e_15 = event_fra)

tf2015 <- transmute(cd_2015,ID,
                    age = age+2, 
                    gender,
                    fra1, fra2, fra3, fra4, fra5, fra6, fra7, fra8, fra9, fra10, fra11, fra12, fra13, fra14)

rm(demographic,biomarkers,health,home,cd_2015)
