setwd("D:\\R\\R_data")
library(haven)
library(tidyverse)
library(dplyr)
library(janitor)

#1.Data for 2018----
demographic <- read_dta('test_CHARLS/wave4 2018/demographic_background.dta')
health <- read_dta('test_CHARLS/wave4 2018/health_status_and_functioning.dta')
home <- read_dta('test_CHARLS/wave4 2018/family_information.dta')
cognition <- read_dta('test_CHARLS/wave4 2018/Cognition.dta')


cd_2018 <- left_join(demographic,health,by = "ID")
cd_2018 <- left_join(cd_2018,cognition,by = "ID")
cd_2018 <- left_join(cd_2018,tf2015,by = "ID")
cd_2018 <- left_join(cd_2018,home,by = "ID")

#2.Cognitive function----
cd_2018 <- mutate(cd_2018,
                  ymd_1 = ifelse(dc001_w4>1,NA,dc001_w4),
                  ymd_2 = ifelse(dc006_w4>1,NA,dc006_w4),
                  ymd_3 = ifelse(dc003_w4>1,NA,dc003_w4),
                  dc002 = ifelse(dc002_w4>1,NA,dc002_w4),
                  dc005 = ifelse(dc005_w4>1,NA,dc005_w4),
                  math_1 = ifelse(dc014_w4_1_1 == 93, 1, 0),    
                  math_2 = ifelse(dc014_w4_2_1 == (dc014_w4_1_1 - 7), 1, 0), 
                  math_3 = ifelse(dc014_w4_3_1 == (dc014_w4_2_1 - 7), 1, 0),
                  math_4 = ifelse(dc014_w4_4_1 == (dc014_w4_3_1 - 7), 1, 0),
                  math_5 = ifelse(dc014_w4_5_1 == (dc014_w4_4_1 - 7), 1, 0),
                  draw = ifelse(dc024_w4>1,0,dc024_w4),
                  dc030_1 = dc028_w4_s1,
                  dc030_2 = ifelse(dc030_w4_s2==2,1,0),
                  dc030_3 = ifelse(dc030_w4_s3==3,1,0),
                  dc030_4 = ifelse(dc030_w4_s4==4,1,0),
                  dc030_5 = ifelse(dc030_w4_s5==5,1,0),
                  dc030_6 = ifelse(dc030_w4_s6==6,1,0),
                  dc030_7 = ifelse(dc030_w4_s7==7,1,0),
                  dc030_8 = ifelse(dc030_w4_s8==8,1,0),
                  dc030_9 = ifelse(dc030_w4_s9==9,1,0),
                  dc030_10 = ifelse(dc030_w4_s10==10,1,0),
                  dc047_1 = dc047_w4_s1,
                  dc047_2 = ifelse(dc047_w4_s2==2,1,0),
                  dc047_3 = ifelse(dc047_w4_s3==3,1,0),
                  dc047_4 = ifelse(dc047_w4_s4==4,1,0),
                  dc047_5 = ifelse(dc047_w4_s5==5,1,0),
                  dc047_6 = ifelse(dc047_w4_s6==6,1,0),
                  dc047_7 = ifelse(dc047_w4_s7==7,1,0),
                  dc047_8 = ifelse(dc047_w4_s8==8,1,0),
                  dc047_9 = ifelse(dc047_w4_s9==9,1,0),
                  dc047_10 = ifelse(dc047_w4_s10==10,1,0),
                  (across(c(starts_with("ymd_"), 
                            starts_with("math_"), 
                            starts_with("dc030_"), 
                            starts_with("dc047_"), 
                            "draw"),               
                          ~ replace(., is.na(.), 0))),
                  cognition1 = ymd_1+ymd_2+ymd_3+dc002+dc005+math_1+math_2+math_3+math_4+math_5+ draw,
                  cognition2_1 = dc030_1+dc030_2+dc030_3+dc030_4+dc030_5+dc030_6+dc030_7+dc030_8+dc030_9+dc030_10,
                  cognition2_2 = dc047_1+dc047_2+dc047_3+dc047_4+dc047_5+dc047_6+dc047_7+dc047_8+dc047_9+dc047_10,
                  cognition2 = rowSums(across(starts_with("cognition2_"))),
                  cognition = rowSums(across(c(cognition1, cognition2))))

#3.Depression----
reverse_vars <- c("dc013", "dc016")
cd_2018[reverse_vars] <- 5 - cd_2018[reverse_vars]
cd_2018 <- mutate(cd_2018,
                  depression = (dc009+dc010+dc011+dc012+dc013+dc014+dc015+dc016+dc017+dc018)-10)

#4.Frailty----
cd_2018 <- mutate(cd_2018,
                  fra1 = ifelse(fra1 == 1, fra1, ifelse(fra1 == 0 & da007_1_ == 1, 1, ifelse(fra1 == 0 & da007_1_ == 2, 0, NA))),
                  fra2 = ifelse(fra2 == 1, fra2, ifelse(fra2 == 0 & da007_2_ == 1, 1, ifelse(fra2 == 0 & da007_2_ == 2, 0, NA))),
                  fra3 = ifelse(fra3 == 1, fra3, ifelse(fra3 == 0 & da007_3_ == 1, 1, ifelse(fra3 == 0 & da007_3_ == 2, 0, NA))),
                  fra4 = ifelse(fra4 == 1, fra4, ifelse(fra4 == 0 & da007_4_ == 1, 1, ifelse(fra4 == 0 & da007_4_ == 2, 0, NA))),
                  fra5 = ifelse(fra5 == 1, fra5, ifelse(fra5 == 0 & da007_5_ == 1, 1, ifelse(fra5 == 0 & da007_5_ == 2, 0, NA))),
                  fra6 = ifelse(fra6 == 1, fra6, ifelse(fra6 == 0 & da007_6_ == 1, 1, ifelse(fra6 == 0 & da007_6_ == 2, 0, NA))),
                  fra7 = ifelse(fra7 == 1, fra7, ifelse(fra7 == 0 & da007_7_ == 1, 1, ifelse(fra7 == 0 & da007_7_ == 2, 0, NA))),
                  fra8 = ifelse(fra8 == 1, fra8, ifelse(fra8 == 0 & da007_8_ == 1, 1, ifelse(fra8 == 0 & da007_8_ == 2, 0, NA))),
                  fra9 = ifelse(fra9 == 1, fra9, ifelse(fra9 == 0 & da007_9_ == 1, 1, ifelse(fra9 == 0 & da007_9_ == 2, 0, NA))),
                  fra10 = ifelse(fra10 == 1, fra10, ifelse(fra10 == 0 & da007_10_ == 1, 1, ifelse(fra10 == 0 & da007_10_ == 2, 0, NA))),
                  fra11 = ifelse(fra11 == 1, fra11, ifelse(fra11 == 0 & da007_11_ == 1, 1, ifelse(fra11 == 0 & da007_11_ == 2, 0, NA))),
                  fra12 = ifelse(fra12 == 1, fra12, ifelse(fra12 == 0 & da007_12_ == 1, 1, ifelse(fra12 == 0 & da007_12_ == 2, 0, NA))),
                  fra13 = ifelse(fra13 == 1, fra13, ifelse(fra13 == 0 & da007_13_ == 1, 1, ifelse(fra13 == 0 & da007_13_ == 2, 0, NA))),
                  fra14 = ifelse(fra14 == 1, fra14, ifelse(fra14 == 0 & da007_14_ == 1, 1, ifelse(fra14 == 0 & da007_14_ == 2, 0, NA))),
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
cd_2018$na_count <- rowSums(is.na(cd_2018[, fra_vars])) 
cd_2018$na_ratio <- cd_2018$na_count / 36

cd_2018$median_value <- apply(cd_2018[, fra_vars], 1, median, na.rm = TRUE)
for (var in fra_vars) {                                
  cd_2018[[var]] <- ifelse(
    cd_2018$na_ratio < 0.1 & is.na(cd_2018[[var]]),
    cd_2018$median_value,
    cd_2018[[var]])}

cd_2018$frailty_index <- ifelse(cd_2018$na_ratio > 0.1,NA,rowMeans(cd_2018[, fra_vars], na.rm = TRUE))
cd_2018 <- cd_2018[, !names(cd_2018) %in% c("na_count", "na_ratio", "median_value")]
cd_2018 <- mutate(cd_2018,
                  frailty_class = case_when(
                    frailty_index <= 0.1 ~ 0,
                    frailty_index > 0.1 & frailty_index < 0.25 ~ 1, 
                    frailty_index >= 0.25 ~ 2,
                    TRUE ~ NA))
cd_2018 <- mutate(cd_2018,
                  event_fra = case_when(
                    frailty_index < 0.25 ~ 0,
                    frailty_index >= 0.25 ~ 1,
                    TRUE ~ NA_real_)) 

#5.Sleep duration----
cd_2018 <- mutate(cd_2018,
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
cd2018 <- transmute(cd_2018,ID, 
                    age = age +3 ,
                    gender = gender,
                    sleep, sleep_c, napping, napping_c, cognition, social_isolation, si_class, depression, frailty_index, frailty_class, event_fra)
cd2018_w <- transmute(cd2018,ID, 
                      age_18 = age, 
                      sleep_18 = sleep, sleep_c_18 = sleep_c, 
                      napping_18 = napping, napping_c_18 = napping_c,
                      cog_18 = cognition, 
                      si_18 = social_isolation,si_c_18 = si_class, 
                      dep_18 = depression, 
                      fra_i_18 = frailty_index, fra_c_18 = frailty_class, fra_e_18 = event_fra)
tf2018 <- transmute(cd_2018,ID,
                    age = age + 3, 
                    gender = gender, 
                    fra1, fra2, fra3, fra4, fra5, fra6, fra7, fra8, fra9, fra10, fra11, fra12, fra13, fra14)

rm(demographic,health,home,cognition,cd_2018)
