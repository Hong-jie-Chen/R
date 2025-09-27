# Required packages
setwd("D:\\R\\R_data")
library(haven)
library(tidyverse)
library(dplyr)
library(janitor)

#1.Data for 2011----
#data retrieval
demographic <- read_dta('test_CHARLS/wave1 2011(baseline)/demographic_background.dta')
health <- read_dta('test_CHARLS/wave1 2011(baseline)/health_status_and_functioning.dta')
biomarkers <- read_dta('test_CHARLS/wave1 2011(baseline)/biomarkers.dta')
home <- read_dta('test_CHARLS/wave1 2011(baseline)/family_information.dta')
household <- read_dta('test_CHARLS/wave1 2011(baseline)/household_roster.dta')
weight <- read_dta('test_CHARLS/wave1 2011(baseline)/weight.dta')
PSU <- read_dta('test_CHARLS/wave1 2011(baseline)/psu.dta')

#ID restructuring
cd_2011 <- left_join(demographic,PSU,by = "communityID")
cd_2011 <- left_join(cd_2011,home,by = "ID")
cd_2011 <- left_join(cd_2011,household,by = "ID")
cd_2011 <- left_join(cd_2011,biomarkers,by = "ID")
cd_2011 <- left_join(cd_2011,health,by = "ID")
cd_2011 <- left_join(cd_2011,weight,by = "ID")
cd_2011 <- cd_2011 %>%
  mutate(ID = ifelse(nchar(ID) > 1, 
                     str_replace(ID, pattern = "(.)(.)$", replacement = "\\10\\2"), 
                     ID))

# Convert all "NA" in the data box to missing values
cd_2011 <- cd_2011 %>%
  mutate(across(where(is.character), ~ na_if(., "NA")))

#2.Cognitive Function----
cd_2011 <- mutate(cd_2011,
                  ymd_1 = dc001s1,
                  ymd_2 = ifelse(dc001s2==2,1,0),#转换数值“把2变为1”
                  ymd_3 = ifelse(dc001s3==3,1,0),
                  dc002,
                  dc003,
                  math_1 = ifelse(dc019 == 93, 1, 0),           # 第一题固定标准
                  math_2 = ifelse(dc020 == (dc019 - 7), 1, 0),  # 后续题目基于前题实际值动态计算
                  math_3 = ifelse(dc021 == (dc020 - 7), 1, 0),
                  math_4 = ifelse(dc022 == (dc021 - 7), 1, 0),
                  math_5 = ifelse(dc023 == (dc022 - 7), 1, 0),
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
                  cognition2 = rowSums(across(starts_with("cognition2_"))),
                  cognition = rowSums(across(c(cognition1, cognition2))))


#3.Depression ----
reverse_vars <- c("dc013", "dc016")
cd_2011[reverse_vars] <- 5 - cd_2011[reverse_vars]
cd_2011 <- mutate(cd_2011,
                  depression = (dc009+dc010+dc011+dc012+dc013+dc014+dc015+dc016+dc017+dc018)-10)

#4.Frailty ----
cd_2011 <- mutate(cd_2011,
                  fra1 = ifelse(da007_1_ == 1, 1, 0),
                  fra2 = ifelse(da007_2_ == 1, 1, 0),
                  fra3 = ifelse(da007_3_ == 1, 1, 0),
                  fra4 = ifelse(da007_4_ == 1, 1, 0),
                  fra5 = ifelse(da007_5_ == 1, 1, 0),
                  fra6 = ifelse(da007_6_ == 1, 1, 0),
                  fra7 = ifelse(da007_7_ == 1, 1, 0),
                  fra8 = ifelse(da007_8_ == 1, 1, 0),
                  fra9 = ifelse(da007_9_ == 1, 1, 0),
                  fra10 = ifelse(da007_10_ == 1, 1, 0),
                  fra11 = ifelse(da007_11_ == 1, 1, 0),
                  fra12 = ifelse(da007_12_ == 1, 1, 0),
                  fra13 = ifelse(da007_13_ == 1, 1, 0),
                  fra14 = ifelse(da007_14_ == 1, 1, 0),
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
### Calculate the percentage missing for each sample
fra_vars <- paste0("fra", 1:36)                          
cd_2011$na_count <- rowSums(is.na(cd_2011[, fra_vars])) 
cd_2011$na_ratio <- cd_2011$na_count / 36
### Median interpolation (samples with <10% missing)
cd_2011$median_value <- apply(cd_2011[, fra_vars], 1, median, na.rm = TRUE)
for (var in fra_vars) {                                  
  cd_2011[[var]] <- ifelse(
    cd_2011$na_ratio < 0.1 & is.na(cd_2011[[var]]),
    cd_2011$median_value,
    cd_2011[[var]])}
###Calculate the debilitation index
#Calculate the mean and deal with high missing samples
cd_2011$frailty_index <- ifelse(cd_2011$na_ratio > 0.1,NA,rowMeans(cd_2011[, fra_vars], na.rm = TRUE))

###Clean up intermediate variables
cd_2011 <- cd_2011[, !names(cd_2011) %in% c("na_count", "na_ratio", "median_value")]

### Divide the frailty
cd_2011 <- mutate(cd_2011,
                  frailty_class = case_when(
                    frailty_index <= 0.1 ~ 0,
                    frailty_index > 0.1 & frailty_index < 0.25 ~ 1,
                    frailty_index >= 0.25 ~ 2,
                    TRUE ~ NA))
###Divide the frailty event
cd_2011 <- mutate(cd_2011,
                  event_fra = case_when(
                    frailty_index < 0.25 ~ 0,
                    frailty_index >= 0.25 ~ 1,
                    TRUE ~ NA_real_))

#5.Sleep duration----
cd_2011 <- mutate(cd_2011,
                  sleep = da049,
                  sleep_c = ifelse(da049 < 6 , 1,
                                   ifelse(da049 >= 6 & da049<= 8, 0,
                                          ifelse(da049 > 8 , 2,NA_real_))),
                  napping = da050,
                  napping_c = ifelse(da050 == 0 , 0,
                                   ifelse(da050 > 0 & da050 <= 30, 1,
                                          ifelse(da050 > 30 & da050 <= 60, 2,
                                                 ifelse(da050 > 60 & da050 <=90 ,3,
                                                        ifelse(da050 >90 ,4, NA_real_))))))


#6.mention of covariate----
cd_2011 <- mutate(cd_2011,
                  age = ifelse(!is.na(ba002_1), 2011 - ba002_1, ba004),
                  gender = rgender,
                  residence = a001,
                  education = ifelse(bd001 == 1 , 1,
                                     ifelse(bd001 %in% c(2, 3, 4), 2,
                                            ifelse(bd001 ==5, 3, 4))),
                  marital = ifelse(be001 %in%c(1,2), 1, 2),
                  smoking = ifelse(da059 == 2, 0,
                                   ifelse(da061 == 2, 1, 2)),
                  drinking = ifelse(da067 == 3, 0, 1),
                  social_active = case_when(
                    da056s12 == 12 ~ 0, da056s1 == 1~1, da056s2 == 2~1, da056s3 == 3~1, da056s4 == 4~1,
                    da056s5 == 5 ~ 1, da056s6 == 6~1, da056s7 == 7~1, da056s8 == 8~1, da056s9 == 9~1,
                    da056s10 == 10 ~ 1, da056s11 == 11~1,
                    TRUE ~ NA_integer_),
                  # Height outliers handled
                  height = ifelse(qi002 > 210 | qi002 < 50, NA, qi002),
                  P25_h = quantile(height, 0.25, na.rm = TRUE),
                  P75_h = quantile(height, 0.75, na.rm = TRUE),
                  IQR_h = P75_h - P25_h,
                  high_h = P75_h + 1.5 * IQR_h,
                  low_h = P25_h - 1.5 * IQR_h,
                  height = ifelse(height > high_h | height < low_h, NA, height),
                  # Handle weight outliers
                  weight = ifelse(ql002 > 150 | ql002 < 10, NA, ql002),
                  P25_w = quantile(weight, 0.25, na.rm = TRUE),
                  P75_w = quantile(weight, 0.75, na.rm = TRUE),
                  IQR_w = P75_w - P25_w,
                  high_w = P75_w + 1.5 * IQR_w,
                  low_w = P25_w - 1.5 * IQR_w,
                  weight = ifelse(weight > high_w | weight < low_w, NA, weight),
                  # Calculate derived variables
                  bmi = weight / (height/100)^2)

cd_2011$education <- ifelse(is.na(cd_2011$education), 4, cd_2011$education)

get_mode <- function(x) {
  ux <- na.omit(unique(x))
  tab <- tabulate(match(x, ux))
  if(length(ux[tab == max(tab)]) > 1) {
    sample(ux[tab == max(tab)], 1) 
  } else {
    ux[which.max(tab)]
  }
}

cd_2011 <- cd_2011 %>%
group_by(education, marital) %>% 
  mutate(residence = ifelse(is.na(residence), get_mode(residence), residence)) %>%
  ungroup()

cd_2011 <- cd_2011 %>%
  group_by(education, marital,residence) %>%
  mutate(smoking = ifelse(is.na(smoking), get_mode(smoking), smoking)) %>%
  ungroup()

cd_2011 <- cd_2011 %>%
  group_by(education, marital, residence, smoking) %>%
  mutate(drinking = ifelse(is.na(drinking), get_mode(drinking), drinking)) %>%
  ungroup()

cd_2011 <- cd_2011 %>%
  group_by(education, marital, residence, smoking, drinking) %>%
  mutate(drinking = ifelse(is.na(drinking), get_mode(drinking), drinking)) %>%
  ungroup()

cd_2011 <- cd_2011 %>%
  group_by(residence, education, social_active) %>% 
  mutate(bmi = ifelse(is.na(bmi), mean(bmi, na.rm = TRUE), bmi)) %>%
  ungroup()

cd_2011 <- mutate(cd_2011, bmi_c = case_when(bmi >=18.5 & bmi <24 ~ 0, bmi < 18.5 ~ 1, bmi >= 24 ~ 2))

##（九）data extraction----
cd2011 <- transmute(cd_2011,ID,communityID = communityID.x,  
                    age, gender, residence, education, marital, smoking, drinking, social_active, bmi, bmi_c,
                    sleep, sleep_c, napping, napping_c, cognition, social_isolation, si_class, depression, frailty_index, frailty_class, event_fra,
                    province, city, urban_nbs, areatype, ind_weight_ad2, bio_weight2)
cd2011_w <- transmute(cd2011,ID, 
                      age_11= age, 
                      gender, residence, education, marital, smoking, drinking, social_active, bmi, bmi_c, 
                      sleep_11 = sleep, sleep_c_11 = sleep_c,
                      napping_11 = napping, napping_c_11 = napping_c,
                      cog_11 = cognition, 
                      si_11 = social_isolation, si_c_11 = si_class, 
                      dep_11 = depression, 
                      fra_i_11 = frailty_index, fra_c_11 = frailty_class, fra_e_11 = event_fra)

tf2011 <- transmute(cd_2011,ID,age,gender,
                    fra1, fra2, fra3, fra4, fra5, fra6, fra7, fra8, fra9, fra10, fra11, fra12, fra13, fra14)

rm(demographic,biomarkers,health,home,household,PSU,weight,cd_2011)

