# Pre-analysis
library(gtsummary)
library(survey)
library(haven)
library(tableone)
library(rms)
library(MatchIt)
library(plyr)
library(tidyverse)
library(dplyr)
library(arsenal)
library(readr)
library(readxl)

#1.Data collation and scheduling---------------------------------------------------------------------------------------------
cd2011 <- cd2011 %>% filter(!is.na(age))
cd2011 <- cd2011 %>% filter(!is.na(gender))
cd2011 <- cd2011 %>% filter(!is.na(frailty_index))
cd2011 <- cd2011 %>% filter(!is.na(depression))
cd2011 <- cd2011 %>% filter(!is.na(cognition))
cd2011 <- cd2011 %>% filter(!is.na(sleep_c))
cd2011 <- cd2011 %>% filter(age>=45)

#2.Long data merge----------------------------------------------------------------------
valid_ids <- unique(cd2011$ID)

cd2013_filtered <- cd2013 %>% 
  filter(ID %in% valid_ids)

cd2015_filtered <- cd2015 %>% 
  filter(ID %in% valid_ids)

cd2018_filtered <- cd2018 %>% 
  filter(ID %in% valid_ids)

cd <- bind_rows(
  cd2011 %>% mutate(year = 2011), 
  cd2013_filtered %>% mutate(year = 2013),
  cd2015_filtered %>% mutate(year = 2015),
  cd2018_filtered %>% mutate(year = 2018),
)
rm(cd2013_filtered, cd2015_filtered, cd2018_filtered)

#3.Main analysis##############################################################################################################################
##Main Analysis 1: GBTM Trajectory Analysis-----------------------------------------------------------------------------------------------------------
library(gbmt)
library(tidyr)
library(reshape2)

valid_ids <- cd %>%
  filter(year %in% c(2013, 2015,2018) & !is.na(frailty_index)) %>%
  group_by(ID) %>%
  filter(n_distinct(year) == 3) %>%
  pull(ID) %>%                      
  unique() 
cd <- cd %>%
  filter(ID %in% valid_ids)

cd$year <- as.numeric(cd$year)
cd<-as.data.frame(cd)
varNames<-c("frailty_index")
m1_3_simu <- gbmt(x.names=varNames, unit="ID", time="year", d=2, ng=1, data=cd,scaling=0)
m2_3_simu <- gbmt(x.names=varNames, unit="ID", time="year", d=2, ng=2, data=cd,scaling=0)
m3_3_simu <- gbmt(x.names=varNames, unit="ID", time="year", d=2, ng=3, data=cd,scaling=0)
m4_3_simu <- gbmt(x.names=varNames, unit="ID", time="year", d=2, ng=4, data=cd,scaling=0)
m5_3_simu <- gbmt(x.names=varNames, unit="ID", time="year", d=2, ng=5, data=cd,scaling=0)
summary(m1_3_simu)
summary(m2_3_simu)
summary(m3_3_simu)
summary(m4_3_simu)
summary(m5_3_simu)

ICs_3<-rbind(m1_3_simu$ic, m2_3_simu$ic, m3_3_simu$ic,m4_3_simu$ic,m5_3_simu$ic)
ICs_3<-as.data.frame(ICs_3)

data_3_1 <- m1_3_simu$appa
data_3_2 <- m2_3_simu$appa
data_3_3 <- m3_3_simu$appa
data_3_4 <- m4_3_simu$appa
data_3_5 <- m5_3_simu$appa

max_length <- max(length(data_3_1), length(data_3_2), length(data_3_3), length(data_3_4), length(data_3_5))
traj_3_1 <- c(data_3_1, rep(NA, max_length - length(data_3_1)))
traj_3_2 <- c(data_3_2, rep(NA, max_length - length(data_3_2)))
traj_3_3 <- c(data_3_3, rep(NA, max_length - length(data_3_3)))
traj_3_4 <- c(data_3_4, rep(NA, max_length - length(data_3_4)))
traj_3_5 <- c(data_3_5, rep(NA, max_length - length(data_3_5)))

avePP <- data.frame(traj_3_1, traj_3_2, traj_3_3, traj_3_4, traj_3_5)
avePP_t <- t(avePP)
colnames(avePP_t) <- c("G1", "G2", "G3","G4", "G5")
avePP_t<-as.data.frame(avePP_t)
avePP_t$model <- row.names(avePP_t)
row.names(avePP_t) <- NULL
avePP_t<-avePP_t[,c(1,2,3,4,5)]


table_simu_3<-cbind(avePP_t,ICs_3)
print(table_simu_3)
write.csv(table_simu_3, "7.29_GBTM_table.csv", row.names = FALSE, fileEncoding = "UTF-8")


OCC_3_2<-((m2_3_simu$appa)/(1-m2_3_simu$appa))/(m2_3_simu$prior/(1-m2_3_simu$prior))
OCC_3_3<-((m3_3_simu$appa)/(1-m3_3_simu$appa))/(m3_3_simu$prior/(1-m3_3_simu$prior))
OCC_3_4<-((m4_3_simu$appa)/(1-m4_3_simu$appa))/(m4_3_simu$prior/(1-m4_3_simu$prior))
OCC_3_5<-((m5_3_simu$appa)/(1-m5_3_simu$appa))/(m5_3_simu$prior/(1-m5_3_simu$prior))
summary(OCC_3_2)
summary(OCC_3_3)
summary(OCC_3_4)
summary(OCC_3_5)

df_wide <- reshape(
  cd,
  direction = "wide",
  idvar = "ID",          # 唯一标识列（如患者ID）
  timevar = "year",      # 时间变量（如2010、2015）
  v.names = c("age","social_isolation","si_class","sleep", "sleep_c","napping","napping_c", "cognition", "depression", 
              "frailty_index", "frailty_class", "event_fra"),  # 需要展开的变量
  sep = "_")


while (!is.null(dev.list())) dev.off()
layout(matrix(1:1, nrow=4, byrow = TRUE))
plot(m2_3_simu,bands = T, 
     title = "Biclassified frailty trajectories",cex.main = 1.5, font.main = 2, 
     xlab = 'years', ylab =  "FI", cex.lab = 1.5)

plot(m3_3_simu,bands = T,
     title = "Triclassified frailty trajectories",cex.main = 1.5, font.main = 2, 
     xlab = 'years', ylab =  "FI", cex.lab = 1.5)

plot(m4_3_simu,bands = T,
     title = "Quad-categorized frailty trajectories",cex.main = 1.5, font.main = 2, 
     xlab = 'years', ylab =  "FI", cex.lab = 1.5)

plot(m5_3_simu,bands = T,
     title = "Five classified frailty trajectories",cex.main = 1.5, font.main = 2, 
     xlab = 'years', ylab =  "FI", cex.lab = 1.5)

##Main Analysis 2: Baseline Characterization-----------------------------------------------------------------------------------------------------------
library(tableone)

df_base_chr<- df_wide%>%
  mutate(
    gender = as.factor(gender),      
    residence = as.factor(residence),
    marital = as.factor(marital),
    education = as.factor(education),
    smoking = as.factor(smoking),
    drinking = as.factor(drinking),
    social_active = as.factor(social_active),
    cognition = cognition_2011,
    sleep_c = as.factor(sleep_c_2011),
    bmi_c = as.factor(bmi_c),
    fra_traj = as.factor(fra_traj),
  ) 
df_base_chr$age <- as.numeric(as.character(df_base_chr$age_2011))
df_base_chr$sleep <- as.numeric(as.character(df_base_chr$sleep_2011))
df_base_chr$cognition <- as.numeric(as.character(df_base_chr$cognition_2011))
df_base_chr$depression <- as.numeric(as.character(df_base_chr$depression_2011))
df_base_chr$frailty_index <- as.numeric(as.character(df_base_chr$frailty_index_2011))

vars <- c("age_2011", "gender", "residence", "education", "marital", "smoking", "drinking","social_active", "bmi_c", "sleep", "sleep_c", "cognition", "depression", "frailty_index")
tableone <- CreateTableOne(vars = vars,
                           data = df_base_chr,
                           strata = "fra_traj", 
                           addOverall = TRUE)

tableone_percent <- print(tableone,
                          showAllLevels = TRUE,
                          printToggle = FALSE,
                          formatOptions = list(
                            categorical = "both",
                            format = "f", 
                            digits = 1, 
                            pDigits = 3 
                          ))

df_export <- as.data.frame(tableone_percent)
df_export$Variable <- rownames(tableone_percent)
rownames(df_export) <- NULL

df_export <- df_export[, c("Variable", colnames(tableone_percent))]

write.csv(df_export, "7.29_BC.csv", row.names = FALSE, na = "")

head(df_export, 6)

##Main analysis 3: Multiple logistic regression analysis-----------------------------------------------------------------------------------------------------------
library(nnet) 
library(caret)
library(dplyr)

df_mlog_reg<- df_wide
df_mlog_reg <- mutate(df_mlog_reg,
                      age_c = case_when(
                        age_2011 < 60 ~ 1,
                        age_2011 >=60 ~ 2,
                        TRUE ~ NA_real_))
df_mlog_reg <- mutate(df_mlog_reg,
                      fra_traj = as.factor(fra_traj),
                      gender = as.factor(gender),      
                      residence = as.factor(residence),
                      marital = as.factor(marital),
                      education = as.factor(education),
                      smoking = as.factor(smoking),
                      drinking = as.factor(drinking),
                      social_active = as.factor(social_active),
                      sleep_c = as.factor(sleep_c_2011),
                      bmi_c = as.factor(bmi_c),
                      frailty_c = as.factor(frailty_class_2011),
                      frailty_eve = as.factor(event_fra_2011),
) 
df_mlog_reg$age <- as.numeric(as.character(df_mlog_reg$age_2011))
df_mlog_reg$cognition <- as.numeric(as.character(df_mlog_reg$cognition_2011))
df_mlog_reg$depression <- as.numeric(as.character(df_mlog_reg$depression_2011))
df_mlog_reg$sleep <- as.numeric(as.character(df_mlog_reg$sleep_2011))


model1 <- multinom(
  fra_traj ~ sleep_c + cognition + depression,
  data = df_mlog_reg,
  MaxNWts = 2000 
)
model2 <- multinom(
  fra_traj ~ sleep_c + cognition + depression + age_c + gender + marital + education + residence + smoking + drinking + bmi_c + social_active,
  data = df_mlog_reg,
  MaxNWts = 2000
)

summary(model1)
summary(model2)

extract_multinom_results <- function(model, model_name) {
  sum_model <- summary(model)

  coefs <- as.data.frame(sum_model$coefficients)
  ses <- as.data.frame(sum_model$standard.errors)

  results <- lapply(1:nrow(coefs), function(i) {
    comparison <- rownames(coefs)[i]
    data.frame(
      Comparison = comparison,
      Variable = colnames(coefs),
      Coef = as.numeric(coefs[i, ]),
      SE = as.numeric(ses[i, ]),
      stringsAsFactors = FALSE
    )
  })

  result_df <- do.call(rbind, results)

  result_df$OR <- exp(result_df$Coef)
  ci_lower <- exp(result_df$Coef - 1.96 * result_df$SE)
  ci_upper <- exp(result_df$Coef + 1.96 * result_df$SE)

  result_df$OR_95CI <- sprintf("%.2f (%.2f~%.2f)", 
                               result_df$OR,
                               ci_lower,
                               ci_upper)
  result_df$P_value <- 2 * pnorm(abs(result_df$Coef / result_df$SE), lower.tail = FALSE)

  result_df$P_value <- format.pval(result_df$P_value, digits = 3, eps = 0.001)
  result_df <- result_df[, c("Comparison", "Variable", "OR_95CI", "P_value")]
  result_df$Model <- model_name
  return(result_df)
}
results_model1 <- extract_multinom_results(model1, "Model1 (Base)")
results_model2 <- extract_multinom_results(model2, "Model2 (Adjusted)")
final_results <- rbind(results_model1, results_model2)

#Main Analysis 4: RCS nonlinearity test-----------------------------------------------------------------------------------------------------------
library(foreign)
library(ggplot2)
library(rms)
library(survival)
library(Hmisc)
library(splines)

df_RCS <- df_wide %>%
  transmute(ID,
            age = age_2011,
            age_c = case_when(
              age_2011 < 60 ~ 1,
              age_2011 >=60 ~ 2,
              TRUE ~ NA_real_),
            gender,
            residence,
            education,
            marital,
            smoking,
            drinking,
            social_active,
            sleep = sleep_2011,
            bmi_c= bmi_c,
            cognition = cognition_2011,
            depression = depression_2011,
            ft_21 = ifelse(fra_traj == 1, 0,
                           ifelse(fra_traj == 2, 1, NA_real_)),
            ft_23 = ifelse(fra_traj == 1, 0,
                           ifelse(fra_traj == 3, 1, NA_real_)),
  )

df_RCS$age_c<-as.factor(df_RCS$age_c)
df_RCS$gender<-as.factor(df_RCS$gender)
df_RCS$residence <- as.factor(df_RCS$residence)
df_RCS$education <- as.factor(df_RCS$education)
df_RCS$marital <- as.factor(df_RCS$marital)
df_RCS$smoking <- as.factor(df_RCS$smoking)
df_RCS$drinking <- as.factor(df_RCS$drinking)
df_RCS$social_active <- as.factor(df_RCS$social_active)
df_RCS$bmi_c <- as.factor(df_RCS$bmi_c)
df_RCS$ft_21 <- as.factor(df_RCS$ft_21)
df_RCS$ft_23 <- as.factor(df_RCS$ft_23)
df_RCS$sleep <- as.numeric(as.character(df_RCS$sleep))
df_RCS$depression <- as.numeric(as.character(df_RCS$depression))
df_RCS$cognition <- as.numeric(as.character(df_RCS$cognition))

df_RCS$age_c<-relevel(df_RCS$age_c, ref=1)
df_RCS$gender<-relevel(df_RCS$gender, ref=1)
df_RCS$education<-relevel(df_RCS$education, ref=1)
df_RCS$marital<-relevel(df_RCS$marital, ref=1)
df_RCS$smoking<-relevel(df_RCS$smoking, ref=1)
df_RCS$drinking<-relevel(df_RCS$drinking, ref=1)
df_RCS$social_active<-relevel(df_RCS$social_active, ref=1)

dd <- datadist(df_RCS)
options(datadist='dd')

###(1)Sleep Duration----
dd$limits$sleep[2]<-7

fit3 <-lrm(ft_23 ~ rcs(sleep,3) + depression + cognition + age_c + gender + residence + education + marital + smoking + drinking + social_active + bmi_c, data = df_RCS)
fit4 <-lrm(ft_23 ~ rcs(sleep,4) + depression + cognition + age_c + gender + residence + education + marital + smoking + drinking + social_active + bmi_c, data = df_RCS)
fit5 <-lrm(ft_23 ~ rcs(sleep,5) + depression + cognition + age_c + gender + residence + education + marital + smoking + drinking + social_active + bmi_c, data = df_RCS)
AIC(fit3)
AIC(fit4)
AIC(fit5)

an<-anova(fit3)
p_overall <- an[rownames(an) == "sleep", "P"]
p_nonlinear <- an[grep("Nonlinear", rownames(an)), "P"]
p_overall_text <- ifelse(p_overall < 0.001, "P-overall < 0.001", 
                         paste("P-overall =", format(p_overall, digits = 3)))
p_nonlinear_text <- ifelse(p_nonlinear < 0.001, "P-nonlinear < 0.001", 
                           paste("P-nonlinear =", format(p_nonlinear, digits = 3)))

plot(Predict(fit3, sleep, fun=exp,ref.zero = TRUE), anova=an, pval=T)
HR<-Predict(fit3, sleep, fun=exp,ref.zero = TRUE) 

ggplot(HR,anova=an, pval=T)
P1 <- ggplot(anova = an, pval =T) +
  geom_line(data = HR, aes(sleep, yhat),linetype=1,size=1, alpha=0.9, colour="red")+ 
  geom_ribbon(data = HR, aes(sleep,ymin = lower, ymax = upper),alpha = 0.3,fill="red")+ 
  geom_vline(aes(xintercept=7), colour="#BB0000", linetype="dashed")+ 
  geom_hline(yintercept=1, linetype=2,size=1)+
  annotate("text", x = 2, y = 4.5, label = p_overall_text, hjust = 0, size = 3) +
  annotate("text", x = 2, y = 4.25, label = p_nonlinear_text, hjust = 0, size = 3)+
  theme_classic()+  
  labs(title = "RSC of sleep duration and medium frailty risk trajectory", x="Sleep Duration", y="OR (95%CI)")+  
  scale_x_continuous(limits = c(0, 13), 
                     breaks = seq(0,13,1))+ 
  scale_y_continuous(limits = c(0, 5),
                     breaks = seq(0,5,0.5))+ 
  geom_text(aes(x=7,y=1,label='sleep = 7'),            
            vjust=1.5,hjust=0,size=2.5)
P1


###(2)Depression----
dd$limits$depression[2]<-10

fit3 <-lrm(ft_21 ~ rcs(depression,3) + sleep + cognition + age_c + gender + residence + education + marital + smoking + drinking + social_active + bmi_c, data = df_RCS)
fit4 <-lrm(ft_21 ~ rcs(depression,4) + sleep + cognition + age_c + gender + residence + education + marital + smoking + drinking + social_active + bmi_c, data = df_RCS)
fit5 <-lrm(ft_2 ~ rcs(depression,5) + sleep + cognition + age_c + gender + residence + education + marital + smoking + drinking + social_active + bmi_c, data = df_RCS)
AIC(fit3)
AIC(fit4)
AIC(fit5)

an<-anova(fit3)
p_overall <- an[1, "P"]
p_nonlinear <- an[2, "P"]
p_overall_text <- ifelse(p_overall < 0.001, "P-overall < 0.001", 
                         paste("P-overall =", format(p_overall, digits = 3)))
p_nonlinear_text <- ifelse(p_nonlinear < 0.001, "P-nonlinear < 0.001", 
                           paste("P-nonlinear =", format(p_nonlinear, digits = 3)))

plot(Predict(fit3, depression, fun=exp,ref.zero = TRUE), anova=an, pval=T)
HR<-Predict(fit3, depression, fun=exp,ref.zero = TRUE) #运行完这句后，查看数据HR

ggplot(HR,anova=an, pval=T)
P1 <- ggplot(anova = an, pval =T) +
  geom_line(data = HR, aes(depression, yhat),linetype=1,size=1, alpha=0.9, colour="blue")+ 
  geom_ribbon(data = HR, aes(depression,ymin = lower, ymax = upper),alpha = 0.3,fill="blue")+ 
  geom_vline(aes(xintercept=10), colour="#BB0000", linetype="dashed")+ 
  geom_hline(yintercept=1, linetype=2,size=1)+
  annotate("text", x = 2, y = 38, label = p_overall_text, hjust = 0, size = 3) +
  annotate("text", x = 2, y = 36.5, label = p_nonlinear_text, hjust = 0, size = 3)+
  theme_classic()+  
  labs(title = "RSC of depression and high frailty risk trajectory", x="Depression Score", y="OR (95%CI)")+  
  scale_x_continuous(limits = c(0, 30), 
                     breaks = seq(0,30,2))+
  scale_y_continuous(limits = c(0, 42),
                     breaks = seq(0,42,2))+
  geom_text(aes(x=10.5,y=1,label='CESD-10 = 10'),            
            vjust=1.5,hjust=0,size=2.5)
P1

###(3)Cognition Function----
dd$limits$cognition[2]<-13

fit3 <-lrm(ft_23 ~ rcs(cognition,3) + sleep + depression + age_c + gender + residence + education + marital + smoking + drinking + social_active + bmi_c, data = df_RCS)
fit4 <-lrm(ft_23 ~ rcs(cognition,4) + sleep + depression + age_c + gender + residence + education + marital + smoking + drinking + social_active + bmi_c, data = df_RCS)
fit5 <-lrm(ft_23 ~ rcs(cognition,5) + sleep + depression + age_c + gender + residence + education + marital + smoking + drinking + social_active + bmi_c, data = df_RCS)
AIC(fit3)
AIC(fit4)
AIC(fit5)

an<-anova(fit3)
p_overall <- an[1, "P"]
p_nonlinear <- an[2, "P"]
p_overall_text <- ifelse(p_overall < 0.001, "P-overall < 0.001", 
                         paste("P-overall =", format(p_overall, digits = 3)))
p_nonlinear_text <- ifelse(p_nonlinear < 0.001, "P-nonlinear < 0.001", 
                           paste("P-nonlinear =", format(p_nonlinear, digits = 3)))

plot(Predict(fit3, cognition, fun=exp,ref.zero = TRUE), anova=an, pval=T)
HR<-Predict(fit3, cognition, fun=exp,ref.zero = TRUE)

ggplot(HR,anova=an, pval=T)
P1 <- ggplot(anova = an, pval =T) +
  geom_line(data = HR, aes(cognition, yhat),linetype=1,size=1, alpha=0.9, colour="orange")+ 
  geom_ribbon(data = HR, aes(cognition,ymin = lower, ymax = upper),alpha = 0.3,fill="orange")+ 
  geom_vline(aes(xintercept=13), colour="#BB0000", linetype="dashed")+ 
  geom_hline(yintercept=1, linetype=2,size=1)+
  annotate("text", x = 2, y = 2.75, label = p_overall_text, hjust = 0, size = 3) +
  annotate("text", x = 2, y = 2.6, label = p_nonlinear_text, hjust = 0, size = 3)+
  theme_classic()+  
  labs(title = "RSC of cognition and high frailty risk trajectory", x="Cognition Score", y="OR (95%CI)")+  
  scale_x_continuous(limits = c(0, 31), 
                     breaks = seq(0,31,2))+
  scale_y_continuous(limits = c(0, 3),
                     breaks = seq(0,3,0.5))+
  geom_text(aes(x=6,y=1,label='Cognition score = 13'),            
            vjust=1.5,hjust=0,size=2.5)
P1

df <- mutate(df,
             D1 = ifelse(sleep_c == 0, 0,
                         ifelse(sleep_c == 1, 1, NA_real_)),
             D2 = ifelse(sleep_c == 0, 0,
                         ifelse(sleep_c == 2, 1, NA_real_)))

#Main analysis 5: Subgroup analysis: stratification based on sleep duration-----------------------------------------------------------------------------------------------------------
library(broom)
df_mlog_reg <- mlog_sle_cog_dep_fra
df_mlog_reg<- df_mlog_reg %>%
  mutate(
    fra_traj = as.factor(fra_traj),
    gender = as.factor(gender),      
    residence = as.factor(residence),
    marital = as.factor(marital),
    education = as.factor(education),
    smoking = as.factor(smoking),
    drinking = as.factor(drinking),
    social_active = as.factor(social_active),
    sleep_c = as.factor(sleep_c_2011),
    bmi_c = as.factor(bmi_c),
    frailty_c = as.factor(frailty_class_2011)
  ) 
df_mlog_reg$age <- as.numeric(as.character(df_mlog_reg$age_2011))
df_mlog_reg$cognition <- as.numeric(as.character(df_mlog_reg$cognition_2011))
df_mlog_reg$depression <- as.numeric(as.character(df_mlog_reg$depression_2011))
df_mlog_reg$sleep <- as.numeric(as.character(df_mlog_reg$sleep_2011))

df_sle1 <- subset(df_mlog_reg, sleep_c == 0)
df_sle2 <- subset(df_mlog_reg, sleep_c == 1)
df_sle3 <- subset(df_mlog_reg, sleep_c == 2) 

model_sle1 <- multinom(
  fra_traj ~ cognition + depression + age_c + gender + marital + 
    education + residence + smoking + drinking + bmi_c + social_active,
  data = df_sle1,
  MaxNWts = 1000)

model_sle2 <- multinom(
  fra_traj ~  cognition + depression + age_c + gender + marital + 
    education + residence + smoking + drinking + bmi_c + social_active,
  data = df_sle2,
  MaxNWts = 1000)

model_sle3 <- multinom(
  fra_traj ~  cognition + depression + age + gender + marital + 
    education + residence + smoking + drinking + bmi_c + social_active,
  data = df_sle3,
  MaxNWts = 1000)

extract_multinom_results <- function(model, model_name) {
  sum_model <- summary(model)

  coefs <- as.data.frame(sum_model$coefficients)
  ses <- as.data.frame(sum_model$standard.errors)

  results <- lapply(1:nrow(coefs), function(i) {
    comparison <- rownames(coefs)[i]
    data.frame(
      Comparison = comparison,
      Variable = colnames(coefs),
      Coef = as.numeric(coefs[i, ]),
      SE = as.numeric(ses[i, ]),
      stringsAsFactors = FALSE
    )
  })

  result_df <- do.call(rbind, results)

  result_df$OR <- exp(result_df$Coef)
  ci_lower <- exp(result_df$Coef - 1.96 * result_df$SE)
  ci_upper <- exp(result_df$Coef + 1.96 * result_df$SE)

  result_df$OR_95CI <- sprintf("%.3f (%.3f~%.3f)", 
                               result_df$OR,
                               ci_lower,
                               ci_upper)
  result_df$P_value <- 2 * pnorm(abs(result_df$Coef / result_df$SE), lower.tail = FALSE)

  result_df$P_value <- format.pval(result_df$P_value, digits = 3, eps = 0.001)
  result_df <- result_df[, c("Comparison", "Variable", "OR_95CI", "P_value")]
  result_df$Model <- model_name
  return(result_df)
}

results_model1 <- extract_multinom_results(model_sle1, "Model2_sle1")
results_model2 <- extract_multinom_results(model_sle2, "Model2_sle2")
results_model3 <- extract_multinom_results(model_sle3, "Model2_sle3")

results_sle <- rbind(results_model1, results_model2, results_model3)

#Main analysis 6: Mediated effects analysis-----------------------------------------------------------------------------------------------------------
 ##Note: Analyzed using SPSS 23.0


#Sensitivity Analysis############################################################################################################################
##Sensitivity Analysis 1: Interaction Effect Tests-----------------------------------------------------------------------------------------------------------

library(car)

df$cognition <- scale(df$cognition, center = TRUE, scale = FALSE)
df$depression <- scale(df$depression, center = TRUE, scale = FALSE)


model_full <- nnet::multinom(
  fra_traj ~ sleep_c + cognition * depression + age_c + gender + marital + 
    education + residence + smoking + drinking + bmi_c + social_active,
  data = df,
  MaxNWts = 1000 
)

model_reduced <- nnet::multinom(
  fra_traj ~ sleep_c + cognition + depression + age_c + gender + marital + 
    education + residence + smoking + drinking + bmi_c + social_active,
  data = df,
  MaxNWts = 1000
)

lrt_result <- anova(model_reduced, model_full)

print(lrt_result)

##Sensitivity Analysis 2: subgroup analysis-----------------------------------------------------------------------------------------------------------
library(broom)
library(nnet) 
library(caret) 

df_sub <- df %>%
  transmute(
    age_c, gender, residence, education, marital, smoking, drinking, social_active, bmi_c,
    sleep_c, cognition, depression, fra_traj
  )
df_sub<- df_sub %>%
  mutate(
    fra_traj = as.factor(fra_traj),
    age_c = as.factor(age_c),
    gender = as.factor(gender),      
    residence = as.factor(residence),
    marital = as.factor(marital),
    education = as.factor(education),
    smoking = as.factor(smoking),
    drinking = as.factor(drinking),
    social_active = as.factor(social_active),
    sleep_c = as.factor(sleep_c),
    bmi_c = as.factor(bmi_c),
    cognition = as.numeric(as.character(df_sub$cognition)),
    depression = as.numeric(as.character(df_sub$depression))
    )

##1.Age----

df_age1 <- subset(df_sub, age_c == 1)
df_age2 <- subset(df_sub, age_c == 2)

model_age1 <- multinom(
  fra_traj ~ sleep_c + cognition + depression + gender + marital + 
    education + residence + smoking + drinking + bmi_c + social_active,
  data = df_age1,
  MaxNWts = 1000)

model_age2 <- multinom(
  fra_traj ~ sleep_c + cognition + depression + gender + marital + 
    education + residence + smoking + drinking + bmi_c + social_active,
  data = df_age2,
  MaxNWts = 1000)

extract_multinom_results <- function(model, model_name) {
  sum_model <- summary(model)

  coefs <- as.data.frame(sum_model$coefficients)
  ses <- as.data.frame(sum_model$standard.errors)

  results <- lapply(1:nrow(coefs), function(i) {
    comparison <- rownames(coefs)[i]
    data.frame(
      Comparison = comparison,
      Variable = colnames(coefs),
      Coef = as.numeric(coefs[i, ]),
      SE = as.numeric(ses[i, ]),
      stringsAsFactors = FALSE
    )
  })

  result_df <- do.call(rbind, results)

  result_df$OR <- exp(result_df$Coef)
  ci_lower <- exp(result_df$Coef - 1.96 * result_df$SE)
  ci_upper <- exp(result_df$Coef + 1.96 * result_df$SE)

  result_df$OR_95CI <- sprintf("%.3f (%.3f~%.3f)", 
                               result_df$OR,
                               ci_lower,
                               ci_upper)
  result_df$P_value <- 2 * pnorm(abs(result_df$Coef / result_df$SE), lower.tail = FALSE)

  result_df$P_value <- format.pval(result_df$P_value, digits = 3, eps = 0.001)
  result_df <- result_df[, c("Comparison", "Variable", "OR_95CI", "P_value")]
  result_df$Model <- model_name
  return(result_df)
}

results_model1 <- extract_multinom_results(model_age1, "Model2_age1")
results_model2 <- extract_multinom_results(model_age2, "Model2_age2")
results_age <- rbind(results_model1, results_model2)

write.csv(results_age, "9.10_sub_age.csv", 
          row.names = FALSE, 
          fileEncoding = "UTF-8")


##2.Gender----
df_sex1 <- subset(df_sub, gender == 1)
df_sex2 <- subset(df_sub, gender == 2)

model_sex1 <- multinom(
  fra_traj ~ sleep_c + cognition + depression + age_c + marital + 
    education + residence + smoking + drinking + bmi_c + social_active,
  data = df_sex1,
  MaxNWts = 1000)

model_sex2 <- multinom(
  fra_traj ~ sleep_c + cognition + depression + age_c + marital + 
    education + residence + smoking + drinking + bmi_c + social_active,
  data = df_sex2,
  MaxNWts = 1000)

extract_multinom_results <- function(model, model_name) {
  sum_model <- summary(model)

  coefs <- as.data.frame(sum_model$coefficients)
  ses <- as.data.frame(sum_model$standard.errors)

  results <- lapply(1:nrow(coefs), function(i) {
    comparison <- rownames(coefs)[i]
    data.frame(
      Comparison = comparison,
      Variable = colnames(coefs),
      Coef = as.numeric(coefs[i, ]),
      SE = as.numeric(ses[i, ]),
      stringsAsFactors = FALSE
    )
  })

  result_df <- do.call(rbind, results)

  result_df$OR <- exp(result_df$Coef)
  ci_lower <- exp(result_df$Coef - 1.96 * result_df$SE)
  ci_upper <- exp(result_df$Coef + 1.96 * result_df$SE)

  result_df$OR_95CI <- sprintf("%.3f (%.3f~%.3f)", 
                               result_df$OR,
                               ci_lower,
                               ci_upper)
  result_df$P_value <- 2 * pnorm(abs(result_df$Coef / result_df$SE), lower.tail = FALSE)

  result_df$P_value <- format.pval(result_df$P_value, digits = 3, eps = 0.001)
  result_df <- result_df[, c("Comparison", "Variable", "OR_95CI", "P_value")]
  result_df$Model <- model_name
  return(result_df)
}
results_model1 <- extract_multinom_results(model_sex1, "Model2_sex1")
results_model2 <- extract_multinom_results(model_sex2, "Model2_sex2")

results_sex <- rbind(results_model1, results_model2)

write.csv(results_sex, "9.10_sub_sex.csv", 
          row.names = FALSE, 
          fileEncoding = "UTF-8")

##3.Residence----

df_res1 <- subset(df_sub, residence == 1)
df_res2 <- subset(df_sub, residence == 2)

model_res1 <- multinom(
  fra_traj ~ sleep_c + cognition + depression + age_c + gender + marital + 
    education + smoking + drinking + bmi_c + social_active,
  data = df_res1,
  MaxNWts = 1000)

model_res2 <- multinom(
  fra_traj ~ sleep_c + cognition + depression + age_c + gender + marital + 
    education + smoking + drinking + bmi_c + social_active,
  data = df_res2,
  MaxNWts = 1000)

extract_multinom_results <- function(model, model_name) {
  sum_model <- summary(model)

  coefs <- as.data.frame(sum_model$coefficients)
  ses <- as.data.frame(sum_model$standard.errors)

  results <- lapply(1:nrow(coefs), function(i) {
    comparison <- rownames(coefs)[i]
    data.frame(
      Comparison = comparison,
      Variable = colnames(coefs),
      Coef = as.numeric(coefs[i, ]),
      SE = as.numeric(ses[i, ]),
      stringsAsFactors = FALSE
    )
  })

  result_df <- do.call(rbind, results)

  result_df$OR <- exp(result_df$Coef)
  ci_lower <- exp(result_df$Coef - 1.96 * result_df$SE)
  ci_upper <- exp(result_df$Coef + 1.96 * result_df$SE)

  result_df$OR_95CI <- sprintf("%.3f (%.3f~%.3f)", 
                               result_df$OR,
                               ci_lower,
                               ci_upper)
  result_df$P_value <- 2 * pnorm(abs(result_df$Coef / result_df$SE), lower.tail = FALSE)

  result_df$P_value <- format.pval(result_df$P_value, digits = 3, eps = 0.001)
  result_df <- result_df[, c("Comparison", "Variable", "OR_95CI", "P_value")]
  result_df$Model <- model_name
  return(result_df)
}

results_model1 <- extract_multinom_results(model_res1, "Model2_res1")
results_model2 <- extract_multinom_results(model_res2, "Model2_res2")

results_res <- rbind(results_model1, results_model2)

write.csv(results_res, "9.10_sub_res.csv", 
          row.names = FALSE, 
          fileEncoding = "UTF-8")


##4.Marital status----
df_mar1 <- subset(df_sub, marital == 1)
df_mar2 <- subset(df_sub, marital == 2)

model_mar1 <- multinom(
  fra_traj ~ sleep_c + cognition + depression + age_c + gender + residence + 
    education + smoking + drinking + bmi_c + social_active,
  data = df_mar1,
  MaxNWts = 1000)

model_mar2 <- multinom(
  fra_traj ~ sleep_c + cognition + depression + age_c + gender + residence + 
    education + smoking + drinking + bmi_c + social_active,
  data = df_mar2,
  MaxNWts = 1000)

extract_multinom_results <- function(model, model_name) {
  sum_model <- summary(model)

  coefs <- as.data.frame(sum_model$coefficients)
  ses <- as.data.frame(sum_model$standard.errors)

  results <- lapply(1:nrow(coefs), function(i) {
    comparison <- rownames(coefs)[i]
    data.frame(
      Comparison = comparison,
      Variable = colnames(coefs),
      Coef = as.numeric(coefs[i, ]),
      SE = as.numeric(ses[i, ]),
      stringsAsFactors = FALSE
    )
  })

  result_df <- do.call(rbind, results)

  result_df$OR <- exp(result_df$Coef)
  ci_lower <- exp(result_df$Coef - 1.96 * result_df$SE)
  ci_upper <- exp(result_df$Coef + 1.96 * result_df$SE)
  # 创建合并列（保留两位小数）
  result_df$OR_95CI <- sprintf("%.3f (%.3f~%.3f)", 
                               result_df$OR,
                               ci_lower,
                               ci_upper)
  result_df$P_value <- 2 * pnorm(abs(result_df$Coef / result_df$SE), lower.tail = FALSE)

  result_df$P_value <- format.pval(result_df$P_value, digits = 3, eps = 0.001)
  result_df <- result_df[, c("Comparison", "Variable", "OR_95CI", "P_value")]
  result_df$Model <- model_name
  return(result_df)
}

results_model1 <- extract_multinom_results(model_mar1, "Model2_mar1")
results_model2 <- extract_multinom_results(model_mar2, "Model2_mar2")

results_mar <- rbind(results_model1, results_model2)

write.csv(results_mar, "9.10_sub_mar.csv", 
          row.names = FALSE, 
          fileEncoding = "UTF-8")

##5.Educational level----
df_edu1 <- subset(df_sub, education == 1)
df_edu2 <- subset(df_sub, education == 2)
df_edu3 <- subset(df_sub, education == 3)
df_edu4 <- subset(df_sub, education == 4)

model_edu1 <- multinom(
  fra_traj ~ sleep_c + cognition + depression + age_c + gender + residence + 
    marital + smoking + drinking + bmi_c + social_active,
  data = df_edu1,
  MaxNWts = 1000)

model_edu2 <- multinom(
  fra_traj ~ sleep_c + cognition + depression + age_c + gender + residence + 
    marital + smoking + drinking + bmi_c + social_active,
  data = df_edu2,
  MaxNWts = 1000)

model_edu3 <- multinom(
  fra_traj ~ sleep_c + cognition + depression + age_c + gender + residence + 
    marital + smoking + drinking + bmi_c + social_active,
  data = df_edu3,
  MaxNWts = 1000)

model_edu4 <- multinom(
  fra_traj ~ sleep_c + cognition + depression + age_c + gender + residence + 
    marital + smoking + drinking + bmi_c + social_active,
  data = df_edu4,
  MaxNWts = 1000)

extract_multinom_results <- function(model, model_name) {
  sum_model <- summary(model)

  coefs <- as.data.frame(sum_model$coefficients)
  ses <- as.data.frame(sum_model$standard.errors)

  results <- lapply(1:nrow(coefs), function(i) {
    comparison <- rownames(coefs)[i]
    data.frame(
      Comparison = comparison,
      Variable = colnames(coefs),
      Coef = as.numeric(coefs[i, ]),
      SE = as.numeric(ses[i, ]),
      stringsAsFactors = FALSE
    )
  })

  result_df <- do.call(rbind, results)

  result_df$OR <- exp(result_df$Coef)
  ci_lower <- exp(result_df$Coef - 1.96 * result_df$SE)
  ci_upper <- exp(result_df$Coef + 1.96 * result_df$SE)

  result_df$OR_95CI <- sprintf("%.3f (%.3f~%.3f)", 
                               result_df$OR,
                               ci_lower,
                               ci_upper)
  result_df$P_value <- 2 * pnorm(abs(result_df$Coef / result_df$SE), lower.tail = FALSE)
  # 优化显示
  result_df$P_value <- format.pval(result_df$P_value, digits = 3, eps = 0.001)
  result_df <- result_df[, c("Comparison", "Variable", "OR_95CI", "P_value")]
  result_df$Model <- model_name
  return(result_df)
}

results_model1 <- extract_multinom_results(model_edu1, "Model2_edu1")
results_model2 <- extract_multinom_results(model_edu2, "Model2_edu2")
results_model3 <- extract_multinom_results(model_edu3, "Model2_edu3")
results_model4 <- extract_multinom_results(model_edu4, "Model2_edu4")

results_edu <- rbind(results_model1, results_model2, results_model3, results_model4)

write.csv(results_edu, "9.10_sub_edu.csv", 
          row.names = FALSE, 
          fileEncoding = "UTF-8")

# Sensitivity Analysis 3: Mediated Path Robustness Analysis-----------------------------------------------------------------------------------------------------------
  ##Note: Analyzed using SPSS 23.0
  ##Analysis 1: Chained intermediary path swaps
  ##Analysis 2: Intermediary Model Replacement: Chain -> Parallel
  ##Analysis 3: Increase in bootstrap sample size: 5000 -> 10000
