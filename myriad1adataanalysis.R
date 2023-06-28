# Setup ----

install.packages("ggpubr")
install.packages("broom")
install.packages("Hmisc")
install.packages("corrplot")
install.packages("car")
install.packages("tmvnsim")
install.packages("psych")
install.packages("moments")
library("ggpubr")
library("tidyverse")
library("broom")
library("Hmisc")
library("corrplot")
library("car")
library("tmvnsim")
library("psych")
library("moments")

rm(list = ls())
data <- read.csv(here::here("./Linear Models/raw_data/rdataset.csv")) # Loads in data

################################################################################
####################  Data Preparation #########################################
################################################################################
# Managing Missing Data ----
list_na <- colnames(data)[ apply(data, 2, anyNA) ]

# Creates means for each variable
average_missing <- apply(data[,colnames(data) %in% list_na], 
                                            2,
                                            mean,
                                            na.rm =  TRUE)

# Substitute NA for Means, and create Indices for Tasks
data_clean <- data %>%
  mutate(genderC  = ifelse(is.na(gender), average_missing[1], gender),
         ageC = ifelse(is.na(age), average_missing[2], age),
         CAMM1C = ifelse(is.na(CAMM1), average_missing[3], CAMM1),
         CAMM2C = ifelse(is.na(CAMM2), average_missing[4], CAMM2),
         CAMM5C = ifelse(is.na(CAMM5), average_missing[5], CAMM5),
         CAMM7C = ifelse(is.na(CAMM7), average_missing[6], CAMM7),
         CAMM9C = ifelse(is.na(CAMM9), average_missing[7], CAMM9),                         
         CAMM10C = ifelse(is.na(CAMM10), average_missing[8], CAMM10),                                                    
         DERS30C = ifelse(is.na(DERS30), average_missing[9], DERS30),
         DERS31C = ifelse(is.na(DERS31), average_missing[10], DERS31),
         DERS32C = ifelse(is.na(DERS32), average_missing[11], DERS32),
         DERS33C = ifelse(is.na(DERS33), average_missing[12], DERS33),
         DERS36C = ifelse(is.na(DERS36), average_missing[13], DERS36),
         totalscoreC = ifelse(is.na(totalscore), average_missing[14], totalscore),
         cesdC = ifelse(is.na(cesd), average_missing[15], cesd),
         rcadsC = ifelse(is.na(rcads), average_missing[16], rcads),
         wemwbsC = ifelse(is.na(wemwbs), average_missing[17], wemwbs),
         social_absdiffC = ifelse(is.na(social_R2R1_diff_abs), average_missing[59], social_R2R1_diff_abs),
         dictator_percC = ifelse(is.na(dictator_perc), average_missing[62], dictator_perc),
         delay_discount_percC = ifelse(is.na(delay_discount_perc), average_missing[61], delay_discount_perc),
         sunk_cost_sumC = ifelse(is.na(sunk_cost_sum), average_missing[63], sunk_cost_sum),
         WM_T1_ATDR_percentage_NegC = ifelse(is.na(WM_T1_ATDR_percentage_Neg), average_missing[53], WM_T1_ATDR_percentage_Neg),
         WM_T1_ATDR_percentage_neuC = ifelse(is.na(WM_T1_ATDR_percentage_neu), average_missing[55], WM_T1_ATDR_percentage_neu),
         SART_COM_NEGC = ifelse(is.na(SART_COM_NEG), average_missing[22], SART_COM_NEG),
         SART_COM_NEUC = ifelse(is.na(SART_COM_NEU), average_missing[23], SART_COM_NEU),
         SART_RT_VAR_NEGC = ifelse(is.na(SART_RT_VAR_NEG), average_missing[24], SART_RT_VAR_NEG),
         SART_RT_VAR_NEUC = ifelse(is.na(SART_RT_VAR_NEU), average_missing[25], SART_RT_VAR_NEU),
         Stroop_con_RT_SADC = ifelse(is.na(Stroop_con_RT_SAD), average_missing[35], Stroop_con_RT_SAD),
         Stroop_incon_RT_SADC = ifelse(is.na(Stroop_incon_RT_SAD), average_missing[37], Stroop_incon_RT_SAD),
         Stroop_neu_RT_SADC = ifelse(is.na(Stroop_neu_RT_SAD), average_missing[36], Stroop_neu_RT_SAD),
         Stroop_Neu_RTC = ifelse(is.na(Stroop_Neu_RT), average_missing[28], Stroop_Neu_RT),
         Stroop_Neu_ACCC = ifelse(is.na(Stroop_Neu_ACC), average_missing[34], Stroop_Neu_ACC),
         Stroop_RT_var_neuC = ifelse(is.na(Stroop_RT_var_neu), average_missing[36], Stroop_RT_var_neu),
         EWMI = WM_T1_ATDR_percentage_NegC - WM_T1_ATDR_percentage_neuC,
         SART_COM_I = SART_COM_NEGC - SART_COM_NEUC,
         SART_RT_VAR_I = SART_RT_VAR_NEGC - SART_RT_VAR_NEUC,
         stroop_neupos = Stroop_incon_RT_SADC - Stroop_neu_RT_SADC,
         stroop_negneu = Stroop_con_RT_SADC - Stroop_neu_RT_SADC,
         stroop_negpos = Stroop_incon_RT_SADC - Stroop_con_RT_SADC,
         dummy_agedec = ageC * totalscoreC,
         dummy_gendec = genderC * totalscoreC)

# Subsets the Data taking only what we need
dataset <- select(data_clean, genderC, ageC, totalscoreC, cesdC, rcadsC, wemwbsC, EWMI, SART_COM_I, SART_RT_VAR_I, 
                  social_absdiffC, dictator_percC, delay_discount_percC, sunk_cost_sumC, 
                  stroop_neupos, stroop_negneu, stroop_negpos, dummy_agedec,dummy_gendec, gender)

dataset$gaf <- factor(dataset$gender)

################################################################################
#################### Normality Testing #########################################
################################################################################
# Decentering ----

ggplot(dataset, aes(`totalscoreC`)) +
  geom_histogram(aes(y = ..density..), colour = "black", fill = "white") +
  stat_function(fun = dnorm, args = list(mean = mean(dataset$totalscoreC, na.rm = T), 
                                         sd = sd(dataset$totalscoreC, na.rm = T))) +
  xlab("Decentering")

shapiro.test(dataset$totalscoreC)
skew(dataset$totalscoreC)
kurtosis(dataset$totalscoreC)

# Square transformation
totalscoreC_trans <- dataset$totalscoreC^2

skew(totalscoreC_trans) # data now better, use totalscoreC_trans for analysis
shapiro.test(totalscoreC_trans)

# Quick Check
mean(totalscoreC_trans[dataset$genderC==1])
mean(totalscoreC_trans[dataset$genderC==0])

ggplot(dataset, aes(`totalscoreC__trans`)) +
  geom_histogram(aes(y = ..density..), colour = "black", fill = "white") +
  stat_function(fun = dnorm, args = list(mean = mean(totalscoreC__trans, na.rm = T), 
                                         sd = sd(totalscoreC__trans, na.rm = T))) +
  xlab("Decentering")

# Depression ----

ggplot(dataset, aes(`cesdC`)) +
  geom_histogram(aes(y = ..density..), colour = "black", fill = "white") +
  stat_function(fun = dnorm, args = list(mean = mean(dataset$cesdC, na.rm = T), 
                                         sd = sd(dataset$cesdC, na.rm = T))) +
  xlab("Depression")

shapiro.test(dataset$cesdC)
skew(dataset$cesdC) # needs to be transformed

cesdC_trans <- sqrt(dataset$cesdC) # log transforms cesdC
skew(cesdC_trans) # fixed, use new variable

# Anxiety ----

ggplot(dataset, aes(`rcadsC`)) +
  geom_histogram(aes(y = ..density..), colour = "black", fill = "white") +
  stat_function(fun = dnorm, args = list(mean = mean(dataset$rcadsC, na.rm = T), 
                                         sd = sd(dataset$rcadsC, na.rm = T))) +
  xlab("Anxiety")

shapiro.test(dataset$rcadsC)
skew(dataset$rcadsC) # needs to be transformed

rcadsC_trans <- sqrt(dataset$rcadsC)
skew(rcadsC_trans) # now fixed, use rcadsC_trans for analysis

# Wellbeing ----

ggplot(dataset, aes(`wemwbsC`)) +
  geom_histogram(aes(y = ..density..), colour = "black", fill = "white") +
  stat_function(fun = dnorm, args = list(mean = mean(dataset$wemwbsC, na.rm = T), 
                                         sd = sd(dataset$wemwbsC, na.rm = T))) +
  xlab("Wellbeing")

shapiro.test(dataset$wemwbsC)
skew(dataset$wemwbsC)  # no transformation needed

# EWMI ----
ggplot(dataset, aes(`EWMI`)) +
  geom_histogram(aes(y = ..density..), colour = "black", fill = "white") +
  stat_function(fun = dnorm, args = list(mean = mean(dataset$EWMI, na.rm = T), 
                                         sd = sd(dataset$EWMI, na.rm = T))) +
  xlab("EWMI")

shapiro.test(dataset$EWMI)
skew(dataset$EWMI) # no transformation needed 

# Sart Commission ----
ggplot(dataset, aes(`SART_COM_I`)) +
  geom_histogram(aes(y = ..density..), colour = "black", fill = "white") +
  stat_function(fun = dnorm, args = list(mean = mean(dataset$SART_COM_I, na.rm = T), 
                                         sd = sd(dataset$SART_COM_I, na.rm = T))) +
  xlab("SART_COM_I")

shapiro.test(dataset$SART_COM_I)
skew(dataset$SART_COM_I) # no transformation needed

# SART RT Variance ----
ggplot(dataset, aes(`SART_RT_VAR_I`)) +
  geom_histogram(aes(y = ..density..), colour = "black", fill = "white") +
  stat_function(fun = dnorm, args = list(mean = mean(dataset$SART_RT_VAR_I, na.rm = T), 
                                         sd = sd(dataset$SART_RT_VAR_I, na.rm = T))) +
  xlab("SART_RT_VAR_I")

shapiro.test(dataset$SART_RT_VAR_I)
skew(dataset$SART_RT_VAR_I) # data needs to be transformed

SARTRT_trans <- log1p(dataset$SART_RT_VAR_I)
skew(SARTRT_trans) # fixed, use transformed data

# Stroop NegNeu ----
ggplot(dataset, aes(`stroop_negneu`)) +
  geom_histogram(aes(y = ..density..), colour = "black", fill = "white") +
  stat_function(fun = dnorm, args = list(mean = mean(dataset$stroop_negneu, na.rm = T), 
                                         sd = sd(dataset$stroop_negneu, na.rm = T))) +
  xlab("stroop_negneu")

shapiro.test(dataset$stroop_negneu)
skew(dataset$stroop_negneu) # needs to be transformed

stroop_negneu_trans <- log10(dataset$stroop_negneu)
skew(stroop_negneu_trans) # fixed, use this variable

# Stroop NegPos ----
ggplot(dataset, aes(`stroop_negpos`)) +
  geom_histogram(aes(y = ..density..), colour = "black", fill = "white") +
  stat_function(fun = dnorm, args = list(mean = mean(dataset$stroop_negpos, na.rm = T), 
                                         sd = sd(dataset$stroop_negpos, na.rm = T))) +
  xlab("stroop_negpos")

shapiro.test(dataset$stroop_negpos)
skew(dataset$stroop_negpos)

stroop_negpos_trans <- log1p(dataset$stroop_negpos)
skew(stroop_negpos_trans) # use ordinal regression, all transformations make worse

# Stroop PosNeu ----
ggplot(dataset, aes(`stroop_neupos`)) +
  geom_histogram(aes(y = ..density..), colour = "black", fill = "white") +
  stat_function(fun = dnorm, args = list(mean = mean(dataset$stroop_neupos, na.rm = T), 
                                         sd = sd(dataset$stroop_neupos, na.rm = T))) +
  xlab("stroop_neupos")

shapiro.test(dataset$stroop_neupos)
skew(dataset$stroop_neupos) # no transformation needed

# Social AbsDiff ----
ggplot(dataset, aes(`social_absdiffC`)) +
  geom_histogram(aes(y = ..density..), colour = "black", fill = "white") +
  stat_function(fun = dnorm, args = list(mean = mean(dataset$social_absdiffC, na.rm = T), 
                                         sd = sd(dataset$social_absdiffC, na.rm = T))) +
  xlab("social_absdiffC")

shapiro.test(dataset$social_absdiffC)
skew(dataset$social_absdiffC) # severely skewed

socialdiff_trans <- log1p(dataset$social_absdiffC)
skew(socialdiff_trans) # brought skew down a lot but still need ordinal regression

# Dictator ----
ggplot(dataset, aes(`dictator_percC`)) +
  geom_histogram(aes(y = ..density..), colour = "black", fill = "white") +
  stat_function(fun = dnorm, args = list(mean = mean(dataset$dictator_percC, na.rm = T), 
                                         sd = sd(dataset$dictator_percC, na.rm = T))) +
  xlab("dictator_percC")

shapiro.test(dataset$dictator_percC)
skew(dataset$dictator_percC) # no transformation needed

# Sunk Cost ----
ggplot(dataset, aes(`sunk_cost_sumC`)) +
  geom_histogram(aes(y = ..density..), colour = "black", fill = "white") +
  stat_function(fun = dnorm, args = list(mean = mean(dataset$sunk_cost_sumC, na.rm = T), 
                                         sd = sd(dataset$sunk_cost_sumC, na.rm = T))) +
  xlab("sunk_cost_sumC")

shapiro.test(dataset$sunk_cost_sumC)
skew(dataset$sunk_cost_sumC) # no transformation needed

# Delay Discount ----
ggplot(dataset, aes(`delay_discount_percC`)) +
  geom_histogram(aes(y = ..density..), colour = "black", fill = "white") +
  stat_function(fun = dnorm, args = list(mean = mean(dataset$delay_discount_percC, na.rm = T), 
                                         sd = sd(dataset$delay_discount_percC, na.rm = T))) +
  xlab("delay_discount_percC")

shapiro.test(dataset$delay_discount_percC)
skew(dataset$delay_discount_percC)

dd_trans <- sqrt(dataset$delay_discount_percC)
skew(dd_trans) # fixed

# Decentering, Age and Gender ----
# Age & Gender Multivariate Linear Models
plot_agedec <- plot(dataset$ageC,dataset$totalscore) # visualises age vs decentering
ggscatter(data_clean, x = "ageC", y = "totalscore", xlab = "Age", ylab = "Decentering", 
          add = "reg.line",    # Add regression line
conf.int = TRUE,                                  # Add confidence interval
add.params = list(color = "blue", fill = "lightgray"))



plot_agedec <- plot(dataset$ageC,totalscoreC_trans) # visualises age vs decentering

m_agedec = lm(totalscoreC_trans~dataset$ageC) # Age and Decentering Model
summary(m_agedec)
abline(m_agedec, col="red", lwd=2)

m_agegen = lm(totalscoreC_trans~dataset$ageC + dataset$gaf) # Age, Gender and Decentering Model
summary(m_agegen)
plot(m_agegen) # model extras

0.08308 /(1 - 0.08308 ) # f2 effect size calculation

# Hierarchical Model A&G

h0 = lm(totalscoreC~cesdC_trans + rcadsC_trans + wemwbsC,data=dataset)
summary(h0)
0.546  /(1 - 0.546 ) # f2 effect size calc

h1 = lm(dataset$totalscoreC~cesdC_trans + rcadsC_trans + dataset$wemwbsC + dataset$ageC)
summary(h1)
0.5783  /(1 - 0.5783 ) # f2 effect size calc

h2 = lm(dataset$totalscoreC~cesdC_trans + rcadsC_trans + dataset$wemwbsC + dataset$ageC + dataset$gaf)
summary(h2)

h3 = lm(dataset$totalscoreC~cesdC_trans + rcadsC_trans + dataset$wemwbsC + dataset$ageC + dataset$gaf + dataset$ageC*dataset$gaf)
summary(h3)

anova(h0,h1)
######################################################################################
#################### HYPOTHESIS 1 - Mental Health Models #############################
######################################################################################
# Depression and Decentering ----

# Scatter Plot - CESD and Decentering
ggscatter(dataset, x = "totalscoreC", y = "cesdC", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Decentering", ylab = "Depression Score")

# Correlation between CESD and Decentering
plot1 <- plot(dataset$cesdC,dataset$totalscoreC)
plot1t <- plot(cesdC_trans,totalscoreC_trans)

# Linear Regression - Predicting Depression using Decentering

m1t = lm(dataset$cesdC~totalscoreC_trans) # TRANSFORMED
summary(m1t)
abline(m1t, col="red", lwd=2)
plot(m1t)

# Multiple Regression - Predicting Depression using Decentering, Gender and Age
m2t = lm(dataset$cesdC ~ totalscoreC_trans + dataset$gaf + dataset$ageC) # TRANSFORMED
summary(m2t)
plot(m2t)
0.4609 /(1 - 0.4609) # effect size f2

vif(m2t) #multicollinearity check
durbinWatsonTest(m2t) # autocorrelation check - slightly significant but not majorly

# Anxiety and Decentering ----

# Scatter Plot - RCADS and Decentering
ggscatter(dataset, x = "totalscoreC", y = "rcadsC", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Decentering", ylab = "Anxiety Score")

# Correlation between RCADS and Decentering
plot3t <- plot(totalscoreC_trans,rcadsC_trans) # transformed

# Linear Regression - Predicting Anxiety using Decentering
m3t = lm(dataset$rcadsC~totalscoreC_trans) # TRANSFORMED
summary(m3t)
abline(m3t, col="red", lwd=1)
plot(m3t)

# Multiple Regression - Predicting Anxiety using Decentering, Gender and Age
m4t = lm(dataset$rcadsC ~ totalscoreC_trans + dataset$gaf + dataset$ageC) # TRANSFORMED
summary(m4t)
plot(m4t)
0.4435 /(1 - 0.4435)

vif(m4t)  #multicollinearity check
durbinWatsonTest(m4t) # autocorrelation check

# Wellbeing and Decentering ----

# Scatter Plot - v and Decentering
ggscatter(dataset, x = "totalscoreC", y = "wemwbsC", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Decentering", ylab = "Wellbeing Score")

# Correlation between WEMWBS and Decentering 
plot5t <- plot(totalscoreC_trans,dataset$wemwbsC)

# Linear Regression - Predicting Wellbeing using Decentering
m5t = lm(dataset$wemwbsC~totalscoreC_trans) # TRANSFORMED
summary(m5t)
abline(m5t, col="red", lwd=2)
plot(m5t)

# Multiple Regression - Predicting Wellbeing using Decentering, Gender and Age
m6t = lm(dataset$wemwbsC ~ totalscoreC_trans + dataset$gaf + dataset$ageC) # NOT TRANSFORMED
summary(m6t)
plot(m6t)
0.3071 /(1 - 0.3071) # effect size f2

vif(m6t)  #multicollinearity check
durbinWatsonTest(m6t) # autocorrelation check

# Hierarchical Regression with Wellbeing, Anx and Depression ----
h0 = lm(dataset$wemwbsC~cesdC_trans,)
summary(h0)

h1 = lm(wemwbsC~ ageC,data=dataset)
summary(h1)
abline(h1, col="red", lwd=2)
0.02858 /(1 - 0.2858) # effect size f2

h2 = lm(wemwbsC~ gaf + ageC,data=dataset)
summary(h2)
0.04141 /(1 - 0.4141) # effect size f2

h3 = lm(dataset$wemwbsC~ rcadsC_trans + cesdC_trans + dataset$gaf + dataset$ageC)
summary(h3)
0.5545 /(1-0.5545) # effect size f2

h4 = lm(dataset$wemwbsC~ totalscoreC_trans + rcadsC_trans + cesdC_trans + dataset$gaf + dataset$ageC)
summary(h4)
0.5555 /(1-0.5555) # effect size f2

anova(h1,h2)

######################################################################################
#################### Neutral Models ##################################################
######################################################################################
# SART Commission Errors ----
ggscatter(data_clean, x = "totalscoreC", y = "SART_COM_NEUC", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Decentering", ylab = "Neutral SART Com")

plot_sart_com_neu <- plot(data_clean$totalscoreC,data_clean$SART_COM_NEUC)

m_sart_com_neu = lm(SART_COM_NEUC~totalscoreC,data = data_clean)
summary(m_sart_com_neu)
abline(m_sart_com_neu, col="red", lwd=2)
-0.001799/(1--0.001799)

m_sart_com_neu2 = lm(SART_COM_NEUC~totalscoreC + genderC + ageC,data = data_clean)
summary(m_sart_neu2)

# SART RT Variance ----
ggscatter(data_clean, x = "totalscoreC", y = "SART_RT_VAR_NEUC", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Decentering", ylab = "Neutral SART RT VAR")

plot_sart_rtv_neu <- plot(data_clean$totalscoreC,data_clean$SART_RT_VAR_NEUC)

m_sart_rtv_neu = lm(SART_RT_VAR_NEUC~totalscoreC,data = data_clean)
summary(m_sart_rtv_neu)
abline(m_sart_rtv_neu, col="red", lwd=2)
0.001494/(1-0.001494)

m_sart_rtv_neu2 = lm(SART_RT_VAR_NEUC~totalscoreC + genderC + ageC,data = data_clean)
summary(m_sart_rtv_neu2)

# EWM Neutral ----
ggscatter(data_clean, x = "totalscoreC", y = "WM_T1_ATDR_percentage_neuC", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Decentering", ylab = "EWM Percentage Words Recalled Neutral")

plot_ewm_neu <- plot(data_clean$totalscoreC,data_clean$WM_T1_ATDR_percentage_neuC)

plot_ewm_neu = lm(WM_T1_ATDR_percentage_neuC~totalscoreC,data = data_clean)
summary(plot_ewm_neu)
abline(plot_ewm_neu, col="red", lwd=2)
0.008827/(1-0.008827)

plot_ewm_neu2 = lm(WM_T1_ATDR_percentage_neuC~totalscoreC + genderC + ageC,data = data_clean)
summary(plot_ewm_neu2)

# Stroop Neutral ----
# RTS
ggscatter(data_clean, x = "totalscoreC", y = "Stroop_Neu_RTC", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Decentering", ylab = "Neutral Stroop RTs")

plot_stroop_neuRT <- plot(data_clean$totalscoreC,data_clean$Stroop_Neu_RT)

m_stroop_neuRT = lm(Stroop_Neu_RTC~totalscoreC,data = data_clean)
summary(m_stroop_neuRT)
abline(m_stroop_neuRT, col="red", lwd=2)
0.006409/(1-0.006409)

m_stroop_neuRT2 = lm(Stroop_Neu_RTC~totalscoreC + genderC + ageC,data = data_clean)
summary(m_stroop_neuRT2)

# Accuracy
ggscatter(data_clean, x = "totalscoreC", y = "Stroop_Neu_ACCC", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Decentering", ylab = "Neutral Stroop Accuracy")

plot_stroop_neuRT <- plot(data_clean$totalscoreC,data_clean$Stroop_Neu_ACCC)

m_stroop_neuACC = lm(Stroop_Neu_ACCC~totalscoreC,data = data_clean)
summary(m_stroop_neuACC)
abline(m_stroop_neuACC, col="red", lwd=2)
0.009635/(1- 0.009635)

m_stroop_neuACC2 = lm(Stroop_Neu_ACCC~totalscoreC + genderC + ageC,data = data_clean)
summary(m_stroop_neuACC2)

# RT VAR
ggscatter(data_clean, x = "totalscoreC", y = "Stroop_RT_var_neuC", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Decentering", ylab = "Neutral Stroop RT Var")

plot_stroop_neuRT <- plot(data_clean$totalscoreC,data_clean$Stroop_RT_var_neuC)

m_stroop_neu_rtv = lm(Stroop_RT_var_neuC~totalscoreC,data = data_clean)
summary(m_stroop_neu_rtv)
abline(m_stroop_neu_rtv, col="red", lwd=2)
0.01307/(1-0.01307)

m_stroop_neu_rtv2 = lm(Stroop_RT_var_neuC~totalscoreC + genderC + ageC,data = data_clean)
summary(m_stroop_neu_rtv2)
abline(m_stroop_neu_rtv2, col="red", lwd=2)

######################################################################################
#################### HYPOTHESIS 2 - EWMI #############################################
######################################################################################

ggscatter(dataset, x = "EWMI", y = "totalscoreC", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Emotional Working Memory Index", ylab = "Decentering")

plot_ewmt <- plot(dataset$EWMI,totalscoreC_trans) # TRANSFORMED

# Linear
m_ewm1t = lm(dataset$EWMI~totalscoreC_trans) # TRANSFORMED
summary(m_ewm1t)
abline(m_ewm1t, col="red", lwd=2)
plot(m_ewm1t)
-9.323e-05 /(1-9.323e-05 ) # f2 effect size

# Multiple
m_ewm2t = lm(dataset$EWMI~totalscoreC_trans + dataset$gaf + dataset$ageC) # TRANSFORMED
summary(m_ewm2t)
plot(m_ewm2t)
vif(m_ewm2t)  #multicollinearity check
durbinWatsonTest(m_ewm2t) # autocorrelation check

######################################################################################
#################### HYPOTHESIS 3 - aSART ############################################
######################################################################################
# Commission Errors ----
ggscatter(dataset, x = "SART_COM_I", y = "totalscoreC", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Commission Errors Index", ylab = "Decentering")

plot_sartcomt <- plot(dataset$SART_COM_I,totalscoreC_trans) # TRANSFORMED

# Linear
m_sartcom1t = lm(dataset$SART_COM_I~totalscoreC_trans) # TRANSFORMED
summary(m_sartcom1t)
abline(m_sartcom1t, col="red", lwd=2)
plot(m_sartcom1t)
0.001381 /(1-0.001381) # effect size

# Multiple 
m_sartcom2t = lm(dataset$SART_COM_I~totalscoreC_trans + dataset$gaf + dataset$ageC) # TRANSFORMED
summary(m_sartcom2t)
abline(m_sartcom2t, col="red", lwd=2)
plot(m_sartcom2t)

vif(m_sartcom2t)  #multicollinearity check
durbinWatsonTest(m_sartcom2t) # autocorrelation check

# RT Variance ----
ggscatter(dataset, x = "totalscoreC", y = "SART_RT_VAR_I", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Decentering", ylab = "RT Variance Index")

plot_sartrtvt <- plot(SARTRT_trans,totalscoreC_trans) # TRANSFORMED

# Linear 
m_sartrtv1t = lm(dataset$SART_RT_VAR_I~totalscoreC_trans) # TRANSFORMED
summary(m_sartrtv1t)
abline(m_sartrtv1t, col="red", lwd=2)
plot(m_sartrtv1t)
0.0009673 /(1-0.0009673 )

# Multiple
m_sartrtv2t = lm(dataset$SART_RT_VAR_I~totalscoreC_trans + dataset$gaf + dataset$ageC) # NOT TRANSFORMED
summary(m_sartrtv2t)
abline(m_sartrtv2t, col="red", lwd=2)
plot(m_sartrtv2t)
vif(m_sartrtv2t)
durbinWatsonTest(m_sartrtv2t)

######################################################################################
#################### HYPOTHESIS 4 - Stroop ###########################################
######################################################################################
# Negative Vs Neutral ----
ggscatter(dataset, x = "totalscoreC", y = "stroop_negneu", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Decentering", ylab = "Neg Minus Neutral")

plot_stroop_negneut <- plot(totalscoreC_trans,stroop_negneu_trans) # TRANSFORMED

# Linear
m_stroop_negneu1t = lm(dataset$stroop_negneu~totalscoreC_trans) # TRANSFORMED
summary(m_stroop_negneu1t)
abline(m_stroop_negneu1t, col="red", lwd=2)
plot(m_stroop_negneu1t)
-0.000766 /(1-(-0.000766 ))

# Multiple
m_stroop_negneu2t = lm(dataset$stroop_negneu~totalscoreC_trans + dataset$gaf + dataset$ageC) # TRANSFORMED
summary(m_stroop_negneu2t)
abline(m_stroop_negneu2t, col="red", lwd=2)
plot(m_stroop_negneu2t)
vif(m_stroop_negneu2t)
durbinWatsonTest(m_stroop_negneu2t) # mildly significant

# Negative Vs Positive ----
ggscatter(dataset, x = "totalscoreC", y = "stroop_negpos", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Decentering", ylab = "Neg Minus Pos")

plot_stroop_negpost <- plot(totalscoreC_trans,dataset$stroop_negpos) # TRANSFORMED

# Linear
m_stroop_negpos1t = lm(dataset$stroop_negpos~totalscoreC_trans) # TRANSFORMED
summary(m_stroop_negpos1t)
abline(m_stroop_negpos1t, col="red", lwd=2)
plot(m_stroop_negpos1t)
-0.0014/(1--0.0014)
# Multiple
m_stroop_negpos2t = lm(dataset$stroop_negpos~totalscoreC_trans + dataset$gaf + dataset$ageC) # TRANSFORMED
summary(m_stroop_negpos2t)
abline(m_stroop_negpos2t, col="red", lwd=2)
plot(m_stroop_negpos2t)
vif(m_stroop_negpos2t) # multico
durbinWatsonTest(m_stroop_negpos2t) # autoco

# Positive Vs Neutral ----
ggscatter(dataset, x = "totalscoreC", y = "stroop_neupos", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Decentering", ylab = "Pos Minus Neutral")

plot_sartrtv <- plot(dataset$totalscoreC,dataset$stroop_neupos)


# Linear
m_stroop_neupos1t = lm(dataset$stroop_neupos~totalscoreC_trans) # TRANSFORMED
summary(m_stroop_neupos1t)
abline(m_stroop_neupos1t, col="red", lwd=2)
0.000717/(1-0.000717)

# Multiple
m_stroop_neupos2t = lm(dataset$stroop_neupos~totalscoreC_trans + dataset$gaf + dataset$ageC) # NOT TRANSFORMED
summary(m_stroop_neupos2t)
abline(m_stroop_neupos2t, col="red", lwd=2)
vif(m_stroop_neupos2t) # multico
durbinWatsonTest(m_stroop_neupos2t) #autoco

######################################################################################
#################### HYPOTHESIS 5 - Social Tasks #####################################
######################################################################################
# Social Influence ----
plot_socdifft <- plot(totalscoreC_trans,socialdiff_trans) # TRANSFORMED

# Linear
m_socdiff1t = lm(dataset$social_absdiffC~totalscoreC_trans) # TRANSFORMED
summary(m_socdiff1t)
abline(m_socdiff1t, col="red", lwd=2)
-0.001114/(1--0.001114)

# Multiple
m_socdiff2t = lm(dataset$social_absdiffC~totalscoreC_trans + dataset$gaf + dataset$ageC) # TRANSFORMED
summary(m_socdiff2t)
abline(m_socdiff2t, col="red", lwd=2)
vif(m_socdiff2t) # multico
durbinWatsonTest(m_socdiff2t) #autoco, slightly significant but not majorly

# Dictator Game ----
plot_dict <- plot(dataset$totalscoreC,dataset$dictator_percC) # NO TRANSFORMATION NEEDED

# Linear
m_dict1t = lm(dataset$dictator_percC~totalscoreC_trans) # TRANSFORMED
summary(m_dict1t)
abline(m_dict1t, col="red", lwd=2)
-0.001809 /(1--0.001809)

# Multiple
m_dict2t = lm(dataset$dictator_percC~totalscoreC_trans + dataset$gaf + dataset$ageC) # TRANSFORMED
summary(m_dict2t)
abline(m_dict2t, col="red", lwd=2)
vif(m_dict2t) # multico
durbinWatsonTest(m_dict2t) #autoco

######################################################################################
#################### Cognitive Bias Tasks ############################################
######################################################################################
# Sunk Cost Bias ----
plot_scs <- plot(dataset$totalscoreC,dataset$sunk_cost_sumC) # NO TRANSFORMATION NEEDED

# Linear
m_sc1t = lm(dataset$sunk_cost_sumC~totalscoreC_trans) # TRANSFORMED
summary(m_sc1t)
abline(m_sc1t, col="red", lwd=2)


# Multiple
m_sc2t = lm(dataset$sunk_cost_sumC~totalscoreC_trans + dataset$gaf + dataset$ageC) # TRANSFORMED
summary(m_sc2t)
abline(m_sc2t, col="red", lwd=2)
vif(m_sc2t) # multico
durbinWatsonTest(m_sc2t) #autoco

# Delay Discount ----
plot_ddt <- plot(totalscoreC_trans,dd_trans) # TRANSFORMED

# Linear
m_dd1t = lm(dataset$delay_discount_percC~totalscoreC_trans) # TRANSFORMED
summary(m_dd1t)
abline(m_dd1t, col="red", lwd=2)

# Multiple
m_dd2t = lm(dataset$delay_discount_percC~totalscoreC_trans + dataset$gaf + dataset$ageC) # TRANSFORMED
summary(m_dd2t)
abline(m_dd2t, col="red", lwd=2)
vif(m_dd2t) # multico
durbinWatsonTest(m_dd2t) #autoco, significant level of autocorrelation

#################### JASP File ############################################
dataset$dectrans <- totalscoreC_trans
write.csv(dataset,"jaspdataset.csv", row.names = FALSE)
