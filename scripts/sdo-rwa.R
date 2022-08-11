# SDO RWA
df <- readRDS(here::here("data_raw", "df.Rds"))

source(here::here("scripts", "funs.R"))

# read libraries in
source(here::here("scripts", "libs.R"))
library("dplyr")
library("tidyr")


# test <- df %>%
#   filter(YearMeasured==1) %>%
#   dplyr::select(Religion.Prayer, Religion.Scripture, Religion.Church, Wave)
# table1::table1(~ Religion.Prayer2 + Religion.Scripture2 + Religion.Church2| Wave, dat = test)
# Perc.Discrim.T11
# Perc.Gend.Discrim.T11
# Perc.Religious.Discrim.T11
# Ethnic.Discrim.T11
# Spiritual.Identification
df$Religion.Scripture
# test <- df%>%
#   filter(YearMeasured == 1 & Wave %in% c("2015" , "2016" , "2017", "2018", "2019"))|>
#   select(Religion.Prayer, Religion.Scripture,  Religion.Church,Religion.Church2,  Wave, Religious)
#
# table1::table1(~ Religion.Church + Religion.Church2 +  Religion.Scripture +  Religion.Prayer |Wave , data = test, transpose = TRUE , overall = F)
#
# test2 <- df%>%
#   filter(YearMeasured == 1 )|>
#   select(  Wave, Religious)
# table1::table1(~ Religious|Wave , data = test2, transpose = TRUE , overall = F)


df <- df |>
  dplyr::rename(
    hopeless_k6 = SWB.Kessler01,
    depressed_k6 = SWB.Kessler02,
    restless_k6 = SWB.Kessler03,
    effort_k6 = SWB.Kessler04,
    worthless_k6 = SWB.Kessler05,
    nervous_k6 = SWB.Kessler06
  )

#
# # order vars
# df$GenCohort <-
#   ordered(
#     df$GenCohort,
#     levels = c(
#       "Gen_Silent: born< 1946",
#       "Gen Boomers: born >= 1946 & b.< 1965",
#       " GenX: born >=1961 & b.< 1981",
#       "GenZ: born >= 1996 "
#     )
#   )
# # # view
# # df %>%   # not there
# #   dplyr::filter(Wave == 2020) %>%
# #   summarise(Respect.Self) #fully missing

# table for participant N
sd_df <- df %>%
  dplyr::mutate(Euro = if_else(EthCat == 1, 1, 0)) %>%
  dplyr::mutate(Male = ifelse(GendAll == 1, 1, 0)) %>%
  dplyr::filter((Wave == 2018  & YearMeasured  == 1) |
                  (Wave == 2019  &
                     YearMeasured  == 1) |
                  (Wave == 2020))  %>% # Eligibility criteria
  dplyr::filter(YearMeasured  != -1) %>% # remove people who passed away
  #dplyr::filter(Id != 9630) %>% # problematic
  dplyr::filter(Euro == 1) %>%
  group_by(Id) %>%
  dplyr::mutate(org2019 = ifelse(Wave == 2019 &
                                   YearMeasured == 1, 1, 0)) %>%  # creating an indicator for the first wave
  dplyr::mutate(hold19 = mean(org2019, na.rm = TRUE)) %>%  # Hack0
  dplyr::filter(hold19 > 0) %>% # hack to enable repeat of baseline in 201
  dplyr::mutate(org2018 =  ifelse(Wave == 2018 &
                                    YearMeasured == 1, 1, 0)) %>%  # creating an indicator for the first wave
  dplyr::mutate(hold18 = mean(org2018, na.rm = TRUE)) %>%  # Hack
  dplyr::filter(hold18 > 0) %>% # hack to enable repeat of baseline
  ungroup() %>%
  droplevels() %>%
  arrange(Id, Wave)

nrow(sd_df)
sd_df$SDO
# check n # 34782
table1::table1(~ SDO + RWA | Wave , data = sd_df, overall = FALSE)
# check N of ids
length(unique(sd_df$Id)) # 34783


## USE 13
sd_df %>%
  filter(Wave == 2018) %>%
  select(NZSEI13) %>%
  count(is.na(.))

sd_df$

## select vars
rm(sd_df1)
sd_df1 <- sd_df %>%
  select(
    Id,
    YearMeasured,
    Wave,
    Age,
    Male,
    NZSEI13,
    CONSCIENTIOUSNESS,
    OPENNESS,
    HONESTY_HUMILITY,
    EXTRAVERSION,
    NEUROTICISM,
    AGREEABLENESS,
    HLTH.Disability,
    Edu,
    NZdep,
    Employed,
    HomeOwner,
    Pol.Orient,
    Household.INC,
    Standard.Living,
    Hours.Work,
    Parent,
    Partner,
    Relid,
    Religion.Church,
    Employed,
    BornNZ,
    KESSLER6sum,
    Smoker,
    Urban,
    BELONG,
    SUPPORT,
    CharityDonate,
    HoursCharity,
    LIFEMEANING,
    LIFESAT,
    PWI,
    SFHEALTH,
    Alcohol.Frequency,
    Alcohol.Intensity,
    Smoker,
    ChildrenNum,
    SDO,
    RWA,
    Warm.Asians,
    Warm.Chinese,
    Warm.Disabled,
    Warm.Elderly,
    Warm.Immigrants,
    Warm.Indians,
    Warm.LGBTQ,
    Warm.Maori,
    Warm.MentalIllness,
    Warm.Muslims,
    Warm.NZEuro,
    Warm.Overweight,
    Warm.Pacific,
    Warm.Refugees,
    ISLAMOPHOBIA
  ) %>%
  dplyr::mutate(Edu = as.numeric(Edu)) %>%
  dplyr::mutate(across(!c(Id, Wave), ~ as.numeric(.x))) %>% # make factors numeric for easy of processing
  arrange(Id, Wave) %>%
  dplyr::mutate(Edu = as.numeric(Edu),
                Volunteers = if_else(HoursCharity == 1, 1, 0),
    Church = ifelse(Religion.Church > 8, 8, Religion.Church),
    income_log = log(Household.INC + 1)) %>%
  arrange(Id, Wave)  %>% #
  dplyr::mutate(across(
    c(
      SDO,
      RWA
    ),
    ~ lead(.x, n = 1),
    .names = "{col}_lead1"
  )) %>% # make leads
  dplyr::mutate(across(
    c(
      Warm.Asians,
      Warm.Chinese,
      Warm.Disabled,
      Warm.Elderly,
      Warm.Immigrants,
      Warm.Indians,
      Warm.LGBTQ,
      Warm.Maori,
      Warm.MentalIllness,
      Warm.Muslims,
      Warm.NZEuro,
      Warm.Overweight,
      Warm.Pacific,
      Warm.Refugees,
      ISLAMOPHOBIA
    ),
    ~ lead(.x, n = 2),
    .names = "{col}_lead2"
  )) %>% # make leads
  dplyr::filter(Wave == 2018) %>%
  #  dplyr::filter(!is.na(Church)) %>%
  #  dplyr::filter(!is.na(Church_lead1)) %>%  #needed for the intervention
  dplyr::select(
    -c(
      Religion.Church,
      HoursCharity,
      Household.INC,
      # org2018,
      #  not_euro,
      #  not_euro_lead2,
      #   hold18,
      #   Euro,
      YearMeasured,
      ISLAMOPHOBIA,
      Warm.LGBTQ,
      Warm.Disabled
      #  org2019,
      #  hold19,
     # retired,
     # semiretired,
    )) %>%
  #  dplyr::mutate(across(!c(Id,Wave), ~ scale(.x)))%>%  # standarise vars for easy computing
  arrange(Id, Wave) %>%
  data.frame() %>%
  mutate(across(where(is.double), as.numeric)) %>%
  arrange(Id)

length(unique(sd_df1$Id)) # 28835

#check
skimr::skim(sd_df1)

# save function
saveh(sd_df1, "sd_df1")

# read if needed
sd_df1<- readh("sd_df1")




# glimse
sd_df1%>%
  summarise(across(c( SDO, SDO_lead1, RWA, RWA_lead1), ~mean(.x, na.rm=TRUE), ~sd(.x, na.rm=TRUE), n_distinct()))



# mice model  -------------------------------------------------------------
library(mice)
dev.off()
# a_mice <- a_df %>%
#   arrange(Id) %>%
#   dplyr::select(-c( Wave, Id, EthCat))

sd_mice <- sd_df1    %>%
  arrange(Id) %>%
  dplyr::select(-c( Wave, Id))

glimpse(sd_mice)

# Visualise missing
library(naniar)
naniar::gg_miss_var(sd_mice)

vis_miss(b_mice,
         warn_large_data = FALSE)
# any colinear vars?
mice:::find.collinear(sd_mice)



# qp <- quickpred(sd_mice) # https://stefvanbuuren.name/fimd/sec-toomany.html
# #Just one forgotten missing data mark may introduce large errors into the imputations.

table(rowSums(qp))
# qp
#for_mice$inc_prop <-
#  for_mice$income_log / (for_mice$income_log_lead1 - 1)



ini <- mice(sd_mice, m = 1,  maxit = 0)
ini
meth <- ini$meth
#meth
#meth["inc_prop"] <- "~ I(income_log/(income_log_lead1 - 1))"
pred <- ini$pred
pred


# impute
out_sdo <- mice::mice(sd_mice,
                    meth = meth,
                    pred = pred,
                    seed = 0,
                    m = 10)

# save
saveh(out_sdo, "out_sdo")

# read
out_sdo <- readh("out_sdo")

# https://www.r-bloggers.com/2020/12/iptw-with-missing-data/
# IPTW   see https://stats.stackexchange.com/questions/563057/multiple-imputation-and-inverse-probability-weighting-for-multiple-treatment##
#https://stats.stackexchange.com/questions/563057/multiple-imputation-and-inverse-probability-weighting-for-multiple-treatment


outlist2 <-
  row.names(out_sdo)[out_sdo$outflux < 0.5]
length(outlist2)

head(out_sdo$loggedEvents, 10)

#test <- mice.impute.fastpmm(out_for_mice,  donors = 5, type = 1, ridge = 1e-05,)


# try mice ranger
# library(doParallel)
# #cl <- makeCluster(2)
# #registerDoParallel(cl)
# library(miceRanger)
# miceObjPar <- miceRanger(for_mice,
#                          m=10,
#                         # meth = meth,
#                          seed = 0,
#                          pred = out,
#                         # parallel = TRUE,
#                          valueSelector = "meanMatch")
#
# saveh(miceObjPar, "miceObjPar")
#
is.mids(out_sdo)
# data warangling
long <- mice::complete(out_sdo, "long", inc = TRUE)
# inspect data -- what do we care about?  Note that the moderate distress category doesn't look useful
skimr::skim(long)
head(long)
#long$LIFESAT_lead1_z <- with(long, scale(LIFESAT_lead1))

# create variables in z score

# HLTH.SleepHours_z
# Religion.Prayer2,
# Religion.Scripture2,
# Spiritual.Identification,

long2 <- long %>%
  # # dplyr::mutate(newkids = ChildrenNum_lead2 - ChildrenNum) %>%
  # dplyr::mutate(Church_lead1bin = if_else(Church_lead1>=1,1,0))%>%
  # dplyr::mutate(Church_bin = if_else(Church>=1,1,0))%>%
  # dplyr::mutate(KESSLER6sum_lead2 = round(as.integer(KESSLER6sum_lead2, 0)))%>%
  # # dplyr::mutate(HLTH.SleepHours_lead2 = round(as.integer(HLTH.SleepHours_lead2, 0)))%>%
  # # dplyr::mutate(HLTH.SleepHours = round(as.integer(HLTH.SleepHours, 0)))%>%
  # dplyr::mutate(Alcohol.Intensity_lead2 = round(Alcohol.Intensity_lead2, 0))%>%
 # dplyr::mutate(CharityDonate_lead2 = round(CharityDonate_lead2, 0))%>%
 # dplyr::mutate(Hours.Exercise_lead2 = round(Hours.Exercise_lead2, 0))%>%
  # dplyr::mutate(Hours.Exercise_lead2_log = log(Hours.Exercise_lead2 + 1))%>%
  # plyr::mutate(Alcohol.Intensity = round(Alcohol.Intensity, 0))%>%
#  dplyr::mutate(CharityDonate = round(CharityDonate, 0))%>%
  # dplyr::mutate(Hours.Exercise = round(Hours.Exercise, 0))%>%
#  dplyr::mutate(CharityDonate_log_lead2 = log(CharityDonate_lead2+1))%>%
  # dplyr::mutate(CharityDonate_log_lead2ord= round(CharityDonate_log_lead2, 0))%>%
  # dplyr::mutate(Alcohol.Intensity_log_lead2 = log(Alcohol.Intensity_lead2+1))%>%
  # dplyr::mutate(Exercise_log_lead2 = log(Hours.Exercise_lead2+1))%>%
  # dplyr::mutate(CharityDonate_log = log(CharityDonate+1))%>%
  # dplyr::mutate(Alcohol.Intensity_log = log(Alcohol.Intensity+1))%>%
  # dplyr::mutate(Rumination_lead2ord = as.integer(round(Rumination_lead2, digits = 0) + 1)) %>%  # needs to start at 1
  # dplyr::mutate(SUPPORT_lead2ord = as.integer(round(SUPPORT_lead2, digits = 0) )) %>%
  # dplyr::mutate(PERFECTIONISM_lead2ord = as.integer(round(PERFECTIONISM_lead2, digits = 0) )) %>%
  # dplyr::mutate(VENGEFUL.RUMIN_lead2ord = as.integer(round(VENGEFUL.RUMIN_lead2, digits = 0) )) %>%
  # dplyr::mutate(Standard.Living_lead2ord = as.integer(round(Standard.Living_lead2, digits = 0) )) %>%
  # dplyr::mutate(Your.Personal.Relationships_lead2ord = as.integer(round(Your.Personal.Relationships_lead2, digits = 0) +1)) %>%
  # dplyr::mutate(LIFEMEANING_lead2ord = as.integer(round(LIFEMEANING_lead2, digits = 0) )) %>%
  # # dplyr::mutate(LifeMeaning01_lead2ord = as.integer(round(LifeMeaning01_lead2, digits = 0) )) %>%
  # # dplyr::mutate(LifeMeaning02_lead2ord = as.integer(round(LifeMeaning02_lead2, digits = 0) )) %>%
  # dplyr::mutate(HLTH.Fatigue_lead2ord = as.integer(round(HLTH.Fatigue_lead2, digits = 0)+1 )) %>%
  # dplyr::mutate(Hours.Exercise_log = log(Hours.Exercise+1))%>%
  # dplyr::mutate(Alcohol.Frequency_lead2ord = as.integer(round(Alcohol.Frequency_lead2, 0)+1))%>%
  # dplyr::mutate(LIFESAT_lead2ord = as.integer(round(LIFESAT_lead2, digits = 0) )) %>%
  # dplyr::mutate(alcohol_bin2 = if_else( Alcohol.Frequency > 3, 1, 0))%>%
  # dplyr::mutate(alcohol_bin = if_else( Alcohol.Frequency > 2, 1, 0))%>%
  #dplyr::mutate(Hours.Work_10 =  Hours.Work/10)%>%
  # dplyr::mutate(Hours.Work_lead1_10 = Hours.Work_lead1/10)%>%
  # dplyr::mutate(Hours.Work_ord = (as.numeric(
  #   cut(
  #     Hours.Work,
  #     breaks = c(-Inf, 0, 10, 20, 30, 40, 50, Inf),
  #     labels = c("0", "1", "2","3", "4", "5", "6"),
  #     right = TRUE
  #   )
  # ) - 1)) %>%
  # dplyr::mutate(Hours.Work_lead1 = (as.numeric(
  #   cut(
  #     Hours.Work_lead1,
  #     breaks = c(-Inf, 0, 10, 20, 30, 40, 50, Inf),
  #     labels = c("0", "1", "2","3", "4", "5", "6"),
  #     right = TRUE
  #   )
  # ) - 1)) %>%
  dplyr::mutate(across(where(is.numeric), ~ scale(.x), .names = "{col}_z")) |>
  # dplyr::mutate(id = 1:length(.)) |>
  # group_by(id) |>
  # dplyr::mutate(LIFEMEANING2 = rowMeans(c(LifeMeaning01, LifeMeaning02, na.rm=TRUE))) %>%
  # dplyr::mutate(KESSLER6sum2 = rowSums(c(hopeless_k6,
  #                                        depressed_k6,
  #                                        restless_k6,
  #                                        effort_k6,
  #                                        worthless_k6,
  #                                        nervous_k6, na.rm=TRUE))) %>%
  select(-c(.imp_z, .id_z))# Respect for Self is fully missing

long3 <- long2 %>% mutate_if(is.matrix, as.vector)
out2_sdo <- mice::as.mids(long3)
saveh(out2_sdo, "out2_sdo")
out2_sdo <- readh("out2_sdo")

out2_sdo
## Try another method
#install.packages("missForest")
library(missForest)
out3_sdo <- missForest(sd_mice)


library("randomForestSRC")
)
# matching ----------------------------------------------------------------


library("MatchThem")
library("optmatch")


models_sdo <- MatchThem::weightthem(SDO_lead1  ~ SDO_z +
    RWA_z +
      AGREEABLENESS_z +
      CONSCIENTIOUSNESS_z +
      EXTRAVERSION_z  +
      HONESTY_HUMILITY_z +
      NEUROTICISM_z +
      OPENNESS_z +
      Age_z +
      Edu_z +
      Employed_z +
      HomeOwner_z +
      Hours.Work_z +
      HLTH.Disability +
      Male_z +
      NZdep_z +
      NZSEI13_z +
      Parent_z +
      Partner_z +
      Pol.Orient_z +
      Warm.Asians_z +
      Warm.Chinese_z +
      Warm.Elderly_z +
      Warm.Immigrants_z +
      Warm.Indians_z +
      Warm.Maori_z +
      Warm.MentalIllness_z +
      Warm.Muslims_z +
      Warm.NZEuro_z +
      Warm.Overweight_z +
      Warm.Pacific_z +
      Warm.Refugees_z +
      Urban_z,
  out2_sdo,
  approach = "within",
  estimand = "ATE",
  stabilize = TRUE,
  method = "ebal"
)
models_rwa <- weightthem(
  RWA_lead1  ~
    RWA_z +
    SDO_z +
    AGREEABLENESS_z +
    CONSCIENTIOUSNESS_z +
    EXTRAVERSION_z  +
    HONESTY_HUMILITY_z +
    NEUROTICISM_z +
    OPENNESS_z +
    Age_z +
    Edu_z +
    Employed_z +
    HomeOwner_z +
    Hours.Work_z +
    HLTH.Disability +
    Male_z +
    NZdep_z +
    NZSEI13_z +
    Parent_z +
    Partner_z +
    Pol.Orient_z +
    Warm.Asians_z +
    Warm.Chinese_z +
    Warm.Elderly_z +
    Warm.Immigrants_z +
    Warm.Indians_z +
    Warm.Maori_z +
    Warm.MentalIllness_z +
    Warm.Muslims_z +
    Warm.NZEuro_z +
    Warm.Overweight_z +
    Warm.Pacific_z +
    Warm.Refugees_z +
    Urban_z,
  out2_sdo,
  approach = "within",
  estimand = "ATE",
  stabilize = TRUE,
  method = "ebal"
)

saveh(models_sdo,"models_sdo")
models_sdo <- readh("models_sdo")

saveh(models_rwa,"models_rwa")
models_rwa <- readh("models_rwa")

summary(models_sdo)
summary(models_rwa)

bal.tab(models_sdo) #
bal.tab(models_rwa) #



ctrim_sdo <- trim(models_sdo, at = .999)
ctrim_rwa <- trim(models_rwa, at = .9996)

summary(ctrim_sdo)
summary(ctrim_rwa)

bal.tab(ctrim_sdo) # good
bal.tab(ctrim_rwa) # good



library(formula.tools)
sdo_lead1 <-  y ~ SD0_lead1_z + SDO_z  +  RWA_z + AGREEABLENESS_z + CONSCIENTIOUSNESS_z + EXTRAVERSION_z  + HONESTY_HUMILITY_z + NEUROTICISM_z + OPENNESS_z + Age_z + Edu_z +  Employed_z + HomeOwner_z + Hours.Work_z + HLTH.Disability + Male_z + NZdep_z + NZSEI13_z + Parent_z + Partner_z + Pol.Orient_z + Warm.Asians_z + Warm.Chinese_z +  Warm.Elderly_z + Warm.Immigrants_z + Warm.Indians_z + Warm.Maori_z + Warm.MentalIllness_z + Warm.Muslims_z +  Warm.NZEuro_z + Warm.Overweight_z + Warm.Pacific_z + Warm.Refugees_z + Urban_z

rwa_lead1 <-  y ~ RWA_lead1_z + RWA_z +  SDO_z  + AGREEABLENESS_z + CONSCIENTIOUSNESS_z + EXTRAVERSION_z  + HONESTY_HUMILITY_z + NEUROTICISM_z + OPENNESS_z + Age_z + Edu_z +  Employed_z + HomeOwner_z + Hours.Work_z + HLTH.Disability + Male_z + NZdep_z + NZSEI13_z + Parent_z + Partner_z + Pol.Orient_z + Warm.Asians_z + Warm.Chinese_z +  Warm.Elderly_z + Warm.Immigrants_z + Warm.Indians_z + Warm.Maori_z + Warm.MentalIllness_z + Warm.Muslims_z +  Warm.NZEuro_z + Warm.Overweight_z + Warm.Pacific_z + Warm.Refugees_z + Urban_z

rhs_sdolead1 <- rhs(sdo_lead1)
rhs_rwalead1  <- rhs(rwa_lead1)
str(rhs_sdolead1)


# neg
out <- with(ctrim_sdo, glm( Warm.Asians_lead2_z ~ SDO_lead1_z, family = "gaussian"))
out <- with(models_sdo, glm( Warm.Asians_lead2_z ~ SDO_lead1_z, family = "gaussian"))
out <- with(out2_sdo, glm( Warm.Asians_lead2_z ~ SDO_lead1_z + SDO_z  +  RWA_z + AGREEABLENESS_z + CONSCIENTIOUSNESS_z + EXTRAVERSION_z  + HONESTY_HUMILITY_z + NEUROTICISM_z + OPENNESS_z + Age_z + Edu_z +  Employed_z + HomeOwner_z + Hours.Work_z + HLTH.Disability + Male_z + NZdep_z + NZSEI13_z + Parent_z + Partner_z + Pol.Orient_z + Warm.Asians_z + Warm.Chinese_z +  Warm.Elderly_z + Warm.Immigrants_z + Warm.Indians_z + Warm.Maori_z + Warm.MentalIllness_z + Warm.Muslims_z +  Warm.NZEuro_z + Warm.Overweight_z + Warm.Pacific_z + Warm.Refugees_z + Urban_z, family = "gaussian"))

output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

round( EValue::evalues.OLS( -0.062264194 , se = 0.01159046, sd = 1, delta = 4, true = 0), 3)

point lower upper
RR       0.797 0.734 0.866
E-values 1.819    NA 1.578

# neg
out <- with(ctrim_sdo, glm(Warm.Chinese_lead2_z  ~ SDO_lead1_z, family = "gaussian"))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)
round( EValue::evalues.OLS( -0.062485793 , se = 0.011133432, sd = 1, delta = 4, true = 0), 3)

point lower upper
RR       0.797 0.736 0.862
E-values 1.822    NA 1.590

# neg
out <- with(ctrim_sdo, glm(  Warm.Disabled_lead2_z ~ SDO_lead1_z, family = "gaussian"))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)
round( EValue::evalues.OLS(   -0.05014969 , se = 0.01099492, sd = 1, delta = 4, true = 0), 3)


point lower upper
RR       0.833  0.77 0.901
E-values 1.691    NA 1.459

# No effect
out <- with(ctrim_sdo, glm(  Warm.Elderly_lead2_z ~ SDO_lead1_z, family = "gaussian"))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)
round( EValue::evalues.OLS(  -0.016811213 , se =  0.011870017, sd = 1, delta = 4, true = 0), 3)

# v big
out <- with(ctrim_sdo, glm(  Warm.Immigrants_lead2_z ~ SDO_lead1_z, family = "gaussian"))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)
round( EValue::evalues.OLS( -0.085470935 , se = 0.01420997, sd = 1, delta = 4, true = 0), 3)

point lower upper
RR       0.733 0.662 0.811
E-values 2.071    NA 1.770


# v neg
out <- with(ctrim_sdo, glm(  Warm.Indians_lead2_z ~ SDO_lead1_z, family = "gaussian"))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)
round( EValue::evalues.OLS( -0.088456442 , se =   0.011441149, sd = 1, delta = 4, true = 0), 3)

point lower upper
RR       0.725 0.668 0.786
E-values 2.104    NA 1.860

# v neg
out <- with(ctrim_sdo, glm(  Warm.LGBTQ_lead2_z ~ SDO_lead1_z, family = "gaussian"))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)
round( EValue::evalues.OLS(   -0.104170442, se =0.014064107, sd = 1, delta = 4, true = 0), 3)

point lower upper
RR       0.684 0.619 0.757
E-values 2.282    NA 1.974

# str
out <- with(ctrim_sdo, glm(  Warm.Maori_lead2_z ~ SDO_lead1_z, family = "gaussian"))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)
round( EValue::evalues.OLS( -0.091142703, se =0.009509109 , sd = 1, delta = 4, true = 0), 3)

point lower upper
RR       0.718 0.671 0.768
E-values 2.134    NA 1.930


# st
out <- with(ctrim_sdo, glm(  Warm.MentalIllness_lead2_z ~ SDO_lead1_z, family = "gaussian"))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)
round( EValue::evalues.OLS( -0.071167232, se =0.010504876, sd = 1, delta = 4, true = 0), 3)

point lower upper
RR       0.772 0.716 0.832
E-values 1.915    NA 1.696

# very neg
out <- with(ctrim_sdo, glm(  Warm.Muslims_lead2_z ~ SDO_lead1_z, family = "gaussian"))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)
round( EValue::evalues.OLS(  -0.1059030 , se = 0.010143036 , sd = 1, delta = 4, true = 0), 3)

point lower upper
RR       0.680 0.633 0.731
E-values 2.302    NA 2.077


# nothing
out <- with(ctrim_sdo, glm(  Warm.NZEuro_lead2_z ~ SDO_lead1_z, family = "gaussian"))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)
round( EValue::evalues.OLS(  , se =, sd = 1, delta = 4, true = 0), 3)


out <- with(ctrim_sdo, glm(  Warm.Pacific_lead2_z ~ SDO_lead1_z, family = "gaussian"))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)
round( EValue::evalues.OLS(  -0.07912611, se = 0.010637507 , sd = 1, delta = 4, true = 0), 3)

point lower upper
RR       0.750 0.695 0.809
E-values 2.001    NA 1.777


out <- with(ctrim_sdo, glm(  Warm.Overweight_lead2_z ~ SDO_lead1_z, family = "gaussian"))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)
round( EValue::evalues.OLS( -0.066788363, se =  0.010152374, sd = 1, delta = 4, true = 0), 3)

point lower upper
RR       0.784  0.73 0.843
E-values 1.868    NA 1.656

out <- with(ctrim_sdo, glm(  Warm.Refugees_lead2_z ~ SDO_lead1_z, family = "gaussian"))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)
round( EValue::evalues.OLS( -0.121897870 , se = 0.01150201, sd = 1, delta = 4, true = 0), 3)

point lower upper
RR       0.642 0.591 0.696
E-values 2.491    NA 2.227


# very neg -- huge
out <- with(ctrim_sdo, glm(  ISLAMOPHOBIA_lead2_z ~ SDO_lead1_z, family = "gaussian"))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)
round( EValue::evalues.OLS(  0.20392308 , se =0.008512492, sd = 1, delta = 4, true = 0), 3)

point lower upper
RR       2.101 1.977 2.232
E-values 3.621 3.367    NA

####

library(mice)
models_rwa
out <- with(ctrim_rwa, glm( Warm.Asians_lead2_z  ~RWA_lead1_z, family = "gaussian"))
out <- with(models_rwa, glm( Warm.Asians_lead2_z  ~RWA_lead1_z, family = "gaussian"))


out <- with(out2_sdo, glm( Warm.Asians_lead2_z  ~ RWA_lead1_z  +  RWA_z + SDO_z + AGREEABLENESS_z + CONSCIENTIOUSNESS_z + EXTRAVERSION_z  + HONESTY_HUMILITY_z + NEUROTICISM_z + OPENNESS_z + Age_z + Edu_z +  Employed_z + HomeOwner_z + Hours.Work_z + HLTH.Disability + Male_z + NZdep_z + NZSEI13_z + Parent_z + Partner_z + Pol.Orient_z + Warm.Asians_z + Warm.Chinese_z +  Warm.Elderly_z + Warm.Immigrants_z + Warm.Indians_z + Warm.Maori_z + Warm.MentalIllness_z + Warm.Muslims_z +  Warm.NZEuro_z + Warm.Overweight_z + Warm.Pacific_z + Warm.Refugees_z + Urban_z, family = "gaussian"))


output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

round( EValue::evalues.OLS(  -0.041311136, se = 0.011612043 , sd = 1, delta = 4, true = 0), 3)

point lower upper
RR       0.860 0.792 0.935
E-values 1.597    NA 1.344

# cross zero
out <- with(models_rwa, glm(Warm.Chinese_lead2_z  ~ RWA_lead1_z, family = "gaussian"))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)
round( EValue::evalues.OLS( .020521001 , se =0.013719954, sd = 1, delta = 4, true = 0), 3)


out <- with(models_rwa, glm(  Warm.Disabled_lead2_z ~ RWA_lead1_z, family = "gaussian"))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)
round( EValue::evalues.OLS( 0.03968692 , se = 0.017467843, sd = 1, delta = 4, true = 0), 3)

point lower upper
RR       1.155 1.020 1.308
E-values 1.579 1.164    NA


# nothing
out <- with(models_rwa, glm(  Warm.Elderly_lead2_z ~ RWA_lead1_z, family = "gaussian"))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)
round( EValue::evalues.OLS(  , se =, sd = 1, delta = 4, true = 0), 3)

# nothing
out <- with(models_rwa, glm(  Warm.Immigrants_lead2_z ~ RWA_lead1_z, family = "gaussian"))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)
round( EValue::evalues.OLS(  , se =, sd = 1, delta = 4, true = 0), 3)

# nothing
out <- with(models_rwa, glm(  Warm.Indians_lead2_z ~ RWA_lead1_z, family = "gaussian"))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)
round( EValue::evalues.OLS(  , se =, sd = 1, delta = 4, true = 0), 3)

# negative stronger
out <- with(models_rwa, glm(  Warm.LGBTQ_lead2_z ~ RWA_lead1_z, family = "gaussian"))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)
round( EValue::evalues.OLS(  -0.06799886 , se = 0.015997479, sd = 1, delta = 4, true = 0), 3)

point lower upper
RR       0.781 0.697 0.875
E-values 1.881    NA 1.547

# Nothing
out <- with(models_rwa, glm(  Warm.Maori_lead2_z ~ RWA_lead1_z, family = "gaussian"))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)
round( EValue::evalues.OLS(  , se =, sd = 1, delta = 4, true = 0), 3)


# nothing
out <- with(models_rwa, glm(  Warm.MentalIllness_lead2_z ~ RWA_lead1_z, family = "gaussian"))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)
round( EValue::evalues.OLS(  , se =, sd = 1, delta = 4, true = 0), 3)


# nothing
out <- with(models_rwa, glm(  Warm.Muslims_lead2_z ~ RWA_lead1_z, family = "gaussian"))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)
round( EValue::evalues.OLS(  , se =, sd = 1, delta = 4, true = 0), 3)

# positive
out <- with(models_rwa, glm(  Warm.NZEuro_lead2_z ~ RWA_lead1_z, family = "gaussian"))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)
round( EValue::evalues.OLS( 0.05963454 , se = 0.019035592, sd = 1, delta = 4, true = 0), 3)

# nothing
out <- with(models_rwa, glm(  Warm.Pacific_lead2_z ~ RWA_lead1_z, family = "gaussian"))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)
round( EValue::evalues.OLS(  , se =, sd = 1, delta = 4, true = 0), 3)


# more over weight
out <- with(models_rwa, glm(  Warm.Overweight_lead2_z ~ RWA_lead1_z, family = "gaussian"))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)
round( EValue::evalues.OLS( 0.03500519 , se = 0.013103871 , sd = 1, delta = 4, true = 0), 3)

# nothing
out <- with(models_rwa, glm(  Warm.Refugees_lead2_z ~ RWA_lead1_z, family = "gaussian"))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)
round( EValue::evalues.OLS(  , se =, sd = 1, delta = 4, true = 0), 3)

# nothing
out <- with(models_rwa, glm(  ISLAMOPHOBIA_lead2_z ~ RWA_lead1_z, family = "gaussian"))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)
round( EValue::evalues.OLS(  , se =, sd = 1, delta = 4, true = 0), 3)
round( EValue::evalues.OLS(  , se =, sd = 1, delta = 4, true = 0), 3)






# Disable printing results in scientific notation
options(scipen=999)

output <- pool(out2, dfcom = NULL)
summary(output)


round( EValue::evalues.OLS( 0.06925731 , se =0.02029099, sd = 1, delta = 4, true = 0), 3)
round( EValue::evalues.OLS( 0.07012287529 , se =.010230794  , sd = 1, delta = 4, true = 0), 3)






