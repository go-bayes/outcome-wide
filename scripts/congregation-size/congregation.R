# Short-Form Subjective Health Scale (General Health Perception Subscale)
# In general, would you say your health is...
# I seem to get sick a little easier than other people.
# I expect my health to get worse.


# Notes
# Church might have subgroups -- boundary of everyone not nec useful
# Contrast cases missing -- all have dunbar's number
#

# Does congregation size cause changes in well-being
# set science digits
options(scipen=999)

library(fs)

# import libraries (jb)
pull_path <-
  fs::path_expand(
    "~/The\ Virtues\ Project\ Dropbox/Joseph\ Bulbulia/00Bulbulia\ Pubs/2021/DATA/ldf.5"
  )
# import functions
pull_path_funs  <-
  fs::path_expand("~/The\ Virtues\ Project\ Dropbox/scripts/funs.R")
pull_path_libs  <-
  fs::path_expand("~/The\ Virtues\ Project\ Dropbox/scripts/libs.R")

#libraries
source(pull_path_libs)

#  functions
source(pull_path_funs)

# # read data
# dff<- readRDS(pull_path)

dff <- readRDS(here::here("data_raw", "df.Rds"))


dff %>%
  filter(Wave == 2016 &  YearMeasured == 1) %>%
  n_distinct("Id")


# table for participant N
 dc <- dat %>%
   arrange(Id,Wave) %>%
   dplyr::mutate(Religion.CongregationSize = ifelse(Religion.Church == 0, 0,  Religion.CongregationSize) ) |> #handle missingness
   dplyr::mutate(Euro = if_else(EthCat == 1, 1, 0)) %>%
   dplyr::mutate(Male = ifelse(GendAll == 1, 1, 0)) %>%
   dplyr::filter((Wave == 2016  & YearMeasured  == 1) |
                   (Wave == 2017  & YearMeasured  == 1) |
                   (Wave == 2018 & YearMeasured != -1))  %>% # Eligibility criteria
   # dplyr::filter(Id != 9630) %>% # problematic for income
   group_by(Id) %>%
   dplyr::mutate(org2 = ifelse(Wave == 2017 &
                                    YearMeasured == 1, 1, 0)) %>%  # creating an indicator for the first wave
   dplyr::mutate(hold2 = mean(org2, na.rm = TRUE)) %>%  # Hack0
   dplyr::filter(hold2 > 0) %>% # hack to enable repeat of baseline in 201
   dplyr::mutate(org1 =  ifelse(Wave == 2016 &
                                     YearMeasured == 1, 1, 0)) %>%  # creating an indicator for the first wave
   dplyr::mutate(hold1 = mean(org1, na.rm = TRUE)) %>%  # Hack
   dplyr::filter(hold1 > 0) %>% # hack to enable repeat of baseline
   ungroup() %>%
   droplevels() %>%
   dplyr::filter(Religious == 1) |>
   arrange(Id, Wave)

 length(unique(dc$Id)) #6835
 # check n

table1::table1(~ Religion.CongregationSize + Religious| Wave , data = dc, overall = FALSE)





# increasing rate
dat%>%
  group_by(Wave) %>%
  summarise(mean(HLTH.Disability, na.rm = TRUE))

# Do you have a health condition or disability that limits you, and that has lasted for 6+ months?


dcc <- dc |>
  select(
    Id,
    YearMeasured,
    Wave,
    Age,
    AGREEABLENESS,
    CONSCIENTIOUSNESS,
    EXTRAVERSION,
    HONESTY_HUMILITY,
    NEUROTICISM,
    OPENNESS,
    Alcohol.Frequency,
    Alcohol.Intensity,
    began_relationship,
    BELONG,
    Believe.Spirit,
    Believe.God,
    Bodysat,
    BornNZ,
    CharityDonate,
    ChildrenNum,
    Edu,
    Emp.JobSecure,
    Euro,
    EthCat,
    Employed,
    Emp.WorkLifeBalance,
    #  GenCohort,
   # GRATITUDE,
    HLTH.BMI,
    HLTH.Fatigue,
    HLTH.Disability,
    HLTH.SleepHours,
    HomeOwner,
    Household.INC,
    HoursCharity,
    Hours.Exercise,
    Hours.Work,
    ImpermeabilityGroup,
    KESSLER6sum,
   # LIFEMEANING,
    LIFESAT,
    lost_job,
    Male,
    NWI,
    NZdep,
    NZSEI13,
    Parent,
    Partner,
    partnerlost_job,
   # PERFECTIONISM,
  #  PermeabilityIndividual,
    Pol.Orient,
  #  POWERDEPENDENCE1,
  #  POWERDEPENDENCE2,
    Relid,
    Religion.CongregationSize,
    Religion.Church2,
    Religion.Prayer2,
    Religion.Scripture2,
    Religious,
    Respect.Self,
    retired,
    RWA,
    Rumination,
    SDO,
    semiretired,
    SELF.CONTROL,
    SELF.ESTEEM,
    SexualSatisfaction,
    SFHEALTH,
    Smoker,
    Spiritual.Identification,
    Standard.Living,
    SUPPORT,
    SWB.SoC01,
    Urban,
  #  VENGEFUL.RUMIN,
    Your.Health,
    Your.Future.Security,
    Your.Personal.Relationships
  ) %>%
  dplyr::rename(community = SWB.SoC01) %>%
  dplyr::mutate(Edu = as.numeric(Edu)) %>%
  dplyr::mutate(across(!c(Id, Wave), ~ as.numeric(.x))) %>% # make factors numeric for easy of processing
  arrange(Id, Wave) %>%
  rename(Religion.Prayer = Religion.Prayer2) |>
  rename(Religion.Scripture = Religion.Scripture2) %>%
  rename(Religion.Church = Religion.Church2) %>%
  dplyr::mutate(
    Edu = as.numeric(Edu),
    Volunteers = if_else(HoursCharity == 1, 1, 0),
    # Depressed = (as.numeric(
    #   cut(
    #     KESSLER6sum,
    #     breaks = c(-Inf, 13, Inf),
    #     labels = c("0", "1"),
    #     right = FALSE
    #   )
    # ) - 1),
    # EthCat = factor(EthCat, labels = c("Euro", "Maori", "Pacific", "Asian")),
    Church = ifelse(Religion.Church > 8, 8, Religion.Church),
    income_log = log(Household.INC + 1)
  ) %>%
  arrange(Id, Wave)  %>% #support
  dplyr::mutate(Religion.CongregationSize_lead1 = lead(Religion.CongregationSize, n = 1)) %>%
  dplyr::mutate(SUPPORT_lead1 = lead(SUPPORT, n = 1))%>%
  # dplyr::mutate(Standard.Living_lead1 = lead(Standard.Living, n = 1)) %>%
  #dplyr::mutate(Church_lead1 = lead(Church, n = 1)) %>%  Your.Future.Security
  # inc_prop = (income_log / (income_log_lead1) - 1),
  dplyr::mutate(across(
    c(
      Id,
      YearMeasured,
      Wave,
      Age,
      AGREEABLENESS,
      CONSCIENTIOUSNESS,
      EXTRAVERSION,
      HONESTY_HUMILITY,
      NEUROTICISM,
      OPENNESS,
      Alcohol.Frequency,
      Alcohol.Intensity,
      began_relationship,
      BELONG,
      Believe.Spirit,
      Believe.God,
      Bodysat,
      BornNZ,
      CharityDonate,
      ChildrenNum,
      Edu,
      Emp.JobSecure,
      Euro,
      EthCat,
      Employed,
      Emp.WorkLifeBalance,
      #  GenCohort,
      # GRATITUDE,
      HLTH.BMI,
      HLTH.Fatigue,
      HLTH.Disability,
      HLTH.SleepHours,
      HomeOwner,
      Household.INC,
      HoursCharity,
      Hours.Exercise,
      Hours.Work,
      ImpermeabilityGroup,
      KESSLER6sum,
      # LIFEMEANING,
      LIFESAT,
      lost_job,
      Male,
      NWI,
      NZdep,
      NZSEI13,
      Parent,
      Partner,
      partnerlost_job,
      # PERFECTIONISM,
      #  PermeabilityIndividual,
      Pol.Orient,
      #  POWERDEPENDENCE1,
      #  POWERDEPENDENCE2,
      Relid,
      Religion.CongregationSize,
      Religion.Church,
      Religion.Prayer,
      Religion.Scripture,
      Religious,
      Respect.Self,
      retired,
      RWA,
      Rumination,
      SDO,
      semiretired,
      SELF.CONTROL,
      SELF.ESTEEM,
      SexualSatisfaction,
      SFHEALTH,
      Smoker,
      Spiritual.Identification,
      Standard.Living,
      SUPPORT,
      community, # SWB.SoC01,
      Urban,
      #  VENGEFUL.RUMIN,
      Your.Health,
      Your.Future.Security,
      Your.Personal.Relationships
    ),
    ~ lead(.x, n = 2),
    .names = "{col}_lead2"
  )) %>% # make leads
  dplyr::filter(Wave == 2016) %>%
  dplyr::filter(!is.na(Religion.CongregationSize)) %>%
  dplyr::filter(!is.na(Religion.CongregationSize_lead1)) %>%
  dplyr::filter(Relid >0) %>%
  # dplyr::filter(retired != 1) %>%
  # dplyr::filter(retired_lead1 != 1) %>%  #needed for the intervention
  # dplyr::filter(semiretired != 1) %>%
  #dplyr::filter(semiretired_lead1 != 1) %>%
  #dplyr::filter(!is.na(income_log_lead1) )%>%  #   ABOUT
  #dplyr::filter(!is.na(income_log) )%>% #  THINK ABOUT
  # dplyr::filter(Household.INC >= 30975) %>% # min income
  # dplyr::filter(income_log_lead1 > income_log) %>%
  #dplyr::filter(!is.na(Standard.Living) )%>%
  # dplyr::filter(!is.na(Standard.Living_lead1) )%>%
  #  dplyr::filter(semiretired_lead1 != 1) %>%  #needed for the intervention
dplyr::select(
  -c(
    # EthCat,
    HoursCharity,
    Respect.Self_lead2,
    Household.INC,
    #  org2018,
    #  not_euro,
    #  not_euro_lead2,
    # hold18,
    #   Euro,
    #  Emp.WorkLifeBalance,
    YearMeasured,
    # HLTH.Disability_lead1,
    # org2019,
    # hold19,
    # retired,

    #  retired_lead1,
    # semiretired,
    #  semiretired_lead1
  )
) %>%
  mutate(across(where(is.double), as.numeric)) |> data.frame() %>%
  arrange(Id)


# number of ids
N <- length(unique(dcc$Id))
N  # 5407


table(!is.na(dc$Religion.CongregationSize))
# inspect data
skim(dcc)


# save function

# read if needed


# mice model  -------------------------------------------------------------
library(mice)

mice_cc <- dcc %>%
  dplyr::select(-c( Wave, Id))  # won't otherwise run

library(naniar)
naniar::gg_miss_var(mice_cc)
vis_miss(mice_cc,
         warn_large_data = FALSE)

# any colinear vars?
mice:::find.collinear(mice_cc)

# impute
mice_cc <- mice::mice(mice_cc,  seed = 0, m = 10)

# save
saveh(mice_cc, "mice_cc")


# read
mice_cc <- readh("mice_cc")

# we create two completed data sets -- the one without the missing data will be useful for
# determing causal contrasts -- which we'll describe below.

cc_f <- mice::complete(mice_cc, "long", inc = F)
cc_l <- mice::complete(mice_cc, "long", inc = TRUE)


# inspect data -- what do we care about?  Note that the moderate distress category doesn't look useful
skimr::skim(cc_l)

# create variables in z score
cc_l <- cc_l %>%
  # dplyr::mutate(newkids = ChildrenNum_lead2 - ChildrenNum) %>%
  dplyr::mutate(KESSLER6sum_lead2 = round(as.integer(KESSLER6sum_lead2, 0)))%>%
  dplyr::mutate(CharityDonate_lead2 = round(CharityDonate_lead2, 0))%>%
  plyr::mutate(Alcohol.Intensity = round(Alcohol.Intensity, 0))%>%
  dplyr::mutate(CharityDonate = round(CharityDonate, 0))%>%
  dplyr::mutate(Hours.Exercise = round(Hours.Exercise, 0))%>%
  dplyr::mutate(CharityDonate_log_lead2 = log(CharityDonate_lead2+1))%>%
  dplyr::mutate(CharityDonate_log = log(CharityDonate+1))%>%
  dplyr::mutate(SUPPORT_lead2ord = as.integer(round(SUPPORT_lead2, digits = 0) )) %>%
  dplyr::mutate(Hours.Exercise_log = log(Hours.Exercise+1))%>%
  dplyr::mutate(LIFESAT_lead2ord = as.integer(round(LIFESAT_lead2, digits = 0) )) %>%
  dplyr::mutate(alcohol_bin2 = if_else( Alcohol.Frequency > 3, 1, 0))%>%
  dplyr::mutate(alcohol_bin = if_else( Alcohol.Frequency > 2, 1, 0))%>%
  dplyr::mutate(Hours.Work_10 =  Hours.Work/10)%>%
  dplyr::mutate(Religion.Prayer_log =  log(Religion.Prayer+1))%>%
  dplyr::mutate(Religion.Prayer_lead2_log =  log(Religion.Prayer_lead2 +1))%>%
  dplyr::mutate(Religion.Scripture_log =  log(Religion.Scripture+1))%>%
  dplyr::mutate(Religion.Scripture_lead2_log =  log(Religion.Scripture_lead2+1))%>%
  dplyr::mutate(Religion.CongregationSize_log=  log(Religion.CongregationSize + 1))%>%
  dplyr::mutate(Religion.CongregationSize_lead1_log=  log(Religion.CongregationSize_lead1 + 1))%>%
  dplyr::mutate(Religion.CongregationSize_lead1_100=  Religion.CongregationSize_lead1/100)%>%
  dplyr::mutate(Religion.CongregationSize_dunbar1=  if_else(Religion.CongregationSize== 0, 0,
                                                            if_else(Religion.CongregationSize > 0 &
                                                                      Religion.CongregationSize< 151, 1, 2)))%>%
  dplyr::mutate(Religion.CongregationSize_lead1_dunbar1=  if_else(Religion.CongregationSize_lead1 == 0, 0,
                                                                  if_else(Religion.CongregationSize_lead1 > 0 &
                                                                            Religion.CongregationSize_lead1 < 151, 1, 2)))%>%
  dplyr::mutate(NZSEI06_10 =  NZSEI06/10)%>%
  dplyr::mutate(across(where(is.numeric), ~ scale(.x), .names = "{col}_z")) %>%
  select(-c(.imp_z, .id_z))%>%
  dplyr::mutate(id = as.factor(rep(1:N, 11)))# needed for g-comp# Respect for Self is fully missing


# for models wihout looping (not advised)
cc_f <- cc_f %>%
  # dplyr::mutate(newkids = ChildrenNum_lead2 - ChildrenNum) %>%
  dplyr::mutate(KESSLER6sum_lead2 = round(as.integer(KESSLER6sum_lead2, 0)))%>%
  dplyr::mutate(CharityDonate_lead2 = round(CharityDonate_lead2, 0))%>%
  plyr::mutate(Alcohol.Intensity = round(Alcohol.Intensity, 0))%>%
  dplyr::mutate(CharityDonate = round(CharityDonate, 0))%>%
  dplyr::mutate(Hours.Exercise = round(Hours.Exercise, 0))%>%
  dplyr::mutate(CharityDonate_log_lead2 = log(CharityDonate_lead2+1))%>%
  dplyr::mutate(CharityDonate_log = log(CharityDonate+1))%>%
  dplyr::mutate(SUPPORT_lead2ord = as.integer(round(SUPPORT_lead2, digits = 0) )) %>%
  dplyr::mutate(Hours.Exercise_log = log(Hours.Exercise+1))%>%
  dplyr::mutate(LIFESAT_lead2ord = as.integer(round(LIFESAT_lead2, digits = 0) )) %>%
  dplyr::mutate(alcohol_bin2 = if_else( Alcohol.Frequency > 3, 1, 0))%>%
  dplyr::mutate(alcohol_bin = if_else( Alcohol.Frequency > 2, 1, 0))%>%
  dplyr::mutate(Hours.Work_10 =  Hours.Work/10)%>%
  dplyr::mutate(Religion.Prayer_log =  log(Religion.Prayer+1))%>%
  dplyr::mutate(Religion.Prayer_lead2_log =  log(Religion.Prayer_lead2 +1))%>%
  dplyr::mutate(Religion.Scripture_log =  log(Religion.Scripture+1))%>%
  dplyr::mutate(Religion.Scripture_lead2_log =  log(Religion.Scripture_lead2+1))%>%
  dplyr::mutate(Religion.CongregationSize_log=  log(Religion.CongregationSize + 1))%>%
  dplyr::mutate(Religion.CongregationSize_lead1_log=  log(Religion.CongregationSize_lead1 + 1))%>%
  dplyr::mutate(Religion.CongregationSize_lead1_100=  Religion.CongregationSize_lead1/100)%>%
  dplyr::mutate(Religion.CongregationSize_dunbar1=  if_else(Religion.CongregationSize== 0, 0,
                                                            if_else(Religion.CongregationSize > 0 &
                                                                      Religion.CongregationSize< 151, 1, 2)))%>%
  dplyr::mutate(Religion.CongregationSize_lead1_dunbar1=  if_else(Religion.CongregationSize_lead1 == 0, 0,
                                                                  if_else(Religion.CongregationSize_lead1 > 0 &
                                                                            Religion.CongregationSize_lead1 < 151, 1, 2)))%>%
  dplyr::mutate(NZSEI06_10 =  NZSEI06/10)%>%
  dplyr::mutate(across(where(is.numeric), ~ scale(.x), .names = "{col}_z")) %>%
  select(-c(.imp_z, .id_z))%>%
  dplyr::mutate(id = as.factor(rep(1:N, 10)))# needed for g-comp# Respect for Self is fully missingneeded for g-comp# Respect for Self is fully missing

# Get data into shape
cc3 <- cc_f %>% mutate_if(is.matrix, as.vector)
cc3l <- cc_l %>% mutate_if(is.matrix, as.vector)

ccu <- mice::as.mids(cc3l)


saveh(cc3, "cc3")
saveh(ccu, "ccu")


skimr::skim(cc3)


###### READ THIS DATA IN BELLA  #########
cc3 <- readh("cc3")
ccu <- readh("ccu")

table(cc3$Religion.CongregationSize_dunbar1 == cc3$Religion.CongregationSize_lead1_dunbar1)

hist(cc3$Hou)
# model equations ---------------------------------------------------------
baselinevars = c("AGREEABLENESS_z","CONSCIENTIOUSNESS_z","EXTRAVERSION_z","HONESTY_HUMILITY_z","NEUROTICISM_z","OPENNESS_z","Age_z","BornNZ_z","Believe.God_z","Believe.Spirit_z","BELONG_z","CharityDonate_log_z","Church_z", "community","Edu_z","Employed_z","Euro_z", "Hours.Exercise_log_z","Hours.Work_z","HLTH.BMI_z", "HLTH.Disability_z", "HLTH.Fatigue_z", "HLTH.SleepHours_z","income_log_z", "KESSLER6sum_z", "LIFESAT_z", "Male_z", "NWI_z","NZSEI06_z","Parent_z","Partner_z","Pol.Orient_z", "PWI_z", "Relid_z", "Respect.Self_z", "Religion.CongregationSize_log_z", "RWA_z",  "SDO_z", "SELF.CONTROL_z", "SELF.ESTEEM_z", "SFHEALTH_z","Smoker_z", "Spiritual.Identification_z","SUPPORT_z","Urban_z",  "Volunteers_z")


# functions ---------------------------------------------------------------

# see "funs.R"

## Also use
round( EValue::evalues.OLS( , se = , sd = 1, delta = 2, true = 0), 3)
round( EValue::evalues.RR( , lo =  , hi =, true = 1), 3)


# set up vars -------------------------------------------------------------
ylim <- c(-1,1)
xlab <- "Congregation Size (log)"
df <-  ccu
m = 10
X = "Religion.CongregationSize_lead1_log"
x = 0:9


# reference level
r = 0

sum(cc3$Religion.CongregationSize_lead1 > 0)
sum(cc3$Religion.CongregationSize > 0)
# no changes
table(cc3$Religion.CongregationSize_lead1_lowerdunbar == cc3$Religion.CongregationSize_lowerdunbar)/10



# sfhealth ----------------------------------------------------------------

# fit
fit_0 = function(formula) {
  with(ccu,
       glm(as.formula(paste("SFHEALTH_lead2_z~ bs(Religion.CongregationSize_lead1_log)+",
                          paste(baselinevars, collapse = "+")))
         ))
}
# ppoled model
out_0 <- fit_0()

main = "SF Health"
ylab = "SF Health (SD)"

# g-formula
pout_0 <- pool_stglm(out_0, df = df, m = m,  X = X, x = x)

# Evalues
# round( EValue::evalues.RR( , lo =  , hi = , true = 1), 3)
# round( EValue::evalues.RR(1.207  , lo =  1.1759, hi =1.2381, true = 1), 3)

round( EValue::evalues.OLS( 0.109, se = 0.00647, sd = 1, delta = 3, true = 0), 3)


# plots
plot_stglm(pout_0, ylim = ylim, main, xlab, ylab)
ggplot_stglm(pout_0, ylim  = c(-.5,.5), main, xlab, ylab)

out_cr <- pool_stglm_contrast(out_0, df = df, m = m,  X = X, x = x, r= r)
plot_stglm_contrast(out_cr, ylim, main, xlab, ylab)
ggplot_stglm_contrast(out_cr, ylim, main, xlab, ylab)


# fatigue -----------------------------------------------------------------

# fit
fit_Fatigue = function(formula) {
  with(ccu, glm(as.formula(paste("HLTH.Fatigue_lead2_z ~ bs(Religion.CongregationSize_lead1_log)+",
                                 paste(baselinevars,
                                       collapse = "+")))))
}
# ppoled model
m6_Fatigue <- fit_Fatigue()

main = "Fatigue"
ylab = "Fatigue (SD)"

# g-formula
out_Fatigue <- pool_stglm(m6_Fatigue, df = df, m = m,  X = X, x = x)

# Evalues
# round( EValue::evalues.RR( , lo =  , hi = , true = 1), 3)
# round( EValue::evalues.RR(1.207  , lo =  1.1759, hi =1.2381, true = 1), 3)

round( EValue::evalues.OLS( 0.109, se = 0.00647, sd = 1, delta = 3, true = 0), 3)


# plots
plot_stglm(out_Fatigue, ylim = ylim, main, xlab, ylab)
ggplot_stglm(out_Fatigue, ylim = ylim = c(-.5,.5), main, xlab, ylab)

out_cr <- pool_stglm_contrast(m6_Fatigue, df = df, m = m,  X = X, x = x, r= r)
plot_stglm_contrast(out_cr, ylim, main, xlab, ylab)
ggplot_stglm_contrast(out_cr, ylim, main, xlab, ylab)


# kessler 6 ---------------------------------------------------------------
## fit
# Kessler-6
# During the last 30 days, how often did.... you feel hopeless?
#   During the last 30 days, how often did.... you feel so depressed that nothing could cheer you up?
#   During the last 30 days, how often did.... you feel restless or fidgety?
#   During the last 30 days, how often did.... you feel that everything was an effort?
#   During the last 30 days, how often did.... you feel worthless?
#   During the last 30 days, how often did.... you feel nervous?

fit_KESSLER6sum_st = function(formula) {
  with(ccu, glm(as.formula(paste("KESSLER6sum_lead2_z~ bs(Religion.CongregationSize_lead1_log)+",
                                 paste(baselinevars,
                                       collapse = "+")))))
}
# ppoled model
m15_KESSLER6sum_st <- fit_KESSLER6sum_st()

main = "Kessler 6 Distress"
ylab = "Kessler 6 Distress (SD)"
out_p <- pool_stglm(m15_KESSLER6sum_st, df = df, m = m,  X = X, x = x)

plot_stglm(out_p, ylim = ylim, main, xlab, ylab)
ggplot_stglm(out_p, ylim = ylim, main, xlab, ylab)
out_p
out_cr <- pool_stglm_contrast(m15_KESSLER6sum_st, df = df, m = m,  X = X, x = x, r= r)
ggplot_stglm_contrast(out_cr, ylim = ylim, main, xlab, ylab)

out_cr
mean(cc3$KESSLER6sum_lead2)
effectunit <- sd(cc3$KESSLER6sum_lead2)
effectunit *  -0.06071452

4.805503

# note that 5 is consider moderate distress
ave40 <-mean(wf3$KESSLER6sum_lead2)
ave40

# the loss of 40 hour work pushes people over this threshold
ave40 + effectunit

# increase
round( EValue::evalues.OLS(  , se =    , sd = 1, delta = 3, true = 0), 3)
# decrease
round( EValue::evalues.OLS(  0.04651621 , se =   0.03914626, sd = 1, delta = 3, true = 0), 3)

# how to examine differences?

diffbase <- pool_stglm_contrast(m15_KESSLER6sum_st, df = df, m = m,  X = X, x = x, r= 0)
diffbase

diffbase2 <- pool_stglm_contrast(m15_KESSLER6sum_st, df = df, m = m,  X = X, x = x, r= 5)
diffbase2

# plot it
ggplot_stglm_contrast(diffbase, ylim, main, xlab, ylab)
ggplot_stglm_contrast(diffbase2, ylim, main, xlab, ylab)


# honesty humility --------------------------------------------------------

# Mini-IPIP6 Honesty-Humility (item overlap with Psychological Entitlement)
# Would like to be seen driving around in a very expensive car.
# Would get a lot of pleasure from owning expensive luxury goods.
# Feel entitled to more of everything.
# Deserve more things in life.

## fit
fit_HONESTY_HUMILITY_st = function(formula) {
  with(ccu, glm(
    as.formula(paste("HONESTY_HUMILITY_lead2_z~ bs(Religion.CongregationSize_lead1_log)+",
                     paste(baselinevars,
                           collapse = "+")))
  ))
}
# pooled model
m23_HONESTY_HUMILITY_st <- fit_HONESTY_HUMILITY_st()

main = "Honesty Humility"
ylab = "Honesty Humility (SD)"
out_p <- pool_stglm(m23_HONESTY_HUMILITY_st, df = df, m = m,  X = X, x = x)

plot_stglm(out_p, ylim = ylim, main, xlab, ylab)
ggplot_stglm(out_p, ylim = ylim, main, xlab, ylab)

out_cr <- pool_stglm_contrast(m23_HONESTY_HUMILITY_st, df = df, m = m,  X = X, x = x, r= r)
ggplot_stglm_contrast(out_cr, ylim, main, xlab, ylab)

# increase
round( EValue::evalues.OLS(  , se =    , sd = 1, delta = 3, true = 0), 3)
# decrease
round( EValue::evalues.OLS(   , se =   , sd = 1, delta = 3, true = 0), 3)

# belonging ---------------------------------------------------------------
# Felt belongingness
# Know that people in my life accept and value me.
# Feel like an outsider.
# Know that people around me share my attitudes and beliefs.

BELONG_st <- as.formula(paste("BELONG_lead2_z~ bs(Religion.CongregationSize_lead1_log)+",
                              paste(baselinevars,
                                    collapse = "+")))
## fit
fit_BELONG_st = function(formula) {
  with(ccu, glm(
    as.formula(paste("BELONG_lead2_z~ bs(Religion.CongregationSize_lead1_log)+",
                     paste(baselinevars,
                           collapse = "+")))
  ))
}
# pooled model
m24_BELONG_st <- fit_BELONG_st()

main = "Social Belonging"
ylab = "Social Belonging (SD)"
out_p <- pool_stglm(m24_BELONG_st, df = df, m = m,  X = X, x = x)
plot_stglm(out_p, ylim = ylim, main, xlab, ylab)
ggplot_stglm(out_p, ylim = ylim, main, xlab, ylab)

out_cr <- pool_stglm_contrast(m24_BELONG_st, df = df, m = m,  X = X, x = x, r= r)
ggplot_stglm_contrast(out_cr, ylim, main, xlab, ylab)

# increase
round( EValue::evalues.OLS(  , se =    , sd = 1, delta = 3, true = 0), 3)
# decrease
round( EValue::evalues.OLS(   , se =   , sd = 1, delta = 3, true = 0), 3)


# soc support -------------------------------------------------------------

# Perceived social support
# There are people I can depend on to help me if I really need it.
# There is no one I can turn to for guidance in times of stress.
# I know there are people I can turn to when I need help.


SUPPORT_st <- as.formula(paste("SUPPORT_lead2_z~ bs(Religion.CongregationSize_lead1_log)+",
                               paste(baselinevars,
                                     collapse = "+")))

## fit
fit_SUPPORT_st = function(formula) {
  with(ccu, glm(
    as.formula(paste("SUPPORT_lead2_z~ bs(Religion.CongregationSize_lead1_log)+",
                     paste(baselinevars,
                           collapse = "+")))
  ))
}

# pooled model
m25_SUPPORT_st <- fit_SUPPORT_st()

main = "Social Support"
ylab = "Social Support (SD)"
out_p<- pool_stglm(m25_SUPPORT_st, df = df, m = m,  X = X, x = x)
plot_stglm(out_p, ylim = ylim, main, xlab, ylab)
out_p
ggplot_stglm(out_p, ylim = ylim, main, xlab, ylab)


out_cr <- pool_stglm_contrast(m25_SUPPORT_st, df = df, m = m,  X = X, x = x, r= r)
ggplot_stglm_contrast(out_cr, ylim, main, xlab, ylab)

dev.off()
# increase
round( EValue::evalues.OLS(  , se =    , sd = 1, delta = 3, true = 0), 3)
# decrease
round( EValue::evalues.OLS(   , se =   , sd = 1, delta = 3, true = 0), 3)


# volunteers --------------------------------------------------------------
Volunteers_st <- as.formula(paste("Volunteers_lead2~ bs(Religion.CongregationSize_lead1_log)+",
                                  paste(baselinevars,  collapse = "+")))


# fit
fit_Volunteers_st = function(formula) {
  with(ccu, glm(as.formula(paste("Volunteers_lead2~ bs(Religion.CongregationSize_lead1_log)+",
                                 paste(baselinevars,
                                       collapse = "+")))))
}
# pooled model
m26_Volunteers <- fit_Volunteers_st()


main = "Volunteer Rate"
ylab = "Volunteer Rate"
out_Volunteers<- pool_stglm(m26_Volunteers, df = df, m = m,  X = X, x = x)

plot_stglm(out_Volunteers, ylim = c(0,1), main, xlab, ylab)
ggplot_stglm(out_Volunteers, ylim = c(0,1), main, xlab, ylab)

out_c <- pool_stglm_contrast_ratio(m26_Volunteers, df = df, m = m,  X = X, x = x, r= r)

plot_stglm_contrast(out_c, ylim = c(0,2), main, xlab, ylab)
ggplot_stglm_contrast(out_c, ylim = c(0,2), main, xlab, ylab)
#

#
# # contrast model
# m26_long <- glm(Volunteers_st, data = wf3, family = "poisson")
# m26_long
# gform_m26<- stdGlm(fit = m26_long, data = wf3, X = X, x =x, clusterid="id")
# summary(gform_m26,
#         contrast = "ratio",
#         #type="odds",
#         reference = r)
#
# plot(gform_m26)
#
# # evalues for risk ratios
# round( EValue::evalues.RR( , lo =  , hi = 0.9172, true = 1), 3)
# round( EValue::evalues.RR(  , lo =  , hi =1.2381, true = 1), 3)
#
# # increase
# round( EValue::evalues.OLS(  , se =     , sd = 1, delta = 3, true = 0), 3)
# # decrease
# round( EValue::evalues.OLS(-    , se =    , sd = 1, delta = 3, true = 0), 3)
#
#
# plot(gform_m26,# ylim = c(0,.06),
#      contrast = "ratio",
#      reference = r,# type = "odds",
#      main="Volunteering risk ratio relative to baseline ", col.main="black",
#      sub="Baseline = 30 hours", col.sub="black",
#      xlab="Work Hours/10", ylab="Volunteering rate",
#      col.lab="black", cex.lab=0.75)
#
# dev.off()

# charity donate ----------------------------------------------------------

## fit
fit_CharityDonate_st = function(formula) {
  with(ccu, glm(
    as.formula(paste("CharityDonate_log_lead2_z~ bs(Religion.CongregationSize_lead1_log)+",
                     paste(baselinevars,
                           collapse = "+")))
  ))
}

# pooled model
m27_CharityDonate_st <- fit_CharityDonate_st()
main = "Charity Donatations (annual SD)"
ylab = "Charity Donatations (annual SD)"
out_CharityDonate <- pool_stglm(m27_CharityDonate_st, df = df, m = m,  X = X, x = x)
out_CharityDonate
plot_stglm(out_CharityDonate, ylim, main, xlab, ylab)
ggplot_stglm(out_CharityDonate, ylim, main, xlab, ylab)


out_cr <- pool_stglm_contrast(m27_CharityDonate_st, df = df, m = m,  X = X, x = x, r= r)
plot_stglm_contrast(out_cr, ylim, main, xlab, ylab)
ggplot_stglm_contrast(out_cr, ylim, main, xlab, ylab)


# community lead ----------------------------------------------------------
# Sense of community
# I feel a sense of community with others in my local neighbourhood.


## fit
fit_community_st = function(formula) {
  with(ccu, glm(
    as.formula(paste("community_lead2_z~ bs(Religion.CongregationSize_lead1_log)+",
                     paste(baselinevars,
                           collapse = "+")))
  ))
}

# pooled model
m28_community_st <- fit_community_st()
main = "Community"
ylab = "Community (SD)"
out_community <- pool_stglm(m28_community_st, df = df, m = m,  X = X, x = x)

plot_stglm(out_community, ylim, main, xlab, ylab)
ggplot_stglm(out_community, ylim, main, xlab, ylab)

out_cr <- pool_stglm_contrast(m28_community_st, df = df, m = m,  X = X, x = x, r= r)
plot_stglm_contrast(out_cr, ylim, main, xlab, ylab)
ggplot_stglm_contrast(out_cr, ylim, main, xlab, ylab)


# national wellbeing ------------------------------------------------------

# National Wellbeing Index
# The economic situation in New Zealand.
# The social conditions in New Zealand.
# Business in New Zealand.

## fit
fit_NWI_st = function(formula) {
  with(ccu, glm(
    as.formula(paste("NWI_lead2_z~ bs(Religion.CongregationSize_lead1_log)+",
                     paste(baselinevars,
                           collapse = "+")))
  ))
}

# pooled model
m29_NWI_st <- fit_NWI_st()
main = "National Well Being"
ylab = "National Well Being (SD)"
out_NWI <- pool_stglm(m29_NWI_st, df = df, m = m,  X = X, x = x)

plot_stglm(out_NWI, ylim, main, xlab, ylab)
ggplot_stglm(out_NWI, ylim, main, xlab, ylab)

out_cr <- pool_stglm_contrast(m29_NWI_st, df = df, m = m,  X = X, x = x, r= r)
ggplot_stglm_contrast(out_cr, ylim, main, xlab, ylab)




# PWI -------------------------------------------------------------

## fit
fit_PWI_st= function(formula) {
  with(ccu, glm(
    as.formula(paste("PWI_lead2_z~ bs(Religion.CongregationSize_lead1_log)+",
                     paste(baselinevars,
                           collapse = "+")))
  ))
}

# pooled model
m33_fit_PWI_st <- fit_PWI_st()
main = "PWI"
ylab = "PWI (SD)"
out_p <- pool_stglm(m33_fit_PWI_st, df = df, m = m,  X = X, x = x)

plot_stglm(out_p, ylim, main, xlab, ylab)
ggplot_stglm(out_p, ylim, main, xlab, ylab)

out_cr <- pool_stglm_contrast(m33_fit_PWI_st, df = df, m = m,  X = X, x = x, r= r)
plot_stglm_contrast(out_cr, ylim, main, xlab, ylab)
ggplot_stglm_contrast(out_cr, ylim, main, xlab, ylab)
out_cr



# life sat ----------------------------------------------------------------
# Satisfaction with life
# I am satisfied with my life.
# In most ways my life is close to ideal.

## fit
fit_LIFESAT_st = function(formula) {
  with(ccu, glm(
    as.formula(paste("LIFESAT_lead2_z~ bs(Religion.CongregationSize_lead1_log)+",
                     paste(baselinevars,
                           collapse = "+")))
  ))
}
# pooled model
m37_LIFESAT_st  <- fit_LIFESAT_st()

main = "Life Satisfaction"
ylab = "Life Satisfaction (SD)"
out_p <- pool_stglm(m37_LIFESAT_st, df = df, m = m,  X = X, x = x)

plot_stglm(out_p, ylim = ylim, main, xlab, ylab)
ggplot_stglm(out_p, ylim = ylim, main, xlab, ylab)

out_cr <- pool_stglm_contrast(m37_LIFESAT_st, df = df, m = m,  X = X, x = x, r= r)
ggplot_stglm_contrast(out_cr, ylim, main, xlab, ylab)

# increase
round( EValue::evalues.OLS(  , se =    , sd = 1, delta = 3, true = 0), 3)
# decrease
round( EValue::evalues.OLS(   , se =   , sd = 1, delta = 3, true = 0), 3)


# Promotion NZSEI ---------------------------------------------------------------
## fit
fit_NZSEI06_st = function(formula) {
  with(ccu, glm(
    as.formula(paste("NZSEI06_lead2_10 ~ bs(Religion.CongregationSize_lead1_log)+",
                     paste(baselinevars,
                           collapse = "+")))
  ))
}
# pooled model
m38_NZSEI06  <- fit_NZSEI06_st()

main = "Occupational Success/10"
ylab = "Occupational Success (/10)"
out_p <- pool_stglm(m38_NZSEI06, df = df, m = m,  X = X, x = x)

plot_stglm(out_p, ylim = ylim, main, xlab, ylab)
ggplot_stglm(out_p, ylim = ylim, main, xlab, ylab)

out_cr <- pool_stglm_contrast(m38_NZSEI06, df = df, m = m,  X = X, x = x, r= r)
ggplot_stglm_contrast(out_cr, ylim, main, xlab, ylab)

# increase
round( EValue::evalues.OLS(  , se =    , sd = 1, delta = 3, true = 0), 3)
# decrease
round( EValue::evalues.OLS(   , se =   , sd = 1, delta = 3, true = 0), 3)


# prayer ------------------------------------------------------------------

fit = function(formula) {
  with(ccu, glm(
    as.formula(paste("Religion.Prayer_lead2_log_z ~ bs(Religion.CongregationSize_lead1_log)+",
                     paste(baselinevars,
                           collapse = "+")))
  ))
}
# pooled model
fit_0  <- fit()

main = "Religion Prayer log (SD)"
ylab = "Religion Prayer log (SD)"
out_p <- pool_stglm(fit_0, df = df, m = m,  X = X, x = x)

plot_stglm(out_p, ylim = ylim, main, xlab, ylab)
ggplot_stglm(out_p, ylim = ylim, main, xlab, ylab)

out_cr <- pool_stglm_contrast(fit_0, df = df, m = m,  X = X, x = x, r= r)
ggplot_stglm_contrast(out_cr, ylim, main, xlab, ylab)

# increase
round( EValue::evalues.OLS(  , se =    , sd = 1, delta = 3, true = 0), 3)
# decrease
round( EValue::evalues.OLS(   , se =   , sd = 1, delta = 3, true = 0), 3)



# scripture ---------------------------------------------------------------

fit = function(formula) {
  with(ccu, glm(
    as.formula(paste("Religion.Scripture_lead2_log_z ~ bs(Religion.CongregationSize_lead1_log)+",
                     paste(baselinevars,
                           collapse = "+")))
  ))
}
# pooled model
fit_0  <- fit()

main = "Religion Scripture log (SD)"
ylab = "Religion Scripture log (SD)"
out_p <- pool_stglm(fit_0, df = df, m = m,  X = X, x = x)

plot_stglm(out_p, ylim = ylim, main, xlab, ylab)
ggplot_stglm(out_p, ylim = ylim, main, xlab, ylab)

out_cr <- pool_stglm_contrast(fit_0, df = df, m = m,  X = X, x = x, r= r)
ggplot_stglm_contrast(out_cr, ylim, main, xlab, ylab)

# increase
round( EValue::evalues.OLS(  , se =    , sd = 1, delta = 3, true = 0), 3)
# decrease
round( EValue::evalues.OLS(   , se =   , sd = 1, delta = 3, true = 0), 3)




# church ------------------------------------------------------------------
fit = function(formula) {
  with(ccu, glm(
    as.formula(paste("Church_lead2 ~ bs(Religion.CongregationSize_lead1_log)+",
                     paste(baselinevars,
                           collapse = "+")))
  ))
}
# pooled model
fit_0  <- fit()

main = "Religion Church log (SD)"
ylab = "Religion Church log (SD)"
out_p <- pool_stglm(fit_0, df = df, m = m,  X = X, x = x)

plot_stglm(out_p, ylim = ylim, main, xlab, ylab)
ggplot_stglm(out_p, ylim=c(0,8), main, xlab, ylab)

out_cr <- pool_stglm_contrast(fit_0, df = df, m = m,  X = X, x = x, r= r)
ggplot_stglm_contrast(out_cr, ylim=c(0,2), main, xlab, ylab)

# increase
round( EValue::evalues.OLS(  , se =    , sd = 1, delta = 3, true = 0), 3)
# decrease
round( EValue::evalues.OLS(   , se =   , sd = 1, delta = 3, true = 0), 3)



# SUPPORT on wellbeing ---------------------------------------------
# set up vars2 -------------------------------------------------------------
ylim <- c(-1,1)
xlab <- "Social Support (SD)"
df <-  ccu
m = 10
X = "SUPPORT_lead1_z"
x = -1:1
r = -1

## SFHEALTH
fit_0 = function(formula) {
  with(ccu, glm(
    as.formula(paste("SFHEALTH_lead2_z~ bs(SUPPORT_lead1_z)+",
                     paste(baselinevars,
                           collapse = "+")))
  ))
}
# pooled model
out_0 <- fit_0()
summary(pool(out_0))
main = "SF Health"
ylab = "SF Health (SD)"
out_p <- pool_stglm(out_0, df = df, m = m,  X = X, x = x)

plot_stglm(out_p, ylim, main, xlab, ylab)
ggplot_stglm(out_p, ylim, main, xlab, ylab)

out_cr <- pool_stglm_contrast(out_0, df = df, m = m,  X = X, x = x, r= r)
plot_stglm_contrast(out_cr, ylim, main, xlab, ylab)
ggplot_stglm_contrast(out_cr, ylim, main, xlab, ylab)


##Fatigue HLTH.Fatigue_lead2_z
fit_0 = function(formula) {
  with(ccu, glm(
    as.formula(paste("HLTH.Fatigue_lead2_z~ bs(SUPPORT_lead1_z)+",
                     paste(baselinevars,
                           collapse = "+")))
  ))
}
# pooled model
out_0 <- fit_0()
summary(pool(out_0))
main = "Fatigue"
ylab = "Support (SD)"
out_p <- pool_stglm(out_0, df = df, m = m,  X = X, x = x)

plot_stglm(out_p, ylim, main, xlab, ylab)
ggplot_stglm(out_p, ylim, main, xlab, ylab)

out_cr <- pool_stglm_contrast(out_0, df = df, m = m,  X = X, x = x, r= r)
plot_stglm_contrast(out_cr, ylim, main, xlab, ylab)
ggplot_stglm_contrast(out_cr, ylim, main, xlab, ylab)

##KESSLER6sum_lead2_z
fit_0 = function(formula) {
  with(ccu, glm(
    as.formula(paste("KESSLER6sum_lead2_z~ bs(SUPPORT_lead1_z)+",
                     paste(baselinevars,
                           collapse = "+")))
  ))
}
# pooled model
out_0 <- fit_0()
summary(pool(out_0))
main = "Kessler 6 Distress (SD)"
ylab = "Support (SD)"
out_p <- pool_stglm(out_0, df = df, m = m,  X = X, x = x)

plot_stglm(out_p, ylim, main, xlab, ylab)
ggplot_stglm(out_p, ylim, main, xlab, ylab)

out_cr <- pool_stglm_contrast(out_0, df = df, m = m,  X = X, x = x, r= r)
plot_stglm_contrast(out_cr, ylim, main, xlab, ylab)
ggplot_stglm_contrast(out_cr, ylim, main, xlab, ylab)



## PWI
fit_0 = function(formula) {
  with(ccu, glm(
    as.formula(paste("PWI_lead2_z~ bs(SUPPORT_lead1_z)+",
                     paste(baselinevars,
                           collapse = "+")))
  ))
}
# pooled model
out_0 <- fit_0()
summary(pool(out_0))
main = "PWI"
ylab = "SUPPORT (SD)"
out_p <- pool_stglm(out_0, df = df, m = m,  X = X, x = x)

plot_stglm(out_p, ylim, main, xlab, ylab)
ggplot_stglm(out_p, ylim, main, xlab, ylab)

out_cr <- pool_stglm_contrast(out_0, df = df, m = m,  X = X, x = x, r= r)
plot_stglm_contrast(out_cr, ylim, main, xlab, ylab)
ggplot_stglm_contrast(out_cr, ylim, main, xlab, ylab)


## LIFE SAT
fit_0 = function(formula) {
  with(ccu, glm(
    as.formula(paste("LIFESAT_lead2_z~ bs(SUPPORT_lead1_z)+",
                     paste(baselinevars,
                           collapse = "+")))
  ))
}

# pooled model
out_0 <- fit_0()
summary(pool(out_0))
main = "Life Satisfaction (SD)"
ylab = "SUPPORT (SD)"
out_p <- pool_stglm(out_0, df = df, m = m,  X = X, x = x)

plot_stglm(out_p, ylim, main, xlab, ylab)
ggplot_stglm(out_p, ylim, main, xlab, ylab)

out_cr <- pool_stglm_contrast(out_0, df = df, m = m,  X = X, x = x, r= r)
plot_stglm_contrast(out_cr, ylim, main, xlab, ylab)
ggplot_stglm_contrast(out_cr, ylim, main, xlab, ylab)

### BELONG_lead2_z
fit_0 = function(formula) {
  with(ccu, glm(
    as.formula(paste("BELONG_lead2_z~ bs(SUPPORT_lead1_z)+",
                     paste(baselinevars,
                           collapse = "+")))
  ))
}

# pooled model
out_0 <- fit_0()
main = "Social Belonging (SD)"
ylab = "SUPPORT (SD)"
out_p <- pool_stglm(out_0, df = df, m = m,  X = X, x = x)

plot_stglm(out_p, ylim, main, xlab, ylab)
ggplot_stglm(out_p, ylim, main, xlab, ylab)

out_cr <- pool_stglm_contrast(out_0, df = df, m = m,  X = X, x = x, r= r)
plot_stglm_contrast(out_cr, ylim, main, xlab, ylab)
ggplot_stglm_contrast(out_cr, ylim, main, xlab, ylab)

# hours-work --------------------------------------------------------------

library(MatchThem)
1library(optmatch)
models_hw <- weightthem(Hours.Work_lead1_z ~
                          Hours.Work_z +
                          AGREEABLENESS_z +
                          CONSCIENTIOUSNESS_z +
                          EXTRAVERSION_z  +
                          HONESTY_HUMILITY_z +
                          NEUROTICISM_z +
                          OPENNESS_z +
                          Age_z +
                          Alcohol.Frequency_z + #
                          Alcohol.Intensity_log_z + #
                          Bodysat_z +
                          Believe.God_z +
                          Believe.Spirit_z +
                          BELONG_z + #
                          CharityDonate_log_z + #
                          ChildrenNum_z +
                          Church_z +
                          community +
                          Edu_z +
                          Employed_z +
                          EmotionRegulation1_z +
                          EmotionRegulation2_z +
                          EmotionRegulation3_z +
                          Euro_z +
                          GRATITUDE_z +
                          HomeOwner_z +
                          Hours.Exercise_log_z +
                          # Hours.Work_z +
                          HLTH.BMI_z  + #
                          HLTH.Fatigue_z + #
                          income_log_z +
                          ImpermeabilityGroup_z +
                          KESSLER6sum_z + #
                          LIFEMEANING_z + #
                          LIFESAT_z + #
                          Male_z +
                          NZdep_z +
                          NWI_z +
                          NZSEI06_z +
                          Parent_z +
                          Partner_z +
                          PERFECTIONISM_z +
                          PermeabilityIndividual_z +
                          Pol.Orient_z +
                          POWERDEPENDENCE1_z + #
                          POWERDEPENDENCE2_z + #
                          # PWI_z +
                          Relid_z +
                          Respect.Self_z + #
                          Rumination_z + #
                          SELF.CONTROL_z + #
                          SELF.ESTEEM_z + #
                          SexualSatisfaction_z +#
                          SFHEALTH_z +#
                          Smoker_z +#
                          Spiritual.Identification_z +
                          Standard.Living_z +
                          SUPPORT_z +#
                          Urban_z +
                          VENGEFUL.RUMIN_z +
                          Volunteers_z +
                          Your.Health_z +
                          Your.Future.Security_z +
                          Your.Personal.Relationships_z,
                        out2_sl,
                        approach = "within",
                        estimand = "ATE",
                        stabilize = TRUE,
                        method = "ebal")


saveh(models_hw,"models_hw")


sum<- summary(models_hw)
plot(sum)
sum
bal.tab(models_hw)


ctrim_st <- trim(models_hw, at = .998)
bal.tab(ctrim_st)
summary(ctrim_st)


# iptw models  STANDARD LIVING -------------------------------------------------------------
# no need to trim


out <- with(ctrim_st, glm( HLTH.BMI_lead2_z ~ Hours.Work_lead1_z ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

round( EValue::evalues.OLS( , se = , sd = 1, delta = 2, true = 0), 3)
round( EValue::evalues.RR(, lo =  , hi =, true = 1), 3)


out <- with(ctrim_st, glm(SFHEALTH_lead2_z  ~ Hours.Work_lead1_z ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

round( EValue::evalues.OLS( , se = , sd = 1, delta = 2, true = 0), 3)
round( EValue::evalues.RR(, lo =  , hi =, true = 1), 3)


out <- with(ctrim_st, glm(Hours.Exercise_lead2_log_z ~ Hours.Work_lead1_z, family = "gaussian" ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

round( EValue::evalues.OLS( , se = , sd = 1, delta = 2, true = 0), 3)
round( EValue::evalues.RR(, lo =  , hi =, true = 1), 3)

out <- with(ctrim_st, glm( Smoker_lead2 ~ Hours.Work_lead1_z, family = "poisson"))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)
exp(-3.02-.14)


round( EValue::evalues.OLS( , se = , sd = 1, delta = 2, true = 0), 3)
round( EValue::evalues.RR(, lo =  , hi =, true = 1), 3)

out <- with(ctrim_st, glm(HLTH.Fatigue_lead2ord ~ Hours.Work_lead1_z ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

round( EValue::evalues.OLS( 0.01023787, se = 0.003899096, sd = 1, delta = 2, true = 0), 3)
round( EValue::evalues.RR(, lo =  , hi =, true = 1), 3)


out <- with(ctrim_st, glm(Alcohol.Frequency_lead2ord_z ~ Hours.Work_lead1_z ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

round( EValue::evalues.OLS( 0.01264201, se = 0.005129188, sd = 1, delta = 2, true = 0), 3)
round( EValue::evalues.RR(, lo =  , hi =, true = 1), 3)

out <- with(ctrim_st, glm(Alcohol.Intensity_lead2_z ~ Hours.Work_lead1_z ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

round( EValue::evalues.OLS( , se = , sd = 1, delta = 2, true = 0), 3)
round( EValue::evalues.RR(, lo =  , hi =, true = 1), 3)

out <- with(ctrim_st, glm(Bodysat_lead2_z ~ Hours.Work_lead1_z ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

round( EValue::evalues.OLS( -0.007741457, se = 0.003929686, sd = 1, delta = 2, true = 0), 3)
round( EValue::evalues.RR(, lo =  , hi =, true = 1), 3)

# out <- with(ctrim, glm(PWI_lead2_z ~ Standard.Living_lead1_z ))
# output <- pool(out, dfcom = NULL)
# summary(output, conf.int = TRUE)

out <- with(ctrim_st, glm( Rumination_lead2ord_z ~ Hours.Work_lead1_z ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

round( EValue::evalues.OLS( , se = , sd = 1, delta = 2, true = 0), 3)
round( EValue::evalues.RR(, lo =  , hi =, true = 1), 3)


out <- with(ctrim_st, glm(SexualSatisfaction_lead2_z ~ Hours.Work_lead1_z ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

round( EValue::evalues.OLS( 0.009236795 , se = 0.004273584 , sd = 1, delta = 2, true = 0), 3)
round( EValue::evalues.RR(, lo =  , hi =, true = 1), 3)

out <- with(ctrim_st, glm(EmotionRegulation1_lead2_z ~ Hours.Work_lead1_z ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

round( EValue::evalues.OLS( , se = , sd = 1, delta = 2, true = 0), 3)
round( EValue::evalues.RR(, lo =  , hi =, true = 1), 3)

out <- with(ctrim_st, glm(EmotionRegulation2_lead2_z~ Hours.Work_lead1_z ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

round( EValue::evalues.OLS( , se = , sd = 1, delta = 2, true = 0), 3)
round( EValue::evalues.RR(, lo =  , hi =, true = 1), 3)

out <- with(ctrim_st, glm( EmotionRegulation3_lead2_z ~ Hours.Work_lead1_z ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

round( EValue::evalues.OLS( , se = , sd = 1, delta = 2, true = 0), 3)
round( EValue::evalues.RR(, lo =  , hi =, true = 1), 3)

out <- with(ctrim_st, glm(KESSLER6sum_lead2_z ~ Hours.Work_lead1_z , family = "gaussian"))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

round( EValue::evalues.OLS(-0.006205853 , se = 0.002415414, sd = 1, delta = 2, true = 0), 3)
round( EValue::evalues.RR(, lo =  , hi =, true = 1), 3)


out <- with(ctrim_st, glm(POWERDEPENDENCE1_lead2_z ~ Hours.Work_lead1_z ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

round( EValue::evalues.OLS( , se = , sd = 1, delta = 2, true = 0), 3)
round( EValue::evalues.RR(, lo =  , hi =, true = 1), 3)

out <- with(ctrim_st, glm(POWERDEPENDENCE1_lead2_z ~ Hours.Work_lead1_z ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

round( EValue::evalues.OLS( , se = , sd = 1, delta = 2, true = 0), 3)
round( EValue::evalues.RR(, lo =  , hi =, true = 1), 3)

out <- with(ctrim_st, glm(PERFECTIONISM_lead2_z ~ Hours.Work_lead1_z ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

round( EValue::evalues.OLS( , se = , sd = 1, delta = 2, true = 0), 3)
round( EValue::evalues.RR(, lo =  , hi =, true = 1), 3)

out <- with(ctrim_st, glm(SELF.ESTEEM_lead2_z ~ Hours.Work_lead1_z ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

round( EValue::evalues.OLS( , se = , sd = 1, delta = 2, true = 0), 3)
round( EValue::evalues.RR(, lo =  , hi =, true = 1), 3)

out <- with(ctrim_st, glm( GRATITUDE_lead2_z ~ Hours.Work_lead1_z ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

round( EValue::evalues.OLS( , se = , sd = 1, delta = 2, true = 0), 3)
round( EValue::evalues.RR(, lo =  , hi =, true = 1), 3)

out <- with(ctrim_st, glm( VENGEFUL.RUMIN_lead2_z ~ Hours.Work_lead1_z ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

round( EValue::evalues.OLS( , se = , sd = 1, delta = 2, true = 0), 3)
round( EValue::evalues.RR(, lo =  , hi =, true = 1), 3)

out <- with(ctrim_st, glm( LIFEMEANING_lead2_z ~ Hours.Work_lead1_z ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)


round( EValue::evalues.OLS( , se = , sd = 1, delta = 2, true = 0), 3)
round( EValue::evalues.RR(, lo =  , hi =, true = 1), 3)


out <- with(ctrim_st, glm( HONESTY_HUMILITY_lead2_z ~ Hours.Work_lead1_z ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

round( EValue::evalues.OLS( , se = , sd = 1, delta = 2, true = 0), 3)
round( EValue::evalues.RR(, lo =  , hi =, true = 1), 3)

out <- with(ctrim_st, glm( BELONG_lead2_z ~ Hours.Work_lead1_z ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)


round( EValue::evalues.OLS( , se = , sd = 1, delta = 2, true = 0), 3)
round( EValue::evalues.RR(, lo =  , hi =, true = 1), 3)

out <- with(ctrim_st, glm( SUPPORT_lead2_z ~ bs(Religion.CongregationSize_lead1_log) + ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)


round( EValue::evalues.OLS( , se = , sd = 1, delta = 2, true = 0), 3)
round( EValue::evalues.RR(, lo =  , hi =, true = 1), 3)


out <- with(ctrim_st, glm( Volunteers_lead2 ~ bs(Religion.CongregationSize_lead1_log) + ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

round( EValue::evalues.OLS( , se = , sd = 1, delta = 2, true = 0), 3)
round( EValue::evalues.RR(, lo =  , hi =, true = 1), 3)

out <- with(ctrim_st, glm( CharityDonate_lead2 ~ bs(Religion.CongregationSize_lead1_log) + , family = "poisson"))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)
exp( 6.9785313)
exp( 6.9785313 - 0.1001891 )
exp( 6.9785313 + 0.1001891 + 0.01807244)
exp( 6.9785313 + 0.1001891 - 0.01807244)



round( EValue::evalues.OLS( , se = , sd = 1, delta = 2, true = 0), 3)
round( EValue::evalues.RR(0.9046663, lo = 0.8884637, hi = 0.9211645, true = 1), 3)

exp(-0.1001891  - 6.9785313)/ exp(-6.9785313 )
exp(-0.1001891 + 0.01807244 - 6.9785313)/ exp(-6.9785313 )
exp(-0.1001891 - 0.01807244 - 6.9785313)/ exp(-6.9785313 )


out <- with(ctrim_st, glm(community_lead2_z ~ bs(Religion.CongregationSize_lead1_log) + ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

round( EValue::evalues.OLS( , se = , sd = 1, delta = 2, true = 0), 3)
round( EValue::evalues.RR(, lo =  , hi =, true = 1), 3)

out <- with(ctrim_st, glm(NWI_lead2_z~ bs(Religion.CongregationSize_lead1_log) + ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

round( EValue::evalues.OLS(0.025172099 , se = 0.007768097, sd = 1, delta = 2, true = 0), 3)
round( EValue::evalues.RR(, lo =  , hi =, true = 1), 3)

out <- with(ctrim_st, glm(ImpermeabilityGroup_lead2 ~ bs(Religion.CongregationSize_lead1_log) + ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

round( EValue::evalues.OLS( , se = , sd = 1, delta = 2, true = 0), 3)
round( EValue::evalues.RR(, lo =  , hi =, true = 1), 3)


out <- with(ctrim_st, glm( Standard.Living_lead2ord_z ~ bs(Religion.CongregationSize_lead1_log) + ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

round( EValue::evalues.OLS( , se = , sd = 1, delta = 2, true = 0), 3)
round( EValue::evalues.RR(, lo =  , hi =, true = 1), 3)


out <- with(ctrim_st, glm( Your.Future.Security_lead2_z ~ bs(Religion.CongregationSize_lead1_log) + ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

round( EValue::evalues.OLS( , se = , sd = 1, delta = 2, true = 0), 3)
round( EValue::evalues.RR(, lo =  , hi =, true = 1), 3)

out <- with(ctrim_st, glm( Your.Health_lead2_z  ~ bs(Religion.CongregationSize_lead1_log) + ))

output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

round( EValue::evalues.OLS( , se = , sd = 1, delta = 2, true = 0), 3)
round( EValue::evalues.RR(, lo =  , hi =, true = 1), 3)

out <- with(ctrim_st, glm( Your.Personal.Relationships_lead2ord_z ~ bs(Religion.CongregationSize_lead1_log) + ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

round( EValue::evalues.OLS( , se = , sd = 1, delta = 2, true = 0), 3)
round( EValue::evalues.RR(, lo =  , hi =, true = 1), 3)


# NZSEI06 -- status variable



# Weights but now with the PWI  variables individually

library(MatchThem)
library(optmatch)
library(splines)
models_sl <- weightthem(Standard.Living_lead1_z ~
                          bs(Standard.Living_z) +
                          AGREEABLENESS_z +
                          CONSCIENTIOUSNESS_z +
                          EXTRAVERSION_z  +
                          HONESTY_HUMILITY_z +
                          NEUROTICISM_z +
                          OPENNESS_z +
                          Age_z +
                          Alcohol.Frequency_z + #
                          Alcohol.Intensity_log_z + #
                          Bodysat_z +
                          Believe.God_z +
                          Believe.Spirit_z +
                          BELONG_z + #
                          CharityDonate_log_z + #
                          ChildrenNum_z +
                          Church_z +
                          community +
                          Edu_z +
                          Employed_z +
                          EmotionRegulation1_z +
                          EmotionRegulation2_z +
                          EmotionRegulation3_z +
                          Euro_z +
                          GRATITUDE_z +
                          HomeOwner_z +
                          Hours.Exercise_log_z +
                          Hours.Work_z +
                          HLTH.BMI_z  + #
                          HLTH.Fatigue_z + #
                          income_log_z +
                          ImpermeabilityGroup_z +
                          KESSLER6sum_z + #
                          LIFEMEANING_z + #
                          LIFESAT_z + #
                          Male_z +
                          NZdep_z +
                          NWI_z +
                          NZSEI06_z +
                          Parent_z +
                          Partner_z +
                          PERFECTIONISM_z +
                          PermeabilityIndividual_z +
                          Pol.Orient_z +
                          POWERDEPENDENCE1_z + #
                          POWERDEPENDENCE2_z + #
                          # PWI_z +
                          Relid_z +
                          Respect.Self_z + #
                          Rumination_z + #
                          SELF.CONTROL_z + #
                          SELF.ESTEEM_z + #
                          SexualSatisfaction_z +#
                          SFHEALTH_z +#
                          Smoker_z +#
                          Spiritual.Identification_z +
                          #  Standard.Living_z +
                          SUPPORT_z +#
                          Urban_z +
                          VENGEFUL.RUMIN_z +
                          Volunteers_z +
                          Your.Health_z +
                          Your.Future.Security_z +
                          Your.Personal.Relationships_z,
                        out2_sl,
                        approach = "within",
                        estimand = "ATE",
                        stabilize = TRUE,
                        method = "ebal")


saveh(models_sl,"models_sl.rds")


sum<- summary(models_sl)
plot(sum)
sum
bal.tab(models_sl)


ctrim <- trim(models_sl, at = .999)
bal.tab(ctrim)
summary(ctrim)


# iptw models -------------------------------------------------------------
# no need to trim


out <- with(ctrim, glm( HLTH.BMI_lead2_z ~ Standard.Living_lead1_z ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

out <- with(ctrim, glm(SFHEALTH_lead2_z  ~ Standard.Living_lead1_z ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

out <- with(ctrim, glm(Hours.Exercise_lead2 ~ Standard.Living_lead1_z, family = "poisson" ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

out <- with(ctrim, glm( Smoker_lead2 ~ Standard.Living_lead1_z , family = "poisson"))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)


out <- with(models_sl, glm(HLTH.Fatigue_lead2ord_z ~ bs(Standard.Living_lead1_z) ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

library(splines)
out2 <-  with(
  long3,
  #  long_f3,
  glm(
    HLTH.Fatigue_lead2ord_z ~  bs(Standard.Living_lead1_z) +
      Standard.Living_z +
      AGREEABLENESS_z +
      CONSCIENTIOUSNESS_z +
      EXTRAVERSION_z  +
      HONESTY_HUMILITY_z +
      NEUROTICISM_z +
      OPENNESS_z +
      Age_z +
      Alcohol.Frequency_z + #
      Alcohol.Intensity_log_z + #
      Bodysat_z +
      Believe.God_z +
      Believe.Spirit_z +
      BELONG_z + #
      CharityDonate_log_z + #
      ChildrenNum_z +
      Church_z +
      community +
      Edu_z +
      Employed_z +
      EmotionRegulation1_z +
      EmotionRegulation2_z +
      EmotionRegulation3_z +
      Euro_z +
      GRATITUDE_z +
      HomeOwner_z +
      Hours.Exercise_log_z +
      Hours.Work_z +
      HLTH.BMI_z  + #
      HLTH.Fatigue_z + #
      #  income_log_z +
      ImpermeabilityGroup_z +
      KESSLER6sum_z + #
      LIFEMEANING_z + #
      LIFESAT_z + #
      Male_z +
      NZdep_z +
      NWI_z +
      NZSEI06_z +
      Parent_z +
      Partner_z +
      PERFECTIONISM_z +
      PermeabilityIndividual_z +
      Pol.Orient_z +
      POWERDEPENDENCE1_z + #
      POWERDEPENDENCE2_z + #
      # PWI_z +
      Relid_z +
      Respect.Self_z + #
      Rumination_z + #
      SELF.CONTROL_z + #
      SELF.ESTEEM_z + #
      SexualSatisfaction_z + #
      SFHEALTH_z + #
      Smoker_z + #
      Spiritual.Identification_z +
      SUPPORT_z + #
      Urban_z +
      VENGEFUL.RUMIN_z +
      Volunteers_z +
      Your.Health_z +
      Your.Future.Security_z +
      Your.Personal.Relationships_z))
library(stdReg)
options(scipen=999)

summary(out2)

output2 <- pool(out2, dfcom = NULL)
output2
#out2$df.null <- 27072

gform_m1<- stdGlm(fit = out2, data = long3, X  = "Standard.Living_lead1_z", x =seq(-1,1))
summary(gform_m1)
plot(gform_m1)

Estimate Std. Error lower 0.95 upper 0.95
-1  0.02876    0.00347   0.021970    0.03556
0   0.00491    0.00238   0.000244    0.00957
1  -0.03225    0.00326  -0.038644   -0.02586

stimate Std. Error lower 0.95 upper 0.95
-1   0.0306    0.00337   0.023963    0.03716
0    0.0050    0.00230   0.000485    0.00952
1   -0.0330    0.00316  -0.039235   -0.02684

out <- with(ctrim, glm(Alcohol.Frequency_lead2ord ~ Standard.Living_lead1_z ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

out <- with(ctrim, glm(Alcohol.Intensity_lead2~ Standard.Living_lead1_z ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

out <- with(ctrim, glm(Bodysat_lead2_z ~ Standard.Living_lead1_z ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

# out <- with(ctrim, glm(PWI_lead2_z ~ Standard.Living_lead1_z ))
# output <- pool(out, dfcom = NULL)
# summary(output, conf.int = TRUE)

out <- with(ctrim, glm( Rumination_lead2ord ~ Standard.Living_lead1_z ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)


out <- with(ctrim, glm(SexualSatisfaction_lead2_z ~ Standard.Living_lead1_z ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)


out <- with(ctrim, glm(EmotionRegulation1_lead2_z ~ Standard.Living_lead1_z ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

out <- with(ctrim, glm(EmotionRegulation2_lead2_z~ Standard.Living_lead1_z ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

out <- with(ctrim, glm( EmotionRegulation3_lead2_z ~ Standard.Living_lead1_z ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

out <- with(ctrim, glm(KESSLER6sum_lead2 ~ Standard.Living_lead1_z , family = "poisson"))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

out <- with(ctrim, glm(LIFEMEANING_lead2ord_z ~ Standard.Living_lead1_z , family = "gaussian"))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)




Estimate Std. Error lower 0.95 upper 0.95
-1 -0.09386    0.00340   -0.10053  -0.087199
0  -0.00529    0.00222   -0.00965  -0.000932
1   0.08709    0.00294    0.08134   0.092848

out <- with(ctrim, glm(POWERDEPENDENCE1_lead2_z ~ Standard.Living_lead1_z ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

out <- with(ctrim, glm(POWERDEPENDENCE1_lead2_z ~ Standard.Living_lead1_z ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

out <- with(ctrim, glm(PERFECTIONISM_lead2_z ~ Standard.Living_lead1_z ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

out <- with(ctrim, glm(SELF.ESTEEM_lead2_z ~ Standard.Living_lead1_z ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

out <- with(ctrim, glm( GRATITUDE_lead2_z ~ Standard.Living_lead1_z ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

out <- with(ctrim, glm( VENGEFUL.RUMIN_lead2_z ~ Standard.Living_lead1_z ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

out <- with(ctrim, glm( LIFEMEANING_lead2ord ~ Standard.Living_lead1_z ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

out <- with(ctrim, glm( HONESTY_HUMILITY_lead2_z ~ Standard.Living_lead1_z ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

out <- with(ctrim, glm( BELONG_lead2_z ~ Standard.Living_lead1_z ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

out <- with(ctrim, glm( SUPPORT_lead2_z ~ Standard.Living_lead1_z ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

out <- with(ctrim, glm( Volunteers_lead2 ~ Standard.Living_lead1_z ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

out <- with(ctrim, glm( CharityDonate_log_lead2_z ~ Standard.Living_lead1_z , family = "gaussian"))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)
exp( 6.867947 + 0.2492043)


out <- with(ctrim, glm(community_lead2_z ~ Standard.Living_lead1_z ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

out <- with(ctrim, glm(NWI_lead2_z~ Standard.Living_lead1_z ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

out <- with(ctrim, glm(ImpermeabilityGroup_lead2 ~ Standard.Living_lead1_z ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)


out <- with(ctrim, glm(PermeabilityIndividual_lead2_z ~ Standard.Living_lead1_z ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)



# out <- with(ctrim2, glm( Standard.Living_lead2ord ~ Standard.Living_lead1_z ))
# output <- pool(out, dfcom = NULL)
# summary(output, conf.int = TRUE)


out <- with(ctrim, glm( Your.Future.Security_lead2_z ~ Standard.Living_lead1_z ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

out <- with(ctrim, glm( Your.Health_lead2_z  ~ Standard.Living_lead1_z ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

out <- with(ctrim, glm( Your.Personal.Relationships_lead2ord ~ Standard.Living_lead1_z ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)










# BRMS --------------------------------------------------------------------



# create list of data frames

out_c <- complete(ctrim, action ="long", include = FALSE, mild = TRUE)

m <- 10
listdat<- list()
for (i in 1:m) {
  listdat[[i]] <- as.data.frame(out_c[[i]])
}

# create list of data frames

out_c2 <- complete(ctrim2, action ="long", include = FALSE, mild = TRUE)

m <- 10
listdat2<- list()
for (i in 1:m) {
  listdat2[[i]] <- as.data.frame(out_c2[[i]])
}


# enable memory
options(future.globals.maxSize = 8000 * 1024^2)  # needed



# BRMS MODELS forms -------------------------------------------------------------

bf_HLTH.BMI_lead2_z <- bf(HLTH.BMI_lead2_z |weights(weights) ~ Standard.Living_lead1_z)
bf_SFHEALTH_lead2_z <- bf(SFHEALTH_lead2_z |weights(weights) ~ Standard.Living_lead1_z)
bf_Hours.Exercise_lead2 <- bf(Hours.Exercise_lead2 |weights(weights) ~ Standard.Living_lead1_z)
bf_Smoker_lead2 <- bf( Smoker_lead2 |weights(weights) ~ Standard.Living_lead1_z)
bf_HLTH.Fatigue_lead2ord <- bf( HLTH.Fatigue_lead2ord |weights(weights) ~ Standard.Living_lead1_z)
bf_Alcohol.Frequency_lead2ord <- bf( Alcohol.Frequency_lead2ord |weights(weights) ~ Standard.Living_lead1_z)
bf_Alcohol.Intensity_lead2 <- bf( as.integer(Alcohol.Intensity_lead2) |weights(weights) ~ Standard.Living_lead1_z)
bf_Bodysat_lead2_z <- bf( Bodysat_lead2_z |weights(weights) ~ Standard.Living_lead1_z)
bf_PWI_lead2_z <- bf( PWI_lead2_z |weights(weights) ~ Standard.Living_lead1_z)
bf_Rumination_lead2ord <- bf(Rumination_lead2ord |weights(weights) ~ Standard.Living_lead1_z)
bf_SexualSatisfaction_lead2_z <- bf(SexualSatisfaction_lead2_z |weights(weights) ~ Standard.Living_lead1_z)
bf_PWI_lead2_z <- bf(PWI_lead2_z |weights(weights) ~ Standard.Living_lead1_z)
bf_EmotionRegulation1_lead2_z <- bf(EmotionRegulation1_lead2_z |weights(weights) ~ Standard.Living_lead1_z)
bf_EmotionRegulation2_lead2_z <- bf(EmotionRegulation2_lead2_z |weights(weights) ~ Standard.Living_lead1_z)
bf_EmotionRegulation3_lead2_z <- bf(EmotionRegulation3_lead2_z |weights(weights) ~ Standard.Living_lead1_z)
bf_KESSLER6sum_lead2 <- bf(KESSLER6sum_lead2 |weights(weights) ~ Standard.Living_lead1_z)
bf_LIFESAT_lead2ord <- bf(LIFESAT_lead2ord |weights(weights) ~ Standard.Living_lead1_z)
bf_POWERDEPENDENCE_lead2_z <- bf(POWERDEPENDENCE_lead2_z |weights(weights) ~ Standard.Living_lead1_z)
bf_PERFECTIONISM_lead2_z <- bf(PERFECTIONISM_lead2_z |weights(weights) ~ Standard.Living_lead1_z)
bf_SELF.ESTEEM_lead2_z <- bf(SELF.ESTEEM_lead2_z |weights(weights) ~ Standard.Living_lead1_z)
bf_Emp.WorkLifeBalance_lead2_z <- bf( Emp.WorkLifeBalance_lead2_z |weights(weights) ~ Standard.Living_lead1_z)
bf_GRATITUDE_lead2_z <- bf( GRATITUDE_lead2_z |weights(weights) ~ Standard.Living_lead1_z)
bf_VENGEFUL.RUMIN_lead2ord <- bf(VENGEFUL.RUMIN_lead2ord  |weights(weights) ~ Standard.Living_lead1_z)
bf_LIFEMEANING_lead2ord <- bf(LIFEMEANING_lead2ord  |weights(weights) ~ Standard.Living_lead1_z)
bf_HONESTY_HUMILITY_lead2_z <- bf( HONESTY_HUMILITY_lead2_z |weights(weights) ~ Standard.Living_lead1_z)
bf_BELONG_lead2_z <- bf( BELONG_lead2_z  |weights(weights) ~ Standard.Living_lead1_z)
bf_SUPPORT_lead2ord <- bf( SUPPORT_lead2ord |weights(weights) ~ Standard.Living_lead1_z)
bf_Volunteers_lead2 <- bf( Volunteers_lead2 |weights(weights) ~ Standard.Living_lead1_z)
bf_CharityDonate_lead2 <- bf( CharityDonate_lead2 |weights(weights) ~ Standard.Living_lead1_z)
bf_community_lead2_z <- bf(community_lead2_z |weights(weights) ~ Standard.Living_lead1_z)
bf_NWI_lead2_z <- bf(NWI_lead2_z |weights(weights) ~ Standard.Living_lead1_z)
bf_ImpermeabilityGroup_z <- bf(ImpermeabilityGroup_z |weights(weights) ~ Standard.Living_lead1_z)
bf_PermeabilityIndividual_z<- bf( PermeabilityIndividual_z|weights(weights) ~ Standard.Living_lead1_z)


## ADD ONE

bf_Standard.Living_lead2ord<- bf( Standard.Living_lead2ord|weights(weights) ~ Standard.Living_lead1_z)
bf_Your.Health_lead2_z<- bf( Your.Health_lead2_z|weights(weights) ~ Standard.Living_lead1_z)
bf_Your.Future.Security_lead2 <- bf(Your.Future.Security_lead2 |weights(weights) ~ Standard.Living_lead1_z)
bf_Your.Personal.Relationships_lead2ord<- bf( Your.Personal.Relationships_lead2ord|weights(weights) ~ Standard.Living_lead1_z)




# bmi ---------------------------------------------------------------------

m1_bmi_stome <- brm_multiple(
  bf_HLTH.BMI_lead2_z,
  data = listdat,
  family = "gaussian",
  #  family = cumulative("probit"),  Chose family
  #  family = "poisson",
  #  family = "negbinomial",
  #  family = bernoulli(link = "cloglog"),
  seed = 1234,
  warmup = 1000,
  iter = 2000,
  chains = 4,
  init = 0,
  backend = "cmdstanr",
  set_prior('normal(0, 1)', class = 'b'),
  file = here::here("mods", "standardliving", "m1_bmi_stome.rds"),
)

m2__SFHEALTH_lead2_z <- brm_multiple(
  bf_SFHEALTH_lead2_z,
  data = listdat,
  family = "gaussian",
  #  family = cumulative("probit"),  Chose family
  #  family = "poisson",
  #  family = "negbinomial",
  #  family = bernoulli(link = "cloglog"),
  seed = 1234,
  warmup = 1000,
  iter = 2000,
  chains = 4,
  init = 0,
  backend = "cmdstanr",
  set_prior('normal(0, 1)', class = 'b'),
  file = here::here("mods", "standardliving", "m2_SFHEALTH_lead2_z.rds"),
)


m3_Hours.Exercise_lead2 <- brm_multiple(
  bf_Hours.Exercise_lead2 ,
  data = listdat,
  # family = "gaussian",
  #  family = cumulative("probit"),  Chose family
  #  family = "poisson",
  family = "negbinomial",
  #  family = bernoulli(link = "cloglog"),
  seed = 1234,
  warmup = 1000,
  iter = 2000,
  chains = 4,
  init = 0,
  backend = "cmdstanr",
  set_prior('normal(0, 1)', class = 'b'),
  file = here::here("mods", "standardliving", "m3_Hours.Exercise_lead2.rds"),
)

m4_Smoker_lead2<- brm_multiple(
  bf_Smoker_lead2,
  data = listdat,
  # family = "gaussian",
  #  family = cumulative("probit"),  Chose family
  #  family = "poisson",
  #  family = "negbinomial",
  family = bernoulli(link = "cloglog"),
  seed = 1234,
  warmup = 1000,
  iter = 2000,
  chains = 4,
  init = 0,
  backend = "cmdstanr",
  set_prior('normal(0, 1)', class = 'b'),
  file = here::here("mods", "standardliving", "m4_Smoker_lead2.rds"),
)

m5_HLTH.Fatigue_lead2ord <- brm_multiple(
  bf_HLTH.Fatigue_lead2ord ,
  data = listdat,
  # family = "gaussian",
  family = cumulative("probit"), # Chose family
  #  family = "poisson",
  #  family = "negbinomial",
  #  family = bernoulli(link = "cloglog"),
  seed = 1234,
  warmup = 1000,
  iter = 2000,
  chains = 4,
  init = 0,
  backend = "cmdstanr",
  set_prior('normal(0, 1)', class = 'b'),
  file = here::here("mods", "standardliving", "m5_HLTH.Fatigue_lead2ord.rds"),
)

m6_Alcohol.Frequency_lead2ord <- brm_multiple(
  bf_Alcohol.Frequency_lead2ord ,
  data = listdat,
  # family = "gaussian",
  family = cumulative("probit"),
  #  family = "poisson",
  #  family = "negbinomial",
  #  family = bernoulli(link = "cloglog"),
  seed = 1234,
  warmup = 1000,
  iter = 2000,
  chains = 4,
  init = 0,
  backend = "cmdstanr",
  set_prior('normal(0, 1)', class = 'b'),
  file = here::here("mods", "standardliving", "m6_Alcohol.Frequency_lead2ord.rds"),
)

m7_Alcohol.Intensity_lead2 <- brm_multiple(
  bf_Alcohol.Intensity_lead2 ,
  data = listdat,
  # family = "gaussian",
  #  family = cumulative("probit"),  Chose family
  #  family = "poisson",
  family = "negbinomial",
  #  family = bernoulli(link = "cloglog"),
  seed = 1234,
  warmup = 1000,
  iter = 2000,
  chains = 4,
  init = 0,
  backend = "cmdstanr",
  set_prior('normal(0, 1)', class = 'b'),
  file = here::here("mods", "standardliving", "m7_Alcohol.Intensity_lead2.rds"),
)

m8_Bodysat_lead2_z<- brm_multiple(
  bf_Bodysat_lead2_z,
  data = listdat,
  family = "gaussian",
  #  family = cumulative("probit"),  Chose family
  #  family = "poisson",
  #  family = "negbinomial",
  #  family = bernoulli(link = "cloglog"),
  seed = 1234,
  warmup = 1000,
  iter = 2000,
  chains = 4,
  init = 0,
  backend = "cmdstanr",
  set_prior('normal(0, 1)', class = 'b'),
  file = here::here("mods", "standardliving", "m8_Bodysat_lead2_z.rds"),
)

m9_PWI_lead2_z <- brm_multiple(
  bf_PWI_lead2_z ,
  data = listdat,
  family = "gaussian",
  #  family = cumulative("probit"),  Chose family
  #  family = "poisson",
  #  family = "negbinomial",
  #  family = bernoulli(link = "cloglog"),
  seed = 1234,
  warmup = 1000,
  iter = 2000,
  chains = 4,
  init = 0,
  backend = "cmdstanr",
  set_prior('normal(0, 1)', class = 'b'),
  file = here::here("mods", "standardliving", "m9_PWI_lead2_z.rds"),
)

m10_Rumination_lead2ord <- brm_multiple(
  bf_Rumination_lead2ord,
  data = listdat,
  # family = "gaussian",
  family = cumulative("probit"),
  #  family = "poisson",
  #  family = "negbinomial",
  #  family = bernoulli(link = "cloglog"),
  seed = 1234,
  warmup = 1000,
  iter = 2000,
  chains = 4,
  init = 0,
  backend = "cmdstanr",
  set_prior('normal(0, 1)', class = 'b'),
  file = here::here("mods", "standardliving", "m10_Rumination_lead2ord.rds"),
)

m11_SexualSatisfaction_lead2_z <- brm_multiple(
  bf_SexualSatisfaction_lead2_z,
  data = listdat,
  family = "gaussian",
  #  family = cumulative("probit"),  Chose family
  #  family = "poisson",
  #  family = "negbinomial",
  #  family = bernoulli(link = "cloglog"),
  seed = 1234,
  warmup = 1000,
  iter = 2000,
  chains = 4,
  init = 0,
  backend = "cmdstanr",
  set_prior('normal(0, 1)', class = 'b'),
  file = here::here("mods", "standardliving", "m11_SexualSatisfaction_lead2_z.rds"),
)


m12_PWI_lead2_z  <- brm_multiple(
  bf_PWI_lead2_z,
  data = listdat,
  family = "gaussian",
  seed = 1234,
  warmup = 1000,
  iter = 2000,
  chains = 4,
  init = 0,
  backend = "cmdstanr",
  file = here::here("mods", "standardliving", "m12_PWI_lead2_z.rds"),
  set_prior('normal(0, 1)', class = 'b')
)

m13_EmotionRegulation1_lead2_z <- brm_multiple(
  bf_EmotionRegulation1_lead2_z,
  data = listdat,
  family = "gaussian",
  #  family = cumulative("probit"),  Chose family
  #  family = "poisson",
  #  family = "negbinomial",
  #  family = bernoulli(link = "cloglog"),
  seed = 1234,
  warmup = 1000,
  iter = 2000,
  chains = 4,
  init = 0,
  backend = "cmdstanr",
  set_prior('normal(0, 1)', class = 'b'),
  file = here::here("mods", "standardliving", "m13_EmotionRegulation1_lead2_z.rds"),
)

m14_EmotionRegulation2_lead2_z<- brm_multiple(
  bf_EmotionRegulation2_lead2_z,
  data = listdat,
  family = "gaussian",
  #  family = cumulative("probit"),  Chose family
  #  family = "poisson",
  #  family = "negbinomial",
  #  family = bernoulli(link = "cloglog"),
  seed = 1234,
  warmup = 1000,
  iter = 2000,
  chains = 4,
  init = 0,
  backend = "cmdstanr",
  set_prior('normal(0, 1)', class = 'b'),
  file = here::here("mods", "standardliving", "m14_EmotionRegulation2_lead2_z.rds"),
)


m15_EmotionRegulation3_lead2_z <- brm_multiple(
  bf_EmotionRegulation3_lead2_z,
  data = listdat,
  family = "gaussian",
  #  family = cumulative("probit"),  Chose family
  #  family = "poisson",
  #  family = "negbinomial",
  #  family = bernoulli(link = "cloglog"),
  seed = 1234,
  warmup = 1000,
  iter = 2000,
  chains = 4,
  init = 0,
  backend = "cmdstanr",
  set_prior('normal(0, 1)', class = 'b'),
  file = here::here("mods", "standardliving", "m15_EmotionRegulation3_lead2_z.rds"),
)


m16_KESSLER6sum_lead2 <- brm_multiple(
  bf_KESSLER6sum_lead2,
  data = listdat,
  #  family = "gaussian",
  #  family = cumulative("probit"),  Chose family
  #  family = "poisson",
  family = "negbinomial",
  #  family = bernoulli(link = "cloglog"),
  seed = 1234,
  warmup = 1000,
  iter = 2000,
  chains = 4,
  init = 0,
  backend = "cmdstanr",
  set_prior('normal(0, 1)', class = 'b'),
  file = here::here("mods", "standardliving", "m16_KESSLER6sum_lead2.rds"),
)


m17_LIFESAT_lead2ord <- brm_multiple(
  bf_LIFESAT_lead2ord ,
  data = listdat,
  # family = "gaussian",
  family = cumulative("probit"),
  #  family = "poisson",
  #  family = "negbinomial",
  #  family = bernoulli(link = "cloglog"),
  seed = 1234,
  warmup = 1000,
  iter = 2000,
  chains = 4,
  init = 0,
  backend = "cmdstanr",
  set_prior('normal(0, 1)', class = 'b'),
  file = here::here("mods", "standardliving", "m17_LIFESAT_lead2_z.rds"),
)


m18_POWERDEPENDENCE_lead2_z <- brm_multiple(
  bf_POWERDEPENDENCE_lead2_z,
  data = listdat,
  family = "gaussian",
  #  family = cumulative("probit"),  Chose family
  #  family = "poisson",
  #  family = "negbinomial",
  #  family = bernoulli(link = "cloglog"),
  seed = 1234,
  warmup = 1000,
  iter = 2000,
  chains = 4,
  init = 0,
  backend = "cmdstanr",
  set_prior('normal(0, 1)', class = 'b'),
  file = here::here("mods", "standardliving", "m18_POWERDEPENDENCE_lead2_z.rds"),
)


m19_PERFECTIONISM_lead2_z <- brm_multiple(
  bf_PERFECTIONISM_lead2_z,
  data = listdat,
  family = "gaussian",
  #  family = cumulative("probit"),  Chose family
  #  family = "poisson",
  #  family = "negbinomial",
  #  family = bernoulli(link = "cloglog"),
  seed = 1234,
  warmup = 1000,
  iter = 2000,
  chains = 4,
  init = 0,
  backend = "cmdstanr",
  set_prior('normal(0, 1)', class = 'b'),
  file = here::here("mods", "standardliving", "m19_PERFECTIONISM_lead2_z.rds"),
)


m20_SELF.ESTEEM_lead2_z<- brm_multiple(
  bf_SELF.ESTEEM_lead2_z ,
  data = listdat,
  family = "gaussian",
  #  family = cumulative("probit"),  Chose family
  #  family = "poisson",
  #  family = "negbinomial",
  #  family = bernoulli(link = "cloglog"),
  seed = 1234,
  warmup = 1000,
  iter = 2000,
  chains = 4,
  init = 0,
  backend = "cmdstanr",
  set_prior('normal(0, 1)', class = 'b'),
  file = here::here("mods", "standardliving", "m20_SELF.ESTEEM_lead2_z.rds"),
)


m21_Emp.WorkLifeBalance_lead2_z <- brm_multiple(
  bf_Emp.WorkLifeBalance_lead2_z ,
  data = listdat,
  family = "gaussian",
  #  family = cumulative("probit"),  Chose family
  #  family = "poisson",
  #  family = "negbinomial",
  #  family = bernoulli(link = "cloglog"),
  seed = 1234,
  warmup = 1000,
  iter = 2000,
  chains = 4,
  init = 0,
  backend = "cmdstanr",
  set_prior('normal(0, 1)', class = 'b'),
  file = here::here("mods", "standardliving", "m21_Emp.WorkLifeBalance_lead2_z.rds"),
)


m22_GRATITUDE_lead2_z <- brm_multiple(
  bf_GRATITUDE_lead2_z,
  data = listdat,
  family = "gaussian",
  #  family = cumulative("probit"),  Chose family
  #  family = "poisson",
  #  family = "negbinomial",
  #  family = bernoulli(link = "cloglog"),
  seed = 1234,
  warmup = 1000,
  iter = 2000,
  chains = 4,
  init = 0,
  backend = "cmdstanr",
  set_prior('normal(0, 1)', class = 'b'),
  file = here::here("mods", "standardliving", "m22_GRATITUDE_lead2_z.rds"),
)


m23_VENGEFUL.RUMIN_lead2ord <- brm_multiple(
  bf_VENGEFUL.RUMIN_lead2ord,
  data = listdat,
  # family = "gaussian",
  family = cumulative("probit"),
  #  family = "poisson",
  #  family = "negbinomial",
  #  family = bernoulli(link = "cloglog"),
  seed = 1234,
  warmup = 1000,
  iter = 2000,
  chains = 4,
  init = 0,
  backend = "cmdstanr",
  set_prior('normal(0, 1)', class = 'b'),
  file = here::here("mods", "standardliving", "m23_VENGEFUL.RUMIN_lead2_z.rds"),
)


m24_LIFEMEANING_lead2ord <- brm_multiple(
  bf_LIFEMEANING_lead2ord ,
  data = listdat,
  # family = "gaussian",
  family = cumulative("probit"),
  #  family = "poisson",
  #  family = "negbinomial",
  #  family = bernoulli(link = "cloglog"),
  seed = 1234,
  warmup = 1000,
  iter = 2000,
  chains = 4,
  init = 0,
  backend = "cmdstanr",
  set_prior('normal(0, 1)', class = 'b'),
  file = here::here("mods", "standardliving", "m24_LIFEMEANING_lead2ord"),
)


m25_HONESTY_HUMILITY_lead2_z <- brm_multiple(
  bf_HONESTY_HUMILITY_lead2_z ,
  data = listdat,
  family = "gaussian",
  #  family = cumulative("probit"),  Chose family
  #  family = "poisson",
  #  family = "negbinomial",
  #  family = bernoulli(link = "cloglog"),
  seed = 1234,
  warmup = 1000,
  iter = 2000,
  chains = 4,
  init = 0,
  backend = "cmdstanr",
  set_prior('normal(0, 1)', class = 'b'),
  file = here::here("mods", "standardliving", "m25_HONESTY_HUMILITY_lead2_z.rds"),
)


m26_BELONG_lead2_z <- brm_multiple(
  bf_BELONG_lead2_z ,
  data = listdat,
  family = "gaussian",
  #  family = cumulative("probit"),  Chose family
  #  family = "poisson",
  #  family = "negbinomial",
  #  family = bernoulli(link = "cloglog"),
  seed = 1234,
  warmup = 1000,
  iter = 2000,
  chains = 4,
  init = 0,
  backend = "cmdstanr",
  set_prior('normal(0, 1)', class = 'b'),
  file = here::here("mods", "standardliving", "m26_BELONG_lead2_z.rds"),
)


m27_SUPPORT_lead2_z <- brm_multiple(
  bf_SUPPORT_lead2_z,
  data = listdat,
  family = "gaussian",
  #  family = cumulative("probit"),  Chose family
  #  family = "poisson",
  #  family = "negbinomial",
  #  family = bernoulli(link = "cloglog"),
  seed = 1234,
  warmup = 1000,
  iter = 2000,
  chains = 4,
  init = 0,
  backend = "cmdstanr",
  set_prior('normal(0, 1)', class = 'b'),
  file = here::here("mods", "standardliving", "m27_SUPPORT_lead2_z.rds"),
)


m28_Volunteers_lead2 <- brm_multiple(
  bf_Volunteers_lead2,
  data = listdat,
  #family = "gaussian",
  #  family = cumulative("probit"),  Chose family
  family = "poisson",
  #  family = "negbinomial",
  #  family = bernoulli(link = "cloglog"),
  seed = 1234,
  warmup = 1000,
  iter = 2000,
  chains = 4,
  init = 0,
  backend = "cmdstanr",
  set_prior('normal(0, 1)', class = 'b'),
  file = here::here("mods", "standardliving", "m28_Volunteers_lead2.rds"),
)


m28_CharityDonate_lead2 <- brm_multiple(
  bf_CharityDonate_lead2,
  data = listdat,
  # family = "gaussian",
  #  family = cumulative("probit"),  Chose family
  #  family = "poisson",
  family = "negbinomial",
  #  family = bernoulli(link = "cloglog"),
  seed = 1234,
  warmup = 1000,
  iter = 2000,
  chains = 4,
  init = 0,
  backend = "cmdstanr",
  set_prior('normal(0, 1)', class = 'b'),
  file = here::here("mods", "standardliving", "m28_CharityDonate_lead2.rds"),
)


m29_community_lead2_z<- brm_multiple(
  bf_community_lead2_z,
  data = listdat,
  family = "gaussian",
  #  family = cumulative("probit"),  Chose family
  #  family = "poisson",
  #  family = "negbinomial",
  #  family = bernoulli(link = "cloglog"),
  seed = 1234,
  warmup = 1000,
  iter = 2000,
  chains = 4,
  init = 0,
  backend = "cmdstanr",
  set_prior('normal(0, 1)', class = 'b'),
  file = here::here("mods", "standardliving", "m29_community_lead2_z.rds"),
)


m30_NWI_lead2_z <- brm_multiple(
  bf_NWI_lead2_z,
  data = listdat,
  family = "gaussian",
  #  family = cumulative("probit"),  Chose family
  #  family = "poisson",
  #  family = "negbinomial",
  #  family = bernoulli(link = "cloglog"),
  seed = 1234,
  warmup = 1000,
  iter = 2000,
  chains = 4,
  init = 0,
  backend = "cmdstanr",
  set_prior('normal(0, 1)', class = 'b'),
  file = here::here("mods", "standardliving", "m30_NWI_lead2_z.rds"),
)


m31_ImpermeabilityGroup_z <- brm_multiple(
  bf_ImpermeabilityGroup_z,
  data = listdat,
  family = "gaussian",
  #  family = cumulative("probit"),  Chose family
  #  family = "poisson",
  #  family = "negbinomial",
  #  family = bernoulli(link = "cloglog"),
  seed = 1234,
  warmup = 1000,
  iter = 2000,
  chains = 4,
  init = 0,
  backend = "cmdstanr",
  set_prior('normal(0, 1)', class = 'b'),
  file = here::here("mods", "standardliving", "m31_ImpermeabilityGroup_z.rds"),
)


m32_PermeabilityIndividual_z <- brm_multiple(
  bf_PermeabilityIndividual_z,
  data = listdat,
  family = "gaussian",
  #  family = cumulative("probit"),  Chose family
  #  family = "poisson",
  #  family = "negbinomial",
  #  family = bernoulli(link = "cloglog"),
  seed = 1234,
  warmup = 1000,
  iter = 2000,
  chains = 4,
  init = 0,
  backend = "cmdstanr",
  set_prior('normal(0, 1)', class = 'b'),
  file = here::here("mods", "standardliving", "m32_PermeabilityIndividual_z.rds"),
)

## try GEE

library(geepack)

out3 <- with(ctrim, geeglm(
  PWI_lead2_z  ~ Standard.Living_lead1_z,
  data = cmodels,
  id = 1:nrow(cmodels),
  family = gaussian))

# same result
output <- pool(out3)
summary(output, conf.int = TRUE)
plot(output)


# brms pwi follow up ------------------------------------------------------

bf_Standard.Living_lead2_z<- bf( Standard.Living_lead2_z|weights(weights) ~ Standard.Living_lead1_z)
bf_Your.Health_lead2_z<- bf( Your.Health_lead2_z|weights(weights) ~ Standard.Living_lead1_z)
bf_Your.Future.Security_lead2_z <- bf(Your.Future.Security_lead2_z |weights(weights) ~ Standard.Living_lead1_z)
bf_Your.Personal.Relationships_lead2ord<- bf( Your.Personal.Relationships_lead2ord|weights(weights) ~ Standard.Living_lead1_z)


long2$Standard.Living_lead2_z


m33_Standard.Living_lead2_z <- brm_multiple(
  bf_Standard.Living_lead2_z,
  data = listdat2,
  family = "gaussian",
  #  family = cumulative("probit"),
  #  family = "poisson",
  #  family = "negbinomial",
  #  family = bernoulli(link = "cloglog"),
  seed = 1234,
  warmup = 1000,
  iter = 2000,
  chains = 4,
  init = 0,
  backend = "cmdstanr",
  set_prior('normal(0, 1)', class = 'b'),
  file = here::here("mods", "standardliving", "m33_Standard.Living_lead2_z.rds"),
)



m34_Your.Health_lead2_z <- brm_multiple(
  bf_Your.Health_lead2_z,
  data = listdat2,
  family = "gaussian",
  #  family = cumulative("probit"),  Chose family
  #  family = "poisson",
  #  family = "negbinomial",
  #  family = bernoulli(link = "cloglog"),
  seed = 1234,
  warmup = 1000,
  iter = 2000,
  chains = 4,
  init = 0,
  backend = "cmdstanr",
  set_prior('normal(0, 1)', class = 'b'),
  file = here::here("mods", "standardliving", "m34_Your.Health_lead2_z.rds"),
)

m35_Your.Future.Security_lead2_z <- brm_multiple(
  bf_Your.Future.Security_lead2_z,
  data = listdat2,
  family = "gaussian",
  #  family = cumulative("probit"),  Chose family
  #  family = "poisson",
  #  family = "negbinomial",
  #  family = bernoulli(link = "cloglog"),
  seed = 1234,
  warmup = 1000,
  iter = 2000,
  chains = 4,
  init = 0,
  backend = "cmdstanr",
  set_prior('normal(0, 1)', class = 'b'),
  file = here::here("mods", "standardliving", "m35_Your.Future.Security_lead2_z.rds"),
)


m36_Your.Personal.Relationships_lead2ord <- brm_multiple(
  bf_Your.Personal.Relationships_lead2ord,
  data = listdat,
  # family = "gaussian",
  family = cumulative("probit"),  Chose family
  #  family = "poisson",
  #  family = "negbinomial",
  #  family = bernoulli(link = "cloglog"),
  seed = 1234,
  warmup = 1000,
  iter = 2000,
  chains = 4,
  init = 0,
  backend = "cmdstanr",
  set_prior('normal(0, 1)', class = 'b'),
  file = here::here("mods", "standardliving", "m36_Your.Personal.Relationships_lead2ord.rds"),
)



# old IPTW ----------------------------------------------------------------

# iptw income  --------------------------------------------------------------------
#income

# Weights but now with the PWI  variables individually

library(MatchThem)
library(optmatch)
models_st <- weightthem(income_log_lead1_z ~
                          income_log_z +
                          AGREEABLENESS_z +
                          CONSCIENTIOUSNESS_z +
                          EXTRAVERSION_z  +
                          HONESTY_HUMILITY_z +
                          NEUROTICISM_z +
                          OPENNESS_z +
                          Age_z +
                          Alcohol.Frequency_z + #
                          Alcohol.Intensity_log_z + #
                          Bodysat_z +
                          Believe.God_z +
                          Believe.Spirit_z +
                          BELONG_z + #
                          CharityDonate_log_z + #
                          ChildrenNum_z +
                          Church_z +
                          community +
                          Edu_z +
                          Employed_z +
                          EmotionRegulation1_z +
                          EmotionRegulation2_z +
                          EmotionRegulation3_z +
                          Euro_z +
                          GRATITUDE_z +
                          HomeOwner_z +
                          Hours.Exercise_log_z +
                          Hours.Work_z +
                          HLTH.BMI_z  + #
                          HLTH.Fatigue_z + #
                          #  income_log_z +
                          ImpermeabilityGroup_z +
                          KESSLER6sum_z + #
                          LIFEMEANING_z + #
                          LIFESAT_z + #
                          Male_z +
                          NZdep_z +
                          NWI_z +
                          NZSEI06_z +
                          Parent_z +
                          Partner_z +
                          PERFECTIONISM_z +
                          PermeabilityIndividual_z +
                          Pol.Orient_z +
                          POWERDEPENDENCE1_z + #
                          POWERDEPENDENCE2_z + #
                          # PWI_z +
                          Relid_z +
                          Respect.Self_z + #
                          Rumination_z + #
                          SELF.CONTROL_z + #
                          SELF.ESTEEM_z + #
                          SexualSatisfaction_z +#
                          SFHEALTH_z +#
                          Smoker_z +#
                          Spiritual.Identification_z +
                          Standard.Living_z +
                          SUPPORT_z +#
                          Urban_z +
                          VENGEFUL.RUMIN_z +
                          Volunteers_z +
                          Your.Health_z +
                          Your.Future.Security_z +
                          Your.Personal.Relationships_z,
                        out2_sl,
                        approach = "within",
                        estimand = "ATE",
                        stabilize = TRUE,
                        method = "ebal")


saveh(models_st,"models_st.rds")


sum<- summary(models_st)
plot(sum)
sum
bal.tab(models_st)


ctrim_st <- trim(models_st, at = .999)
bal.tab(ctrim_st)
summary(ctrim_st)


# iptw models  income -------------------------------------------------------------
# no need to trim

out <- with(ctrim_st, glm( HLTH.BMI_lead2_z ~ income_log_lead1_z ))
out

output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)
round( EValue::evalues.OLS(0.02376129 , se = 0.009437003, sd = 1, delta = 2, true = 0), 3)
round( EValue::evalues.RR(, lo =  , hi =, true = 1), 3)

