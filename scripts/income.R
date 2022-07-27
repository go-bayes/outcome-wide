# income.R

## MICE MODEL
# library("dplyr")
# library("here")
# library("skimr")
# library("tidyr")
# library("Amelia")
# library("ggplot2")
# library("purrr")
# library("patchwork")
# library("kableExtra")
# library("parameters")
# library("mice")
# library("ggokabeito")   # color palette
# library("brms") # bayesian estimation
# library("ggpubr")
# library("cmdstanr")
# rstan_options(auto_write = TRUE) # bayesian estimation
# options(mc.cores = parallel::detectCores ()) # use all course
# theme_set(theme_pubclean()) # nice theme
# # theme_set(theme_classic())


# read data
df <- readRDS(here::here("data_raw", "df.Rds"))

# SWB.SoC01.T10	I feel a sense of community with others in my local neighbourhood.
# read files
source(here::here("scripts", "funs.R"))

# read libraries in
source(here::here("scripts", "libs.R"))
library("dplyr")
library("tidyr")


# order vars
df$GenCohort <-
  ordered(
    df$GenCohort,
    levels = c(
      "Gen_Silent: born< 1946",
      "Gen Boomers: born >= 1946 & b.< 1965",
      " GenX: born >=1961 & b.< 1981",
      "GenZ: born >= 1996 "
    )
  )


# # view
# df %>%   # not there
#   dplyr::filter(Wave == 2020) %>%
#   summarise(Respect.Self) #fully missing


# table for participant N
tabinc_df <- df %>%
  dplyr::filter((Wave == 2018  & YearMeasured  == 1) |
                  (Wave == 2019  &
                     YearMeasured  == 1) |
                  (Wave == 2020))  %>% # Eligibility criteria
  dplyr::filter(Id != 9630) %>% # problematic
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
  dplyr::filter(YearMeasured  != -1) %>% # remove people who passed away
  droplevels() %>%
  arrange(Id, Wave) %>%
  dplyr::group_by(Id) %>%   # get all people who were in that wave unless they died
  dplyr::mutate(org2018 =  ifelse(Wave == 2018 &
                                    YearMeasured == 1, 1, 0)) %>%  # creating an indicator for the first wave
  dplyr::mutate(hold18 = mean(org2018, na.rm = TRUE)) %>%  # Hack
  dplyr::filter(hold18 > 0) %>% # hack to enable repeat of baseline
  ungroup() %>%
  data.frame() %>%
  arrange(Id, Wave)

# check n # 34782
table1::table1(~ Household.INC | Wave , data = tabinc_df, overall = FALSE)
# check N of ids
length(unique(tabinc_df$Id)) # 34782


## select vars
i_df <- tabinc_df %>%
  dplyr::filter((Wave == 2018  & YearMeasured  == 1) |
                  (Wave == 2019  &
                     YearMeasured  == 1) |
                  (Wave == 2020))  %>% # Eligibility criteria
  dplyr::filter(Id != 9630) %>% # problematic
  select(
    Id,
    YearMeasured,
    Wave,
    EthCat,
    Age,
    GendAll,
    Male,
    NZSEI18,
    CONSCIENTIOUSNESS,
    OPENNESS,
    HONESTY_HUMILITY,
    EXTRAVERSION,
    NEUROTICISM,
    AGREEABLENESS,
    Edu,
    NZdep,
    Employed,
    HomeOwner,
    Pol.Orient,
    Urban,
    Household.INC,
    Parent,
    Partner,
    Relid,
    Religion.Church,
    Believe.Spirit,
    Believe.God,
    SWB.SoC01,
    EmotionRegulation1,
    EmotionRegulation2,
    EmotionRegulation3,
    Bodysat,
    VENGEFUL.RUMIN,
    retired,
    semiretired,
    # BornNZ, nor working
    KESSLER6sum,
    HLTH.Fatigue,
    Rumination,
    Smoker,
    #   ChildrenNum,
    NWI,
    BELONG,
    SUPPORT,
    CharityDonate,
    HoursCharity,
    GRATITUDE,
    # Volunteers,
    Hours.Work,
    Hours.Exercise,
    LIFEMEANING,
    LIFESAT,
    PWI,
    NWI,
    SFHEALTH,
    SELF.CONTROL,
    SFHEALTH,
    SELF.ESTEEM,
    Respect.Self,
    #  GenCohort,
    SELF.ESTEEM,
    SELF.CONTROL,
    #  Respect.Self,
    Emp.WorkLifeBalance,
    Alcohol.Frequency,
    Alcohol.Intensity,
    HLTH.BMI,
    Smoker,
    ChildrenNum,
    # GenCohort,
    # Euro,
    # partnerlost_job, rare
    #lost_job,
    #began_relationship,
    Alcohol.Intensity,
    Alcohol.Frequency,
    SexualSatisfaction,
    POWERDEPENDENCE,
    # Your.Future.Security,
    #  Your.Personal.Relationships,
    # Your.Health,
    # Standard.Living,
    #Env.SacWilling,
    #Env.SacMade,
    PERFECTIONISM ,
    PermeabilityIndividual,
    ImpermeabilityGroup
    # Emp.JobSecure,
    #  Env.ClimateChgCause,
    # Env.ClimateChgReal #,
  ) %>%
  dplyr::rename(community = SWB.SoC01) %>%
  dplyr::mutate(Edu = as.numeric(Edu)) %>%
  group_by(Id) %>%
  dplyr::mutate(org2019 =  ifelse(Wave == 2019 &
                                    YearMeasured == 1, 1, 0)) %>%  # creating an indicator for the first wave
  dplyr::mutate(hold19 = mean(org2019, na.rm = TRUE)) %>%  # Hack0
  dplyr::filter(hold19 > 0) %>% # hack to enable repeat of baseline in 201
  dplyr::mutate(org2018 =  ifelse(Wave == 2018 &
                                    YearMeasured == 1, 1, 0)) %>%  # creating an indicator for the first wave
  dplyr::mutate(hold18 = mean(org2018, na.rm = TRUE)) %>%  # Hack
  dplyr::filter(hold18 > 0) %>% # hack to enable repeat of baseline
  # dplyr::filter(YearMeasured != -1) %>% # remove people who passed away
  ungroup() %>%
  dplyr::filter(YearMeasured  != -1) %>%
  droplevels() %>%
  arrange(Id, Wave) %>%
  dplyr::group_by(Id) %>%   # get all people who were in that wave unless they died
  dplyr::mutate(org2018 =  ifelse(Wave == 2018 &
                                    YearMeasured == 1, 1, 0)) %>%  # creating an indicator for the first wave
  dplyr::mutate(hold18 = mean(org2018, na.rm = TRUE)) %>%  # Hack
  dplyr::filter(hold18 > 0) %>% # hack to enable repeat of baseline
  ungroup() %>%
  dplyr::mutate(across(!c(Id, EthCat, Wave), ~ as.numeric(.x))) %>% # make factors numeric for easy of processing
  arrange(Id, Wave) %>%
  dplyr::mutate(
    Edu = as.numeric(Edu),
    Volunteers = if_else(HoursCharity == 1, 1, 0),
    Depressed = (as.numeric(
      cut(
        KESSLER6sum,
        breaks = c(-Inf, 13, Inf),
        labels = c("0", "1"),
        right = FALSE
      )
    ) - 1),
    EthCat = factor(EthCat, labels = c("Euro", "Maori", "Pacific", "Asian")),
    Euro = as.numeric(if_else(EthCat == "Euro", 1, 0)),
    Male = ifelse(GendAll == 1, 1, 0),
    Church = ifelse(Religion.Church > 8, 8, Religion.Church),
    income_log = log(Household.INC + 1),
  ) %>%
  arrange(Id, Wave)  %>%
  dplyr::mutate(income_log_lead1 = lead(income_log, n = 1)) %>%
  dplyr::mutate(retired_lead1 = lead(retired, n = 1)) %>%
  dplyr::mutate(semiretired_lead1 = lead(semiretired, n = 1)) %>%
  #dplyr::mutate(Church_lead1 = lead(Church, n = 1)) %>%
  # inc_prop = (income_log / (income_log_lead1) - 1),
  dplyr::mutate(across(
    c(
      income_log,
      Depressed,
      KESSLER6sum,
      HLTH.Fatigue,
      Rumination,
      community,
      SFHEALTH,
      LIFEMEANING,
      LIFESAT,
      PWI,
      #  not_euro,
      SELF.ESTEEM,
      SELF.CONTROL,
      Respect.Self,
      Alcohol.Frequency,
      Hours.Exercise,
      HLTH.BMI,
      Smoker,
      ChildrenNum,
      NWI,
      BELONG,
      SUPPORT,
      Volunteers,
      GRATITUDE,
      SexualSatisfaction,
      POWERDEPENDENCE,
      #Env.SacWilling,
      #Env.SacMade,
      #  Env.ClimateChgCause,
      #  Env.ClimateChgReal,
      CharityDonate,
      Alcohol.Intensity,
      PERFECTIONISM,
      Bodysat,
      VENGEFUL.RUMIN,
      community,
      HONESTY_HUMILITY,
      EmotionRegulation1,
      EmotionRegulation2,
      EmotionRegulation3,
      Emp.WorkLifeBalance
    ),
    ~ lead(.x, n = 2),
    .names = "{col}_lead2"
  )) %>% # make leads
  dplyr::filter(Wave == 2018) %>%
 dplyr::filter(retired != 1) %>%
 dplyr::filter(retired_lead1 != 1) %>%  #needed for the intervention
  dplyr::filter(semiretired != 1) %>%
  dplyr::filter(semiretired_lead1 != 1) %>%  #needed for the intervention
  #  dplyr::filter(!is.na(Church)) %>%
  #  dplyr::filter(!is.na(Church_lead1)) %>%  #needed for the intervention
  dplyr::select(
    -c(
      Religion.Church,
      EthCat,
      HoursCharity,
      Respect.Self_lead2,
      Household.INC,
      org2018,
      #  not_euro,
      #  not_euro_lead2,
      hold18,
      Euro,
      Emp.WorkLifeBalance,
      YearMeasured,
      org2019,
      hold19,
      retired,
      retired_lead1,
      semiretired,
      semiretired_lead1
    )
  ) %>%
  #  dplyr::mutate(across(!c(Id,Wave), ~ scale(.x)))%>%  # standarise vars for easy computing
  arrange(Id, Wave) %>%
  data.frame() %>%
  mutate(across(where(is.double), as.numeric)) %>%
  arrange(Id)

table1::table1( ~ income_log + income_log_lead1, data = i_df)


length(unique(i_df$Id)) # 31280

# inspect data
skim(i_df)

i_df %>%
  group_by(Wave) %>%
  summarise(across(Id, n_distinct))


# glimse
i_df%>%
  summarise(across(c(PWI_lead2, LIFESAT_lead2, LIFEMEANING_lead2), ~mean(.x, na.rm=TRUE), ~sd(.x, na.rm=TRUE), n_distinct()))

#check
skimr::skim(i_df)

# save function
saveh(i_df, "i_df")

# read if needed
i_df<- readh("i_df")



i_df %>%
  summarise(across(c(income_log, income_log_lead2),
                   list(mean = mean, range=range, sd = sd), na.rm = TRUE, .names = "{col}_{fn}"))


# GFS coding
# Church
#[Never, a few times a year, a few times a month, weekly, more than once per week] [BMMRS 34]

# Religious TXTs
#[Never, occasionally, daily, more than daily] [BMMRS 15, modified]

#  Charity
#  In the past month, have you volunteered your time to an organization?2


table1::table1(
  ~ income_log +
    income_log_lead1 +
    Depressed +
    KESSLER6sum +
    HLTH.Fatigue +
    Rumination +
    SFHEALTH  +
    LIFEMEANING +
    LIFESAT +
    PWI +
    SELF.ESTEEM +
    SELF.CONTROL +
    Respect.Self +
    #Emp.WorkLifeBalance +
    Alcohol.Frequency +
    Alcohol.Intensity+
    Hours.Exercise +
    HLTH.BMI +
    Smoker +
    NWI +
    ChildrenNum +
    BELONG +
    SUPPORT +
    CharityDonate+
    Volunteers +
    SexualSatisfaction+
    POWERDEPENDENCE +
  PERFECTIONISM +
  Bodysat +
  VENGEFUL.RUMIN +
  community +
  HONESTY_HUMILITY +
  EmotionRegulation1 +
  EmotionRegulation2 +
  EmotionRegulation3,# +
 # Emp.WorkLifeBalance,
    # +
   # Your.Future.Security +
   # Your.Personal.Relationships +
   # Your.Health +
   # Standard.Living,
  #Env.SacWilling,
  #Env.SacMade,
  data = i_df
)


# missingness in 2020  1/3 of the data are missing

# I do not have enough power or control over important parts of my life.

skimr::skim(i_df)

# mice model  -------------------------------------------------------------
library(mice)
inc_mice <- i_df %>%
  dplyr::select(-c( Wave, Id,))
# Visualise missing
library(naniar)
naniar::gg_miss_var(inc_mice)

vis_miss(inc_mice,
         warn_large_data = FALSE)

# any colinear vars?
mice:::find.collinear(inc_mice)

#for_mice$inc_prop <-
#  for_mice$income_log / (for_mice$income_log_lead1 - 1)


ini <- mice(inc_mice, m = 1, maxit = 0)
ini
meth <- ini$meth
#meth
#meth["inc_prop"] <- "~ I(income_log/(income_log_lead1 - 1))"
pred <- ini$pred
pred

# impute
out_idf <- mice::mice(inc_mice,
                  meth = meth,
                  pred = pred,
                  seed = 0,
                  m = 10)

# save
saveh(out_idf, "out_idf")

# read
out_idf <- readh("out_idf")


outlist2 <- row.names(out_idf)[out_idf$outflux < 0.5]
length(outlist2)

head(out_idf$loggedEvents, 10)

# plots  (takes time)
#dev.off()
#stripplot(out)
#densityplot(out) # too much time
#plot(out)



# data warangling
long <- mice::complete(out_idf, "long", inc = TRUE)
# inspect data -- what do we care about?  Note that the moderate distress category doesn't look useful
skimr::skim(long)

#long$LIFESAT_lead1_z <- with(long, scale(LIFESAT_lead1))


# create variables in z score
long2 <- long %>%
  dplyr::mutate(across(where(is.numeric), ~ scale(.x), .names = "{col}_z"))%>%
  select(-c(.imp_z, .id_z))# Respect for Self is fully missing

# review
skim(long2)

# get colnames
colnames(long2) # get names

#long$attrition <- with(long, ndf$attrition_lead2)
# neect to get into shape
long3 <- long2 %>% mutate_if(is.matrix, as.vector)
out2 <- mice::as.mids(long3)


saveh(out2, "out2")
out2 <- readh("out2")


# urban vs rural
#xyplot(out2, attrition ~ LIFESAT_lead1_z,pch=18,cex=1)

# Try full model
#devtools::install_github('IQSS/Zelig')

# k6 ----------------------------------------------------------------------

test1 <-
  with(out2, glm(
    as.numeric(KESSLER6sum_lead2_z) ~
      income_log_lead1_z +
      income_log_z +
      GRATITUDE_z +
      income_log_z +
      Church_z +
      income_log_z +  #inc_prop
      AGREEABLENESS_z +
      CONSCIENTIOUSNESS_z +
      OPENNESS_z +
      HONESTY_HUMILITY_z +
      EXTRAVERSION_z +
      NEUROTICISM_z +
      Age_z +
      began_relationship_z +
      Believe.God_z +
      Believe.Spirit_z +
      CharityDonate_log_z +
      ChildrenNum_z +
      Church_z +
      Edu_z +
      Exercise_log_z +
      GendAll_z +
      HomeOwner_z +
      HoursCharity_z +
      Hours.Work_z +
      lost_job_z +
      not_euro_z +
      NZdep_z +
      Parent_z +
      Partner_z +
      # partnerlost_job_z +
      Pol.Orient_z +
      Religious_z +
      Smoker_z +
      SUPPORT_z +
      Volunteers_z +
      Urban_z +
      Alcohol.Frequency_z +
      Alcohol.Intensity_log_z +
      BELONG_z +
      ChildrenNum_z +
      # Depressed +
      KESSLER6sum_z +
      HLTH.BMI_z +
      HLTH.Fatigue_z +
      Rumination_z +
      LIFEMEANING_z +
      SFHEALTH_z +
      LIFESAT_z +
      NWI_z +
      PWI_z +
      Respect.Self_z +
      Rumination_z +
      SELF.CONTROL_z +
      SELF.ESTEEM_z +
      SUPPORT_z
  ))
tp <- parameters::model_parameters(test1)
plot(tp)


parameters::model_parameters(tp) %>% print_html()


# Exercise = cut(
#   Hours.Exercise,
#   breaks = c(-Inf,0,2.5,7,Inf),
#   labels = c("Never","0-upto-2.5hrs","2.5-upto-7hrs",">7hrs"),
#   right = FALSE),


skim(adf)

# check consistent IDs, and drop thise who were deceased

i_df %>%
  group_by(Wave) %>%
  summarise(across(Id, n_distinct))
# inspect data focus on distributions

df %>%
  filter(Wave == 2020) %>%
  summarise(Respect.Self) #fully missing

# check leads in random sub-sample of vars

df%>%
  dplyr::filter(Id != 9630) %>%  # Strong measurement error, reports 1 dollar income (yet owns house)
  dplyr::filter(
    (Wave == 2018  & YearMeasured  == 1 & Employed == 1) |
      (Wave == 2019  &
         YearMeasured  == 1 & Employed == 1) |  (Wave == 2020 &   YearMeasured  != -1)
  ) %>%
  droplevels() %>%
  dplyr::group_by(Id) %>%
  dplyr::mutate(org2019 =  ifelse(Wave == 2019 & YearMeasured == 1, 1, 0)) %>%  # creating an indicator for the first wave
  dplyr::mutate(hold19 = mean(org2019, na.rm = TRUE)) %>%  # Hack0
  dplyr::filter(hold19 > 0) %>% # hack to enable repeat of baseline in 201
  dplyr::mutate(org2018 =  ifelse(Wave == 2018 &
                                    YearMeasured == 1, 1, 0)) %>%  # creating an indicator for the first wave
  dplyr::mutate(hold18 = mean(org2018, na.rm = TRUE)) %>%  # Hack
  dplyr::filter(hold18 > 0) %>% # hack to enable repeat of baseline
  ungroup() %>%
  filter(Wave == 2020) %>%
  summarise(across(c(PWI, LIFESAT, LIFEMEANING), ~mean(.x, na.rm=TRUE), ~sd(.x, na.rm=TRUE)))

# compare, looks good
adf%>%
  summarise(across(c(PWI_lead2, LIFESAT_lead2, LIFEMEANING_lead2), ~mean(.x, na.rm=TRUE), ~sd(.x, na.rm=TRUE), n_distinct()))


# cases
length(unique(adf$Id))


#check
skimr::skim(adf)

saveRDS(adf, here::here("mods", "adf"))
adf <- readRDS(here::here("mods", "adf"))


# check
# dev.off()
# hist(adf$inc_prop)
# abline(v=0, col = "blue", lwd=3)
# abline(v = mean(adf$inc_prop, na.rm=TRUE), col = "red", lwd=3) # prop loss is strange


adf %>%
  summarise(across(c(income_log, income_log_lead2),
                   list(mean = mean, range=range, sd = sd), na.rm = TRUE, .names = "{col}_{fn}"))

adf

# GFS coding
# Church
#[Never, a few times a year, a few times a month, weekly, more than once per week] [BMMRS 34]

# Religious TXTs
#[Never, occasionally, daily, more than daily] [BMMRS 15, modified]

#  Charity
#  In the past month, have you volunteered your time to an organization?2


# mice model  -------------------------------------------------------------
library(mice)
for_mice <- adf %>%
  dplyr::select(-c( Wave, Id))

library(naniar)
naniar::gg_miss_var(for_mice)
# mice:::find.collinear(df)
head(for_mice)

#for_mice$inc_prop <-
#  for_mice$income_log / (for_mice$income_log_lead1 - 1)
ini <- mice(for_mice, m = 1, maxit = 0)
ini
# meth <- ini$meth
# meth
# #meth["inc_prop"] <- "~ I(income_log/(income_log_lead1 - 1))"
# pred <- ini$pred


# impute
out <- mice::mice(for_mice,
                  #meth = "rfcont",
                  #pred = pred,
                  seed = 0, m = 10)

saveRDS(out, here::here("mods", "out"))

out <- readRDS(here::here("mods", "out"))
str(out)
# plots
dev.off()
#
#stripplot(out)
#densityplot(out) # too much time
#plot(out)
#stripplot(out)

# data warangling
long <- mice::complete(out, "long", inc = TRUE)

table1::table1( ~ income_log_lead1 , data = long)
table1::table1( ~ income_log_lead1 , data = adf)

# inspect data -- what do we care about?  Note that the moderate distress category doesn't look useful
skimr::skim(long)

#long$LIFESAT_lead1_z <- with(long, scale(LIFESAT_lead1))


# create variables in z score
library(dplyr)
long2 <- long %>%
  dplyr::mutate(across(where(is.numeric), ~ scale(.x), .names = "{col}_z"))%>%
  select(-c(.imp_z, .id_z))# Respect for Self is fully missing

# review
skimr::skim(long2)

# get colnames
colnames(long2) # get names

#long$attrition <- with(long, ndf$attrition_lead2)
# neect to get into shape
long3 <- long2 %>% mutate_if(is.matrix, as.vector)
out2 <- mice::as.mids(long3)


saveRDS(out2, here::here("mods", "out2"))

out2 <- readRDS(here::here("mods", "out2"))



dch <- ipw::ipwpoint(
  exposure = ch_s,
  family = "gaussian",
  numerator = ~ 1,
  denominator = ~ age_s + edu_s + male_2 + ses_s + country,
  trunc = 0.01,
  data = as.data.frame(d4)
)

d1p <- d4 %>%
  mutate(ipwCH = dch$weights.trunc)









# k6 ----------------------------------------------------------------------

# Church
test1 <-
  with(out2, glm(
    as.numeric(KESSLER6sum_lead2_z) ~
      #income_log_lead1_z +
      Church_lead1_z +
      Church_z +
      income_log_z +  #inc_prop
      AGREEABLENESS_z +
      CONSCIENTIOUSNESS_z +
      OPENNESS_z +
      HONESTY_HUMILITY_z +
      EXTRAVERSION_z +
      NEUROTICISM_z +
      Age_z +
      began_relationship_z +
      Believe.God_z +
      Believe.Spirit_z +
      CharityDonate_log_z +
      ChildrenNum_z +
      Church_z +
      Edu_z +
      Exercise_log_z +
      GendAll_z +
      HomeOwner_z +
      HoursCharity_z +
      Hours.Work_z +
      lost_job_z +
      not_euro_z +
      NZdep_z +
      Parent_z +
      Partner_z +
      # partnerlost_job_z +
      Pol.Orient_z +
      Religious_z +
      Smoker_z +
      SUPPORT_z +
      Volunteers_z +
      Urban_z +
      Alcohol.Frequency_z +
      Alcohol.Intensity_log_z +
      BELONG_z +
      ChildrenNum_z +
      # Depressed +
      KESSLER6sum_z +
      HLTH.BMI_z +
      HLTH.Fatigue_z +
      Rumination_z +
      LIFEMEANING_z +
      SFHEALTH_z +
      LIFESAT_z +
      NWI_z +
      PWI_z +
      Respect.Self_z +
      Rumination_z +
      SELF.CONTROL_z +
      SELF.ESTEEM_z +
      SUPPORT_z
  ))
tp <- parameters::model_parameters(test1)
plot(tp)

long2$Church

# set priors
long2$KESSLER6sum_lead2_z

test2 <-
  with(out2, glm(
    as.numeric(KESSLER6sum_lead2_z) ~ income_log_lead1_z + income_log_z +  #inc_prop
      AGREEABLENESS_z +
      CONSCIENTIOUSNESS_z +
      OPENNESS_z +
      HONESTY_HUMILITY_z +
      EXTRAVERSION_z +
      NEUROTICISM_z +
      Age_z +
      began_relationship_z +
      Believe.God_z +
      Believe.Spirit_z +
      CharityDonate_log_z +
      ChildrenNum_z +
      Church +
      Edu_z +
      Exercise_log_z +
      GendAll_z +
      HomeOwner_z +
      HoursCharity_z +
      Hours.Work_z +
      lost_job_z +
      not_euro_z +
      NZdep_z +
      Parent_z +
      Partner_z +
      # partnerlost_job_z +
      Pol.Orient_z +
      Religious_z +
      Smoker_z +
      SUPPORT_z +
      Volunteers_z +
      Urban_z +
      Alcohol.Frequency_z +
      Alcohol.Intensity_log_z +
      BELONG_z +
      ChildrenNum_z +
      # Depressed +
      KESSLER6sum_z +
      HLTH.BMI_z +
      HLTH.Fatigue_z +
      Rumination_z +
      LIFEMEANING_z +
      SFHEALTH_z +
      LIFESAT_z +
      NWI_z +
      PWI_z +
      Respect.Self_z +
      Rumination_z +
      SELF.CONTROL_z +
      SELF.ESTEEM_z
  ))
tp2 <- parameters::model_parameters(test2)
plot(tp2)




m1 <- brms::brm_multiple(
  Depressed  ~
    income_lead1_logz +
    income_base_logz  +
    # k6_bz +
    partner18_bz +
    age_z +
    EthnicCats_b +
    male_2z +
    ubran_bz +
    nzdep_bz +
    parent_bz +
    pol_bz +
    church_bz_logc +
    nr_bz +
    ex_bz +
    h_bz +
    op_bz +
    csbz +
    agr_bz +
    ChildrenNum_bz +
    PWI_bz +
    began_relationship_bz +
    partnerlost_job_bz +
    lost_job_bz +
    HomeOwner_bz +
    Hours.Work_logbz
) #+
# LIFEMEANING_bz +
# LIFESAT_bz +
# SELF.CONTROL_bz +
# SELF.ESTEEM_bz +
# Respect.Self_bz +
# Rumination_bz +
# SFHEALTH_bz +
# BELONG_bz +
# SUPPORT_bz)

exp(+(2 * 0.0848113873))  # 16% loss of risk by 2 SD deviation change
exp(-(1 * 0.20))  # 16% loss of risk by 2 SD deviation change

summary(pool(m1))
exp(1.05)

m1p <-
  parameters::model_parameters(m1,  ci_method = "wald")
m1p
# do not interpret any variable except the exposure
plot(m1p, show_labels = TRUE)

# latex table
m1p [, c(1:5)] %>%
  # print_md()%>%
  kbl("latex", booktabs = TRUE, digits = 2)



### CODE FOR MAKING GRAPH
# Brms

m1_bayes <- brms::brm_multiple(
  k6_lead2 ~  income_lead1_logc +
    income_base_logc +
    k6_bz +
    partner18_bz +
    age_z +
    EthnicCats_b +
    male_2z +
    ubran_bz +
    nzdep_bz +
    parent_bz +
    pol_bz +
    church_bz_logc +
    nr_bz +
    ex_bz +
    h_bz +
    op_bz +
    csbz +  # note change in name
    agr_bz +
    ChildrenNum_bz +
    PWI_bz +
    began_relationship_bz +
    partnerlost_job_bz +
    lost_job_bz +
    HomeOwner_bz +
    Hours.Work_logbz,
  family = "poisson",
  data = out2,
  seed = 1234,
  warmup = 1000,
  iter = 2000,
  chains = 4,
  backend = "cmdstanr",
  file = here::here("mods", "m1_bayes")  #save model
)

# check posteriors
pp_check(m1_bayes)
#
# # standardised
m1_bayez <- brms::brm_multiple(
  k6_lead2 ~  income_lead1_logc + #inc_prop * # income_lead_log1c + #income_lead_log1c
    income_base_logc +   # baseline incoem
    k6_bz +
    partner18_bz +
    age_z +
    EthnicCats_b +
    male_2z +
    ubran_bz +
    nzdep_bz +
    parent_bz +
    pol_bz +
    church_bz_logc +
    nr_bz +
    ex_bz +
    h_bz +
    op_bz +
    csbz +  # note change in name
    agr_bz +
    ChildrenNum_bz +
    PWI_bz +
    began_relationship_bz +
    partnerlost_job_bz +
    lost_job_bz +
    HomeOwner_bz +
    Hours.Work_logbz,
  family = "negbinomial",
  data = out2,
  seed = 1234,
  warmup = 1000,
  iter = 2000,
  chains = 4,
  backend = "cmdstanr",
  file = here::here("mods", "m1_bayez")  #save model
)
pp_check(m1_bayes)

# much better!
pp_check(m1_bayez)


#table
lazerhawk::brms_SummaryTable(m1_bayez, panderize =
                               F)

# another table
m1_bayes_table <-
  parameters::model_parameters(m1_bayez)
m1_bayes_table
plot(m1_bayes_table)

# graph
plot_smooth_m1 <-
  plot(
    conditional_effects(
      m1_bayez,
      "income_lead1_logc",
      ndraws = 200,
      spaghetti = T
    ),
    points = T,
    alpha = .01,
    point_args = list(alpha = .005, width = .1)
  )

# tidy graph
m1_plot <-
  plot_smooth_m1[[1]] + # scale_x_continuous(limits = c(-5,5)) +
  # scale_y_continuous(limits = c(0,24)) +
  # geom_hline(
  #   yintercept = 5,
  #   linetype = "dashed",
  #   color = "red",
  #   size = .5
  # ) +
  labs(title = "Kessler 6 Distress (negative binomial regression)",
       # subtitle = "Loss shows greater distress",
       y = "Kessler6 distress (0-24)",
       x = "Log income (centered)") #+ scale_colour_okabe_ito(alpha=.5)

m1_plot

#scale_color_viridis_d(option = "cividis")

# ggsave(
#   bayes_9,
#   path = here::here(here::here("figs")),
#   width = 12,
#   height =9,
#   units = "in",
#   filename = "m1_plot.jpg",
#   device = 'jpeg',
#   limitsize = FALSE,
#   dpi = 1000
# )


library(kableExtra)
# table

# pool parameters using rubins rule
# tab <-
#   parameters::pool_parameters(model_amy$model, ci_method = "wald")
# tab
# tab <- tab %>%
#   slice(1:3)
# tab
#
# tab [, c(1:5)] %>%
#   # print_md()%>%
#   kbl("latex", booktabs = TRUE, digits = 2)
#
# # do not interpret any variable except the exposure
# plot(tab, show_labels = TRUE, ci_method = "wald")




# lifemeaning -------------------------------------------------------------


m2 <-
  with(
    out2,
    glm(
      LIFEMEANING_lead2 ~  income_lead1_logc +
        income_base_logc  +
        LIFEMEANING_bz +
        partner18_bz +
        age_z +
        EthnicCats_b +
        male_2z +
        ubran_bz +
        nzdep_bz +
        parent_bz +
        pol_bz +
        church_bz_logc +
        nr_bz +
        ex_bz +
        h_bz +
        op_bz +
        cs_bz +
        agr_bz +
        ChildrenNum_bz +
        PWI_bz +
        began_relationship_bz +
        partnerlost_job_bz +
        lost_job_bz +
        HomeOwner_bz +
        Hours.Work_logbz
    )
  )

summary(pool(m2))


summary(pool(m2))
m2 <-
  parameters::model_parameters(m2,  ci_method = "wald")
m2


# do not interpret any variable except the exposure
plot(m2, show_labels = TRUE)

# latex table
m2 [, c(1:5)] %>%
  # print_md()%>%
  kbl("latex", booktabs = TRUE, digits = 2)


# bayesian
m2_bayes <- brms::brm_multiple(
  LIFEMEANING_lead2 ~  income_lead1_logc +
    income_base_logc  +
    LIFEMEANING_bz +
    partner18_bz +
    age_z +
    EthnicCats_b +
    male_2z +
    ubran_bz +
    nzdep_bz +
    parent_bz +
    pol_bz +
    church_bz_logc +
    nr_bz +
    ex_bz +
    h_bz +
    op_bz +
    csbz + # note
    agr_bz +
    ChildrenNum_bz +
    PWI_bz +
    began_relationship_bz +
    partnerlost_job_bz +
    lost_job_bz +
    HomeOwner_bz +
    Hours.Work_logbz,
  data = out2,
  seed = 1234,
  warmup = 1000,
  iter = 2000,
  chains = 4,
  backend = "cmdstanr",
  file = here::here("mods", "m2_bayes")  #save model
)


# pp_check(m1_bayez)
pp_check(m2_bayes)   # not good

LIFEMEANING_lead2ord


m2_bayes_ord <- brms::brm_multiple(
  LIFEMEANING_lead2ord ~  income_lead1_logc +
    income_base_logc  +
    LIFEMEANING_bz +
    partner18_bz +
    age_z +
    EthnicCats_b +
    male_2z +
    ubran_bz +
    nzdep_bz +
    parent_bz +
    pol_bz +
    church_bz_logc +
    nr_bz +
    ex_bz +
    h_bz +
    op_bz +
    csbz + # note
    agr_bz +
    ChildrenNum_bz +
    PWI_bz +
    began_relationship_bz +
    partnerlost_job_bz +
    lost_job_bz +
    HomeOwner_bz +
    Hours.Work_logbz,
  family = cumulative("probit"),
  data = out2,
  init = 0,
  seed = 1234,
  warmup = 1000,
  iter = 2000,
  chains = 4,
  backend = "cmdstanr",
  file = here::here("mods", "m2_bayes_ord")  #save model
)


pp_check(m2_bayes_ord) # perfect

#table
lazerhawk::brms_SummaryTable(m2_bayes_ord, panderize =
                               F)

# another table
m2_bayes_table <-
  parameters::model_parameters(m2_bayes_ord)
m2_bayes_table
plot(m2_bayes_table)

# graph
plot_smooth_m2 <-
  plot(
    conditional_effects(
      m2_bayes_ord,
      "income_lead1_logc",
      ndraws = 200,
      spaghetti = T,
      categorical = F
    ),
    points = T,
    alpha = .01,
    point_args = list(alpha = .005, width = .1)
  )

# tidy graph
m2_plot <-
  plot_smooth_m2[[1]] + # scale_x_continuous(limits = c(-5,5)) +
  # scale_y_continuous(limits = c(1,7)) +
  labs(
    title = "Meaning of Life (ordinal regression)",
    subtitle = "",
    y = "Meaning of Life (1-7)",
    x = "Log income (centered)"
  ) + scale_colour_okabe_ito(alpha = .5)

m2_plot


# LIFESAT  ----------------------------------------------------------------------


m3 <-
  with(
    out2,
    glm(
      LIFESAT_lead2 ~  income_lead1_logc + #inc_prop * # income_lead_log1c + #income_lead_log1c
        income_base_logc +   # baseline incoem
        LIFESAT_bz +
        partner18_bz +
        age_z +
        EthnicCats_b +
        male_2z +
        ubran_bz +
        nzdep_bz +
        parent_bz +
        pol_bz +
        church_bz_logc +
        nr_bz +
        ex_bz +
        h_bz +
        op_bz +
        cs_bz +
        agr_bz +
        ChildrenNum_bz +
        PWI_bz +
        began_relationship_bz +
        partnerlost_job_bz +
        lost_job_bz +
        HomeOwner_bz +
        Hours.Work_logbz
    )
  )


summary(pool(m3))

m3 <-
  parameters::model_parameters(m3,  ci_method = "wald")
m3
# do not interpret any variable except the exposure
plot(m3, show_labels = TRUE)

# latex table
m3 [, c(1:5)] %>%
  # print_md()%>%
  kbl("latex", booktabs = TRUE, digits = 2)



m3_bayes <- brms::brm_multiple(
  LIFESAT_lead2 ~  income_lead1_logc +
    income_base_logc +
    LIFESAT_bz +
    partner18_bz +
    age_z +
    EthnicCats_b +
    male_2z +
    ubran_bz +
    nzdep_bz +
    parent_bz +
    pol_bz +
    church_bz_logc +
    nr_bz +
    ex_bz +
    h_bz +
    op_bz +
    csbz +
    agr_bz +
    ChildrenNum_bz +
    PWI_bz +
    began_relationship_bz +
    partnerlost_job_bz +
    lost_job_bz +
    HomeOwner_bz +
    Hours.Work_logbz,
  data = out2,
  seed = 1234,
  warmup = 1000,
  iter = 2000,
  chains = 4,
  backend = "cmdstanr",
  file = here::here("mods", "m3_bayes")  #save model
)

summary(m3_bayes)


# pp_check(m1_bayez)

pp_check(m3_bayes)

#table
lazerhawk::brms_SummaryTable(m3_bayes, panderize =
                               F)

# another table
m3_bayes_table <-
  parameters::model_parameters(m3_bayes)
m3_bayes_table
plot(m3_bayes_table)

# graph
plot_smooth_m3 <-
  plot(
    conditional_effects(
      m3_bayes,
      "income_lead1_logc",
      ndraws = 200,
      spaghetti = T
    ),
    points = T,
    alpha = .01,
    point_args = list(alpha = .005, width = .1)
  )

# tidy graph
m3_plot <-
  plot_smooth_m3[[1]] + # scale_x_continuous(limits = c(-5,5)) +
  # scale_y_continuous(limits = c(1,7)) +
  labs(
    title = "Life Satisifaction",
    subtitle = "",
    y = "Life Satisfaction (1-7)",
    x = "Log income (centered)"
  ) #+ scale_colour_okabe_ito(alpha=.5)

m3_plot



# SELF.CONTROL_bz  ----------------------------------------------------------------------


m4 <-
  with(
    out2,
    glm(
      SELF.CONTROL_lead2 ~  income_lead1_logc + #inc_prop * # income_lead_log1c + #income_lead_log1c
        income_base_logc +
        SELF.CONTROL_bz +
        partner18_bz +
        age_z +
        EthnicCats_b +
        male_2z +
        ubran_bz +
        nzdep_bz +
        parent_bz +
        pol_bz +
        church_bz_logc +
        nr_bz +
        ex_bz +
        h_bz +
        op_bz +
        cs_bz +
        agr_bz +
        ChildrenNum_bz +
        PWI_bz +
        began_relationship_bz +
        partnerlost_job_bz +
        lost_job_bz +
        HomeOwner_bz +
        Hours.Work_logbz
    )
  )


summary(pool(m4))

m4 <-
  parameters::model_parameters(m4,  ci_method = "wald")
m4
# do not interpret any variable except the exposure
plot(m4, show_labels = TRUE)

# latex table
m4 [, c(1:5)] %>%
  # print_md()%>%
  kbl("latex", booktabs = TRUE, digits = 2)


m4_bayes <- brms::brm_multiple(
  SELF.CONTROL_lead2 ~  income_lead1_logc + #inc_prop * # income_lead_log1c + #income_lead_log1c
    income_base_logc +
    SELF.CONTROL_bz +
    partner18_bz +
    age_z +
    EthnicCats_b +
    male_2z +
    ubran_bz +
    nzdep_bz +
    parent_bz +
    pol_bz +
    church_bz_logc +
    nr_bz +
    ex_bz +
    h_bz +
    op_bz +
    csbz +
    agr_bz +
    ChildrenNum_bz +
    PWI_bz +
    began_relationship_bz +
    partnerlost_job_bz +
    lost_job_bz +
    HomeOwner_bz +
    Hours.Work_logbz,
  data = out2,
  seed = 1234,
  warmup = 1000,
  iter = 2000,
  chains = 4,
  backend = "cmdstanr",
  file = here::here("mods", "m4_bayes")  #save model
)

summary(m4_bayes)

pp_check(m4_bayes)
#table
lazerhawk::brms_SummaryTable(m4_bayes, panderize =
                               F)
# another table
m4_bayes_table <-
  parameters::model_parameters(m4_bayes)
m4_bayes_table
plot(m4_bayes_table)

# graph
plot_smooth_m4 <-
  plot(
    conditional_effects(
      m4_bayes,
      "income_lead1_logc",
      ndraws = 200,
      spaghetti = T
    ),
    points = T,
    alpha = .01,
    point_args = list(alpha = .005, width = .1)
  )

# tidy graph
m4_plot <-
  plot_smooth_m4[[1]] + # scale_x_continuous(limits = c(-5,5)) +
  # scale_y_continuous(limits = c(1,7)) +
  labs(
    title = "Self Control",
    subtitle = "",
    y = "Self Control (1-7)",
    x = "Log income (centered)"
  ) #+ scale_colour_okabe_ito(alpha=.5)

m4_plot


# Respect self ------------------------------------------------------------


# NOT MEASURED IN 2020
#
# m5 <- with(out2, glm(Respect.Self_lead2 ~  income_lead1_logc + #inc_prop * # income_lead_log1c + #income_lead_log1c
#                        income_base_logc +   # baseline incoem
#                        Respect.Self_bz +
#                        partner18_bz +
#                        age_z +
#                        EthnicCats_b +
#                        male_2z +
#                        ubran_bz +
#                        nzdep_bz +
#                        parent_bz +
#                        pol_bz +
#                        church_bz_logc +
#                        nr_bz +
#                        ex_bz +
#                        h_bz +
#                        op_bz +
#                        csbz +
#                        agr_bz +
#                        ChildrenNum_bz +
#                        PWI_bz +
#                        began_relationship_bz +
#                        partnerlost_job_bz +
#                        lost_job_bz +
#                        HomeOwner_bz +
#                        Hours.Work_logbz))
#
# m5<- parameters::model_parameters(m5,  ci_method = "wald")
# m5
# # do not interpret any variable except the exposure
# plot(m5, show_labels = TRUE)
#
# # latex table
# m5 [, c(1:5)] %>%
#   # print_md()%>%
#   kbl("latex", booktabs = TRUE, digits = 2)
#
#
#
# m5_bayes <- brms::brm_multiple(
#   Respect.Self_lead2 ~  income_lead1_logc +
#     income_base_logc +
#     Respect.Self_bz +
#     partner18_bz +
#     age_z +
#     EthnicCats_b +
#     male_2z +
#     ubran_bz +
#     nzdep_bz +
#     parent_bz +
#     pol_bz +
#     church_bz_logc +
#     nr_bz +
#     ex_bz +
#     h_bz +
#     op_bz +
#     csbz +
#     agr_bz +
#     ChildrenNum_bz +
#     PWI_bz +
#     began_relationship_bz +
#     partnerlost_job_bz +
#     lost_job_bz +
#     HomeOwner_bz +
#     Hours.Work_logbz,
#   data = out2,
#   seed = 1234,
#   warmup = 1000,
#   iter = 2000,
#   chains = 4,
#   backend = "cmdstanr",
#   file = here::here( "mods", "m5_bayes")  #save model
# )
#
#
# pp_check(m5_bayes)
# #table
# lazerhawk::brms_SummaryTable(m5_bayes, panderize=F)
#
# # another table
# m5_bayes_table <- parameters::model_parameters(m5_bayes )
# m5_bayes_table
# plot(m5_bayes_table)
#
# # graph
# plot_smooth_m5 <- plot(conditional_effects(m5_bayes,  "income_lead1_logc", ndraws = 200, spaghetti = T), points = T, alpha = .01,
#                        point_args = list(alpha = .005, width = .1))
#
# # tidy graph
# m5_plot <- plot_smooth_m5[[1]] + # scale_x_continuous(limits = c(-5,5)) +
#   # scale_y_continuous(limits = c(1,7)) +
#   labs(title ="Self Respect",
#        subtitle = "",
#        y= "Self Respect (1-7)",
#        x = "Log income (centered)") #+ scale_colour_okabe_ito(alpha=.5)
#
# m5_plot
#
#

# rumination --------------------------------------------------------------



m6 <-
  with(
    out2,
    glm(
      Rumination_lead2 ~  income_lead1_logc + #inc_prop * # income_lead_log1c + #income_lead_log1c
        income_base_logc +   # baseline incoem
        Rumination_bz +
        partner18_bz +
        age_z +
        EthnicCats_b +
        male_2z +
        ubran_bz +
        nzdep_bz +
        parent_bz +
        pol_bz +
        church_bz_logc +
        nr_bz +
        ex_bz +
        h_bz +
        op_bz +
        csbz +
        agr_bz +
        ChildrenNum_bz +
        PWI_bz +
        began_relationship_bz +
        partnerlost_job_bz +
        lost_job_bz +
        HomeOwner_bz +
        Hours.Work_logbz
    )
  )

m6 <-
  parameters::model_parameters(m6,  ci_method = "wald")
m6
# do not interpret any variable except the exposure
plot(m6, show_labels = TRUE)

# latex table
m6 [, c(1:5)] %>%
  # print_md()%>%
  kbl("latex", booktabs = TRUE, digits = 2)


m6_bayes <- brms::brm_multiple(
  Rumination_lead2ord ~  income_lead1_logc +
    income_base_logc +
    Rumination_bz +
    partner18_bz +
    age_z +
    EthnicCats_b +
    male_2z +
    ubran_bz +
    nzdep_bz +
    parent_bz +
    pol_bz +
    church_bz_logc +
    nr_bz +
    ex_bz +
    h_bz +
    op_bz +
    csbz +
    agr_bz +
    ChildrenNum_bz +
    PWI_bz +
    began_relationship_bz +
    partnerlost_job_bz +
    lost_job_bz +
    HomeOwner_bz +
    Hours.Work_logbz,
  data = out2,
  seed = 1234,
  warmup = 1000,
  iter = 2000,
  chains = 4,
  backend = "cmdstanr",
  file = here::here("mods", "m6_bayes")  #save model
)

# # checks
pp_check(m6_bayes)  # Not good!


# #table
# lazerhawk::brms_SummaryTable(m6_bayes, panderize=F)
# # another table
# m6_bayes_table <- parameters::model_parameters(m6_bayes )
# #plot
# plot(m6_bayes_table)
# # graph
# plot_smooth_m6 <- plot(conditional_effects(m6_bayes,  "income_lead1_logc", ndraws = 200, spaghetti = T), points = T, alpha = .01,
#                        point_args = list(alpha = .005, width = .1))
#
# # tidy graph
# m6_plot <- plot_smooth_m6[[1]] + # scale_x_continuous(limits = c(-5,5)) +
#   # scale_y_continuous(limits = c(1,7)) +
#   labs(title ="Rumination",
#        subtitle = "",
#        y= "Rumination (0-4)",
#        x = "Log income (centered)") #+ scale_colour_okabe_ito(alpha=.5)
#
# m6_plot


# ORDINAL MODEL TBA

m6_bayes_ord <- brms::brm_multiple(
  Rumination_lead2ord ~  income_lead1_logc +
    income_base_logc +
    Rumination_bz +
    partner18_bz +
    age_z +
    EthnicCats_b +
    male_2z +
    ubran_bz +
    nzdep_bz +
    parent_bz +
    pol_bz +
    church_bz_logc +
    nr_bz +
    ex_bz +
    h_bz +
    op_bz +
    csbz +
    agr_bz +
    ChildrenNum_bz +
    PWI_bz +
    began_relationship_bz +
    partnerlost_job_bz +
    lost_job_bz +
    HomeOwner_bz +
    Hours.Work_logbz,
  family = cumulative("probit"),
  data = out2,
  init = 0,
  seed = 1234,
  warmup = 1000,
  iter = 2000,
  chains = 4,
  backend = "cmdstanr",
  file = here::here("mods", "m6_bayes_ord")  #save model
)


# checks
pp_check(m6_bayes_ord)  # NOto good
#table
lazerhawk::brms_SummaryTable(m6_bayes_ord, panderize =
                               F)
# another table
m6_bayes_ord_table <-
  parameters::model_parameters(m6_bayes_ord)
#plot
plot(m6_bayes_ord_table)
# graph
plot_smooth_m6_ord <-
  plot(
    conditional_effects(
      m6_bayes_ord,
      "income_lead1_logc",
      ndraws = 200,
      spaghetti = T
    ),
    points = T,
    alpha = .01,
    point_args = list(
      alpha = .005,
      width = .1,
      height = .1
    )
  )

# tidy graph
m6_plot <-
  plot_smooth_m6_ord[[1]] + # scale_x_continuous(limits = c(-5,5)) +
  # scale_y_continuous(limits = c(1,7)) +
  labs(
    title = "Rumination (ordinal regression) ",
    subtitle = "negative scored",
    y = "Rumination (1-5)",
    x = "Log income (centered)"
  ) #+ scale_colour_okabe_ito(alpha=.5)

m6_plot



# sf health ---------------------------------------------------------------

# SFHEALTH_bz +
# BELONG_bz +
# SUPPORT_bz,

m7 <-
  with(
    out2,
    glm(
      SFHEALTH_lead2 ~  income_lead1_logc + #inc_prop * # income_lead_log1c + #income_lead_log1c
        income_base_logc + # baseline incoem
        SFHEALTH_bz +
        partner18_bz +
        age_z +
        EthnicCats_b +
        male_2z +
        ubran_bz +
        nzdep_bz +
        parent_bz +
        pol_bz +
        church_bz_logc +
        nr_bz +
        ex_bz +
        h_bz +
        op_bz +
        cs_bz +
        agr_bz +
        ChildrenNum_bz +
        PWI_bz +
        began_relationship_bz +
        partnerlost_job_bz +
        lost_job_bz +
        HomeOwner_bz +
        Hours.Work_logbz
    )
  )

m7 <-
  parameters::model_parameters(m7,  ci_method = "wald")
m7
# do not interpret any variable except the exposure
plot(m7, show_labels = TRUE)

# latex table
m7 [, c(1:5)] %>%
  # print_md()%>%
  kbl("latex", booktabs = TRUE, digits = 2)



m7_bayes <- brms::brm_multiple(
  SFHEALTH_lead2 ~  income_lead1_logc +
    income_base_logc +
    SFHEALTH_bz +
    partner18_bz +
    age_z +
    EthnicCats_b +
    male_2z +
    ubran_bz +
    nzdep_bz +
    parent_bz +
    pol_bz +
    church_bz_logc +
    nr_bz +
    ex_bz +
    h_bz +
    op_bz +
    csbz +
    agr_bz +
    ChildrenNum_bz +
    PWI_bz +
    began_relationship_bz +
    partnerlost_job_bz +
    lost_job_bz +
    HomeOwner_bz +
    Hours.Work_logbz,
  data = out2,
  seed = 1234,
  warmup = 1000,
  iter = 2000,
  chains = 4,
  backend = "cmdstanr",
  file = here::here("mods", "m7_bayes")  #save model
)


# checks
pp_check(m7_bayes)  # NOto good
#table
lazerhawk::brms_SummaryTable(m7_bayes, panderize =
                               F)
# another table
m7_bayes_table <-
  parameters::model_parameters(m7_bayes)
#plot
plot(m7_bayes_table)
# graph
plot_smooth_m7 <-
  plot(
    conditional_effects(
      m7_bayes,
      "income_lead1_logc",
      ndraws = 200,
      spaghetti = T
    ),
    points = T,
    alpha = .01,
    point_args = list(alpha = .005, width = .1)
  )

# tidy graph
m7_plot <-
  plot_smooth_m7[[1]] + # scale_x_continuous(limits = c(-5,5)) +
  # scale_y_continuous(limits = c(1,7)) +
  labs(
    title = "Satisfaction with Health",
    subtitle = "",
    y = "Satisfaction with Health (1-7)",
    x = "Log income (centered)"
  ) #+ scale_colour_okabe_ito(alpha=.5)

m7_plot

# BELONG ----------------------------------------------------------------

m8 <-
  with(
    out2,
    glm(
      BELONG_lead2 ~  income_lead1_logc + #inc_prop * # income_lead_log1c + #income_lead_log1c
        income_base_logc +   # baseline incoem
        BELONG_bz +
        partner18_bz +
        age_z +
        EthnicCats_b +
        male_2z +
        ubran_bz +
        nzdep_bz +
        parent_bz +
        pol_bz +
        church_bz_logc +
        nr_bz +
        ex_bz +
        h_bz +
        op_bz +
        cs_bz +
        agr_bz +
        ChildrenNum_bz +
        PWI_bz +
        began_relationship_bz +
        partnerlost_job_bz +
        lost_job_bz +
        HomeOwner_bz +
        Hours.Work_logbz
    )
  )


m8 <-
  parameters::model_parameters(m8,  ci_method = "wald")
m8
# do not interpret any variable except the exposure
plot(m8, show_labels = TRUE)

# latex table
m8 [, c(1:5)] %>%
  # print_md()%>%
  kbl("latex", booktabs = TRUE, digits = 2)


m8_bayes <- brms::brm_multiple(
  BELONG_lead2 ~  income_lead1_logc + #inc_prop * # income_lead_log1c + #income_lead_log1c
    income_base_logc +   # baseline incoem
    BELONG_bz +
    partner18_bz +
    age_z +
    EthnicCats_b +
    male_2z +
    ubran_bz +
    nzdep_bz +
    parent_bz +
    pol_bz +
    church_bz_logc +
    nr_bz +
    ex_bz +
    h_bz +
    op_bz +
    csbz +
    agr_bz +
    ChildrenNum_bz +
    PWI_bz +
    began_relationship_bz +
    partnerlost_job_bz +
    lost_job_bz +
    HomeOwner_bz +
    Hours.Work_logbz,
  data = out2,
  seed = 1234,
  warmup = 1000,
  iter = 2000,
  chains = 4,
  backend = "cmdstanr",
  file = here::here("mods", "m8_bayes")  #save model
)

# checks
pp_check(m8_bayes)  #
#table
lazerhawk::brms_SummaryTable(m8_bayes, panderize =
                               F)
# another table
m8_bayes_table <-
  parameters::model_parameters(m8_bayes)
#plot
plot(m8_bayes_table)
# graph
plot_smooth_m8 <-
  plot(
    conditional_effects(
      m8_bayes,
      "income_lead1_logc",
      ndraws = 200,
      spaghetti = T
    ),
    points = T,
    alpha = .01,
    point_args = list(alpha = .005, width = .1)
  )

# tidy graph
m8_plot <-
  plot_smooth_m8[[1]] + # scale_x_continuous(limits = c(-5,5)) +
  # scale_y_continuous(limits = c(1,7)) +
  labs(
    title = "Social Belonging",
    subtitle = "",
    y = "Sense of Belonging (1-7)",
    x = "Log income (centered)"
  ) #+ scale_colour_okabe_ito(alpha=.5)

m8_plot

# support -----------------------------------------------------------------

m9 <-
  with(
    out2,
    glm(
      SUPPORT_lead2 ~  income_lead1_logc + #inc_prop * # income_lead_log1c + #income_lead_log1c
        income_base_logc +   # baseline incoem
        SUPPORT_bz  +
        partner18_bz +
        age_z +
        EthnicCats_b +
        male_2z +
        ubran_bz +
        nzdep_bz +
        parent_bz +
        pol_bz +
        church_bz_logc +
        nr_bz +
        ex_bz +
        h_bz +
        op_bz +
        cs_bz +
        agr_bz +
        ChildrenNum_bz +
        PWI_bz +
        began_relationship_bz +
        partnerlost_job_bz +
        lost_job_bz +
        HomeOwner_bz +
        Hours.Work_logbz
    )
  )


m9 <-
  parameters::model_parameters(m9,  ci_method = "wald")
m9
# do not interpret any variable except the exposure
plot(m9, show_labels = TRUE)

# latex table
m9 [, c(1:5)] %>%
  # print_md()%>%
  kbl("latex", booktabs = TRUE, digits = 2)


m9_bayes <- brms::brm_multiple(
  SUPPORT_lead2 ~  income_lead1_logc +
    income_base_logc +
    SUPPORT_bz  +
    partner18_bz +
    age_z +
    EthnicCats_b +
    male_2z +
    ubran_bz +
    nzdep_bz +
    parent_bz +
    pol_bz +
    church_bz_logc +
    nr_bz +
    ex_bz +
    h_bz +
    op_bz +
    csbz +
    agr_bz +
    ChildrenNum_bz +
    PWI_bz +
    began_relationship_bz +
    partnerlost_job_bz +
    lost_job_bz +
    HomeOwner_bz +
    Hours.Work_logbz,
  data = out2,
  seed = 1234,
  warmup = 1000,
  iter = 2000,
  chains = 4,
  backend = "cmdstanr",
  file = here::here("mods", "m9_bayes")  #save model
)

# checks
pp_check(m9_bayes)  # not good!

## ordinal


m9_bayes_ord <- brms::brm_multiple(
  SUPPORT_lead2ord ~  income_lead1_logc +
    income_base_logc +
    SUPPORT_bz  +
    partner18_bz +
    age_z +
    EthnicCats_b +
    male_2z +
    ubran_bz +
    nzdep_bz +
    parent_bz +
    pol_bz +
    church_bz_logc +
    nr_bz +
    ex_bz +
    h_bz +
    op_bz +
    csbz +
    agr_bz +
    ChildrenNum_bz +
    PWI_bz +
    began_relationship_bz +
    partnerlost_job_bz +
    lost_job_bz +
    HomeOwner_bz +
    Hours.Work_logbz,
  family = cumulative("probit"),
  data = out2,
  init = 0,
  seed = 1234,
  warmup = 1000,
  iter = 2000,
  chains = 4,
  backend = "cmdstanr",
  file = here::here("mods", "m9_bayes_ord")  #save model
)

## ppcheck
pp_check(m9_bayes_ord) # much better
#table
lazerhawk::brms_SummaryTable(m9_bayes_ord, panderize =
                               F)
# another table
m9_bayes_table <-
  parameters::model_parameters(m9_bayes_ord)
#plot
plot(m9_bayes_table)
# graph
plot_smooth_m9 <-
  plot(
    conditional_effects(
      m9_bayes,
      "income_lead1_logc",
      ndraws = 200,
      spaghetti = T
    ),
    points = T,
    alpha = .01,
    point_args = list(alpha = .005, width = .1)
  )

# tidy graph
m9_plot <-
  plot_smooth_m9[[1]] + # scale_x_continuous(limits = c(-5,5)) +
  # scale_y_continuous(limits = c(1,7)) +
  labs(
    title = "Social Belonging (ordinal regression) ",
    subtitle = "",
    y = "Sense of Belonging (1-7)",
    x = "Log income (centered)"
  ) #+ scale_colour_okabe_ito(alpha=.5)

m9_plot



# NWI ---------------------------------------------------------------------


m10 <-
  with(
    out2,
    glm(
      NWI_lead2 ~  income_lead1_logc + #inc_prop * # income_lead_log1c + #income_lead_log1c
        income_base_logc +   # baseline incoem
        NWI_bz  +
        partner18_bz +
        age_z +
        EthnicCats_b +
        male_2z +
        ubran_bz +
        nzdep_bz +
        parent_bz +
        pol_bz +
        church_bz_logc +
        nr_bz +
        ex_bz +
        h_bz +
        op_bz +
        cs_bz +
        agr_bz +
        ChildrenNum_bz +
        PWI_bz +
        began_relationship_bz +
        partnerlost_job_bz +
        lost_job_bz +
        HomeOwner_bz +
        Hours.Work_logbz
    )
  )


m10 <-
  parameters::model_parameters(m10,  ci_method = "wald")
m10
# do not interpret any variable except the exposure
plot(m10, show_labels = TRUE)

# latex table
m10 [, c(1:5)] %>%
  # print_md()%>%
  kbl("latex", booktabs = TRUE, digits = 2)



m10_bayes <- brms::brm_multiple(
  NWI_lead2 ~  income_lead1_logc +
    income_base_logc +
    NWI_bz  +
    partner18_bz +
    age_z +
    EthnicCats_b +
    male_2z +
    ubran_bz +
    nzdep_bz +
    parent_bz +
    pol_bz +
    church_bz_logc +
    nr_bz +
    ex_bz +
    h_bz +
    op_bz +
    csbz +
    agr_bz +
    ChildrenNum_bz +
    PWI_bz +
    began_relationship_bz +
    partnerlost_job_bz +
    lost_job_bz +
    HomeOwner_bz +
    Hours.Work_logbz,
  data = out2,
  seed = 1234,
  warmup = 1000,
  iter = 2000,
  chains = 4,
  backend = "cmdstanr",
  file = here::here("mods", "m10_bayes")  #save model
)
summary(m10_bayes)

## pp checks
pp_check(m10_bayes)
#table
lazerhawk::brms_SummaryTable(m10_bayes, panderize =
                               F)
# another table
m10_bayes_table <-
  parameters::model_parameters(m10_bayes)
#plot
plot(m10_bayes_table)
# graph
plot_smooth_m10 <-
  plot(
    conditional_effects(
      m10_bayes,
      "income_lead1_logc",
      ndraws = 200,
      spaghetti = T
    ),
    points = T,
    alpha = .01,
    point_args = list(alpha = .005, width = .1)
  )

# tidy graph
m10_plot <-
  plot_smooth_m10[[1]] + # scale_x_continuous(limits = c(-5,5)) +
  # scale_y_continuous(limits = c(1,7)) +
  labs(
    title = "National Wellbeing Index",
    subtitle = "",
    y = "National Wellbeing Index (1-10)",
    x = "Log income (centered)"
  ) #+ scale_colour_okabe_ito(alpha=.5)

m10_plot


# PWI --------------------------------------------------------------------



m11 <-
  with(
    out2,
    glm(
      PWI_lead2 ~  income_lead1_logc + #inc_prop * # income_lead_log1c + #income_lead_log1c
        income_base_logc +   # baseline incoem
        PWI_bz  +
        partner18_bz +
        age_z +
        EthnicCats_b +
        male_2z +
        ubran_bz +
        nzdep_bz +
        parent_bz +
        pol_bz +
        church_bz_logc +
        nr_bz +
        ex_bz +
        h_bz +
        op_bz +
        cs_bz +
        agr_bz +
        ChildrenNum_bz +
        PWI_bz +
        began_relationship_bz +
        partnerlost_job_bz +
        lost_job_bz +
        HomeOwner_bz +
        Hours.Work_logbz
    )
  )


m11 <-
  parameters::model_parameters(m11,  ci_method = "wald")
m11
# do not interpret any variable except the exposure
plot(m11, show_labels = TRUE)

# latex table
m11 [, c(1:5)] %>%
  # print_md()%>%
  kbl("latex", booktabs = TRUE, digits = 2)



m11_bayes <- brms::brm_multiple(
  PWI_lead2 ~  income_lead1_logc +
    income_base_logc +
    PWI_bz  +
    partner18_bz +
    age_z +
    EthnicCats_b +
    male_2z +
    ubran_bz +
    nzdep_bz +
    parent_bz +
    pol_bz +
    church_bz_logc +
    nr_bz +
    ex_bz +
    h_bz +
    op_bz +
    csbz +
    agr_bz +
    ChildrenNum_bz +
    PWI_bz +
    began_relationship_bz +
    partnerlost_job_bz +
    lost_job_bz +
    HomeOwner_bz +
    Hours.Work_logbz,
  data = out2,
  seed = 1234,
  warmup = 1000,
  iter = 2000,
  chains = 4,
  backend = "cmdstanr",
  file = here::here("mods", "m11_bayes")  #save model
)

summary(m11_bayes)


## pp checks
pp_check(m11_bayes)
#table
lazerhawk::brms_SummaryTable(m11_bayes, panderize =
                               F)
# another table
m11_bayes_table <-
  parameters::model_parameters(m11_bayes)
#plot
plot(m11_bayes_table)
# graph
plot_smooth_m11 <-
  plot(
    conditional_effects(
      m11_bayes,
      "income_lead1_logc",
      ndraws = 200,
      spaghetti = T
    ),
    points = T,
    alpha = .01,
    point_args = list(alpha = .005, width = .1)
  )

# tidy graph
m11_plot <-
  plot_smooth_m11[[1]] + # scale_x_continuous(limits = c(-5,5)) +
  # scale_y_continuous(limits = c(1,7)) +
  labs(
    title = "Personal Wellbeing Index ",
    subtitle = "",
    y = "Personal Wellbeing Index (1-7)",
    x = "Log income (centered)"
  ) #+ scale_colour_okabe_ito(alpha=.5)

m11_plot


# Children ----------------------------------------------------------------


m12 <-
  with(
    out2,
    glm(
      ChildrenNum_lead2 ~  income_lead1_logc + #inc_prop * # income_lead_log1c + #income_lead_log1c
        income_base_logc +   # baseline incoem
        ChildrenNum_bz  +
        partner18_bz +
        age_z +
        EthnicCats_b +
        male_2z +
        ubran_bz +
        nzdep_bz +
        parent_bz +
        pol_bz +
        church_bz_logc +
        nr_bz +
        ex_bz +
        h_bz +
        op_bz +
        cs_bz +
        agr_bz +
        PWI_bz +
        began_relationship_bz +
        partnerlost_job_bz +
        lost_job_bz +
        HomeOwner_bz +
        Hours.Work_logbz
    )
  )

#
m12 <-
  parameters::model_parameters(m12,  ci_method = "wald")
m12
# # do not interpret any variable except the exposure
# plot(m12, show_labels = TRUE)
#
# # latex table
# m12 [, c(1:5)] %>%
#   # print_md()%>%
#   kbl("latex", booktabs = TRUE, digits = 2)
#
m12_bayes <- brms::brm_multiple(
  ChildrenNum_lead2 ~  income_lead1_logc +
    income_base_logc +
    ChildrenNum_bz  +
    partner18_bz +
    age_z +
    EthnicCats_b +
    male_2z +
    ubran_bz +
    nzdep_bz +
    parent_bz +
    pol_bz +
    church_bz_logc +
    nr_bz +
    ex_bz +
    h_bz +
    op_bz +
    csbz +
    agr_bz +
    PWI_bz +
    began_relationship_bz +
    partnerlost_job_bz +
    lost_job_bz +
    HomeOwner_bz +
    Hours.Work_logbz,
  data = out2,
  seed = 1234,
  warmup = 1000,
  iter = 2000,
  chains = 4,
  backend = "cmdstanr",
  file = here::here("mods", "m12_bayes")  #save model
)


## pp checks
pp_check(m12_bayes)
#table
lazerhawk::brms_SummaryTable(m12_bayes, panderize =
                               F)
# another table
m12_bayes_table <-
  parameters::model_parameters(m12_bayes)
#plot
plot(m12_bayes_table)
# graph
plot_smooth_m12 <-
  plot(
    conditional_effects(
      m12_bayes,
      "income_lead1_logc",
      ndraws = 200,
      spaghetti = T
    ),
    points = T,
    alpha = .01,
    point_args = list(alpha = .005, width = .1)
  )

# tidy graph
m12_plot <-
  plot_smooth_m12[[1]] + # scale_x_continuous(limits = c(-5,5)) +
  scale_y_continuous(limits = c(1, 3)) +
  labs(
    title = "Children counts",
    subtitle = "",
    y = "Children",
    x = "Log income (centered)"
  ) #+ scale_colour_okabe_ito(alpha=.5)

m12_plot


# wlbalence ---------------------------------------------------------------

# Loss of work life balance


m13 <-
  with(
    out2,
    glm(
      Emp.WorkLifeBalance_z ~  income_lead1_logc + #inc_prop * # income_lead_log1c + #income_lead_log1c
        income_base_logc +   # baseline incoem
        partner18_bz +
        age_z +
        EthnicCats_b +
        male_2z +
        ubran_bz +
        nzdep_bz +
        parent_bz +
        pol_bz +
        church_bz_logc +
        nr_bz +
        ex_bz +
        h_bz +
        op_bz +
        cs_bz +
        agr_bz +
        ChildrenNum_bz +
        PWI_bz +
        began_relationship_bz +
        partnerlost_job_bz +
        lost_job_bz +
        HomeOwner_bz +
        Hours.Work_logbz
    )
  )


m13 <-
  parameters::model_parameters(m13,  ci_method = "wald")
m13
# do not interpret any variable except the exposure
plot(m13, show_labels = TRUE)

# latex table
m13 [, c(1:5)] %>%
  # print_md()%>%
  kbl("latex", booktabs = TRUE, digits = 2)


m13_bayes <- brms::brm_multiple(
  Emp.WorkLifeBalance_z ~  income_lead1_logc + #inc_prop * # income_lead_log1c + #income_lead_log1c
    income_base_logc +   # baseline incoem
    partner18_bz +
    age_z +
    EthnicCats_b +
    male_2z +
    ubran_bz +
    nzdep_bz +
    parent_bz +
    pol_bz +
    church_bz_logc +
    nr_bz +
    ex_bz +
    h_bz +
    op_bz +
    csbz +
    agr_bz +
    ChildrenNum_bz +
    PWI_bz +
    began_relationship_bz +
    partnerlost_job_bz +
    lost_job_bz +
    HomeOwner_bz +
    Hours.Work_logbz,
  data = out2,
  seed = 1234,
  warmup = 1000,
  iter = 2000,
  chains = 4,
  backend = "cmdstanr",
  file = here::here("mods", "m13_bayes")  #save model
)



## pp checks
pp_check(m13_bayes)
#table
lazerhawk::brms_SummaryTable(m13_bayes, panderize =
                               F)
# another table
m13_bayes_table <-
  parameters::model_parameters(m13_bayes)
#plot
plot(m13_bayes_table)
# graph
plot_smooth_m13 <-
  plot(
    conditional_effects(
      m13_bayes,
      "income_lead1_logc",
      ndraws = 200,
      spaghetti = T
    ),
    points = T,
    alpha = .01,
    point_args = list(alpha = .005, width = .1)
  )

# tidy graph
m13_plot <-
  plot_smooth_m13[[1]] + # scale_x_continuous(limits = c(-5,5)) +
  # scale_y_continuous(limits = c(1,7)) +
  labs(
    title = "Life Work Balance",
    subtitle = "",
    y = "Life Work Balance (Standardised)",
    x = "Log income (centered)"
  ) #+ scale_colour_okabe_ito(alpha=.5)

m13_plot



library(patchwork)

# move so that the stand deviations axises are on the same line
combo_plot_income <-
  m1_plot + m2_plot + m3_plot  + m4_plot  + m6_plot  + m7_plot +
  m8_plot + m9_plot  +  m10_plot + m11_plot + m12_plot + m13_plot +
  plot_annotation(tag_levels = "i",
                  title = "Causal effect of income on dimensions of Flourishing",
                  subtitle = "New Zealand Attitudes and Values Study, years 2018-2021, N = 19,462") +
  plot_layout(ncol = 3)
combo_plot_income

ggsave(
  combo_plot_income,
  path = here::here(here::here("mods")),
  width = 15,
  height = 15,
  units = "in",
  filename = "combo_plot_income.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 1000
)
