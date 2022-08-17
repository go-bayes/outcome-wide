# church-use R
# set digits = 3
options(scipen=999)

#libraries and functions
source(here::here("scripts", "libs.R"))
source(here::here("scripts", "funs.R"))

### ELIGIBILITY CRITERIA
# read data
dat <- readRDS(here::here("data_raw", "df.Rds"))

# table for participant N
tab_in <- dat %>%
  dplyr::mutate(Euro = if_else(EthCat == 1, 1, 0)) %>%
  dplyr::mutate(Male = ifelse(GendAll == 1, 1, 0)) %>%
  dplyr::filter((Wave == 2018  & YearMeasured  == 1) |
                  (Wave == 2019  &
                     YearMeasured  == 1) |
                  (Wave == 2020))  %>% # Eligibility criteria
  dplyr::filter(YearMeasured  != -1) %>% # remove people who passed away
  # dplyr::filter(Id != 9630) %>% # problematic for income
  group_by(Id) %>%
  dplyr::mutate(org2018 =  ifelse(Wave == 2018 &
                                    YearMeasured == 1, 1, 0)) %>%  # creating an indicator for the first wave
  dplyr::mutate(hold18 = mean(org2018, na.rm = TRUE)) %>%  # Hack
  dplyr::filter(hold18 > 0) %>% # hack to enable repeat of baseline
  dplyr::mutate(org2019 = ifelse(Wave == 2019 &
                                   YearMeasured == 1, 1, 0)) %>%  # creating an indicator for the first wave
  dplyr::mutate(hold19 = mean(org2019, na.rm = TRUE)) %>%  # Hack0
  dplyr::filter(hold19 > 0) %>% # hack to enable repeat of baseline in 201
  ungroup() %>%
  droplevels() %>%
  arrange(Id, Wave)
# check n # 34782

length(unique(tab_in$Id)) # 34783




# increasing rate
dat%>%
  group_by(Wave) %>%
  summarise(mean(HLTH.Disability, na.rm = TRUE))

# Do you have a health condition or disability that limits you, and that has lasted for 6+ months?

dat$Religion.Church2

## select vars
df_cr <- tab_in %>%
 # dplyr::filter(Id != 9630) %>% # problematic
  select(
    Id,
    YearMeasured,
    Wave,
    Partner,
    Euro,
    Age,
    Male,
    NZSEI13,
    CONSCIENTIOUSNESS,
    OPENNESS,
    HONESTY_HUMILITY,
    EXTRAVERSION,
    NEUROTICISM,
    AGREEABLENESS,
    Edu,
    SDO,
    RWA,
    NZdep,
    Employed,
    HomeOwner,
    Pol.Orient,
    Urban,
    Household.INC,
    Parent,
    Relid,
    Religious,
    Religion.Church2,
    Believe.Spirit,
    Believe.God,
    Spiritual.Identification,
    SWB.SoC01,
    # EmotionRegulation1,
    # EmotionRegulation2,
    # EmotionRegulation3,
    Bodysat,
    VENGEFUL.RUMIN,
    retired,
    semiretired,
    BornNZ,
    KESSLER6sum,
    HLTH.Fatigue,
    Rumination,
    Smoker,
    ChildrenNum,
    NWI,
    BELONG,
    SUPPORT,
    CharityDonate,
    HoursCharity,
    GRATITUDE,
    # Volunteers,
    Hours.Work,
    HLTH.SleepHours,
    HLTH.Disability,
    Hours.Exercise,
    LIFEMEANING,
    LIFESAT,
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
    POWERDEPENDENCE1,
    POWERDEPENDENCE2,
    Your.Future.Security,
    Your.Personal.Relationships,
    Your.Health,
    Standard.Living,
    #Env.SacWilling,
    #Env.SacMade,
    PERFECTIONISM,
    PermeabilityIndividual,
    ImpermeabilityGroup
    # Emp.JobSecure,
    # Env.ClimateChgCause,
    # Env.ClimateChgReal #,
  ) %>%
  dplyr::rename(community = SWB.SoC01) %>%
  dplyr::mutate(Edu = as.numeric(Edu)) %>%
  dplyr::mutate(across(!c(Id, Wave), ~ as.numeric(.x))) %>% # make factors numeric for easy of processing
  arrange(Id, Wave) %>%
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
    Church = ifelse(Religion.Church2 > 8, 8, Religion.Church2),
    income_log = log(Household.INC + 1),
  ) %>%
  arrange(Id, Wave)  %>% #
  dplyr::mutate(Church_lead1 = lead(Church, n = 1)) %>%
  #dplyr::mutate(Church_lead1 = lead(Church, n = 1)) %>%  Your.Future.Security
  # inc_prop = (income_log / (income_log_lead1) - 1),
  dplyr::mutate(across(
    c(
      KESSLER6sum,
      HLTH.Fatigue,
      Rumination,
      community,
      SFHEALTH,
      LIFEMEANING,
      LIFESAT,
      Hours.Work,
      SELF.ESTEEM,
      SELF.CONTROL,
      Respect.Self,
      Alcohol.Frequency,
      HLTH.SleepHours,
      Hours.Exercise,
      HLTH.BMI,
      HLTH.Disability,
      Smoker,
      NWI,
      BELONG,
      SUPPORT,
      Volunteers,
      GRATITUDE,
      SexualSatisfaction,
      POWERDEPENDENCE1,
      POWERDEPENDENCE2,
      CharityDonate,
      Alcohol.Intensity,
      PERFECTIONISM,
      Bodysat,
      VENGEFUL.RUMIN,
      community,
      HONESTY_HUMILITY,
     # EmotionRegulation1,
     # EmotionRegulation2,
     # EmotionRegulation3, Every high missingness
      Emp.WorkLifeBalance,
      PermeabilityIndividual,
      ImpermeabilityGroup,
      Your.Future.Security,
      Your.Personal.Relationships,
      Your.Health,
      Standard.Living,
      PermeabilityIndividual,
      ImpermeabilityGroup,
      NZSEI13
    ),
    ~ lead(.x, n = 2),
    .names = "{col}_lead2"
  )) %>% # make leads
  dplyr::filter(Wave == 2018) %>%
  # dplyr::filter(retired != 1) %>%
  # dplyr::filter(retired_lead1 != 1) %>%  #needed for the intervention
  # dplyr::filter(semiretired != 1) %>%
  #dplyr::filter(semiretired_lead1 != 1) %>%
  #dplyr::filter(!is.na(income_log_lead1) )%>%  #   ABOUT
  #dplyr::filter(!is.na(income_log) )%>% #  THINK ABOUT
 # dplyr::filter(Household.INC >= 30975) %>% # min income
  # dplyr::filter(income_log_lead1 > income_log) %>%
  dplyr::filter(!is.na(Church)) %>%
  dplyr::filter(!is.na(Church_lead1)) %>%
  dplyr::mutate(Religious = as.numeric(Religious)-1) |>
  dplyr::filter(Religious == 1) %>%
  #dplyr::filter(!is.na(Standard.Living) )%>%
  # dplyr::filter(!is.na(Standard.Living_lead1) )%>%
  #  dplyr::filter(semiretired_lead1 != 1) %>%  #needed for the intervention
  dplyr::select(-c(
    Religion.Church2,
    # EthCat,
    Religious,
    HoursCharity,
    Respect.Self_lead2,
    Household.INC,
    #  org2018,
    #  not_euro,
    #  not_euro_lead2,
    # hold18,
    #   Euro,
    Emp.WorkLifeBalance,
    YearMeasured,
    #HLTH.Disability_lead1,
    # org2019,
    # hold19,
    # retired,
    # semiretired,
  )
  ) %>%
  #  dplyr::mutate(across(!c(Id,Wave), ~ scale(.x)))%>%  # standarise vars for easy computing-- do this after imputation
  arrange(Id, Wave) %>%
  droplevels() %>%
  data.frame() %>%
  mutate(across(where(is.double), as.numeric)) %>%
  arrange(Id)



table1::table1(~Church + SDO| Wave , data = df_cr, overall = FALSE)#11953


# Filtering retirement -- consistency and positivity assumptions
# number of ids
N <- length(unique(df_cr$Id))
N  #11953

# inspect data
skim(df_cr)


# mice model  -------------------------------------------------------------
library(mice)

mice_cr <- df_cr %>%
  dplyr::select(-c( Wave, Id))  # won't otherwise run

hist(mice_cr$SDO)
library(naniar)
naniar::gg_miss_var(mice_cr)
vis_miss(mice_cr,
         warn_large_data = FALSE)

# any colinear vars?
mice:::find.collinear(mice_cr)

# impute
cr_mice <- mice::mice(mice_cr,  seed = 0, m = 10)

# save
saveh(cr_mice, "cr_mice")

# read
cr_mice <- readh("cr_mice")

# checks
outlist2 <-
  row.names(cr_mice)[cr_mice$outflux < 0.5]
length(outlist2)

# checks
head(cr_mice$loggedEvents, 10)

# data warangling
# we create two completed data sets -- the one without the missing data will be useful for
# determing causal contrasts -- which we'll describe below.

mf <- mice::complete(cr_mice, "long", inc = F)
ml <- mice::complete(cr_mice, "long", inc = TRUE)


# inspect data -- what do we care about?  Note that the moderate distress category doesn't look useful
skimr::skim(ml)

# create variables in z score
ml <- ml %>%
  # dplyr::mutate(newkids = ChildrenNum_lead2 - ChildrenNum) %>%
  dplyr::mutate(KESSLER6sum_lead2 = round(as.integer(KESSLER6sum_lead2, 0)))%>%
  dplyr::mutate(Alcohol.Intensity_lead2 = round(Alcohol.Intensity_lead2, 0))%>%
  dplyr::mutate(CharityDonate_lead2 = round(CharityDonate_lead2, 0))%>%
  dplyr::mutate(Hours.Exercise_lead2 = round(Hours.Exercise_lead2, 0))%>%
  dplyr::mutate(Hours.Exercise_lead2_log = log(Hours.Exercise_lead2 + 1))%>%
  plyr::mutate(Alcohol.Intensity = round(Alcohol.Intensity, 0))%>%
  dplyr::mutate(CharityDonate = round(CharityDonate, 0))%>%
  dplyr::mutate(Hours.Exercise = round(Hours.Exercise, 0))%>%
  dplyr::mutate(CharityDonate_log_lead2 = log(CharityDonate_lead2+1))%>%
  dplyr::mutate(Alcohol.Intensity_log_lead2 = log(Alcohol.Intensity_lead2+1))%>%
  dplyr::mutate(Exercise_log_lead2 = log(Hours.Exercise_lead2+1))%>%
  dplyr::mutate(CharityDonate_log = log(CharityDonate+1))%>%
  dplyr::mutate(Alcohol.Intensity_log = log(Alcohol.Intensity+1))%>%
  dplyr::mutate(Rumination_lead2ord = as.integer(round(Rumination_lead2, digits = 0) + 1)) %>%  # needs to start at 1
  dplyr::mutate(SUPPORT_lead2ord = as.integer(round(SUPPORT_lead2, digits = 0) )) %>%
  dplyr::mutate(PERFECTIONISM_lead2ord = as.integer(round(PERFECTIONISM_lead2, digits = 0) )) %>%
  dplyr::mutate(VENGEFUL.RUMIN_lead2ord = as.integer(round(VENGEFUL.RUMIN_lead2, digits = 0) )) %>%
  dplyr::mutate(Standard.Living_lead2ord = as.integer(round(Standard.Living_lead2, digits = 0) )) %>%
  dplyr::mutate(Your.Personal.Relationships_lead2ord = as.integer(round(Your.Personal.Relationships_lead2, digits = 0) +1)) %>%
  dplyr::mutate(LIFEMEANING_lead2ord = as.integer(round(LIFEMEANING_lead2, digits = 0) )) %>%
  dplyr::mutate(HLTH.Fatigue_lead2ord = as.integer(round(HLTH.Fatigue_lead2, digits = 0)+1 )) %>%
  dplyr::mutate(Hours.Exercise_log = log(Hours.Exercise+1))%>%
  dplyr::mutate(Alcohol.Frequency_lead2ord = as.integer(round(Alcohol.Frequency_lead2, 0)+1))%>%
  dplyr::mutate(LIFESAT_lead2ord = as.integer(round(LIFESAT_lead2, digits = 0) )) %>%
  dplyr::mutate(alcohol_bin2 = if_else( Alcohol.Frequency > 3, 1, 0))%>%
  dplyr::mutate(alcohol_bin = if_else( Alcohol.Frequency > 2, 1, 0))%>%
  dplyr::mutate(Hours.Work_10 =  Hours.Work/10)%>%
  # dplyr::mutate(Hours.Work_lead1_10 =  as.integer(Hours.Work_lead1/10))%>%
  # dplyr::mutate(Hours.Work_lead1_sqrt =  as.integer(sqrt(Hours.Work_lead1)))%>%
  dplyr::mutate(NZSEI13_10 =  NZSEI13/10)%>%
  dplyr::mutate(NZSEI13_lead2_10 =  as.integer(NZSEI13_lead2/10))%>%
  dplyr::mutate(across(where(is.numeric), ~ scale(.x), .names = "{col}_z")) %>%
  select(-c(.imp_z, .id_z))%>%
  dplyr::mutate(id = as.factor(rep(1:N, 11)))# needed for g-comp# Respect for Self is fully missing



# for models wihout looping (not advised)

mf <- mf %>%
  # dplyr::mutate(newkids = ChildrenNum_lead2 - ChildrenNum) %>%
  dplyr::mutate(KESSLER6sum_lead2 = round(as.integer(KESSLER6sum_lead2, 0)))%>%
  dplyr::mutate(Alcohol.Intensity_lead2 = round(Alcohol.Intensity_lead2, 0))%>%
  dplyr::mutate(CharityDonate_lead2 = round(CharityDonate_lead2, 0))%>%
  dplyr::mutate(Hours.Exercise_lead2 = round(Hours.Exercise_lead2, 0))%>%
  dplyr::mutate(Hours.Exercise_lead2_log = log(Hours.Exercise_lead2 + 1))%>%
  plyr::mutate(Alcohol.Intensity = round(Alcohol.Intensity, 0))%>%
  dplyr::mutate(CharityDonate = round(CharityDonate, 0))%>%
  dplyr::mutate(Hours.Exercise = round(Hours.Exercise, 0))%>%
  dplyr::mutate(CharityDonate_log_lead2 = log(CharityDonate_lead2+1))%>%
  dplyr::mutate(Alcohol.Intensity_log_lead2 = log(Alcohol.Intensity_lead2+1))%>%
  dplyr::mutate(Exercise_log_lead2 = log(Hours.Exercise_lead2+1))%>%
  dplyr::mutate(CharityDonate_log = log(CharityDonate+1))%>%
  dplyr::mutate(Alcohol.Intensity_log = log(Alcohol.Intensity+1))%>%
  dplyr::mutate(Rumination_lead2ord = as.integer(round(Rumination_lead2, digits = 0) + 1)) %>%  # needs to start at 1
  dplyr::mutate(SUPPORT_lead2ord = as.integer(round(SUPPORT_lead2, digits = 0) )) %>%
  dplyr::mutate(PERFECTIONISM_lead2ord = as.integer(round(PERFECTIONISM_lead2, digits = 0) )) %>%
  dplyr::mutate(VENGEFUL.RUMIN_lead2ord = as.integer(round(VENGEFUL.RUMIN_lead2, digits = 0) )) %>%
  dplyr::mutate(Standard.Living_lead2ord = as.integer(round(Standard.Living_lead2, digits = 0) )) %>%
  dplyr::mutate(Your.Personal.Relationships_lead2ord = as.integer(round(Your.Personal.Relationships_lead2, digits = 0) +1)) %>%
  dplyr::mutate(LIFEMEANING_lead2ord = as.integer(round(LIFEMEANING_lead2, digits = 0) )) %>%
  dplyr::mutate(HLTH.Fatigue_lead2ord = as.integer(round(HLTH.Fatigue_lead2, digits = 0)+1 )) %>%
  dplyr::mutate(Hours.Exercise_log = log(Hours.Exercise+1))%>%
  dplyr::mutate(Alcohol.Frequency_lead2ord = as.integer(round(Alcohol.Frequency_lead2, 0)+1))%>%
  dplyr::mutate(LIFESAT_lead2ord = as.integer(round(LIFESAT_lead2, digits = 0) )) %>%
  dplyr::mutate(alcohol_bin2 = if_else( Alcohol.Frequency > 3, 1, 0))%>%
  dplyr::mutate(alcohol_bin = if_else( Alcohol.Frequency > 2, 1, 0))%>%
  # dplyr::mutate(Hours.Work_lead1_10 =  as.integer(Hours.Work_lead1/10))%>%
  # dplyr::mutate(Hours.Work_lead1_sqrt =  as.integer(sqrt(Hours.Work_lead1)))%>%
  dplyr::mutate(NZSEI13_10 =  NZSEI13/10)%>%
  dplyr::mutate(NZSEI13_lead2_10 =  as.integer(NZSEI13_lead2/10))%>%
  dplyr::mutate(across(where(is.numeric), ~ scale(.x), .names = "{col}_z")) %>%
  select(-c(.imp_z, .id_z)) %>%
  dplyr::mutate(id = as.factor(rep(1:N, 10)))# needed for g-comp


# Get data into shape
mf <- mf %>% mutate_if(is.matrix, as.vector)
ml <- ml %>% mutate_if(is.matrix, as.vector)

ml <- mice::as.mids(ml)

saveh(ml, "churchl_cr")
saveh(mf, "churchf_cr")



###### READ THIS DATA IN   #########
ml <- readh("churchl")
mf <- readh("churchf")


# model equations ---------------------------------------------------------

baselinevars = c("AGREEABLENESS_z","CONSCIENTIOUSNESS_z","EXTRAVERSION_z","HONESTY_HUMILITY_z","NEUROTICISM_z","OPENNESS_z","Age_z","Alcohol.Frequency_z","Alcohol.Intensity_log_z","Bodysat_z","BornNZ_z","Believe.God_z","Believe.Spirit_z","BELONG_z","CharityDonate_log_z","ChildrenNum_z","Church_z", "community","Edu_z","Employed_z","Euro_z", "GRATITUDE_z","HomeOwner_z","Hours.Exercise_log_z","Hours.Work_z","HLTH.BMI_z", "HLTH.Disability_z", "HLTH.Fatigue_z", "HLTH.SleepHours_z", "ImpermeabilityGroup_z", "income_log_z", "KESSLER6sum_z", "LIFEMEANING_z", "LIFESAT_z", "Male_z","NZdep_z", "NWI_z","NZSEI13_z","Parent_z","Partner_z","PERFECTIONISM_z", "PermeabilityIndividual_z", "Pol.Orient_z", "POWERDEPENDENCE1_z","POWERDEPENDENCE2_z","Relid_z", "Respect.Self_z", "retired", "Rumination_z","RWA_z","SDO_z", "SELF.CONTROL_z", "SELF.ESTEEM_z", "semiretired", "SexualSatisfaction_z","SFHEALTH_z","Smoker_z", "Spiritual.Identification_z","Standard.Living_z", "SUPPORT_z","Urban_z", "VENGEFUL.RUMIN_z", "Volunteers_z", "Your.Health_z", "Your.Future.Security_z", "Your.Personal.Relationships_z")


# functions ---------------------------------------------------------------

# see "funs.R"

## Also use
round( EValue::evalues.OLS( , se = , sd = 1, delta = 2, true = 0), 3)
round( EValue::evalues.RR( , lo =  , hi =, true = 1), 4)


# set up vars -------------------------------------------------------------

ylim <- c(-.25,.25)
xlab <- "Monthly Church"
df <-  ml
m = 10
X = "Church_lead1"
x =  0:8
xlab = "Church_lead1"
c = c(0,4)
# reference level
r = 0



# bmi ---------------------------------------------------------------------

# prepare
out_f = function(formula) {
  with(ml, glm(as.formula(paste("HLTH.BMI_lead2_z ~ bs(Church_lead1) +",
                                paste(baselinevars,
                                      collapse = "+")))) )
}

# labels
main = "BMI"
ylab = "BMI (SD)"

# clean oven
rm(out_m)
rm(pool_m)
rm(out_ct)

# bake
out_m <- out_f()
out_m

## contrasts
out_ct <- pool_stglm_contrast(out_m, df = df, m = m,  X = X, x = c, r= r)

# graph of contrasts
ggplot_stglm_contrast(out_ct, ylim, main, xlab, ylab)

# table
out_ct %>%
  kbl(format = "markdown", booktabs = T, digits = 3)

# out_ct %>%
#   kbl(format = "latex", booktabs = T, digits = 3)

# evalues
round( EValue::evalues.OLS( , se = 0.018, sd = 1, delta = 4, true = 0), 3)
round( EValue::evalues.RR( , lo =  , hi =, true = 1), 4)


# g-computation
pool_m <- pool_stglm(out_m, df = df, m = m, x = x,  X = X)
pool_m %>%
  kbl(format = "markdown", booktabs = T, digits = 3)

# graph
ggplot_stglm(pool_m, ylim, main, xlab, ylab, c=c)


#
# boostrap
# library(marginaleffects)
# library(boot)
# bootfun <- function(data, indices, ...) {
#   d <- data[indices, ]
#   mod <- glm(as.formula(paste("HLTH.BMI_lead2_z ~ bs(Church_lead1) +",
#                               paste(baselinevars,
#                                     collapse = "+"))), data =d)
#   cmp <- comparisons(mod, newdata = datagrid(), vcov = FALSE, variables = list(Church_lead1 = c(0,4)))
#   tidy(cmp)$estimate
# }
#
# b <- boot(data = mf, statistic = bootfun, R = 3)
# b
# # Bootstrap Statistics :
# #   original      bias    std. error
# # t1* -0.004582486 0.005836902 0.005184014
# round( EValue::evalues.OLS(0.003680699  , se = 0.00522961, sd = 1, delta = 4, true = 0), 3)
# round( EValue::evalues.OLS(0.003654699  , se = 0.01821223, sd = 1, delta = 4, true = 0), 3)


# sf-health ---------------------------------------------------------------
## fit
fit_sfhealth = function(formula) {
  with(ml, glm(as.formula(paste("SFHEALTH_lead2_z~ bs(Church_lead1) +",
                                 paste(baselinevars,
                                       collapse = "+")))))
}

#labels
main = "SF Health"
ylab = "SF Health (SD)"

# clean oven
rm(out_m)
rm(pool_m)
rm(out_ct)

# bake
out_m <- out_f()

## contrasts
out_ct <- pool_stglm_contrast(out_m, df = df, m = m,  X = X, x = c, r= r)

# graph of contrasts
ggplot_stglm_contrast(out_ct, ylim, main, xlab, ylab)

# table
out_ct %>%
  kbl(format = "markdown", booktabs = T, digits = 3)

# out_ct %>%
# kbl(format = "latex", booktabs = T, digits = 3)

# evalues
round( EValue::evalues.OLS( , se = , sd = 1, delta = 4, true = 0), 3)
round( EValue::evalues.RR( , lo =  , hi =, true = 1), 4)

# g-computation
pool_m <- pool_stglm(out_m, df = df, m = m, x = x,  X = X)

# graph
ggplot_stglm(pool_m, ylim, main, xlab, ylab, c=c)

# exercise ---------------------------------------------------------------
out_f = function(formula) {
  with(ml, glm(as.formula(paste("Hours.Exercise_lead2_log_z~ bs(Church_lead1) +",  paste(baselinevars,collapse = "+")))))
}

# lb
main = "Log Hours Exercise"
ylab = "Log Hours Exercise (SD)"
# clean oven
rm(out_m)
rm(pool_m)
rm(out_ct)

# bake
out_m <- out_f()

## contrasts
out_ct <- pool_stglm_contrast(out_m, df = df, m = m,  X = X, x = c, r= r)

# graph of contrasts
ggplot_stglm_contrast(out_ct, ylim, main, xlab, ylab)

# table
out_ct %>%
  kbl(format = "markdown", booktabs = T, digits = 3)

# out_ct %>%
# kbl(format = "latex", booktabs = T, digits = 3)

# evalues
round( EValue::evalues.OLS( , se = , sd = 1, delta = 4, true = 0), 3)
round( EValue::evalues.RR( , lo =  , hi =, true = 1), 4)

# g-computation
pool_m <- pool_stglm(out_m, df = df, m = m, x = x,  X = X)

# graph
ggplot_stglm(pool_m, ylim, main, xlab, ylab, c=c)

# HLTH.Sleep --------------------------------------------------------------
## fit
out_f = function(formula) {
  with(ml, glm(as.formula(paste("HLTH.SleepHours_lead2_z ~ bs(Church_lead1) +",  paste(baselinevars,collapse = "+")))))
}
#lb
main = "Hours Sleep (SD)"
ylab = "Hours Sleep (SD)"
# clean oven
rm(out_m)
rm(pool_m)
rm(out_ct)

# bake
out_m <- out_f()

## contrasts
out_ct <- pool_stglm_contrast(out_m, df = df, m = m,  X = X, x = c, r= r)

# graph of contrasts
ggplot_stglm_contrast(out_ct, ylim, main, xlab, ylab)

# table
out_ct %>%
  kbl(format = "markdown", booktabs = T, digits = 3)

# out_ct %>%
# kbl(format = "latex", booktabs = T, digits = 3)

# evalues
round( EValue::evalues.OLS( , se = , sd = 1, delta = 4, true = 0), 3)
round( EValue::evalues.RR( , lo =  , hi =, true = 1), 4)

# g-computation
pool_m <- pool_stglm(out_m, df = df, m = m, x = x,  X = X)

# graph
ggplot_stglm(pool_m, ylim, main, xlab, ylab, c=c)

#
# sd(wf3$HLTH.SleepHours_lead2)
# 1.061555 * number  # t/10th standard deviation
# 60 * number)

# ;smoker ------------------------------------------------------------------

# fit
out_f = function(formula) {
  with(ml, glm(as.formula(paste("Smoker_lead2 ~ bs(Church_lead1) +",
                                 paste(baselinevars,
                                       collapse = "+"))), family = "poisson"))
}
main = "Smoking Rate"
ylab = "Smoking Rate"
# clean oven
rm(out_m)
rm(pool_m)
rm(out_ct)

# bake
out_m <- out_f()

## contrasts (note ratio)
out_ct <- pool_stglm_contrast_ratio(out_m, df = df, m = m,  X = X, x = c, r= r)

ggplot_stglm_contrast(out_ct, ylim=c(0,2), main, xlab, ylab)

# table
out_ct %>%
  kbl(format = "markdown", booktabs = T, digits = 3)

# evalues
round( EValue::evalues.RR( , lo =  , hi =, true = 1), 4)


# fatigue -----------------------------------------------------------------
# fit
out_f = function(formula) {
  with(ml, glm(as.formula(paste("HLTH.Fatigue_lead2_z ~ bs(Church_lead1) +",
                                 paste(baselinevars,
                                       collapse = "+")))))
}
main = "Fatigue"
ylab = "Fatigue (SD)"
# clean oven
rm(out_m)
rm(pool_m)
rm(out_ct)

# bake
out_m <- out_f()

## contrasts
out_ct <- pool_stglm_contrast(out_m, df = df, m = m,  X = X, x = c, r= r)

# graph of contrasts
ggplot_stglm_contrast(out_ct, ylim, main, xlab, ylab)

# table
out_ct %>%
  kbl(format = "markdown", booktabs = T, digits = 3)

# evalues
round( EValue::evalues.OLS( , se = , sd = 1, delta = 4, true = 0), 3)
round( EValue::evalues.RR( , lo =  , hi =, true = 1), 4)

# g-computation
pool_m <- pool_stglm(out_m, df = df, m = m, x = x,  X = X)
# graph
ggplot_stglm(pool_m, ylim, main, xlab, ylab, c=c)

# alcohol freq ------------------------------------------------------------
# fit
out_f = function(formula) {
  with(ml, glm(as.formula(paste("Alcohol.Frequency_lead2ord_z~ bs(Church_lead1) +",
                                 paste(baselinevars, collapse = "+")))))
}
main = "Alcohol Frequency"
ylab = "Alcohol Frequency (SD)"
# clean oven
rm(out_m)
rm(pool_m)
rm(out_ct)

# bake
out_m <- out_f()

## contrasts
out_ct <- pool_stglm_contrast(out_m, df = df, m = m,  X = X, x = c, r= r)

# graph of contrasts
ggplot_stglm_contrast(out_ct, ylim, main, xlab, ylab)

# table
out_ct %>%
  kbl(format = "markdown", booktabs = T, digits = 3)

# evalues
round( EValue::evalues.OLS(-0.08 , se = 0.022, sd = 1, delta = 4, true = 0), 3)
round( EValue::evalues.RR( , lo =  , hi =, true = 1), 4)

# g-computation
pool_m <- pool_stglm(out_m, df = df, m = m, x = x,  X = X)
# graph
ggplot_stglm(pool_m, ylim, main, xlab, ylab, c=c)


# Alcohol.Intensity ----------------------------------------------------------
# fit
out_f = function(formula) {
  with(ml, glm(as.formula(paste("Alcohol.Intensity_log_lead2_z~ bs(Church_lead1) +",
                                 paste(baselinevars,
                                       collapse = "+")))))
}
main = "Alcohol Intensity"
ylab = "Alcohol Intensity (SD)"
# clean oven
rm(out_m)
rm(pool_m)
rm(out_ct)

# bake
out_m <- out_f()

## contrasts
out_ct <- pool_stglm_contrast(out_m, df = df, m = m,  X = X, x = c, r= r)

# graph of contrasts
ggplot_stglm_contrast(out_ct, ylim, main, xlab, ylab)

# table
out_ct %>%
  kbl(format = "markdown", booktabs = T, digits = 3)

# evalues
round( EValue::evalues.OLS(-0.065 , se = 0.024, sd = 1, delta = 4, true = 0), 3)
round( EValue::evalues.RR( , lo =  , hi =, true = 1), 4)

# g-computation
pool_m <- pool_stglm(out_m, df = df, m = m, x = x,  X = X)
# graph
ggplot_stglm(pool_m, ylim, main, xlab, ylab, c=c)


# body satisfaction -------------------------------------------------------
# fit
out_f = function(formula) {
  with(ml, glm(as.formula(paste("Bodysat_lead2_z~ bs(Church_lead1) +",
                                 paste(baselinevars,
                                       collapse = "+")))))
}
main = "Body Satisfaction"
ylab = "Body Satisfaction (SD)"

# clean oven
rm(out_m)
rm(pool_m)
rm(out_ct)

# bake
out_m <- out_f()

## contrasts
out_ct <- pool_stglm_contrast(out_m, df = df, m = m,  X = X, x = c, r= r)

# graph of contrasts
ggplot_stglm_contrast(out_ct, ylim, main, xlab, ylab)

# table
out_ct %>%
  kbl(format = "markdown", booktabs = T, digits = 3)

# evalues
round( EValue::evalues.OLS(0.03 , se = 0.029, sd = 1, delta = 4, true = 0), 3)
round( EValue::evalues.RR( , lo =  , hi =, true = 1), 4)

# g-computation
pool_m <- pool_stglm(out_m, df = df, m = m, x = x,  X = X)
# graph
ggplot_stglm(pool_m, ylim, main, xlab, ylab, c=c)


# rumination --------------------------------------------------------------
# During the last 30 days, how often did.... you have negative thoughts that repeated over and over?
# fit
out_f = function(formula) {
  with(ml, glm(as.formula(paste("Rumination_lead2ord_z~ bs(Church_lead1) +",
                                 paste(baselinevars,
                                       collapse = "+")))))
}

#labels
main = "Rumination"
ylab = "Rumination (SD)"

# clean oven
rm(out_m)
rm(pool_m)
rm(out_ct)

# bake
out_m <- out_f()

## contrasts
out_ct <- pool_stglm_contrast(out_m, df = df, m = m,  X = X, x = c, r= r)

# graph of contrasts
ggplot_stglm_contrast(out_ct, ylim, main, xlab, ylab)

# table
out_ct %>%
  kbl(format = "markdown", booktabs = T, digits = 3)

# evalues
round( EValue::evalues.OLS( , se = , sd = 1, delta = 4, true = 0), 3)
round( EValue::evalues.RR( , lo =  , hi =, true = 1), 4)

# g-computation
pool_m <- pool_stglm(out_m, df = df, m = m, x = x,  X = X)
# graph
ggplot_stglm(pool_m, ylim, main, xlab, ylab, c=c)


# sex satisfaction --------------------------------------------------------
out_f = function(formula) {
  with(ml, glm( as.formula(paste("SexualSatisfaction_lead2_z~ bs(Church_lead1) +", paste(baselinevars, collapse = "+")))))
}
main = "Sexual Satisfaction (SD)"
ylab = "Sexual Satisfaction (SD)"
# clean oven
rm(out_m)
rm(pool_m)
rm(out_ct)

# bake
out_m <- out_f()

## contrasts
out_ct <- pool_stglm_contrast(out_m, df = df, m = m,  X = X, x = c, r= r)

# graph of contrasts
ggplot_stglm_contrast(out_ct, ylim, main, xlab, ylab)

# table
out_ct %>%
  kbl(format = "markdown", booktabs = T, digits = 3)

# evalues
round( EValue::evalues.OLS( , se = , sd = 1, delta = 4, true = 0), 3)
round( EValue::evalues.RR( , lo =  , hi =, true = 1), 4)

# g-computation
pool_m <- pool_stglm(out_m, df = df, m = m, x = x,  X = X)
# graph
ggplot_stglm(pool_m, ylim, main, xlab, ylab, c=c)



# emotional regulation 1 ----------------------------------------------------
# When I feel negative emotions, my emotions feel out of control.

# kessler 6 ---------------------------------------------------------------
## fit
# Kessler-6
# During the last 30 days, how often did.... you feel hopeless?
#   During the last 30 days, how often did.... you feel so depressed that nothing could cheer you up?
#   During the last 30 days, how often did.... you feel restless or fidgety?
#   During the last 30 days, how often did.... you feel that everything was an effort?
#   During the last 30 days, how often did.... you feel worthless?
#   During the last 30 days, how often did.... you feel nervous?

out_f = function(formula) {
  with(ml, glm(as.formula(paste("KESSLER6sum_lead2~ bs(Church_lead1) +",
                                 paste(baselinevars,
                                       collapse = "+"))), family = "poisson"))
}
main = "Kessler 6 Distress"
ylab = "Kessler 6 Distress (SD)"
# clean oven
rm(out_m)
rm(pool_m)
rm(out_ct)

# bake
out_m <- out_f()

## contrasts (ratio scale)
out_ct <- pool_stglm_contrast_ratio(out_m, df = df, m = m,  X = X, x = c, r= r)

# graph of contrasts
ggplot_stglm_contrast(out_ct, ylim = c(.75,1.25), main, xlab, ylab)

# table
out_ct %>%
  kbl(format = "markdown", booktabs = T, digits = 3)

# evalues
round( EValue::evalues.OLS( , se = , sd = 1, delta = 4, true = 0), 3)
round( EValue::evalues.RR( , lo =  , hi =, true = 1), 4)

# g-computation
pool_m <- pool_stglm(out_m, df = df, m = m, x = x,  X = X)
# graph
ggplot_stglm(pool_m, ylim, main, xlab, ylab, c=c)

# power dependence 1 ------------------------------------------------------
# I do not have enough power or control over important parts of my life.
## fit
out_f = function(formula) {
  with(ml, glm(as.formula(paste("POWERDEPENDENCE1_lead2_z~ bs(Church_lead1) +",
                                 paste(baselinevars,
                                       collapse = "+")))))
}
main = "Power Dependence 1"
ylab = "Power Dependence 1(SD)"


# power dependence 2 ------------------------------------------------------
## fit
#Other people have too much power or control over important parts of my life.

out_f = function(formula) {
  with(ml, glm(as.formula(paste("POWERDEPENDENCE2_lead2_z~ bs(Church_lead1) +",
                                 paste(baselinevars,
                                       collapse = "+")))))
}


main = "Power Dependence 2"
ylab = "Power Dependence 2(SD)"

# clean oven
rm(out_m)
rm(pool_m)
rm(out_ct)

# bake
out_m <- out_f()

## contrasts
out_ct <- pool_stglm_contrast(out_m, df = df, m = m,  X = X, x = c, r= r)

# graph of contrasts
ggplot_stglm_contrast(out_ct, ylim = ylim, main, xlab, ylab)

# table
out_ct %>%
  kbl(format = "markdown", booktabs = T, digits = 3)

# evalues
round( EValue::evalues.OLS( , se = , sd = 1, delta = 4, true = 0), 3)
round( EValue::evalues.RR( , lo =  , hi =, true = 1), 4)

# g-computation
pool_m <- pool_stglm(out_m, df = df, m = m, x = x,  X = X)
# graph
ggplot_stglm(pool_m, ylim, main, xlab, ylab, c=c)

# perfectionism  ----------------------------------------------------------
# Perfectionism Discrepancy Subscale
# Doing my best never seems to be enough.
# My performance rarely measures up to my standards.
# I am hardly ever satisfied with my performance.

## fit
out_f = function(formula) {
  with(ml, glm( as.formula(paste("PERFECTIONISM_lead2_z~ bs(Church_lead1) +",
                                  paste(baselinevars,
                                        collapse = "+")))))
}

main = "Perfectionism"
ylab = "Perfectionism (SD)"
# clean oven
rm(out_m)
rm(pool_m)
rm(out_ct)

# bake
out_m <- out_f()

## contrasts
out_ct <- pool_stglm_contrast(out_m, df = df, m = m,  X = X, x = c, r= r)

# graph of contrasts
ggplot_stglm_contrast(out_ct, ylim = ylim, main, xlab, ylab)

# table
out_ct %>%
  kbl(format = "markdown", booktabs = T, digits = 3)

# evalues
round( EValue::evalues.OLS( , se = , sd = 1, delta = 4, true = 0), 3)
round( EValue::evalues.RR( , lo =  , hi =, true = 1), 4)

# g-computation
pool_m <- pool_stglm(out_m, df = df, m = m, x = x,  X = X)
# graph
ggplot_stglm(pool_m, ylim, main, xlab, ylab, c=c)

# self esteem -------------------------------------------------------------
# Self-esteem
# On the whole am satisfied with myself.
# Take a positive attitude toward myself.
# Am inclined to feel that I am a failure.

## fit
out_f = function(formula) {
  with(ml, glm( as.formula(paste("SELF.ESTEEM_lead2_z~ bs(Church_lead1) +",
                                  paste(baselinevars,
                                        collapse = "+")))))
}
main = "Self Esteem"
ylab = "Self Esteem (SD)"
# clean oven
rm(out_m)
rm(pool_m)
rm(out_ct)
# bake
out_m <- out_f()

## contrasts (ratio scale)
out_ct <- pool_stglm_contrast(out_m, df = df, m = m,  X = X, x = c, r= r)

# graph of contrasts
ggplot_stglm_contrast(out_ct, ylim = ylim, main, xlab, ylab)

# table
out_ct %>%
  kbl(format = "markdown", booktabs = T, digits = 3)

# evalues
round( EValue::evalues.OLS(.046 , se = 0.026, sd = 1, delta = 4, true = 0), 3)
round( EValue::evalues.RR( , lo =  , hi =, true = 1), 4)

# g-computation
pool_m <- pool_stglm(out_m, df = df, m = m, x = x,  X = X)
# graph
ggplot_stglm(pool_m, ylim, main, xlab, ylab, c=c)


# gratitude ---------------------------------------------------------------
# Gratitude
# I have much in my life to be thankful for.
# When I look at the world, I don’t see much to be grateful for.
# I am grateful to a wide variety of people.

## fit
out_f = function(formula) {
  with(ml, glm( as.formula(paste("GRATITUDE_lead2_z~ bs(Church_lead1) +",
                                  paste(baselinevars,
                                        collapse = "+")))
  ))
}

# compare!

# out_f = function(formula) {
#   with(ml, glm( as.formula(paste("GRATITUDE_lead2_z~ bs(Church_lead1)"))))
# }
main = "Gratitude"
ylab = "Gratitude (SD)"
# clean oven
rm(out_m)
rm(pool_m)
rm(out_ct)
# bake
out_m <- out_f()

## contrasts (ratio scale)
out_ct <- pool_stglm_contrast(out_m, df = df, m = m,  X = X, x = c, r= r)

# graph of contrasts
ggplot_stglm_contrast(out_ct, ylim = c(0,1), main, xlab, ylab)

# table
out_ct %>%
  kbl(format = "markdown", booktabs = T, digits = 3)

# evalues
round( EValue::evalues.OLS( , se = , sd = 1, delta = 4, true = 0), 3)
round( EValue::evalues.RR( , lo =  , hi =, true = 1), 4)

# g-computation
pool_m <- pool_stglm(out_m, df = df, m = m, x = x,  X = X)
# graph
ggplot_stglm(pool_m, ylim, main, xlab, ylab, c=c)

# veng rumination ---------------------------------------------------------
# Forgivingness versus Vengeful Rumination
# Sometimes I can't sleep because of thinking about past wrongs I have suffered.
# I can usually forgive and forget when someone does me wrong.
# I find myself regularly thinking about past times that I have been wronged.

## fit
out_f = function(formula) {
  with(ml, glm( as.formula(paste("VENGEFUL.RUMIN_lead2_z~ bs(Church_lead1) +",
                                  paste(baselinevars,
                                        collapse = "+")))
  ))
}
#
main = "Vengefulness (anti-Foregiveness)"
ylab = "Vengefulness (anti-Foregiveness) (SD)"
# clean oven
rm(out_m)
rm(pool_m)
rm(out_ct)
# bake
out_m <- out_f()

## contrasts (ratio scale)
out_ct <- pool_stglm_contrast(out_m, df = df, m = m,  X = X, x = c, r= r)

# graph of contrasts
ggplot_stglm_contrast(out_ct, ylim = ylim, main, xlab, ylab)

# table
out_ct %>%
  kbl(format = "markdown", booktabs = T, digits = 3)

# evalues
round( EValue::evalues.OLS( , se = , sd = 1, delta = 4, true = 0), 3)
round( EValue::evalues.RR( , lo =  , hi =, true = 1), 4)

# g-computation
pool_m <- pool_stglm(out_m, df = df, m = m, x = x,  X = X)
# graph
ggplot_stglm(pool_m, ylim, main, xlab, ylab, c=c)



# life meaning ------------------------------------------------------------
# Meaning in Life
# My life has a clear sense of purpose.
# I have a good sense of what makes my life meaningful.

## fit
out_f = function(formula) {
  with(ml, glm(
    as.formula(paste("LIFEMEANING_lead2ord_z~ bs(Church_lead1) +",
                     paste(baselinevars,
                           collapse = "+")))
  ))
}
main = "Life Meaning"
ylab = "Life Meaning (SD)"

# clean oven
rm(out_m)
rm(pool_m)
rm(out_ct)
# bake
out_m <- out_f()

## contrasts ( )
out_ct <- pool_stglm_contrast(out_m, df = df, m = m,  X = X, x = c, r= r)

# graph of contrasts
ggplot_stglm_contrast(out_ct, ylim = ylim, main, xlab, ylab)

# table
out_ct %>%
  kbl(format = "markdown", booktabs = T, digits = 3)

# evalues
round( EValue::evalues.OLS( 0.055, se =0.027 , sd = 1, delta = 4, true = 0), 3)
round( EValue::evalues.RR( , lo =  , hi =, true = 1), 4)

# g-computation
pool_m <- pool_stglm(out_m, df = df, m = m, x = x,  X = X)
# graph
ggplot_stglm(pool_m, ylim, main, xlab, ylab, c=c)

# honesty humility --------------------------------------------------------

# Mini-IPIP6 Honesty-Humility (item overlap with Psychological Entitlement)
# Would like to be seen driving around in a very expensive car.
# Would get a lot of pleasure from owning expensive luxury goods.
# Feel entitled to more of everything.
# Deserve more things in life.

## fit
out_f = function(formula) {
  with(ml, glm(
    as.formula(paste("HONESTY_HUMILITY_lead2_z~ bs(Church_lead1) +",
                     paste(baselinevars,
                           collapse = "+")))
  ))
}
#
main = "Honesty Humility"
ylab = "Honesty Humility (SD)"

# clean oven
rm(out_m)
rm(pool_m)
rm(out_ct)
# bake
out_m <- out_f()

## contrasts (ratio scale)
out_ct <- pool_stglm_contrast(out_m, df = df, m = m,  X = X, x = c, r= r)

# graph of contrasts
ggplot_stglm_contrast(out_ct, ylim = ylim, main, xlab, ylab)

  # table
out_ct %>%
  kbl(format = "markdown", booktabs = T, digits = 3)

# evalues
round( EValue::evalues.OLS( , se = , sd = 1, delta = 4, true = 0), 3)
round( EValue::evalues.RR( , lo =  , hi =, true = 1), 4)

# g-computation
pool_m <- pool_stglm(out_m, df = df, m = m, x = x,  X = X)
# graph
ggplot_stglm(pool_m, ylim, main, xlab, ylab, c=c)
# belonging ---------------------------------------------------------------
# Felt belongingness
# Know that people in my life accept and value me.
# Feel like an outsider.
# Know that people around me share my attitudes and beliefs.

out_f = function(formula) {
  with(ml, glm(
    as.formula(paste("BELONG_lead2_z~ bs(Church_lead1) +",
                     paste(baselinevars,
                           collapse = "+")))
  ))
}

main = "Social Belonging"
ylab = "Social Belonging (SD)"
# clean oven
rm(out_m)
rm(pool_m)
rm(out_ct)
# bake
out_m <- out_f()

## contrasts (ratio scale)
out_ct <- pool_stglm_contrast(out_m, df = df, m = m,  X = X, x = c, r= r)

# graph of contrasts
ggplot_stglm_contrast(out_ct, ylim = ylim, main, xlab, ylab)

# table
out_ct %>%
  kbl(format = "markdown", booktabs = T, digits = 3)

# evalues
round( EValue::evalues.OLS( 0.082, se = 0.03, sd = 1, delta = 4, true = 0), 3)
round( EValue::evalues.RR( , lo =  , hi =, true = 1), 4)

# g-computation
pool_m <- pool_stglm(out_m, df = df, m = m, x = x,  X = X)
# graph
ggplot_stglm(pool_m, ylim, main, xlab, ylab, c=c)

# soc support -------------------------------------------------------------
# Perceived social support
# There are people I can depend on to help me if I really need it.
# There is no one I can turn to for guidance in times of stress.
# I know there are people I can turn to when I need help.
## fit
out_f = function(formula) {
  with(ml, glm(
    as.formula(paste("SUPPORT_lead2_z~ bs(Church_lead1) +",
                     paste(baselinevars,
                           collapse = "+")))
  ))
}
main = "Social Support"
ylab = "Social Support (SD)"
# clean oven
rm(out_m)
rm(pool_m)
rm(out_ct)
# bake
out_m <- out_f()

## contrasts
out_ct <- pool_stglm_contrast(out_m, df = df, m = m,  X = X, x = c, r= r)

# graph of contrasts
ggplot_stglm_contrast(out_ct, ylim = ylim, main, xlab, ylab)

# table
out_ct %>%
  kbl(format = "markdown", booktabs = T, digits = 3)

# evalues
round( EValue::evalues.OLS( , se = , sd = 1, delta = 4, true = 0), 3)
round( EValue::evalues.RR( , lo =  , hi =, true = 1), 4)

# g-computation
pool_m <- pool_stglm(out_m, df = df, m = m, x = x,  X = X)
# graph
ggplot_stglm(pool_m, ylim, main, xlab, ylab, c=c)
# volunteers --------------------------------------------------------------
# fit
out_f = function(formula) {
  with(ml, glm(as.formula(paste("Volunteers_lead2~ bs(Church_lead1) +",
                                 paste(baselinevars,
                                       collapse = "+"))), family = "poisson"))
}



# compare to without confounding control
# out_f = function(formula) {
#   with(ml, glm(as.formula(paste("Volunteers_lead2~ bs(Church_lead1)"
#                              )), family = "poisson"))
# }


main = "Volunteers"
ylab = "Volunteers (rate)"
# clean oven
rm(out_m)
rm(pool_m)
rm(out_ct)
# bake
out_m <- out_f()

## contrasts (ratio scale)
out_ct <- pool_stglm_contrast_ratio(out_m, df = df, m = m,  X = X, x = c, r= r)

# graph of contrasts
ggplot_stglm_contrast(out_ct, ylim = c(1,3), main, xlab, ylab)

# table
out_ct %>%
  kbl(format = "markdown", booktabs = T, digits = 3)

# evalues
round( EValue::evalues.OLS( , se = , sd = 1, delta = 4, true = 0), 3)
round( EValue::evalues.RR( 1.479, lo =  0.219, hi =, true = 1), 4)

# g-computation
pool_m <- pool_stglm(out_m, df = df, m = m, x = c(0:8),  X = X)
# graph
ggplot_stglm(pool_m, ylim = c(-.5, 1), main, xlab, ylab, c=(0:8))

# charity donate ----------------------------------------------------------
## fit
out_f = function(formula) {
  with(ml, glm(
    as.formula(paste("CharityDonate_log_lead2_z ~ bs(Church_lead1) +",
                     paste(baselinevars,
                           collapse = "+")))
    ))
}
main = "Charity Donations (annual)"
ylab = "Charity Donations (annual)"
# clean oven
rm(out_m)
rm(pool_m)
rm(out_ct)
# bake
out_m <- out_f()
summary(pool(out_m))

## contrasts (ratio scale)
out_ct <- pool_stglm_contrast(out_m, df = df, m = m,  X = X, x = c, r= r)
out_ct
# graph of contrasts
ggplot_stglm_contrast(out_ct, ylim =c(0,.5), main, xlab, ylab)

# sd(mf$CharityDonate_lead2) * 0.2538613  # 100 dollars per month

# table
out_ct %>%
  kbl(format = "markdown", booktabs = T, digits = 3)

# evalues
round( EValue::evalues.OLS( , se = , sd = 1, delta = 4, true = 0), 3)
round( EValue::evalues.RR( , lo =  , hi =, true = 1), 4)

# g-computation
pool_m <- pool_stglm(out_m, df = df, m = m, x = x,  X = X)
# graph

# community lead ----------------------------------------------------------
## fit
out_f = function(formula) {
  with(ml, glm(
    as.formula(paste("community_lead2_z~ bs(Church_lead1) +",
                     paste(baselinevars,
                           collapse = "+")))
  ))
}

main = "Community"
ylab = "Community (SD)"

# clean oven
rm(out_m)
rm(pool_m)
rm(out_ct)
# bake
out_m <- out_f()

## contrasts
out_ct <- pool_stglm_contrast(out_m, df = df, m = m,  X = X, x = c, r= r)

# graph of contrasts
ggplot_stglm_contrast(out_ct, ylim = ylim, main, xlab, ylab)

# table
out_ct %>%
  kbl(format = "markdown", booktabs = T, digits = 3)

# evalues
round( EValue::evalues.OLS( , se = , sd = 1, delta = 4, true = 0), 3)
round( EValue::evalues.RR( , lo =  , hi =, true = 1), 4)

# g-computation
pool_m <- pool_stglm(out_m, df = df, m = m, x = x,  X = X)
# graph
ggplot_stglm(pool_m, ylim, main, xlab, ylab, c=c)

# national wellbeing ------------------------------------------------------

# National Wellbeing Index
# The economic situation in New Zealand.
# The social conditions in New Zealand.
# Business in New Zealand.

## fit
out_f = function(formula) {
  with(ml, glm(
    as.formula(paste("NWI_lead2_z~ bs(Church_lead1) +",
                     paste(baselinevars,
                           collapse = "+")))
  ))
}

main = "National Well Being"
ylab = "National Well Being (SD)"

# clean oven
rm(out_m)
rm(pool_m)
rm(out_ct)
# bake
out_m <- out_f()

## contrasts
out_ct <- pool_stglm_contrast(out_m, df = df, m = m,  X = X, x = c, r= r)

# graph of contrasts
ggplot_stglm_contrast(out_ct, ylim = ylim, main, xlab, ylab)

# table
out_ct %>%
  kbl(format = "markdown", booktabs = T, digits = 3)

# evalues
round( EValue::evalues.OLS( , se = , sd = 1, delta = 4, true = 0), 3)
round( EValue::evalues.RR( , lo =  , hi =, true = 1), 4)

# g-computation
pool_m <- pool_stglm(out_m, df = df, m = m, x = x,  X = X)
# graph
ggplot_stglm(pool_m, ylim, main, xlab, ylab, c=c)

# imperm group ------------------------------------------------------------

## fit
out_f = function(formula) {
  with(ml, glm(
    as.formula(paste("ImpermeabilityGroup_lead2_z~ bs(Church_lead1) +",
                     paste(baselinevars,
                           collapse = "+")))
  ))
}

main = "Impermeability Group"
ylab = "Impermeability Group (SD)"

# clean oven
rm(out_m)
rm(pool_m)
rm(out_ct)
# bake
out_m <- out_f()

## contrasts
out_ct <- pool_stglm_contrast(out_m, df = df, m = m,  X = X, x = c, r= r)

# graph of contrasts
ggplot_stglm_contrast(out_ct, ylim = ylim, main, xlab, ylab)

# table
out_ct %>%
  kbl(format = "markdown", booktabs = T, digits = 3)

# evalues
round( EValue::evalues.OLS( , se = , sd = 1, delta = 4, true = 0), 3)
round( EValue::evalues.RR( , lo =  , hi =, true = 1), 4)

# g-computation
pool_m <- pool_stglm(out_m, df = df, m = m, x = x,  X = X)
# graph
ggplot_stglm(pool_m, ylim, main, xlab, ylab, c=c)


# stand living ------------------------------------------------------------
# Part of pwi
# Personal Wellbeing Index
# Your health.
# Your standard of living.
# Your future security.
# Your personal relationships.

## fit
out_f = function(formula) {
  with(ml, glm(
    as.formula(paste("Standard.Living_lead2ord_z~ bs(Church_lead1) +",
                     paste(baselinevars,
                           collapse = "+")))
  ))
}

main = "Standard Living"
ylab = "Standard Living (SD)"

rm(out_m)
rm(pool_m)
rm(out_ct)
# bake
out_m <- out_f()

## contrasts
out_ct <- pool_stglm_contrast(out_m, df = df, m = m,  X = X, x = c, r= r)

# graph of contrasts
ggplot_stglm_contrast(out_ct, ylim = ylim, main, xlab, ylab)

# table
out_ct %>%
  kbl(format = "markdown", booktabs = T, digits = 3)

# evalues
round( EValue::evalues.OLS( , se = , sd = 1, delta = 4, true = 0), 3)
round( EValue::evalues.RR( , lo =  , hi =, true = 1), 4)

# g-computation
pool_m <- pool_stglm(out_m, df = df, m = m, x = x,  X = X)
# graph
ggplot_stglm(pool_m, ylim, main, xlab, ylab, c=c)

# future security ---------------------------------------------------------

out_f = function(formula) {
  with(ml, glm(
    as.formula(paste("Your.Future.Security_lead2_z~ bs(Church_lead1) +",
                     paste(baselinevars,
                           collapse = "+")))
  ))
}

main = "Future Security"
ylab = "Future Security (SD)"

# clean oven
rm(out_m)
rm(pool_m)
rm(out_ct)
# bake
out_m <- out_f()

## contrasts
out_ct <- pool_stglm_contrast(out_m, df = df, m = m,  X = X, x = c, r= r)

# graph of contrasts
ggplot_stglm_contrast(out_ct, ylim = ylim, main, xlab, ylab)

# table
out_ct %>%
  kbl(format = "markdown", booktabs = T, digits = 3)

# evalues
round( EValue::evalues.OLS( , se = , sd = 1, delta = 4, true = 0), 3)
round( EValue::evalues.RR( , lo =  , hi =, true = 1), 4)

# g-computation
pool_m <- pool_stglm(out_m, df = df, m = m, x = x,  X = X)
# graph
ggplot_stglm(pool_m, ylim, main, xlab, ylab, c=c)


# your health -------------------------------------------------------------
out_f= function(formula) {
  with(ml, glm(
    as.formula(paste("Your.Health_lead2_z~ bs(Church_lead1) +",
                     paste(baselinevars,
                           collapse = "+")))
  ))
}
main = "Your Health"
ylab = "Your Health (SD)"

# clean oven
rm(out_m)
rm(pool_m)
rm(out_ct)
# bake
out_m <- out_f()

## contrasts
out_ct <- pool_stglm_contrast(out_m, df = df, m = m,  X = X, x = c, r= r)

# graph of contrasts
ggplot_stglm_contrast(out_ct, ylim = ylim, main, xlab, ylab)

# table
out_ct %>%
  kbl(format = "markdown", booktabs = T, digits = 3)

# evalues
round( EValue::evalues.OLS( , se = , sd = 1, delta = 4, true = 0), 3)
round( EValue::evalues.RR( , lo =  , hi =, true = 1), 4)

# g-computation
pool_m <- pool_stglm(out_m, df = df, m = m, x = x,  X = X)
# graph
ggplot_stglm(pool_m, ylim, main, xlab, ylab, c=c)


# personal relationships --------------------------------------------------
## fit
out_f= function(formula) {
  with(ml, glm(
    as.formula(paste("Your.Personal.Relationships_lead2ord_z~ bs(Church_lead1) +",  paste(baselinevars,collapse = "+")))
  ))
}

main = "Your Personal Relationships"
ylab = "Your Personal Relationships (SD)"

# clean oven
rm(out_m)
rm(pool_m)
rm(out_ct)
# bake
out_m <- out_f()

## contrasts
out_ct <- pool_stglm_contrast(out_m, df = df, m = m,  X = X, x = c, r= r)

# graph of contrasts
ggplot_stglm_contrast(out_ct, ylim = ylim, main, xlab, ylab)

# table
out_ct %>%
  kbl(format = "markdown", booktabs = T, digits = 3)

# evalues
round( EValue::evalues.OLS( , se = , sd = 1, delta = 4, true = 0), 3)
round( EValue::evalues.RR( , lo =  , hi =, true = 1), 4)

# g-computation
pool_m <- pool_stglm(out_m, df = df, m = m, x = x,  X = X)
# graph
ggplot_stglm(pool_m, ylim, main, xlab, ylab, c=c)

# Work-life balance -------------------------------------------------------
out_f = function(formula) {
  with(ml, glm(
    as.formula(paste("Emp.WorkLifeBalance_lead2_z~ bs(Church_lead1) +",
                     paste(baselinevars,
                           collapse = "+")))
  ))
}

main = "Work Life Balance"
ylab = "Work Life Balance (SD)"

# clean oven
rm(out_m)
rm(pool_m)
rm(out_ct)
# bake
out_m <- out_f()

## contrasts
out_ct <- pool_stglm_contrast(out_m, df = df, m = m,  X = X, x = c, r= r)

# graph of contrasts
ggplot_stglm_contrast(out_ct, ylim = ylim, main, xlab, ylab)

# table
out_ct %>%
  kbl(format = "markdown", booktabs = T, digits = 3)

# evalues
round( EValue::evalues.OLS( , se = , sd = 1, delta = 4, true = 0), 3)
round( EValue::evalues.RR( , lo =  , hi =, true = 1), 4)

# g-computation
pool_m <- pool_stglm(out_m, df = df, m = m, x = x,  X = X)
# graph
ggplot_stglm(pool_m, ylim, main, xlab, ylab, c=c)



# life sat ----------------------------------------------------------------
# Satisfaction with life
# I am satisfied with my life.
# In most ways my life is close to ideal.
## fit
out_f = function(formula) {
  with(ml, glm(
    as.formula(paste("LIFESAT_lead2_z~ bs(Church_lead1) +",
                     paste(baselinevars,
                           collapse = "+")))
  ))
}
# pooled model
main = "Life Satisfaction"
ylab = "Life Satisfaction (SD)"
# clean oven
rm(out_m)
rm(pool_m)
rm(out_ct)
# bake
out_m <- out_f()

## contrasts
out_ct <- pool_stglm_contrast(out_m, df = df, m = m,  X = X, x = c, r= r)

# graph of contrasts
ggplot_stglm_contrast(out_ct, ylim = ylim, main, xlab, ylab)

# table
out_ct %>%
  kbl(format = "markdown", booktabs = T, digits = 3)

# evalues
round( EValue::evalues.OLS( , se = , sd = 1, delta = 4, true = 0), 3)
round( EValue::evalues.RR( , lo =  , hi =, true = 1), 4)

# g-computation
pool_m <- pool_stglm(out_m, df = df, m = m, x = x,  X = X)
# graph
ggplot_stglm(pool_m, ylim, main, xlab, ylab, c=c)

# Promotion NZSEI ---------------------------------------------------------------
## fit
out_f = function(formula) {
  with(ml, glm(
    as.formula(paste("NZSEI13_lead2_10_z ~ bs(Church_lead1) +",
                     paste(baselinevars,
                           collapse = "+")))
  ))
}
main = "Occupational Status/10"
ylab = "Occupational Status/10"

# clean oven
rm(out_m)
rm(pool_m)
rm(out_ct)
# bake
out_m <- out_f()

## contrasts
out_ct <- pool_stglm_contrast(out_m, df = df, m = m,  X = X, x = c, r= r)

# graph of contrasts
ggplot_stglm_contrast(out_ct, ylim = ylim, main, xlab, ylab)

# table
out_ct %>%
  kbl(format = "markdown", booktabs = T, digits = 3)

# evalues
round( EValue::evalues.OLS( , se = , sd = 1, delta = 4, true = 0), 3)
round( EValue::evalues.RR( , lo =  , hi =, true = 1), 4)

# g-computation
pool_m <- pool_stglm(out_m, df = df, m = m, x = x,  X = X)
# graph
ggplot_stglm(pool_m, ylim, main, xlab, ylab, c=c)





# hours-work --------------------------------------------------------------

library(MatchThem)
library(optmatch)
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
                          NZSEI13_z +
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

out <- with(ctrim_st, glm( SUPPORT_lead2_z ~ Hours.Work_lead1_10 ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)


round( EValue::evalues.OLS( , se = , sd = 1, delta = 2, true = 0), 3)
round( EValue::evalues.RR(, lo =  , hi =, true = 1), 3)


out <- with(ctrim_st, glm( Volunteers_lead2 ~ Hours.Work_lead1_10 ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

round( EValue::evalues.OLS( , se = , sd = 1, delta = 2, true = 0), 3)
round( EValue::evalues.RR(, lo =  , hi =, true = 1), 3)

out <- with(ctrim_st, glm( CharityDonate_lead2 ~ Hours.Work_lead1_10 , family = "poisson"))
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


out <- with(ctrim_st, glm(community_lead2_z ~ Hours.Work_lead1_10 ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

round( EValue::evalues.OLS( , se = , sd = 1, delta = 2, true = 0), 3)
round( EValue::evalues.RR(, lo =  , hi =, true = 1), 3)

out <- with(ctrim_st, glm(NWI_lead2_z~ Hours.Work_lead1_10 ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

round( EValue::evalues.OLS(0.025172099 , se = 0.007768097, sd = 1, delta = 2, true = 0), 3)
round( EValue::evalues.RR(, lo =  , hi =, true = 1), 3)

out <- with(ctrim_st, glm(ImpermeabilityGroup_lead2 ~ Hours.Work_lead1_10 ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

round( EValue::evalues.OLS( , se = , sd = 1, delta = 2, true = 0), 3)
round( EValue::evalues.RR(, lo =  , hi =, true = 1), 3)


out <- with(ctrim_st, glm( Standard.Living_lead2ord_z ~ Hours.Work_lead1_10 ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

round( EValue::evalues.OLS( , se = , sd = 1, delta = 2, true = 0), 3)
round( EValue::evalues.RR(, lo =  , hi =, true = 1), 3)


out <- with(ctrim_st, glm( Your.Future.Security_lead2_z ~ Hours.Work_lead1_10 ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

round( EValue::evalues.OLS( , se = , sd = 1, delta = 2, true = 0), 3)
round( EValue::evalues.RR(, lo =  , hi =, true = 1), 3)

out <- with(ctrim_st, glm( Your.Health_lead2_z  ~ Hours.Work_lead1_10 ))

output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

round( EValue::evalues.OLS( , se = , sd = 1, delta = 2, true = 0), 3)
round( EValue::evalues.RR(, lo =  , hi =, true = 1), 3)

out <- with(ctrim_st, glm( Your.Personal.Relationships_lead2ord_z ~ Hours.Work_lead1_10 ))
output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)

round( EValue::evalues.OLS( , se = , sd = 1, delta = 2, true = 0), 3)
round( EValue::evalues.RR(, lo =  , hi =, true = 1), 3)


# NZSEI13 -- status variable



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
                          NZSEI13_z +
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
      NZSEI13_z +
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
                          NZSEI13_z +
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


EmotionRegulation1_st <- as.formula(paste("EmotionRegulation1_lead2_z~ bs(Church_lead1) +", paste(baselinevars, collapse = "+")))

## fit
fit_EmotionRegulation1 = function(formula) {
  with(ml, glm(as.formula(paste("EmotionRegulation1_lead2_z~ bs(Church_lead1) +", paste(baselinevars, collapse = "+")))))
}
# ppoled model
m12_fit_EmotionRegulation1 <- fit_EmotionRegulation1()

main = "Emotion Regulation1"
ylab = "Emotion Regulation1 (SD)"
out_p<- pool_stglm(m12_fit_EmotionRegulation1, df = df, m = m,  X = X, x = x)

plot_stglm(out_p, ylim = ylim, main, xlab, ylab)
ggplot_stglm(out_p, ylim = ylim, main, xlab, ylab)

out_cr <- pool_stglm_contrast(m12_fit_EmotionRegulation1, df = df, m = m,  X = X, x = c, r= r)
plot_stglm_contrast(out_cr, ylim, main, xlab, ylab)
ggplot_stglm_contrast(out_cr, ylim, main, xlab, ylab)


# increase
round( EValue::evalues.OLS(  , se =    , sd = 1, delta = 3, true = 0), 3)
# decrease
round( EValue::evalues.OLS(   , se =   , sd = 1, delta = 3, true = 0), 3)



# emotional reg 2 ---------------------------------------------------------
# When I feel negative emotions, I suppress or hide my emotions.

EmotionRegulation2_st <- as.formula(paste("EmotionRegulation2_lead2_z~ bs(Church_lead1) +",paste(baselinevars,collapse = "+")))


## fit
fit_EmotionRegulation2 = function(formula) {
  with(ml, glm(as.formula(paste("EmotionRegulation2_lead2_z~ bs(Church_lead1) +", paste(baselinevars, collapse = "+")))))
}
# ppoled model
m13_fit_EmotionRegulation2 <- fit_EmotionRegulation2()

main = "Emotion Regulation2"
ylab = "Emotion Regulation2 (SD)"
out_p <- pool_stglm(m13_fit_EmotionRegulation2, df = df, m = m,  X = X, x = x)

plot_stglm(out_p, ylim = ylim, main, xlab, ylab)
ggplot_stglm(out_p, ylim = ylim, main, xlab, ylab)

out_cr <- pool_stglm_contrast(m13_fit_EmotionRegulation2, df = df, m = m,  X = X, x = c, r= r)
plot_stglm_contrast(out_cr, ylim, main, xlab, ylab)

# increase
round( EValue::evalues.OLS(  , se =    , sd = 1, delta = 3, true = 0), 3)
# decrease
round( EValue::evalues.OLS(   , se =   , sd = 1, delta = 3, true = 0), 3)


# emotional reg 3 ---------------------------------------------------------
# When I feel negative emotions, I change the way I think to help me stay calm.

## fit
fit_EmotionRegulation3 = function(formula) {
  with(ml, glm(as.formula(paste("EmotionRegulation3_lead2_z~ bs(Church_lead1) +", paste(baselinevars, collapse = "+")))))
}
# ppoled model
m14_fit_EmotionRegulation3 <- fit_EmotionRegulation3()

main = "Emotion Regulation3"
ylab = "Emotion Regulation3 (SD)"
out_p <- pool_stglm(m14_fit_EmotionRegulation3, df = df, m = m,  X = X, x = x)

plot_stglm(out_p, ylim = ylim, main, xlab, ylab)
ggplot_stglm(out_p, ylim = ylim, main, xlab, ylab)

out_cr <- pool_stglm_contrast(m14_fit_EmotionRegulation3, df = df, m = m,  X = X, x = c, r= r)
ggplot_stglm_contrast(out_cr, ylim, main, xlab, ylab)

# increase
round( EValue::evalues.OLS(  , se =    , sd = 1, delta = 3, true = 0), 3)
# decrease
round( EValue::evalues.OLS(   , se =   , sd = 1, delta = 3, true = 0), 3)

