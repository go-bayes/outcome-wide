# Bella hours work
# set science digits
options(scipen=999)

# https://www.careers.govt.nz/plan-your-career/get-ideas-for-your-career/types-of-work-arrangements-and-hours/#cID_7472

### ELIGIBILITY CRITERIA
# 2018/ 2019 - Hours Working Reported
# Not retired baseline?
# Not semi retired baseline?
# Employed at baseline and Employed in 2019?
# income above poverty

# read data
df <- readRDS(here::here("data_raw", "df.Rds"))


# df %>%
#   filter(Wave == 2020 &  YearMeasured == 1) %>%
#   n_distinct("Id")

# read libraries in
source(here::here("scripts", "libs.R"))
source(here::here("scripts", "funs.R"))



# table for participant N
tab_in <- df %>%
  dplyr::mutate(Euro = if_else(EthCat == 1, 1, 0)) %>%
  dplyr::mutate(Male = ifelse(GendAll == 1, 1, 0)) %>%
  dplyr::filter((Wave == 2018  & YearMeasured  == 1) |
                  (Wave == 2019  &
                     YearMeasured  == 1) |
                  (Wave == 2020))  %>% # Eligibility criteria
  dplyr::filter(YearMeasured  != -1) %>% # remove people who passed away
  # dplyr::filter(Id != 9630) %>% # problematic for income
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
# check n # 34782
table1::table1(~ Household.INC | Wave , data = tab_in, overall = FALSE)

length(unique(tab_in$Id)) # 34783

# increasing rate
dff%>%
  group_by(Wave) %>%
  summarise(mean(HLTH.Disability, na.rm = TRUE))

# Do you have a health condition or disability that limits you, and that has lasted for 6+ months?



## select vars
df_wk_t <- tab_in %>%
  dplyr::filter((Wave == 2018  & YearMeasured  == 1) |
                  (Wave == 2019  &
                     YearMeasured  == 1) |
                  (Wave == 2020))  %>% # Eligibility criteria
  dplyr::filter(Id != 9630) %>% # problematic
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
    NZdep,
    Employed,
    HomeOwner,
    Pol.Orient,
    Urban,
    Household.INC,
    Parent,
    Relid,
    Religion.Church,
    Believe.Spirit,
    Believe.God,
    Spiritual.Identification,
    SWB.SoC01,
    EmotionRegulation1,
    EmotionRegulation2,
    EmotionRegulation3,
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
    # PWI,  ##  we use the individual
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
    Church = ifelse(Religion.Church > 8, 8, Religion.Church),
    income_log = log(Household.INC + 1),
  ) %>%
  arrange(Id, Wave)  %>% #
  dplyr::mutate(income_log_lead1 = lead(income_log, n = 1)) %>%
  dplyr::mutate(Hours.Work_lead1 = lead(Hours.Work, n = 1)) %>%
  dplyr::mutate(HLTH.Disability_lead1 = lead(Hours.Work, n = 1)) %>%
  # dplyr::mutate(Standard.Living_lead1 = lead(Standard.Living, n = 1)) %>%
  dplyr::mutate(retired_lead1 = lead(retired, n = 1)) %>%
  dplyr::mutate(semiretired_lead1 = lead(semiretired, n = 1)) %>%
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
      #  PWI,
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
      # ChildrenNum,
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
      EmotionRegulation1,
      EmotionRegulation2,
      EmotionRegulation3,
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
  dplyr::filter(Household.INC >= 30975) %>% # min income
  # dplyr::filter(income_log_lead1 > income_log) %>%
  dplyr::filter(!is.na(Hours.Work)) %>%
  dplyr::filter(!is.na(Hours.Work_lead1)) %>%
  dplyr::filter(Hours.Work > 35 & Hours.Work < 45 ) %>%
  #dplyr::filter(!is.na(Standard.Living) )%>%
  # dplyr::filter(!is.na(Standard.Living_lead1) )%>%
  #  dplyr::filter(semiretired_lead1 != 1) %>%  #needed for the intervention
  dplyr::select(-c(
    Religion.Church,
    # EthCat,
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
    HLTH.Disability_lead1,
    # org2019,
    # hold19,
    # retired,
    retired_lead1,
    # semiretired,
    semiretired_lead1
  )
  ) %>%
  #  dplyr::mutate(across(!c(Id,Wave), ~ scale(.x)))%>%  # standarise vars for easy computing-- do this after imputation
  arrange(Id, Wave) %>%
  data.frame() %>%
  mutate(across(where(is.double), as.numeric)) %>%
  arrange(Id)

# Filtering retirement -- consistency and positivity assumptions

# number of ids
N <- length(unique(df_wk_t$Id)) # 28676
N

# inspect data
skim(df_wk_t)

# save function
saveh(df_wk_t, "df_wk_t")

# read if needed
df_wk_t<- readh("df_wk_t")

hist(df_wk_t$Hours.Work_lead2)

table(df_wk_t$Hours.Work_lead2 > 40)
table(df_wk_t$Hours.Work_lead2 == 40)
table(df_wk_t$Hours.Work_lead2 < 40)



# mice model  -------------------------------------------------------------
library(mice)

mice_wkt <- df_wk_t %>%
  dplyr::select(-c( Wave, Id))  # won't otherwise run

library(naniar)
naniar::gg_miss_var(mice_wkt)
vis_miss(mice_wkt,
         warn_large_data = FALSE)

# any colinear vars?
mice:::find.collinear(mice_wkt)

# ini <- mice(mice_upinc, m = 1, maxit = 0)
# meth<- ini$method
# meth
# predmat["Id",]= 0
# predmat
# #meth
# #meth["inc_prop"] <- "~ I(income_log/(income_log_lead1 - 1))"
# meth["Id"] <- 0


# impute
wk_micet <- mice::mice(mice_wkt,  seed = 0, m = 10)

# save
saveh(wk_micet, "wk_micet")

# read
wk_micet <- readh("wk_micet")

# checks
outlist2 <-
  row.names(wk_micet)[wk_micet$outflux < 0.5]
length(outlist2)

# checks
head(wk_micet$loggedEvents, 10)

# data warangling
# we create two completed data sets -- the one without the missing data will be useful for
# determing causal contrasts -- which we'll describe below.

w_ft <- mice::complete(wk_micet, "long", inc = F)
w_lt <- mice::complete(wk_micet, "long", inc = TRUE)


# inspect data -- what do we care about?  Note that the moderate distress category doesn't look useful
skimr::skim(w_lt)

# create variables in z score
w_l2t <- w_lt %>%
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
  dplyr::mutate(Hours.Work_lead1_10 =  as.integer(Hours.Work_lead1/10))%>%
  dplyr::mutate(Hours.Work_lead1_sqrt =  as.integer(sqrt(Hours.Work_lead1)))%>%
  dplyr::mutate(NZSEI13_10 =  NZSEI13/10)%>%
  dplyr::mutate(NZSEI13_lead2_10 =  as.integer(NZSEI13_lead2/10))%>%
  # dplyr::mutate(Hours.Work_lead1_10 = Hours.Work_lead1/10)%>%
  # dplyr::mutate(Hours.Work_lead1ord = (as.numeric(
  #   cut(
  #     Hours.Work_lead1,
  #     breaks = c(-Inf,  20, 40, Inf),
  #     labels = c("0", "20", "40",  "over40"),
  #     right = TRUE
  #   )
  # ) - 1)) %>%
  dplyr::mutate(across(where(is.numeric), ~ scale(.x), .names = "{col}_z")) %>%
  select(-c(.imp_z, .id_z))%>%
  dplyr::mutate(id = as.factor(rep(1:N, 11)))# needed for g-comp# Respect for Self is fully missing

# for models wihout looping (not advised)

w_f2t <- w_ft %>%
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
  dplyr::mutate(Hours.Work_lead1_10 =  as.integer(Hours.Work_lead1/10))%>%
  dplyr::mutate(Hours.Work_lead1_sqrt =  as.integer(sqrt(Hours.Work_lead1)))%>%
  dplyr::mutate(NZSEI13_10 =  NZSEI13/10)%>%
  dplyr::mutate(NZSEI13_lead2_10 =  as.integer(NZSEI13_lead2/10))%>%
  # dplyr::mutate(Hours.Work_lead1_10 = Hours.Work_lead1/10)%>%
  # dplyr::mutate(Hours.Work_lead1ord = (as.numeric(
  #   cut(
  #     Hours.Work_lead1,
  #     breaks = c(-Inf,  20, 40, Inf),
  #     labels = c("0", "20", "40",  "over40"),
  #     right = TRUE
  #   )
  # ) - 1)) %>%
  dplyr::mutate(across(where(is.numeric), ~ scale(.x), .names = "{col}_z")) %>%
  select(-c(.imp_z, .id_z)) %>%
  dplyr::mutate(id = as.factor(rep(1:N, 10)))# needed for g-comp


# Get data into shape
wf3t <- w_f2t %>% mutate_if(is.matrix, as.vector)
wl3t <- w_l2t %>% mutate_if(is.matrix, as.vector)

w_mtt <- mice::as.mids(wl3t)

saveh(w_mt, "w_mtt")
saveh(wf3, "wf3t")

###### READ THIS DATA IN BELLA  #########

w_mtt <- readh("w_mtt")

wf3t <- readh("wf3t")


# hist(wf3$Hours.Work_lead1_10_z)
# hist(wf3$BornNZ_z)




# BornNZ_z
# HLTH.SleepHours_z
# HLTH.Disability_z

# model equations ---------------------------------------------------------
baselinevars = c("AGREEABLENESS_z","CONSCIENTIOUSNESS_z","EXTRAVERSION_z","HONESTY_HUMILITY_z","NEUROTICISM_z","OPENNESS_z","Age_z","Alcohol.Frequency_z","Alcohol.Intensity_log_z","Bodysat_z","BornNZ_z","Believe.God_z","Believe.Spirit_z","BELONG_z","CharityDonate_log_z","ChildrenNum_z","Church_z", "community","Edu_z","Employed_z","EmotionRegulation1_z", "EmotionRegulation2_z","EmotionRegulation3_z","Euro_z", "GRATITUDE_z","HomeOwner_z","Hours.Exercise_log_z","Hours.Work_z","HLTH.BMI_z", "HLTH.Disability_z", "HLTH.Fatigue_z", "HLTH.SleepHours_z", "ImpermeabilityGroup_z","income_log_z", "KESSLER6sum_z", "LIFEMEANING_z", "LIFESAT_z", "Male_z","NZdep_z", "NWI_z","NZSEI13_z","Parent_z","Partner_z","PERFECTIONISM_z", "PermeabilityIndividual_z", "Pol.Orient_z", "POWERDEPENDENCE1_z","POWERDEPENDENCE2_z","Relid_z", "Respect.Self_z", "retired", "Rumination_z","SELF.CONTROL_z", "SELF.ESTEEM_z", "semiretired", "SexualSatisfaction_z","SFHEALTH_z","Smoker_z", "Spiritual.Identification_z","Standard.Living_z", "SUPPORT_z","Urban_z", "VENGEFUL.RUMIN_z", "Volunteers_z", "Your.Health_z", "Your.Future.Security_z", "Your.Personal.Relationships_z")


# functions ---------------------------------------------------------------

# see "funs.R"

## Also use
round( EValue::evalues.OLS( , se = , sd = 1, delta = 2, true = 0), 3)
round( EValue::evalues.RR( , lo =  , hi =, true = 1), 3)


# set up vars -------------------------------------------------------------

ylim <- c(-.25,.25)
xlab <- "Weekly Hours Work/10"
df <-  w_mt
m = 10
X = "Hours.Work_lead1_10"
x = c(0,2,4,6)
xlab = "Work/10 hours"
# reference level
r = 4

# bmi ---------------------------------------------------------------------
#conditional  -- not we are using splines
bmi_st <- as.formula(paste("HLTH.BMI_lead2_z ~ bs(Hours.Work_lead1_10) +",
                           paste(baselinevars,
                                 collapse = "+")))

# this mice code won't work, alas!
#m1_bmi <- with(w_mt, exp = glm(bmi_st))

## for some reason, writing a function works.
fit_bmi = function(formula) {
  with(w_mt, glm(as.formula(paste("HLTH.BMI_lead2_z ~ bs(Hours.Work_lead1_10) +",
                                 paste(baselinevars,
                                       collapse = "+")))) )
}
# pooled model
m1_bmi <- fit_bmi()

main = "BMI"
ylab = "BMI (SD)"

# gcomputation
out_bmi <- pool_stglm(m1_bmi, df = df, m = m,  X = X, x = x)

## tba
out_bmi %>%
  print_html()

# turn off image
dev.off()

# plot
plot_stglm(out_bmi, ylim, main, xlab, ylab)
ggplot_stglm(out_bmi, ylim, main, xlab, ylab)

##
out_bmic <- pool_stglm_contrast(m1_bmi, df = df, m = m,  X = X, x = x, r= r)
plot_stglm_contrast(out_bmic, ylim, main, xlab, ylab)
ggplot_stglm_contrast(out_bmic, ylim, main, xlab, ylab)

out_bmic %>%
  print_md()
#
# # Marginal Contrasts requires the long data from MICED
# # G computation
# m1_long <- glm(bmi_st, data = wf3)
# gform_m1<- stdGlm(fit = m1_long, data = wf3, X = "Hours.Work_lead1_10", x =x, clusterid="id")
# summary(gform_m1, contrast = "difference", reference = r)
#
# # increase
# round( EValue::evalues.OLS(  , se =    , sd = 1, delta = 3, true = 0), 3)
#
# # decrease
# round( EValue::evalues.OLS( -0.0222  , se = 0.00768  , sd = 1, delta = 3, true = 0), 3)
#
# # Graph
# plot(gform_m1, ylim = ylim,
#      contrast = "difference", reference = r,
#      main="Standarised BMI difference relative to baseline", col.main="black",
#      sub="Baseline = 30 hours", col.sub="black",
#      xlab="Work Hours/10", ylab="BMI",
#      col.lab="black", cex.lab=0.75)
#

# verify comparisons
library(marginaleffects)
# unit level change
comps_m0 <- comparisons(m1_long, variables = "Hours.Work_lead1_10")

summary(comps_m0) |>
  print_md()


# distribution of unit level changes
ggplot(comps_m0, aes(comparison)) +
  geom_histogram(bins = 30) +
  facet_wrap(~contrast, scale = "free_x") +
  labs(x = "Distribution of unit-level contrasts")

#comparisons at diff levels
comps_m1 <- comparisons(m1_long, variables = list(Hours.Work_lead1_10 = c(0,3))) %>% tidy()
comps_m1

comps_m2 <- comparisons(m1_long, variables = list(Hours.Work_lead1_10 = c(3,6))) %>% tidy()
comps_m2


# sf-health ---------------------------------------------------------------
sfhealth_st <- as.formula(paste("SFHEALTH_lead2_z~ bs(Hours.Work_lead1_10) +",
                                paste(baselinevars,
                                      collapse = "+")))
## fit
fit_sfhealth = function(formula) {
  with(w_mt, glm(as.formula(paste("SFHEALTH_lead2_z~ bs(Hours.Work_lead1_10) +",
                                 paste(baselinevars,
                                       collapse = "+")))))
}
# ppoled model
m2_sfhealth <- fit_sfhealth()

main = "SF Health"
ylab = "SF Health (SD)"
out_sfhealth <- pool_stglm(m2_sfhealth, df = df, m = m,  X = X, x = x)
plot_stglm(out_sfhealth, ylim, main, xlab, ylab)
ggplot_stglm(out_sfhealth, ylim, main, xlab, ylab)


out_c <- pool_stglm_contrast(m2_sfhealth, df = df, m = m,  X = X, x = x, r= r)
plot_stglm_contrast(out_c, ylim, main, xlab, ylab)
ggplot_stglm_contrast(out_c, ylim, main, xlab, ylab)

out_c %>%
  print_md()

# exercise ---------------------------------------------------------------
Hours.Exercise_st <- as.formula(paste("Hours.Exercise_lead2_log_z~ bs(Hours.Work_lead1_10) +",  paste(baselinevars, collapse = "+")))

## fit for mice to work, don't ask why
fit_Hours.Exercise = function(formula) {
  with(w_mt, glm(as.formula(paste("Hours.Exercise_lead2_log_z~ bs(Hours.Work_lead1_10) +",  paste(baselinevars,collapse = "+")))))
}

# pooled model
m3_Hours.Exercise <- fit_Hours.Exercise()

main = "Log Hours Exercise"
ylab = "Log Hours Exercise (SD)"

out_Hours.Exercise <- pool_stglm(m3_Hours.Exercise, df = df, m = m,  X = X, x = x)
plot_stglm(out_Hours.Exercise, ylim = ylim, main, xlab, ylab)
ggplot_stglm(out_Hours.Exercise, ylim, main, xlab, ylab)


out_c <- pool_stglm_contrast(m3_Hours.Exercise, df = df, m = m,  X = X, x = x, r= r)
plot_stglm_contrast(out_c, ylim, main, xlab, ylab)
ggplot_stglm_contrast(out_c, ylim, main, xlab, ylab)

# how much is one standard deviation of exercise

sd(exp(wf3$Hours.Exercise_lead2_log)+1)

sd(wf3$Hours.Exercise_lead2)

#7.15373/10  # t/10th standard deviation
# 60 * .715373 = 42 mins

# HLTH.Sleep --------------------------------------------------------------

HLTH.SleepHours_st <- as.formula(paste("HLTH.SleepHours_lead2 ~ bs(Hours.Work_lead1_10) +",  paste(baselinevars, collapse = "+")))

## fit
fit_HLTH.SleepHours = function(formula) {
  with(w_mt, glm(as.formula(paste("HLTH.SleepHours_lead2_z ~ bs(Hours.Work_lead1_10) +",  paste(baselinevars,collapse = "+")))))
}
# pooled model
m4_HLTH.SleepHours <- fit_HLTH.SleepHours()

main = "Hours Sleep (SD)"
ylab = "Hours Sleep (SD)"
out_HLTH.SleepHours <- pool_stglm(HLTH.SleepHours_lead2_z, df = df, m = m,  X = X, x = x)
out_HLTH.SleepHours

plot_stglm(out_HLTH.SleepHours, ylim = ylim, main, xlab, ylab)
ggplot_stglm(out_HLTH.SleepHours, ylim, main, xlab, ylab)


dev.off()
out_c <- pool_stglm_contrast(m4_HLTH.SleepHours, df = df, m = m,  X = X, x = x, r= r)
plot_stglm_contrast(out_c, ylim, main, xlab, ylab)
ggplot_stglm_contrast(out_c, ylim, main, xlab, ylab)
out_c


#
# sd(wf3$HLTH.SleepHours_lead2)
# 1.061555 * number  # t/10th standard deviation
# 60 * number)

# smoker ------------------------------------------------------------------

Smoker_st <- as.formula(paste("Smoker_lead2 ~ bs(Hours.Work_lead1_10) +",
                              paste(baselinevars,
                                    collapse = "+")))

# fit
fit_Smoker = function(formula) {
  with(w_mt, glm(as.formula(paste("Smoker_lead2 ~ bs(Hours.Work_lead1_10) +",
                                 paste(baselinevars,
                                       collapse = "+"))), family = "poisson"))
}
# pooled model
m5_Smoker <- fit_Smoker()
main = "Smoking Rate"
ylab = "Smoking Rate"
out_Smoker <- pool_stglm(m5_Smoker, df = df, m = m,  X = X, x = x)

plot_stglm(out_Smoker, ylim = c(0,.06), main, xlab, ylab)
ggplot_stglm(out_Smoker, ylim = c(0,.06), main, xlab, ylab)

out_c <- pool_stglm_contrast_ratio(m5_Smoker, df = df, m = m,  X = X, x = x, r= r)


plot_stglm_contrast(out_c, ylim = c(.75,1.5), main, xlab, ylab)
ggplot_stglm_contrast(out_c, ylim = c(.75,1.5), main, xlab, ylab)
#
# # contrast model for comparison
# m5_long <- glm(Smoker_st, data = wf3, family = "poisson")
# m5_long
# gform_m5<- stdGlm(fit = m5_long, data = wf3, X = X, x =x, clusterid="id")
# summary(gform_m5,
#         contrast = "ratio",
#         type="odds",
#         reference = r)
#
# plot(gform_m5)
#
# # evalues for risk ratios
# round( EValue::evalues.RR(0.899 , lo =  0.8808, hi = 0.9172, true = 1), 3)
# round( EValue::evalues.RR(1.207  , lo =  1.1759, hi =1.2381, true = 1), 3)
#
# # increase
# round( EValue::evalues.OLS(  , se =     , sd = 1, delta = 3, true = 0), 3)
# # decrease
# round( EValue::evalues.OLS(-    , se =    , sd = 1, delta = 3, true = 0), 3)
#
#
# plot(gform_m5,# ylim = c(0,.06),
#      contrast = "ratio",
#      reference = r,# type = "odds",
#      main="Smoking risk ratio relative to baseline ", col.main="black",
#      sub="Baseline = 30 hours", col.sub="black",
#      xlab="Work Hours/10", ylab="Smoking rate",
#      col.lab="black", cex.lab=0.75)

# ratio of adjusted predictions

# p1 <- plot_cco(
#   m5_long,
#   effect = "Hours.Work_lead1_10",
#   condition = "Hours.Work_lead1_10",
#   transform_pre = "ratio") +
#   ylab("Adjusted Risk Ratio\nP(Smoke | Work + 1) / P(Smoke | work)")
#
# p1

# fatigue -----------------------------------------------------------------

# formula
HLTH.Fatigue_st <- as.formula(paste("HLTH.Fatigue_lead2_z~ bs(Hours.Work_lead1_10) +",
                                    paste(baselinevars, collapse = "+")))

# fit
fit_Fatigue = function(formula) {
  with(w_mt, glm(as.formula(paste("HLTH.Fatigue_lead2_z ~ bs(Hours.Work_lead1_10) +",
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
ggplot_stglm(out_Fatigue, ylim = ylim, main, xlab, ylab)

out_cr <- pool_stglm_contrast(m6_Fatigue, df = df, m = m,  X = X, x = x, r= r)
plot_stglm_contrast(out_cr, ylim, main, xlab, ylab)
ggplot_stglm_contrast(out_cr, ylim, main, xlab, ylab)

wf3$HLTH.Fatigue_lead2_z
sd(wf3$HLTH.Fatigue_lead2)

# alcohol freq ------------------------------------------------------------

# fit
fit_Alcohol.Frequency = function(formula) {
  with(w_mt, glm(as.formula(paste("Alcohol.Frequency_lead2ord_z~ bs(Hours.Work_lead1_10) +",
                                 paste(baselinevars, collapse = "+")))))
}
# ppoled model
m7_Alcohol.Frequency <- fit_Alcohol.Frequency()

main = "Alcohol Frequency"
ylab = "Alcohol Frequency (SD)"

# g-formula
out_Alcohol.Frequency <- pool_stglm(m7_Alcohol.Frequency, df = df, m = m,  X = X, x = x)

# plots
out_Alcohol.Frequency

# evalue
round( EValue::evalues.OLS( , se = , sd = 1, delta = 3, true = 0), 3)


plot_stglm(out_Alcohol.Frequency, ylim = ylim, main, xlab, ylab)
ggplot_stglm(out_Alcohol.Frequency, ylim = ylim, main, xlab, ylab)

out_cr <- pool_stglm_contrast(m7_Alcohol.Frequency, df = df, m = m,  X = X, x = x, r= r)
plot_stglm_contrast(out_cr, ylim, main, xlab, ylab)

# Evalues
# round( EValue::evalues.RR( , lo =  , hi = , true = 1), 3)
# round( EValue::evalues.RR(1.207  , lo =  1.1759, hi =1.2381, true = 1), 3)

# evalue  for loss
round( EValue::evalues.OLS( -0.029038016, se = 0.01082889, sd = 1, delta = 3, true = 0), 3)



# Alcohol.Intensity ----------------------------------------------------------

# fit
fit_Alcohol.Intensity = function(formula) {
  with(w_mt, glm(as.formula(paste("Alcohol.Intensity_log_lead2_z~ bs(Hours.Work_lead1_10) +",
                                 paste(baselinevars,
                                       collapse = "+")))))
}

# ppoled model
m8_Alcohol.Intensity  <- fit_Alcohol.Intensity()

main = "Alcohol Intensity"
ylab = "Alcohol Intensity (SD)"

# g-formula
out_Alcohol.Intensity <- pool_stglm(m8_Alcohol.Intensity, df = df, m = m,  X = X, x = x)

# plots
plot_stglm(out_Alcohol.Frequency, ylim = ylim, main, xlab, ylab)
ggplot_stglm(out_Alcohol.Frequency, ylim = ylim, main, xlab, ylab)

out_cr <- pool_stglm_contrast(m8_Alcohol.Intensity, df = df, m = m,  X = X, x = x, r= r)
ggplot_stglm_contrast(out_cr, ylim, main, xlab, ylab)

# body satisfaction -------------------------------------------------------
# fit
fit_Bodysat_st = function(formula) {
  with(w_mt, glm(as.formula(paste("Bodysat_lead2_z~ bs(Hours.Work_lead1_10) +",
                                 paste(baselinevars,
                                       collapse = "+")))))
}

# ppoled model
m9_Bodysat_st  <- fit_Bodysat_st()

# g-formula
out_p <- pool_stglm(m9_Bodysat_st, df = df, m = m,  X = X, x = x)

# plots
main = "Body Satisfaction"
ylab = "Body Satisfaction (SD)"

plot_stglm(out_p, ylim = ylim, main, xlab, ylab)
ggplot_stglm(out_p, ylim = ylim, main, xlab, ylab)

out_cr <- pool_stglm_contrast(m9_Bodysat_st, df = df, m = m,  X = X, x = x, r= r)
plot_stglm_contrast(out_cr, ylim, main, xlab, ylab)

ggplot_stglm_contrast(out_cr, ylim, main, xlab, ylab)

# increase
round( EValue::evalues.OLS(  , se =    , sd = 1, delta = 3, true = 0), 3)

# decrease
round( EValue::evalues.OLS(   , se =   , sd = 1, delta = 3, true = 0), 3)


# rumination --------------------------------------------------------------
# During the last 30 days, how often did.... you have negative thoughts that repeated over and over?

# fit
fit_Rumination_st = function(formula) {
  with(w_mt, glm(as.formula(paste("Rumination_lead2ord_z~ bs(Hours.Work_lead1_10) +",
                                 paste(baselinevars,
                                       collapse = "+")))))
}

m10_Rumination <- fit_Rumination_st()

#labels
main = "Rumination"
ylab = "Rumination (SD)"

#g-compuations
out_p <- pool_stglm(m10_Rumination, df = df, m = m,  X = X, x = x)

plot_stglm(out_p, ylim = ylim, main, xlab, ylab)
ggplot_stglm(out_p, ylim = ylim, main, xlab, ylab)

out_cr <- pool_stglm_contrast(m10_Rumination, df = df, m = m,  X = X, x = x, r= r)
plot_stglm_contrast(out_cr, ylim, main, xlab, ylab)

# increase
round( EValue::evalues.OLS(  , se =    , sd = 1, delta = 3, true = 0), 3)
# decrease
round( EValue::evalues.OLS(   , se =   , sd = 1, delta = 3, true = 0), 3)


# sex satisfaction --------------------------------------------------------


SexualSatisfaction_st <- as.formula(paste("SexualSatisfaction_lead2_z~ bs(Hours.Work_lead1_10) +", paste(baselinevars, collapse = "+")))

## fit
fit_SexualSatisfaction_st = function(formula) {
  with(w_mt, glm( as.formula(paste("SexualSatisfaction_lead2_z~ bs(Hours.Work_lead1_10) +", paste(baselinevars, collapse = "+")))))
}
# ppoled model
m11_SexualSatisfaction_st <- fit_SexualSatisfaction_st()

main = "Sexual Satisfaction (SD)"
ylab = "Sexual Satisfaction (SD)"
out_p <- pool_stglm(m11_SexualSatisfaction_st, df = df, m = m,  X = X, x = x)

plot_stglm(out_p, ylim = ylim, main, xlab, ylab)
ggplot_stglm(out_p, ylim = ylim, main, xlab, ylab)

out_cr <- pool_stglm_contrast(m11_SexualSatisfaction_st, df = df, m = m,  X = X, x = x, r= r)
plot_stglm_contrast(out_cr, ylim, main, xlab, ylab)

# increase
round( EValue::evalues.OLS(  , se =    , sd = 1, delta = 3, true = 0), 3)
# decrease
round( EValue::evalues.OLS(   , se =   , sd = 1, delta = 3, true = 0), 3)



# emotional regulation 1 ----------------------------------------------------
# When I feel negative emotions, my emotions feel out of control.

EmotionRegulation1_st <- as.formula(paste("EmotionRegulation1_lead2_z~ bs(Hours.Work_lead1_10) +", paste(baselinevars, collapse = "+")))

## fit
fit_EmotionRegulation1 = function(formula) {
  with(w_mt, glm(as.formula(paste("EmotionRegulation1_lead2_z~ bs(Hours.Work_lead1_10) +", paste(baselinevars, collapse = "+")))))
}
# ppoled model
m12_fit_EmotionRegulation1 <- fit_EmotionRegulation1()

main = "Emotion Regulation1"
ylab = "Emotion Regulation1 (SD)"
out_p<- pool_stglm(m12_fit_EmotionRegulation1, df = df, m = m,  X = X, x = x)

plot_stglm(out_p, ylim = ylim, main, xlab, ylab)
ggplot_stglm(out_p, ylim = ylim, main, xlab, ylab)

out_cr <- pool_stglm_contrast(m12_fit_EmotionRegulation1, df = df, m = m,  X = X, x = x, r= r)
plot_stglm_contrast(out_cr, ylim, main, xlab, ylab)
ggplot_stglm_contrast(out_cr, ylim, main, xlab, ylab)


# increase
round( EValue::evalues.OLS(  , se =    , sd = 1, delta = 3, true = 0), 3)
# decrease
round( EValue::evalues.OLS(   , se =   , sd = 1, delta = 3, true = 0), 3)



# emotional reg 2 ---------------------------------------------------------
# When I feel negative emotions, I suppress or hide my emotions.

EmotionRegulation2_st <- as.formula(paste("EmotionRegulation2_lead2_z~ bs(Hours.Work_lead1_10) +",paste(baselinevars,collapse = "+")))


## fit
fit_EmotionRegulation2 = function(formula) {
  with(w_mt, glm(as.formula(paste("EmotionRegulation2_lead2_z~ bs(Hours.Work_lead1_10) +", paste(baselinevars, collapse = "+")))))
}
# ppoled model
m13_fit_EmotionRegulation2 <- fit_EmotionRegulation2()

main = "Emotion Regulation2"
ylab = "Emotion Regulation2 (SD)"
out_p <- pool_stglm(m13_fit_EmotionRegulation2, df = df, m = m,  X = X, x = x)

plot_stglm(out_p, ylim = ylim, main, xlab, ylab)
ggplot_stglm(out_p, ylim = ylim, main, xlab, ylab)

out_cr <- pool_stglm_contrast(m13_fit_EmotionRegulation2, df = df, m = m,  X = X, x = x, r= r)
plot_stglm_contrast(out_cr, ylim, main, xlab, ylab)

# increase
round( EValue::evalues.OLS(  , se =    , sd = 1, delta = 3, true = 0), 3)
# decrease
round( EValue::evalues.OLS(   , se =   , sd = 1, delta = 3, true = 0), 3)


# emotional reg 3 ---------------------------------------------------------
# When I feel negative emotions, I change the way I think to help me stay calm.

## fit
fit_EmotionRegulation3 = function(formula) {
  with(w_mt, glm(as.formula(paste("EmotionRegulation3_lead2_z~ bs(Hours.Work_lead1_10) +", paste(baselinevars, collapse = "+")))))
}
# ppoled model
m14_fit_EmotionRegulation3 <- fit_EmotionRegulation3()

main = "Emotion Regulation3"
ylab = "Emotion Regulation3 (SD)"
out_p <- pool_stglm(m14_fit_EmotionRegulation3, df = df, m = m,  X = X, x = x)

plot_stglm(out_p, ylim = ylim, main, xlab, ylab)
ggplot_stglm(out_p, ylim = ylim, main, xlab, ylab)

out_cr <- pool_stglm_contrast(m14_fit_EmotionRegulation3, df = df, m = m,  X = X, x = x, r= r)
ggplot_stglm_contrast(out_cr, ylim, main, xlab, ylab)

# increase
round( EValue::evalues.OLS(  , se =    , sd = 1, delta = 3, true = 0), 3)
# decrease
round( EValue::evalues.OLS(   , se =   , sd = 1, delta = 3, true = 0), 3)


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
  with(w_mt, glm(as.formula(paste("KESSLER6sum_lead2_z~ bs(Hours.Work_lead1_10) +",
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

out_cr <- pool_stglm_contrast(m15_KESSLER6sum_st, df = df, m = m,  X = X, x = x, r= r)
ggplot_stglm_contrast(out_cr, ylim, main, xlab, ylab)

out_cr

effectunit <- sd(wf3$KESSLER6sum_lead2) *  0.036227231

# note that 5 is consider moderate distress
ave40 <-mean(wf3$KESSLER6sum_lead2)
ave40

# the loss of 40 hour work pushes people over this threshold
ave40 + effectunit

# increase
round( EValue::evalues.OLS(  , se =    , sd = 1, delta = 3, true = 0), 3)
# decrease
round( EValue::evalues.OLS(   , se =   , sd = 1, delta = 3, true = 0), 3)

# how to examine differences?

diffbase <- pool_stglm_contrast(m15_KESSLER6sum_st, df = df, m = m,  X = X, x = x, r= 0)
diffbase

diffbase2 <- pool_stglm_contrast(m15_KESSLER6sum_st, df = df, m = m,  X = X, x = x, r= 6)
diffbase2

# plot it
ggplot_stglm_contrast(diffbase, ylim, main, xlab, ylab)
ggplot_stglm_contrast(diffbase2, ylim, main, xlab, ylab)



# power dependence 1 ------------------------------------------------------
# I do not have enough power or control over important parts of my life.
## fit
fit_POWERDEPENDENCE1_st = function(formula) {
  with(w_mt, glm(as.formula(paste("POWERDEPENDENCE1_lead2_z~ bs(Hours.Work_lead1_10) +",
                                 paste(baselinevars,
                                       collapse = "+")))))
}
# ppoled model
m16_POWERDEPENDENCE1_st <- fit_POWERDEPENDENCE1_st()

main = "Power Dependence 1"
ylab = "Power Dependence 1(SD)"
out_p <- pool_stglm(m16_POWERDEPENDENCE1_st, df = df, m = m,  X = X, x = x)

plot_stglm(out_p, ylim = ylim, main, xlab, ylab)
ggplot_stglm(out_p, ylim = ylim, main, xlab, ylab)

out_cr <- pool_stglm_contrast(m16_POWERDEPENDENCE1_st, df = df, m = m,  X = X, x = x, r= r)
ggplot_stglm_contrast(out_cr, ylim, main, xlab, ylab)

# increase
round( EValue::evalues.OLS(  , se =    , sd = 1, delta = 3, true = 0), 3)
# decrease
round( EValue::evalues.OLS(   , se =   , sd = 1, delta = 3, true = 0), 3)

# power dependence 2 ------------------------------------------------------
## fit
#Other people have too much power or control over important parts of my life.

fit_POWERDEPENDENCE2_st = function(formula) {
  with(w_mt, glm(as.formula(paste("POWERDEPENDENCE2_lead2_z~ bs(Hours.Work_lead1_10) +",
                                 paste(baselinevars,
                                       collapse = "+")))))
}
# ppoled model
m17_POWERDEPENDENCE2_st <- fit_POWERDEPENDENCE2_st()

main = "Power Dependence 2"
ylab = "Power Dependence 2(SD)"
out_p <- pool_stglm(m17_POWERDEPENDENCE2_st, df = df, m = m,  X = X, x = x)

plot_stglm(out_p, ylim = ylim, main, xlab, ylab)
ggplot_stglm(out_p, ylim = ylim, main, xlab, ylab)

out_cr <- pool_stglm_contrast(m17_POWERDEPENDENCE2_st, df = df, m = m,  X = X, x = x, r= r)
ggplot_stglm_contrast(out_cr, ylim, main, xlab, ylab)

# increase
round( EValue::evalues.OLS(  , se =    , sd = 1, delta = 3, true = 0), 3)
# decrease
round( EValue::evalues.OLS(   , se =   , sd = 1, delta = 3, true = 0), 3)

# perfectionism  ----------------------------------------------------------
# Perfectionism Discrepancy Subscale
# Doing my best never seems to be enough.
# My performance rarely measures up to my standards.
# I am hardly ever satisfied with my performance.

## fit
fit_PERFECTIONISM_st = function(formula) {
  with(w_mt, glm( as.formula(paste("PERFECTIONISM_lead2_z~ bs(Hours.Work_lead1_10) +",
                                  paste(baselinevars,
                                        collapse = "+")))))
}
# ppoled model
m18_PERFECTIONISM_st <- fit_PERFECTIONISM_st()

main = "Perfectionism"
ylab = "Perfectionism (SD)"
out_p <- pool_stglm(m18_PERFECTIONISM_st, df = df, m = m,  X = X, x = x)

plot_stglm(out_p, ylim = ylim, main, xlab, ylab)
ggplot_stglm(out_p, ylim = ylim, main, xlab, ylab)

out_cr <- pool_stglm_contrast(m18_PERFECTIONISM_st, df = df, m = m,  X = X, x = x, r= r)
ggplot_stglm_contrast(out_cr, ylim, main, xlab, ylab)

# increase
round( EValue::evalues.OLS(  , se =    , sd = 1, delta = 3, true = 0), 3)
# decrease
round( EValue::evalues.OLS(   , se =   , sd = 1, delta = 3, true = 0), 3)
# self esteem -------------------------------------------------------------
# Self-esteem
# On the whole am satisfied with myself.
# Take a positive attitude toward myself.
# Am inclined to feel that I am a failure.

## fit
fit_SELF.ESTEEM_st = function(formula) {
  with(w_mt, glm( as.formula(paste("SELF.ESTEEM_lead2_z~ bs(Hours.Work_lead1_10) +",
                                  paste(baselinevars,
                                        collapse = "+")))))
}
#  model
m19_SELF.ESTEEM_st <- fit_SELF.ESTEEM_st()

main = "Self Esteem"
ylab = "Self Esteem (SD)"
out_p <- pool_stglm(m19_SELF.ESTEEM_st, df = df, m = m,  X = X, x = x)
plot_stglm(out_p, ylim = ylim, main, xlab, ylab)
ggplot_stglm(out_p, ylim = ylim, main, xlab, ylab)

out_cr <- pool_stglm_contrast(m19_SELF.ESTEEM_st, df = df, m = m,  X = X, x = x, r= r)
ggplot_stglm_contrast(out_cr, ylim, main, xlab, ylab)

# increase
round( EValue::evalues.OLS(  , se =    , sd = 1, delta = 3, true = 0), 3)
# decrease
round( EValue::evalues.OLS(   , se =   , sd = 1, delta = 3, true = 0), 3)


# gratitude ---------------------------------------------------------------
# Gratitude
# I have much in my life to be thankful for.
# When I look at the world, I don’t see much to be grateful for.
# I am grateful to a wide variety of people.

## fit
fit_GRATITUDE_st = function(formula) {
  with(w_mt, glm( as.formula(paste("GRATITUDE_lead2_z~ bs(Hours.Work_lead1_10) +",
                                  paste(baselinevars,
                                        collapse = "+")))
  ))
}
#  model
m20_GRATITUDE_st <- fit_GRATITUDE_st()

main = "Gratitude"
ylab = "Gratitude (SD)"
out_p <- pool_stglm(m20_GRATITUDE_st, df = df, m = m,  X = X, x = x)

plot_stglm(out_p, ylim = ylim, main, xlab, ylab)
ggplot_stglm(out_p, ylim = ylim, main, xlab, ylab)

out_cr <- pool_stglm_contrast(m20_GRATITUDE_st, df = df, m = m,  X = X, x = x, r= r)
ggplot_stglm_contrast(out_cr, ylim, main, xlab, ylab)

# increase
round( EValue::evalues.OLS(  , se =    , sd = 1, delta = 3, true = 0), 3)
# decrease
round( EValue::evalues.OLS(   , se =   , sd = 1, delta = 3, true = 0), 3)

# veng rumination ---------------------------------------------------------
# Forgivingness versus Vengeful Rumination
# Sometimes I can't sleep because of thinking about past wrongs I have suffered.
# I can usually forgive and forget when someone does me wrong.
# I find myself regularly thinking about past times that I have been wronged.


## fit
fit_VENGEFUL.RUMIN_st = function(formula) {
  with(w_mt, glm( as.formula(paste("VENGEFUL.RUMIN_lead2_z~ bs(Hours.Work_lead1_10) +",
                                  paste(baselinevars,
                                        collapse = "+")))
  ))
}
# pooled model
m21_VENGEFUL.RUMIN_st <- fit_VENGEFUL.RUMIN_st()

main = "Vengefulness (anti-Foregiveness)"
ylab = "Vengefulness (anti-Foregiveness) (SD)"
out_p <- pool_stglm(m21_VENGEFUL.RUMIN_st, df = df, m = m,  X = X, x = x)

plot_stglm(out_p, ylim = ylim, main, xlab, ylab)
ggplot_stglm(out_p, ylim = ylim, main, xlab, ylab)

out_cr <- pool_stglm_contrast(m21_VENGEFUL.RUMIN_st, df = df, m = m,  X = X, x = x, r= r)
ggplot_stglm_contrast(out_cr, ylim, main, xlab, ylab)

# increase
round( EValue::evalues.OLS(  , se =    , sd = 1, delta = 3, true = 0), 3)
# decrease
round( EValue::evalues.OLS(   , se =   , sd = 1, delta = 3, true = 0), 3)


# life meaning ------------------------------------------------------------

# Meaning in Life
# My life has a clear sense of purpose.
# I have a good sense of what makes my life meaningful.

LIFEMEANING_st <- as.formula(paste("LIFEMEANING_lead2_z~ bs(Hours.Work_lead1_10) +",
                                   paste(baselinevars,
                                         collapse = "+")))
## fit
fit_LIFEMEANING_st = function(formula) {
  with(w_mt, glm(
    as.formula(paste("LIFEMEANING_lead2ord_z~ bs(Hours.Work_lead1_10) +",
                     paste(baselinevars,
                           collapse = "+")))
  ))
}
# pooled model
m22_fit_LIFEMEANING_st <- fit_LIFEMEANING_st()

main = "Life Meaning"
ylab = "Life Meaning (SD)"
out_p <- pool_stglm(m22_fit_LIFEMEANING_st, df = df, m = m,  X = X, x = x)

plot_stglm(out_p, ylim = ylim, main, xlab, ylab)
ggplot_stglm(out_p, ylim = ylim, main, xlab, ylab)

out_cr <- pool_stglm_contrast(m22_fit_LIFEMEANING_st, df = df, m = m,  X = X, x = x, r= r)
ggplot_stglm_contrast(out_cr, ylim, main, xlab, ylab)
out_cr
# increase
round( EValue::evalues.OLS(  , se =    , sd = 1, delta = 3, true = 0), 3)
# decrease
round( EValue::evalues.OLS(   , se =   , sd = 1, delta = 3, true = 0), 3)

# honesty humility --------------------------------------------------------

# Mini-IPIP6 Honesty-Humility (item overlap with Psychological Entitlement)
# Would like to be seen driving around in a very expensive car.
# Would get a lot of pleasure from owning expensive luxury goods.
# Feel entitled to more of everything.
# Deserve more things in life.

## fit
fit_HONESTY_HUMILITY_st = function(formula) {
  with(w_mt, glm(
    as.formula(paste("HONESTY_HUMILITY_lead2_z~ bs(Hours.Work_lead1_10) +",
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

BELONG_st <- as.formula(paste("BELONG_lead2_z~ bs(Hours.Work_lead1_10) +",
                              paste(baselinevars,
                                    collapse = "+")))
## fit
fit_BELONG_st = function(formula) {
  with(w_mt, glm(
    as.formula(paste("BELONG_lead2_z~ bs(Hours.Work_lead1_10) +",
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


SUPPORT_st <- as.formula(paste("SUPPORT_lead2_z~ bs(Hours.Work_lead1_10) +",
                               paste(baselinevars,
                                     collapse = "+")))

## fit
fit_SUPPORT_st = function(formula) {
  with(w_mt, glm(
    as.formula(paste("SUPPORT_lead2_z~ bs(Hours.Work_lead1_10) +",
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
ggplot_stglm(out_p, ylim = ylim, main, xlab, ylab)

out_cr <- pool_stglm_contrast(m25_SUPPORT_st, df = df, m = m,  X = X, x = x, r= r)
ggplot_stglm_contrast(out_cr, ylim, main, xlab, ylab)

# increase
round( EValue::evalues.OLS(  , se =    , sd = 1, delta = 3, true = 0), 3)
# decrease
round( EValue::evalues.OLS(   , se =   , sd = 1, delta = 3, true = 0), 3)


# volunteers --------------------------------------------------------------
Volunteers_st <- as.formula(paste("Volunteers_lead2~ bs(Hours.Work_lead1_10) +",
                                  paste(baselinevars,  collapse = "+")))


# fit
fit_Volunteers_st = function(formula) {
  with(w_mt, glm(as.formula(paste("Volunteers_lead2~ bs(Hours.Work_lead1_10) +",
                                 paste(baselinevars,
                                       collapse = "+")))))
}
# pooled model
m26_Volunteers <- fit_Volunteers_st()


main = "Volunteer Rate"
ylab = "Volunteer Rate"
out_Volunteers<- pool_stglm(m26_Volunteers, df = df, m = m,  X = X, x = x)

plot_stglm(out_Volunteers, ylim = c(0,.06), main, xlab, ylab)
ggplot_stglm(out_Volunteers, ylim = c(0,.06), main, xlab, ylab)

out_c <- pool_stglm_contrast_ratio(m26_Volunteers, df = df, m = m,  X = X, x = x, r= r)

plot_stglm_contrast(out_c, ylim = c(.75,1.5), main, xlab, ylab)
ggplot_stglm_contrast(out_c, ylim = c(.75,1.5), main, xlab, ylab)
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
  with(w_mt, glm(
    as.formula(paste("CharityDonate_log_lead2_z~ bs(Hours.Work_lead1_10) +",
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
  with(w_mt, glm(
    as.formula(paste("community_lead2_z~ bs(Hours.Work_lead1_10) +",
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
  with(w_mt, glm(
    as.formula(paste("NWI_lead2_z~ bs(Hours.Work_lead1_10) +",
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
plot_stglm_contrast(out_cr, ylim, main, xlab, ylab)


# imperm group ------------------------------------------------------------
ImpermeabilityGroup_st <- as.formula(paste("ImpermeabilityGroup_lead2_z~ bs(Hours.Work_lead1_10) +",
                                           paste(baselinevars,
                                                 collapse = "+")))
## fit
fit_ImpermeabilityGroup_st = function(formula) {
  with(w_mt, glm(
    as.formula(paste("ImpermeabilityGroup_lead2_z~ bs(Hours.Work_lead1_10) +",
                     paste(baselinevars,
                           collapse = "+")))
  ))
}

# pooled model
m30_ImpermeabilityGroup_st <- fit_ImpermeabilityGroup_st()
main = "Impermeability Group"
ylab = "Impermeability Group (SD)"
out_p <- pool_stglm(m30_ImpermeabilityGroup_st, df = df, m = m,  X = X, x = x)

plot_stglm(out_p, ylim, main, xlab, ylab)
ggplot_stglm(out_p, ylim, main, xlab, ylab)

out_cr <- pool_stglm_contrast(m30_ImpermeabilityGroup_st, df = df, m = m,  X = X, x = x, r= r)
plot_stglm_contrast(out_cr, ylim, main, xlab, ylab)


# stand living ------------------------------------------------------------
# Part of pwi
# Personal Wellbeing Index
# Your health.
# Your standard of living.
# Your future security.
# Your personal relationships.


Standard.Living_st <- as.formula(paste("Standard.Living_lead2ord_z~ bs(Hours.Work_lead1_10) +",
                                       paste(baselinevars,
                                             collapse = "+")))

## fit
fit_Standard.Living_st = function(formula) {
  with(w_mt, glm(
    as.formula(paste("Standard.Living_lead2ord_z~ bs(Hours.Work_lead1_10) +",
                     paste(baselinevars,
                           collapse = "+")))
  ))
}

# pooled model
m31_Standard.Living_st <- fit_Standard.Living_st()
main = "Standard Living"
ylab = "Standard Living (SD)"
out_p <- pool_stglm(m31_Standard.Living_st, df = df, m = m,  X = X, x = x)

plot_stglm(out_p, ylim, main, xlab, ylab)
ggplot_stglm(out_p, ylim, main, xlab, ylab)

out_cr <- pool_stglm_contrast(m31_Standard.Living_st, df = df, m = m,  X = X, x = x, r= r)
plot_stglm_contrast(out_cr, ylim, main, xlab, ylab)


# future security ---------------------------------------------------------

Your.Future.Security_st <- as.formula(paste("Your.Future.Security_lead2_z~ bs(Hours.Work_lead1_10) +",
                                            paste(baselinevars,
                                                  collapse = "+")))
## fit
fit_Your.Future.Security = function(formula) {
  with(w_mt, glm(
    as.formula(paste("Your.Future.Security_lead2_z~ bs(Hours.Work_lead1_10) +",
                     paste(baselinevars,
                           collapse = "+")))
  ))
}

# pooled model
m32_Your.Future.Security_st <- fit_Your.Future.Security()
main = "Your Future Security"
ylab = "Your Future Security (SD)"
out_p <- pool_stglm(m32_Your.Future.Security_st, df = df, m = m,  X = X, x = x)

plot_stglm(out_p, ylim, main, xlab, ylab)
ggplot_stglm(out_p, ylim, main, xlab, ylab)

out_cr <- pool_stglm_contrast(m32_Your.Future.Security_st, df = df, m = m,  X = X, x = x, r= r)
plot_stglm_contrast(out_cr, ylim, main, xlab, ylab)




# your health -------------------------------------------------------------

Your.Health_st <- as.formula(paste("Your.Health_lead2_z~ bs(Hours.Work_lead1_10) +",
                                   paste(baselinevars,
                                         collapse = "+")))

## fit
fit_Your.Health_st= function(formula) {
  with(w_mt, glm(
    as.formula(paste("Your.Health_lead2_z~ bs(Hours.Work_lead1_10) +",
                     paste(baselinevars,
                           collapse = "+")))
  ))
}

# pooled model
m33_Your.Health_st <- fit_Your.Health_st()
main = "Your Health"
ylab = "Your Health (SD)"
out_p <- pool_stglm(m33_Your.Health_st, df = df, m = m,  X = X, x = x)

plot_stglm(out_p, ylim, main, xlab, ylab)
ggplot_stglm(out_p, ylim, main, xlab, ylab)

out_cr <- pool_stglm_contrast(m33_Your.Health_st, df = df, m = m,  X = X, x = x, r= 3)
plot_stglm_contrast(out_cr, ylim, main, xlab, ylab)


# personal relationships --------------------------------------------------

Your.Personal.Relationships_st <- as.formula(paste("Your.Personal.Relationships_lead2ord_z~ bs(Hours.Work_lead1_10) +",  paste(baselinevars,collapse = "+")))

## fit
fit_Your.Personal.Relationships_st= function(formula) {
  with(w_mt, glm(
    as.formula(paste("Your.Personal.Relationships_lead2ord_z~ bs(Hours.Work_lead1_10) +",  paste(baselinevars,collapse = "+")))
  ))
}

# pooled model
m34_Your.Personal.Relationships <- fit_Your.Personal.Relationships_st()
main = "Your Personal Relationships"
ylab = "Your Personal Relationships (SD)"
out_p <- pool_stglm(m34_Your.Personal.Relationships, df = df, m = m,  X = X, x = x)

plot_stglm(out_p, ylim, main, xlab, ylab)
ggplot_stglm(out_p, ylim, main, xlab, ylab)

out_cr <- pool_stglm_contrast(m34_Your.Personal.Relationships, df = df, m = m,  X = X, x = x, r= 3)
plot_stglm_contrast(out_cr, ylim, main, xlab, ylab)

# Work-life balance -------------------------------------------------------

Emp.WorkLifeBalance_st <- as.formula(paste("Emp.WorkLifeBalance_lead2_z~ bs(Hours.Work_lead1_10) +",
                                           paste(baselinevars,
                                                 collapse = "+")))
#fit
fit_Emp.WorkLifeBalance_st= function(formula) {
  with(w_mt, glm(
    as.formula(paste("Emp.WorkLifeBalance_lead2_z~ bs(Hours.Work_lead1_10) +",
                     paste(baselinevars,
                           collapse = "+")))
  ))
}

# pooled model
m35_Emp.WorkLifeBalance_st <- fit_Emp.WorkLifeBalance_st()
main = "Work Life Balance"
ylab = "Work Life Balance (SD)"
out_p <- pool_stglm(m35_Emp.WorkLifeBalance_st, df = df, m = m,  X = X, x = x)

plot_stglm(out_p, ylim = c(-.5,.5), main, xlab, ylab)
ggplot_stglm(out_p, ylim = c(-.5,.5), main, xlab, ylab)

out_cr <- pool_stglm_contrast(m35_Emp.WorkLifeBalance_st, df = df, m = m,  X = X, x = x, r= 3)
plot_stglm_contrast(out_cr, ylim = c(-.6,.6), main, xlab, ylab)



# life sat ----------------------------------------------------------------
# Satisfaction with life
# I am satisfied with my life.
# In most ways my life is close to ideal.

## fit
fit_LIFESAT_st = function(formula) {
  with(w_mt, glm(
    as.formula(paste("LIFESAT_lead2_z~ bs(Hours.Work_lead1_10) +",
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
fit_NZSEI13_st = function(formula) {
  with(w_mt, glm(
    as.formula(paste("NZSEI13_lead2_10 ~ bs(Hours.Work_lead1_10) +",
                     paste(baselinevars,
                           collapse = "+")))
  ))
}
# pooled model
m38_NZSEI13  <- fit_NZSEI13_st()

main = "Life Satisfaction"
ylab = "Life Satisfaction (SD)"
out_p <- pool_stglm(m38_NZSEI13, df = df, m = m,  X = X, x = x)

plot_stglm(out_p, ylim = ylim, main, xlab, ylab)
ggplot_stglm(out_p, ylim = ylim, main, xlab, ylab)

out_cr <- pool_stglm_contrast(m38_NZSEI13, df = df, m = m,  X = X, x = x, r= r)
ggplot_stglm_contrast(out_cr, ylim, main, xlab, ylab)

# increase
round( EValue::evalues.OLS(  , se =    , sd = 1, delta = 3, true = 0), 3)
# decrease
round( EValue::evalues.OLS(   , se =   , sd = 1, delta = 3, true = 0), 3)



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

# standard living ---------------------------------------------------------


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
bal.tab(models_st)


ctrim_st <- trim(models_st, at = .999)
bal.tab(ctrim_st)
summary(ctrim_st)


# iptw models  income -------------------------------------------------------------
# no need to trim

Hours.Work_lead1_10

out <- with(ctrim_st, glm( HLTH.BMI_lead2_z ~ income_log_lead1_z ))
out

output <- pool(out, dfcom = NULL)
summary(output, conf.int = TRUE)
round( EValue::evalues.OLS(0.02376129 , se = 0.009437003, sd = 1, delta = 2, true = 0), 3)
round( EValue::evalues.RR(, lo =  , hi =, true = 1), 3)

