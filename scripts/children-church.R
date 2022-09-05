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
    EthCat,
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
    retired,
    semiretired,
    BornNZ,
    KESSLER6sum,
    Smoker,
    ChildrenNum,
    BELONG,
    SUPPORT,
    # Volunteers,
    Hours.Work,
    HLTH.SleepHours,
    HLTH.Disability,
    Hours.Exercise,
    LIFEMEANING,
    LIFESAT,
    PWI,
    SFHEALTH,
    SELF.ESTEEM,
    Respect.Self,
    #  GenCohort,
    #  Respect.Self,
    Emp.WorkLifeBalance,
    Alcohol.Frequency,
    Alcohol.Intensity,
    HLTH.BMI,
    # GenCohort,
    # Euro,
    partnerlost_job,
    lost_job,
    #began_relationship,
    #Env.SacWilling,
    #Env.SacMade,
    PERFECTIONISM,
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
#    Volunteers = if_else(HoursCharity == 1, 1, 0),
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
  ) %>%
  arrange(Id, Wave)  %>% #
  dplyr::mutate(Church_lead1 = lead(Church, n = 1)) %>%
  #dplyr::mutate(Church_lead1 = lead(Church, n = 1)) %>%  Your.Future.Security
  # inc_prop = (income_log / (income_log_lead1) - 1),
  dplyr::mutate(across(
    c(
      ChildrenNum
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
 # dplyr::filter(Religious == 1) %>%
  #dplyr::filter(!is.na(Standard.Living) )%>%
  # dplyr::filter(!is.na(Standard.Living_lead1) )%>%
  #  dplyr::filter(semiretired_lead1 != 1) %>%  #needed for the intervention
  dplyr::select(-c(
    Religion.Church2,
    # EthCat,
    Religious,
  #  Respect.Self_lead2,
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



table1::table1(~Church + NZdep| Wave , data = df_cr, overall = FALSE)#11953


# Filtering retirement -- consistency and positivity assumptions
# number of ids
N <- length(unique(df_cr$Id))
N  #33137

# inspect data
skim(df_cr)


# mice model  -------------------------------------------------------------
library(mice)

mice_cr_children <- df_cr %>%
  dplyr::select(-c( Wave, Id))  # won't otherwise run

hist(mice_cr_children$SDO)
library(naniar)
naniar::gg_miss_var(mice_cr_children)
vis_miss(mice_cr_children,
         warn_large_data = FALSE)

# any colinear vars?
mice:::find.collinear(mice_cr_children)

# impute
cr_mice <- mice::mice(mice_cr_children,  seed = 0, m = 10)

# save
saveh(cr_mice, "mice_cr_children")

# read
cr_mice <- readh("mice_cr_children")

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
skimr::skim(mf)

# create variables in z score
ml <- ml %>%
  dplyr::mutate(newkids = ChildrenNum_lead2 - ChildrenNum) %>%
  dplyr::mutate(childborn = ifelse(newkids > 1, 1, 0)) %>%
  # dplyr::mutate(KESSLER6sum_lead2 = round(as.integer(KESSLER6sum_lead2, 0)))%>%
  # dplyr::mutate(Alcohol.Intensity_lead2 = round(Alcohol.Intensity_lead2, 0))%>%
  # dplyr::mutate(CharityDonate_lead2 = round(CharityDonate_lead2, 0))%>%
  # dplyr::mutate(Hours.Exercise_lead2 = round(Hours.Exercise_lead2, 0))%>%
  #  dplyr::mutate(Hours.Exercise_lead2_log = log(Hours.Exercise_lead2 + 1))%>%
  # plyr::mutate(Alcohol.Intensity = round(Alcohol.Intensity, 0))%>%
  # dplyr::mutate(CharityDonate = round(CharityDonate, 0))%>%
  dplyr::mutate(Hours.Exercise = round(Hours.Exercise, 0))%>%
  # dplyr::mutate(CharityDonate_log_lead2 = log(CharityDonate_lead2+1))%>%
  # dplyr::mutate(Alcohol.Intensity_log_lead2 = log(Alcohol.Intensity_lead2+1))%>%
  # dplyr::mutate(Exercise_log_lead2 = log(Hours.Exercise_lead2+1))%>%
  # dplyr::mutate(CharityDonate_log = log(CharityDonate+1))%>%
  dplyr::mutate(Alcohol.Intensity_log = log(Alcohol.Intensity+1))%>%
  # dplyr::mutate(Rumination_lead2ord = as.integer(round(Rumination_lead2, digits = 0) + 1)) %>%  # needs to start at 1
  # dplyr::mutate(SUPPORT_lead2ord = as.integer(round(SUPPORT_lead2, digits = 0) )) %>%
  # dplyr::mutate(PERFECTIONISM_lead2ord = as.integer(round(PERFECTIONISM_lead2, digits = 0) )) %>%
  # dplyr::mutate(VENGEFUL.RUMIN_lead2ord = as.integer(round(VENGEFUL.RUMIN_lead2, digits = 0) )) %>%
  # dplyr::mutate(Standard.Living_lead2ord = as.integer(round(Standard.Living_lead2, digits = 0) )) %>%
  # dplyr::mutate(Your.Personal.Relationships_lead2ord = as.integer(round(Your.Personal.Relationships_lead2, digits = 0) +1)) %>%
  # dplyr::mutate(LIFEMEANING_lead2ord = as.integer(round(LIFEMEANING_lead2, digits = 0) )) %>%
  # dplyr::mutate(HLTH.Fatigue_lead2ord = as.integer(round(HLTH.Fatigue_lead2, digits = 0)+1 )) %>%
  dplyr::mutate(Hours.Exercise_log = log(Hours.Exercise+1))%>%
  # dplyr::mutate(Alcohol.Frequency_lead2ord = as.integer(round(Alcohol.Frequency_lead2, 0)+1))%>%
  # dplyr::mutate(LIFESAT_lead2ord = as.integer(round(LIFESAT_lead2, digits = 0) )) %>%
  # dplyr::mutate(alcohol_bin2 = if_else( Alcohol.Frequency > 3, 1, 0))%>%
  # dplyr::mutate(alcohol_bin = if_else( Alcohol.Frequency > 2, 1, 0))%>%
  # # dplyr::mutate(Hours.Work_lead1_10 =  as.integer(Hours.Work_lead1/10))%>%
  # # dplyr::mutate(Hours.Work_lead1_sqrt =  as.integer(sqrt(Hours.Work_lead1)))%>%
  # dplyr::mutate(NZSEI13_10 =  NZSEI13/10)%>%
  # dplyr::mutate(NZSEI13_lead2_10 =  as.integer(NZSEI13_lead2/10))%>%
dplyr::mutate(EthCat = factor(EthCat-1)) |>
  dplyr::mutate(across(where(is.numeric), ~ scale(.x), .names = "{col}_z")) %>%
  select(-c(.imp_z, .id_z))%>%
  dplyr::mutate(id = as.factor(rep(1:N, 11)))# needed for g-comp# Respect for Self is fully missing



# for models wihout looping (not advised)

mf <- mf %>%
  dplyr::mutate(newkids = ChildrenNum_lead2 - ChildrenNum) %>%
  dplyr::mutate(childborn = ifelse(newkids > 1, 1, 0) )%>%
  # dplyr::mutate(KESSLER6sum_lead2 = round(as.integer(KESSLER6sum_lead2, 0)))%>%
  # dplyr::mutate(Alcohol.Intensity_lead2 = round(Alcohol.Intensity_lead2, 0))%>%
  # dplyr::mutate(CharityDonate_lead2 = round(CharityDonate_lead2, 0))%>%
  # dplyr::mutate(Hours.Exercise_lead2 = round(Hours.Exercise_lead2, 0))%>%
 #  dplyr::mutate(Hours.Exercise_lead2_log = log(Hours.Exercise_lead2 + 1))%>%
  # plyr::mutate(Alcohol.Intensity = round(Alcohol.Intensity, 0))%>%
  # dplyr::mutate(CharityDonate = round(CharityDonate, 0))%>%
   dplyr::mutate(Hours.Exercise = round(Hours.Exercise, 0))%>%
  # dplyr::mutate(CharityDonate_log_lead2 = log(CharityDonate_lead2+1))%>%
 # dplyr::mutate(Alcohol.Intensity_log_lead2 = log(Alcohol.Intensity_lead2+1))%>%
  # dplyr::mutate(Exercise_log_lead2 = log(Hours.Exercise_lead2+1))%>%
  # dplyr::mutate(CharityDonate_log = log(CharityDonate+1))%>%
  dplyr::mutate(Alcohol.Intensity_log = log(Alcohol.Intensity+1))%>%
  # dplyr::mutate(Rumination_lead2ord = as.integer(round(Rumination_lead2, digits = 0) + 1)) %>%  # needs to start at 1
  # dplyr::mutate(SUPPORT_lead2ord = as.integer(round(SUPPORT_lead2, digits = 0) )) %>%
  # dplyr::mutate(PERFECTIONISM_lead2ord = as.integer(round(PERFECTIONISM_lead2, digits = 0) )) %>%
  # dplyr::mutate(VENGEFUL.RUMIN_lead2ord = as.integer(round(VENGEFUL.RUMIN_lead2, digits = 0) )) %>%
  # dplyr::mutate(Standard.Living_lead2ord = as.integer(round(Standard.Living_lead2, digits = 0) )) %>%
  # dplyr::mutate(Your.Personal.Relationships_lead2ord = as.integer(round(Your.Personal.Relationships_lead2, digits = 0) +1)) %>%
  # dplyr::mutate(LIFEMEANING_lead2ord = as.integer(round(LIFEMEANING_lead2, digits = 0) )) %>%
  # dplyr::mutate(HLTH.Fatigue_lead2ord = as.integer(round(HLTH.Fatigue_lead2, digits = 0)+1 )) %>%
   dplyr::mutate(Hours.Exercise_log = log(Hours.Exercise+1))%>%
  # dplyr::mutate(Alcohol.Frequency_lead2ord = as.integer(round(Alcohol.Frequency_lead2, 0)+1))%>%
  # dplyr::mutate(LIFESAT_lead2ord = as.integer(round(LIFESAT_lead2, digits = 0) )) %>%
  # dplyr::mutate(alcohol_bin2 = if_else( Alcohol.Frequency > 3, 1, 0))%>%
  # dplyr::mutate(alcohol_bin = if_else( Alcohol.Frequency > 2, 1, 0))%>%
  # # dplyr::mutate(Hours.Work_lead1_10 =  as.integer(Hours.Work_lead1/10))%>%
  # # dplyr::mutate(Hours.Work_lead1_sqrt =  as.integer(sqrt(Hours.Work_lead1)))%>%
  # dplyr::mutate(NZSEI13_10 =  NZSEI13/10)%>%
  # dplyr::mutate(NZSEI13_lead2_10 =  as.integer(NZSEI13_lead2/10))%>%
dplyr::mutate(EthCat = factor(EthCat-1)) |>
   dplyr::mutate(across(where(is.numeric), ~ scale(.x), .names = "{col}_z")) %>%
  select(-c(.imp_z, .id_z)) %>%
  dplyr::mutate(id = as.factor(rep(1:N, 10)))# needed for g-comp


# Get data into shape
mf <- mf %>% mutate_if(is.matrix, as.vector)
ml <- ml %>% mutate_if(is.matrix, as.vector)

ml <- mice::as.mids(ml)

# saveh(ml, "churchl_cr")
# saveh(mf, "churchf_cr")

table(mf$ChildrenNum_lead2)

###### READ THIS DATA IN   #########
# ml <- readh("churchl_cr")
# mf <- readh("churchf")


# model equations ---------------------------------------------------------

head(mf)

baselinevars = c("AGREEABLENESS_z","CONSCIENTIOUSNESS_z","EXTRAVERSION_z","HONESTY_HUMILITY_z","NEUROTICISM_z","OPENNESS_z","Age_z","Alcohol.Frequency_z","Alcohol.Intensity_log_z","EthCat","Believe.God_z","Believe.Spirit_z","BELONG_z","ChildrenNum_z","Church_z", "community","Edu_z","Employed_z", "HomeOwner_z","Hours.Exercise_log_z","Hours.Work_z","HLTH.BMI_z", "HLTH.Disability_z", "HLTH.SleepHours_z","KESSLER6sum_z", "LIFEMEANING_z", "LIFESAT_z", "lost_job_z", "Male_z", "NZdep_z", "NZSEI13_z","Parent_z","Partner_z","partnerlost_job_z", "PERFECTIONISM_z",  "Pol.Orient_z", "PWI_z", "Relid_z", "retired", "RWA_z","SDO_z", "semiretired", "SFHEALTH_z","Smoker_z", "Spiritual.Identification_z", "SUPPORT_z","Urban_z")


# functions ---------------------------------------------------------------

# see "funs.R"

## Also use
round( EValue::evalues.OLS( , se = , sd = 1, delta = 2, true = 0), 3)
round( EValue::evalues.RR( , lo =  , hi =, true = 1), 4)


# set up vars -------------------------------------------------------------

ylim <- c(.9,1.1)
xlab <- "New Children"
df <-  ml
m = 10
X = "Church_lead1"
x =  0:8
xlab = "Church_lead1"
c = c(0,4)
# reference level
r = 0



# Children ---------------------------------------------------------------------

# prepare
out_f = function(formula) {
  with(ml, glm(as.formula(paste("ChildrenNum_lead2 ~ bs(Church_lead1) + ",
                                paste(baselinevars,
                                      collapse = "+"))), family = "poisson") )
}
library(cmdstanr)


## WEAKER VERSIONS
# labels
main = "Children"
ylab = "Children (count)"

# clean oven
rm(out_m)
rm(pool_m)
rm(out_ct)

# bake
out_m <- out_f()
out_m

## contrasts
out_ct <- pool_stglm_contrast_ratio(out_m, df = df, m = m,  X = X, x = c, r= r)
out_ct
# graph of contrasts
ggplot_stglm_contrast(out_ct, ylim = c(.9,1.2), main, xlab, ylab)

# table
out_ct %>%
  kbl(format = "markdown", booktabs = T, digits = 3)

# out_ct %>%
#   kbl(format = "latex", booktabs = T, digits = 3)

# evalues
round( EValue::evalues.OLS( , se = 0.018, sd = 1, delta = 4, true = 0), 3)
round( EValue::evalues.RR( 1.037, lo =  0.994, hi = 1.08, true = 1), 4)


# g-computation
pool_m <- pool_stglm_contrast(out_m, df = df, m = m, x = x,  X = X, r = r)
pool_m %>%
  kbl(format = "markdown", booktabs = T, digits = 3)

# graph
ggplot_stglm(pool_m, ylim = c(.9,2.1), main = main, xlab = xlab, ylab = ylab, c=c)


