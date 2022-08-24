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
options(scipen = 999)

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

table1::table1( ~  HLTH.SleepHours    | Wave , data = dff)


dff %>%
  filter(Wave == 2016 &  YearMeasured == 1) %>%
  n_distinct("Id")


# table for participant N
dc <- dat %>%
  arrange(Id, Wave) %>%
  dplyr::mutate(Religion.CongregationSize = ifelse(Religion.Church == 0, 0,  Religion.CongregationSize)) |> #handle missingness
  dplyr::mutate(Euro = if_else(EthCat == 1, 1, 0)) %>%
  dplyr::mutate(Male = ifelse(GendAll == 1, 1, 0)) %>%
  dplyr::filter((Wave == 2016  & YearMeasured  == 1) |
                  (Wave == 2017  & YearMeasured  == 1) |
                  (Wave == 2018 &
                     YearMeasured != -1)
  )  %>% # Eligibility criteria
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
#dplyr::filter(Religious == 1) |>
  arrange(Id, Wave)

length(unique(dc$Id)) #15786
# check n

table1::table1( ~ Religion.CongregationSize + NZdep |
                  Wave ,
                data = dc,
                overall = FALSE)





# increasing rate
dat %>%
  group_by(Wave) %>%
  summarise(mean(HLTH.Disability, na.rm = TRUE))

# Do you have a health condition or disability that limits you, and that has lasted for 6+ months?


dcc <- dc |>
  select(
    Id,
    YearMeasured,
    Wave,
    Age,
    Male,
    Edu,
    Urban,
    EthCat,
    AGREEABLENESS,
    CONSCIENTIOUSNESS,
    EXTRAVERSION,
    HONESTY_HUMILITY,
    NEUROTICISM,
    OPENNESS,
    began_relationship,
    BELONG,
    Believe.Spirit,
    Believe.God,
    Bodysat,
    BornNZ,
    CharityDonate,
    ChildrenNum,
    Edu,
    #  Emp.JobSecure,
   # Euro,
   # EthCat,
    Employed,
    # Emp.WorkLifeBalance,
    #  GenCohort,
    # GRATITUDE,
    HLTH.BMI,
    HLTH.Fatigue,
    HLTH.Disability,
    HLTH.SleepHours,
    #  HomeOwner,
    Household.INC,
    HoursCharity,
    Hours.Exercise,
    Hours.Work,
    # ImpermeabilityGroup,
    KESSLER6sum,
    # LIFEMEANING,
    LIFESAT,
    lost_job,
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
   # Religious,
    Respect.Self,
    retired,
    RWA,
    #  Rumination,
    SDO,
    semiretired,
    SELF.CONTROL,
    SELF.ESTEEM,
    #   SexualSatisfaction,
    SFHEALTH,
    Smoker,
    #Spiritual.Identification,
    Standard.Living,
    SUPPORT,
    SWB.SoC01,
    Urban,
    #  VENGEFUL.RUMIN,
    Your.Health,
    Your.Future.Security,
    Your.Personal.Relationships,
    Alcohol.Frequency,
    Alcohol.Intensity,
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
  dplyr::mutate(
    Religion.CongregationSize_lead1 = lead(Religion.CongregationSize, n = 1),
    NZdep_lead1  = lead(NZdep, n = 1)
  ) %>%
  dplyr::mutate(SUPPORT_lead1 = lead(SUPPORT, n = 1)) %>%
  # dplyr::mutate(Standard.Living_lead1 = lead(Standard.Living, n = 1)) %>%
  #dplyr::mutate(Church_lead1 = lead(Church, n = 1)) %>%  Your.Future.Security
  # inc_prop = (income_log / (income_log_lead1) - 1),
  dplyr::mutate(across(
    c(
      Believe.Spirit,
      Believe.God,
      BELONG,
      Bodysat,
      CharityDonate,
      Employed,
      #  Emp.WorkLifeBalance,
      #  GenCohort,
      # GRATITUDE,
      HLTH.BMI,
      HLTH.Fatigue,
      HLTH.Disability,
      HLTH.SleepHours,
      #     HomeOwner,
      Household.INC,
      HoursCharity,
      Hours.Exercise,
      Hours.Work,
      #  ImpermeabilityGroup,
      KESSLER6sum,
      # LIFEMEANING,
      LIFESAT,
      #lost_job,
      NWI,
      NZdep,
      NZSEI13,
    #  Parent,
    #  partnerlost_job,
      # PERFECTIONISM,
      #  PermeabilityIndividual,
      #  POWERDEPENDENCE1,
      #  POWERDEPENDENCE2,
      Relid,
      Religion.Church,
      Religion.Prayer,
      Religion.Scripture,
      Respect.Self,
      #  Rumination,
      SELF.CONTROL,
      SELF.ESTEEM,
      #SexualSatisfaction,
      SFHEALTH,
      Smoker,
    #  Spiritual.Identification,
      Standard.Living,
      SUPPORT,
      community,
      # SWB.SoC01,
    #  Urban,
      #  VENGEFUL.RUMIN,
      Your.Health,
      Your.Future.Security,
      Your.Personal.Relationships,
      Alcohol.Frequency,
      Alcohol.Intensity,
    ),
    ~ lead(.x, n = 2),
    .names = "{col}_lead2"
  )) %>% # make leads
  dplyr::filter(Wave == 2016) %>%
  dplyr::filter(!is.na(Religion.CongregationSize)) %>%
  dplyr::filter(!is.na(Religion.CongregationSize_lead1)) %>%
  dplyr::filter(Relid > 0) %>%
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
dplyr::select(-c(
  # EthCat,
  HoursCharity,
  Respect.Self_lead2,
 # Household.INC,
  #  org2018,
  #  not_euro,
  #  not_euro_lead2,
  # hold18,
  #   Euro,
  #  Emp.WorkLifeBalance,
  YearMeasured,
#  Religious,
  NZdep,
  # HLTH.Disability_lead1,
  # org2019,
  # hold19,
  # retired,

  #  retired_lead1,
  # semiretired,
  #  semiretired_lead1
)) %>%
  rename(NZdep = NZdep_lead1) |>   ## much lower missingness
  mutate(across(where(is.double), as.numeric)) |> data.frame() %>%
  arrange(Id)


# inspect data
skim(dcc)
dev.off()
table(!is.na(dcc$Religion.CongregationSize))

# number of ids
N <- length(unique(dcc$Id))
N  # 5452

# mice model  -------------------------------------------------------------
library(mice)

mice_cc <- dcc %>%
  dplyr::select(-c(Wave, Id))  # won't otherwise run

library(naniar)
naniar::gg_miss_var(mice_cc)
# vis_miss(mice_cc,
#          warn_large_data = FALSE)

# any colinear vars?
mice:::find.collinear(mice_cc)

# impute
mice_cc <- mice::mice(mice_cc,  seed = 0, m = 10)

# save
saveh(mice_cc, "mice_cc")
# checks
outlist2 <-
  row.names(mice_cc)[mice_cc$outflux < 0.5]
length(outlist2)

# checks
head(mice_cc$loggedEvents, 10)

# read
mice_cc <- readh("mice_cc")

# we create two completed data sets -- the one without the missing data will be useful for
# determing causal contrasts -- which we'll describe below.

cc_l <- mice::complete(mice_cc, "long", inc = TRUE)



# inspect data -- what do we care about?  Note that the moderate distress category doesn't look useful
skimr::skim(cc_l)
# create variables in z score
cc_l2 <- cc_l %>%
  dplyr::mutate(EthCat = as.factor(EthCat)) |>
  dplyr::mutate(Volunteers_lead2 = if_else(HoursCharity_lead2 >0, 1, 0)) |>
  dplyr::mutate(income_log = log(Household.INC + 1)) |>
  dplyr::mutate(income_log_lead2 = log(Household.INC_lead2 + 1)) |>
  # dplyr::mutate(newkids = ChildrenNum_lead2 - ChildrenNum) %>%
  dplyr::mutate(KESSLER6sum_lead2 = round(as.integer(KESSLER6sum_lead2, 0))) %>%
  dplyr::mutate(CharityDonate_lead2 = round(CharityDonate_lead2, 0)) %>%
  plyr::mutate(Alcohol.Intensity = round(Alcohol.Intensity, 0)) %>%
  dplyr::mutate(CharityDonate = round(CharityDonate, 0)) %>%
  dplyr::mutate(Hours.Exercise = round(Hours.Exercise, 0)) %>%
  dplyr::mutate(CharityDonate_log_lead2 = log(CharityDonate_lead2 + 1)) %>%
  dplyr::mutate(CharityDonate_log = log(CharityDonate + 1)) %>%
  dplyr::mutate(SUPPORT_lead2ord = as.integer(round(SUPPORT_lead2, digits = 0))) %>%
  dplyr::mutate(Hours.Exercise_log = log(Hours.Exercise + 1)) %>%
  dplyr::mutate(Hours.Exercise_log_lead2 = log(Hours.Exercise_lead2 + 1)) %>%
  dplyr::mutate(LIFESAT_lead2ord = as.integer(round(LIFESAT_lead2, digits = 0))) %>%
  dplyr::mutate(alcohol_bin2 = if_else(Alcohol.Frequency > 3, 1, 0)) %>%
  dplyr::mutate(alcohol_bin = if_else(Alcohol.Frequency > 2, 1, 0)) %>%
  dplyr::mutate(Hours.Work_10 =  Hours.Work / 10) %>%
  dplyr::mutate(Religion.Prayer_log =  log(Religion.Prayer + 1)) %>%
  dplyr::mutate(Religion.Prayer_lead2_log =  log(Religion.Prayer_lead2 +
                                                   1)) %>%
  dplyr::mutate(Religion.Church_lead2_log =  log(Religion.Church_lead2 +
                                                   1)) %>%
  dplyr::mutate(Religion.Church_log =  log(Religion.Church +
                                                   1)) %>%
  dplyr::mutate(Religion.Scripture_log =  log(Religion.Scripture + 1)) %>%
  dplyr::mutate(Religion.Scripture_lead2_log =  log(Religion.Scripture_lead2 +
                                                      1)) %>%
  dplyr::mutate(Religion.CongregationSize_log =  log(Religion.CongregationSize + 1)) %>%
  dplyr::mutate(Religion.CongregationSize_lead1_log =  log(Religion.CongregationSize_lead1 + 1)) %>%
  dplyr::mutate(Religion.CongregationSize_lead1_100 =  Religion.CongregationSize_lead1 /
                  100) %>%
  dplyr::mutate(Religion.CongregationSize_dunbar1 =  if_else(
    Religion.CongregationSize == 0,
    0,
    if_else(
      Religion.CongregationSize > 0 &
        Religion.CongregationSize < 151,
      1,
      2
    )
  )) %>%
  dplyr::mutate(
    Religion.CongregationSize_lead1_dunbar1 =  if_else(
      Religion.CongregationSize_lead1 == 0,
      0,
      if_else(
        Religion.CongregationSize_lead1 > 0 &
          Religion.CongregationSize_lead1 < 151,
        1,
        2
      )
    )
  ) %>%
  dplyr::mutate(NZSEI13_10 =  NZSEI13 / 10) %>%
  dplyr::mutate(NZSEI13_10 =  NZSEI13/10)%>%
  dplyr::mutate(NZSEI13_lead2_10 =  as.integer(NZSEI13_lead2/10))%>%
  dplyr::mutate(id = as.factor(rep(1:N, 11))) |> # needed for g-comp
  dplyr::group_by(id) |> mutate(PWI = mean(
    c(
      Your.Future.Security,
      Your.Personal.Relationships,
      Your.Health,
      Standard.Living
    ),
    na.rm = TRUE
  )) |>
  dplyr::mutate(PWI_lead2 = mean(
    c(
      Your.Future.Security_lead2,
      Your.Personal.Relationships_lead2,
      Your.Health_lead2,
      Standard.Living_lead2
    ),
    na.rm = TRUE
  )) |>
  ungroup() |>
  dplyr::mutate(across(where(is.numeric), ~ scale(.x), .names = "{col}_z")) %>%
  select(-c(.imp_z, .id_z)) %>%
  dplyr::mutate(EthCat = as.factor(EthCat)) |>
  dplyr::mutate(Religion.CongregationSize_dunbar1 = as.factor(Religion.CongregationSize_dunbar1)) |>
  dplyr::mutate(Religion.CongregationSize_lead1_dunbar1 = as.factor(Religion.CongregationSize_lead1_dunbar1))

# # Get data into shape
cc_l2 <- cc_l2 %>% mutate_if(is.matrix, as.vector)
# cc3l <- cc_l %>% mutate_if(is.matrix, as.vector)

ccu <- mice::as.mids(cc_l2)
ccf <- mice::complete(ccu, "long", inc = F)


saveh(ccf, "ccf")
saveh(ccu, "ccu")


skimr::skim(ccf)

###### READ THIS DATA IN   #########
ccf <- readh("ccf")
ccu <- readh("ccu")


# model equations ---------------------------------------------------------
baselinevars = c(
  "Religion.CongregationSize_log",
  "Age_z",
  "Male_z",
  "Edu_z",
  "Urban_z",
  "EthCat",
  "AGREEABLENESS_z",
  "CONSCIENTIOUSNESS_z",
  "EXTRAVERSION_z",
  "HONESTY_HUMILITY_z",
  "NEUROTICISM_z",
  "OPENNESS_z",
  "began_relationship_z",
  "BELONG_z",
  "Believe.Spirit_z",
  "Believe.God_z",
  "Bodysat_z",
  "BornNZ_z",
  "CharityDonate_z",
  "ChildrenNum_z",
  "Edu_z",
  "Employed_z",
  "HLTH.BMI_z",
  "HLTH.Fatigue_z",
  "HLTH.Disability_z",
  "HLTH.SleepHours_z",
  "Household.INC_z",
  "Volunteers_z",
  "Hours.Exercise_log_z",
  "Hours.Work_10_z",
  "KESSLER6sum_z",
  "LIFESAT_z",
  "lost_job_z",
  "NWI_z",
  "NZdep_z",
  "NZSEI13_z",
  "Parent_z",
  "Partner_z",
  "partnerlost_job_z",
  "Pol.Orient_z",
  "Relid_z",
  "Religion.Church_log_z",
  "Religion.Prayer_z",
  "Religion.Scripture_z",
  "Respect.Self_z",
  "retired_z",
  "RWA_z",
  "SDO_z",
  "semiretired_z",
  "SELF.CONTROL_z",
  "SELF.ESTEEM_z",
  "SFHEALTH_z",
  "Smoker_z",
  "Standard.Living_z",
  "SUPPORT_z",
  "community_z",
  "Your.Health_z",
  "Your.Future.Security_z",
  "Your.Personal.Relationships_z",
  "Alcohol.Frequency_z",
  "Alcohol.Intensity_z"
)


# functions ---------------------------------------------------------------

# SET UP ----------------------------------------------------------

# ylimits
# ylim <- c(-.3,.3)
# ylim7<- c(-.7,.7)
# ylim <- c(-.5,.5)
ylim8 = c(-.2,.75)
# data
df <-  ccu
# n imputations
m = 10
ylim_contrast <- c(.6,2.5)
ylim_contrast_diff <- c(-6,2.5)


exp(5)
hist(ccf$Religion.CongregationSize_lead1_log)
# Scripture set up ---------------------------------------------------------------
#How many times did you pray in the last week?
X = "Religion.CongregationSize_lead1_log"
xlab = "Log Religious Congregation Size"
min= 0
max = 7
# baseline
r = 0
# focal contrast
f = 5

# range of estimates
x =  min:max

# for model functions
c = x #c(0,5)
# contrast for graphs -- absolute distance from baseline
p = 5
s = 1 # slot for contrast graph

delta = 5
# functions ---------------------------------------------------------------

## Also use
round( EValue::evalues.OLS( ESTIMATE_GOES_HERE, se = GOES_HERE, sd = 1, delta = delta, true = 0), 3)
round( EValue::evalues.RR( , lo =  , hi =, true = 1), 4)

# sd(df_a$Household.INC_lead1)
# mean(mf$income_log_lead1)
# exp(mean(mf$income_log_lead1))
#
# min((mf$income_log_lead1))
# sd((mf$income_log_lead1))
#
# exp(11.62 - (2 * .58))


# HEALTH  ------------------------------------------------------------------
#  functions
source(pull_path_funs)

# BMI ---------------------------------------------------------------------
out_f = function(formula) {
  with(df, glm(as.formula(
    paste(
      "HLTH.BMI_lead2_z ~ bs(Religion.CongregationSize_lead1_log) +",
      paste(baselinevars,
            collapse = "+")
    )
  )))
}
# labels
main = "BMI"
ylab = "BMI (SD)"
# clean oven
rm(out_m)
rm(out_ct)
# run model
out_m <- out_f()
summary(pool(out_m))

## contrasts
# using the stdGlm package which does g-computation
out_ct <- pool_stglm_contrast(out_m, df = df, m = 10,  X = X, x = c, r= r)
out_ct
out_ct %>%
  slice(6) |>
  kbl(digits = 3, "markdown")

# you can make an html table, for options see:
#https://cran.r-project.org/web/packages/kableExtra/vignettes/awesome_table_in_html.html

bmi_t <- out_ct %>%
  slice(1:8) |>
  tibble() |>
  rename(Contrast = row,
         Estimate = est,
         Std_error = se,
         CI_hi = ui,
         CI_lo = li) |>
  kbl(caption = main,
      digits = 3,
      "html") |>
   kable_styling() %>%
  row_spec(c(6), bold = T, color = "white", background = "dodgerblue") |>
  kable_minimal(full_width = F)

bmi_t
bmi_p<- ggplot_stglm(out_ct, ylim = ylim8, main, xlab, ylab, min = min, p=p, r= 1)
bmi_p
round( EValue::evalues.OLS( , se = , sd = 1, delta = delta, true = 0), 3)
round( EValue::evalues.RR( , lo =  , hi =, true = 1), 4)


# sf-health ---------------------------------------------------------------
# Short-Form Subjective Health Scale (General Health Perception Subscale)
# In general, would you say your health is...
# I seem to get sick a little easier than other people.
# I expect my health to get worse.

out_f = function(formula) {
  with(df, glm(as.formula(
    paste(
      "SFHEALTH_lead2_z~ bs(Religion.CongregationSize_lead1_log) +",
      paste(baselinevars,
            collapse = "+")
    )
  )))
}
# labels
main = "Short Form Health"
ylab = "SFHEALTH (SD)"
# clean oven
rm(out_m)
rm(out_ct)
# fit regression model
out_m <- out_f()
# g-computation - contrasts
out_ct <- pool_stglm_contrast(out_m, df = df, m = 10,  X = X, x = c, r= r)

#table
sfhealth_t<- out_ct %>%
  slice(1:8) |>
  tibble() |>
  rename(Contrast = row,
         Estimate = est,
         std_error = se,
         CI_hi = ui,
         CI_lo = li) |>
  kbl(caption = "This is my caption",
      digits = 3,
      "html") |>
  kable_styling() %>%
  row_spec(c(6), bold = T, color = "white", background = "dodgerblue") |>
  kable_minimal(full_width = F)
sfhealth_t

sfhealth_p <- ggplot_stglm(out_ct, ylim = ylim8, main, xlab, ylab, min = min, p=p, r= 1)
sfhealth_p
round( EValue::evalues.OLS( , se = , sd = 1, delta = delta, true = 0), 3)
round( EValue::evalues.RR( , lo =  , hi =, true = 1), 4)


# exercise ---------------------------------------------------------------
## fit for mice to work, don't ask why
out_f = function(formula) {
  with(df, glm(as.formula(
    paste(
      "Hours.Exercise_log_lead2~ bs(Religion.CongregationSize_lead1_log) +",
      paste(baselinevars, collapse = "+")
    )
  )))
}
# labels
main = "Log Weekly Hours Exercise (SD)"
ylab = "Log WeeklyHours Exercise (SD)"
# clean oven
rm(out_m)

rm(out_ct)
# bake
# fit regression model
out_m <- out_f()
# g-computation - contrasts
out_ct <- pool_stglm_contrast(out_m, df = df, m = 10,  X = X, x = c, r= r)
#table
exercise_t<- out_ct %>%
  slice(1:8) |>
  tibble() |>
  rename(Contrast = row,
         Estimate = est,
         std_error = se,
         CI_hi = ui,
         CI_lo = li) |>
  kbl(caption = main,
      digits = 3,
      "html") |>
  kable_styling() %>%
  row_spec(c(6), bold = T, color = "white", background = "dodgerblue") |>
  kable_minimal(full_width = F)
exercise_t
exercise_p<- ggplot_stglm(out_ct, ylim = ylim8, main, xlab, ylab, min = min, p=p, r= 1)
exercise_p
round( EValue::evalues.OLS( , se = , sd = 1, delta = delta, true = 0), 3)
round( EValue::evalues.RR( , lo =  , hi =, true = 1), 4)


# smoker ------------------------------------------------------------------
# fit
#Do you currently smoke?

out_f = function(formula) {
  with(df, glm(as.formula(
    paste(
      "Smoker_lead2 ~ bs(Religion.CongregationSize_lead1_log) +",
      paste(baselinevars,
            collapse = "+")
    )
  ), family = "poisson"))
}
main = "Smoking Rate"
ylab = "Smoking Rate"
# clean oven
rm(out_m)

rm(out_ct)
# bake
out_m <- out_f()
out_m
## contrasts
out_ct <- pool_stglm_contrast_ratio(out_m, df = df, m = m,  X = X, x = c, r= r)
# g-computation - contrasts
#table
smoker_t <- out_ct %>%
  slice(1:8) |>
  tibble() |>
  rename(Contrast = row,
         Estimate = est,
         std_error = se,
         CI_hi = ui,
         CI_lo = li) |>
  kbl(caption = main,
      digits = 3,
      "html") |>
  kable_styling() %>%
  row_spec(c(6), bold = T, color = "white", background = "dodgerblue") |>
  kable_minimal(full_width = F)
smoker_t
smoker_p <- ggplot_stglm(out_ct, ylim = c(0,2.9), main, xlab, ylab, min = min, p=p, r= 1) + expand_limits(x = 0, y = 0)
smoker_p
round( EValue::evalues.OLS( , se = , sd = 1, delta = delta, true = 0), 3)
round( EValue::evalues.RR( , lo =  , hi =, true = 1), 4)

# alcohol freq ------------------------------------------------------------
out_f = function(formula) {
  with(df, glm(as.formula(
    paste(
      "Alcohol.Frequency_lead2_z~ bs(Religion.CongregationSize_lead1_log) +",
      paste(baselinevars, collapse = "+")
    )
  )))
}
main = "Alcohol Frequency"
ylab = "Alcohol Frequency (SD)"
# clean oven
rm(out_m)

rm(out_ct)
# fit regression model
out_m <- out_f()
# g-computation - contrasts
out_ct <- pool_stglm_contrast(out_m, df = df, m = 10,  X = X, x = c, r= r)
#table
alcoholfreq_t <- out_ct %>%
  slice(1:8) |>
  tibble() |>
  rename(Contrast = row,
         Estimate = est,
         std_error = se,
         CI_hi = ui,
         CI_lo = li) |>
  kbl(caption = main,
      digits = 3,
      "html") |>
  kable_styling() %>%
  row_spec(c(6), bold = T, color = "white", background = "dodgerblue") |>
  kable_minimal(full_width = F)
alcoholfreq_t
alcoholfreq_p <- ggplot_stglm(out_ct, ylim = ylim8, main, xlab, ylab, min = min, p=p, r= 1)
alcoholfreq_p
round( EValue::evalues.OLS( , se = , sd = 1, delta = delta, true = 0), 3)
round( EValue::evalues.RR( , lo =  , hi =, true = 1), 4)

# Alcohol.Intensity ----------------------------------------------------------
# fit
out_f = function(formula) {
  with(df, glm(as.formula(
    paste(
      "Alcohol.Intensity_lead2_z~ bs(Religion.CongregationSize_lead1_log) +",
      paste(baselinevars,
            collapse = "+")
    )
  )))
}
main = "Alcohol Intensity"
ylab = "Alcohol Intensity (SD)"
# clean oven
rm(out_m)

rm(out_ct)
# fit regression model
out_m <- out_f()
# g-computation - contrasts
out_ct <- pool_stglm_contrast(out_m, df = df, m = 10,  X = X, x = c, r= r)
#table
alcoholintensity_t<- out_ct %>%
  slice(1:8) |>
  tibble() |>
  rename(Contrast = row,
         Estimate = est,
         std_error = se,
         CI_hi = ui,
         CI_lo = li) |>
  kbl(caption = main,
      digits = 3,
      "html") |>
  kable_styling() %>%
  row_spec(c(6), bold = T, color = "white", background = "dodgerblue") |>
  kable_minimal(full_width = F)
alcoholintensity_t

alcoholintensity_p<- ggplot_stglm(out_ct, ylim = ylim8, main, xlab, ylab, min = min, p=p, r= 1)
alcoholintensity_p
round( EValue::evalues.OLS( -0.055, se = 0.037, sd = 1, delta = delta, true = 0), 3) |>
  kbl(caption = main,
      digits = 3,
      "html") |>
  kable_styling() %>%
  kable_minimal(full_width = F)
round( EValue::evalues.RR( , lo =  , hi =, true = 1), 4)





# GRAPHS HEALTH -----------------------------------------------------------

health_plots <- bmi_p + sfhealth_p + exercise_p   +
  alcoholintensity_p + alcoholfreq_p + smoker_p + plot_annotation(title = "Causal effects of congregation size on health outcomes", tag_levels = "A") + plot_layout(guides = 'collect') #+ plot_layout(nrow = 3, byrow = T)
health_plots
ggsave(
  health_plots,
  path = here::here(here::here("figs", "congregation")),
  width = 16,
  height = 9,
  units = "in",
  filename = "health_plots.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 1200
)

dev.off()



##### EMBODIED #####


# HLTH.Sleep --------------------------------------------------------------
#
## fit
out_f = function(formula) {
  with(df, glm(as.formula(
    paste(
      "HLTH.SleepHours_lead2_z ~ bs(Religion.CongregationSize_lead1_log) +",
      paste(baselinevars, collapse = "+")
    )
  )))
}
main = "Hours Sleep (SD)"
ylab = "Hours Sleep (SD)"
# clean oven
rm(out_m)

rm(out_ct)
# fit regression model
out_m <- out_f()
# g-computation - contrasts
out_ct <- pool_stglm_contrast(out_m, df = df, m = 10,  X = X, x = c, r= r)
#table
sleep_t<-out_ct %>%
  slice(1:8) |>
  tibble() |>
  rename(Contrast = row,
         Estimate = est,
         std_error = se,
         CI_hi = ui,
         CI_lo = li) |>
  kbl(caption = main,
      digits = 3,
      "html") |>
  kable_styling() %>%
  row_spec(c(6), bold = T, color = "white", background = "dodgerblue") |>
  kable_minimal(full_width = F)
sleep_t
sleep_p<- ggplot_stglm(out_ct, ylim = ylim8, main, xlab, ylab, min = min, p=p, r= 1)
sleep_p
round( EValue::evalues.OLS( , se = , sd = 1, delta = delta, true = 0), 3)
round( EValue::evalues.RR( , lo =  , hi =, true = 1), 4)


# fatigue -----------------------------------------------------------------
out_f = function(formula) {
  with(df, glm(as.formula(
    paste(
      "HLTH.Fatigue_lead2_z ~ bs(Religion.CongregationSize_lead1_log) +",
      paste(baselinevars,
            collapse = "+")
    )
  )))
}
main = "Fatigue"
ylab = "Fatigue (SD)"
# clean oven
rm(out_m)

rm(out_ct)
# fit regression model
out_m <- out_f()
# g-computation - contrasts
out_ct <- pool_stglm_contrast(out_m, df = df, m = 10,  X = X, x = c, r= r)
#table
fatigue_t<- out_ct %>%
  slice(1:8) |>
  tibble() |>
  rename(Contrast = row,
         Estimate = est,
         std_error = se,
         CI_hi = ui,
         CI_lo = li) |>
  kbl(caption = main,
      digits = 3,
      "html") |>
  kable_styling() %>%
  row_spec(c(6), bold = T, color = "white", background = "dodgerblue") |>
  kable_minimal(full_width = F)
fatigue_t
fatigue_p <- ggplot_stglm(out_ct, ylim = ylim8, main, xlab, ylab, min = min, p=p, r= 1)
fatigue_p

round( EValue::evalues.OLS( , se = , sd = 1, delta = delta, true = 0), 3)
round( EValue::evalues.RR( , lo =  , hi =, true = 1), 4)

# body satisfaction -------------------------------------------------------
# fit
out_f = function(formula) {
  with(df, glm(as.formula(
    paste(
      "Bodysat_lead2_z~ bs(Religion.CongregationSize_lead1_log) +",
      paste(baselinevars,
            collapse = "+")
    )
  )))
}
# plots
main = "Body Satisfaction"
ylab = "Body Satisfaction (SD)"
# clean oven
rm(out_m)

rm(out_ct)
# fit regression model
out_m <- out_f()
# g-computation - contrasts
out_ct <- pool_stglm_contrast(out_m, df = df, m = 10,  X = X, x = c, r= r)
#table
bodysat_t<- out_ct %>%
  slice(1:8) |>
  tibble() |>
  rename(Contrast = row,
         Estimate = est,
         std_error = se,
         CI_hi = ui,
         CI_lo = li) |>
  kbl(caption = main,
      digits = 3,
      "html") |>
  kable_styling() %>%
  row_spec(c(6), bold = T, color = "white", background = "dodgerblue") |>
  kable_minimal(full_width = F)
bodysat_t
bodysat_p <- ggplot_stglm(out_ct, ylim = ylim8, main, xlab, ylab, min = min, p=p, r= 1)
bodysat_p
round( EValue::evalues.OLS( , se = , sd = 1, delta = delta, true = 0), 3)
round( EValue::evalues.RR( , lo =  , hi =, true = 1), 4)

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
  with(df, glm(as.formula(
    paste(
      "KESSLER6sum_lead2_z~ bs(Religion.CongregationSize_lead1_log) +",
      paste(baselinevars,
            collapse = "+")
    )
  )))
}
main = "Kessler 6 Distress"
ylab = "Kessler 6 Distress (SD)"
# clean oven
rm(out_m)

rm(out_ct)
# fit regression model
out_m <- out_f()
# g-computation - contrasts
out_ct <- pool_stglm_contrast(out_m, df = df, m = 10,  X = X, x = c, r= r)
#table
distress_t<- out_ct %>%
  slice(1:8) |>
  tibble() |>
  rename(Contrast = row,
         Estimate = est,
         std_error = se,
         CI_hi = ui,
         CI_lo = li) |>
  kbl(caption = main,
      digits = 3,
      "html") |>
  kable_styling() %>%
  row_spec(c(6), bold = T, color = "white", background = "dodgerblue") |>
  kable_minimal(full_width = F)
distress_t
distress_p<- ggplot_stglm(out_ct, ylim = ylim8, main, xlab, ylab, min = min, p=p, r= 1)
distress_p

round( EValue::evalues.OLS( , se = , sd = 1, delta = delta, true = 0), 3)
round( EValue::evalues.RR( , lo =  , hi =, true = 1), 4)


# GRAPHS EMBODIED  --------------------------------------------------------


embody_plots <- sleep_p + fatigue_p + bodysat_p + distress_p + plot_annotation(title = "Causal effects of congregation size on embodied wellbeing", tag_levels = "A") +
  plot_layout(guides = 'collect') #+ plot_layout(nrow = 3, byrow = T)

embody_plots
ggsave(
  embody_plots,
  path = here::here(here::here("figs", "congregation")),
  width = 16,
  height = 9,
  units = "in",
  filename = "embody_plots.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 1200
)

dev.off()
fatigue_t
distress_t


# self esteem -------------------------------------------------------------
# Self-esteem
# On the whole am satisfied with myself.
# Take a positive attitude toward myself.
# Am inclined to feel that I am a failure.

## fit
out_f = function(formula) {
  with(df, glm(as.formula(
    paste(
      "SELF.ESTEEM_lead2_z~ bs(Religion.CongregationSize_lead1_log) +",
      paste(baselinevars,
            collapse = "+")
    )
  )))
}
main = "Self Esteem"
ylab = "Self Esteem (SD)"
# clean oven
rm(out_m)

rm(out_ct)
# fit regression model
out_m <- out_f()
# g-computation - contrasts
out_ct <- pool_stglm_contrast(out_m, df = df, m = 10,  X = X, x = c, r= r)
#table
selfesteem_t <-out_ct %>%
  slice(1:8) |>
  tibble() |>
  rename(Contrast = row,
         Estimate = est,
         std_error = se,
         CI_hi = ui,
         CI_lo = li) |>
  kbl(caption = main,
      digits = 3,
      "html") |>
  kable_styling() %>%
  row_spec(c(6), bold = T, color = "white", background = "dodgerblue") |>
  kable_minimal(full_width = F)
selfesteem_t
selfesteem_p <- ggplot_stglm(out_ct, ylim = ylim8, main, xlab, ylab, min = min, p=p, r= 1)
selfesteem_p

round( EValue::evalues.OLS( 0.018, se = 0.037, sd = 1, delta = delta, true = 0), 3)
round( EValue::evalues.RR( , lo =  , hi =, true = 1), 4)

#
# stand living ------------------------------------------------------------
# Part of pwi
# Personal Wellbeing Index
# Your health.
# Your standard of living.
# Your future security.
# Your personal relationships.

# ## fit
# out_f = function(formula) {
#   with(df, glm(as.formula(
#     paste(
#       "Standard.Living_lead2_z~ bs(Religion.CongregationSize_lead1_log) +",
#       paste(baselinevars,
#             collapse = "+")
#     )
#   )))
# }
# main = "Standard Living"
# ylab = "Standard Living (SD)"
# # clean oven
# rm(out_m)
#
# rm(out_ct)
# # fit regression model
# out_m <- out_f()
# # g-computation - contrasts
# out_ct <- pool_stglm_contrast(out_m, df = df, m = 10,  X = X, x = c, r= r)
# #table
# out_ct %>%
#   slice(1:8) |>
#   tibble() |>
#   rename(Contrast = row,
#          Estimate = est,
#          std_error = se,
#          CI_hi = ui,
#          CI_lo = li) |>
#   kbl(caption = main,
#       digits = 3,
#       "html") |>
#   kable_styling() %>%
#   row_spec(c(6), bold = T, color = "white", background = "dodgerblue") |>
#   kable_minimal(full_width = F)
#
# ggplot_stglm(out_ct, ylim = ylim8, main, xlab, ylab, min = min, p=p, r= 1)
#
# round( EValue::evalues.OLS( 0.093, se = 0.045, sd = 1, delta = delta, true = 0), 3)
# round( EValue::evalues.RR( , lo =  , hi =, true = 1), 4)
#
# # amount
# sd(ccf$Standard.Living_lead2) * 0.093
#
# # future security ---------------------------------------------------------
#
# ## fit
# out_f = function(formula) {
#   with(df, glm(as.formula(
#     paste(
#       "Your.Future.Security_lead2_z~ bs(Religion.CongregationSize_lead1_log) +",
#       paste(baselinevars,
#             collapse = "+")
#     )
#   )))
# }
# main = "Your Future Security"
# ylab = "Your Future Security (SD)"
# # clean oven
# rm(out_m)
#
# rm(out_ct)
# # fit regression model
# out_m <- out_f()
# # g-computation - contrasts
# out_ct <- pool_stglm_contrast(out_m, df = df, m = 10,  X = X, x = c, r= r)
# #table
# out_ct %>%
#   slice(1:8) |>
#   tibble() |>
#   rename(Contrast = row,
#          Estimate = est,
#          std_error = se,
#          CI_hi = ui,
#          CI_lo = li) |>
#   kbl(caption = main,
#       digits = 3,
#       "html") |>
#   kable_styling() %>%
#   row_spec(c(6), bold = T, color = "white", background = "dodgerblue") |>
#   kable_minimal(full_width = F)
#
# ggplot_stglm(out_ct, ylim = ylim8, main, xlab, ylab, min = min, p=p, r= 1)
#
# round( EValue::evalues.OLS( 0.093, se = 0.045, sd = 1, delta = delta, true = 0), 3)
# round( EValue::evalues.RR( , lo =  , hi =, true = 1), 4)
#
# # your health -------------------------------------------------------------
# ## fit
# out_f = function(formula) {
#   with(df, glm(as.formula(
#     paste(
#       "Your.Health_lead2_z~ bs(Religion.CongregationSize_lead1_log) +",
#       paste(baselinevars,
#             collapse = "+")
#     )
#   )))
# }
# main = "Your Health"
# ylab = "Your Health (SD)"
# # clean oven
# rm(out_m)
#
# rm(out_ct)
# # bake
# # fit regression model
# out_m <- out_f()
# # g-computation - contrasts
# out_ct <- pool_stglm_contrast(out_m, df = df, m = 10,  X = X, x = c, r= r)
# #table
# out_ct %>%
#   slice(1:8) |>
#   tibble() |>
#   rename(Contrast = row,
#          Estimate = est,
#          std_error = se,
#          CI_hi = ui,
#          CI_lo = li) |>
#   kbl(caption = main,
#       digits = 3,
#       "html") |>
#   kable_styling() %>%
#   row_spec(c(6), bold = T, color = "white", background = "dodgerblue") |>
#   kable_minimal(full_width = F)
#
# ggplot_stglm(out_ct, ylim = ylim8, main, xlab, ylab, min = min, p=p, r= 1)
#
# round( EValue::evalues.OLS( 0.093, se = 0.045, sd = 1, delta = delta, true = 0), 3)
# round( EValue::evalues.RR( , lo =  , hi =, true = 1), 4)
#
# # personal relationships --------------------------------------------------
#
# ## fit
# out_f = function(formula) {
#   with(df, glm(as.formula(
#     paste(
#       "Your.Personal.Relationships_lead2_z~ bs(Religion.CongregationSize_lead1_log) +",
#       paste(baselinevars, collapse = "+")
#     )
#   )))
# }
#
# main = "Your Personal Relationships"
# ylab = "Your Personal Relationships (SD)"
# # clean oven
# rm(out_m)
#
# rm(out_ct)
# # fit regression model
# out_m <- out_f()
# # g-computation - contrasts
# out_ct <- pool_stglm_contrast(out_m, df = df, m = 10,  X = X, x = c, r= r)
# #table
# out_ct %>%
#   slice(1:8) |>
#   tibble() |>
#   rename(Contrast = row,
#          Estimate = est,
#          std_error = se,
#          CI_hi = ui,
#          CI_lo = li) |>
#   kbl(caption = main,
#       digits = 3,
#       "html") |>
#   kable_styling() %>%
#   row_spec(c(6), bold = T, color = "white", background = "dodgerblue") |>
#   kable_minimal(full_width = F)
#
# ggplot_stglm(out_ct, ylim = ylim8, main, xlab, ylab, min = min, p=p, r= 1)
#
# round( EValue::evalues.OLS( 0.093, se = 0.045, sd = 1, delta = delta, true = 0), 3)
# round( EValue::evalues.RR( , lo =  , hi =, true = 1), 4)




# pwi ---------------------------------------------------------------------
# Personal Wellbeing Index
# Your health.
# Your standard of living.
# Your future security.
# Your personal relationships.
## fit
out_f = function(formula) {
  with(df, glm(as.formula(
    paste(
      "PWI_lead2_z~ bs(Religion.CongregationSize_lead1_log) +",
      paste(baselinevars,
            collapse = "+")
    )
  )))
}

main = "Personal Well Being Index (SD)"
ylab = "Personal Well Being Index (SD)"
# clean oven
rm(out_m)

rm(out_ct)
# fit regression model
out_m <- out_f()
# g-computation - contrasts
out_ct <- pool_stglm_contrast(out_m, df = df, m = 10,  X = X, x = c, r= r)
#table
pwi_t<- out_ct %>%
  slice(1:8) |>
  tibble() |>
  rename(Contrast = row,
         Estimate = est,
         std_error = se,
         CI_hi = ui,
         CI_lo = li) |>
  kbl(caption = main,
      digits = 3,
      "html") |>
  kable_styling() %>%
  row_spec(c(6), bold = T, color = "white", background = "dodgerblue") |>
  kable_minimal(full_width = F)
pwi_t
pwi_p <- ggplot_stglm(out_ct, ylim = ylim8, main, xlab, ylab, min = min, p=p, r= 1)

round( EValue::evalues.OLS( 0.093, se = 0.032, sd = 1, delta = delta, true = 0), 3) |>
  kbl(caption = main,
      digits = 3,
      "html") |>
  kable_styling() %>%
  kable_minimal(full_width = F)

round( EValue::evalues.RR( , lo =  , hi =, true = 1), 4)

# point lower upper
# RR       1.368 1.089 1.718
# E-values 2.077 1.400    NA


# life sat ----------------------------------------------------------------
# Satisfaction with life
# I am satisfied with my life.
# In most ways my life is close to ideal.

## fit
out_f = function(formula) {
  with(df, glm(as.formula(
    paste(
      "LIFESAT_lead2_z~ bs(Religion.CongregationSize_lead1_log) +",
      paste(baselinevars,
            collapse = "+")
    )
  )))
}
main = "Life Satisfaction"
ylab = "Life Satisfaction (SD)"
# clean oven
rm(out_m)

rm(out_ct)
# fit regression model
out_m <- out_f()
# g-computation - contrasts
out_ct <- pool_stglm_contrast(out_m, df = df, m = 10,  X = X, x = c, r= r)
#table
lifesat_t<- out_ct %>%
  slice(1:8) |>
  tibble() |>
  rename(Contrast = row,
         Estimate = est,
         std_error = se,
         CI_hi = ui,
         CI_lo = li) |>
  kbl(caption = main,
      digits = 3,
      "html") |>
  kable_styling() %>%
  row_spec(c(6), bold = T, color = "white", background = "dodgerblue") |>
  kable_minimal(full_width = F)
lifesat_t
lifesat_p <- ggplot_stglm(out_ct, ylim = ylim8, main, xlab, ylab, min = min, p=p, r= 1)

lifesat_p

round( EValue::evalues.OLS( 0.006, se = 0.037, sd = 1, delta = delta, true = 0), 3) |>
  kbl(caption = main,
      digits = 3,
      "html") |>
  kable_styling() %>%
  kable_minimal(full_width = F)

round( EValue::evalues.RR( , lo =  , hi =, true = 1), 4)



# GRAPHS REFLECTIVE WELLBEING ---------------------------------------------


reflective_plots <- selfesteem_p + lifesat_p + pwi_p + plot_annotation(title = "Causal effects of congregation size on reflective wellbeing") +
  plot_layout(guides = 'collect') #+ plot_layout(nrow = 3, byrow = T)

ggsave(
  reflective_plots,
  path = here::here(here::here("figs", "congregation")),
  width = 16,
  height = 9,
  units = "in",
  filename = "reflective_plots.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 1200
)

reflective_plots
dev.off()





# Promotion NZSEI ---------------------------------------------------------------
##
out_f = function(formula) {
  with(df, glm(as.formula(
    paste(
      "NZSEI13_lead2_z ~ bs(Religion.CongregationSize_lead1_log) +",
      paste(baselinevars,
            collapse = "+")
    )
  )))
}
main = "Occupational Status"
ylab = "Occupational Status (SD)"
# clean oven
rm(out_m)

rm(out_ct)
# fit regression model
out_m <- out_f()
# g-computation - contrasts
out_ct <- pool_stglm_contrast(out_m, df = df, m = 10,  X = X, x = c, r= r)
#table
occupational_t <- out_ct %>%
  slice(1:8) |>
  tibble() |>
  rename(Contrast = row,
         Estimate = est,
         std_error = se,
         CI_hi = ui,
         CI_lo = li) |>
  kbl(caption = main,
      digits = 3,
      "html") |>
  kable_styling() %>%
  row_spec(c(6), bold = T, color = "white", background = "dodgerblue") |>
  kable_minimal(full_width = F)
occupational_t
occupational_p <- ggplot_stglm(out_ct, ylim = ylim8, main, xlab, ylab, min = min, p=p, r= 1)

occupational_p
round( EValue::evalues.OLS( .012, se = .033, sd = 1, delta = delta, true = 0), 3)
round( EValue::evalues.RR( , lo =  , hi =, true = 1), 4)


# income ------------------------------------------------------------------
##
out_f = function(formula) {
  with(df, glm(as.formula(
    paste(
      "income_log_lead2_z ~ bs(Religion.CongregationSize_lead1_log) +",
      paste(baselinevars,
            collapse = "+")
    )
  )))
}
main = "Log Household Income (SD)"
ylab = "Log Household Income (SD)"
# clean oven
rm(out_m)

rm(out_ct)
# fit regression model
out_m <- out_f()
# g-computation - contrasts
out_ct <- pool_stglm_contrast(out_m, df = df, m = 10,  X = X, x = c, r= r)
#table
income_t <- out_ct %>%
  slice(1:8) |>
  tibble() |>
  rename(Contrast = row,
         Estimate = est,
         std_error = se,
         CI_hi = ui,
         CI_lo = li) |>
  kbl(caption = main,
      digits = 3,
      "html") |>
  kable_styling() %>%
  row_spec(c(6), bold = T, color = "white", background = "dodgerblue") |>
  kable_minimal(full_width = F)
income_t
income_p <- ggplot_stglm(out_ct, ylim = ylim8, main, xlab, ylab, min = min, p=p, r= 1)

income_p

round( EValue::evalues.OLS( , se = , sd = 1, delta = delta, true = 0), 3)
round( EValue::evalues.RR( , lo =  , hi =, true = 1), 4)



# volunteers --------------------------------------------------------------
# fit
out_f = function(formula) {
  with(df, glm(as.formula(
    paste(
      "Volunteers_lead2~ bs(Religion.CongregationSize_lead1_log) +",
      paste(baselinevars,
            collapse = "+")
    )
  ),  family = "poisson"))
}
main = "Volunteer Rate"
ylab = "Volunteer Rate"
# clean oven
rm(out_m)
rm(out_ct)
# fit regression model
out_m <- out_f()
# g-computation - contrasts
out_ct <- pool_stglm_contrast_ratio(out_m, df = df, m = 10,  X = X, x = c, r= r)
#table
volunteers_t<- out_ct %>%
  slice(1:8) |>
  tibble() |>
  rename(Contrast = row,
         Estimate = est,
         std_error = se,
         CI_hi = ui,
         CI_lo = li) |>
  kbl(caption = main,
      digits = 3,
      "html") |>
  kable_styling() %>%
  row_spec(c(6), bold = T, color = "white", background = "dodgerblue") |>
  kable_minimal(full_width = F)
volunteers_t
volunteers_p<- ggplot_stglm(out_ct, ylim =c(.9,2), main, xlab, ylab, min = min, p=p, r= 1)
volunteers_p

#round( EValue::evalues.OLS( , se = , sd = 1, delta = delta, true = 0), 3)


round( EValue::evalues.RR( 1.259, lo =  1.079, hi = 1.440, true = 1), 4) |>
  kbl(caption = main,
      digits = 3,
      "html") |>
  kable_styling() %>%
  kable_minimal(full_width = F)

# “With an observed risk ratio of RR=1.3, an unmeasured confounder that was associated with both the outcome and the exposure by a risk ratio of 1.8-fold each, above and beyond the measured confounders, could explain away the estimate, but weaker joint confounder associations could not; to move the confidence interval to include the null, an unmeasured confounder that was associated with the outcome and the exposure by a risk ratio of 1.4-fold each could do so, but weaker joint confounder associations could not.”
# We could include statements like this in all empirical papers
# 41

# charity donate ----------------------------------------------------------
## fit
out_f = function(formula) {
  with(df, glm(as.formula(
    paste(
      "CharityDonate_log_lead2_z~ bs(Religion.CongregationSize_lead1_log) +",
      paste(baselinevars,
            collapse = "+")
    )
  )))
}
main = "Log Annual Charity Donatations (annual SD)"
ylab = "Log Annual Charity Donatations (annual SD)"
# clean oven
rm(out_m)

rm(out_ct)
# fit regression model
out_m <- out_f()
# g-computation - contrasts
out_ct <- pool_stglm_contrast(out_m, df = df, m = 10,  X = X, x = c, r= r)
#table
charity_t <- out_ct %>%
  slice(1:8) |>
  tibble() |>
  rename(Contrast = row,
         Estimate = est,
         std_error = se,
         CI_hi = ui,
         CI_lo = li) |>
  kbl(caption = main,
      digits = 3,
      "html") |>
  kable_styling() %>%
  row_spec(c(6), bold = T, color = "white", background = "dodgerblue") |>
  kable_minimal(full_width = F)
charity_t
charity_p <- ggplot_stglm(out_ct, ylim = ylim8, main, xlab, ylab, min = min, p=p, r= 1)
charity_p

round( EValue::evalues.OLS(0.228 , se = 0.044, sd = 1, delta = delta, true = 0), 3) |>
  kbl(caption = main,
      digits = 3,
      "html") |>
  kable_styling() %>%
  kable_minimal(full_width = F)


round( EValue::evalues.RR( , lo =  , hi =, true = 1), 4)

# point lower upper
# RR       1.333 1.116 1.593
# E-values 2.000 1.475    NA

#### SOCIAL CONNECTION



# GRAPHS ECONOMIC OUTCOMES -------------------------------------------------------

economic_plots <- occupational_p +  income_p +   charity_p +  volunteers_p+ plot_annotation(title = "Causal effects of congregation size on economic and charitable outcomes", tag_levels = "A") +
  plot_layout(guides = 'collect') #+ plot_layout(nrow = 3, byrow = T)

ggsave(
  economic_plots,
  path = here::here(here::here("figs", "congregation")),
  width = 16,
  height = 9,
  units = "in",
  filename = "economic_plots.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 1200
)

economic_plots
dev.off()



# belonging ---------------------------------------------------------------
# Felt belongingness
# 1. Know that people in my life accept and value me.
# 2. Feel like an outsider.
# 3. Know that people around me share my attitudes and beliefs.

out_f = function(formula) {
  with(df, glm(as.formula(
    paste(
      "BELONG_lead2_z~ bs(Religion.CongregationSize_lead1_log) +",
      paste(baselinevars,
            collapse = "+")
    )
  )))
}
main = "Social Belonging"
ylab = "Social Belonging (SD)"
# clean oven
rm(out_m)
rm(out_ct)
# fit regression model
out_m <- out_f()
# g-computation - contrasts
out_ct <- pool_stglm_contrast(out_m, df = df, m = 10,  X = X, x = c, r= r)
#table
belonging_t <- out_ct %>%
  slice(1:8) |>
  tibble() |>
  rename(Contrast = row,
         Estimate = est,
         std_error = se,
         CI_hi = ui,
         CI_lo = li) |>
  kbl(caption = main,
      digits = 3,
      "html") |>
  kable_styling() %>%
  row_spec(c(6), bold = T, color = "white", background = "dodgerblue") |>
  kable_minimal(full_width = F)
belonging_t
belonging_p <- ggplot_stglm(out_ct, ylim = ylim8, main, xlab, ylab, min = min, p=p, r= 1)
belonging_p
round( EValue::evalues.OLS( 0.071, se = 0.042, sd = 1, delta = delta, true = 0), 3) |>
  kbl(caption = main,
      digits = 3,
      "html") |>
  kable_styling() %>%
  kable_minimal(full_width = F)

round( EValue::evalues.RR( , lo =  , hi =, true = 1), 4)

# soc support -------------------------------------------------------------

# Perceived social support
# There are people I can depend on to help me if I really need it.
# There is no one I can turn to for guidance in times of stress.
# I know there are people I can turn to when I need help.

## fit
out_f = function(formula) {
  with(df, glm(as.formula(
    paste(
      "SUPPORT_lead2_z~ bs(Religion.CongregationSize_lead1_log) +",
      paste(baselinevars,
            collapse = "+")
    )
  )))
}
main = "Social Support"
ylab = "Social Support (SD)"
# clean oven
rm(out_m)
rm(out_ct)
# fit regression model
out_m <- out_f()
# g-computation - contrasts
out_ct <- pool_stglm_contrast(out_m, df = df, m = 10,  X = X, x = c, r= r)
#table
support_t <-out_ct %>%
  slice(1:8) |>
  tibble() |>
  rename(Contrast = row,
         Estimate = est,
         std_error = se,
         CI_hi = ui,
         CI_lo = li) |>
  kbl(caption = main,
      digits = 3,
      "html") |>
  kable_styling() %>%
  row_spec(c(6), bold = T, color = "white", background = "dodgerblue") |>
  kable_minimal(full_width = F)
support_t
support_p <- ggplot_stglm(out_ct, ylim = ylim8, main, xlab, ylab, min = min, p=p, r= 1)
support_p
round( EValue::evalues.OLS(0.107 , se = 0.042, sd = 1, delta = delta, true = 0), 3) |>
  kbl(caption = main,
      digits = 3,
      "html") |>
  kable_styling() %>%
  kable_minimal(full_width = F)


round( EValue::evalues.RR( , lo =  , hi =, true = 1), 4)

# community lead ----------------------------------------------------------
# Sense of community
# I feel a sense of community with others in my local neighbourhood.

## fit
out_f = function(formula) {
  with(df, glm(as.formula(
    paste(
      "community_lead2_z~ bs(Religion.CongregationSize_lead1_log) +",
      paste(baselinevars,
            collapse = "+")
    )
  )))
}
main = "Community"
ylab = "Community (SD)"
# clean oven
rm(out_m)

rm(out_ct)
# fit regression model
out_m <- out_f()
# g-computation - contrasts
out_ct <- pool_stglm_contrast(out_m, df = df, m = 10,  X = X, x = c, r= r)
#table
community_t <-out_ct %>%
  slice(1:8) |>
  tibble() |>
  rename(Contrast = row,
         Estimate = est,
         std_error = se,
         CI_hi = ui,
         CI_lo = li) |>
  kbl(caption = main,
      digits = 3,
      "html") |>
  kable_styling() %>%
  row_spec(c(6), bold = T, color = "white", background = "dodgerblue") |>
  kable_minimal(full_width = F)
community_t
community_p <- ggplot_stglm(out_ct, ylim = ylim8, main, xlab, ylab, min = min, p=p, r= 1)
community_p

round( EValue::evalues.OLS( 0.086, se = 0.046, sd = 1, delta = delta, true = 0), 3) |>
  kbl(caption = main,
    digits = 3,
    "html") |>
  kable_styling() %>%
  kable_minimal(full_width = F)

round( EValue::evalues.RR( , lo =  , hi =, true = 1), 4)


# national wellbeing ------------------------------------------------------

# National Wellbeing Index
# The economic situation in New Zealand.
# The social conditions in New Zealand.
# Business in New Zealand.

## fit
out_f = function(formula) {
  with(df, glm(as.formula(
    paste(
      "NWI_lead2_z~ bs(Religion.CongregationSize_lead1_log) +",
      paste(baselinevars,
            collapse = "+")
    )
  )))
}

main = "National Well Being"
ylab = "National Well Being (SD)"
# clean oven
rm(out_m)

rm(out_ct)
# fit regression model
out_m <- out_f()
# g-computation - contrasts
out_ct <- pool_stglm_contrast(out_m, df = df, m = 10,  X = X, x = c, r= r)
#table
out_ct %>%
  slice(1:8) |>
  tibble() |>
  rename(Contrast = row,
         Estimate = est,
         std_error = se,
         CI_hi = ui,
         CI_lo = li) |>
  kbl(caption = main,
      digits = 3,
      "html") |>
  kable_styling() %>%
  row_spec(c(6), bold = T, color = "white", background = "dodgerblue") |>
  kable_minimal(full_width = F)

nwi_p <- ggplot_stglm(out_ct, ylim = ylim8, main, xlab, ylab, min = min, p=p, r= 1)
nwi_p
round( EValue::evalues.OLS(0.003 , se = 0.044, sd = 1, delta = delta, true = 0), 3)
round( EValue::evalues.RR( , lo =  , hi =, true = 1), 4)


# GRAPHS SOCIAL WELL-BEING ------------------------------------------------

library(patchwork)

social_plots <- belonging_p +  support_p +   community_p + nwi_p +
  plot_annotation(title = "Causal effects of congregation size on social wellbeing") +
  plot_layout(guides = 'collect') #+ plot_layout(nrow = 3, byrow = T)
social_plots
ggsave(
  social_plots,
  path = here::here(here::here("figs", "congregation")),
  width = 16,
  height = 9,
  units = "in",
  filename = "social_plots.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 1200
)

social_plots
dev.off()


# scripture ---------------------------------------------------------------
#### RELIGIOUS OUTCOMES
out_f = function(formula) {
  with(df, glm(as.formula(
    paste(
      "Religion.Scripture_lead2_log_z ~ bs(Religion.CongregationSize_lead1_log) +",
      paste(baselinevars,
            collapse = "+")
    )
  )))
}
main = "Log weekly Scripture Reading (SD)"
ylab = "Log weekly Scripture Reading (SD)"
# clean oven
rm(out_m)
rm(out_ct)
# fit regression model
out_m <- out_f()
# g-computation - contrasts
out_ct <- pool_stglm_contrast(out_m, df = df, m = 10,  X = X, x = c, r= r)
#table
scripture_t <- out_ct %>%
  slice(1:8) |>
  tibble() |>
  rename(Contrast = row,
         Estimate = est,
         std_error = se,
         CI_hi = ui,
         CI_lo = li) |>
  kbl(caption = main,
      digits = 3,
      "html") |>
  kable_styling() %>%
  row_spec(c(6), bold = T, color = "white", background = "dodgerblue") |>
  kable_minimal(full_width = F)
scripture_t
scripture_p <- ggplot_stglm(out_ct, ylim = ylim8, main, xlab, ylab, min = min, p=p, r= 1)
scripture_p
round( EValue::evalues.OLS( 0.315, se = 0.039, sd = 1, delta = delta, true = 0), 3) |>
  kbl(caption = main,
      digits = 3,
      "html") |>
  kable_styling() %>%
  kable_minimal(full_width = F)
round( EValue::evalues.RR( , lo =  , hi =, true = 1), 4)

# log prayer --------------------------------------------------------------
out_f = function(formula) {
  with(df, glm(as.formula(
    paste(
      "Religion.Prayer_lead2_log_z ~ bs(Religion.CongregationSize_lead1_log) +",
      paste(baselinevars,
            collapse = "+")
    )
  )))
}
main = "Log Weekly Prayer  (SD)"
ylab = "Log Weekly Prayer (SD)"
# clean oven
rm(out_m)

rm(out_ct)
# fit regression model
out_m <- out_f()
# g-computation - contrasts
out_ct <- pool_stglm_contrast(out_m, df = df, m = 10,  X = X, x = c, r= r)
#table
prayer_t <- out_ct %>%
  slice(1:8) |>
  tibble() |>
  rename(Contrast = row,
         Estimate = est,
         std_error = se,
         CI_hi = ui,
         CI_lo = li) |>
  kbl(caption = main,
      digits = 3,
      "html") |>
  kable_styling() %>%
  row_spec(c(6), bold = T, color = "white", background = "dodgerblue") |>
  kable_minimal(full_width = F)
prayer_t
prayer_p <- ggplot_stglm(out_ct, ylim = ylim8, main, xlab, ylab, min = min, p=p, r= 1)
prayer_p
round( EValue::evalues.OLS( 0.267	, se = 0.043, sd = 1, delta = delta, true = 0), 3) |>
  kbl(caption = main,
      digits = 3,
      "html") |>
  kable_styling() %>%
  kable_minimal(full_width = F)

round( EValue::evalues.RR( , lo =  , hi =, true = 1), 4)


#
# “With an observed risk ratio of RR=3.4 , an unmeasured confounder that was associated with both the outcome and the exposure by a risk ratio of 6.2-fold each, above and beyond the measured confounders, could explain away the estimate, but weaker joint confounder associations could not; to move the confidence interval to include the null, an unmeasured confounder that was associated with the outcome and the exposure by a risk ratio of 4.0-fold each could do so, but weaker joint confounder associations could not.”


# church attendance -------------------------------------------------------

out_f = function(formula) {
  with(df, glm(as.formula(
    paste(
      "Religion.Church_lead2_log_z ~ bs(Religion.CongregationSize_lead1_log) +",
      paste(baselinevars,
            collapse = "+")
    )
  )))
}
main = "Log Monthly Church  (SD)"
ylab = "Log Monthly Church (SD)"
# clean oven
rm(out_m)
rm(out_ct)
# fit regression model
out_m <- out_f()
# g-computation - contrasts
out_ct <- pool_stglm_contrast(out_m, df = df, m = 10,  X = X, x = c, r= r)
#table
church_t<-out_ct %>%
  slice(1:8) |>
  tibble() |>
  rename(Contrast = row,
         Estimate = est,
         std_error = se,
         CI_hi = ui,
         CI_lo = li) |>
  kbl(caption = main,
      digits = 3,
      "html") |>
  kable_styling() %>%
  row_spec(c(6), bold = T, color = "white", background = "dodgerblue") |>
  kable_minimal(full_width = F)
church_t
church_p<- ggplot_stglm(out_ct, ylim = ylim8, main, xlab, ylab, min = min, p=p, r= 1)
church_p
round( EValue::evalues.OLS( 0.537, se = 0.041, sd = 1, delta = delta, true = 0), 3)
round( EValue::evalues.RR( , lo =  , hi =, true = 1), 4)


# Religious ---------------------------------------------------------------
out_f = function(formula) {
  with(df, glm(as.formula(
    paste(
      "Relid_lead2_z ~ bs(Religion.CongregationSize_lead1_log) +",
      paste(baselinevars,
            collapse = "+")
    )
  )))
}
main = "Religious Identification (SD)"
ylab = "Religious Identification (SD)"
# clean oven
rm(out_m)
rm(out_ct)
# fit regression model
out_m <- out_f()
# g-computation - contrasts
out_ct <- pool_stglm_contrast(out_m, df = df, m = 10,  X = X, x = c, r= r)
#table
religious_t<-out_ct %>%
  slice(1:8) |>
  tibble() |>
  rename(Contrast = row,
         Estimate = est,
         std_error = se,
         CI_hi = ui,
         CI_lo = li) |>
  kbl(caption = main,
      digits = 3,
      "html") |>
  kable_styling() %>%
  row_spec(c(6), bold = T, color = "white", background = "dodgerblue") |>
  kable_minimal(full_width = F)
religious_t
religious_p<- ggplot_stglm(out_ct, ylim = ylim8, main, xlab, ylab, min = min, p=p, r= 1)
religious_p
round( EValue::evalues.OLS( 0.247, se = 0.036, sd = 1, delta = delta, true = 0), 3)
round( EValue::evalues.RR( , lo =  , hi =, true = 1), 4)


# believe God -------------------------------------------------------------
# fit
out_f = function(formula) {
  with(df, glm(as.formula(
    paste(
      "Believe.God_lead2~ bs(Religion.CongregationSize_lead1_log) +",
      paste(baselinevars,
            collapse = "+")
    )
  ), family = "poisson"))
}
main = "God Belief Rate"
ylab = "God Belief Rate"
# clean oven
rm(out_m)

rm(out_ct)
# fit regression model
out_m <- out_f()
# g-computation - contrasts
out_ct <- pool_stglm_contrast_ratio(out_m, df = df, m = 10,  X = X, x = c, r= r)
#table
god_t<- out_ct %>%
  slice(1:8) |>
  tibble() |>
  rename(Contrast = row,
         Estimate = est,
         std_error = se,
         CI_hi = ui,
         CI_lo = li) |>
  kbl(caption = main,
      digits = 3,
      "html") |>
  kable_styling() %>%
  row_spec(c(6), bold = T, color = "white", background = "dodgerblue") |>
  kable_minimal(full_width = F)
god_t
god_p<- ggplot_stglm(out_ct, ylim =c(.9,1.1), main, xlab, ylab, min = min, p=p, r= 1)
god_p

round( EValue::evalues.OLS( , se = , sd = 1, delta = delta, true = 0), 3)


round( EValue::evalues.RR( , lo =  , hi = , true = 1), 4) |>
  kbl(caption = main,
      digits = 3,
      "html") |>
  kable_styling() %>%
  kable_minimal(full_width = F)



# believe spirit -------------------------------------------------------------
# fit
out_f = function(formula) {
  with(df, glm(as.formula(
    paste(
      "Believe.Spirit_lead2~ bs(Religion.CongregationSize_lead1_log) +",
      paste(baselinevars,
            collapse = "+")
    )
  ), family = "poisson"))
}
main = "Believe Spirit Rate"
ylab = "Believe Spirit Rate"
# clean oven
rm(out_m)

rm(out_ct)
# fit regression model
out_m <- out_f()
# g-computation - contrasts
out_ct <- pool_stglm_contrast_ratio(out_m, df = df, m = 10,  X = X, x = c, r= r)
#table
spirit_t<- out_ct %>%
  slice(1:8) |>
  tibble() |>
  rename(Contrast = row,
         Estimate = est,
         std_error = se,
         CI_hi = ui,
         CI_lo = li) |>
  kbl(caption = main,
      digits = 3,
      "html") |>
  kable_styling() %>%
  row_spec(c(6), bold = T, color = "white", background = "dodgerblue") |>
  kable_minimal(full_width = F)
spirit_t
spirit_p<- ggplot_stglm(out_ct, ylim =c(.9,1.1), main, xlab, ylab, min = min, p=p, r= 1)
spirit_p

round( EValue::evalues.OLS( , se = , sd = 1, delta = delta, true = 0), 3)


round( EValue::evalues.RR( , lo =  , hi = , true = 1), 4) |>
  kbl(caption = main,
      digits = 3,
      "html") |>
  kable_styling() %>%
  kable_minimal(full_width = F)




# PLOTS OF RELIGIOUS WELL-BEING -------------------------------------------

religion_plots <- prayer_p +  religious_p  + scripture_p +
  god_p + spirit_p + church_p  + plot_annotation(title = "Causal effects of congregation size on religious behaviour", tag_levels = "A") +
  plot_layout(guides = 'collect')  # plot_layout(nrow = 1, byrow = FALSE)


religion_plots
ggsave(
  religion_plots,
  path = here::here(here::here("figs", "congregation")),
  width = 16,
  height = 9,
  units = "in",
  filename = "religion_plots.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 1200
)

religion_plots
dev.off()
