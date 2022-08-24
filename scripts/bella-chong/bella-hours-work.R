# Bella hours work
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
#source("libs.R")
#  functions
source(pull_path_funs)
#source("funs.R")

# read data
dat <- readRDS(pull_path)

# dat$COVID19.Timeline
# bels:
#   value                                                                                              label
# 0.0                                                                                   The Before Times
# 1.0                                31.12.2019 -- 27.02.2020 [First cluster of cases in Wuhan reported]
# 1.1                                      28.02.2020 -- 25.02.2020 [First case recorded in New Zealand]
# 1.2                                                           26.03.2020 -- 27.04-2020 [Alert Level 4]
# 1.3                                                           28.04.2020 -- 13.05.2020 [Alert Level 3]
# 1.4                                                          14.05.2020 -- 08.06.2020 [Alert Level 2].
# 1.5                                                           09.06.2020 -- 11.08.2020 [Alert Level 1]
# 2.1 12.08.2020 -- 30.08.2020 [Second Outbreak - Auckland Alert Level 3, Rest of Country Alert Level 2]
# 2.2                 30.08.2020 -- 21.09.2020 [Auckland Alert Level 2.5, Rest of Country Alert Level 2]
# 2.3                    22.09.2020 -- 07.10.2020 [Auckland Alert Level 2, Rest of Country Alert Level 1
# 2.4                                                              08.10.2020 -- onwards [Alert Level 1]

# dat$REGC_2018

# labels:
#   value                     label
# 1          Northland Region
# 2           Auckland Region
# 3            Waikato Region
# 4      Bay of Plenty Region
# 5           Gisborne Region
# 6         Hawkes Bay Region
# 7           Taranaki Region
# 8 Manawatu-Whanganui Region
# 9         Wellington Region
# 12         West Coast Region
# 13         Canterbury Region
# 14              Otago Region
# 15          Southland Region
# 16             Tasman Region
# 17             Nelson Region
# 18        Marlborough Region
# 99       Area Outside Region

#  This isn't sensible
# dat1 <- dat %>%
#   dplyr::mutate(NZSEI06_lead1 = lead(NZSEI06, n = 1),
#                 KESSLER6_lead1 = lead(KESSLER6, n = 1),
#                 KESSLER6_lag1 = dplyr::lag(KESSLER6),
#                 NZSEI06_lag1 =  dplyr::lag(NZSEI06),
#                 Employed_lead1 = lead(Employed, n = 1),
#                 Employed_lag1 = dplyr::lag(Employed, n = 1))|>
#   dplyr::filter(Wave == 2019 & YearMeasured==1) |>
#   dplyr::mutate(cum_lockdowns_baseline = if_else(COVID19.Timeline < 1.2, 0,
#                                  if_else(COVID19.Timeline >  1:2 & COVID19.Timeline  < 2, 2,
#                                          ifelse(COVID19.Timeline > 2 & REGC_2018 == 2  | COVID19.Timeline > 2 & REGC_2018 == 1, 4, 3))))
#
# summary(test<- lm(KESSLER6~ cum_lockdowns_baseline + KESSLER6_lag1, data = dat1))
# summary(test<- lm(NZSEI06_lead1 ~ cum_lockdowns_baseline + NZSEI06_lag1, data = dat1))
# summary(test<- glm(Employed_lead1 ~ cum_lockdowns_baseline + Employed_lag1, family = "binomial" ,  data = dat1))
# summary(test<- glm(Employed ~ cum_lockdowns_baseline + Employed_lag1, family = "binomial" ,  data = dat1))

# Code for timeline if needed
#   dplyr::mutate(cum_lockdowns_baseline = if_else(COVID19.Timeline < 1.2, 0,
# if_else(COVID19.Timeline >  1:2 & COVID19.Timeline  < 2, 2,
#         ifelse(COVID19.Timeline > 2 & REGC_2018 == 2  | COVID19.Timeline > 2 &
#                  REGC_2018 == 1, 4, 3)))) |>




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

# Check ids
length(unique(tab_in$Id)) # 34783



### ELIGIBILITY CRITERIA
# 2018/ 2019 - Hours Working Reported
# Not retired baseline?
# Not semi retired baseline?
# Employed at baseline and Employed in 2019
# income above poverty


## select vars
df_wk <- tab_in %>%
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
    SDO,
    RWA,
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
    Emp.WorkLifeBalance,
    Alcohol.Frequency,
    Alcohol.Intensity,
    HLTH.BMI,
    Smoker,
    ChildrenNum,
    # GenCohort,
    partnerlost_job,
    lost_job,
    began_relationship,
    Alcohol.Intensity,
    Alcohol.Frequency,
    SexualSatisfaction,
    POWERDEPENDENCE1,
    POWERDEPENDENCE2,
    Your.Future.Security,
    Your.Personal.Relationships,
    Your.Health,
    Standard.Living,
    PERFECTIONISM,
    PermeabilityIndividual,
    ImpermeabilityGroup,
    Emp.JobSecure
  ) %>%
  dplyr::rename(community = SWB.SoC01) %>%
  dplyr::mutate(Edu = as.numeric(Edu)) %>%
  dplyr::mutate(across(!c(Id, Wave), ~ as.numeric(.x))) %>% # make factors numeric for easy of
  arrange(Id, Wave) %>%
  dplyr::mutate(
    Volunteers = if_else(HoursCharity == 1, 1, 0),
    Church = ifelse(Religion.Church > 8, 8, Religion.Church),
  ) %>%
  arrange(Id, Wave)  %>% # dplyr::mutate(Hours.Work_lead1 = lead(Hours.Work, n = 1)) %>%
  dplyr::mutate(across(
    c(
      Hours.Work,
      HLTH.Disability
    ),
    ~ lead(.x, n = 1),
    .names = "{col}_lead1"
  )) %>% # make leads
  dplyr::mutate(across(
    c(
      NZSEI13,
      NZdep,
      Employed,
      Household.INC,
      community,
      Hours.Work,
      HLTH.Disability,
      EmotionRegulation1,
      EmotionRegulation2,
      EmotionRegulation3,
      Bodysat,
      VENGEFUL.RUMIN,
      KESSLER6sum,
      HLTH.Fatigue,
      Rumination,
      Smoker,
      HLTH.BMI,
      BELONG,
      SUPPORT,
      CharityDonate,
      HoursCharity,
      GRATITUDE,
      Hours.Work,
      HLTH.SleepHours,
      HLTH.Disability,
      Hours.Exercise,
      LIFEMEANING,
      LIFESAT,
      # PWI, can reconstruct later
      NWI,
      SFHEALTH,
      SELF.CONTROL,
      SFHEALTH,
      SELF.ESTEEM,
      Respect.Self,
      SELF.ESTEEM,
      SELF.CONTROL,
      Emp.WorkLifeBalance,
      Alcohol.Frequency,
      Alcohol.Intensity,
      SexualSatisfaction,
      POWERDEPENDENCE1,
      POWERDEPENDENCE2,
      Your.Future.Security,
      Your.Personal.Relationships,
      Your.Health,
      Standard.Living,
      PERFECTIONISM,
      PermeabilityIndividual,
      ImpermeabilityGroup
    ),
    ~ lead(.x, n = 2),
    .names = "{col}_lead2"
  )) %>% # make leads
  dplyr::filter(Wave == 2018) %>%
#  dplyr::filter(retired != 1) %>%
#  dplyr::filter(semiretired != 1) %>%
  dplyr::filter(Household.INC >= 30975) %>% # min income
  dplyr::filter(!is.na(Hours.Work)) %>%
  dplyr::filter(!is.na(Hours.Work_lead1)) %>%
  dplyr::select(
    -c(
      Religion.Church,
      HoursCharity,
      Respect.Self_lead2,  # not there
      Emp.WorkLifeBalance, # not at baseline
      YearMeasured,
      HLTH.Disability_lead1
 )
  ) %>%
  #  dplyr::mutate(across(!c(Id,Wave), ~ scale(.x)))%>%  # standarise vars for easy computing-- do this after imputation
  arrange(Id, Wave) %>%
  data.frame() %>%
  mutate(across(where(is.double), as.numeric)) %>%
  arrange(Id)


# Filtering retirement -- consistency and positivity assumptions

# number of ids
length(unique(df_wk$Id))

# inspect data, with eye to large missingness
skim(df_wk) |>
  arrange(n_missing)

# save data (if you want)
saveh(df_wk, "df_wk")

# read if needed
df_wk <- readh("df_wk")


# table code --------------------------------------------------------------



# mice model  -------------------------------------------------------------
library(mice)

mice_wk <- df_wk %>%
  dplyr::select(-c(Wave, Id))  # won't otherwise run
library(naniar)
naniar::gg_miss_var(mice_wk)

# any col.inear vars?
mice:::find.collinear(mice_wk)

# impute
wk_mice <- mice::mice(mice_wk,  seed = 0, m = 10)
# save
saveh(wk_mice, "wk_mice")

# read
wk_mice <- readh("wk_mice")

# checks
outlist2 <-
  row.names(wk_mice)[wk_mice$outflux < 0.5]
length(outlist2)

# checks
head(wk_mice$loggedEvents, 10)

# data warangling
# we create two completed data sets -- the one without the missing data will be useful for inspecting variable

w_l <- mice::complete(wk_mice, "long", inc = TRUE)

# inspect data -- what do we care about?  Note that the moderate distress category doesn't look useful
skimr::skim(w_l)

# n ids
N<- length(1:nrow(wk_mice$data))

# create variables in z score
w_l2 <- w_l %>%
  # dplyr::mutate(newkids = ChildrenNum_lead2 - ChildrenNum) %>%
  dplyr::mutate(income_log = log(Household.INC + 1)) |>
  dplyr::mutate(KESSLER6sum_lead2 = round(as.integer(KESSLER6sum_lead2, 0))) %>%
  dplyr::mutate(Alcohol.Intensity_lead2 = round(Alcohol.Intensity_lead2, 0)) %>%
  dplyr::mutate(CharityDonate_lead2 = round(CharityDonate_lead2, 0)) %>%
  dplyr::mutate(Hours.Exercise_lead2 = round(Hours.Exercise_lead2, 0)) %>%
  dplyr::mutate(Hours.Exercise_lead2_log = log(Hours.Exercise_lead2 + 1)) %>%
  dplyr::mutate(Alcohol.Intensity = round(Alcohol.Intensity, 0)) %>%
  dplyr::mutate(CharityDonate = round(CharityDonate, 0)) %>%
  dplyr::mutate(Hours.Exercise = round(Hours.Exercise, 0)) %>%
  dplyr::mutate(CharityDonate_log_lead2 = log(CharityDonate_lead2 + 1)) %>%
  dplyr::mutate(Alcohol.Intensity_log_lead2 = log(Alcohol.Intensity_lead2 + 1)) %>%
  dplyr::mutate(Exercise_log_lead2 = log(Hours.Exercise_lead2 + 1)) %>%
  dplyr::mutate(CharityDonate_log = log(CharityDonate + 1)) %>%
  dplyr::mutate(Alcohol.Intensity_log = log(Alcohol.Intensity + 1)) %>%
  dplyr::mutate(Rumination_lead2ord = as.integer(round(Rumination_lead2, digits = 0) + 1)) %>%  # needs to start at 1
  dplyr::mutate(SUPPORT_lead2ord = as.integer(round(SUPPORT_lead2, digits = 0))) %>%
  dplyr::mutate(PERFECTIONISM_lead2ord = as.integer(round(PERFECTIONISM_lead2, digits = 0))) %>%
  dplyr::mutate(VENGEFUL.RUMIN_lead2ord = as.integer(round(VENGEFUL.RUMIN_lead2, digits = 0))) %>%
  dplyr::mutate(Standard.Living_lead2ord = as.integer(round(Standard.Living_lead2, digits = 0))) %>%
  dplyr::mutate(Your.Personal.Relationships_lead2ord = as.integer(round( Your.Personal.Relationships_lead2, digits = 0
  ) + 1)) %>%
  dplyr::mutate(LIFEMEANING_lead2ord = as.integer(round(LIFEMEANING_lead2, digits = 0))) %>%
  dplyr::mutate(HLTH.Fatigue_lead2ord = as.integer(round(HLTH.Fatigue_lead2, digits = 0) + 1)) %>%
  dplyr::mutate(Hours.Exercise_log = log(Hours.Exercise + 1)) %>%
  dplyr::mutate(Alcohol.Frequency_lead2ord = as.integer(round(Alcohol.Frequency_lead2, 0) + 1)) %>%
  dplyr::mutate(LIFESAT_lead2ord = as.integer(round(LIFESAT_lead2, digits = 0))) %>%
  dplyr::mutate(alcohol_bin2 = if_else(Alcohol.Frequency > 3, 1, 0)) %>%
  dplyr::mutate(alcohol_bin = if_else(Alcohol.Frequency > 2, 1, 0)) %>%
  dplyr::mutate(Hours.Work_10 =  Hours.Work / 10) %>%
  dplyr::mutate(Hours.Work_lead1_10 =  as.integer(Hours.Work_lead1 / 10)) %>%
  dplyr::mutate(Hours.Work_lead1_sqrt =  as.integer(sqrt(Hours.Work_lead1))) %>%
  dplyr::mutate(NZSEI13_10 =  NZSEI13 / 10) %>%
  dplyr::mutate(NZSEI13_lead2_10 =  as.integer(NZSEI13_lead2 / 10)) %>%
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
  dplyr::ungroup() |>
  droplevels() |>
  dplyr::mutate(across(where(is.numeric), ~ scale(.x), .names = "{col}_z")) %>%
  select(-c(.imp_z, .id_z))

# Get data into shape
wl3 <- w_l2 %>% mutate_if(is.matrix, as.vector)

w_m <- mice::as.mids(wl3)
w_f <- mice::complete(w_m, "long", inc = TRUE)

saveh(w_m, "w_m")
saveh(w_f, "w_f")




#########################################
###### READ THIS DATA IN BELLA  #########



# 1. Create a folder called "scripts" in your home directory.

# 2. create a folder called "data" in your home directory.

# 3. download these files and place them in the "scripts" folder
# https://www.dropbox.com/s/ypisiw5c8zyknnn/funs.R?dl=0
# https://www.dropbox.com/s/25lbssmf4hqhko1/libs.R?dl=0

# then download these data and place them in your data folder
# https://www.dropbox.com/s/pnwr3jzogm1fdaz/df_wk?dl=0

# https://www.dropbox.com/s/xu0wm1pqo58mpze/w_f?dl=0

# https://www.dropbox.com/s/6y2z7m9bp0pgz9m/w_m?dl=0

#https://www.dropbox.com/s/7jszcwlpp8q4x26/funs2.R?dl=0

# Read the "libs.R" file and make sure you have installed all packages.

# Then run these commands to load your library and your files

source(here::here("scripts", "libs.R"))
#source(here::here("scripts", "funs.R"))
source(here::here("scripts", "funs2.R"))


# Read data here for descriptive data
df_wk <- readh("df_wk")

# imputed data
# this is what we need for the actual data analysis.
w_m <- readh("w_m")

# needed only to get information about the data for interpreting models.
w_f <- readh("w_f")


# example of a descriptive table ------------------------------------------
library(ggplot2)

# Build descriptive table
dev.off()

# make labels as follows
df_wk$Male <- factor(df_wk$Male, labels = c("No", "Yes"))
df_wk$Euro <- factor(df_wk$Euro, labels = c("No", "Yes"))
df_wk$Retired <- factor(df_wk$retired, labels = c("No", "Yes"))

#and continue this way to obtain factor labels ...etc.

table1::table1(
  ~
    Age +
    Male +
    Euro +
    NZSEI13 +
    AGREEABLENESS +
    CONSCIENTIOUSNESS +
    EXTRAVERSION +
    HONESTY_HUMILITY +
    NEUROTICISM +
    OPENNESS, #... etc
  data = df_wk,
  transpose = F
)

# make more tables ... using this method


# Save figs

# you have made a plot and now you want to save it
my_plot <- #ggplot(some coded etc etc. )

  # save as follows
  ggsave(
    my_plot,  # the plot object
    path = here::here(here::here("figs")), # this will send the plot to your figures folder
    width = 12,
    height = 8,
    units = "in",
    filename = "name_of_graph.jpg", # make sure to name your plot so that you can find
    device = 'jpeg',
    limitsize = FALSE,
    dpi = 800
  )


#########################################
#########################################


# model equations ---------------------------------------------------------

# baseline covariates. (we don't want to write them all out)  The "_z" at the end indicates centering.

baselinevars = c(
  "AGREEABLENESS_z",
  "CONSCIENTIOUSNESS_z",
  "EXTRAVERSION_z",
  "HONESTY_HUMILITY_z",
  "NEUROTICISM_z",
  "OPENNESS_z",
  "Age_z",
  "Alcohol.Frequency_z",
  "Alcohol.Intensity_log_z",
  "Bodysat_z",
  "BornNZ_z",
  "Believe.God_z",
  "Believe.Spirit_z",
  "BELONG_z",
  "CharityDonate_log_z",
  "ChildrenNum_z",
  "Church_z",
  "community",
  "Edu_z",
  "Employed_z",
  "Emp.JobSecure_z",
  "EmotionRegulation1_z",
  "EmotionRegulation2_z",
  "EmotionRegulation3_z",
  "Euro_z",
  "GRATITUDE_z",
  "HomeOwner_z",
  "Hours.Exercise_log_z",
  "Hours.Work_z",
  "HLTH.BMI_z",
  "HLTH.Disability_z",
  "HLTH.Fatigue_z",
  "HLTH.SleepHours_z",
  "ImpermeabilityGroup_z",
  "income_log_z",
  "KESSLER6sum_z",
  "LIFEMEANING_z",
  "LIFESAT_z",
  "lost_job_z",
  "Male_z",
  "NZdep_z",
  "NWI_z",
  "NZSEI13_z",
  "Parent_z",
  "Partner_z",
  "PERFECTIONISM_z",
  "PermeabilityIndividual_z",
  "Pol.Orient_z",
  "POWERDEPENDENCE1_z",
  "POWERDEPENDENCE2_z",
  "Relid_z",
  "Respect.Self_z",
  "retired",
  "Rumination_z",
  "SELF.CONTROL_z",
  "SELF.ESTEEM_z",
  "semiretired",
  "SexualSatisfaction_z",
  "SFHEALTH_z",
  "Smoker_z",
  "Spiritual.Identification_z",
  "Standard.Living_z",
  "SUPPORT_z",
  "Urban_z",
  "VENGEFUL.RUMIN_z",
  "Volunteers_z",
  "Your.Health_z",
  "Your.Future.Security_z",
  "Your.Personal.Relationships_z"
)

# functions ---------------------------------------------------------------

# General set up ----------------------------------------------------------

# set up of the graphs
# ylimits
ylim <- c(-.3,.3)

# for poisson/binary plots
ylim_contrast <- c(.8,1.2)

# for poisson/binary pooled plots
ylim_contrast_diff <- c(.9,1.1)

# data
df <-  w_m
# n imputations
m = 10

#  set up ---------------------------------------------------------------
# Work hours per week

X = "Hours.Work_lead1_10"
xlab = "Weekly Hours Work/ 10"
min= 0
max = 8
# baseline
r = 0
# focal contrast
f = 4
# for model functions
c = c(r,f)
# range of estimates
x =  min:max
# contrast for graphs
p = 5

# functions ---------------------------------------------------------------

## Also use
round( EValue::evalues.OLS( est = , se = , sd = 1, delta = 4, true = 0), 3)
round( EValue::evalues.RR( est =  , lo =  , hi =, true = 1), 4)


# models ------------------------------------------------------------------

# BMI ---------------------------------------------------------------------
out_f = function(formula) {
  with(w_m, glm(as.formula(
    paste(
      "HLTH.BMI_lead2_z ~ bs(Hours.Work_lead1_10) +",
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
rm(pool_m)
rm(out_ct)
# bake
out_m <- out_f()
# if you need to make a table
# out_m
summary(pool(out_m)) |>
  kbl(digits = 3, "html")
## contrasts
# using the stdGlm package which does g-computation
out_ct <- pool_stglm_contrast(out_m, df = df, m = 10,  X = X, x = c, r= r)
out_ct %>%
  slice(2) |>
  kbl(digits = 3, "markdown")

# you can make an html table, for options see:
#https://cran.r-project.org/web/packages/kableExtra/vignettes/awesome_table_in_html.html

out_ct %>%
  slice(2) |>
  tibble() |>
  rename(Contrast = row,
         Estimate = est,
         std_error = se,
         CI_hi = ui,
         CI_lo = li) |>
  kbl(caption = "This is my caption",
      digits = 3,
      "html") |>
  kable_minimal(full_width = F)

# graph of contrasts
ggplot_stglm_contrast(out_ct, ylim = ylim, main, xlab, ylab)

# simple table
out_ct %>%
  kbl(format = "markdown", booktabs = T, digits = 3)

# g-computation - estimate marginal effects across the range
pool_m <- pool_stglm(out_m, df = df, m = m, x = x,  X = X)
pool_m %>%
  kbl(format = "markdown", booktabs = T, digits = 3)

# graph
ggplot_stglm(pool_m, ylim = ylim, main, xlab, ylab, min = min, p=p, r=r)
ggplot_stglm(pool_m, ylim = ylim, main, xlab, ylab, min = min, p=p, r=r)

# evalues
round( EValue::evalues.OLS( , se = , sd = 1, delta = 4, true = 0), 3)
round( EValue::evalues.RR( , lo =  , hi =, true = 1), 4)

## make a combo graph
contrast_points_bmi <- ggplot_stglm_contrast(out_ct, ylim = ylim, main, xlab, ylab)
all_points_bmi <- ggplot_stglm(pool_m, ylim = ylim, main, xlab, ylab, min = min, p=p, r=r)

library(patchwork)
dev.off()
contrast_points_bmi / all_points_bmi + plot_annotation(title="Marginal predictions of BMI change from work change", tag_levels = "A")

# sf-health ---------------------------------------------------------------
out_f = function(formula) {
  with(w_m, glm(as.formula(
    paste(
      "SFHEALTH_lead2_z~ bs(Hours.Work_lead1_10) +",
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

# g-computation
pool_m <- pool_stglm(out_m, df = df, m = m, x = x,  X = X)
pool_m %>%
  kbl(format = "markdown", booktabs = T, digits = 3)
# graph
ggplot_stglm(pool_m, ylim = ylim, main, xlab, ylab, min = min, p=p, r=r)
# evalues
round( EValue::evalues.OLS( est = 0.053, se = 0.015, sd = 1, delta = 4, true = 0), 3)
round( EValue::evalues.RR( , lo =  , hi =, true = 1), 4)


## what is a standard deviation of BMI?
sd(mf$HLTH.BMI) * 0.029

# exercise ---------------------------------------------------------------

## fit for mice to work, don't ask why
out_f = function(formula) {
  with(w_m, glm(as.formula(
    paste(
      "Hours.Exercise_lead2_log_z~ bs(Hours.Work_lead1_10) +",
      paste(baselinevars, collapse = "+")
    )
  )))
}
# labels
main = "Hours Exercise"
ylab = "Hours Exercise (SD)"
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

# g-computation
pool_m <- pool_stglm(out_m, df = df, m = m, x = x,  X = X)
pool_m %>%
  kbl(format = "markdown", booktabs = T, digits = 3)
# graph
ggplot_stglm(pool_m, ylim = ylim, main, xlab, ylab, min = min, p=p, r=r)
# evalues
round( EValue::evalues.OLS( , se = , sd = 1, delta = 4, true = 0), 3)
round( EValue::evalues.RR( , lo =  , hi =, true = 1), 4)


# HLTH.Sleep --------------------------------------------------------------

## fit
out_f = function(formula) {
  with(w_m, glm(as.formula(
    paste(
      "HLTH.SleepHours_lead2_z ~ bs(Hours.Work_lead1_10) +",
      paste(baselinevars, collapse = "+")
    )
  )))
}
main = "Hours Sleep (SD)"
ylab = "Hours Sleep (SD)"
# clean oven
rm(out_m)
rm(pool_m)
rm(out_ct)
# bake
out_m <- out_f()
out_m
## contrasts
out_ct <- pool_stglm_contrast(out_m, df = df, m = m,  X = X, x = c, r= r)
out_ct
# graph of contrasts
ggplot_stglm_contrast(out_ct, ylim, main, xlab, ylab)

# table
out_ct %>%
  kbl(format = "markdown", booktabs = T, digits = 3)

# g-computation
pool_m <- pool_stglm(out_m, df = df, m = m, x = x,  X = X)
pool_m %>%
  kbl(format = "markdown", booktabs = T, digits = 3)
# graph
ggplot_stglm(pool_m, ylim = ylim, main, xlab, ylab, min = min, p=p, r=r)
# evalues
round( EValue::evalues.OLS( , se = , sd = 1, delta = 4, true = 0), 3)
round( EValue::evalues.RR( , lo =  , hi =, true = 1), 4)


# smoker ------------------------------------------------------------------
# fit
out_f = function(formula) {
  with(w_m, glm(as.formula(
    paste(
      "Smoker_lead2 ~ bs(Hours.Work_lead1_10) +",
      paste(baselinevars,
            collapse = "+")
    )
  ), family = "poisson"))
}
main = "Smoking Rate"
ylab = "Smoking Rate"
# clean oven
rm(out_m)
rm(pool_m)
rm(out_ct)
# bake
out_m <- out_f()
out_m
## contrasts
out_ct <- pool_stglm_contrast(out_m, df = df, m = m,  X = X, x = c, r= r)
out_ct
# graph of contrasts
ggplot_stglm_contrast_ratio(out_ct, ylim, main, xlab, ylab)
ggplot_stglm_contrast(out_ct, ylim, main, xlab, ylab)
# table
out_ct %>%
  kbl(format = "markdown", booktabs = T, digits = 3)

# g-computation
pool_m <- pool_stglm(out_m, df = df, m = m, x = x,  X = X)
pool_m %>%
  kbl(format = "markdown", booktabs = T, digits = 3)
# graph
ggplot_stglm(pool_m, ylim = ylim, main, xlab, ylab, min = min, p=p, r=r)
# evalues
round( EValue::evalues.OLS( , se = , sd = 1, delta = 4, true = 0), 3)
round( EValue::evalues.RR( , lo =  , hi =, true = 1), 4)
#
# out_c <-
#   pool_stglm_contrast_ratio(
#     m5_Smoker,
#     df = df,
#     m = m,
#     X = X,
#     x = x,
#     r = r
#   )
#plot_stglm_contrast(out_c, ylim = c(.75, 1.5), main, xlab, ylab)
# fatigue -----------------------------------------------------------------
out_f = function(formula) {
  with(w_m, glm(as.formula(
    paste(
      "HLTH.Fatigue_lead2_z ~ bs(Hours.Work_lead1_10) +",
      paste(baselinevars,
            collapse = "+")
    )
  )))
}
main = "Fatigue"
ylab = "Fatigue (SD)"
# clean oven
rm(out_m)
rm(pool_m)
rm(out_ct)
# bake
out_m <- out_f()
out_m
## contrasts
out_ct <- pool_stglm_contrast(out_m, df = df, m = m,  X = X, x = c, r= r)
out_ct
# graph of contrasts
ggplot_stglm_contrast(out_ct, ylim, main, xlab, ylab)

# table
out_ct %>%
  kbl(format = "markdown", booktabs = T, digits = 3)

# g-computation
pool_m <- pool_stglm(out_m, df = df, m = m, x = x,  X = X)
pool_m %>%
  kbl(format = "markdown", booktabs = T, digits = 3)
# graph
ggplot_stglm(pool_m, ylim = ylim, main, xlab, ylab, min = min, p=p, r=r)
# evalues
round( EValue::evalues.OLS( , se = , sd = 1, delta = 4, true = 0), 3)
round( EValue::evalues.RR( , lo =  , hi =, true = 1), 4)

# alcohol freq ------------------------------------------------------------
out_f = function(formula) {
  with(w_m, glm(as.formula(
    paste(
      "Alcohol.Frequency_lead2ord_z~ bs(Hours.Work_lead1_10) +",
      paste(baselinevars, collapse = "+")
    )
  )))
}
main = "Alcohol Frequency"
ylab = "Alcohol Frequency (SD)"
# clean oven
rm(out_m)
rm(pool_m)
rm(out_ct)
# bake
out_m <- out_f()
out_m
## contrasts
out_ct <- pool_stglm_contrast(out_m, df = df, m = m,  X = X, x = c, r= r)
out_ct
# graph of contrasts
ggplot_stglm_contrast(out_ct, ylim, main, xlab, ylab)

# table
out_ct %>%
  kbl(format = "markdown", booktabs = T, digits = 3)

# g-computation
pool_m <- pool_stglm(out_m, df = df, m = m, x = x,  X = X)
pool_m %>%
  kbl(format = "markdown", booktabs = T, digits = 3)
# graph
ggplot_stglm(pool_m, ylim = ylim, main, xlab, ylab, min = min, p=p, r=r)
# evalues
round( EValue::evalues.OLS( , se = , sd = 1, delta = 4, true = 0), 3)
round( EValue::evalues.RR( , lo =  , hi =, true = 1), 4)


# Alcohol.Intensity ----------------------------------------------------------
# fit
out_f = function(formula) {
  with(w_m, glm(as.formula(
    paste(
      "Alcohol.Intensity_log_lead2_z~ bs(Hours.Work_lead1_10) +",
      paste(baselinevars,
            collapse = "+")
    )
  )))
}
main = "Alcohol Intensity"
ylab = "Alcohol Intensity (SD)"
# clean oven
rm(out_m)
rm(pool_m)
rm(out_ct)
# bake
out_m <- out_f()
out_m
## contrasts
out_ct <- pool_stglm_contrast(out_m, df = df, m = m,  X = X, x = c, r= r)
out_ct
# graph of contrasts
ggplot_stglm_contrast(out_ct, ylim, main, xlab, ylab)

# table
out_ct %>%
  kbl(format = "markdown", booktabs = T, digits = 3)

# g-computation
pool_m <- pool_stglm(out_m, df = df, m = m, x = x,  X = X)
pool_m %>%
  kbl(format = "markdown", booktabs = T, digits = 3)
# graph
ggplot_stglm(pool_m, ylim = ylim, main, xlab, ylab, min = min, p=p, r=r)
# evalues
round( EValue::evalues.OLS( , se = , sd = 1, delta = 4, true = 0), 3)
round( EValue::evalues.RR( , lo =  , hi =, true = 1), 4)

# body satisfaction -------------------------------------------------------
# fit
out_f = function(formula) {
  with(w_m, glm(as.formula(
    paste(
      "Bodysat_lead2_z~ bs(Hours.Work_lead1_10) +",
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
rm(pool_m)
rm(out_ct)
# bake
out_m <- out_f()
out_m
## contrasts
out_ct <- pool_stglm_contrast(out_m, df = df, m = m,  X = X, x = c, r= r)
out_ct
# graph of contrasts
ggplot_stglm_contrast(out_ct, ylim, main, xlab, ylab)

# table
out_ct %>%
  kbl(format = "markdown", booktabs = T, digits = 3)

# g-computation
pool_m <- pool_stglm(out_m, df = df, m = m, x = x,  X = X)
pool_m %>%
  kbl(format = "markdown", booktabs = T, digits = 3)
# graph
ggplot_stglm(pool_m, ylim = ylim, main, xlab, ylab, min = min, p=p, r=r)
# evalues
round( EValue::evalues.OLS( , se = , sd = 1, delta = 4, true = 0), 3)
round( EValue::evalues.RR( , lo =  , hi =, true = 1), 4)

# rumination --------------------------------------------------------------
# During the last 30 days, how often did.... you have negative thoughts that repeated over and over?
# fit
out_f = function(formula) {
  with(w_m, glm(as.formula(
    paste(
      "Rumination_lead2ord_z~ bs(Hours.Work_lead1_10) +",
      paste(baselinevars,
            collapse = "+")
    )
  )))
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
out_m
## contrasts
out_ct <- pool_stglm_contrast(out_m, df = df, m = m,  X = X, x = c, r= r)
out_ct
# graph of contrasts
ggplot_stglm_contrast(out_ct, ylim, main, xlab, ylab)

# table
out_ct %>%
  kbl(format = "markdown", booktabs = T, digits = 3)

# g-computation
pool_m <- pool_stglm(out_m, df = df, m = m, x = x,  X = X)
pool_m %>%
  kbl(format = "markdown", booktabs = T, digits = 3)
# graph
ggplot_stglm(pool_m, ylim = ylim, main, xlab, ylab, min = min, p=p, r=r)
# evalues
round( EValue::evalues.OLS( , se = , sd = 1, delta = 4, true = 0), 3)
round( EValue::evalues.RR( , lo =  , hi =, true = 1), 4)

# sex satisfaction --------------------------------------------------------
## fit
out_f = function(formula) {
  with(w_m, glm(as.formula(
    paste(
      "SexualSatisfaction_lead2_z~ bs(Hours.Work_lead1_10) +",
      paste(baselinevars, collapse = "+")
    )
  )))
}
main = "Sexual Satisfaction (SD)"
ylab = "Sexual Satisfaction (SD)"
# clean oven
rm(out_m)
rm(pool_m)
rm(out_ct)
# bake
out_m <- out_f()
out_m
## contrasts
out_ct <- pool_stglm_contrast(out_m, df = df, m = m,  X = X, x = c, r= r)
out_ct
# graph of contrasts
ggplot_stglm_contrast(out_ct, ylim, main, xlab, ylab)

# table
out_ct %>%
  kbl(format = "markdown", booktabs = T, digits = 3)

# g-computation
pool_m <- pool_stglm(out_m, df = df, m = m, x = x,  X = X)
pool_m %>%
  kbl(format = "markdown", booktabs = T, digits = 3)
# graph
ggplot_stglm(pool_m, ylim = ylim, main, xlab, ylab, min = min, p=p, r=r)
# evalues
round( EValue::evalues.OLS( , se = , sd = 1, delta = 4, true = 0), 3)
round( EValue::evalues.RR( , lo =  , hi =, true = 1), 4)

# emotional regulation 1 ----------------------------------------------------
# When I feel negative emotions, my emotions feel out of control.
## fit
out_f = function(formula) {
  with(w_m, glm(as.formula(
    paste(
      "EmotionRegulation1_lead2_z~ bs(Hours.Work_lead1_10) +",
      paste(baselinevars, collapse = "+")
    )
  )))
}
main = "Emotion Regulation1"
ylab = "Emotion Regulation1 (SD)"
# clean oven
rm(out_m)
rm(pool_m)
rm(out_ct)
# bake
out_m <- out_f()
out_m
## contrasts
out_ct <- pool_stglm_contrast(out_m, df = df, m = m,  X = X, x = c, r= r)
out_ct
# graph of contrasts
ggplot_stglm_contrast(out_ct, ylim, main, xlab, ylab)

# table
out_ct %>%
  kbl(format = "markdown", booktabs = T, digits = 3)

# g-computation
pool_m <- pool_stglm(out_m, df = df, m = m, x = x,  X = X)
pool_m %>%
  kbl(format = "markdown", booktabs = T, digits = 3)
# graph
ggplot_stglm(pool_m, ylim = ylim, main, xlab, ylab, min = min, p=p, r=r)
# evalues
round( EValue::evalues.OLS( , se = , sd = 1, delta = 4, true = 0), 3)
round( EValue::evalues.RR( , lo =  , hi =, true = 1), 4)

# emotional reg 2 ---------------------------------------------------------
# When I feel negative emotions, I suppress or hide my emotions.
## fit
out_f = function(formula) {
  with(w_m, glm(as.formula(
    paste(
      "EmotionRegulation2_lead2_z~ bs(Hours.Work_lead1_10) +",
      paste(baselinevars, collapse = "+")
    )
  )))
}
main = "Emotion Regulation2"
ylab = "Emotion Regulation2 (SD)"
# clean oven
rm(out_m)
rm(pool_m)
rm(out_ct)
# bake
out_m <- out_f()
out_m
## contrasts
out_ct <- pool_stglm_contrast(out_m, df = df, m = m,  X = X, x = c, r= r)
out_ct
# graph of contrasts
ggplot_stglm_contrast(out_ct, ylim, main, xlab, ylab)

# table
out_ct %>%
  kbl(format = "markdown", booktabs = T, digits = 3)

# g-computation
pool_m <- pool_stglm(out_m, df = df, m = m, x = x,  X = X)
pool_m %>%
  kbl(format = "markdown", booktabs = T, digits = 3)
# graph
ggplot_stglm(pool_m, ylim = ylim, main, xlab, ylab, min = min, p=p, r=r)
# evalues
round( EValue::evalues.OLS( , se = , sd = 1, delta = 4, true = 0), 3)
round( EValue::evalues.RR( , lo =  , hi =, true = 1), 4)


# emotional reg 3 ---------------------------------------------------------
# When I feel negative emotions, I change the way I think to help me stay calm.

## fit
out_f = function(formula) {
  with(w_m, glm(as.formula(
    paste(
      "EmotionRegulation3_lead2_z~ bs(Hours.Work_lead1_10) +",
      paste(baselinevars, collapse = "+")
    )
  )))
}

main = "Emotion Regulation3"
ylab = "Emotion Regulation3 (SD)"
# clean oven
rm(out_m)
rm(pool_m)
rm(out_ct)
# bake
out_m <- out_f()
out_m
## contrasts
out_ct <- pool_stglm_contrast(out_m, df = df, m = m,  X = X, x = c, r= r)
out_ct
# graph of contrasts
ggplot_stglm_contrast(out_ct, ylim, main, xlab, ylab)

# table
out_ct %>%
  kbl(format = "markdown", booktabs = T, digits = 3)

# g-computation
pool_m <- pool_stglm(out_m, df = df, m = m, x = x,  X = X)
pool_m %>%
  kbl(format = "markdown", booktabs = T, digits = 3)
# graph
ggplot_stglm(pool_m, ylim = ylim, main, xlab, ylab, min = min, p=p, r=r)
# evalues
round( EValue::evalues.OLS( , se = , sd = 1, delta = 4, true = 0), 3)
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
  with(w_m, glm(as.formula(
    paste(
      "KESSLER6sum_lead2_z~ bs(Hours.Work_lead1_10) +",
      paste(baselinevars,
            collapse = "+")
    )
  )))
}
main = "Kessler 6 Distress"
ylab = "Kessler 6 Distress (SD)"
# clean oven
rm(out_m)
rm(pool_m)
rm(out_ct)
# bake
out_m <- out_f()
out_m
## contrasts
out_ct <- pool_stglm_contrast(out_m, df = df, m = m,  X = X, x = c, r= r)
out_ct
# graph of contrasts
ggplot_stglm_contrast(out_ct, ylim, main, xlab, ylab)

# table
out_ct %>%
  kbl(format = "markdown", booktabs = T, digits = 3)

# g-computation
pool_m <- pool_stglm(out_m, df = df, m = m, x = x,  X = X)
pool_m %>%
  kbl(format = "markdown", booktabs = T, digits = 3)
# graph
ggplot_stglm(pool_m, ylim = ylim, main, xlab, ylab, min = min, p=p, r=r)
# evalues
round( EValue::evalues.OLS( , se = , sd = 1, delta = 4, true = 0), 3)
round( EValue::evalues.RR( , lo =  , hi =, true = 1), 4)

# power dependence 1 ------------------------------------------------------
# I do not have enough power or control over important parts of my life.
## fit
out_f = function(formula) {
  with(w_m, glm(as.formula(
    paste(
      "POWERDEPENDENCE1_lead2_z~ bs(Hours.Work_lead1_10) +",
      paste(baselinevars,
            collapse = "+")
    )
  )))
}

main = "Power Dependence 1"
ylab = "Power Dependence 1(SD)"
# clean oven
rm(out_m)
rm(pool_m)
rm(out_ct)
# bake
out_m <- out_f()
out_m
## contrasts
out_ct <- pool_stglm_contrast(out_m, df = df, m = m,  X = X, x = c, r= r)
out_ct
# graph of contrasts
ggplot_stglm_contrast(out_ct, ylim, main, xlab, ylab)

# table
out_ct %>%
  kbl(format = "markdown", booktabs = T, digits = 3)

# g-computation
pool_m <- pool_stglm(out_m, df = df, m = m, x = x,  X = X)
pool_m %>%
  kbl(format = "markdown", booktabs = T, digits = 3)
# graph
ggplot_stglm(pool_m, ylim = ylim, main, xlab, ylab, min = min, p=p, r=r)
# evalues
round( EValue::evalues.OLS( , se = , sd = 1, delta = 4, true = 0), 3)
round( EValue::evalues.RR( , lo =  , hi =, true = 1), 4)

# power dependence 2 ------------------------------------------------------
## fit
#Other people have too much power or control over important parts of my life.

out_f = function(formula) {
  with(w_m, glm(as.formula(
    paste(
      "POWERDEPENDENCE2_lead2_z~ bs(Hours.Work_lead1_10) +",
      paste(baselinevars,
            collapse = "+")
    )
  )))
}
main = "Power Dependence 2"
ylab = "Power Dependence 2(SD)"
# clean oven
rm(out_m)
rm(pool_m)
rm(out_ct)
# bake
out_m <- out_f()
out_m
## contrasts
out_ct <- pool_stglm_contrast(out_m, df = df, m = m,  X = X, x = c, r= r)
out_ct
# graph of contrasts
ggplot_stglm_contrast(out_ct, ylim, main, xlab, ylab)

# table
out_ct %>%
  kbl(format = "markdown", booktabs = T, digits = 3)

# g-computation
pool_m <- pool_stglm(out_m, df = df, m = m, x = x,  X = X)
pool_m %>%
  kbl(format = "markdown", booktabs = T, digits = 3)
# graph
ggplot_stglm(pool_m, ylim = ylim, main, xlab, ylab, min = min, p=p, r=r)
# evalues
round( EValue::evalues.OLS( , se = , sd = 1, delta = 4, true = 0), 3)
round( EValue::evalues.RR( , lo =  , hi =, true = 1), 4)

# perfectionism  ----------------------------------------------------------
# Perfectionism Discrepancy Subscale
# Doing my best never seems to be enough.
# My performance rarely measures up to my standards.
# I am hardly ever satisfied with my performance.

## fit
out_f = function(formula) {
  with(w_m, glm(as.formula(
    paste(
      "PERFECTIONISM_lead2_z~ bs(Hours.Work_lead1_10) +",
      paste(baselinevars,
            collapse = "+")
    )
  )))
}
main = "Perfectionism"
ylab = "Perfectionism (SD)"
# clean oven
rm(out_m)
rm(pool_m)
rm(out_ct)
# bake
out_m <- out_f()
out_m
## contrasts
out_ct <- pool_stglm_contrast(out_m, df = df, m = m,  X = X, x = c, r= r)
out_ct
# graph of contrasts
ggplot_stglm_contrast(out_ct, ylim, main, xlab, ylab)

# table
out_ct %>%
  kbl(format = "markdown", booktabs = T, digits = 3)

# g-computation
pool_m <- pool_stglm(out_m, df = df, m = m, x = x,  X = X)
pool_m %>%
  kbl(format = "markdown", booktabs = T, digits = 3)
# graph
ggplot_stglm(pool_m, ylim = ylim, main, xlab, ylab, min = min, p=p, r=r)
# evalues
round( EValue::evalues.OLS( , se = , sd = 1, delta = 4, true = 0), 3)
round( EValue::evalues.RR( , lo =  , hi =, true = 1), 4)

# self esteem -------------------------------------------------------------
# Self-esteem
# On the whole am satisfied with myself.
# Take a positive attitude toward myself.
# Am inclined to feel that I am a failure.

## fit
out_f = function(formula) {
  with(w_m, glm(as.formula(
    paste(
      "SELF.ESTEEM_lead2_z~ bs(Hours.Work_lead1_10) +",
      paste(baselinevars,
            collapse = "+")
    )
  )))
}
main = "Self Esteem"
ylab = "Self Esteem (SD)"
# clean oven
rm(out_m)
rm(pool_m)
rm(out_ct)
# bake
out_m <- out_f()
out_m
## contrasts
out_ct <- pool_stglm_contrast(out_m, df = df, m = m,  X = X, x = c, r= r)
out_ct
# graph of contrasts
ggplot_stglm_contrast(out_ct, ylim, main, xlab, ylab)

# table
out_ct %>%
  kbl(format = "markdown", booktabs = T, digits = 3)

# g-computation
pool_m <- pool_stglm(out_m, df = df, m = m, x = x,  X = X)
pool_m %>%
  kbl(format = "markdown", booktabs = T, digits = 3)
# graph
ggplot_stglm(pool_m, ylim = ylim, main, xlab, ylab, min = min, p=p, r=r)
# evalues
round( EValue::evalues.OLS( , se = , sd = 1, delta = 4, true = 0), 3)
round( EValue::evalues.RR( , lo =  , hi =, true = 1), 4)

# gratitude ---------------------------------------------------------------
# Gratitude
# I have much in my life to be thankful for.
# When I look at the world, I don’t see much to be grateful for.
# I am grateful to a wide variety of people.

## fit
out_f = function(formula) {
  with(w_m, glm(as.formula(
    paste(
      "GRATITUDE_lead2_z~ bs(Hours.Work_lead1_10) +",
      paste(baselinevars,
            collapse = "+")
    )
  )))
}
main = "Gratitude"
ylab = "Gratitude (SD)"
# clean oven
rm(out_m)
rm(pool_m)
rm(out_ct)
# bake
out_m <- out_f()
out_m
## contrasts
out_ct <- pool_stglm_contrast(out_m, df = df, m = m,  X = X, x = c, r= r)
out_ct
# graph of contrasts
ggplot_stglm_contrast(out_ct, ylim, main, xlab, ylab)

# table
out_ct %>%
  kbl(format = "markdown", booktabs = T, digits = 3)

# g-computation
pool_m <- pool_stglm(out_m, df = df, m = m, x = x,  X = X)
pool_m %>%
  kbl(format = "markdown", booktabs = T, digits = 3)
# graph
ggplot_stglm(pool_m, ylim = ylim, main, xlab, ylab, min = min, p=p, r=r)
# evalues
round( EValue::evalues.OLS( , se = , sd = 1, delta = 4, true = 0), 3)
round( EValue::evalues.RR( , lo =  , hi =, true = 1), 4)

# veng rumination ---------------------------------------------------------
# Forgivingness versus Vengeful Rumination
# Sometimes I can't sleep because of thinking about past wrongs I have suffered.
# I can usually forgive and forget when someone does me wrong.
# I find myself regularly thinking about past times that I have been wronged.

## fit
out_f = function(formula) {
  with(w_m, glm(as.formula(
    paste(
      "VENGEFUL.RUMIN_lead2_z~ bs(Hours.Work_lead1_10) +",
      paste(baselinevars,
            collapse = "+")
    )
  )))
}
main = "Vengefulness (anti-Foregiveness)"
ylab = "Vengefulness (anti-Foregiveness) (SD)"
# clean oven
rm(out_m)
rm(pool_m)
rm(out_ct)
# bake
out_m <- out_f()
out_m
## contrasts
out_ct <- pool_stglm_contrast(out_m, df = df, m = m,  X = X, x = c, r= r)
out_ct
# graph of contrasts
ggplot_stglm_contrast(out_ct, ylim, main, xlab, ylab)

# table
out_ct %>%
  kbl(format = "markdown", booktabs = T, digits = 3)

# g-computation
pool_m <- pool_stglm(out_m, df = df, m = m, x = x,  X = X)
pool_m %>%
  kbl(format = "markdown", booktabs = T, digits = 3)
# graph
ggplot_stglm(pool_m, ylim = ylim, main, xlab, ylab, min = min, p=p, r=r)
# evalues
round( EValue::evalues.OLS( , se = , sd = 1, delta = 4, true = 0), 3)
round( EValue::evalues.RR( , lo =  , hi =, true = 1), 4)


# life meaning ------------------------------------------------------------

# Meaning in Life
# My life has a clear sense of purpose.
# I have a good sense of what makes my life meaningful.

out_f = function(formula) {
  with(w_m, glm(as.formula(
    paste(
      "LIFEMEANING_lead2ord_z~ bs(Hours.Work_lead1_10) +",
      paste(baselinevars,
            collapse = "+")
    )
  )))
}
main = "Life Meaning"
ylab = "Life Meaning (SD)"
# clean oven
rm(out_m)
rm(pool_m)
rm(out_ct)
# bake
out_m <- out_f()
out_m
## contrasts
out_ct <- pool_stglm_contrast(out_m, df = df, m = m,  X = X, x = c, r= r)
out_ct
# graph of contrasts
ggplot_stglm_contrast(out_ct, ylim, main, xlab, ylab)

# table
out_ct %>%
  kbl(format = "markdown", booktabs = T, digits = 3)

# g-computation
pool_m <- pool_stglm(out_m, df = df, m = m, x = x,  X = X)
pool_m %>%
  kbl(format = "markdown", booktabs = T, digits = 3)
# graph
ggplot_stglm(pool_m, ylim = ylim, main, xlab, ylab, min = min, p=p, r=r)
# evalues
round( EValue::evalues.OLS( , se = , sd = 1, delta = 4, true = 0), 3)
round( EValue::evalues.RR( , lo =  , hi =, true = 1), 4)

# honesty humility --------------------------------------------------------

# Mini-IPIP6 Honesty-Humility (item overlap with Psychological Entitlement)
# Would like to be seen driving around in a very expensive car.
# Would get a lot of pleasure from owning expensive luxury goods.
# Feel entitled to more of everything.
# Deserve more things in life.

## fit
out_f = function(formula) {
  with(w_m, glm(as.formula(
    paste(
      "HONESTY_HUMILITY_lead2_z~ bs(Hours.Work_lead1_10) +",
      paste(baselinevars,
            collapse = "+")
    )
  )))
}
main = "Honesty Humility"
ylab = "Honesty Humility (SD)"
# clean oven
rm(out_m)
rm(pool_m)
rm(out_ct)
# bake
out_m <- out_f()
out_m
## contrasts
out_ct <- pool_stglm_contrast(out_m, df = df, m = m,  X = X, x = c, r= r)
out_ct
# graph of contrasts
ggplot_stglm_contrast(out_ct, ylim, main, xlab, ylab)

# table
out_ct %>%
  kbl(format = "markdown", booktabs = T, digits = 3)

# g-computation
pool_m <- pool_stglm(out_m, df = df, m = m, x = x,  X = X)
pool_m %>%
  kbl(format = "markdown", booktabs = T, digits = 3)
# graph
ggplot_stglm(pool_m, ylim = ylim, main, xlab, ylab, min = min, p=p, r=r)
# evalues
round( EValue::evalues.OLS( , se = , sd = 1, delta = 4, true = 0), 3)
round( EValue::evalues.RR( , lo =  , hi =, true = 1), 4)

# belonging ---------------------------------------------------------------
# Felt belongingness
# Know that people in my life accept and value me.
# Feel like an outsider.
# Know that people around me share my attitudes and beliefs.

out_f = function(formula) {
  with(w_m, glm(as.formula(
    paste(
      "BELONG_lead2_z~ bs(Hours.Work_lead1_10) +",
      paste(baselinevars,
            collapse = "+")
    )
  )))
}
main = "Social Belonging"
ylab = "Social Belonging (SD)"
# clean oven
rm(out_m)
rm(pool_m)
rm(out_ct)
# bake
out_m <- out_f()
out_m
## contrasts
out_ct <- pool_stglm_contrast(out_m, df = df, m = m,  X = X, x = c, r= r)
out_ct
# graph of contrasts
ggplot_stglm_contrast(out_ct, ylim, main, xlab, ylab)

# table
out_ct %>%
  kbl(format = "markdown", booktabs = T, digits = 3)

# g-computation
pool_m <- pool_stglm(out_m, df = df, m = m, x = x,  X = X)
pool_m %>%
  kbl(format = "markdown", booktabs = T, digits = 3)
# graph
ggplot_stglm(pool_m, ylim = ylim, main, xlab, ylab, min = min, p=p, r=r)
# evalues
round( EValue::evalues.OLS( , se = , sd = 1, delta = 4, true = 0), 3)
round( EValue::evalues.RR( , lo =  , hi =, true = 1), 4)

# soc support -------------------------------------------------------------

# Perceived social support
# There are people I can depend on to help me if I really need it.
# There is no one I can turn to for guidance in times of stress.
# I know there are people I can turn to when I need help.

## fit
out_f = function(formula) {
  with(w_m, glm(as.formula(
    paste(
      "SUPPORT_lead2_z~ bs(Hours.Work_lead1_10) +",
      paste(baselinevars,
            collapse = "+")
    )
  )))
}
main = "Social Support"
ylab = "Social Support (SD)"
# clean oven
rm(out_m)
rm(pool_m)
rm(out_ct)
# bake
out_m <- out_f()
out_m
## contrasts
out_ct <- pool_stglm_contrast(out_m, df = df, m = m,  X = X, x = c, r= r)
out_ct
# graph of contrasts
ggplot_stglm_contrast(out_ct, ylim, main, xlab, ylab)

# table
out_ct %>%
  kbl(format = "markdown", booktabs = T, digits = 3)

# g-computation
pool_m <- pool_stglm(out_m, df = df, m = m, x = x,  X = X)
pool_m %>%
  kbl(format = "markdown", booktabs = T, digits = 3)
# graph
ggplot_stglm(pool_m, ylim = ylim, main, xlab, ylab, min = min, p=p, r=r)
# evalues
round( EValue::evalues.OLS( , se = , sd = 1, delta = 4, true = 0), 3)
round( EValue::evalues.RR( , lo =  , hi =, true = 1), 4)

# volunteers --------------------------------------------------------------
# fit
out_f = function(formula) {
  with(w_m, glm(as.formula(
    paste(
      "Volunteers_lead2~ bs(Hours.Work_lead1_10) +",
      paste(baselinevars,
            collapse = "+")
    )
  )))
}
main = "Volunteer Rate"
ylab = "Volunteer Rate"
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
ggplot_stglm_contrast(out_ct, ylim, main, xlab, ylab)

# table
out_ct %>%
  kbl(format = "markdown", booktabs = T, digits = 3)

# g-computation
pool_m <- pool_stglm_contrast_ratio(out_m, df = df, m = m, x = x,  X = X)
pool_m %>%
  kbl(format = "markdown", booktabs = T, digits = 3)
# graph
ggplot_stglm(pool_m, ylim = ylim, main, xlab, ylab, min = min, p=p, r=r)
# evalues
round( EValue::evalues.OLS( , se = , sd = 1, delta = 4, true = 0), 3)
round( EValue::evalues.RR( , lo =  , hi =, true = 1), 4)

# charity donate ----------------------------------------------------------
## fit
out_f = function(formula) {
  with(w_m, glm(as.formula(
    paste(
      "CharityDonate_log_lead2_z~ bs(Hours.Work_lead1_10) +",
      paste(baselinevars,
            collapse = "+")
    )
  )))
}
main = "Charity Donatations (annual SD)"
ylab = "Charity Donatations (annual SD)"
# clean oven
rm(out_m)
rm(pool_m)
rm(out_ct)
# bake
out_m <- out_f()
out_m
## contrasts
out_ct <- pool_stglm_contrast(out_m, df = df, m = m,  X = X, x = c, r= r)
out_ct
# graph of contrasts
ggplot_stglm_contrast(out_ct, ylim, main, xlab, ylab)

# table
out_ct %>%
  kbl(format = "markdown", booktabs = T, digits = 3)

# g-computation
pool_m <- pool_stglm(out_m, df = df, m = m, x = x,  X = X)
pool_m %>%
  kbl(format = "markdown", booktabs = T, digits = 3)
# graph
ggplot_stglm(pool_m, ylim = ylim, main, xlab, ylab, min = min, p=p, r=r)
# evalues
round( EValue::evalues.OLS( , se = , sd = 1, delta = 4, true = 0), 3)
round( EValue::evalues.RR( , lo =  , hi =, true = 1), 4)


# community lead ----------------------------------------------------------
# Sense of community
# I feel a sense of community with others in my local neighbourhood.


## fit
out_f = function(formula) {
  with(w_m, glm(as.formula(
    paste(
      "community_lead2_z~ bs(Hours.Work_lead1_10) +",
      paste(baselinevars,
            collapse = "+")
    )
  )))
}
main = "Community"
ylab = "Community (SD)"
# clean oven
rm(out_m)
rm(pool_m)
rm(out_ct)
# bake
out_m <- out_f()
out_m
## contrasts
out_ct <- pool_stglm_contrast(out_m, df = df, m = m,  X = X, x = c, r= r)
out_ct
# graph of contrasts
ggplot_stglm_contrast(out_ct, ylim, main, xlab, ylab)

# table
out_ct %>%
  kbl(format = "markdown", booktabs = T, digits = 3)

# g-computation
pool_m <- pool_stglm(out_m, df = df, m = m, x = x,  X = X)
pool_m %>%
  kbl(format = "markdown", booktabs = T, digits = 3)
# graph
ggplot_stglm(pool_m, ylim = ylim, main, xlab, ylab, min = min, p=p, r=r)
# evalues
round( EValue::evalues.OLS( , se = , sd = 1, delta = 4, true = 0), 3)
round( EValue::evalues.RR( , lo =  , hi =, true = 1), 4)

# national wellbeing ------------------------------------------------------

# National Wellbeing Index
# The economic situation in New Zealand.
# The social conditions in New Zealand.
# Business in New Zealand.

## fit
out_f = function(formula) {
  with(w_m, glm(as.formula(
    paste(
      "NWI_lead2_z~ bs(Hours.Work_lead1_10) +",
      paste(baselinevars,
            collapse = "+")
    )
  )))
}

main = "National Well Being"
ylab = "National Well Being (SD)"
# clean oven
rm(out_m)
rm(pool_m)
rm(out_ct)
# bake
out_m <- out_f()
out_m
## contrasts
out_ct <- pool_stglm_contrast(out_m, df = df, m = m,  X = X, x = c, r= r)
out_ct
# graph of contrasts
ggplot_stglm_contrast(out_ct, ylim, main, xlab, ylab)

# table
out_ct %>%
  kbl(format = "markdown", booktabs = T, digits = 3)

# g-computation
pool_m <- pool_stglm(out_m, df = df, m = m, x = x,  X = X)
pool_m %>%
  kbl(format = "markdown", booktabs = T, digits = 3)
# graph
ggplot_stglm(pool_m, ylim = ylim, main, xlab, ylab, min = min, p=p, r=r)
# evalues
round( EValue::evalues.OLS( , se = , sd = 1, delta = 4, true = 0), 3)
round( EValue::evalues.RR( , lo =  , hi =, true = 1), 4)

# imperm group ------------------------------------------------------------

out_f = function(formula) {
  with(w_m, glm(as.formula(
    paste(
      "ImpermeabilityGroup_lead2_z~ bs(Hours.Work_lead1_10) +",
      paste(baselinevars,
            collapse = "+")
    )
  )))
}
main = "Impermeability Group"
ylab = "Impermeability Group (SD)"
# clean oven
rm(out_m)
rm(pool_m)
rm(out_ct)
# bake
out_m <- out_f()
out_m
## contrasts
out_ct <- pool_stglm_contrast(out_m, df = df, m = m,  X = X, x = c, r= r)
out_ct
# graph of contrasts
ggplot_stglm_contrast(out_ct, ylim, main, xlab, ylab)

# table
out_ct %>%
  kbl(format = "markdown", booktabs = T, digits = 3)

# g-computation
pool_m <- pool_stglm(out_m, df = df, m = m, x = x,  X = X)
pool_m %>%
  kbl(format = "markdown", booktabs = T, digits = 3)
# graph
ggplot_stglm(pool_m, ylim = ylim, main, xlab, ylab, min = min, p=p, r=r)
# evalues
round( EValue::evalues.OLS( , se = , sd = 1, delta = 4, true = 0), 3)
round( EValue::evalues.RR( , lo =  , hi =, true = 1), 4)


# stand living ------------------------------------------------------------
# Part of pwi
# Personal Wellbeing Index
# Your health.
# Your standard of living.
# Your future security.
# Your personal relationships.

## fit
out_f = function(formula) {
  with(w_m, glm(as.formula(
    paste(
      "Standard.Living_lead2ord_z~ bs(Hours.Work_lead1_10) +",
      paste(baselinevars,
            collapse = "+")
    )
  )))
}
main = "Standard Living"
ylab = "Standard Living (SD)"
# clean oven
rm(out_m)
rm(pool_m)
rm(out_ct)
# bake
out_m <- out_f()
out_m
## contrasts
out_ct <- pool_stglm_contrast(out_m, df = df, m = m,  X = X, x = c, r= r)
out_ct
# graph of contrasts
ggplot_stglm_contrast(out_ct, ylim, main, xlab, ylab)

# table
out_ct %>%
  kbl(format = "markdown", booktabs = T, digits = 3)

# g-computation
pool_m <- pool_stglm(out_m, df = df, m = m, x = x,  X = X)
pool_m %>%
  kbl(format = "markdown", booktabs = T, digits = 3)
# graph
ggplot_stglm(pool_m, ylim = ylim, main, xlab, ylab, min = min, p=p, r=r)
# evalues
round( EValue::evalues.OLS( , se = , sd = 1, delta = 4, true = 0), 3)
round( EValue::evalues.RR( , lo =  , hi =, true = 1), 4)


# future security ---------------------------------------------------------

## fit
out_f = function(formula) {
  with(w_m, glm(as.formula(
    paste(
      "Your.Future.Security_lead2_z~ bs(Hours.Work_lead1_10) +",
      paste(baselinevars,
            collapse = "+")
    )
  )))
}
main = "Your Future Security"
ylab = "Your Future Security (SD)"
# clean oven
rm(out_m)
rm(pool_m)
rm(out_ct)
# bake
out_m <- out_f()
out_m
## contrasts
out_ct <- pool_stglm_contrast(out_m, df = df, m = m,  X = X, x = c, r= r)
out_ct
# graph of contrasts
ggplot_stglm_contrast(out_ct, ylim, main, xlab, ylab)

# table
out_ct %>%
  kbl(format = "markdown", booktabs = T, digits = 3)

# g-computation
pool_m <- pool_stglm(out_m, df = df, m = m, x = x,  X = X)
pool_m %>%
  kbl(format = "markdown", booktabs = T, digits = 3)
# graph
ggplot_stglm(pool_m, ylim = ylim, main, xlab, ylab, min = min, p=p, r=r)
# evalues
round( EValue::evalues.OLS( , se = , sd = 1, delta = 4, true = 0), 3)
round( EValue::evalues.RR( , lo =  , hi =, true = 1), 4)

# your health -------------------------------------------------------------

## fit
out_f = function(formula) {
  with(w_m, glm(as.formula(
    paste(
      "Your.Health_lead2_z~ bs(Hours.Work_lead1_10) +",
      paste(baselinevars,
            collapse = "+")
    )
  )))
}
main = "Your Health"
ylab = "Your Health (SD)"
# clean oven
rm(out_m)
rm(pool_m)
rm(out_ct)
# bake
out_m <- out_f()
out_m
## contrasts
out_ct <- pool_stglm_contrast(out_m, df = df, m = m,  X = X, x = c, r= r)
out_ct
# graph of contrasts
ggplot_stglm_contrast(out_ct, ylim, main, xlab, ylab)

# table
out_ct %>%
  kbl(format = "markdown", booktabs = T, digits = 3)

# g-computation
pool_m <- pool_stglm(out_m, df = df, m = m, x = x,  X = X)
pool_m %>%
  kbl(format = "markdown", booktabs = T, digits = 3)
# graph
ggplot_stglm(pool_m, ylim = ylim, main, xlab, ylab, min = min, p=p, r=r)
# evalues
round( EValue::evalues.OLS( , se = , sd = 1, delta = 4, true = 0), 3)
round( EValue::evalues.RR( , lo =  , hi =, true = 1), 4)

# personal relationships --------------------------------------------------

## fit
out_f = function(formula) {
  with(w_m, glm(as.formula(
    paste(
      "Your.Personal.Relationships_lead2ord_z~ bs(Hours.Work_lead1_10) +",
      paste(baselinevars, collapse = "+")
    )
  )))
}

main = "Your Personal Relationships"
ylab = "Your Personal Relationships (SD)"
# clean oven
rm(out_m)
rm(pool_m)
rm(out_ct)
# bake
out_m <- out_f()
out_m
## contrasts
out_ct <- pool_stglm_contrast(out_m, df = df, m = m,  X = X, x = c, r= r)
out_ct
# graph of contrasts
ggplot_stglm_contrast(out_ct, ylim, main, xlab, ylab)

# table
out_ct %>%
  kbl(format = "markdown", booktabs = T, digits = 3)

# g-computation
pool_m <- pool_stglm(out_m, df = df, m = m, x = x,  X = X)
pool_m %>%
  kbl(format = "markdown", booktabs = T, digits = 3)
# graph
ggplot_stglm(pool_m, ylim = ylim, main, xlab, ylab, min = min, p=p, r=r)
# evalues
round( EValue::evalues.OLS( , se = , sd = 1, delta = 4, true = 0), 3)
round( EValue::evalues.RR( , lo =  , hi =, true = 1), 4)

# Work-life balance -------------------------------------------------------

#fit
out_f = function(formula) {
  with(w_m, glm(as.formula(
    paste(
      "Emp.WorkLifeBalance_lead2_z~ bs(Hours.Work_lead1_10) +",
      paste(baselinevars,
            collapse = "+")
    )
  )))
}
main = "Work Life Balance"
ylab = "Work Life Balance (SD)"
# clean oven
rm(out_m)
rm(pool_m)
rm(out_ct)
# bake
out_m <- out_f()
out_m
## contrasts
out_ct <- pool_stglm_contrast(out_m, df = df, m = m,  X = X, x = c, r= r)
out_ct
# graph of contrasts
ggplot_stglm_contrast(out_ct, ylim, main, xlab, ylab)

# table
out_ct %>%
  kbl(format = "markdown", booktabs = T, digits = 3)

# g-computation
pool_m <- pool_stglm(out_m, df = df, m = m, x = x,  X = X)
pool_m %>%
  kbl(format = "markdown", booktabs = T, digits = 3)
# graph
ggplot_stglm(pool_m, ylim = ylim, main, xlab, ylab, min = min, p=p, r=r)
# evalues
round( EValue::evalues.OLS( , se = , sd = 1, delta = 4, true = 0), 3)
round( EValue::evalues.RR( , lo =  , hi =, true = 1), 4)

# life sat ----------------------------------------------------------------
# Satisfaction with life
# I am satisfied with my life.
# In most ways my life is close to ideal.

## fit
out_f = function(formula) {
  with(w_m, glm(as.formula(
    paste(
      "LIFESAT_lead2_z~ bs(Hours.Work_lead1_10) +",
      paste(baselinevars,
            collapse = "+")
    )
  )))
}
main = "Life Satisfaction"
ylab = "Life Satisfaction (SD)"
# clean oven
rm(out_m)
rm(pool_m)
rm(out_ct)
# bake
out_m <- out_f()
out_m
## contrasts
out_ct <- pool_stglm_contrast(out_m, df = df, m = m,  X = X, x = c, r= r)
out_ct
# graph of contrasts
ggplot_stglm_contrast(out_ct, ylim, main, xlab, ylab)

# table
out_ct %>%
  kbl(format = "markdown", booktabs = T, digits = 3)

# g-computation
pool_m <- pool_stglm(out_m, df = df, m = m, x = x,  X = X)
pool_m %>%
  kbl(format = "markdown", booktabs = T, digits = 3)
# graph
ggplot_stglm(pool_m, ylim = ylim, main, xlab, ylab, min = min, p=p, r=r)
# evalues
round( EValue::evalues.OLS( , se = , sd = 1, delta = 4, true = 0), 3)
round( EValue::evalues.RR( , lo =  , hi =, true = 1), 4)

# Promotion NZSEI ---------------------------------------------------------------
## fit
out_f = function(formula) {
  with(w_m, glm(as.formula(
    paste(
      "NZSEI13_lead2_10_z ~ bs(Hours.Work_lead1_10) +",
      paste(baselinevars,
            collapse = "+")
    )
  )))
}
main = "Occupational Status"
ylab = "Occupational Status (SD)"
# clean oven
rm(out_m)
rm(pool_m)
rm(out_ct)
# bake
out_m <- out_f()
out_m
## contrasts
out_ct <- pool_stglm_contrast(out_m, df = df, m = m,  X = X, x = c, r= r)
out_ct
# graph of contrasts
ggplot_stglm_contrast(out_ct, ylim, main, xlab, ylab)

# table
out_ct %>%
  kbl(format = "markdown", booktabs = T, digits = 3)

# g-computation
pool_m <- pool_stglm(out_m, df = df, m = m, x = x,  X = X)
pool_m %>%
  kbl(format = "markdown", booktabs = T, digits = 3)
# graph
ggplot_stglm(pool_m, ylim = ylim, main, xlab, ylab, min = min, p=p, r=r)
# evalues
round( EValue::evalues.OLS( , se = , sd = 1, delta = 4, true = 0), 3)
round( EValue::evalues.RR( , lo =  , hi =, true = 1), 4)

#IGNORE BELOW --------------------------------------------------------------
