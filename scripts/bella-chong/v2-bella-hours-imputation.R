# Bella hours work
# set science digits
# March 2023

options(scipen = 999)

# You can find more information about the NZAVS:[HERE](https://www.psych.auckland.ac.nz/en/about/new-zealand-attitudes-and-values-study.html)

# step one: import libraries:
source("https://raw.githubusercontent.com/go-bayes/templates/main/functions/libs2.R")


# read functions
source("https://raw.githubusercontent.com/go-bayes/templates/main/functions/funs.R")

# read data

# set your folder path : unique to each user
pull_path <-
  fs::path_expand(
    "/Users/joseph/v-project\ Dropbox/Joseph\ Bulbulia/00Bulbulia\ Pubs/2021/DATA/time13"
  )

# set path to where you will push data: unique for each user

# for saving models
push_mods <-
  fs::path_expand("/Users/joseph/v-project\ Dropbox/Joseph\ Bulbulia/outcomewide/hours_work/mods")

# for saving figures
push_figs <-
  fs::path_expand(" /Users/joseph/v-project\ Dropbox/Joseph\ Bulbulia/outcomewide/hours_work/figs")

pull_path <-
  fs::path_expand(
    "/Users/joseph/v-project\ Dropbox/Joseph\ Bulbulia/00Bulbulia\ Pubs/2021/DATA/time13"
  )

# read data: note that you need use the arrow package
dat <- arrow::read_parquet(pull_path)

# check data
str(dat)

# count unique individuals
skimr::n_unique(dat$Id)

# more robust option for counting:
length(unique(dat$Id))

# participants by wave,
dat |>
  filter(YearMeasured == 1) |>
  droplevels() |>
  arrange(Wave) |>
  group_by(Wave) |>
  count()

# checks
# increasing rate
dat |>
  group_by(Wave) |>
  summarise(mean(HLTH.Disability, na.rm = TRUE))


# for ref
# dt0 <- dat %>%
#   filter(YearMeasured == 1 & Wave == 2019) %>%
#   select(REGC_2022, COVID19.Timeline) %>%
#   mutate(cum_lockdowns_baseline = case_when(
#     COVID19.Timeline < 1.2 ~ 0,
#     COVID19.Timeline > 1.2 & COVID19.Timeline < 2 ~ 2,
#     COVID19.Timeline > 2 & REGC_2022 %in% c(1, 2) ~ 3,
#     COVID19.Timeline > 2 & REGC_2022 == 4 ~ 4,
#     TRUE ~ NA_real_
#   ))




# # example
# dat |>
#   filter(Wave == 2018 & YearMeasured==1) |>
#   select(PERFECTIONISM, Gender, Id, Wave) |>
#   drop_na() |>
#   summarise(count_distinct = n_distinct(Id))
#
#
# # another example
# dat |>
#   filter(Wave == 2018 & YearMeasured==1) |>
#   select(PERFECTIONISM, Gender, Id, Wave) |>
#   mutate(Male = as.factor(Gender)) |>
#   drop_na() |>
#   ggplot(aes(x=as.factor(Gender), y=PERFECTIONISM, colour = factor(Gender))) +
#   geom_boxplot(notch = TRUE) + geom_jitter(shape=16, position=position_jitter(0.2), alpha = .1) + labs(
#     title = "Perfectionism by Gender: NZAVS years 2018-2019, N = 47823",
#     y = "Doing my best never seems to be enough.\nMy performance rarely measures up to my standards.\nI am hardly ever satisfied with my performance.",
#     x = "Male coded as 1, other identities coded as 0") + scale_color_viridis_d(option = "D")


#
# From some initial testing, the following:
#   dplyr::mutate(org2018 = ifelse(Wave == 2018 & YearMeasured == 1, 1, 0)) |>
#   dplyr::mutate(hold18 = mean(org2018, na.rm = TRUE)) |>
#   dplyr::filter(hold18 > 0)  # hack to enable repeat of baseline
#
# Can be simplified to:
#   dplyr::filter(any(Wave == 2018 & YearMeasured == 1))


# initial wrangling to get cohort: all those who participated in both t10 and t11

dat_new <- dat |>
  arrange(Id, Wave) |>
  dplyr::mutate(Euro = if_else(EthCat == 1, 1, 0),
                SexualOrientation = as.factor(if_else(
                  SexualOrientationL1 == 1,
                  "Heterosexual",
                  if_else(SexualOrientationL1 ==
                            2, "Homosexual", "OtherSexuality")
                ))) %>%
  dplyr::mutate(Gender3 = as.factor(ifelse(
    GendAll == 0,
    "Female",
    if_else(GendAll == 1, "Male", "GenderDiverse")
  ))) |>
  dplyr::mutate(Male = as.factor(ifelse(GendAll == 1,
                                        "Male", "Not_male"))) |>
  dplyr::rename(# use this for some studies
    # My life has a clear sense of purpose.
    meaning_purpose = LifeMeaning01,
    # I have a good sense of what makes my life meaningful.
    meaning_sense = LifeMeaning02) |>
  dplyr::filter(Id != 9630) |> # problematic for income/ hours work
  dplyr::mutate(work_div10 = Hours.Work / 10) |> # create hours work variable
  dplyr::mutate(work_cat4 = if_else(
    work_div10 == 0,
    0,
    if_else(
      work_div10 > 0 & work_div10 < 2,
      1,
      if_else(work_div10 >= 2 &
                work_div10 <= 4, 2,
              3)
    )
  )) |> # create hours work variable
  dplyr::mutate(work_cat4 = if_else(
    work_div10 == 0,
    0,
    if_else(
      work_div10 > 0 & work_div10 < 2,
      1,
      if_else(work_div10 >= 2 &
                work_div10 <= 4, 2,
              3)
    )
  )) |> # create hours work variable
  dplyr::mutate(work_cat2 = if_else(work_div10 <= 2 , 0, 1)) |> # create hours work variable
  dplyr::filter((Wave == 2018  & YearMeasured  == 1) |
                  (Wave == 2019  &
                     YearMeasured  == 1) |
                  (Wave == 2020))  |>  # Eligibility criteria
  #  dplyr::filter(YearMeasured  != -1) %>% #
  droplevels() |>
  arrange(Id, Wave) |>
  mutate(Time = as.numeric(Wave)) |>
  dplyr::mutate(exposed = if_else(!is.na(work_cat2) |
                                    Wave == 2020, 1, 0)) |> # full information on exposure
  dplyr::filter(exposed == 1) |>
  ungroup() |>
  droplevels() |>
  group_by(Id) %>%
  dplyr::mutate(org2018 =  ifelse(Wave == 2018 &
                                    YearMeasured == 1, 1, 0)) %>%  # creating an indicator for the first wave
  dplyr::mutate(hold18 = mean(org2018, na.rm = TRUE)) %>%  # Hack
  dplyr::filter(hold18 > 0) %>% # hack to enable repeat of baseline
  dplyr::mutate(org2019 = ifelse(Wave == 2019 &
                                   YearMeasured == 1, 1, 0)) %>%  # creating an indicator for the first wave
  dplyr::mutate(hold19 = mean(org2019, na.rm = TRUE)) %>%  # Hack0
  dplyr::filter(hold19 > 0) %>% # hack to enable repeat of baseline in 20
  # mutate(nCensoring = ifelse(Time == max(Time), 0,   # will be tricky because censoring differs for each outcome
  #                            ifelse(
  #                              lead(YearMeasured) == -1 |
  #                                lead(YearMeasured) == 0,
  #                              0,
  #                              ifelse(is.na(lead(kessler_worthless)), 0, 1)
  #                            ))) |>
  ungroup() |>
  arrange(Id, Time) |>
  droplevels()

# check no missing in the exposure -- looks good.
# missing in outcomes means censoring case by case if we use censoring approaches.
table1::table1(~ work_cat2 + kessler_worthless |
                 Wave, data = dat_new)

# check data
hist(dat_new$work_div10)
# check data
hist(dat_new$work_cat4)
# check data
hist(dat_new$work_cat2)
hist(dat_new$Time)

# count those in this group

dat_new |>
  filter(Wave == 2018 & YearMeasured == 1) |>
  select(work_div10, work_cat4, Id, Wave) |>
  drop_na() |>
  summarise(count_distinct = n_distinct(Id))

msm::statetable.msm(round(work_cat4, 0), Id, data = dat_new) |>
  kbl() |>
  kable_paper(full_width = F)

msm::statetable.msm(round(work_cat2, 0), Id, data = dat_new) |>
  kbl() |>
  kable_paper(full_width = F)



# decision to use cat_2 based on evolutionary theory that 21 hours = normal for people.

# variables for baseline
table(is.na(dat_new$NZSEI13))
table(is.na(dat_new$NZSEI18))
table(is.na(dat_new$Rural_GCH2018))
table(is.na(dat_new$NZDep2018)) # USE THIS NOT A LOT OF NA


# inclusion criteria
# under 59 at baseline - help to min heterogeneity of treatment
#



# Do you have a health condition or disability that limits you, and that has lasted for 6+ months?

## select vars
dt_prep <- dat_new %>%
  dplyr::filter(Id != 9630) %>% # problematic
  select(
    Id,
    # nCensoring,
    Time,
    Wave,
    Partner,
    EthCat,
    Age,
    Gender3,
    SexualOrientation,
    SampleFrame,
    NZSEI13,
    # fewest NA
    NZDep2018,
    Rural_GCH2018,
    # fewest NA
    REGC_2022,
    # region and fewest NA
    CONSCIENTIOUSNESS,
    OPENNESS,
    HONESTY_HUMILITY,
    EXTRAVERSION,
    NEUROTICISM,
    AGREEABLENESS,
    Edu,
    Employed,
    #   BornNZ,
    #   HomeOwner,
    Pol.Orient,
    Household.INC,
    Parent,
    Relid,
    ChildrenNum,
    #    Religion.Church,
    #   Believe.Spirit,
    #    Believe.God,
    #    Spiritual.Identification,
  #  SWB.SoC01,
    # EmotionRegulation1,
    #  EmotionRegulation2,
    #  EmotionRegulation3,
    #  Bodysat,
#    VENGEFUL.RUMIN,
    kessler_hopeless,
    #   # …  you feel hopeless?
    kessler_depressed,
    #   #…  you feel so depressed that nothing could cheer you up?
    kessler_restless,
    #   #…  you feel restless or fidgety?
    kessler_effort,
    #   #…  you feel that everything was an effort?
    kessler_worthless,
    #   #…  you feel worthless?
    kessler_nervous,
    #…  you feel nervous?
    # KESSLER6sum,
 #   HLTH.Fatigue,
    # Rumination,
    #   retired,  selection error prone
    #   semiretired, selection error prone
    #  KESSLER6sum,
#    Rumination,
#    Smoker,
    ChildrenNum,
 #   NWI,
 #   BELONG,
 #   SUPPORT,
  #  CharityDonate,  # reduce missing
  #  HoursCharity, #educe missing
#    GRATITUDE,
    # Volunteers,
    Hours.Work,
#    HLTH.SleepHours,
#    HLTH.Disability,
#    Hours.Exercise,
    #  LIFEMEANING,
    meaning_purpose,
    meaning_sense,
#    LIFESAT,
    # PWI,  ##  we use the individual
#    NWI,
#    SFHEALTH,
#    SELF.CONTROL,
#    SFHEALTH,
#   SELF.ESTEEM,
    # Respect.Self, heavy NA
    #  GenCohort,
 #   SELF.ESTEEM,
 #   SELF.CONTROL,
    #  Respect.Self,
    # Emp.WorkLifeBalance, not at baseline
#    Alcohol.Frequency,
#    Alcohol.Intensity,
#    HLTH.BMI,
#    ChildrenNum,
    # GenCohort,
    # Euro,
    # partnerlost_job, rare
    #lost_job,
    #began_relationship,
    #   SexualSatisfaction,
    #   POWERDEPENDENCE1,
    #   POWERDEPENDENCE2,
 #   Your.Future.Security,
 #   Your.Personal.Relationships,
 #   Your.Health,
 #   Standard.Living,
    #Env.SacWilling,
    #Env.SacMade,
  #  PERFECTIONISM,
  #  PermeabilityIndividual,
  #  ImpermeabilityGroup,
    work_cat2
    # Emp.JobSecure,
    # Env.ClimateChgCause,
    # Env.ClimateChgReal #,
  ) %>%
#  dplyr::rename(Community = SWB.SoC01) %>%
  dplyr::mutate(Edu = as.numeric(Edu)) %>%
  dplyr::mutate(across(!c(Id, Wave), ~ as.numeric(.x))) %>% # make factors numeric for easy of
  arrange(Id, Wave) %>%
  # dplyr::mutate(#  Volunteers = if_else(HoursCharity == 1, 1, 0),
  #   Church = ifelse(Religion.Church > 8, 8, Religion.Church),) %>%
  arrange(Id, Wave) %>%
  dplyr::mutate(
    Edu = as.numeric(Edu),
 #   Volunteers = if_else(HoursCharity == 1, 1, 0),
    # Depressed = (as.numeric(
    #   cut(
    #     KESSLER6sum,
    #     breaks = c(-Inf, 13, Inf),
    #     labels = c("0", "1"),
    #     right = FALSE
    #   )
    # ) - 1),
    # EthCat = factor(EthCat, labels = c("Euro", "Maori", "Pacific", "Asian")),
    # Church = ifelse(Religion.Church > 8, 8, Religion.Church),
    income_log = log(Household.INC + 1),
  ) %>%
  select(-Household.INC) |>
  arrange(Id, Time) %>%
  data.frame() %>%
  mutate(across(where(is.double), as.numeric)) %>%
  rename(time = Time) |>
  mutate(time = time - 1) |>
  select(-c(Wave)) |>
  arrange(Id)

table(dt_prep$time)

table1::table1(~ work_cat2 + kessler_worthless |
                 time, data = dt_prep)


# check data
# check -- Don't use work life balance -- don't have it at baseline
naniar::vis_miss(dt_prep, warn_large_data = FALSE)


# dplyr::mutate(income_log_lead1 = lead(income_log, n = 1)) %>%
# dplyr::mutate(Hours.Work_lead1 = lead(Hours.Work, n = 1)) %>%
# dplyr::mutate(HLTH.Disability_lead1 = lead(Hours.Work, n = 1)) %>%
# dplyr::mutate(Standard.Living_lead1 = lead(Standard.Living, n = 1)) %>%
# dplyr::mutate(retired_lead1 = lead(retired, n = 1)) %>%
# dplyr::mutate(semiretired_lead1 = lead(semiretired, n = 1)) %>%
#dplyr::mutate(Church_lead1 = lead(Church, n = 1)) %>%  Your.Future.Security
# inc_prop = (income_log / (income_log_lead1) - 1),

# pivot wide
dt_wide <- dt_prep |>
  pivot_wider(
    id_cols = Id,
    names_from = time,
    values_from = -c(Id, time),
    names_glue = "l{time}_{.value}",
    names_prefix = "l"
  )

colnames(dt_wide)




# wrangle
dt_w <- dt_wide |>
  rename(a1 = l1_work_cat2) |>
  rename_with(~ stringr::str_replace(.x, "^l2_", "y2_"), starts_with("l2_")) |>
  select(c(starts_with("l0_"),
           a1,
           starts_with("y2_")))
# check
colnames(dt_w)

#check
naniar::vis_miss(dt_w, warn_large_data = FALSE)

# too big, select only worthless


## COMPARE IMPUTATIONS
dt_kessler <- dt_w |> select(c(starts_with("l0_"),
                               a1,
                               starts_with("y2_kessler_",)))
naniar::vis_miss(dt_kessler, warn_large_data = FALSE)

dt_meaning <- dt_w |> select(c(starts_with("l0_"),
                               a1,
                               starts_with("y2_meaning_",)))

naniar::vis_miss(dt_meaning, warn_large_data = FALSE)


dt_both <- dt_w |> select(c(starts_with("l0_"),
                               a1,
                               starts_with("y2_kessler_"),
                               starts_with("y2_meaning_")))



naniar::vis_miss(dt_meaning, warn_large_data = FALSE)



gg_miss_upset(dt_kessler, nsets = n_var_miss(dt_kessler))
gg_miss_upset(dt_kessler,
              nsets = 10,
              nintersects = NA)








# mice model  -------------------------------------------------------------
library(mice)

# check
colnames(dt_kessler)

# check again
naniar::gg_miss_var(dt_kessler)

vis_miss(dt_kessler,
         warn_large_data = FALSE)

# any colinear vars?
mice:::find.collinear(dt_kessler)

# ini <- mice(mice_upinc, m = 1, maxit = 0)
# meth<- ini$method
# meth
# predmat["Id",]= 0
# predmat
# #meth
# #meth["inc_prop"] <- "~ I(income_log/(income_log_lead1 - 1))"
# meth["Id"] <- 0


# impute
mice_kessler <- mice::mice(dt_kessler,  seed = 0, m = 10)


mice_kessler

# save
saveRDS(mice_kessler, here::here( push_mods, "mice_kessler"))

# read
mice_kessler <- readRDS(here::here( push_mods, "mice_kessler"))

# checks
outlist2 <-
  row.names(mice_kessler)[mice_kessler$outflux < 0.5]
length(outlist2)

# checks
head(mice_kessler$loggedEvents, 10)

# data warangling
# we create two completed data sets -- the one without the missing data will be useful for
# determing causal contrasts -- which we'll describe below.

mc  <- mice::complete(mice_kessler, "long", inc = T)


skimr::skim(mc)
N <- nrow(dt_kessler) # number of ids
N

# create variables in z score
mc_v <- mc %>%
  dplyr::mutate(id = as.factor(rep(1:N, 11))) |>
  # dplyr::mutate(newkids = ChildrenNum_lead2 - ChildrenNum) %>%
  dplyr::group_by(id) |>
  mutate(y2_k6 = mean(
    c(
      y2_kessler_depressed,
      y2_kessler_effort,
      y2_kessler_nervous,
      y2_kessler_hopeless,
      y2_kessler_restless,
      y2_kessler_worthless
    ),
    na.rm = TRUE
  )) |>
  #mutate(PWI = mean(
  #   c(
  #     Your.Future.Security,
  #     Your.Personal.Relationships,
  #     Your.Health,
  #     Standard.Living
  #   ),
  #   na.rm = TRUE
  # )) |>
  # dplyr::group_by(id) |> mutate(PWI_lead2 = mean(
  #   c(
  #     Your.Future.Security_lead2,
  #     Your.Personal.Relationships_lead2,
  #     Your.Health_lead2,
  #     Standard.Living_lead2
  #   ),
  #   na.rm = TRUE
  # )) |> mutate(POWERDEPENDENCE = mean(
  #   c(
  #     POWERDEPENDENCE1,
  #     POWERDEPENDENCE2
  #   ),
  #   na.rm = TRUE
  # )) |>
  # mutate(POWERDEPENDENCE_lead2 = mean(
  #   c(
  #     POWERDEPENDENCE1_lead2,
  #     POWERDEPENDENCE2_lead2
  #   ),
  #   na.rm = TRUE
  # )) |>
  dplyr::ungroup() |>
  dplyr::mutate(l0_EthCat = as.factor(l0_EthCat),
                l0_Gender3  = as.factor(l0_Gender3),
                l0_SexualOrientation  = as.factor(l0_SexualOrientation)) |>
  dplyr::mutate(across(where(is.numeric), ~ scale(.x), .names = "{col}_z")) |>
  select(-c(.imp_z, .id_z))


# Get data into shape
ml <- mc_v %>% mutate_if(is.matrix, as.vector)

ml <- mice::as.mids(ml)

mf <- mice::complete(ml, "long", inc = TRUE)

saveRDS(ml, here::here(push_mods, "k6_only_mice-ml"))
saveRDS(mf, here::here(push_mods, "k6_only_mice-mf"))

# for models wihout looping (not advised)


# basenline vars ---------------------------------------------------------

# get
cvars =
  colnames_with_l0 <- dt_kessler |>
  select(starts_with("l0_")) |>
  names()

cvars


# weights -----------------------------------------------------------------

# standard name for data frame
df <- ml
family <- "gaussian"

# create weights
library(MatchThem)
library(optmatch)
library(MatchIt)
library(WeightIt)
library(cobalt)



# church-use R
# set digits = 3
options(scipen = 999)




# WEIGHTS

match_ml <- weightthem(
  as.formula(paste(as.formula(paste(
    paste("a1", "~",
          paste(cvars, collapse = "+"))
  )))),
  df,
  approach = "within",
  estimand = "ATT",
  stabilize = TRUE,
  method = "ebal"
)


sum <- summary(match_ml)
plot(sum)
sum
bal.tab(match_ml)


length(unique(mlf$id))



# KESSLER6Total -----------------------------------------------------------


Y <- "y2_k6_z"
X <- "a1"


fits <- lapply(complete(match_ml, "all"), function(d) {
  glm(
    as.formula(paste(
      paste(Y, "~", X, "+"),
      paste(cvars, collapse = "+")
    )),
    weights = weights,
    family = family,
    data = d
  )
})

# worthess
mice::pool(fits)

# TWO WAYS TO COMUTE CAUSAL EFFECTS

library("marginaleffects")

comp.imp <- lapply(fits, function(fit) {
  avg_comparisons(
    fit,
    newdata = subset(fit$data, a1 == 1),
    variables = X,
    wts = "weights",
    vcov = "HC3"#,
    #transform_pre = "lnratioavg"
  )
})

pooled.comp <- mice::pool(comp.imp)
pooled.comp


# worthless to look at coefs
k6_tab <- summary(pooled.comp, conf.int = TRUE,
        exponentiate = FALSE)

k6_tab

## another way -- very nice
library(clarify)
sim.imp <- misim(fits, n = 1000, vcov = "HC3")
sim.imp
summary(sim.imp)

sim.att <- sim_ame(
  sim.imp,
  var = "a1",
  subset = a1 == 1,
  cl = 8,
  verbose = FALSE
)
sim.att

# sim.att <- transform(sim.att, RR = `E[Y(1)]` / `E[Y(0)]`)
#
# sim.att
# summary(sim.att, null = c(RR = 1))

sim_est <- transform(sim.att, `RD` = `E[Y(1)]` - `E[Y(0)]`)
summary(sim_est, null = c(`RD` = 0))
plot(sim_est)



# model each dimension of k6 ----------------------------------------------
# we will just use the marginal effects delta method because it is faster
# y2_kessler_depressed,
# y2_kessler_effort,
# y2_kessler_nervous
# y2_kessler_hopeless,
# y2_kessler_restless,
# y2_kessler_worthless



# y2_kessler_depressed_z ---------------------------------------------------------------


Y <- "y2_kessler_depressed_z"
X <- "a1"


fits <- lapply(complete(match_ml, "all"), function(d) {
  glm(
    as.formula(paste(
      paste(Y, "~", X, "+"),
      paste(cvars, collapse = "+")
    )),
    weights = weights,
    family = family,
    data = d
  )
})

mice::pool(fits)


library("marginaleffects")
comp.imp <- lapply(fits, function(fit) {
  avg_comparisons(
    fit,
    newdata = subset(fit$data, a1 == 1),
    variables = X,
    wts = "weights",
    vcov = "HC3"#,
    #transform_pre = "lnratioavg"
  )
})
comp.imp

depressed_tab  <- comp.imp

depressed_tab




# y2_kessler_effort ---------------------------------------------------------------


Y <- "y2_kessler_effort"


fits <- lapply(complete(match_ml, "all"), function(d) {
  glm(
    as.formula(paste(
      paste(Y, "~", X, "+"),
      paste(cvars, collapse = "+")
    )),
    weights = weights,
    family = family,
    data = d
  )
})

mice::pool(fits)


library("marginaleffects")
comp.imp <- lapply(fits, function(fit) {
  avg_comparisons(
    fit,
    newdata = subset(fit$data, a1 == 1),
    variables = X,
    wts = "weights",
    vcov = "HC3"#,
    #transform_pre = "lnratioavg"
  )
})
comp.imp

depressed_tab  <- comp.imp

depressed_tab



# y2_kessler_nervous ------------------------------------------------------

Y <- "y2_kessler_nervous"


fits <- lapply(complete(match_ml, "all"), function(d) {
  glm(
    as.formula(paste(
      paste(Y, "~", X, "+"),
      paste(cvars, collapse = "+")
    )),
    weights = weights,
    family = family,
    data = d
  )
})

mice::pool(fits)


library("marginaleffects")
comp.imp <- lapply(fits, function(fit) {
  avg_comparisons(
    fit,
    newdata = subset(fit$data, a1 == 1),
    variables = X,
    wts = "weights",
    vcov = "HC3"#,
    #transform_pre = "lnratioavg"
  )
})
comp.imp

nervous_tab  <- comp.imp

nervous_tab




# y2_kessler_hopeless -----------------------------------------------------


Y <- "y2_kessler_hopeless"


fits <- lapply(complete(match_ml, "all"), function(d) {
  glm(
    as.formula(paste(
      paste(Y, "~", X, "+"),
      paste(cvars, collapse = "+")
    )),
    weights = weights,
    family = family,
    data = d
  )
})

mice::pool(fits)


library("marginaleffects")
comp.imp <- lapply(fits, function(fit) {
  avg_comparisons(
    fit,
    newdata = subset(fit$data, a1 == 1),
    variables = X,
    wts = "weights",
    vcov = "HC3"#,
    #transform_pre = "lnratioavg"
  )
})
comp.imp

hopeless_tab  <- comp.imp

hopeless_tab



# y2_kessler_restless -----------------------------------------------------
Y <- "y2_kessler_restless"


fits <- lapply(complete(match_ml, "all"), function(d) {
  glm(
    as.formula(paste(
      paste(Y, "~", X, "+"),
      paste(cvars, collapse = "+")
    )),
    weights = weights,
    family = family,
    data = d
  )
})

mice::pool(fits)


library("marginaleffects")
comp.imp <- lapply(fits, function(fit) {
  avg_comparisons(
    fit,
    newdata = subset(fit$data, a1 == 1),
    variables = X,
    wts = "weights",
    vcov = "HC3"#,
    #transform_pre = "lnratioavg"
  )
})
comp.imp

restless_tab  <- comp.imp

restless_tab

# y2_kessler_worthless ----------------------------------------------------

Y <- "y2_kessler_worthless"


fits <- lapply(complete(match_ml, "all"), function(d) {
  glm(
    as.formula(paste(
      paste(Y, "~", X, "+"),
      paste(cvars, collapse = "+")
    )),
    weights = weights,
    family = family,
    data = d
  )
})

mice::pool(fits)


library("marginaleffects")
comp.imp <- lapply(fits, function(fit) {
  avg_comparisons(
    fit,
    newdata = subset(fit$data, a1 == 1),
    variables = X,
    wts = "weights",
    vcov = "HC3"#,
    #transform_pre = "lnratioavg"
  )
})
comp.imp

worthless_tab  <- comp.imp

worthless_tab



### NEXT WE TRY MEANING


# MEANING IN LIFE ---------------------------------------------------------


# mice model  -------------------------------------------------------------
library(mice)

# check
colnames(dt_meaning)

# check again
naniar::gg_miss_var(dt_meaning)

vis_miss(dt_meaning,
         warn_large_data = FALSE)

# any colinear vars?
mice:::find.collinear(dt_meaning)

# ini <- mice(mice_upinc, m = 1, maxit = 0)
# meth<- ini$method
# meth
# predmat["Id",]= 0
# predmat
# #meth
# #meth["inc_prop"] <- "~ I(income_log/(income_log_lead1 - 1))"
# meth["Id"] <- 0


# impute
mice_meaning <- mice::mice(dt_meaning,  seed = 0, m = 10)


mice_meaning

# save
saveRDS(mice_meaning, here::here( push_mods, "mice_meaning"))

# read
mice_meaning <- readRDS(here::here( push_mods, "mice_meaning"))

# checks
outlist2 <-
  row.names(mice_meaning)[mice_meaning$outflux < 0.5]
length(outlist2)

# checks
head(mice_meaning$loggedEvents, 10)

# data warangling
# we create two completed data sets -- the one without the missing data will be useful for
# determing causal contrasts -- which we'll describe below.

mc_meaning  <- mice::complete(mice_meaning, "long", inc = T)


skimr::skim(mc_meaning)
N <- nrow(dt_meaning) # number of ids
N

# create variables in z score
mc_meaning_v <- mc_meaning %>%
  dplyr::mutate(id = as.factor(rep(1:N, 11))) |>
  # dplyr::mutate(newkids = ChildrenNum_lead2 - ChildrenNum) %>%
  dplyr::group_by(id) |>
  # mutate(y2_k6 = mean(
  #   c(
  #     y2_kessler_depressed,
  #     y2_kessler_effort,
  #     y2_kessler_nervous,
  #     y2_kessler_hopeless,
  #     y2_kessler_restless,
  #     y2_kessler_worthless
  #   ),
  #   na.rm = TRUE
  # )) |>
mutate(y2_meaning = mean(
  c(
    y2_meaning_sense,
    y2_meaning_purpose
  ),
  na.rm = TRUE
)) |>
  #mutate(PWI = mean(
  #   c(
  #     Your.Future.Security,
  #     Your.Personal.Relationships,
  #     Your.Health,
  #     Standard.Living
  #   ),
  #   na.rm = TRUE
  # )) |>
  # dplyr::group_by(id) |> mutate(PWI_lead2 = mean(
  #   c(
#     Your.Future.Security_lead2,
#     Your.Personal.Relationships_lead2,
#     Your.Health_lead2,
#     Standard.Living_lead2
#   ),
#   na.rm = TRUE
# )) |> mutate(POWERDEPENDENCE = mean(
#   c(
#     POWERDEPENDENCE1,
#     POWERDEPENDENCE2
#   ),
#   na.rm = TRUE
# )) |>
# mutate(POWERDEPENDENCE_lead2 = mean(
#   c(
#     POWERDEPENDENCE1_lead2,
#     POWERDEPENDENCE2_lead2
#   ),
#   na.rm = TRUE
# )) |>
dplyr::ungroup() |>
  dplyr::mutate(l0_EthCat = as.factor(l0_EthCat),
                l0_Gender3  = as.factor(l0_Gender3),
                l0_SexualOrientation  = as.factor(l0_SexualOrientation)) |>
  dplyr::mutate(across(where(is.numeric), ~ scale(.x), .names = "{col}_z")) |>
  select(-c(.imp_z, .id_z))


# Get data into shape
ml_meaning <- mc_meaning_v %>% mutate_if(is.matrix, as.vector)

ml_meaning <- mice::as.mids(ml_meaning)

mf_meaning <- mice::complete(ml_meaning, "long", inc = TRUE)

saveRDS(ml, here::here(push_mods, "ml_meaning"))
saveRDS(mf, here::here(push_mods, "mf_meaning"))

# for models wihout looping (not advised)


# basenline vars ---------------------------------------------------------

# get
cvars =
  colnames_with_l0 <- dt_meaning |>
  select(starts_with("l0_")) |>
  names()

cvars


# weights -----------------------------------------------------------------

# standard name for data frame
df <- ml_meaning
family <- "gaussian"

# create weights
library(MatchThem)
library(optmatch)
library(MatchIt)
library(WeightIt)
library(cobalt)



# church-use R
# set digits = 3
options(scipen = 999)




# WEIGHTS

match_ml <- weightthem(
  as.formula(paste(as.formula(paste(
    paste("a1", "~",
          paste(cvars, collapse = "+"))
  )))),
  df,
  approach = "within",
  estimand = "ATT",
  stabilize = TRUE,
  method = "ebal"
)


sum <- summary(match_ml)
plot(sum)
sum
bal.tab(match_ml)



# meaning_total -----------------------------------------------------------


Y <- "y2_meaning"
X <- "a1"


fits <- lapply(complete(match_ml, "all"), function(d) {
  glm(
    as.formula(paste(
      paste(Y, "~", X, "+"),
      paste(cvars, collapse = "+")
    )),
    weights = weights,
    family = family,
    data = d
  )
})

mice::pool(fits)

# TWO WAYS TO COMPUTE CAUSAL EFFECTS

library("marginaleffects")

comp.imp <- lapply(fits, function(fit) {
  avg_comparisons(
    fit,
    newdata = subset(fit$data, a1 == 1),
    variables = X,
    wts = "weights",
    vcov = "HC3"#,
    #transform_pre = "lnratioavg"
  )
})

pooled.comp <- mice::pool(comp.imp)
pooled.comp


# worthless to look at coefs
meaning_tab <- summary(pooled.comp, conf.int = TRUE,
                  exponentiate = FALSE)

meaning_tab

## another way -- very nice
library(clarify)
sim.imp <- misim(fits, n = 1000, vcov = "HC3")
sim.imp
summary(sim.imp)

sim.att <- sim_ame(
  sim.imp,
  var = "a1",
  subset = a1 == 1,
  cl = 8,
  verbose = FALSE
)
sim.att

# sim.att <- transform(sim.att, RR = `E[Y(1)]` / `E[Y(0)]`)
#
# sim.att
# summary(sim.att, null = c(RR = 1))

sim_est <- transform(sim.att, `RD` = `E[Y(1)]` - `E[Y(0)]`)
summary(sim_est, null = c(`RD` = 0))
plot(sim_est)






# meaning_sense -----------------------------------------------------------

Y <- "y2_meaning_sense"
X <- "a1"


fits <- lapply(complete(match_ml, "all"), function(d) {
  glm(
    as.formula(paste(
      paste(Y, "~", X, "+"),
      paste(cvars, collapse = "+")
    )),
    weights = weights,
    family = family,
    data = d
  )
})

mice::pool(fits)

# TWO WAYS TO COMPUTE CAUSAL EFFECTS

library("marginaleffects")

comp.imp <- lapply(fits, function(fit) {
  avg_comparisons(
    fit,
    newdata = subset(fit$data, a1 == 1),
    variables = X,
    wts = "weights",
    vcov = "HC3"#,
    #transform_pre = "lnratioavg"
  )
})

pooled.comp <- mice::pool(comp.imp)
pooled.comp


# worthless to look at coefs
tab_meaning_sense  <- summary(pooled.comp, conf.int = TRUE,
                       exponentiate = FALSE)

tab_meaning_sense
#
# ## another way -- very nice
# library(clarify)
# sim.imp <- misim(fits, n = 1000, vcov = "HC3")
# sim.imp
# summary(sim.imp)
#
# sim.att <- sim_ame(
#   sim.imp,
#   var = "a1",
#   subset = a1 == 1,
#   cl = 8,
#   verbose = FALSE
# )
# sim.att
#
# # sim.att <- transform(sim.att, RR = `E[Y(1)]` / `E[Y(0)]`)
# #
# # sim.att
# # summary(sim.att, null = c(RR = 1))
#
# sim_est <- transform(sim.att, `RD` = `E[Y(1)]` - `E[Y(0)]`)
# summary(sim_est, null = c(`RD` = 0))
# plot(sim_est)



# meaning_purpose -----------------------------------------------------------

Y <- "y2_meaning_purpose"


fits <- lapply(complete(match_ml, "all"), function(d) {
  glm(
    as.formula(paste(
      paste(Y, "~", X, "+"),
      paste(cvars, collapse = "+")
    )),
    weights = weights,
    family = family,
    data = d
  )
})

mice::pool(fits)

# TWO WAYS TO COMPUTE CAUSAL EFFECTS

library("marginaleffects")

comp.imp <- lapply(fits, function(fit) {
  avg_comparisons(
    fit,
    newdata = subset(fit$data, a1 == 1),
    variables = X,
    wts = "weights",
    vcov = "HC3"#,
    #transform_pre = "lnratioavg"
  )
})

pooled.comp <- mice::pool(comp.imp)
pooled.comp


# worthless to look at coefs
tab_meaning_purpose  <- summary(pooled.comp, conf.int = TRUE,
                              exponentiate = FALSE)

tab_meaning_purpose

## another way -- very nice
library(clarify)
sim.imp <- misim(fits, n = 1000, vcov = "HC3")
sim.imp
summary(sim.imp)

sim.att <- sim_ame(
  sim.imp,
  var = "a1",
  subset = a1 == 1,
  cl = 8,
  verbose = FALSE
)
sim.att

# sim.att <- transform(sim.att, RR = `E[Y(1)]` / `E[Y(0)]`)
#
# sim.att
# summary(sim.att, null = c(RR = 1))

sim_est <- transform(sim.att, `RD` = `E[Y(1)]` - `E[Y(0)]`)
summary(sim_est, null = c(`RD` = 0))
plot(sim_est)



# G-formula IMPUTE test ---------------------------------------------------


library(gFormulaMI)


colnames(mice_meaning$data)
# read data, imputed already from the pets -lost file
head(mice_meaning)



# imps <- gFormulaImpute(
#   data = mice_meaning,
#   M = 10,
#   trtVarStem = "a1",
#   timePoints = 2,
#   trtRegimes = list(0, 1)
#   #  trtRegimes = list(c(0,0), c(0,1))
# )

#
# fits <- with(imps, lm(y2_sf ~ factor(regime)))
#
# syntheticPool(fits)





# old method --------------------------------------------------------------

# GET DATA INTO ORDER

# make into mids
#Extracting the original dataset with missing value
maindataset <- complete(match_ml, action = 0)
#Some spit-and-polish
maindataset <- data.frame(.imp = 0, .id = seq_len(nrow(maindataset)), maindataset)

#Extracting imputed-weighted datasets in the long format
alldataset  <- complete(match_ml, action = "long")

#Binding them together
alldataset  <- rbind(maindataset, alldataset)

#Converting to .mids
newmids <- as.mids(alldataset)

# set data
dff <- newmids


# old way setup
# You may set your label for your graphs  HERE WE STICK TO THE EXAMPLE OF WORK

xlab = "Work more than 20 hours"  ## Weekly hours devided by 10


# SET THE RANGE OF WORK HOURS FROM ZERO TO 80
min = 0
max =  1

# set full range of X
x =  min:max
x


# range for some graphs
minmax <- paste(c(x), sep = ",")

# baseline condition
r = 0

# focal contrast for X
f = 1

# REQUIRED for certain model model functions
c = x

# contrast for graphs -- absolute distance from baseline
p = c(r, f) #


# Needed for E-VALUES -- how much do we move on the X scale to obtain our effect?

delta = abs(r - f)

ylim = c(-.5, .5)  # SET AS YOU LIKE -- here, how much movement across a standard deviation unit of the outcome
ylim_contrast = c(0, 3)  # SET AS YOU LIKE (FOR CONTRASTS )

# mice imputed data
## THIS IS KEY, NAME THE DATA I GAVE YOU "DF"

# n imputations
m = 10

# standard deviation of the outcome (for evalues)
# We have stanadardised the (non-binary) outcomes for comparable effect sizes.
sd = 1


family = "gaussian"



# old-way-purpose ---------------------------------------------------------
X = a1

sub = "meaning purpose"
Y = "y2_meaning_purpose"
main = "Meaning Purpose"
ylab = "Meaning Purpose (SD)"

# regression

out_m <-
  mice_iptw_lin(
    df = dff,
    X = X,
    Y = Y,
    cvars = cvars,
    family = "gaussian"
  )

summary(out_m)



## g-computation
out_ct <-
  pool_stglm_contrast(
    out_m,
    df = df,
    m = 10,
    X = X,
    x = x,
    r = r
  )

out_ct %>%
  slice(f + 1 - min) |>
  kbl(digits = 3, "markdown")


meaning_purpose_t <- out_ct %>%
  #slice(1:max) |>
  tibble() |>
  rename(
    Contrast = row,
    Estimate = est,
    Std_error = se,
    CI_hi = ui,
    CI_lo = li
  ) |>
  kbl(caption = main,
      digits = 3,
      "html") |>
  kable_styling() %>%
  row_spec(
    c(f + 1 - min),
    bold = T,
    color = "white",
    background = "dodgerblue"
  ) |>
  kable_minimal(full_width = F)
# show table
meaning_purpose_t

# compare with above same

tab_meaning_purpose


# graph
meaning_purpose_p <-
  ggplot_stglm(
    out_ct,
    ylim = ylim,
    main,
    xlab,
    ylab,
    min = min,
    p = p,
    sub = sub
  )
meaning_purpose_p
# coef + estimate
meaning_purpose_c <-  vanderweelevalue_ols(out_ct, f - min, delta, sd)
meaning_purpose_c



pool_stglm_contrast


