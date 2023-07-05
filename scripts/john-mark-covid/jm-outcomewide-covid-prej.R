## lockdown on prejudice

options(scipen = 999)

#libraries and functions
# read libraries
source("https://raw.githubusercontent.com/go-bayes/templates/main/functions/libs2.R")

# read functions
source("https://raw.githubusercontent.com/go-bayes/templates/main/functions/funs.R")


# read data



pull_path <-
  fs::path_expand(
    "/Users/joseph/v-project\ Dropbox/data/current/nzavs_13_arrow"
)

# for saving models
push_mods <-
  fs::path_expand("/Users/joseph/v-project\ Dropbox/Joseph\ Bulbulia/outcomewide/jm/mods")


dat <- arrow::read_parquet(pull_path)

dat$covid19_timeline

dt_cov <- dat |>
  dplyr::filter((wave == 2018 & year_measured  == 1) |
                  (wave == 2019  &
                     year_measured  == 1)) |>  # Eligibility criteria  Observed in 2018/2019 & Outcomes in 2020 or 2021
  dplyr::filter(regc_2022 == 2 | regc_2022 == 9) |>
  group_by(id) |>
  dplyr::mutate(k_18 =  ifelse(wave == 2018, 1, 0)) |>   # creating an indicator for the first wave. Inclusion criteria
  dplyr::mutate(h_18 = mean(k_18, na.rm = TRUE)) |>   # Hack
  dplyr::mutate(k_19 =  ifelse(
    wave == 2019 &
      year_measured == 1 &  (covid19_timeline ==  2.1 |   covid19_timeline ==  2.2) ,#  Inclusion criteria
    1,
    0)) |>   # creating an indicator for the first wave; note that we allow people t
  dplyr::mutate(h_19 = mean(k_19, na.rm = TRUE)) |>  # Hack
  dplyr::filter(h_18 > 0) |>  # hack to enable repeat of baseline
  dplyr::filter(h_19 > 0) |>  # hack to enable repeat of baseline
  dplyr::ungroup() |>
  droplevels() |>
#  mutate(exposure =  ifelse(!is.na(covid19_timeline), 1, 0) ) |>
  select( starts_with("warm_"), regc_2022, id, wave, covid19_timeline,  male, religious, eth_cat) |>  # add other covariates if needed
  mutate(city_region = as.factor( if_else(regc_2022 == 2, "auckland", "wellington")))

table(dt_cov$city_region)



summary(
 lm( warm_chinese  ~ wave * city_region, data = dt_cov ) )

summary(
  lm( warm_nz_euro  ~ wave * city_region, data = dt_cov ) )


# Get the names of variables that end with "warm_"
names_list <- dt_cov |>
  select(starts_with("warm_")) |>
  select(-warm_lgbtq) |>
  colnames()

names_list
# Create a list of formulas
formulas_list <- lapply(names_list, function(name) {
  as.formula(paste0(name, " ~ wave * city_region"))
})

# Apply lm and summary to each formula
results <- lapply(formulas_list, function(formula) {
  dt_subset <- dt_cov[!is.na(dt_cov[[as.character(formula[[2]])]]), ]  # subset the data for each "warm_" variable

  # If city_region has only one level in the subset, return a message
  if (nlevels(dt_subset$city_region) < 2) {
    return(paste0("Not enough levels in city_region for the model: ", as.character(formula)))
  } else {
    model <- lm(formula, data = dt_subset)
    return(summary(model))
  }
})

#
# names_list  <- dt_cov |>
#   select(starts_with("warm_")) |>
#   colnames()names_string <- paste(names_list, collapse = ", ")
# test = c(names_string)
# test
# prep_cov <- margot_wide(dt_cov,
#                         baseline_vars = "warm_chinese",
#                         exposure_var = c("warm_chinese","wave","regc_2022"),
#                         outcome_vars = "warm_chinese")




#sucka

# CONVENTIONS FOR COVID TIMELINE
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

# dat$REGC_2018 -- DON'T USE RATHER USE REGC_2022

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


# criteria for inclusion

# nzavs  in wave 10 (baseline )
# wave 10 - responded before 15 march
# wave 11 - responded before 27 April (2020)
# condition 0 -- measured prejudice between start of wave 11 and start of lockdown (condition 0)
# condiiton 1 -- lockdown

# baseline = wave 10
# exposure_condition: wave 10 pre lockdown == 0, locdown == 1

## regression is

# prejudice Y  ~ exposure_condition  * ( baseline_prejudice + baseline_covariates) # winston lin 2013
#
# model result from regression, we predict the average response for everyone under both exposures 30k Y0 , 30k in Y1

# after we use regression coefficients to predict -- E[Y(1)] -- prejudice if everyone in population from which the study was sampled was  exposed, and E[Y(0)] -- prejudice if not exposed, we take the causal contrast is the difference between these two predicted averages, i.e. E[Y(1)]  - E[Y(0)]

#  E[Y(1)| A = 1, l] - E[Y(0)| A = 1, l]  ATT

dat$covid_tim

# for the pre analsis
dt_00 <- dat %>%
  mutate(attacks_to_covid = ifelse(
    # not used
    TSCORE < 3545,
    "pre-attacks",
    ifelse(
      TSCORE >= 3545 & TSCORE < 3922,
      "attacks_to_lockdown",
      # no spaces!!
      ifelse(TSCORE >= 3922 &
               TSCORE <= 3954, "lockdown",
             "post-lockdown")
    )
  )) |>
  mutate(post_attacks  = as.factor(ifelse(TSCORE < 3545, 1, 0))) |>
  mutate(covid_condition  = as.factor(ifelse(
    TSCORE < 3922,
    "pre_covid",
    ifelse(TSCORE >= 3922 &
             TSCORE <= 3954, "lockdown",
           "post-lockdown")
  ))) |>
  dplyr::filter(
    Wave == 2018  &
      YearMeasured  == 1 & !is.na(covid_condition) |
      # need clean baseline
      (Wave == 2019  &
         YearMeasured  == 1 & !is.na(covid_condition)) |
      (Wave == 2020)
  ) %>%
  #dplyr::filter(YearMeasured  != -1) %>% # remove people who passed away -- don't worry about this
  group_by(Id) %>%
  dplyr::mutate(org2018 =  ifelse(Wave == 2018 &
                                    YearMeasured == 1, 1, 0)) %>%  # creating an indicator for the first wave
  dplyr::mutate(hold18 = mean(org2018, na.rm = TRUE)) %>%  # Hack
  dplyr::filter(hold18 > 0) %>% # hack to enable repeat of baseline
  dplyr::mutate(org2019 = ifelse(Wave == 2019 &
                                   YearMeasured == 1, 1, 0)) %>%  # creating an indicator for the first wave
  dplyr::mutate(hold19 = mean(org2019, na.rm = TRUE)) %>%  # Hack0
  dplyr::filter(hold19 > 0) %>% # hack to enable repeat of baseline in 201
  ungroup() |>
  droplevels() %>%
  mutate(time = as.numeric(Wave) - 1) |>
  arrange(Id, time) |>
  droplevels() |>
  arrange(Id, time) |>
  # mutate(Rural_GCH2018 = as.factor(Rural_GCH2018),
  #        REGC_2022 = as.factor(REGC_2022))|>
  select(
    Id,
    w_GendAgeEuro,
    attacks_to_covid,
    post_attacks,
    covid_condition,
    YearMeasured,
    time,
    TSCORE,
    Wave,
    Partner,
    EthCat,
    Age,
    # Gender3,
    Male,
    SampleFrame,
    NZSEI13,
    NZDep2018,
    Rural_GCH2018,
    REGC_2022,
    CONSCIENTIOUSNESS,
    OPENNESS,
    HONESTY_HUMILITY,
    EXTRAVERSION,
    NEUROTICISM,
    AGREEABLENESS,
    #  ChildrenNum,
    # Hours.Work,
    edu_n,
    Employed,
    BornNZ,
    #   HomeOwner,
    Pol.Orient,
    #   income_log,
    Parent,
    Relid,
    Warm.Muslims,
    Warm.Asians,
    Warm.Chinese,
    #Warm.Disabled, # begins wave12
    Warm.Elderly,
    # begins wave 10
    Warm.Immigrants,
    Warm.Indians,
    Warm.Maori,
    Warm.MentalIllness,
    #begins wave 9
    Warm.Muslims,
    Warm.NZEuro,
    Warm.Overweight,
    Warm.Pacific,
    Warm.Refugees
  )

colnames(dt_00)


dt_00$Warm.MentalIllness

table1(~ Warm.MentalIllness |Wave, data = dt_00)


table(dt_00$post_attacks)

# save data
write_parquet(dt_00, here::here(push_mods, "dt_00"))


## Analysis 1, only investigate effects 2019
table(dt_00$attacks_to_covid)
table(dt_00$post_attacks)

table1::table1(~ attacks_to_covid | Wave, data = dt_00)



## try without
dt_19 <- dt_00 |>
  dplyr::select(-c(attacks_to_covid)) |>
  dplyr::filter(covid_condition != "post-lockdown") |>
  droplevels() |>
  dplyr::rename(exposure = covid_condition) |>
  dplyr::mutate(across(
    !c(
      Id,
      TSCORE,
      Wave,
      EthCat,
      Male,
      SampleFrame,
      REGC_2022,
      exposure,
      Rural_GCH2018
    ),
    ~ as.numeric(.x)
  )) |>
  #make factors numeric for model
  #  dplyr::mutate(across(where(is.numeric), ~ scale(.x), .names = "{col}_z")) |> won't work if we impute
  # select(EthCat, Gender3, SampleFrame, exposure,
  #        Rural_GCH2018,
  #        REGC_2022,
  #        ends_with("_z")) |>
  # select(EthCat, Gender3, SampleFrame, exposure,
  #        Rural_GCH2018,
  #        REGC_2022,
  #        ends_with("_z")) |>
  mutate(exposure = as.numeric(exposure)) |>
  mutate(exposure = abs(exposure - 2)) |> # order so that the exposure is lockdown
  mutate(post_attacks = post_attacks - 1) |> # order so that the exposure is lockdown
  filter(!is.na(exposure)) |>
  group_by(Id) %>%
  dplyr::mutate(org2018 =  ifelse(Wave == 2018 &
                                    YearMeasured == 1, 1, 0)) %>%  # creating an indicator for the first wave
  dplyr::mutate(hold18 = mean(org2018, na.rm = TRUE)) %>%  # Hack
  dplyr::filter(hold18 > 0) %>% # hack to enable repeat of baseline
  dplyr::mutate(org2019 = ifelse(Wave == 2019 &
                                   YearMeasured == 1, 1, 0)) %>%  # creating an indicator for the first wave
  dplyr::mutate(hold19 = mean(org2019, na.rm = TRUE)) %>%  # Hack0
  dplyr::filter(hold19 > 0) %>% # hack to enable repeat of baseline in 201
  ungroup() |>
  select(-c(
    Wave,
    YearMeasured,
    SampleFrame,
    hold19,
    org2019,
    hold18,
    org2018
  )) |>
  arrange(Id, time) |>
  droplevels()

table( dt_19$post_attacks )


table1(~ Warm.MentalIllness |Wave, data = dt_00)


nrow(dt_19)
# check again
naniar::gg_miss_var(dt_19)
# make wide
dim(dt_19)
# pivot wide
dt_wide <- dt_19 |>
  pivot_wider(
    id_cols = Id,
    names_from = time,
    values_from = -c(Id, time),
    names_glue = "t{time}_{.value}",
    names_prefix = "t"
  )


# ) |>
# filter(!is.na(t0_exposure)) |>
# filter(!is.na(t1_exposure))

colnames(dt_wide)
naniar::gg_miss_var(dt_wide)


# for time series sensitivity analysis
dt_wide_t <- dt_wide

write_parquet(dt_wide_t, here::here(push_mods, "dt_wide_t"))


# order correctly for imputation
dt_wide <- dt_wide |>
  select(-c(t0_TSCORE, t1_TSCORE)) |>  ## remove TSCORE
  relocate(starts_with("t0"), .before = starts_with("t1")) |>
  relocate("t0_exposure", .before = "t0_post_attacks") |>
  relocate("t1_exposure", .before = "t1_Partner") |>
  select(-t1_post_attacks)
colnames(dt_wide)

table1(~ t1_Warm.MentalIllness + t0_Warm.MentalIllness |t1_exposure, data = dt_wide)



# test
table((dt_wide$t1_exposure))

# total n  in the study
n_of_sample <- nrow(dt_wide)

# breakdown
table((dt_wide$t1_exposure))


# sum of conditions
sum_columns_by_condition <- 28679 + 1648

sum_columns_by_condition == n_of_sample

# descriptive tables
table1::table1( ~ t1_Warm.Muslims |
                  factor(t1_exposure),
                data = dt_wide,
                overall = FALSE)
table1::table1( ~ t1_Warm.NZEuro |
                  factor(t1_exposure),
                data = dt_wide,
                overall = FALSE)
table1::table1( ~ t1_Warm.Immigrants |
                  factor(t1_exposure),
                data = dt_wide,
                overall = FALSE)
table1::table1( ~ t1_Warm.Refugees |
                  factor(t1_exposure),
                data = dt_wide,
                overall = FALSE)
table1::table1( ~ t1_Warm.MentalIllness |
                  factor(t1_exposure),
                data = dt_wide,
                overall = FALSE)
table1::table1( ~ t1_Warm.Elderly |
                  factor(t1_exposure),
                data = dt_wide,
                overall = FALSE)
table1::table1( ~ t1_Warm.Pacific |
                  factor(t1_exposure),
                data = dt_wide,
                overall = FALSE)
table1::table1( ~ t1_Warm.Asians |
                  factor(t1_exposure),
                data = dt_wide,
                overall = FALSE)
table1::table1( ~ t1_Warm.Chinese |
                  factor(t1_exposure),
                data = dt_wide,
                overall = FALSE)
table1::table1( ~ t1_Warm.Indians |
                  factor(t1_exposure),
                data = dt_wide,
                overall = FALSE)


# looks lower
table1::table1( ~ t1_Warm.MentalIllness |
                  factor(t1_exposure),
                data = dt_wide,
                overall = FALSE)


# select

dt_wide2 <- dt_wide |>
  select(starts_with("t0"), "t1_exposure" , starts_with("t1_Warm.")) |>
  select(-t0_exposure) |>
  mutate()

colnames(dt_wide2)


# QUICK CHECK
model_parameters(lm(data = dt_wide2,
                    t1_Warm.Muslims_z ~ t1_exposure + t0_Warm.Muslims))

model_parameters(lm(data = dt_wide2,
                    t1_Warm.Chinese ~ t1_exposure + t0_Warm.Chinese))

model_parameters(lm(data = dt_wide2,
                    t1_Warm.Immigrants ~ t1_exposure + t0_Warm.Immigrants))

model_parameters(lm(data = dt_wide2,
                    t1_Warm.NZEuro ~ t1_exposure + t0_Warm.NZEuro))

model_parameters(lm(
  data = dt_wide2,
  t1_Warm.MentalIllness ~ t1_exposure + t0_Warm.MentalIllness
))

# ... etc


# check again
naniar::gg_miss_var(dt_wide2)

vis_miss(dt_wide2,
         warn_large_data = FALSE)

# any colinear vars?
mice:::find.collinear(dt_wide2)

mice_covid_prejudice <- mice::mice(dt_wide2, m = 10)

saveRDS(mice_covid_prejudice,
        here::here(push_mods, "mice_covid_prejudice"))

mice_covid_prejudice <- readRDS(here::here(push_mods, "mice_covid_prejudice"))
# impute missing vars

# checks
outlist2 <-
  row.names(mice_covid_prejudice)[mice_covid_prejudice$outflux < 0.5]
length(outlist2)

# checks
head(mice_covid_prejudice$loggedEvents, 10)

# data warangling
# we create two completed data sets -- the one without the missing data will be useful for
# determing causal contrasts -- which we'll describe below.

mc  <- mice::complete(mice_covid_prejudice, "long", inc = T)


# checks
skimr::skim(mc)
N <- nrow(dt_wide2) # number of ids
N

# create variables in z score -- NOT WORKING FOR IPTW AT MOMENT
mc_v <- mc %>%
  dplyr::mutate(across(where(is.numeric), ~ scale(.x), .names = "{col}_z"))



# Get data into shape
ml <- mc_v %>% mutate_if(is.matrix, as.vector)

ml <- mice::as.mids(ml)
ml

mf <- mice::complete(ml, "long", inc = TRUE)
mf

saveRDS(ml, here::here(push_mods, "jm-mice-ml"))
saveRDS(mf, here::here(push_mods, "jm-mice-mf"))

ml <- readRDS(here::here(push_mods, "jm-mice-ml"))
mf <- readRDS(here::here(push_mods, "jm-mice-mf"))

hist(mf$t0_Warm.Muslims_z)
# basenline vars ---------------------------------------------------------

colnames(mf)

# measured before lockdowns. Not we hae the post-attack as a covariate
# cvars = mf |>
#   dplyr::select( starts_with("t0_")) |>
#   dplyr::select( -c(starts_with("t0_Warm") & !ends_with("_z")))


cvars_full = mf |>
  dplyr::select(-c(
    t0_EthCat_z,
    t0_REGC_2022_z,
    t0_Rural_GCH2018_z,
    t0_post_attacks_z
  )) |> # include?
  dplyr::select(t0_EthCat,
                t0_Rural_GCH2018,
                t0_REGC_2022,
                starts_with("t0_")  & ends_with("_z"))


cvars <- colnames(cvars_full)
cvars

# cvars_sim = mf |>
#   dplyr::select(
#     -c(
#       t0_EthCat_z,
#       t0_SampleFrame_z,
#       t0_REGC_2022_z,
#       t0_Rural_GCH2018_z,
#       t0_post_attacks_z,
#       t0_SampleFrame
#     )
#   ) |> # include?
#   dplyr::select(starts_with("t0_")  &
#                   !ends_with("_z") & !starts_with("t0_Warm"))
#
# cvars_sim






## test without MI
#
# dt_na <- dt_wide2 |>
#   drop_na()
#
#
#
#
# match_ml <- weightit(
#   as.formula(paste(as.formula(paste(
#     paste("t1_exposure", "~",
#           paste(cvars_sim, collapse = "+"))
#   )))),
#   dt_na,
#   estimand = "ATT",
#   stabilize = TRUE,
#   method = "ps"
# )

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

# match_ml <- weightthem(
#   as.formula(paste(as.formula(paste(
#     paste("t1_exposure", "~",
#           paste(cvars_sim, collapse = "+"))
#   )))),
#   df,
#   approach = "within",
#   estimand = "ATT",
#   stabilize = TRUE,
#   method = "ebal"
# )
#
# hist( mf$t1_exposure )


match_ml <- weightthem(
  as.formula(paste(as.formula(paste(
    paste("t1_exposure", "~",
          paste(cvars, collapse = "+"))
  )))),
  ml,
  estimand = "ATE",
  stabilize = TRUE,
  method = "ps"
)


saveRDS(match_ml, here::here(push_mods, "match_ml"))

sum <- summary(match_ml)
plot(sum)
sum
bal.tab(match_ml)
love.plot(match_ml)


match_ml_ebal <- weightthem(
  as.formula(paste(as.formula(paste(
    paste("t1_exposure", "~",
          paste(cvars, collapse = "+"))
  )))),
  ml,
  estimand = "ATE",
  stabilize = TRUE,
  method = "ebal"
)


saveRDS(match_ml_ebal, here::here(push_mods, "match_ml_ebal"))

sum <- summary(match_ml_ebal)
plot(sum)
sum
bal.tab(match_ml_ebal)

love.plot(match_ml_ebal)





match_ml_energy <- weightthem(
  as.formula(paste(as.formula(paste(
    paste("t1_exposure", "~",
          paste(cvars, collapse = "+"))
  )))),
  ml,
  estimand = "ATE",
  stabilize = TRUE,
  method = "energy"
)


saveRDS(match_ml_energy, here::here(push_mods, "match_ml_energy"))

sum <- summary(match_ml_energy)
plot(sum)
sum
bal.tab(match_ml_energy)

love.plot(match_ml_energy)




# examples ----------------------------------------------------------------


# standard deviation

Y <- "t1_Warm.Muslims_z"
X <- "t1_exposure"


fits_muslim <- lapply(complete(match_ml_ebal, "all"), function(d) {
  glm(
    as.formula(paste(
      paste(Y, "~", X, "*"),
      paste(cvars, collapse = "+")
    )),
    weights = weights,
    family = family,
    data = d
  )
})

mice::pool(fits_muslim)


# library("marginaleffects")
# comp.imp <- lapply(fits, function(fit) {
#   avg_comparisons(
#     fit,
#     newdata = subset(fit$data, a1 == 1),
#     variables = X,
#     wts = "weights",
#     vcov = "HC3"#,
#     #transform_pre = "lnratioavg"
#   )
# })
# comp.imp
#
# mus_tab  <- comp.imp
#
# mus_tab



# example sims ------------------------------------------------------------


library(clarify)
sim.imp_muslim <- misim(fits_muslim, n = 1000, vcov = "HC3") # robust standard errors


sim.att_muslim <- sim_ame(
  sim.imp_muslim,
  var = "t1_exposure",
  subset = t1_exposure == 1,
  cl = 8,
  # cores
  verbose = FALSE
)



# sim.att <- transform(sim.att, RR = `E[Y(1)]` / `E[Y(0)]`)
#
# sim.att
# summary(sim.att, null = c(RR = 1))
plot(sim.att_muslim)


sim_est_muslims <- transform(sim.att_muslim, `RD` = `E[Y(1)]` - `E[Y(0)]`)
summary(sim_est_muslims, null = c(`RD` = 0))

summary(sim_est_muslims)


# We used doubly robust estimation to identify the causal effect of the covid on prejudice.

# The Doubly Robust Estimation method for Subgroup Analysis Estimator is combines features of both IPTW and G-computation methods, providing unbiased estimates if either the propensity score or outcome model is correctly specified. The process involves five main steps:
#
#   Step 1 involves the estimation of the propensity score, a measure of the conditional probability of exposure given the covariates and the subgroup indicator. This score is calculated using statistical models such as logistic regression, with the model choice depending on the nature of the data and exposure. Weights for each individual are then calculated using this propensity score. These weights depend on the exposure status and are computed differently for exposed and unexposed individuals.


# Following Greifer 2023, we attempted a variety of balancing methods, chosing the method that acheieves the best balance. Figure x shows a love plot of the entropy balancing approach, which performed best.  As indicated in Appendix X, all Max.Diff.Adj scores were well-below the .05 threshold, meaning that weighting achieved good balance.



#
# Step 2 involves  fitting a weighted outcome model, making use of the previously calculated weights from the propensity scores. This model estimates the outcome conditional on exposure, covariates, and subgroup, integrating the weights into the estimation process. Unlike the propensity score model estimation, covariates are included as variables in the outcome model. This inclusion makes the method doubly robust - providing a consistent effect estimate if either the propensity score or the outcome model is correctly specified, thereby reducing the assumption of correct model specification.

# Here we used maximum likelihood estimation, modeling the outcome (prejudice) as a linear function of the exposure (A = 0,1), the baseline covariates (pre-exposure values of ....), and the weights obtained by the propensity score model (entropy-balanced)....



#
# Step 3 entails the simulation of potential outcomes for each individual in each subgroup. These hypothetical scenarios assume universal exposure to the intervention within each subgroup, regardless of actual exposure levels. The expectation of potential outcomes is calculated for each individual in each subgroup, using individual-specific weights. These scenarios are performed for everyone under both counterfactual conditions. One in which, perhaps contrary to fact, they recieved the covid lockdown, and one in which, perhaps contrary to fact, they did not.
#
# Step 4, we estimated of the average causal effect for each treatment condition, achieved by comparing the computed expected values of potential outcomes under each of the two intervention levels. The difference represents the average causal effect of changing the exposure across the population.
#
# Step 5 involves computing confidence intervals and standard errors for these calculations are determined using simulation-based inference and robust estimation of the error terms. (Greifer et al. 2023). This step recovers appropriate uncertainy for the different exposures.



## Results

prep_muslims <- summary(sim_est_muslims)
tab_muslims <- tab_ate(prep_muslims, new_name = "Muslim Warmth")

tab_muslims




## Another


# standard deviation

Y <- "t1_Warm.Pacific_z"
X <- "t1_exposure"


fits_pacific <- lapply(complete(match_ml_ebal, "all"), function(d) {
  glm(
    as.formula(paste(
      paste(Y, "~", X, "*"),
      paste(cvars, collapse = "+")
    )),
    weights = weights,
    family = family,
    data = d
  )
})




library(clarify)
sim.imp_pacific <- misim(fits_pacific, n = 1000, vcov = "HC3") # robust standard errors

sim.att_pacific <- sim_ame(
  sim.imp_pacific,
  var = "t1_exposure",
  subset = t1_exposure == 1,
  cl = 8,
  # cores
  verbose = FALSE
)




sim_est_pacific  <- transform(sim.att_pacific, `RD` = `E[Y(1)]` - `E[Y(0)]`)
summary(sim_est_pacific, null = c(`RD` = 0))

prep_pacific <- summary(sim_est_pacific)

tab_test_pacific <- tab_ate(prep_pacific, new_name = "Pacific Warmth")




input_group_tab <- rbind(tab_muslims,
      tab_test_pacific)


group_tab_test <- group_tab(input_group_tab, type = "RD")

group_tab_test |>
  kbl(format = "markdown")


# interpret table
interpret_table(group_tab_test, estimand = "ATE", causal_scale = "risk_difference")


# Nice graph
group_plot_ate(group_tab_test, title = "John Mark is in the best lab", subtitle = "We x0x0 JM",  x_offset = 0,
               x_lim_lo = -.5,
               x_lim_hi = .5)

#
# With an observed risk ratio of RR=1.34, an unmeasured confounder that was associated with both the outcome and the exposure by a risk ratio of 1.34-fold each, above and beyond the measured confounders, could explain away the estimate, but weaker joint confounder associations could not; to move the confidence interval to include the null, an unmeasured confounder that was associated with the outcome and the exposure by a risk ratio of 1.20-fold each could do so, but weaker joint confounder associations could not.



# Table interpretation:
#
#   Average Treatment Effect (ATE) represents the expected difference in outcomes between treatment and control groups for the whole population.
#
# For the outcome 'Pacific Warmth', the ATE causal contrast is -0.028. The confidence interval ranges from -0.071 to 0.015. The E-value for this outcome is 1.189, indicating evidence for causality is not conclusive.
#
# For the outcome 'Muslim Warmth', the ATE causal contrast i







## Discussion....

