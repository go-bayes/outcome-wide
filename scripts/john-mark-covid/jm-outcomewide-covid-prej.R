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
    "/Users/joseph/v-project\ Dropbox/Joseph\ Bulbulia/00Bulbulia\ Pubs/2021/DATA/time13"
  )

# for saving models
push_mods <-
  fs::path_expand("/Users/joseph/v-project\ Dropbox/Joseph\ Bulbulia/outcomewide/jm/mods")


dat <- arrow::read_parquet(pull_path)

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

table(dt_00$post_attacks)

## Analysis 1, only investigate effects 2019
table(dt_00$attacks_to_covid)
table(dt_00$post_attacks)

table1::table1(~ attacks_to_covid | Wave, data = dt_00)




## try without




dt_19 <- dt_00 |>
  dplyr::select(-c(TSCORE, attacks_to_covid)) |>
  dplyr::filter(covid_condition != "post-lockdown") |>
  droplevels() |>
  dplyr::rename(exposure = covid_condition) |>
  dplyr::mutate(across(
    !c(
      Id,
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

dt_19$post_attacks

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


dim(dt_wide)
colnames(dt_wide)
naniar::gg_miss_var(dt_wide)

# order correctly for imputation
dt_wide <- dt_wide |>
  relocate(starts_with("t0"), .before = starts_with("t1")) |>
  relocate("t0_exposure", .before = "t0_post_attacks") |>
  relocate("t1_exposure", .before = "t1_Partner") |>
  select(-t1_post_attacks)
colnames(dt_wide)



# test
table((dt_wide$t1_exposure))

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
  select(-t0_exposure)

colnames(dt_wide2)


# QUICK CHECK
model_parameters(lm(data = dt_wide2,
                    t1_Warm.Muslims ~ t1_exposure + t0_Warm.Muslims))

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

cvars_sim = mf |>
  dplyr::select(
    -c(
      t0_EthCat_z,
      t0_SampleFrame_z,
      t0_REGC_2022_z,
      t0_Rural_GCH2018_z,
      t0_post_attacks_z,
      t0_SampleFrame
    )
  ) |> # include?
  dplyr::select(starts_with("t0_")  &
                  !ends_with("_z") & !starts_with("t0_Warm"))

cvars_sim






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

cvars_sim

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
  estimand = "ATT",
  stabilize = TRUE,
  method = "ps"
)


saveRDS(match_ml, here::here(push_mods, "match_ml"))

sum <- summary(match_ml)
plot(sum)
sum
bal.tab(match_ml)




# examples ----------------------------------------------------------------


# standard deviation

Y <- "t1_Warm.Muslims_z"
X <- "t1_exposure"


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

mus_tab  <- comp.imp

mus_tab



# example sims ------------------------------------------------------------


library(clarify)
sim.imp <- misim(fits, n = 1000, vcov = "HC3")
sim.imp
summary(sim.imp)

sim.att <- sim_ame(
  sim.imp,
  var = "t1_exposure",
  subset = t1_exposure == 1,
  cl = 8,
  # cores
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




# # buls way - doubly robust ----------------------------------------------


# GET DATA INTO ORDER

# make into mids
#Extracting the original dataset with missing value
maindataset <- complete(match_ml, action = 0)

#Some spit-and-polish
maindataset <-
  data.frame(.imp = 0, .id = seq_len(nrow(maindataset)), maindataset)

#Extracting imputed-weighted datasets in the long format
alldataset  <- complete(match_ml, action = "long")

#Binding them together
alldataset  <- rbind(maindataset, alldataset)

#Converting to .mids
newmids <- as.mids(alldataset)

# set data
dff <- newmids


# old way setup
# You may set your label for your graphs

xlab = "Muslim Warmth in SD"


# SET THE RANGE OF EXPOSURE FROM ZERO TO 1
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
X = "t1_exposure"

sub = "Covid Lockdown Conditions"
Y = "t1_Warm.Muslims_z"
main = "Warm Muslims"
ylab = "Warm Muslims (SD)"

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


warm_muslims_t <- out_ct %>%
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
warm_muslims_t



# compare with above -- VERY CLOSE!!
mus_tab


# graph
warm_muslims_p <-
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
warm_muslims_p
# coef + estimate
warm_muslims_e <-
  vanderweelevalue_ols(out_ct, f - min, delta, sd)
warm_muslims_e




# only iptw ---------------------------------------------------------------

X = "t1_exposure"

sub = "Covid Lockdown Conditions"
Y = "t1_Warm.Muslims_z"
main = "Warm Muslims"
ylab = "Warm Muslims (SD)"

# regression

out_m <-
  mice_iptw_lin(
    df = dff,
    X = X,
    Y = Y,
    cvars = 1,  # NOTE CHANGE
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


warm_muslims_iptw_t <- out_ct %>%
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




# compare with above -- VERY CLOSE!!
mus_tab
warm_muslims_t
warm_muslims_iptw_t

# graph
warm_muslims_iptw_p <-
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
warm_muslims_iptw_p
# coef + estimate
warm_muslims_iptw_p <-
  vanderweelevalue_ols(out_ct, f - min, delta, sd)
warm_muslims_iptw_e



# G-computation only -----------------------------------------------------------
mice_generalised_lin = function(df, X, Y, cvars, family) {
  require("mice")
  out <- with(df, glm(as.formula(paste(
    paste(Y, "~", X, "+"),
    paste(cvars, collapse = "+")
  )), family = family))
  out
}



X = "t1_exposure"
sub = "Covid Lockdown Conditions"
Y = "t1_Warm.Muslims_z"
main = "Warm Muslims"
ylab = "Warm Muslims (SD)"
family = "gaussian"

# regression

out_m <-
  mice_generalised_lin(
    df = dff,
    X = X,
    Y = Y,
    cvars = cvars,  # NOTE CHANGE
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


warm_muslims_gcomp_t <- out_ct %>%
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




# compare with above -- VERY CLOSE!!
mus_tab
warm_muslims_t
warm_muslims_iptw_t
warm_muslims_gcomp_t

# graph
warm_muslims_gcomp_p <-
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
warm_muslims_gcomp_p
# coef + estimate
warm_muslims_iptw_e <-
  vanderweelevalue_ols(out_ct, f - min, delta, sd)
warm_muslims_iptw_e





