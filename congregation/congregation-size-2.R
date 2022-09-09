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
# read libraries
source("https://raw.githubusercontent.com/go-bayes/templates/main/functions/libs.R")

# read functions
source("https://raw.githubusercontent.com/go-bayes/templates/main/functions/funs.R")


table <- dff |>
  filter(YearMeasured ==1) |>
  filter(Wave == 2018)

table1::table1(~Religious_Group, data = table)

# # read data

dff <- readRDS(here::here("data_raw", "df.Rds"))

# Denominations
# dff %>%
#   filter(Wave == 2016 &  YearMeasured == 1) %>%
#   n_distinct("Id")
#
# table(dff$Bigger_Doms)
# # table for participant N
# dc <- dff %>%
#   arrange(Id, Wave) %>%
#   dplyr::mutate(Religion.CongregationSize = ifelse(Religion.Church == 0, 0,  Religion.CongregationSize)) |> #handle missingness
#   dplyr::mutate(Euro = if_else(EthCat == 1, 1, 0)) %>%
#   dplyr::mutate(Male = ifelse(GendAll == 1, 1, 0)) %>%
#   dplyr::filter((Wave == 2016  & YearMeasured  == 1) |
#                   (Wave == 2017  & YearMeasured  == 1) |
#                   (Wave == 2018 &
#                      YearMeasured != -1)
#   )  %>% # Eligibility criteria
#   # dplyr::filter(Id != 9630) %>% # problematic for income
#   group_by(Id) %>%
#   dplyr::mutate(org2 = ifelse(Wave == 2017 &
#                                 YearMeasured == 1, 1, 0)) %>%  # creating an indicator for the first wave
#   dplyr::mutate(hold2 = mean(org2, na.rm = TRUE)) %>%  # Hack0
#   dplyr::filter(hold2 > 0) %>% # hack to enable repeat of baseline in 201
#   dplyr::mutate(org1 =  ifelse(Wave == 2016 &
#                                  YearMeasured == 1, 1, 0)) %>%  # creating an indicator for the first wave
#   dplyr::mutate(hold1 = mean(org1, na.rm = TRUE)) %>%  # Hack
#   dplyr::filter(hold1 > 0) %>% # hack to enable repeat of baseline
#   ungroup()
#

# increasing rate
dff %>%
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
    Parent,
    Partner,
    ChildrenNum,
    Edu,
    NZdep,
    NZSEI13,
    Urban,
    EthCat,
    BornNZ,
    Edu,
    Employed,
    Smoker,
    retired,
    Alcohol.Frequency,
    Alcohol.Intensity,
    semiretired,
    HLTH.Disability,
    National.Identity,
    NWI,
    PATRIOT,
    RWA,
    SDO,
    AGREEABLENESS,
    CONSCIENTIOUSNESS,
    EXTRAVERSION,
    HONESTY_HUMILITY,
    NEUROTICISM,
    OPENNESS,
    Religion.CongregationSize,
    HoursCharity,
    Hours.Exercise,
    Hours.Work,
    Pol.Orient,
    Believe.Spirit,
    Believe.God,
    Relid,
    Bigger_Doms,
    Religion.Church2,
    Religion.Prayer,
    Religion.Scripture,
    Respect.Self,
    CharityDonate,
    KESSLER6sum,
    BELONG,
    LIFESAT,
    SUPPORT,
    SFHEALTH,
    Standard.Living,
    SELF.CONTROL,
    SELF.ESTEEM,
    SWB.SoC01,
    Your.Health,
    Your.Future.Security,
    Your.Personal.Relationships#
  ) |>
  dplyr::rename(community = SWB.SoC01) %>%
  dplyr::mutate(Edu = as.numeric(Edu)) %>%
  dplyr::mutate(across(!c(Id, Wave, Bigger_Doms), ~ as.numeric(.x))) %>% # make factors numeric for easy of processing
  arrange(Id, Wave) %>%
  rename(Religion.Church = Religion.Church2) %>%
  dplyr::mutate(
    Edu = as.numeric(Edu),
    Volunteers = if_else(HoursCharity == 1, 1, 0),
    Church = ifelse(Religion.Church > 8, 8, Religion.Church)
  ) %>%
  arrange(Id, Wave)  %>%
  dplyr::mutate(
    Religion.CongregationSize_lead1 = lead(Religion.CongregationSize, n = 1),
    NZdep_lead1  = lead(NZdep, n = 1),
    Bigger_Doms_lead1 = lead(Bigger_Doms, n = 1)
  ) %>%
  dplyr::mutate(SUPPORT_lead1 = lead(SUPPORT, n = 1)) %>%
  dplyr::mutate(across(
    c(
      Religion.Church,
      Religion.Prayer,
      Religion.Scripture,
      Believe.Spirit,
      Believe.God,
      BELONG,
      CharityDonate,
      HoursCharity,
      Hours.Work,
      Employed,
      SELF.CONTROL,
      SELF.ESTEEM,
      SFHEALTH,
     # Household.INC, #Highly skewed, avoid, use NZSEI
      Hours.Exercise,
      #  ImpermeabilityGroup,
      KESSLER6sum,
      # LIFEMEANING,
      LIFESAT,
      National.Identity,
      NWI,
      PATRIOT,
      SUPPORT,
      community,
      Relid,
      # Rumination,
      #SexualSatisfaction,
      Standard.Living,
      Your.Health,
      Your.Future.Security,
      Your.Personal.Relationships,

    ),
    ~ lead(.x, n = 2),
    .names = "{col}_lead2"
  )) %>% # make leads
  dplyr::filter(Wave == 2016) %>%
  dplyr::filter(!is.na(Religion.CongregationSize)) %>%
  dplyr::filter(!is.na(Religion.CongregationSize_lead1)) %>%
  dplyr::filter(Relid > 0) %>%
  dplyr::select(-c(YearMeasured,
                   NZdep)) %>%
  rename(NZdep = NZdep_lead1) |>   ## much lower missingness
  dplyr::filter(Relid != 0) |>
  droplevels() |>
  dplyr::mutate(across(!c(Id, Bigger_Doms, Wave), ~ as.numeric(.x))) %>% # make factors numeric for easy of processing
  droplevels() |>
  arrange(Id)

#table(is.na(dcc$NZSEI13))

# inspect data
skim(da) %>%
  arrange(n_missing)

table(!is.na(dcc$Religion.CongregationSize))

# number of ids
N <- length(unique(dcc$Id))
N  # 5452

# table
dcct <- dcc
# make labels as follows
table(dcc$Bigger_Doms)

dcct <-  dcc |>
  dplyr::mutate(Volunteers = if_else(HoursCharity > 0, 1, 0))

dcct <- dcc |> dplyr::group_by(Id) |> mutate(PWI = mean(
  c(
    Your.Future.Security,
    Your.Personal.Relationships,
    Your.Health,
    Standard.Living
  ),
  na.rm = TRUE
))

dcct$Male <- factor(dcc$Male, labels = c("No", "Yes"))
dcct$EthCat <-
  factor(dcc$EthCat, labels = c("Euro", "Maori", "Pacific", "Asian"))
dcct$Believe.Spirit <-
  factor(dcc$Believe.Spirit, labels = c("No", "Yes"))
dcct$Believe.God <-
  factor(dcc$Believe.God, labels = c("No", "Yes"))
dcct$Employed <- factor(dcc$Employed, labels = c("No", "Yes"))
dcct$Volunteers <-
  factor(dcct$Volunteers, labels = c("No", "Yes"))
dcct$Parent <- factor(dcc$Parent, labels = c("No", "Yes"))
dcct$Partner <- factor(dcc$Partner, labels = c("No", "Yes"))
dcct$Retired <- factor(dcc$retired, labels = c("No", "Yes"))
dcct$SemiRetired <-
  factor(dcc$semiretired, labels = c("No", "Yes"))
dcct$Urban <- factor(dcc$Urban, labels = c("No", "Yes"))
#dcct$BigDoms <-factor(dcc$BigDoms, labels = c("Buddhist", "Christian","Muslim","TheOthers"))
dcct$NeighbourhoodCommunity <- dcc$community
dcct$MajorDenominations <- dcc$Bigger_Doms




##and continue this way to obtain factor labels ...etc.

table1::table1(
  ~ Age +
    BornNZ +
    Edu +
    Employed +
    EthCat +
    NZdep +
    NZSEI13 +
    Parent +
    Partner +
    Pol.Orient +
    Male +
    Urban,
  data = dcct,
  transpose = F
)

# Personality


table1::table1(
  ~ AGREEABLENESS +
    CONSCIENTIOUSNESS +
    EXTRAVERSION +
    HONESTY_HUMILITY +
    NEUROTICISM +
    OPENNESS +
    KESSLER6sum +
    LIFESAT +
    PWI +
    Respect.Self +
    RWA +
    SDO +
    SELF.CONTROL +
    SELF.ESTEEM +
    SFHEALTH,
  data = dcct,
  transpose = F
)


# religious
table1::table1(
  ~ Religion.CongregationSize +
    Relid +
    Religion.Prayer +
    Religion.Scripture +
    Church +
    Believe.Spirit +
    Believe.God |
    MajorDenominations,
  data = dcct,
  transpose = F
)



# Social variables
table1::table1(
  ~  Relid +
    SUPPORT +
    BELONG +
    NeighbourhoodCommunity +
    CharityDonate +
    Volunteers +
    National.Identity,
  data = dcct,
  transpose = F
)



# mice model  -------------------------------------------------------------
library(mice)
mice_cc <- dcc %>%
  dplyr::mutate(across(!c(Id, Wave), ~ as.numeric(.x))) %>%
  #  mutate(across(where(is.double), as.numeric)) |>
  dplyr::select(-c(Wave, Id)) |> data.frame()
#table(mice_cc$Bigger_Doms_lead1 == mice_cc2$Bigger_Doms)


library(naniar)
str(mice_cc)
naniar::gg_miss_var(mice_cc)
vis_miss(mice_cc,
         warn_large_data = FALSE)

# any colinear vars?
mice:::find.collinear(mice_cc)
str(mice_cc)
# impute
mice_cc_out <- mice::mice(mice_cc,  seed = 0, m = 10)

`# save
saveh(mice_cc_out, "mice_cc_out")

mice_cc_out <- readh("mice_cc_out")
# check s
outlist2 <-
  row.names(mice_cc_out)[mice_cc_out$outflux < 0.5]
length(outlist2)

# checks

# we create two completed data sets -- the one without the missing data will be useful for
# determing causal contrasts -- which we'll describe below.

cc_l <- mice::complete(mice_cc_out, "long", inc = TRUE)

# inspect data -- what do we care about?  Note that the moderate distress category doesn't look useful
skimr::skim(cc_l)
# create variables in z score
cc_l2 <- cc_l %>%
  dplyr::mutate(EthCat = as.factor(EthCat)) |>
  dplyr::mutate(Volunteers_lead2 = if_else(HoursCharity_lead2 > 0, 1, 0)) |>
  # dplyr::mutate(income_log = log(Household.INC + 1)) |>
  # dplyr::mutate(income_log_lead2 = log(Household.INC_lead2 + 1)) |>
  # dplyr::mutate(newkids = ChildrenNum_lead2 - ChildrenNum) %>%
  dplyr::mutate(KESSLER6sum_lead2 = round(as.integer(KESSLER6sum_lead2, 0))) %>%
  dplyr::mutate(CharityDonate_lead2 = round(CharityDonate_lead2, 0)) %>%
  #  plyr::mutate(Alcohol.Intensity = round(Alcohol.Intensity, 0)) %>%
  dplyr::mutate(CharityDonate = round(CharityDonate, 0)) %>%
  dplyr::mutate(Hours.Exercise = round(Hours.Exercise, 0)) %>%
  dplyr::mutate(CharityDonate_log_lead2 = log(CharityDonate_lead2 + 1)) %>%
  dplyr::mutate(CharityDonate_log = log(CharityDonate + 1)) %>%
  dplyr::mutate(SUPPORT_lead2ord = as.integer(round(SUPPORT_lead2, digits = 0))) %>%
  dplyr::mutate(Hours.Exercise_log = log(Hours.Exercise + 1)) %>%
  dplyr::mutate(Hours.Exercise_log_lead2 = log(Hours.Exercise_lead2 + 1)) %>%
  dplyr::mutate(Believe.God = Believe.God - 1) %>%  # so that start is at zero
  dplyr::mutate(Believe.God_lead2 = Believe.God_lead2 - 1) %>%  # so that start is at zero
  dplyr::mutate(Believe.Spirit = Believe.Spirit - 1) %>% # so that start is at zero
  dplyr::mutate(Believe.Spirit_lead2 = Believe.Spirit_lead2 - 1) %>% # so that start is at zero
  dplyr::mutate(LIFESAT_lead2ord = as.integer(round(LIFESAT_lead2, digits = 0))) %>%
  # dplyr::mutate(alcohol_bin2 = if_else(Alcohol.Frequency > 3, 1, 0)) %>%
  # dplyr::mutate(alcohol_bin = if_else(Alcohol.Frequency > 2, 1, 0)) %>%
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
  #  dplyr::mutate(NZSEI13_10 =  NZSEI13/10)%>%
  #  dplyr::mutate(NZSEI13_lead2_10 =  as.integer(NZSEI13_lead2/10))%>%
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
  dplyr::mutate(EthCat = as.factor(EthCat)) |>
  dplyr::mutate(Religion.CongregationSize_dunbar1 = as.factor(Religion.CongregationSize_dunbar1)) |>
  dplyr::mutate(
    Religion.CongregationSize_lead1_dunbar1 = as.factor(Religion.CongregationSize_lead1_dunbar1)
  ) |>
  dplyr::mutate(Bigger_Doms = as.factor(Bigger_Doms)) |>
  dplyr::mutate(Bigger_Doms_lead1 = as.factor(Bigger_Doms_lead1)) # not used




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
cvars = c(
  "Religion.CongregationSize_log",
  "Bigger_Doms",
  # additive interaction
  "Age_z",
  "Male_z",
  "Edu_z",
  "Urban_z",
  "EthCat",
  "AGREEABLENESS_z",
  "CONSCIENTIOUSNESS_z",
  "EXTRAVERSION_z",
  "HONESTY_HUMILITY_z",
  "National.Identity_z",
  "NEUROTICISM_z",
  "OPENNESS_z",
  "BELONG_z",
  "Believe.Spirit_z",
  "Believe.God_z",
  "BornNZ_z",
  "CharityDonate_z",
  "ChildrenNum_z",
  "community_z",
  "Edu_z",
  "Employed_z",
  # "income_log_z", Skewed
  "Volunteers_z",
   "Hours.Exercise_log_z",
  "Hours.Work_10_z",
  "KESSLER6sum_z",
  "LIFESAT_z",
  "NWI_z",
  "NZdep_z",
  "NZSEI13_z",
  "PATRIOT_z",
  "Parent_z",
  "Partner_z",
  "Pol.Orient_z",
  "PWI_z",
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
  # "Smoker_z",
  #  "Standard.Living_z",
  "SUPPORT_z"
  # "Your.Health_z",
  # "Your.Future.Security_z",
  # "Your.Personal.Relationships_z"#,
  # "Alcohol.Frequency_z",
  # "Alcohol.Intensity_z"
)


# functions ---------------------------------------------------------------

# set up ---------------------------------------------------------------
# Approximately how many people belong to the church or place of worship that you attend most often?


###### EXPOSURE VARIABLE DEFINED HERE FROM THE MICE IMPUTED DATA SET

X = "Religion.CongregationSize_lead1_log"


#### DEFINE THE MICE IMPUTED DATA SET AS "DF"

df <-  ccu


####################################################################


xlab = "Log Religious Congregation Size"

min = 0
max = 7
# baseline
r = 0
# focal contrast
f = 5

# range of estimates
x =  min:max

# for model functions
c = x

# contrast for graphs -- absolute distance from baseline
p = c(0, 5)
p1 = c(1, 5) # for church

# Needed for E-VALUES -- how much do we move on the X scale to obtain our effect?
delta = 5 #
delta1 = 4 # for church


## PLOT LIMITS ON Y AXIS
ylim = c(-.4, .75)

# n imputations
m = 10
ylim_contrast <- c(.5, 1.5)


# FOCAL HYPOTHESIS RELIGIOUS ID --------------------------------------------------------


# Religious ---------------------------------------------------------------
# Religiosity
# Do you identify with a religion and/or spiritual group?
#   What religion or spiritual group?
#   Religious Identification
# How important is your religion to how you see yourself?

Y = "Relid_lead2_z"
main = "Religious Identification (SD)"
ylab = "Religious Identification (SD)"
sub = "Do you identify with a religion and/or spiritual group?\nWhat religion or spiritual group?\nHow important is your religion to how you see yourself?"

# regression
out_m <- mice_gaussian(df = df, X = X, Y = Y, cvars = cvars)

## g-computation
out_ct <-
  pool_stglm_contrast(
    out_m,
    df = df,
    m = 10,
    X = X,
    x = c,
    r = r
  )

relid_c <- vanderweelevalue_ols(out_ct, f, delta, sd)
relid_c

relid_t <- out_ct %>%
  slice(1:9) |>
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
  row_spec(c(f + 1),
           bold = T,
           color = "white",
           background = "dodgerblue") |>
  kable_minimal(full_width = F)
# show table
relid_t
# graph
relid_p <-
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
relid_p

# SOCIAL CONNECTION AND BELONGING -----------------------------------------


# belonging ---------------------------------------------------------------
# Felt belongingness
# Know that people in my life accept and value me.
# Feel like an outsider.
# Know that people around me share my attitudes and beliefs.


Y = "BELONG_lead2_z"
main = "Social Belonging"
ylab = "Social Belonging (SD)"
sub = "Know that people in my life accept and value me.\nFeel like an outsider.\nKnow that people around me share my attitudes and beliefs."


# regression
out_m <- mice_gaussian(df = df, X = X, Y = Y, cvars = cvars)

## g-computation
out_ct <-
  pool_stglm_contrast(
    out_m,
    df = df,
    m = 10,
    X = X,
    x = c,
    r = r
  )
out_ct %>%
  slice(f + 1 - min) |>
  kbl(digits = 3, "markdown")

belong_c <-  vanderweelevalue_ols(out_ct, f - min, delta, sd)
belong_c


belong_t <- out_ct %>%
  slice(1:max) |>
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
  row_spec(c(f + 1 - min),
           bold = T,
           color = "white",
           background = "dodgerblue") |>
  kable_minimal(full_width = F)
# show table
belong_t
# graph
belong_p <-
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
belong_p

# community ----------------------------------------------------------
#I feel a sense of community with others in my local neighbourhood.
Y = "community_lead2_z"
main = "Community"
ylab = "Community (SD)"
sub = "I feel a sense of community with others\nin my local neighbourhood."

# regression
out_m <- mice_gaussian(df = df, X = X, Y = Y, cvars = cvars)

## g-computation
out_ct <-
  pool_stglm_contrast(
    out_m,
    df = df,
    m = 10,
    X = X,
    x = c,
    r = r
  )
out_ct %>%
  slice(f + 1 - min) |>
  kbl(digits = 3, "markdown")

community_c <-  vanderweelevalue_ols(out_ct, f - min, delta, sd)
community_c

community_t <- out_ct %>%
  slice(1:max) |>
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
  row_spec(c(f + 1 - min),
           bold = T,
           color = "white",
           background = "dodgerblue") |>
  kable_minimal(full_width = F)
# show table
community_t
# graph
community_p <-
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
community_p

# soc support -------------------------------------------------------------
# Perceived social support
# There are people I can depend on to help me if I really need it.
# There is no one I can turn to for guidance in times of stress.
# I know there are people I can turn to when I need help.
## fit
Y = "SUPPORT_lead2_z"
main = "Social Support"
ylab = "Social Support (SD)"
sub = 'There are people I can depend on to help me if I really need it.\nThere is no one I can turn to for guidance in times of stress.\nI know there are people I can turn to when I need help.'


# regression
out_m <- mice_gaussian(df = df, X = X, Y = Y, cvars = cvars)

## g-computation
out_ct <-
  pool_stglm_contrast(
    out_m,
    df = df,
    m = 10,
    X = X,
    x = c,
    r = r
  )
out_ct %>%
  slice(f + 1 - min) |>
  kbl(digits = 3, "markdown")


support_c <-  vanderweelevalue_ols(out_ct, f - min, delta, sd)
support_c


support_t <- out_ct %>%
  slice(1:max) |>
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
  row_spec(c(f + 1 - min),
           bold = T,
           color = "white",
           background = "dodgerblue") |>
  kable_minimal(full_width = F)
# show table
support_t
# graph
support_p <-
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
support_p



# charity donate ----------------------------------------------------------
#How much money have you donated to charity in the last year?

Y = "CharityDonate_log_lead2_z"
main = "Charity Donations (annual)"
ylab = "Charity Donations (annual)"
sub = "How much money have you donated to charity in the last year?"

# regression
out_m <- mice_gaussian(df = df, X = X, Y = Y, cvars = cvars)

## g-computation
out_ct <-
  pool_stglm_contrast(
    out_m,
    df = df,
    m = 10,
    X = X,
    x = c,
    r = r
  )
out_ct %>%
  slice(f + 1 - min) |>
  kbl(digits = 3, "markdown")

charity_c <-  vanderweelevalue_ols(out_ct, f - min, delta, sd)
charity_c

charity_t <- out_ct %>%
  slice(1:max) |>
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
  row_spec(c(f + 1 - min),
           bold = T,
           color = "white",
           background = "dodgerblue") |>
  kable_minimal(full_width = F)
# show table
charity_t
# graph

charity_p <-
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
charity_p


# volunteers --------------------------------------------------------------
#Hours spent in activities
#Hours spent … voluntary/charitable work

Y = "Volunteers_lead2"
main = "Volunteer (RR)"
ylab = "Volunteer (Risk Ratio)"
family = "poisson" # binary outcome not rare
sub = "Hours spent … voluntary/charitable work"
# clean oven
rm(out_m)
rm(out_ct)
# fit regression model
out_m <- mice_generalised(df = df,
                          X = X,
                          Y = Y,
                          family = family,
                          cvars = cvars)
# g-computation - contrasts
out_ct <-
  pool_stglm_contrast_ratio(
    out_m,
    df = df,
    m = 10,
    X = X,
    x = c,
    r = r
  )
#table

# coef + estimate
volunteers_c <- vanderweelevalue_rr(out_ct, f)
volunteers_c


volunteers_t <- out_ct %>%
  slice(1:max) |>
  tibble() |>
  rename(
    Contrast = row,
    Estimate = est,
    std_error = se,
    CI_hi = ui,
    CI_lo = li
  ) |>
  kbl(caption = main,
      digits = 3,
      "html") |>
  kable_styling() %>%
  row_spec(c(f + 1 - min),
           bold = T,
           color = "white",
           background = "dodgerblue") |>
  kable_minimal(full_width = F)
volunteers_t
volunteers_p <-
  ggplot_stglm(
    out_ct,
    ylim = ylim_contrast,
    main,
    xlab,
    ylab,
    min = min,
    p = p,
    sub = sub
  ) +  expand_limits(x = 0, y = 0)
volunteers_p


## NEGATIVE CONTROLS
#
# # Patriotism --------------------------------------------------------------
# # I feel a great pride in the land that is our New Zealand.
# # Although at times I may not agree with the government, my commitment to New Zealand always remains strong.
#
# Y = "PATRIOT_lead2_z"
# main = "Patriotism"
# ylab = "Patriotism (SD)"
# sub = "I feel a great pride in the land that is our New Zealand.\nAlthough at times I may not agree with the government, my commitment to\nNew Zealand always remains strong."
#
# # regression
# out_m <- mice_gaussian(df = df, X = X, Y = Y, cvars = cvars)
#
# ## g-computation
# out_ct <-
#   pool_stglm_contrast(
#     out_m,
#     df = df,
#     m = 10,
#     X = X,
#     x = c,
#     r = r
#   )
# out_ct %>%
#   slice(f + 1) |>
#   kbl(digits = 3, "markdown")
#
# patriot_c <- vanderweelevalue_ols(out_ct, f, delta, sd)
# patriot_c
#
#
# patriot_t <- out_ct %>%
#   slice(1:9) |>
#   tibble() |>
#   rename(
#     Contrast = row,
#     Estimate = est,
#     Std_error = se,
#     CI_hi = ui,
#     CI_lo = li
#   ) |>
#   kbl(caption = main,
#       digits = 3,
#       "html") |>
#   kable_styling() %>%
#   row_spec(c(f + 1),
#            bold = T,
#            color = "white",
#            background = "dodgerblue") |>
#   kable_minimal(full_width = F)
# # show table
# patriot_t
# # graph
# patriot_p <-
#   ggplot_stglm(
#     out_ct,
#     ylim = ylim,
#     main,
#     xlab,
#     ylab,
#     min = min,
#     p = p,
#     r = r,
#     sub = sub
#   )
# patriot_p


# National.Identity_lead2_z--------------------------------------------------------------
#I identify with New Zealand.

Y = "National.Identity_lead2_z"
main = "National Identity"
ylab = "National Identity (SD)"
sub = "I identify with New Zealand."
# regression
out_m <- mice_gaussian(df = df, X = X, Y = Y, cvars = cvars)

## g-computation
out_ct <-
  pool_stglm_contrast(
    out_m,
    df = df,
    m = 10,
    X = X,
    x = c,
    r = r
  )
out_ct %>%
  slice(f + 1) |>
  kbl(digits = 3, "markdown")

national_c <- vanderweelevalue_ols(out_ct, f, delta, sd)
national_c


national_t <- out_ct %>%
  slice(1:9) |>
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
  row_spec(c(f + 1),
           bold = T,
           color = "white",
           background = "dodgerblue") |>
  kable_minimal(full_width = F)
# show table
national_t
# graph
national_p <-
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
national_p


# NWI ---------------------------------------------------------------------

# national wellbeing ------------------------------------------------------
#
# # National Wellbeing Index
# # The economic situation in New Zealand.
# # The social conditions in New Zealand.
# # Business in New Zealand.
#
# Y = "NWI_lead2_z"
# main = "National Well Being"
# ylab = "National Well Being (SD)"
#
# # regression
# out_m <- mice_gaussian(df = df, X = X, Y = Y, cvars = cvars)
#
# ## g-computation
# out_ct <-
#   pool_stglm_contrast(
#     out_m,
#     df = df,
#     m = 10,
#     X = X,
#     x = c,
#     r = r
#   )
# out_ct %>%
#   slice(f + 1) |>
#   kbl(digits = 3, "markdown")
#
# nwi_c <- vanderweelevalue_ols(out_ct, f, delta, sd)
# nwi_c
#
# nwi_t <- out_ct %>%
#   slice(1:9) |>
#   tibble() |>
#   rename(
#     Contrast = row,
#     Estimate = est,
#     Std_error = se,
#     CI_hi = ui,
#     CI_lo = li
#   ) |>
#   kbl(caption = main,
#       digits = 3,
#       "html") |>
#   kable_styling() %>%
#   row_spec(c(f + 1),
#            bold = T,
#            color = "white",
#            background = "dodgerblue") |>
#   kable_minimal(full_width = F)
# # show table
# nwi_t
# # graph
#
# nwi_p <-
#   ggplot_stglm(
#     out_ct,
#     ylim = ylim,
#     main,
#     xlab,
#     ylab,
#     min = min,
#     p = p,
#     r = r,
#     sub = sub
#   )
# nwi_p
#

# TABLE SOCIAL WELLBEING --------------------------------------------------
main = "Social connection estimands / Evalues"
social_tab <- rbind(relid_c,
                    belong_c,
                    community_c,
                    support_c,
                    charity_c,
                    volunteers_c,
                    national_c)

social_tab |>
  kbl(caption = main,
      digits = 3,
      "html") |>
  kable_styling() %>%
  row_spec(c(1:6),
           bold = T,
           color = "black",
           background = "bold") |>
  kable_minimal(full_width = F)

social_tab

# GRAPH SOCIAL ------------------------------------------------------------
social_fig <-   relid_p +
belong_p+
community_p+
support_p+
charity_p+
volunteers_p+
national_p+
  plot_annotation(title = "Causal effects of religious community size on religious identity & cooperation") +
  plot_layout(guides = 'collect') #+ plot_layout(nrow = 3, byrow = T)

# view
social_fig


ggsave(
  social_fig,  # LABEL
  path = here::here(here::here("figs", "congregation", "social_fig")),  # LABEL
  width = 15,
  height = 10,
  units = "in",
  filename = "social_fig.jpg",  ## LABEL
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 1200
)

# scripture ---------------------------------------------------------------
#### RELIGIOUS OUTCOMES
# How many times did you read religious scripture in the last week?
Y = "Religion.Scripture_lead2_log_z"
main = "Log weekly Scripture Reading (SD)"
ylab = "Log weekly Scripture Reading (SD)"
sub = "How many times did you read religious scripture in the last week?"
# regression
out_m <- mice_gaussian(df = df, X = X, Y = Y, cvars = cvars)

## g-computation
out_ct <-
  pool_stglm_contrast(
    out_m,
    df = df,
    m = 10,
    X = X,
    x = c,
    r = r
  )
out_ct %>%
  slice(f + 1 - min) |>
  kbl(digits = 3, "markdown")

scripture_c <-  vanderweelevalue_ols(out_ct, f - min, delta, sd)
scripture_c


scripture_t <- out_ct %>%
  slice(1:max) |>
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
  row_spec(c(f + 1 - min),
           bold = T,
           color = "white",
           background = "dodgerblue") |>
  kable_minimal(full_width = F)
# show table
scripture_t
# graph
scripture_p <-
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
scripture_p
# log prayer --------------------------------------------------------------
# How many times did you pray in the last week?
Y = "Religion.Prayer_lead2_log_z"
main = "Log Weekly Prayer  (SD)"
ylab = "Log Weekly Prayer (SD)"
sub = "How many times did you pray in the last week?"
# regression
out_m <- mice_gaussian(df = df, X = X, Y = Y, cvars = cvars)

## g-computation
out_ct <-
  pool_stglm_contrast(
    out_m,
    df = df,
    m = 10,
    X = X,
    x = c,
    r = r
  )
out_ct %>%
  slice(f + 1 - min) |>
  kbl(digits = 3, "markdown")

prayer_c <-  vanderweelevalue_ols(out_ct, f - min, delta, sd)
prayer_c


prayer_t <- out_ct %>%
  slice(1:max) |>
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
  row_spec(c(f + 1 - min),
           bold = T,
           color = "white",
           background = "dodgerblue") |>
  kable_minimal(full_width = F)
# show table
prayer_t
# graph
prayer_p <-
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
prayer_p

# “With an observed risk ratio of RR=3.4 , an unmeasured confounder that was associated with both the outcome and the exposure by a risk ratio of 6.2-fold each, above and beyond the measured confounders, could explain away the estimate, but weaker joint confounder associations could not; to move the confidence interval to include the null, an unmeasured confounder that was associated with the outcome and the exposure by a risk ratio of 4.0-fold each could do so, but weaker joint confounder associations could not.”


# church attendance -------------------------------------------------------
# How many times did you attend a church or place of worship in the last month?
Y = "Religion.Church_lead2_log_z"
main = "Log Monthly Church  (SD)"
ylab = "Log Monthly Church (SD)"
sub = "How many times did you attend a church\nor place of worship in the last month?"
# regression
out_m <- mice_gaussian(df = df, X = X, Y = Y, cvars = cvars)
## g-computation
out_ct <-
  pool_stglm_contrast(
    out_m,
    df = df,
    m = 10,
    X = X,
    x = c,
    r = r + 1  # church zero ill defined
  )

out_ct %>%
  slice(f + 1 - min) |> # church zero ill defined
  kbl(digits = 3, "markdown")

church_c <-  vanderweelevalue_ols(out_ct, f - min + 1, delta1, sd)
church_c

church_t <- out_ct %>%
  slice(1:max) |>
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
  row_spec(c(f + 1 - min),
           bold = T,
           color = "white",
           background = "dodgerblue") |>
  kable_minimal(full_width = F)
# show table
church_t
# graph


church_p <-
  ggplot_stglm(
    out_ct,
    ylim = c(-.4, .8),
    main,
    xlab,
    ylab,
    min = min,
    p = p1,
    sub = sub
  )

church_p
# believe God -------------------------------------------------------------
# fit
Y = "Believe.God_lead2"
family = "poisson" # outcome not rare
main = "Belief in God"
ylab = "God Belief (RR)"
family = "poisson" # binary outcome not rare
sub = "Do you believe in a God?"

# fit regression model
out_m <- mice_generalised(df = df,
                          X = X,
                          Y = Y,
                          family = family,
                          cvars = cvars)
# g-computation - contrasts
out_ct <-
  pool_stglm_contrast_ratio(
    out_m,
    df = df,
    m = 10,
    X = X,
    x = c,
    r = r
  )
#table

# coef + estimate
god_c <- vanderweelevalue_rr(out_ct, f)
god_c

god_t <- out_ct %>%
  slice(1:max) |>
  tibble() |>
  rename(
    Contrast = row,
    Estimate = est,
    std_error = se,
    CI_hi = ui,
    CI_lo = li
  ) |>
  kbl(caption = main,
      digits = 3,
      "html") |>
  kable_styling() %>%
  row_spec(c(f + 1 - min),
           bold = T,
           color = "white",
           background = "dodgerblue") |>
  kable_minimal(full_width = F)
god_t
god_p <-
  ggplot_stglm(
    out_ct,
    ylim = ylim_contrast,
    main,
    xlab,
    ylab,
    min = min,
    p = p,
    sub = sub
  ) +  expand_limits(x = 0, y = 0)
god_p


# believe spirit -------------------------------------------------------------
# Do you believe in some form of spirit or lifeforce?
Y = "Believe.Spirit_lead2"
main = "Belief in Spirit"
ylab = "Spirit Belief (RR)"
family = "poisson"
sub = "Do you believe in some form of spirit or lifeforce?"
# fit regression model
out_m <- mice_generalised(df = df,
                          X = X,
                          Y = Y,
                          family = family,
                          cvars = cvars)
# g-computation - contrasts
out_ct <-
  pool_stglm_contrast_ratio(
    out_m,
    df = df,
    m = 10,
    X = X,
    x = c,
    r = r
  )
#table

# coef + estimate
spirit_c <- vanderweelevalue_rr(out_ct, f)
spirit_c

spirit_t <- out_ct %>%
  slice(1:max) |>
  tibble() |>
  rename(
    Contrast = row,
    Estimate = est,
    std_error = se,
    CI_hi = ui,
    CI_lo = li
  ) |>
  kbl(caption = main,
      digits = 3,
      "html") |>
  kable_styling() %>%
  row_spec(c(f + 1 - min),
           bold = T,
           color = "white",
           background = "dodgerblue") |>
  kable_minimal(full_width = F)
spirit_t
spirit_p <-
  ggplot_stglm(
    out_ct,
    ylim = ylim_contrast,
    main,
    xlab,
    ylab,
    min = min,
    p = p,
    sub = sub
  ) +  expand_limits(x = 0, y = 0)
spirit_p



# negative control  spirit -------------------------------------------------------------
# Do you believe in some form of spirit or lifeforce?
Y = "Hours.Exercise_log_lead2_z"
main = "Log Hours Excercise (SD)"
ylab = "Log Hours Excercise (SD)"
sub = "Hours spent … exercising/physical activity"
# fit regression model
out_m <- mice_gaussian(df = df,
                          X = X,
                          Y = Y,
                          cvars = cvars)
# g-computation - contrasts
out_ct <-
  pool_stglm_contrast(
    out_m,
    df = df,
    m = 10,
    X = X,
    x = c,
    r = r + 1  # church zero ill defined
  )
#table
# coef + estimate
excercise_c <- vanderweelevalue_ols(out_ct, f, delta = delta)
excercise_c

excercise_t <- out_ct %>%
  slice(1:max) |>
  tibble() |>
  rename(
    Contrast = row,
    Estimate = est,
    std_error = se,
    CI_hi = ui,
    CI_lo = li
  ) |>
  kbl(caption = main,
      digits = 3,
      "html") |>
  kable_styling() %>%
  row_spec(c(f + 1 - min),
           bold = T,
           color = "white",
           background = "dodgerblue") |>
  kable_minimal(full_width = F)
excercise_t
excercise_p <-
  ggplot_stglm(
    out_ct,
    ylim = ylim,
    main,
    xlab,
    ylab,
    min = min,
    p = p,
    sub = sub
  ) #+  expand_limits(x = 0, y = 0)
excercise_p




# PIETY TAB ---------------------------------------------------------------

main = "Religious Piety estimands / Evalues"
piety_tab <- rbind(scripture_c,
                   prayer_c,
                   church_c,
                   god_c,
                   spirit_c,
                     excercise_c)
piety_tab |>
  kbl(caption = main,
      digits = 3,
      "html") |>
  kable_styling() %>%
  row_spec(c(1:3),
           bold = T,
           color = "black",
           background = "bold") |>
  kable_minimal(full_width = F)


exp(1)
log(2.71)

# piety graph ------------------------------------------------

piety_fig <-
  scripture_p +
  prayer_p +
  church_p +
  god_p +
  spirit_p + excercise_p +
  plot_annotation(title = "Causal effects of religious community size on religious piety") +
  plot_layout(guides = 'collect') #+ plot_layout(nrow = 3, byrow = T)

# view
piety_fig

log(8)

ggsave(
  piety_fig,
  path = here::here(here::here("figs" , "congregation", "piety_fig")),
  width = 15,
  height = 12,
  units = "in",
  filename = "piety_fig.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 1200
)
