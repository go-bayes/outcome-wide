

# labels <- c("no_pets",
#             "dogs_only",
#             "cats_only",
#             "cats_dogs",
#             "other_pets_only")
#
#


# import libraries
source("https://raw.githubusercontent.com/go-bayes/templates/main/functions/libs2.R")

# read functions
source("https://raw.githubusercontent.com/go-bayes/templates/main/functions/funs.R")



options(scipen = 999)

# You can find more information about the NZAVS:[HERE](https://www.psych.auckland.ac.nz/en/about/new-zealand-attitudes-and-values-study.html)

# step one: import libraries:
source("https://raw.githubusercontent.com/go-bayes/templates/main/functions/libs2.R")


# read functions
source("https://raw.githubusercontent.com/go-bayes/templates/main/functions/funs.R")

# read data

# set your folder path : unique to each user

# for saving models

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


# From jake
# From some initial testing, the following:
#   dplyr::mutate(org2018 = ifelse(Wave == 2018 & YearMeasured == 1, 1, 0)) |>
#   dplyr::mutate(hold18 = mean(org2018, na.rm = TRUE)) |>
#   dplyr::filter(hold18 > 0)  # hack to enable repeat of baseline
#
# Can be simplified to:
#   dplyr::filter(any(Wave == 2018 & YearMeasured == 1))


# initial wrangling to get cohort: all those who participated in both t10 and t11
#
# dat |>
#   filter(Wave == 2015 & YearMeasured==1 | Wave == 2019 & YearMeasured==1) |>
#   select(KESSLER6, Pets,Pets.coded.catdog_factor, Id, Wave) |>
#   drop_na() |>
#   ggplot(aes(x= Pets.coded.catdog_factor, y=KESSLER6, colour = Pets.coded.catdog_factor)) +
#   facet_grid(.~Wave) +
#   geom_boxplot(notch = TRUE) +
#   geom_jitter(shape=16, position=position_jitter(0.2), alpha = .1) +
#   labs(
#     title = "Do you have any pets?",
#     y = "KESSLER6",
#     x = "'Do you have any pets? (coded)'") + scale_color_viridis_d(option = "D")
#
# test <- dat |>
#   filter(any( Wave == 2015 & YearMeasured==1)) |>
#   filter(any( Wave == 2019 & YearMeasured==1) )|>
#   select(KESSLER6, Pets,Pets.coded.catdog_factor, Id, Wave) |>
#   drop_na()
#
# n_unique(test$Id)
#
#
# test <- dat |>
#   filter(any( Wave == 2015 & YearMeasured==1) |  ( Wave == 2019 & YearMeasured==1) ) |>
#   select(KESSLER6, Pets,Pets.coded.catdog_factor, Id, Wave) |>
#   drop_na()
#
# n_unique(test$Id)
#
# #|>
#   ggplot(aes(x= Pets.coded.catdog_factor, y=KESSLER6, colour = Pets.coded.catdog_factor)) +
#   facet_grid(.~Wave) +
#   geom_boxplot(notch = TRUE) +
#   geom_jitter(shape=16, position=position_jitter(0.2), alpha = .1) +
#   labs(
#     title = "Do you have any pets?",
#     y = "KESSLER6",
#     x = "'Do you have any pets? (coded)'") + scale_color_viridis_d(option = "D")
#
#
# # create a table summary
# library(gtsummary)
#
#
# # not working
# df <- dat |>
#   filter(Wave == 2015 & YearMeasured==1 | Wave == 2019 & YearMeasured==1) |>
#   select(KESSLER6, Pets.coded.catdog_factor, Wave)
#
# # Summarize means of Kessler6 by Pets_coded and wave
# tbl_summary(df,
#             by = Pets.coded.catdog_factor,
#             missing = "no",
#             statistic = list(all_continuous() ~ "{mean} ({sd})")) %>%
#   add_p(by = Wave, p = "none")
#


dat_new <- dat |>
  group_by(Id) %>%
  dplyr::mutate(org2015 =  ifelse(Wave == 2015 &
                                    YearMeasured == 1, 1, 0)) %>%  # creating an indicator for the first wave
  dplyr::mutate(hold15 = mean(org2015, na.rm = TRUE)) %>%  # Hack
  dplyr::filter(hold15 > 0) %>% # hack to enable repeat of baseline
  dplyr::mutate(org2019 = ifelse(Wave == 2019 &
                                   YearMeasured == 1, 1, 0)) %>%  # creating an indicator for the first wave
  dplyr::mutate(hold19 = mean(org2019, na.rm = TRUE)) %>%  # Hack0
  dplyr::filter(hold19 > 0) %>% # hack to enable repeat of baseline in 201
  ungroup() %>%
  mutate(
    Pets = factor(Pets),
    pets = as.numeric(Pets),
  ) |>
  dplyr::filter((Wave == 2015  & YearMeasured  == 1) |
                  (Wave == 2019  &
                     YearMeasured  == 1) |
                  (Wave == 2020))  %>%
  group_by(Id) |>
  arrange(Id, Wave) |>
  dplyr::mutate(org2015 =  ifelse(Wave == 2015 &
                                    YearMeasured == 1, 1, 0)) %>%  # creating an indicator for the first wave
  dplyr::mutate(hold15 = mean(org2015, na.rm = TRUE)) %>%  # Hack
  dplyr::filter(hold15 > 0) %>% # hack to enable repeat of baseline
  dplyr::mutate(org2019 = ifelse(Wave == 2019 &
                                   YearMeasured == 1, 1, 0)) %>%  # creating an indicator for the first wave
  dplyr::mutate(hold19 = mean(org2019, na.rm = TRUE)) %>%  # Hack0
  dplyr::filter(hold19 > 0) %>% # hack to enable repeat of baseline in 201
  ungroup() %>%
  arrange(Id, Wave) |>
  droplevels() |>
  mutate(time = as.numeric(Wave)-1) |>
  arrange(Id, time) |>
  droplevels()


# # example


# check no missing in the exposure -- looks good.
# missing in outcomes means censoring case by case if we use censoring approaches.
table1::table1(~ catdog_f * KESSLER6 + PWI + BELONG + LIFESAT + SELF.ESTEEM|
                 time, data = dat_new, overall = FALSE)

dat_new$SELF.ESTEEM


# check change
## Look at change
ss_pets <- dat_new |>
  select(
    Id,
    catdog,
  )

# Set the starting date as June 30, 2009
start_date <- as.Date("2009-06-30")

# Add 3545 days to the starting date
end_date <- start_date + 3545

# Print the end date in yyyy-mm-dd format
format(end_date, "%Y-%m-%d")



table_msm <- print ( msm::statetable.msm(round(catdog, 0), Id, data = ss_pets)  )

my_table <- table_msm |> as_tibble()

my_table |>
  kbl(hover = T, fullwidth = T)


factor_levels <- 0:4
factor_labels <- c("no_pets",
                   "dogs_only",
                   "cats_only",
                   "cats_dogs",
                   "other_pets_only")

my_table$from <- factor(my_table$from, levels = factor_levels, labels = factor_labels)
my_table$to <- factor(my_table$to, levels = factor_levels, labels = factor_labels)

print ( my_table, n = Inf )

# decision to use cat_2 based on evolutionary theory that 21 hours = normal for people.

# variables for baseline
table(is.na(dat_new$NZSEI13))
table(is.na(dat_new$NZSEI18))
table(is.na(dat_new$Rural_GCH2018))
table(is.na(dat_new$NZDep2018)) # USE THIS NOT A LOT OF NA


# inclusion criteria
# under 59 at baseline - help to min heterogeneity of treatment
#

# Personal Wellbeing Index
# Your health.
# Your standard of living.
# Your future security.
# Your personal relationships.

# Do you have a health condition or disability that limits you, and that has lasted for 6+ months?
dat_new$time


## select vars
dt_prep <- dat_new %>%
  select(
    Id,
    # nCensoring,
    YearMeasured,
    time,
    Wave,
    Partner,
    EthCat,
    Age,
    Gender3,
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
    ChildrenNum,
    Hours.Work,
    Hours.Pets,
    edu_n,
    Employed,
    BornNZ,
    #   HomeOwner,
    Pol.Orient,
    income_log,
    Parent,
    Relid,
    ChildrenNum,
    # exposure
    catdog,
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
    # Your future security.
    pwi_security,
    # Your personal relationships.
    pwi_relationships,
    # Your health.
    pwi_health,
    # Your standard of living.
    pwi_standardliving,
    # My life has a clear sense of purpose.
    # meaning_purpose,    # missing at baseline
    # # I have a good sense of what makes my life meaningful.
    # meaning_sense,
    # #Know that people in my life accept and value me.
    belong_accept,
    #Feel like an outsider.
    belong_routsider,
    # Know that people around me share my attitudes and beliefs.
    belong_beliefs,
    # There are people I can depend on to help me if I really need it.
    support_help,
    #There is no one I can turn to for guidance in times of stress.
    support_rnoguidance,
    # I know there are people I can turn to when I need help.
    support_turnto,
    # On the whole am satisfied with myself.
    selfesteem_satself,
    #Take a positive attitude toward myself.
    selfesteem_postiveself,
    # Am inclined to feel that I am a failure.
    selfesteem_rfailure,
    #I am satisfied with my life.
    lifesat_satlife,
    # In most ways my life is close to ideal.
    lifesat_ideal ) %>%
  #  dplyr::rename(Community = SWB.SoC01) %>%
  dplyr::mutate(across(!c(Id, Wave), ~ as.numeric(.x))) %>% # make factors numeric for easy of
  arrange(Id, Wave) %>%
  # dplyr::mutate(#  Volunteers = if_else(HoursCharity == 1, 1, 0),
  #   Church = ifelse(Religion.Church > 8, 8, Religion.Church),) %>%
  arrange(Id, Wave) %>%
#  dplyr::mutate(
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
  arrange(Id, time) %>%
  data.frame() %>%
  mutate(across(where(is.double), as.numeric)) %>%
#  select(-c(Wave)) |>
  arrange(Id)

table(dt_prep$time)

dt_prep2   <- dt_prep |>
  mutate(owns_nopet = ifelse(catdog == 0, 1, 0 )) |>
  mutate(owns_dogonly = ifelse(catdog == 1, 1, 0 )) |>
  mutate(owns_catonly = ifelse(catdog == 2, 1, 0 )) |>
  mutate(owns_catanddog  = ifelse(catdog == 3, 1, 0 )) |>
  mutate(owns_otherpet  = ifelse(catdog == 4, 1, 0 ))
#
# ## GAINS DOG BUT NOT ANY OTHER PET
# dt_gaindog<- dt_prep2|>
#   filter((owns_nopet == 1 & time ==0 )  &
#            (owns_catonly == 0 & time == 1) &  (owns_catanddog == 0 & time == 1) &  (owns_otherpet == 0 & time == 1) | time == 2) |>
#   group_by(Id) |>
#   dplyr::mutate(org2015 =  ifelse(Wave == 2015 &
#                                     YearMeasured == 1, 1, 0)) %>%  # creating an indicator for the first wave
#   dplyr::mutate(hold15 = mean(org2015, na.rm = TRUE)) %>%  # Hack
#   dplyr::filter(hold15 > 0) %>% # hack to enable repeat of baseline
#   dplyr::mutate(org2019 = ifelse(Wave == 2019 &
#                                    YearMeasured == 1, 1, 0)) %>%  # creating an indicator for the first wave
#   dplyr::mutate(hold19 = mean(org2019, na.rm = TRUE)) %>%  # Hack0
#   dplyr::filter(hold19 > 0) %>% # hack to enable repeat of baseline in 201
#   ungroup |>
#   select(-c(Wave, YearMeasured)) |>
#   arrange(Id, time)

#
# # looks good
# dt_gaindog
# table1::table1(data = dt_gaindog,
#                ~ owns_nopet + owns_dogonly + owns_catonly + owns_otherpet  |factor( time))
#


## GAINS CAT BUT NOT ANY OTHER PET
# dt_gaincat<- dt_prep2|>
#   filter((owns_nopet == 1 & time ==0 ) | (owns_dogonly != 1 & time == 1) |  (owns_catanddog != 1 & time == 1) |  (owns_otherpet != 1 & time == 1) | time == 2) |>
#   group_by(Id) |>
#   dplyr::mutate(org2015 =  ifelse(Wave == 2015 &
#                                     YearMeasured == 1, 1, 0)) %>%  # creating an indicator for the first wave
#   dplyr::mutate(hold15 = mean(org2015, na.rm = TRUE)) %>%  # Hack
#   dplyr::filter(hold15 > 0) %>% # hack to enable repeat of baseline
#   dplyr::mutate(org2019 = ifelse(Wave == 2019 &
#                                    YearMeasured == 1, 1, 0)) %>%  # creating an indicator for the first wave
#   dplyr::mutate(hold19 = mean(org2019, na.rm = TRUE)) %>%  # Hack0
#   dplyr::filter(hold19 > 0) %>% # hack to enable repeat of baseline in 201
#   ungroup |>
#   select(-c(Wave, YearMeasured)) |>
#   arrange(Id, time)


## Lost dog but does not gain other pet
dt_lostdog<- dt_prep2|>
  filter((owns_dogonly == 1 & time ==0 ) | (owns_catanddog == 0 & time == 1)  & (owns_otherpet == 0 & time == 1) | time == 2) |>
  group_by(Id) |>
  dplyr::mutate(org2015 =  ifelse(Wave == 2015 &
                                    YearMeasured == 1, 1, 0)) %>%  # creating an indicator for the first wave
  dplyr::mutate(hold15 = mean(org2015, na.rm = TRUE)) %>%  # Hack
  dplyr::filter(hold15 > 0) %>% # hack to enable repeat of baseline
  dplyr::mutate(org2019 = ifelse(Wave == 2019 &
                                   YearMeasured == 1, 1, 0)) %>%  # creating an indicator for the first wave
  dplyr::mutate(hold19 = mean(org2019, na.rm = TRUE)) %>%  # Hack0
  dplyr::filter(hold19 > 0) %>% # hack to enable repeat of baseline in 201
  ungroup |>
  select(-c(Wave, YearMeasured)) |>
  arrange(Id, time)



# looks good
dt_gaindog
table1::table1(data = dt_gaindog,
               ~ owns_nopet + owns_dogonly + owns_catonly + owns_otherpet +
                catdog |factor( time))


dt_gaincat
table1::table1(data = dt_gaincat,
               ~ owns_nopet + owns_dogonly + owns_catonly + owns_otherpet +
                 catdog |factor( time))

#lost dog
table1::table1(data = dt_lostdog,
               ~ owns_nopet + owns_dogonly + owns_catonly + owns_otherpet +
                 catdog |factor( time))






# make wide ---------------------------------------------------------------



table(dt_prep2$time)

# check data
# check
naniar::vis_miss(dt_prep, warn_large_data = FALSE)


# pivot wide
dt_wide <- dt_prep2 |>
  pivot_wider(
    id_cols = Id,
    names_from = time,
    values_from = -c(Id, time),
    names_glue = "l{time}_{.value}",
    names_prefix = "l"
  )

colnames(dt_wide)

dt_gaindog <- dt_wide |>
  filter(l0_owns_nopet == 1) |>
  filter(l1_owns_catanddog ==0) |>
  filter(l1_owns_catonly == 0) |>
  filter(l1_owns_otherpet == 0)


dt_gaincat <- dt_wide |>
  filter(l0_owns_nopet == 1) |>
  filter(l1_owns_catanddog ==0) |>
  filter(l1_owns_dogonly == 0) |>
  filter(l1_owns_otherpet == 0)


dt_lostdog <- dt_wide |>
  filter(l0_owns_dogonly == 1) |>
  filter(l0_owns_catanddog == 0) |>
  filter(l0_owns_otherpet == 0) |>
  filter(l1_owns_catanddog ==0) |>
  filter(l1_owns_catonly == 0) |>
  filter(l1_owns_otherpet == 0)

dt_lostcat <- dt_wide |>
  filter(l0_owns_catonly == 1) |>
  filter(l0_owns_catanddog == 0) |>
  filter(l0_owns_otherpet == 0) |>
  filter(l1_owns_catanddog ==0) |>
  filter(l1_owns_dogonly == 0) |>
  filter(l1_owns_otherpet == 0)

# checks
table1(data = dt_lostdog, ~
         l0_owns_dogonly +
         l0_owns_catonly +
         l0_owns_catanddog +
         l0_owns_otherpet +
         l1_owns_dogonly +
         l1_owns_catonly +
         l1_owns_catanddog +
         l1_owns_otherpet)

table1(data = dt_gaindog, ~
         l0_owns_dogonly +
         l0_owns_catonly +
         l0_owns_catanddog +
         l0_owns_otherpet +
         l1_owns_dogonly +
         l1_owns_catonly +
         l1_owns_catanddog +
         l1_owns_otherpet)

table1(data = dt_gaincat, ~
         l0_owns_dogonly +
         l0_owns_catonly +
         l0_owns_catanddog +
         l0_owns_otherpet +
         l1_owns_dogonly +
         l1_owns_catonly +
         l1_owns_catanddog +
         l1_owns_otherpet)


table1(data = dt_lostcat, ~
         l0_owns_dogonly +
         l0_owns_catonly +
         l0_owns_catanddog +
         l0_owns_otherpet +
         l1_owns_dogonly +
         l1_owns_catonly +
         l1_owns_catanddog +
         l1_owns_otherpet)


## QUICK test
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
# Your future security.
pwi_security,
# Your personal relationships.
pwi_relationships,
# Your health.
pwi_health,
# Your standard of living.
pwi_standardliving,
# My life has a clear sense of purpose.
# meaning_purpose,    # missing at baseline
# # I have a good sense of what makes my life meaningful.
# meaning_sense,
# #Know that people in my life accept and value me.
belong_accept,
#Feel like an outsider.
belong_routsider,
# Know that people around me share my attitudes and beliefs.
belong_beliefs,
# There are people I can depend on to help me if I really need it.
support_help,
#There is no one I can turn to for guidance in times of stress.
support_rnoguidance,
# I know there are people I can turn to when I need help.
support_turnto,
# On the whole am satisfied with myself.
selfesteem_satself,
#Take a positive attitude toward myself.
selfesteem_postiveself,
# Am inclined to feel that I am a failure.
selfesteem_rfailure,
#I am satisfied with my life.
lifesat_satlife,
# In most ways my life is close to ideal.
lifesat_ideal



# lost cat ----------------------------------------------------------------


# lost cat

summary(k1_lc <- lm (l2_kessler_hopeless ~ l1_owns_catonly + l0_kessler_hopeless,   data = dt_lostcat))
summary(k2_lc <-lm (l2_kessler_worthless ~ l1_owns_catonly + l0_kessler_worthless,   data = dt_lostcat))
summary(k3_lc <-lm (l2_kessler_restless ~ l1_owns_catonly + l0_kessler_restless,   data = dt_lostcat))
summary(k4_lc <-lm (l2_kessler_effort ~ l1_owns_catonly + l0_kessler_effort,   data = dt_lostcat))
summary(k5_lc <-lm (l2_kessler_nervous ~ l1_owns_catonly + l0_kessler_nervous,   data = dt_lostcat))
summary(k6_lc <-lm (l2_kessler_depressed ~ l1_owns_catonly + l0_kessler_depressed,   data = dt_lostcat))

library(gtsummary)
library(broom)

# Convert each model to a tidy data frame
k1_tidy <- tidy(k1_lc)
k2_tidy <- tidy(k2_lc)
k3_tidy <- tidy(k3_lc)
k4_tidy <- tidy(k4_lc)
k5_tidy <- tidy(k5_lc)
k6_tidy <- tidy(k6_lc)

# Combine the tidy data frames into a single data frame
tidy_models <- bind_rows(k1_tidy, k2_tidy, k3_tidy, k4_tidy, k5_tidy, k6_tidy)

# Check if any of the models are NULL
sapply(model_list, is.null)

# Print a regression table for all six models
tbl_regression(tidy_models)
tab_model( k1_lc, k2_lc, k3_lc, k4_lc, k5_lc, k6_lc)


model_list <- list( k1_lc, k2_lc, k3_lc, k4_lc, k5_lc, k6_lc)
gtsummary::tbl_summary(model_list)

library(sjPlot)
library(sjmisc)

  ?model_summary
# belong_accept,
# #Feel like an outsider.
# belong_routsider,
# # Know that people around me share my attitudes and beliefs.
# belong_beliefs,

# Belong
summary(lm(l2_belong_accept  ~ l1_owns_catonly + l0_belong_accept, data = dt_lostcat))
summary(lm(l2_belong_routsider  ~ l1_owns_catonly + l0_belong_routsider, data = dt_lostcat))
summary(lm (l2_belong_beliefs ~ l1_owns_catonly + l0_belong_beliefs,   data = dt_lostcat))



# pwi_security,
# # Your personal relationships.
# pwi_relationships,
# # Your health.
# pwi_health,
# # Your standard of living.
# pwi_standardliving,
# # My life has a clear sense of purpose.
#pwi
summary(lm (l2_pwi_security ~ l1_owns_catonly + l0_pwi_security,   data = dt_lostcat))
summary(lm (l2_pwi_relationships ~ l1_owns_catonly + l0_pwi_relationships,   data = dt_lostcat))
summary(lm (l2_pwi_health ~ l1_owns_catonly + l0_pwi_health,   data = dt_lostcat))
summary(lm (l2_pwi_standardliving ~ l1_owns_catonly + l0_pwi_standardliving,   data = dt_lostcat))

# support
# support_help,
# #There is no one I can turn to for guidance in times of stress.
# support_rnoguidance,
# # I know there are people I can turn to when I need help.
# support_turnto,
# # On the whole am satisfied with myself.

summary(lm (l2_support_help ~ l1_owns_catonly + l0_support_help,   data = dt_lostcat))
summary(lm (l2_support_rnoguidance ~ l1_owns_catonly + l0_support_rnoguidance,   data = dt_lostcat))
summary(lm (l2_support_turnto ~ l1_owns_catonly + l0_support_turnto,   data = dt_lostcat))

# selfesteem_satself,
# #Take a positive attitude toward myself.
# selfesteem_postiveself,
# # Am inclined to feel that I am a failure.
# selfesteem_rfailure,

summary(lm (l2_selfesteem_satself ~ l1_owns_catonly + l0_selfesteem_satself,   data = dt_lostcat))
summary(lm (l2_selfesteem_postiveself ~ l1_owns_catonly + l0_selfesteem_postiveself,   data = dt_lostcat))
summary(lm (l2_selfesteem_rfailure ~ l1_owns_catonly + l0_selfesteem_rfailure,   data = dt_lostcat))

# #I am satisfied with my life.
# lifesat_satlife,
# # In most ways my life is close to ideal.
# lifesat_ideal
summary(lm (l2_lifesat_satlife ~ l1_owns_catonly + l0_lifesat_satlife,   data = dt_lostcat))
summary(lm (l2_lifesat_ideal ~ l1_owns_catonly + l0_lifesat_ideal,   data = dt_lostcat))



# lost dog ----------------------------------------------------------------


# lost cat

dt_lostdog$l1_owns_dogonly

summary(lm (l2_kessler_hopeless ~ l1_owns_dogonly + l0_kessler_hopeless,   data = dt_lostdog))

summary(lm (l2_kessler_worthless ~ l1_owns_dogonly + l0_kessler_worthless,   data = dt_lostdog))
summary(lm (l2_kessler_restless ~ l1_owns_dogonly + l0_kessler_restless,   data = dt_lostdog))
summary(lm (l2_kessler_effort ~ l1_owns_dogonly + l0_kessler_effort,   data = dt_lostdog))
summary(lm (l2_kessler_nervous ~ l1_owns_dogonly + l0_kessler_nervous,   data = dt_lostdog))
summary(lm (l2_kessler_depressed ~ l1_owns_dogonly + l0_kessler_depressed,   data = dt_lostdog))




# pwi_security,
# # Your personal relationships.
# pwi_relationships,
# # Your health.
# pwi_health,
# # Your standard of living.
# pwi_standardliving,
# # My life has a clear sense of purpose.


# belong_accept,
# #Feel like an outsider.
# belong_routsider,
# # Know that people around me share my attitudes and beliefs.
# belong_beliefs,

# Belong
summary(lm(l2_belong_accept  ~ l1_owns_dogonly + l0_belong_accept, data = dt_lostdog))
summary(lm(l2_belong_routsider  ~ l1_owns_dogonly + l0_belong_routsider, data = dt_lostdog))
summary(lm (l2_belong_beliefs ~ l1_owns_dogonly + l0_belong_beliefs,   data = dt_lostdog))

#pwi
summary(lm (l2_pwi_security ~ l1_owns_dogonly + l0_pwi_security,   data = dt_lostdog))
summary(lm (l2_pwi_relationships ~ l1_owns_dogonly + l0_pwi_relationships,   data = dt_lostdog))
summary(lm (l2_pwi_health ~ l1_owns_dogonly + l0_pwi_health,   data = dt_lostdog))
summary(lm (l2_pwi_standardliving ~ l1_owns_dogonly + l0_pwi_standardliving,   data = dt_lostdog))

# support
# support_help,
# #There is no one I can turn to for guidance in times of stress.
# support_rnoguidance,
# # I know there are people I can turn to when I need help.
# support_turnto,
# # On the whole am satisfied with myself.

summary(lm (l2_support_help ~ l1_owns_dogonly + l0_support_help,   data = dt_lostdog))
summary(lm (l2_support_rnoguidance ~ l1_owns_dogonly + l0_support_rnoguidance,   data = dt_lostdog))
summary(lm (l2_support_turnto ~ l1_owns_dogonly + l0_support_turnto,   data = dt_lostdog))

# selfesteem_satself,
# #Take a positive attitude toward myself.
# selfesteem_postiveself,
# # Am inclined to feel that I am a failure.
# selfesteem_rfailure,

summary(lm (l2_selfesteem_satself ~ l1_owns_dogonly + l0_selfesteem_satself,   data = dt_lostdog))
summary(lm (l2_selfesteem_postiveself ~ l1_owns_dogonly + l0_selfesteem_postiveself,   data = dt_lostdog))
summary(lm (l2_selfesteem_rfailure ~ l1_owns_dogonly + l0_selfesteem_rfailure,   data = dt_lostdog))

# #I am satisfied with my life.
# lifesat_satlife,
# # In most ways my life is close to ideal.
# lifesat_ideal
summary(lm (l2_lifesat_satlife ~ l1_owns_dogonly + l0_lifesat_satlife,   data = dt_lostdog))
summary(lm (l2_lifesat_ideal ~ l1_owns_dogonly + l0_lifesat_ideal,   data = dt_lostdog))



# gain cat ----------------------------------------------------------------



summary(lm (l2_kessler_hopeless ~ l1_owns_catonly + l0_kessler_hopeless,   data = dt_gaincat))
summary(lm (l2_kessler_worthless ~ l1_owns_catonly + l0_kessler_worthless,   data = dt_gaincat))
summary(lm (l2_kessler_restless ~ l1_owns_catonly + l0_kessler_restless,   data = dt_gaincat))
summary(lm (l2_kessler_effort ~ l1_owns_catonly + l0_kessler_effort,   data = dt_gaincat))

# gain cat
summary(lm (l2_kessler_nervous ~ l1_owns_catonly + l0_kessler_nervous,   data = dt_gaincat))

summary(lm (l2_kessler_depressed ~ l1_owns_catonly + l0_kessler_depressed,   data = dt_gaincat))



# belong_accept,
# #Feel like an outsider.
# belong_routsider,
# # Know that people around me share my attitudes and beliefs.
# belong_beliefs,

# Belong
summary(lm(l2_belong_accept  ~ l1_owns_catonly + l0_belong_accept, data = dt_gaincat))
# lower
summary(lm(l2_belong_routsider  ~ l1_owns_catonly + l0_belong_routsider, data = dt_gaincat))
summary(lm (l2_belong_beliefs ~ l1_owns_catonly + l0_belong_beliefs,   data = dt_gaincat))



# pwi_security,
# # Your personal relationships.
# pwi_relationships,
# # Your health.
# pwi_health,
# # Your standard of living.
# pwi_standardliving,
# # My life has a clear sense of purpose.
#pwi
summary(lm (l2_pwi_security ~ l1_owns_catonly + l0_pwi_security,   data = dt_gaincat))
summary(lm (l2_pwi_relationships ~ l1_owns_catonly + l0_pwi_relationships,   data = dt_gaincat))
summary(lm (l2_pwi_health ~ l1_owns_catonly + l0_pwi_health,   data = dt_gaincat))
summary(lm (l2_pwi_standardliving ~ l1_owns_catonly + l0_pwi_standardliving,   data = dt_gaincat))

# support
# support_help,
# #There is no one I can turn to for guidance in times of stress.
# support_rnoguidance,
# # I know there are people I can turn to when I need help.
# support_turnto,
# # On the whole am satisfied with myself.

summary(lm (l2_support_help ~ l1_owns_catonly + l0_support_help,   data = dt_gaincat))
summary(lm (l2_support_rnoguidance ~ l1_owns_catonly + l0_support_rnoguidance,   data = dt_gaincat))
summary(lm (l2_support_turnto ~ l1_owns_catonly + l0_support_turnto,   data = dt_gaincat))

# selfesteem_satself,
# #Take a positive attitude toward myself.
# selfesteem_postiveself,
# # Am inclined to feel that I am a failure.
# selfesteem_rfailure,

summary(lm (l2_selfesteem_satself ~ l1_owns_catonly + l0_selfesteem_satself,   data = dt_gaincat))
# lower!
summary(lm (l2_selfesteem_postiveself ~ l1_owns_catonly + l0_selfesteem_postiveself,   data = dt_gaincat))
summary(lm (l2_selfesteem_rfailure ~ l1_owns_catonly + l0_selfesteem_rfailure,   data = dt_gaincat))

# #I am satisfied with my life.
# lifesat_satlife,
# # In most ways my life is close to ideal.
# lifesat_ideal
summary(lm (l2_lifesat_satlife ~ l1_owns_catonly + l0_lifesat_satlife,   data = dt_gaincat))
summary(lm (l2_lifesat_ideal ~ l1_owns_catonly + l0_lifesat_ideal,   data = dt_gaincat))



# doggain -----------------------------------------------------------------

dt_gaindog
# lost cat

dt_lostdog$l1_owns_dogonly

summary(lm (l2_kessler_hopeless ~ l1_owns_dogonly + l0_kessler_hopeless,   data = dt_gaindog))

summary(lm (l2_kessler_worthless ~ l1_owns_dogonly + l0_kessler_worthless,   data = dt_gaindog))
summary(lm (l2_kessler_restless ~ l1_owns_dogonly + l0_kessler_restless,   data = dt_gaindog))
summary(lm (l2_kessler_effort ~ l1_owns_dogonly + l0_kessler_effort,   data = dt_gaindog))
summary(lm (l2_kessler_nervous ~ l1_owns_dogonly + l0_kessler_nervous,   data = dt_gaindog))
summary(lm (l2_kessler_depressed ~ l1_owns_dogonly + l0_kessler_depressed,   data = dt_gaindog))




# pwi_security,
# # Your personal relationships.
# pwi_relationships,
# # Your health.
# pwi_health,
# # Your standard of living.
# pwi_standardliving,
# # My life has a clear sense of purpose.


# belong_accept,
# #Feel like an outsider.
# belong_routsider,
# # Know that people around me share my attitudes and beliefs.
# belong_beliefs,

# Belong
summary(lm(l2_belong_accept  ~ l1_owns_dogonly + l0_belong_accept, data = dt_gaindog))
summary(lm(l2_belong_routsider  ~ l1_owns_dogonly + l0_belong_routsider, data = dt_gaindog))
summary(lm (l2_belong_beliefs ~ l1_owns_dogonly + l0_belong_beliefs,   data = dt_gaindog))

#pwi
summary(lm (l2_pwi_security ~ l1_owns_dogonly + l0_pwi_security,   data = dt_gaindog))
summary(lm (l2_pwi_relationships ~ l1_owns_dogonly + l0_pwi_relationships,   data = dt_gaindog))
summary(lm (l2_pwi_health ~ l1_owns_dogonly + l0_pwi_health,   data = dt_gaindog))
summary(lm (l2_pwi_standardliving ~ l1_owns_dogonly + l0_pwi_standardliving,   data = dt_gaindog))

# support
# support_help,
# #There is no one I can turn to for guidance in times of stress.
# support_rnoguidance,
# # I know there are people I can turn to when I need help.
# support_turnto,
# # On the whole am satisfied with myself.

summary(lm (l2_support_help ~ l1_owns_dogonly + l0_support_help,   data = dt_gaindog))
summary(lm (l2_support_rnoguidance ~ l1_owns_dogonly + l0_support_rnoguidance,   data = dt_gaindog))
summary(lm (l2_support_turnto ~ l1_owns_dogonly + l0_support_turnto,   data = dt_gaindog))

# selfesteem_satself,
# #Take a positive attitude toward myself.
# selfesteem_postiveself,
# # Am inclined to feel that I am a failure.
# selfesteem_rfailure,

summary(lm (l2_selfesteem_satself ~ l1_owns_dogonly + l0_selfesteem_satself,   data = dt_gaindog))
summary(lm (l2_selfesteem_postiveself ~ l1_owns_dogonly + l0_selfesteem_postiveself,   data = dt_gaindog))
summary(lm (l2_selfesteem_rfailure ~ l1_owns_dogonly + l0_selfesteem_rfailure,   data = dt_gaindog))

# #I am satisfied with my life.
# lifesat_satlife,
# # In most ways my life is close to ideal.
# lifesat_ideal
summary(lm (l2_lifesat_satlife ~ l1_owns_dogonly + l0_lifesat_satlife,   data = dt_gaindog))
summary(lm (l2_lifesat_ideal ~ l1_owns_dogonly + l0_lifesat_ideal,   data = dt_gaindog))









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








