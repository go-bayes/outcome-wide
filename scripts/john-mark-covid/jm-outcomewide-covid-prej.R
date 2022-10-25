## lockdown on prejudice

options(scipen = 999)

#libraries and functions
# read libraries
source("https://raw.githubusercontent.com/go-bayes/templates/main/functions/libs.R")

# read functions
source("https://raw.githubusercontent.com/go-bayes/templates/main/functions/funs.R")

conflict_prefer("pool", "mice")
conflict_prefer("cbind", "base")
# for saving models
push_mods <-
  fs::path_expand("~/The\ Virtues\ Project\ Dropbox/outcomewide/mods")
push_figs <-
  fs::path_expand("~/Users/joseph/The\ Virtues\ Project\ Dropbox/outcomewide/figs")

# read data
pull_path <-
  fs::path_expand(
    "~/The\ Virtues\ Project\ Dropbox/Joseph\ Bulbulia/00Bulbulia\ Pubs/2021/DATA/ldf.5"
  )



# C_time_vary = c("Pol.Oriernt", "Religion.Church", "SDO")
#
# C_base = c(GendAll, EthAll, GenCohort)

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


dat1 <- dat %>%
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

dat2<- dat|>
  dplyr::mutate(NZSEI06_lead1 = lead(NZSEI06, n = 1),
                KESSLER6_lead1 = lead(KESSLER6, n = 1),
                KESSLER6_lag1 = dplyr::lag(KESSLER6),
                NZSEI06_lag1 =  dplyr::lag(NZSEI06),
                Employed_lead1 = lead(Employed, n = 1),
                Employed_lag1 = dplyr::lag(Employed, n = 1))|>
  dplyr::filter(Wave == 2019 & YearMeasured==1) |>
  dplyr::mutate(cum_lockdowns_baseline = if_else(COVID19.Timeline < 1.2, 0,
                                                 if_else(COVID19.Timeline >  1:2 & COVID19.Timeline  < 2, 2,
                                                         ifelse(COVID19.Timeline > 2 & REGC_2018 == 2  | COVID19.Timeline > 2 & REGC_2018 == 1, 4, 3)))) |>
  dplyr::mutate(COVID19.Timeline = factor(COVID19.Timeline),
                REGC_2018 = factor(REGC_2018))

#
table1::table1(~Religion.Church +factor(COVID19.Timeline) | Wave, data = dat2 )

summary(test<- lm(KESSLER6~ factor(COVID19.Timeline)  + KESSLER6_lag1, data = dat1))

summary(test<- lm(Religion.Church ~  (COVID19.Timeline), data = dat2))

plot(ggeffects::ggpredict(test, terms = "COVID19.Timeline[all]"  ))


summary(test<- lm(NZSEI06_lead1 ~ cum_lockdowns_baseline + NZSEI06_lag1, data = dat1))
summary(test<- lm(NZSEI06_lead1 ~ cum_lockdowns_baseline* factor(REGC_2018) + NZSEI06_lag1, data = dat1))



summary(test<- glm(Employed_lead1 ~ cum_lockdowns_baseline + Employed_lag1, family = "binomial" ,  data = dat1))
summary(test<- glm(Employed ~ cum_lockdowns_baseline + Employed_lag1, family = "binomial" ,  data = dat1))



dat1 <- dat %>%
  dplyr::mutate(NZSEI06_lead1 = lead(NZSEI06, n = 1),
                KESSLER6_lead1 = lead(KESSLER6, n = 1),
                KESSLER6_lag1 = dplyr::lag(KESSLER6),
                NZSEI06_lag1 =  dplyr::lag(NZSEI06),
                Employed_lead1 = lead(Employed, n = 1),
                Employed_lag1 = dplyr::lag(Employed, n = 1))|>
  dplyr::filter(Wave == 2019 & YearMeasured==1) |>
  dplyr::mutate(cum_lockdowns_baseline = if_else(COVID19.Timeline < 1.2, 0,
                                                 if_else(COVID19.Timeline >  1:2 & COVID19.Timeline  < 2, 2,
                                                         ifelse(COVID19.Timeline > 2 & REGC_2018 == 2  | COVID19.Timeline > 2 & REGC_2018 == 1, 4, 3))))


summary(test<- lm(KESSLER6_lead1 ~   + KESSLER6_lag1, data = dat1))
summary(test<- lm(KESSLER6_lead1 ~  factor(COVID19.Timeline) * KESSLER6_lag1, data = dat1))

summary(test<- lm(NZSEI06_lead1 ~ cum_lockdowns_baseline + NZSEI06_lag1, data = dat1))
summary(test<- glm(Employed_lead1 ~ cum_lockdowns_baseline + Employed_lag1, family = "binomial" ,  data = dat1))
summary(test<- glm(Employed ~ cum_lockdowns_baseline + Employed_lag1, family = "binomial" ,  data = dat1))

# Code for timeline if needed
#   dplyr::mutate(cum_lockdowns_baseline = if_else(COVID19.Timeline < 1.2, 0,
# if_else(COVID19.Timeline >  1:2 & COVID19.Timeline  < 2, 2,
#         ifelse(COVID19.Timeline > 2 & REGC_2018 == 2  | COVID19.Timeline > 2 &
#                  REGC_2018 == 1, 4, 3)))) |>

