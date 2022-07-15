# description religion 

# read files
source(here::here("scripts", "funs.R"))

# read libraries in
source(here::here("scripts", "libs.R"))


# read data
df <- readRDS(here::here("data_raw", "df"))

# order 
df$GenCohort <- ordered(df$GenCohort, levels = c("Gen_Silent: born< 1946", "Gen Boomers: born >= 1946 & b.< 1965",
                                                 " GenX: born >=1961 & b.< 1981","GenZ: born >= 1996 "))


# rename
df <- df %>%
  dplyr::mutate(Euro = if_else(EthCat == 1, 1, 0)) %>%
  dplyr::mutate(Male = ifelse(GendAll == 1, 1, 0) )



# facets ------------------------------------------------------------------
library(lcsm)
# Religious behaviours
# How many times did you attend a church or place of worship in the last month?


churchwide <-  df %>%
  dplyr::filter(YearMeasured == 1) %>%
  dplyr::filter(!is.na(Religion.Church)) %>%
  dplyr::select(Id, Religion.Church, Wave) %>%
  group_by(Id) %>% filter(n() > 9) %>%
  filter(n() != 0) %>%
  ungroup(Id) %>%
  spread(Wave, Religion.Church)

churchwide <- churchwide[complete.cases(churchwide),]
dim(churchwide)
Relidwide
x_var_list <- names(churchwide[, 2:11])


plot_trajectories(
  data = churchwide,
  id_var = "Id",
  var_list = x_var_list,
  line_colour = "red",
  xlab = "Years",
  ylab = "Church Attendance",
  connect_missing = F,
  scale_x_num = T,
  scale_x_num_start = 1,
  random_sample_frac = 0.086,
  seed = 1234
  # title_n = T
)  +
  facet_wrap( ~ Id)

dev.off()


# facet religious ---------------------------------------------------------

Relid <-  df %>%
  dplyr::filter(YearMeasured == 1) %>%
  dplyr::filter(!is.na(Relid)) %>%
  dplyr::select(Id, Relid, Wave) %>%
  group_by(Id) %>% filter(n() > 9) %>%
  filter(n() != 0) %>%
  ungroup(Id) %>%
  spread(Wave, Relid)

Relid <- Relid[complete.cases(Relid),]
dim(Relid)
Relid
x_var_list <- names(Relid[, 2:11])


plot_trajectories(
  data = Relid,
  id_var = "Id",
  var_list = x_var_list,
  line_colour = "red",
  xlab = "Years",
  ylab = "Religious Identification",
  connect_missing = F,
  scale_x_num = T,
  scale_x_num_start = 1,
  random_sample_frac = 0.086,
  seed = 1234
  # title_n = T
)  +
  facet_wrap( ~ Id)

dev.off()







# facet religious ---------------------------------------------------------
#Religiosity
#Do you identify with a religion and/or spiritual group?
#  What religion or spiritual group?

#Religious Identification
#How important is your religion to how you see yourself?

Relid <-  df %>%
  dplyr::filter(YearMeasured == 1) %>%
  dplyr::filter(!is.na(Relid)) %>%
  dplyr::select(Id, Relid, Wave) %>%
  group_by(Id) %>% filter(n() > 9) %>%
  filter(n() != 0) %>%
  ungroup(Id) %>%
  spread(Wave, Relid)

Relid <- Relid[complete.cases(Relid),]
dim(Relid)
Relid
x_var_list <- names(Relid[, 2:12])


plot_trajectories(
  data = Relid,
  id_var = "Id",
  var_list = x_var_list,
  line_colour = "red",
  xlab = "Years",
  ylab = "Religious Identification",
  connect_missing = F,
  scale_x_num = T,
  scale_x_num_start = 1,
  random_sample_frac = 0.086,
  seed = 1234
  # title_n = T
)  +
  facet_wrap( ~ Id)

dev.off()



# facet God ---------------------------------------------------------------
# Belief in a God 
# Do you believe in a God

library(dplyr)
Believe.God <-  df %>%
  dplyr::filter(
    YearMeasured == 1 & Wave %in%
      c("2010",
        "2011",
        "2012",
        "2013",
        "2014",
        "2015",
        "2016",
        "2017",
        "2018",
        "2019",
        "2020")) %>%
  droplevels() %>%
  filter(!is.na(Believe.God))%>%
  dplyr::select(Id, Believe.God, Wave) %>%
  group_by(Id) %>% filter(n() > 5) %>%
  filter(n() != 0) %>%
  ungroup(Id) %>%
  spread(Wave, Believe.God)
#Believe.God
Believe.God <- Believe.God[complete.cases(Believe.God),]
dim(Believe.God)
x_var_list <- names(Believe.God[, 2:11])


plot_trajectories(
  data = Believe.God,
  id_var = "Id",
  var_list = x_var_list,
  line_colour = "red",
  xlab = "Years",
  ylab = "Belief in God",
  connect_missing = F,
  scale_x_num = T,
  scale_x_num_start = 1,
  random_sample_frac = 0.086,
  seed = 1234
  # title_n = T
)  +
  facet_wrap( ~ Id)

dev.off()



# believe Spirit ----------------------------------------------------------
# Belief in a spirit or life force 
# Do you believe in some form of spirit or lifeforce?
  
library(dplyr)
Believe.Spirit <-  df %>%
  dplyr::filter(
    YearMeasured == 1 & Wave %in%
      c("2010",
        "2011",
        "2012",
        "2013",
        "2014",
        "2015",
        "2016",
        "2017",
        "2018",
        "2019",
        "2020")) %>%
  droplevels() %>%
  filter(!is.na(Believe.Spirit))%>%
  dplyr::select(Id, Believe.Spirit, Wave) %>%
  group_by(Id) %>% filter(n() > 5) %>%
  filter(n() != 0) %>%
  ungroup(Id) %>%
  spread(Wave, Believe.Spirit)
#Believe.God
Believe.Spirit <- Believe.Spirit[complete.cases(Believe.Spirit),]
dim(Believe.Spirit)
x_var_list <- names(Believe.Spirit[, 2:11])


plot_trajectories(
  data = Believe.Spirit,
  id_var = "Id",
  var_list = x_var_list,
  line_colour = "red",
  xlab = "Years",
  ylab = "Belief in Spirit",
  connect_missing = F,
  scale_x_num = T,
  scale_x_num_start = 1,
  random_sample_frac = 0.086,
  seed = 1234
  # title_n = T
)  +
  facet_wrap( ~ Id)

dev.off()



# spirit identification ---------------------------------------------------
#I identify as a spiritual person.
library(report)
df1 <- df%>%
  filter(YearMeasured == 1) %>%
  filter(Wave == 2018 | Wave == 2020) %>% 
  droplevels() %>%
  select(Wave, Spiritual.Identification) %>%
  arrange(Wave)


df2 <-  df1 %>% group_by(Wave) %>%
  summarise(
    mean = mean(Spiritual.Identification, na.rm = T),
    sd = sd(Spiritual.Identification, na.rm = T)
  )

df2

# 
# ggplot(df2, aes(x = Wave, y = mean, fill = Wave)) +
#   geom_line() +
#   geom_point() +
#   geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd),
#                 width = .2,
#                 position = position_dodge(.9))
# 
# library(see)
# p2 <-
#   ggplot(df1, aes(x = Wave, y = Spiritual.Identification, fill = Wave)) +
#   geom_violindot(fill_dots = "black") +
#   theme_modern() +
#   scale_fill_material_d()
# 
# p2

# How many times did you pray in the last week? ---------------------------


library(dplyr)
Religion.Prayer <-  df %>%
  dplyr::filter(
    YearMeasured == 1 & Wave %in%
      c("2010",
        "2011",
        "2012",
        "2013",
        "2014",
        "2015",
        "2016",
        "2017",
        "2018",
        "2019",
        "2020")) %>%
  droplevels() %>%
  filter(!is.na(Religion.Prayer))%>%
  dplyr::select(Id, Religion.Prayer, Wave) %>%
  dplyr::mutate(Religion.Prayer = log(Religion.Prayer + 1)) %>%
  group_by(Id) %>% filter(n() > 5) %>%
  filter(n() != 0) %>%
  ungroup(Id) %>%
  spread(Wave, Religion.Prayer)
#Believe.God
Religion.Prayer <- Religion.Prayer[complete.cases(Religion.Prayer),]
dim(Religion.Prayer)

Religion.Prayer

x_var_list <- names(Religion.Prayer[, 2:7])

df1 <- df%>%
  filter(YearMeasured ==1& Wave %in%
  c(
    "2014",
    "2015",
    "2016",
    "2017",
    "2018",
    "2019")) %>%
  droplevels() %>%
  dplyr::filter(!is.na(Religious)) %>%
  dplyr::mutate(prays = if_else(Religion.Prayer >0, 1, 0))  

# table(df1$notrelprays)
# 
# table1::table1(~ as.factor(prays) * as.factor(Religious), data = df1,   overall = "FALSE")
# 
# table(nr$Religion.Prayer)

plot_trajectories(
  data = Religion.Prayer,
  id_var = "Id",
  var_list = x_var_list,
  line_colour = "red",
  xlab = "Years",
  ylab = "Log Prayer Freq",
  connect_missing = F,
  scale_x_num = T,
  scale_x_num_start = 1,
  random_sample_frac = 0.086,
  seed = 1234
  # title_n = T
)  +
  facet_wrap( ~ Id)

dev.off()



# How many times did you read religious scripture in the last  --------


#  How many times did you read religious scripture in the last week?

Religion.Scripture <-  df %>%
  dplyr::filter(
    YearMeasured == 1 & Wave %in%
      c("2010",
        "2011",
        "2012",
        "2013",
        "2014",
        "2015",
        "2016",
        "2017",
        "2018",
        "2019",
        "2020")) %>%
  droplevels() %>%
  filter(!is.na(Religion.Scripture))%>%
  dplyr::select(Id, Religion.Scripture, Wave) %>%
 dplyr::mutate(Religion.Scripture = log(Religion.Scripture + 1)) %>%
  group_by(Id) %>% filter(n() > 5) %>%
  filter(n() != 0) %>%
  ungroup(Id) %>%
  spread(Wave, Religion.Scripture)
#Believe.God
Religion.Scripture <- Religion.Scripture[complete.cases(Religion.Scripture),]
dim(Religion.Scripture)

Religion.Scripture

x_var_list <- names(Religion.Scripture[, 2:7])


plot_trajectories(
  data = Religion.Scripture,
  id_var = "Id",
  var_list = x_var_list,
  line_colour = "red",
  xlab = "Years",
  ylab = "Log Scripture Freq",
  connect_missing = F,
  scale_x_num = T,
  scale_x_num_start = 1,
  random_sample_frac = 0.086,
  seed = 1234
  # title_n = T
)  +
  facet_wrap( ~ Id)

dev.off()



# religious ---------------------------------------------------------------


# How many times did you read religious scripture in the last  --------


#  How many times did you read religious scripture in the last week?

Religious <-  df %>%
  dplyr::filter(
    YearMeasured == 1) %>% 
  droplevels() %>%
  filter(!is.na(Religious))%>%
  dplyr::select(Id, Religious, Wave) %>%
  group_by(Id) %>% filter(n() > 5) %>%
  filter(n() != 0) %>%
  ungroup(Id) %>%
  spread(Wave, Religious)
#Believe.God
Religious <- Religious[complete.cases(Religious),]
dim(Religious)

Religious

x_var_list <- names(Religious[, 2:13])


plot_trajectories(
  data = Religious,
  id_var = "Id",
  var_list = x_var_list,
  line_colour = "red",
  xlab = "Years",
  ylab = "Religious",
  connect_missing = F,
  scale_x_num = T,
  scale_x_num_start = 1,
  random_sample_frac = 0.086,
  seed = 1234
  # title_n = T
)  +
  facet_wrap( ~ Id)

dev.off()


# compare generation 2009 2021 -------------------------------------------------------
#https://stackoverflow.com/questions/68263841/different-robust-standard-errors-of-poisson-regression-in-stata-and-r

library(sandwich) # for robust poisson regression 
library(lmtest)
library(ggthemes)

log2prob <- function(log...){
  return(exp(log...))
}

dat <- df %>% 
  filter(YearMeasured == 1 & Wave %in% c("2010","2020")) %>% 
  dplyr::mutate(rel = as.integer(Religious)-1)%>% 
  droplevels()
dat$Spiritual.Identification
dat <- transform(dat, 
                 god = as.numeric(Believe.God) - 1,
                 spirit = as.numeric(Believe.Spirit) - 1,
                 spiritual = Spiritual.Identification,
                 relid = Relid)

# descriptive data
dat %>% 
  select(Wave, Id) %>% 
  group_by(Wave) %>%
  count()
                
# models

rel <- glm(rel ~ Wave, data = dat , family = poisson(link = log))
summary(rel)

relid <- lm(relid  ~ Wave, data = dat )
spirit <- glm(spirit ~ Wave, data = dat , family = poisson(link = log))
god <-  glm(god ~ Wave, data = dat , family = poisson(link = log))
spiritual <-  lm(god ~ Wave, data = dat )

(smy <- coeftest(m0, vcovHC(m0, type="HC0")))
(irr <- exp(coef(spirit)))
rse <- sqrt(diag(vcovHC(spirit, type="HC0")))
irr*rse


(smy <- coeftest(spirit, vcovHC(spirit, type="HC0")))
(irr <- exp(coef(spirit)))
rse <- sqrt(diag(vcovHC(spirit, type="HC0")))
irr*rse

(smy <- coeftest(god, vcovHC(god, type="HC0")))
(irr <- exp(coef(god)))
rse <- sqrt(diag(vcovHC(god, type="HC0")))
irr*rse



rel_plot <- plot(ggeffects::ggpredict(rel, "Wave", vcov.type = "HC0")) + scale_y_continuous(limits = c(0,1)) +
  labs(title = "Probability of being religious identification: 2010/11 (n = 4441) v 2020/21 (n=38,551)",
       subtitle = "New Zealand Attitudes and Values Study" )
rel_plot

ggeffects::ggpredict(spirit, "Wave", vcov.type = "HC0")
sprit_plot <- plot(ggeffects::ggpredict(spirit, "Wave", vcov.type = "HC0")) + scale_y_continuous(limits = c(0,1)) +
  labs(title = "Probability of believing in a spirit or life force: 2010/11 (n = 4441) v 2020/21 (n=38,551)",
       subtitle = "New Zealand Attitudes and Values Study" )
sprit_plot


ggeffects::ggpredict(god, "Wave", vcov.type = "HC0")
god_plot <- plot(ggeffects::ggpredict(god, "Wave", vcov.type = "HC0")) + scale_y_continuous(limits = c(0,1)) +
  labs(title = "Probability of believing in God: 2010/11 (n = 4441) v 2020/21 (n=38,551)",
       subtitle = "New Zealand Attitudes and Values Study" )
god_plot


ggeffects::ggpredict(relid, "Wave", vcov.type = "HC0")

religious_plot <- plot(ggeffects::ggpredict(relid, "Wave", vcov.type = "HC0")) + scale_y_continuous(limits = c(1,7)) +
  labs(title = "Level of religious identification : 2010/11 (n = 4441) v 2020/21 (n=38,551)",
       subtitle = "New Zealand Attitudes and Values Study",
       y= "Religious Identification (1-7)")  + theme_empty()
religious_plot



## pop graphs 
dr <- df %>% 
  dplyr::filter(YearMeasured == 1 & Wave %in% c("2010", "2011", "2012", "2013", "2014" ,"2015", "2016", "2017", "2018", 
                    "2019","2020"))%>% 
  dplyr::mutate(yrs = TSCORE - min(TSCORE),
                church = if_else(Religion.Church > 4, 4, Religion.Church))

dr_r <- df %>% 
  dplyr::filter( Religious == 1) %>%
  dplyr::filter(YearMeasured == 1 & Wave %in% c("2010", "2011", "2012", "2013", "2014" ,"2015", "2016", "2017", "2018", 
                                                "2019","2020"))%>% 
  dplyr::mutate(yrs = TSCORE - min(TSCORE),
                church = if_else(Religion.Church > 4, 4, Religion.Church))


(
  reltime <- ggplot(data = dr,
               aes(x = yrs,
                   y = Relid)) +
    #  geom_line(alpha = .005) +
    stat_summary(
      geom = "point",
      fun = mean,
      size = .1,
      alpha = .2
    ) +
    # geom_smooth() +
    stat_smooth(method = "gam") +
    theme_minimal() + labs("")   + theme(
      legend.position = "right",
      legend.text = element_text(size = 6),
      legend.title = element_text(color = "Black", size = 8)
    ) +
    scale_colour_viridis_d(option = "plasma") +
    ylab("Religious Identification (1-7)") +
    labs(title = "Religious Identification") +
    xlab("Years 2010-2021")
)

(
  churchtime <- ggplot(data = dr,
                aes(x = yrs,
                    y = church)) +
    #  geom_line(alpha = .005) +
    stat_summary(
      geom = "point",
      fun = mean,
      size = .2,
      alpha = .2
    ) +
    # geom_smooth() +
   # stat_smooth(method = "gam") +
    stat_smooth(method = "lm") +
    theme_minimal() + labs("")   + theme(
      legend.position = "right",
      legend.text = element_text(size = 6),
      legend.title = element_text(color = "Black", size = 8)
    ) +
    scale_colour_viridis_d(option = "plasma") +
    ylab("Church Attendance (1-7)") +
    labs(title = "Church Attendance") +
    xlab("Years 2010-2021")
)


(
  churchtime <- ggplot(data = dr_r,
                       aes(x = yrs,
                           y = church)) +
    #  geom_line(alpha = .005) +
    stat_summary(
      geom = "point",
      fun = mean,
      size = .2,
      alpha = .2
    ) +
    # geom_smooth() +
    stat_smooth(method = "lm") +
    theme_minimal() + labs("")   + theme(
      legend.position = "right",
      legend.text = element_text(size = 6),
      legend.title = element_text(color = "Black", size = 8)
    ) +
    scale_colour_viridis_d(option = "plasma") +
    ylab("Church attednance (1-4)") +
    labs(title = "Church attendance among religious") +
    xlab("Years 2010-2021")
)


# perceived religious discrimination  -------------------------------------

dt <- df %>% 
  dplyr::filter(YearMeasured == 1 & Religious == 1 & Wave %in% 
           c("2015", "2016", "2017", "2018", 
             "2019","2020")) %>% 
  droplevels() %>% 
  dplyr::select(Id, Wave, Perc.Religious.Discrim,BigDoms, Religious, TSCORE)  %>%
  dplyr::mutate(yrs = TSCORE - min(TSCORE))



(
  ld <- ggplot(data = dt,
               aes(x = yrs,
                   y = Perc.Religious.Discrim)) +
  #  geom_line(alpha = .005) +
    stat_summary(
      geom = "point",
      fun = mean,
      size = .1,
      alpha = .2
    ) +
   # geom_smooth() +
    stat_smooth(method = "gam") +
    theme_minimal() + labs("")   + theme(
      legend.position = "right",
      legend.text = element_text(size = 6),
      legend.title = element_text(color = "Black", size = 8)
    ) +
    scale_colour_viridis_d(option = "plasma") +
    ylab("Perceived Religious Discrimination (1-7)") +
    labs(title = "Perceived Religious Discrimination") +
    xlab("Years 2015-2021")
)

dtrel <- dt %>% 
  dplyr::filter(Religious == 1)

(perc <- lme4::lmer(Perc.Religious.Discrim ~ yrs  + (1|Id), data = dtrel)  )

plot(
  ggeffects::ggpredict(perc, terms = "yrs"), add.data = T, dot.alpha = .01
  )

# https://stackoverflow.com/questions/68263841/different-robust-standard-errors-of-poisson-regression-in-stata-and-r


dtrel20 <- df %>% 
  dplyr::filter(YearMeasured == 1  &  Wave == "2020")%>% 
  dplyr::select(Id, Wave, Perc.Religious.Discrim,BigDoms, Religious, TSCORE)  %>%
  dplyr::mutate(yrs = TSCORE - min(TSCORE))

dtrel20 <- transform( dtrel20 , 
                      BigDoms=relevel(BigDoms, ref="Not_Rel"))


(perm <- lm(Perc.Religious.Discrim ~  -1 + BigDoms, data = dtrel20))
length(unique(dtrel20$Id))

plot(
  ggeffects::ggpredict(perm, terms = "BigDoms"), add.data = T, dot.alpha = .01
) + labs(title = "Perceived Religious Discrimination 2020/21 (N = 38551, NZAVS)", 
         y = "Perceived Religious Discrimination",
         x = "Denominations")



# attitudes to religion ---------------------------------------------------

at <- df %>% 
  dplyr::filter(YearMeasured == 1  &  Wave %in% c("2016", "2018") )%>% 
  droplevels() %>%
  dplyr::select(Id, Wave, RELIGIOUS.ATTS,BigDoms, Religious, TSCORE)  %>%
  dplyr::mutate(yrs = TSCORE - min(TSCORE))

at %>%
  group_by(Wave)%>%
  count()

(atts <- lm(RELIGIOUS.ATTS ~  Wave + BigDoms, data = at))

plot(
  ggeffects::ggpredict(atts, terms = c("Wave", "BigDoms")), add.data = F, dot.alpha = .002
) + labs(title = "Attitudes to religion as a social good 2016-17 (n = 21936) & 2018-19 (N = 47948)", 
         y = "Religion as social good",
         x = "Waves") + scale_color_flat_d() + scale_y_continuous(limits = c(1,7))




# attitudes to religion ---------------------------------------------------




