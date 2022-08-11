# functions.R
library("here")
library("fs")
library("stdReg")
library("ggplot2")
library("mice")

# set paths for pushing files (off of github)
push_mods <- fs::path_expand("~/The\ Virtues\ Project\ Dropbox/outcomewide/mods")
push_figs <- fs::path_expand("~/Users/joseph/The\ Virtues\ Project\ Dropbox/outcomewide/figs")


## function for saving
saveh <- function(df, name) {
  x = df
  saveRDS( x,  here::here(push_mods,  paste0(name, '')))
}


# function for reading
readh <- function(name) {
  df = readRDS(here::here(push_mods,  paste0(name, '')))
  df
}



# function for mice models
pool_stglm <- function(models, df, m, x, X) {
  nx <- length(x)
  est.all <- matrix(nrow = nx, ncol = m)
  var.all <- matrix(nrow = nx, ncol = m)
  for (i in 1:m) {
    g.comp <-
      stdGlm(
        fit = models$analyses[[i]],
        data = complete(df, i),
        X = X,
        x = x
      )
    est.all[, i] <- g.comp$est
    var.all[, i] <- diag(g.comp$vcov)
  }
  #estimate
  est <- rowMeans(est.all)

  #within-variance
  W <- rowMeans(var.all)

  #between-variance
  B <- apply(X = est.all, MARGIN = 1, FUN = var)

  #total variance
  var <- W + (1 + 1 / m) * B

  #total standard error
  se <- sqrt(var)
  #confidence intervals
  ci <- cbind(est - 1.96 * se, est + 1.96 * se)
  # lower interval
  ui <- est + (1.96 * se)
  #upper interval
  li <- est - (1.96 * se)
  # row units
  row <- x
  # make data frame
  outp <- as.data.frame(cbind(row, est, se, ui, li))
  outp
}

# base R plot for mice pool_stglm outputs  (above)
plot_stglm <- function(out, ylim, main, xlab, ylab) {
  plot(
    out$row,
    out$est,
    type = "l",
    ylim = ylim,
    main = main,
    xlab = xlab,
    ylab = ylab,
    col.main = "black",
    #sub="My Sub-title", col.sub="black",
    col.lab = "black",
    cex.lab = 0.75
  )
  polygon(c(x, rev(x)),
          c(out$li, rev(out$ui)),
          col = "grey75",
          border = FALSE)
  lines(out$row, out$est, lwd = 1)
  lines(out$row, out$li, col = "red", lty = 2)
  lines(out$row, out$ui, col = "red", lty = 2)
}
# function for ggplot

ggplot_stglm <- function(out, ylim, main, xlab, ylab) {
  require(ggplot2)
  ggplot2::ggplot(out, aes(x = row, y = est)) + geom_point() + geom_pointrange(aes(ymin =  li, ymax = ui))  +
    scale_y_continuous(limits = ylim) + labs(
      title = main,
      subtitle = "Marginal predictions by g-computation",
      x = xlab,
      y = ylab
    ) +  theme_classic()
}



## function for contrasts using multiply imputed data WOW

pool_stglm_contrast <- function(out, df, m, x, X, r) {
  nx <- length(x)
  est.all <- matrix(nrow=nx, ncol=m)
  var.all <- matrix(nrow=nx, ncol=m)
  for(i in 1:m){
    g.comp <- stdGlm(fit=out$analyses[[i]], data=complete(df, i), X= X, x=x)
    ss <- summary(object=g.comp, contrast="difference", reference=r)
    #est.all[, i] <- g.comp$est
    est.all[, i] <- ss$est.table[, "Estimate"]
    #var.all[, i] <- diag(g.comp$vcov)
    var.all[, i] <- ss$est.table[, "Std. Error"]^2
  }

  #estimate
  est <- rowMeans(est.all)

  #within-variance
  W <- rowMeans(var.all)

  #between-variance
  B <- apply(X=est.all, MARGIN=1, FUN=var)

  #total variance
  var <- W+(1+1/m)*B

  #total standard error
  se <- sqrt(var)

  #confidence intervals
  ci <- cbind(est-1.96*se, est+1.96*se)

  # lower interval
  ui <- est + (1.96 * se)

  #upper interval
  li <- est - (1.96 * se)
  # row units
  row <- x
  # make data frame
  outp <- as.data.frame(cbind(row, est, se, ui, li))
  outp
}



pool_stglm_contrast_ratio <- function(out, df, m, x, X, r) {
  nx <- length(x)
  est.all <- matrix(nrow=nx, ncol=m)
  var.all <- matrix(nrow=nx, ncol=m)
  for(i in 1:m){
    g.comp <- stdGlm(fit=out$analyses[[i]], data=complete(df, i), X= X, x=x)
    ss <- summary(object=g.comp, contrast="ratio", reference=r)
    #est.all[, i] <- g.comp$est
    est.all[, i] <- ss$est.table[, "Estimate"]
    #var.all[, i] <- diag(g.comp$vcov)
    var.all[, i] <- ss$est.table[, "Std. Error"]^2
  }

  #estimate
  est <- rowMeans(est.all)

  #within-variance
  W <- rowMeans(var.all)

  #between-variance
  B <- apply(X=est.all, MARGIN=1, FUN=var)

  #total variance
  var <- W+(1+1/m)*B

  #total standard error
  se <- sqrt(var)

  #confidence intervals
  ci <- cbind(est-1.96*se, est+1.96*se)

  # lower interval
  ui <- est + (1.96 * se)

  #upper interval
  li <- est - (1.96 * se)
  # row units
  row <- x
  # make data frame
  outp <- as.data.frame(cbind(row, est, se, ui, li))
  outp
}



plot_stglm <- function(out, ylim, main, xlab, ylab) {
  plot(
    out$row,
    out$est,
    type = "l",
    ylim = ylim,
    main = main,
    xlab = xlab,
    ylab = ylab,
    col.main = "black",
    sub="Marginal predictions by g-computation",
    col.sub="black",
    col.lab = "black",
    cex.lab = 0.75
  )
  polygon(c(x, rev(x)),
          c(out$li, rev(out$ui)),
          col = "grey75",
          border = FALSE)
  lines(out$row, out$est, lwd = 1)
  lines(out$row, out$li, col = "red", lty = 2)
  lines(out$row, out$ui, col = "red", lty = 2)
}


## Contrast plot
plot_stglm_contrast <- function(out, ylim, main, xlab, ylab) {
  plot(
    out$row,
    out$est,
    type = "l",
    ylim = ylim,
    main = main,
    xlab = xlab,
    ylab = ylab,
    col.main = "black",
    sub="Marginal contrasts relative to baseline by g-computation",
    col.sub="black",
    col.lab = "black",
    cex.lab = 0.75
  )
  polygon(c(x, rev(x)),
          c(out$li, rev(out$ui)),
          col = "grey75",
          border = FALSE)
  lines(out$row, out$est, lwd = 1)
  lines(out$row, out$li, col = "red", lty = 2)
  lines(out$row, out$ui, col = "red", lty = 2)
}


# function for ggplot

ggplot_stglm_contrast <- function(out, ylim, main, xlab, ylab) {
  require(ggplot2)
  ggplot2::ggplot(out, aes(x = row, y = est)) + geom_point() + geom_pointrange(aes(ymin =  li, ymax = ui))  +
    scale_y_continuous(limits = ylim) + labs(
      title = main,
      subtitle = "Marginal contrasts relative to baseline by g-computation",
      x = xlab,
      y = ylab
    ) +  theme_classic()
}




# library(mice)
# library(stdReg)
# data(nhanes2)
#
# m <- 5
# imp_dat <- mice(data=nhanes2, printFlag=FALSE, seed=0, m=m)
# models <- with(imp_dat, glm(bmi~chl+hyp))
#
# x <- 180:190
# nx <- length(x)
# est.all <- matrix(nrow=nx, ncol=m)
# var.all <- matrix(nrow=nx, ncol=m)
# for(i in 1:m){
#   g.comp <- stdGlm(fit=models$analyses[[i]], data=complete(imp_dat, i), X="chl", x=x)
#   ss <- summary(object=g.comp, contrast="difference", reference=180)
#   #est.all[, i] <- g.comp$est
#   est.all[, i] <- ss$est.table[, "Estimate"]
#   #var.all[, i] <- diag(g.comp$vcov)
#   var.all[, i] <- ss$est.table[, "Std. Error"]^2
# }
#
# #estimate
# est <- rowMeans(est.all)
#
# #within-variance
# W <- rowMeans(var.all)
#
# #between-variance
# B <- apply(X=est.all, MARGIN=1, FUN=var)
#
# #total variance
# var <- W+(1+1/m)*B
#
# #total standard error
# se <- sqrt(var)
#
# #confidence intervals
# ci <- cbind(est-1.96*se, est+1.96*se)


# alternatives tbc
# require(gplots)
# gplots::plotCI(out$row, out$est, ui=out$ui, li=out$li)
# plotrix::plotCI(out$row, out$est, ui=out$ui, li=out$li)


# test data
# imp_dat <- mice(data=nhanes2, printFlag=FALSE, seed=0, m=m)
# models <- with(imp_dat, glm(bmi~chl+hyp))


# useful code snippet
# m18_SELF.ESTEEM <- lapply(1:10, function(i) {
#   m <- glm(SELF.ESTEEM_st, data = complete(inc_m, action = i))
# })




#
# library(MatchThem)
# library(optmatch)
# models_hw <- weightthem(Hours.Work_lead1_z ~
#                           Hours.Work_z +
#                           AGREEABLENESS_z +
#                           CONSCIENTIOUSNESS_z +
#                           EXTRAVERSION_z  +
#                           HONESTY_HUMILITY_z +
#                           NEUROTICISM_z +
#                           OPENNESS_z +
#                           Age_z +
#                           Alcohol.Frequency_z + #
#                           Alcohol.Intensity_log_z + #
#                           Bodysat_z +
#                           Believe.God_z +
#                           Believe.Spirit_z +
#                           BELONG_z + #
#                           CharityDonate_log_z + #
#                           ChildrenNum_z +
#                           Church_z +
#                           community +
#                           Edu_z +
#                           Employed_z +
#                           EmotionRegulation1_z +
#                           EmotionRegulation2_z +
#                           EmotionRegulation3_z +
#                           Euro_z +
#                           GRATITUDE_z +
#                           HomeOwner_z +
#                           Hours.Exercise_log_z +
#                           # Hours.Work_z +
#                           HLTH.BMI_z  + #
#                           HLTH.Fatigue_z + #
#                           income_log_z +
#                           ImpermeabilityGroup_z +
#                           KESSLER6sum_z + #
#                           LIFEMEANING_z + #
#                           LIFESAT_z + #
#                           Male_z +
#                           NZdep_z +
#                           NWI_z +
#                           NZSEI13_z +
#                           Parent_z +
#                           Partner_z +
#                           PERFECTIONISM_z +
#                           PermeabilityIndividual_z +
#                           Pol.Orient_z +
#                           POWERDEPENDENCE1_z + #
#                           POWERDEPENDENCE2_z + #
#                           # PWI_z +
#                           Relid_z +
#                           Respect.Self_z + #
#                           Rumination_z + #
#                           SELF.CONTROL_z + #
#                           SELF.ESTEEM_z + #
#                           SexualSatisfaction_z +#
#                           SFHEALTH_z +#
#                           Smoker_z +#
#                           Spiritual.Identification_z +
#                           Standard.Living_z +
#                           SUPPORT_z +#
#                           Urban_z +
#                           VENGEFUL.RUMIN_z +
#                           Volunteers_z +
#                           Your.Health_z +
#                           Your.Future.Security_z +
#                           Your.Personal.Relationships_z,
#                         out2_sl,
#                         approach = "within",
#                         estimand = "ATE",
#                         stabilize = TRUE,
#                         method = "ebal")
#
#
# saveh(models_hw,"models_hw")
#
#
# sum<- summary(models_hw)
# plot(sum)
# sum
# bal.tab(models_hw)
#
#
# ctrim_st <- trim(models_hw, at = .998)
# bal.tab(ctrim_st)
# summary(ctrim_st)
#
#
# # iptw models  STANDARD LIVING -------------------------------------------------------------
# # no need to trim
#
#
# out <- with(ctrim_st, glm( HLTH.BMI_lead2_z ~ Hours.Work_lead1_z ))
# output <- pool(out, dfcom = NULL)
# summary(output, conf.int = TRUE)
#
# round( EValue::evalues.OLS( , se = , sd = 1, delta = 2, true = 0), 3)
# round( EValue::evalues.RR(, lo =  , hi =, true = 1), 3)

# test <- df_in %>%
#   mutate(inc_diff = (Household.INC_100 - Household.INC_100_lead1) * 100000) %>%
#   select(Id, inc_diff) %>%
#   arrange(inc_diff)
# test %>% as_tibble() %>% print(n=500)
