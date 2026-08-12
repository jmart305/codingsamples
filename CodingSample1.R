# Simulation of Trial Data; Random Effects model; Cluster-robust Variance Estimation
# Julia Martin

# This script simulates trial data for a two-arm trial with clustering in the treatment arm and fits a random effects model to estimate 
# the treatment effect. The script also extracts treatment effect variance estimate form the random effects model and compares it to 
# the variance estimate from a cluster-robust estimation approach 


# import libraries
library(nlme)
library(parallel)
library(jubilee)
library(boot)
library(mixedup)
library(lme4)
library(lmeInfo)
library(clubSandwich)
library(dfadjust)
library(geepack)
library(dplyr)

# Data Generation function

data_generation <- function(n = 346, n_clusters = 6, mean_diff = -4, sd = 9.75, ICC = 0.02) {
  # assign each patient to treatment or control
  trt <- c(rep(0, n/2), rep(1, n/2)) # 0 = education, 1 = ARBOR
  
  # assign each patient in treatment group to a PT
  pt <- c(rep(0, n/2), rep(1:n_clusters, length.out = n/2)) 
  
  # generate change in ODI score at 12 weeks for control group
  ODI_change_ctrl <- c(rnorm(n/2, 0, sd)) 
  
  # treatment group variance with clustering 
  var_total = sd^2
  ICC = ICC
  var_between = ICC * var_total
  var_within = var_total - var_between
  
  # draw random effects 
  random_effect_draw <- rnorm(n_clusters, 0, sqrt(var_between))
  random_effect <- rep(random_effect_draw, length.out = n/2)
  
  # treatment group ODI change = mu + random effect + error
  ODI_change_trt <- mean_diff + random_effect + rnorm(n/2, 0, sqrt(var_within)) 
  
  # check difference in means 
  mean(ODI_change_trt - ODI_change_ctrl)
  
  # control arm each person is one cluster
  control_clustering <- as.factor(c((n_clusters+1):((n/2)+n_clusters), pt[((n/2)+1):n]))
  
  # compile into dataframe
  arbor <- data.frame(id = 1:n, trt = trt, pt = pt, ODI_change = c(ODI_change_ctrl, ODI_change_trt), cluster = control_clustering)
  return(arbor)
}  

# Random Effects Model 
random_int_model <- function(input_data) {
  mean_diff <-  as.data.frame(matrix(numeric(), nrow = length(input_data), ncol = 6))
  colnames(mean_diff) <- c("trt_effect_est", "lower_ci", "upper_ci", "p-value", "variance", "convergence")
  for(i in 1:length(input_data)) {
    data <- input_data[[i]]
    
    # function to flag models that do not converge
    safe_lme <- function(expr) {
      tryCatch(eval(expr), error = function(e) NULL)
    }
    
    model <- safe_lme(quote(lme(ODI_change ~ trt, random = list(pt = pdDiag(~ 0 + trt)), 
                                weights = varIdent(form = ~ 1 | trt), data = data)))

    if(!is.null(model)) {
      df <- summary(model)$tTable[2,3]
      t <- qt(0.95, df)
      mean_diff[i, 1] <- fixef(model)[2]
      mean_diff[i, 2] <- intervals(model, which = "fixed")$fixed[2,1]
      mean_diff[i, 3] <- intervals(model, which = "fixed")$fixed[2,3] 
      mean_diff[i, 4] <- anova(model)$`p-value`[2]
      # extract trt effect variance
      mean_diff[i, 5] <- model$varFix[2,2]
      mean_diff[i, 6] <- 1
    } else {
      mean_diff[i, 1] <- NA
      mean_diff[i, 2] <- NA
      mean_diff[i, 3] <- NA
      mean_diff[i, 4] <- NA
      mean_diff[i, 5] <- NA
      mean_diff[i, 6] <- 0
    }
  }
  return(mean_diff)
}

# Cluster-robust variance estimation

cluster_robust <- function(input_data) {
  mean_diff <-  as.data.frame(matrix(numeric(), nrow = length(input_data), ncol = 5))
  colnames(mean_diff) <- c("trt_effect_est", "lower_ci", "upper_ci", "p-value", "variance")
  for(i in 1:length(input_data)) {
    data <- input_data[[i]]
    # run GEE model 
    model <- geeglm(ODI_change ~ trt, id = cluster, data = data, corstr = "independence", 
                    std.err = "san.se")
    # compute cluster-robust variance estimate using clubSandwich package
    v <- vcovCR(model, cluster = data$cluster, type = "CR2") 
    # compute confidence intervals using Satterthwaite approximation
    ci <- conf_int(model, vcov = v, test = "Satterthwaite", coefs = 2, level = 0.95, p_values = TRUE) # naive-t
    # extract treatment effect estimate, confidence intervals, p-value, and variance estimate
    mean_diff[i, 1] <- model$coefficients[2]
    mean_diff[i, 2] <- ci[,5] 
    mean_diff[i, 3]<- ci[,6] 
    mean_diff[i, 4] <- ci[,7]
    mean_diff[i, 5] <- v[2,2]
  }
  return(mean_diff)
}

# Simulate Data

set.seed(456)
input_data <- replicate(10000, data_generation(mean_diff = 0), simplify = F)

results_random_effects <- random_int_model(input_data)
results_cluster_robust <- cluster_robust(input_data)

# Compile results into a summary table
CI_Coverage <- c(mean(results_random_effects$upper_ci >= 0, na.rm = T),  mean(results_cluster_robust$upper_ci >= 0))
TypeI <- c(mean(results_random_effects$upper_ci < 0, na.rm = T),  mean(results_cluster_robust$upper_ci < 0))

data.frame(CI_Coverage, TypeI)
