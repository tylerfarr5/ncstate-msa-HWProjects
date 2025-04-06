ins <- read.csv("C:\\Users\\coope\\OneDrive\\Desktop\\IAA Material\\Fall\\Logistic Regression\\HW\\HW1\\Homework1_LR\\insurance_t.csv")
# Starts final code
# Libraries
library(vcdExtra)
library(gmodels)
library(DescTools)
library(mgcv)
# Code to sort variables into 4 categories and then get p-value
# df Will contain the variable type and the p-value
df <- data.frame(matrix(ncol = 3, nrow = 0))
# df_cont will contain continuous variables and whether they meet linearity assumption
df_cont <- data.frame(matrix(ncol = 3, nrow = 0))
# df_odds will contain binary variables and the odds ratio
df_odds <- data.frame(matrix(ncol = 3, nrow = 0))
for (i in 1:48) {
  # This is categorical, were told specifically that BRANCH is categorical
  if (names(ins[i]) == "BRANCH") {
    # Nominal variables use Pearson Chi-Square Test to calculate p-value
    df <- rbind(df,c(names(ins)[i], "Nominal", chisq.test(table(ins$INS, ins$BRANCH))[[3]]))
  }
  # Excludes the target variable INS from any tests
  else if (names(ins)[i] == "INS") {
    df <- rbind(df,c(names(ins)[i], "Target", "N/A"))
  }
  # This is binary, due to only taking on 2 data types
  else if (length(na.omit(unique(ins[[i]]))) == 2 & typeof(ins[[i]]) == "integer") {
    # Adds variable type and p-value from Mantel-Haenszel Chi-Square Test
    df <- rbind(df,c(names(ins)[i],"Binary", CMHtest(table(ins$INS, ins[[i]]))$table[1,3]))
    # Creates table of odds ratio for binary variables
    df_odds <- rbind(df_odds,c(names(ins)[i],"Binary", OddsRatio(table(ins$INS, ins[[i]]))))
  }
  # This is nominal
  else if (is.character(ins[[i]]) == TRUE) {
    # Nominal variables use Pearson Chi-Square Test to calculate p-value
    df <- rbind(df,c(names(ins)[i], "Nominal", chisq.test(table(ins$INS, ins[[i]]))[[3]]))
  }
  # This is continuous due to taking on more than 10 unique values
  else if (length(unique(ins[[i]])) >= 10) {
    # Creates traditional logistic regression model that only has the continous variable
    logit.model <- glm(INS ~ (ins[[i]]),
                       data = ins, family = binomial(link = "logit"))
    # Creates vector of values for the current continous variable, this is needed to use gam when iterating
    x_var <- (ins[[i]])
    # Creates GAM logistic regression with spline function around the continous variable
    fit.gam <- gam(INS ~ s(x_var),
                   data = ins, family = binomial(link = 'logit'), method = 'REML')
    # Anova tests if there is a difference between a normal logistic regression model and a GAM one
    linear_assump_p <- anova(logit.model, fit.gam, test = 'Chisq')$`Pr(>Chi)`[2]
    # if the p-value is less than the significance level, there is a difference between the two models, and the linearity assumption is not met
    if (linear_assump_p < 0.002) {
      assump <- "Not Met"
    }
    # Linearity assumption met otherwise
    else {
      assump <- "Met"
    }
    # Calculates p-value for continuous variable by modeling a logistic regression
    df <- rbind(df,c(names(ins)[i], "Continuous", summary(logit.model)[["coefficients"]][2,4]))
    # Calculates whether linearity assumption is met
    df_cont <- rbind(df_cont, c(names(ins)[i], linear_assump_p, assump))
  }
  # This is for numeric variables that is going to be treated as ordinal instead (due to having less than 10 unique values)
  else  {
    # Adds variable type and p-value from Mantel-Haenszel Chi-Square Test
    df <- rbind(df,c(names(ins)[i], "Ordinal", CMHtest(table(ins$INS, factor(ins[[i]])))$table[1,3]))
  }
}
colnames(df) <- c('Variable_Name', 'Variable_Type', 'P-Value')
colnames(df_odds) <- c('Variable_Name', 'Variable_Type', 'Odds_Ratio')
colnames(df_cont) <- c('Variable_Name', 'P-Value', 'Assumption Met ?')
options("scipen" = 101)
options()$scipen
# Table with all variables, ordered by p-value
total_p_ranked <- df[order(df$`P-Value`),]
# Table with only significant variables, ordered by p-value
significant_p_ranked <- total_p_ranked[total_p_ranked$`P-Value` <= 0.002,]
# Table with odds ratio only for binary variables, ordered by p-value
odds_ranked <- df_odds[order(df_odds$Odds_Ratio, decreasing = TRUE),]
# Table with continous variables and whether they meet linearity assumption or not
continous_sorted <- df_cont[order(df_cont$`Assumption Met ?`),]
# Summary of important data considerations
# Visual representation of which variables have the highest amount of missing values.
sapply(ins, function(x) sum(is.na(x)))
# Variables not needed (Could be colinear ?)
# POSAMT, LOCBAL, MTG
OddsRatio(table(ins$INV, ins$INS))
OddsRatio(table(ins$DDA, ins$INS))
table(ins$INS, ins$INV)