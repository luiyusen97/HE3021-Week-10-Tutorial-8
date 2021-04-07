library(haven) # read dta files
library(tidyverse) # manipulate dataframes
library(ivreg) # 2 stage linear regression
library(aod) # for chi2 and F tests
library(AER) # for ivreg's associated statistical tests

filpaths <- list.files(path = paste0(getwd(),"//rawdata//"), 
                       full.names = TRUE)

wage2 <- read_dta(file = filpaths[1])

instr_exo_test_model <- lm(formula = educ ~ sibs + brthord + meduc + married + exper + tenure, data = wage2)
instr_exo_test <- wald.test(Sigma = vcov(instr_exo_test_model), b = coefficients(instr_exo_test_model), Terms = c(2,3,4))
# instrumental relevance true

twostage_model <- ivreg(formula = lwage ~ married + exper + tenure + educ | married + exper + tenure + sibs + brthord + meduc, data = wage2)
twostage_model_summary <-(summary(twostage_model, diagnostics = TRUE))

OLS_model <- lm(formula = lwage ~ educ + married + exper + tenure, data = wage2)

# hausman_model <- data.frame(twostage_model$model$lwage, instr_exo_test_model$model$educ,
#                        instr_exo_test_model$model$sibs, instr_exo_test_model$model$brthord,
#                        instr_exo_test_model$model$meduc, instr_exo_test_model$model$married,
#                        instr_exo_test_model$model$exper, instr_exo_test_model$model$tenure,
#                        instr_exo_test_model$residuals)
# colnames(hausman_model) <- c("lwage", "educ", "sibs", " brthord", "meduc", "married", "exper", "tenure", "residuals")
# hausman_model <- lm(formula = lwage ~ educ + married + exper + tenure + residuals, data = hausman_model)
# summary(hausman_model)

ivreg_manual_data <- cbind(twostage_model$model$lwage, twostage_model$model$married, twostage_model$model$exper,
                           twostage_model$model$tenure, instr_exo_test_model$fitted.values)
ivreg_manual_data <- as.data.frame(ivreg_manual_data)
colnames(ivreg_manual_data) <- c("lwage", "married", "exper", "tenure", "hateduc")
ivreg_manual_model <- lm(lwage ~ married + exper + tenure + hateduc, data = ivreg_manual_data)