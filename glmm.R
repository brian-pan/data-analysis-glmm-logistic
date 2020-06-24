
# libraries:
library(tidyverse)
library(lmtest)
library(glmmTMB)

# import data:
csiRaw <- read_csv("analysisvariables.csv")

# Create a new variable called response that is 
# 1 if ControlType equals case:
csi_data <- csi_raw %>% 
  mutate(response = ifelse(ControlType == 'case', 1, 0)) %>% 
  filter(ControlType %in% c("case","ran")) %>% 
  mutate(site_fac = as.factor(SITE)) %>% 
  select(response, ControlType, Predisposed,
         AlteredMentalStatus, AxialLoadAnyDoc, 
         HighriskMVC, HighriskHitByCar, HighriskDiving, 
         Clotheslining, site_fac)

# explore data:
glimpse(csi_data)

head(csi_data)

# ignore missings:
csi_data_clean <- na.omit(csi_data)

# number of cases and controls:
table(csi_data_clean$ControlType)
table(csi_data_clean$site_fac)

# Fit a generalized linear model (logistic regression):
csi_glm <- glm(response ~ Predisposed + AlteredMentalStatus + AxialLoadAnyDoc + 
                 HighriskMVC + HighriskHitByCar + HighriskDiving + Clotheslining, 
               family = binomial(link = "logit"), 
               data = csi_data_clean)
summary(csi_glm)
confint(csi_glm)

# Fit a generalized linear mixed model (logistic regression):
csi_glmm = glmmTMB(response ~ Predisposed + AlteredMentalStatus + AxialLoadAnyDoc + 
                     HighriskMVC + HighriskHitByCar + HighriskDiving + Clotheslining +
                     (1|site_fac),
                   family = binomial(link="logit"), 
                   data = csi_data_clean)
summary(csi_glmm)
confint(csi_glmm)
# Create a exp table with 2 digits:
knitr::kable(exp(confint(csi_glmm)), digits = 2)
# create a plot to visualize:
Pmisc::ranefPlot(csi_glmm)

# Fit a generalized linear model (logistic) WITHOUT covariate "site":
csi_glm_no_site <- glm(response ~ Predisposed + AlteredMentalStatus + AxialLoadAnyDoc + 
                         HighriskMVC + HighriskHitByCar + HighriskDiving + Clotheslining, 
                       family = binomial(link = "logit"), 
                       data = csi_data_clean)

# Do a likelihood ratio test for the two GLMs,
# to see whether "site" is significant or not:
lmtest::lrtest(csi_glm_no_site, csi_glm)

#Do a likelihood ratio test for the GLM and GLMM,
# to see whether random effect is significant or not:
lmtest::lrtest(csi_glm, csi_glmm)
P. = sqrt( climhyd[,"Precip"] ) %>% ts(freq = 12)
I. = log( climhyd[,"Inflow"] ) %>% ts(freq=12) 