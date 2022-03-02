library(tidyverse)
library(haven)

# 
# investigate the relationship between class position and support for social
# spending and/or redistribution
#

gss_class_tbl <- read_dta("data/GSS2018.dta") %>% as_tibble()

gss_class_tbl <-  
  transmute(
    
    # CLASS LOCATION
    #
    # following variables assess the class location of a respondent in terms
    # of a (very) simplified, relational framework concocted by Erik Olin Wright
    # 
    #                          Self-Employed
    #                     
    #                         YES            NO 
    #                 ------------------------------
    # Supervise   YES | Capitalists     | Managers |
    # the Labor       ------------------------------
    # of others    NO | Petty Bourgeois | Workers  |
    #                 ------------------------------
    
    gss_class_tbl,
    working = wrkstat, # working?
    self_employed = (wrkslf == 1), # self-employed?
    supervisor = !is.na(yousup), # supervisor?
    real_income = realrinc, # respondent real income, base = 1986
    
    class_position = ifelse(!self_employed & !supervisor, "worker",
                       ifelse(self_employed & !supervisor, "petty_bourgeois",
                       ifelse(!self_employed & supervisor, "manager",
                       ifelse(self_employed & supervisor, "capitalist", NA)))),
    
     
    # SUPPORT FOR SOCIAL SPENDING
    # 
    # following variables assess whether the respondent feels we are
    # spending either too much, too little, or about the right amount on
    # the specified social spending issues
    
    state_childcare_support = natchld, # assistance for childcare
    state_education_support = nateduc, # improving nation's education system
    state_drug_treatment_support = natdrug, # dealing with drug addiction
    state_welfare_support = natfare, # welfare
    state_health_support = natheal, # improving and protecting nation's health
    state_mass_transport_support = natmass, # mass transportation
    state_parks_and_recreation = natpark, # parks and recreation
    state_highways_bridges_support = natroad, # highways and bridges
    state_social_security_support = natsoc, # social security
    
    social_spending_support = state_childcare_support + 
      state_education_support + state_drug_treatment_support + 
      state_welfare_support + state_health_support + 
      state_mass_transport_support + state_parks_and_recreation + 
      state_parks_and_recreation + state_highways_bridges_support + 
      state_social_security_support,
    
    
    # SUPPORT FOR REDISTRIBUTION
    #
    # following variables assess whether the respondent feels we are
    # spending either too much, too little, or about the right amount on
    # the specified social spending issues
    state_racial_redistribution_support = natracey, # assistance to blacks (NO)
     
    # 1 as meaning that the government ought to reduce the income 
    # differences between rich and poor, and a score of 7 meaning that the
    # government should not concern itself with reducing income 
    # differences
    
    state_economic_redistribution_support = eqwlth,
  )

ggplot(gss_class_tbl) +
  geom_boxplot(aes(class_position, 
                   state_economic_redistribution_support, 
                   color = class_position)) +
  labs(title = 
         "Marxian Class Position and Support for Economic Redistribution") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(gss_class_tbl) +
  geom_boxplot(aes(class_position, 
                   state_racial_redistribution_support * (7/3), 
                   color = class_position)) +
  labs(title = 
         "Marxian Class Position and Support for Racial Redistribution") +
  theme(plot.title = element_text(hjust = 0.5))

# class position linear regression
print(summary(lm(state_economic_redistribution_support ~ class_position,
                   gss_class_tbl)))