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
    # following variables assess the class location of a respondent in terms
    # of a (very) simplified, relational framework concocted by Erik Olin Wright
    # 
    #                          self-employed
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
    real_income = realrinc, # respondent real income, base = 1986,
    
    
     
    # SUPPORT FOR SOCIAL SPENDING
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
    
    
     
    # SUPPORT FOR REDISTRIBUTION
    # following variables assess whether the respondent feels we are
    # spending either too much, too little, or about the right amount on
    # the specified social spending issues
    state_racial_redistribution_support = natracey, # assistance to blacks (NO)
     
    # 1 as meaning that the government ought to reduce the income 
    # differences between rich and poor, and a score of 7 meaning that the
    # government should not concern itself with reducing income 
    # differences
    state_economic_redistribution_support = eqwlth
  )


ggplot(data = gss_class_tbl) +
  geom_smooth(mapping = aes(x = real_income, 
                            y = state_economic_redistribution_support))

# relationship between self-employment and support for redistribution
ggplot(data = gss_class_tbl) +
  geom_boxplot(mapping = aes(x = self_employed,
                         y = state_economic_redistribution_support))

# relationship between supervisor status and support for redistribution
ggplot(data = gss_class_tbl) +
  geom_boxplot(mapping = aes(x = supervisor,
                             y = state_economic_redistribution_support))

# relationship between supervisor status and support for racial redistribution
ggplot(data = gss_class_tbl) +
  geom_smooth(mapping = aes(x = real_income,
                            y = state_racial_redistribution_support))