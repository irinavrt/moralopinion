library(tidyverse)
library(rptR)

full_arguments <- read_rds("data/cleaned-arguments.rds")

full_arguments %>% 
  filter(mf == "fair") %>% 
  group_by(opinion) %>% 
  summarise(mean(arg_chosen, na.rm = TRUE))
  group_by()

  
m <- lme4::glmer(arg_chosen ~ opinion + politics  + change_belief + 
             (1|position) + (1|issue) + (1|workerid),
            full_arguments %>% 
              filter(mf == "fair"),
            family = binomial())
  
  
# baseline model ----------------------------------------------------------

estimate_icc_baseline <- function(data, nboot = 1000, ncores = 4){
  rptBinary(arg_chosen ~ opinion + politics  + (1|position) + (1|issue) + (1|workerid), 
            grname = c("position", "issue", "workerid", "Fixed"), 
            data = data, 
            link = "logit", 
            npermut = 0, nboot = nboot, 
            parallel = TRUE, ncores = ncores,
            adjusted = FALSE)
}

by_mf_baseline <- full_arguments %>% 
  filter(!mf %in% c("lib", "other"), # exclude other argumetns and arguments based on liberty foundation, as it is not part of moral foundation quesionnaire
         politics != 0) %>%  # exlude libertarians
  group_by(mf) %>% 
  nest()
  
# this will run 5 rpt models with nboot = 100 and might take a day
# set nboot = 0 to point estimate ICC without CI
# the models fit is singulare due to the issue random effect being close to 0
icc_baseline <- by_mf_baseline %>% 
  mutate(rpt_model = map(data, estimate_icc_baseline, nboot = 1000))

# full model --------------------------------------------------------------

estimate_icc_full <- function(data, nboot = 1000, ncores = 4){
  rptBinary(arg_chosen ~ opinion + politics  + change_belief + (1|position) + (1|issue) + (1|workerid), 
            grname = c("position", "issue", "workerid", "Fixed"), 
            data = data, 
            link = "logit", 
            npermut = 0, nboot = nboot, 
            parallel = TRUE, ncores = ncores,
            adjusted = FALSE)
}

by_mf_full <- full_arguments %>% 
  filter(!mf %in% c("lib", "other"), # exclude other arguments and arguments based on liberty foundation, as it is not part of moral foundation quesionnaire
         politics != 0, # exlude libertarians
         !is.na(change_belief)) %>%  
  group_by(mf) %>% 
  nest()

# this will run 5 rpt models with nboot = 100 and might take a day
# set nboot = 0 to point estimate ICC without CI
icc_full <- by_mf_full %>% 
              mutate(rpt_model = map(data, estimate_icc_full, nboot = 1000))

icc_baseline %>% 
  mutate(fixed_eff = "baseline") %>% 
  bind_rows(icc_full %>% 
              mutate(fixed_eff = "full")) %>% 
  select(-data) %>% 
  write_rds("data/arguments-agreement-rpt-models.rds", compress = "gz")
