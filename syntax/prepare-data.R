# This script used to prepare both argument data collected with Mturk and GSS data.
# Output of this file is used in
# - modelling in argument-agreement.R
# - display of results in figures.R
# - results reported in text in results-in-text.Rmd
# - supplementary results in supplement.Rmd
# 
# Raw GSS date used in this script is not provided in the repository 
# and has to be downloaded from http://gss.norc.org/get-the-data/spss

library(tidyverse) 
library(haven)

# list of 74 moral issues from GSS with questions and variable that notes if the issue was selected by one or both coders
gss_items <- read_csv("data/gss-items.csv")

source("auxiliary_functions.R")

# mturk data --------------------------------------------------------------

mt <- read_csv("data/mturk-agrument-data.csv", guess_max = 10000)
mt_demogr <- read_csv("data/mturk-participants.csv")

# add gss items names and select relevant variable
mt <- mt %>%
  right_join(gss_items, by = c("Input.question" = "question")) %>% 
  select(issue,
         question = Input.question,
         answer = Answer.answer,
         arg = Answer.arg,
         counter_arg = Answer.counterarg,
         workerid = WorkerId, 
         change_belief = Answer.opinion,
         collection) %>% 
  left_join(mt_demogr %>% 
              select(workerid = WorkerId, 
                     age = Answer.Age, 
                     gender = Answer.Gender, 
                     politics = Answer.Politics,
                     party = Answer.Party,
                     ethnic = Answer.ethnic,
                     origin = Answer.origin))

# save individual responses data for demographics description in the paper
write_rds(mt, "data/mturk-responses.rds")

# reformat arguments in long format 
full_arguments <- mt %>% 
  separate(arg, str_c("arg.", 1:7), fill = "right") %>% 
  separate(counter_arg, str_c("counter_arg.", 1:7), fill = "right") %>% 
  gather(number, mf, arg.1:counter_arg.7, na.rm = TRUE) %>% 
  separate(number, c("type", "number"), sep = "\\.") %>% 
  select(-number, -age, -gender, -party:-origin) %>% 
  mutate(mf = factor(mf, 
                     labels = c("harm", "fair", "ingr", "auth", "pure", "lib", "other" )),
         arg_chosen = 1) %>% 
  spread(mf, arg_chosen, fill = 0) %>% 
  gather(mf, arg_chosen, harm:other) %>% 
  spread(type, arg_chosen) %>% 
  mutate(pro = ifelse(answer == 1, arg, counter_arg),
         against = ifelse(answer == 1, counter_arg, arg))  %>% 
  select(-arg, -counter_arg) %>% 
  gather(type, arg_chosen, pro, against) %>% 
  mutate(opinion = ifelse(type == "pro", answer, 1 - answer),
         change_belief = ifelse(type == "pro", change_belief, -change_belief),
         position = str_c(issue, type, sep = "_")) %>% 
  filter(!mf %in% c("lib", "other"))

write_rds(full_arguments, "data/cleaned-arguments.rds")


# Estimate harm-fairness advantage ----------------------------------------

mf_measures <- full_arguments %>% 
  group_by(issue, mf, type) %>% 
  summarise(prop = mean(arg_chosen, na.rm = TRUE)) %>% 
  spread(type, prop) %>% 
  mutate(mf_advantage = pro - against) %>% 
  select(-pro, -against) %>% 
  spread(mf, mf_advantage) %>%
  mutate(hf_advantage = (harm + fair)/2, 
         lap_advantage = (ingr + auth + pure)/3)

# hf_advantage estimated separately among liberals and conservatives
mf_polv <- full_arguments %>% 
  filter(!mf %in% c("lib", "other")) %>% 
  mutate(polviews = case_when(
    between(politics, 1, 3) ~ "liberal",
    between(politics, 5, 7) ~ "conservative",
    TRUE ~ NA_character_
  )) %>% 
  drop_na(polviews) %>% 
  group_by(issue, mf, type, polviews) %>% 
  summarise(prop = mean(arg_chosen, na.rm = TRUE)) %>% 
  spread(type, prop) %>% 
  mutate(mf_advantage = pro - against) %>% 
  select(-pro, -against) %>% 
  spread(mf, mf_advantage) %>%
  mutate(hf_adv = (harm + fair)/2) %>% 
  select(issue, polviews, hf_adv) %>% 
  spread(polviews, hf_adv) %>% 
  rename(lib_hf_adv = liberal, cons_hf_adv = conservative)

# hf_advantage estimated separately among those who hold the default position, or the opposite position
mf_position <- full_arguments %>% 
  filter(!mf %in% c("lib", "other")) %>% 
  mutate(answer = factor(answer, labels = c("opposite", "default"))) %>% 
  group_by(issue,  mf, type, answer) %>% 
  summarise(prop = mean(arg_chosen, na.rm = TRUE)) %>% 
  spread(type, prop) %>% 
  mutate(mf_advantage = pro - against) %>% 
  select(-pro, -against) %>% 
  spread(mf, mf_advantage) %>%
  mutate(hf_adv = (harm + fair)/2) %>% 
  select(issue, answer, hf_adv) %>% 
  spread(answer, hf_adv) %>% 
  rename(opposite_hf_adv = opposite, default_hf_adv = default)

# hf_advantage estimated separately in 2 data collections
mf_collection <- full_arguments %>% 
  filter(!mf %in% c("lib", "other")) %>% 
  group_by(issue,  mf, type, collection) %>% 
  summarise(prop = mean(arg_chosen, na.rm = TRUE)) %>% 
  spread(type, prop) %>% 
  mutate(mf_advantage = pro - against) %>% 
  select(-pro, -against) %>% 
  spread(mf, mf_advantage) %>%
  mutate(hf_adv = (harm + fair)/2) %>% 
  select(issue, collection, hf_adv) %>% 
  spread(collection, hf_adv) %>% 
  rename(hf_adv_1 = `1`, hf_adv_2 = `2`)

mf_measures %>% 
  left_join(mf_polv) %>% 
  left_join(mf_position) %>% 
  left_join(mf_collection) %>% 
  write_rds("data/mf_measures.rds")

# GSS data ----------------------------------------------------------------

# The latest version of GSS data can be downloaded from http://gss.norc.org/get-the-data/spss
# We use here 1972-2016 Cross-Sectional Cumulative Data 
gss_full <- read_sav("data/GSS7216_R4.sav") # large file, requires time and 3 GB RAM 

gss_full <- gss_full %>% 
  rename_all(tolower)

issues <- gss_items$issue
other_vars <- c("year","wtssall", "sample", "sex","age", "degree", 
                "race","class", "region", "partyid", "polviews")

gss <- gss_full %>% 
  select(one_of(c(other_vars, issues)))

gss <- gss %>% 
  mutate_at(vars(age, year, polviews, wtssall), zap_labels) %>% 
  mutate_if(is.labelled, ~fct_relabel(as_factor(.), tolower)) %>% 
  mutate(birth_year = year - age,
         polviews_cont = polviews,
         polviews = cut(polviews, c(0, 3, 4, 7), 
                        labels = c("liberal", "moderate", "conservative")),
         time = (year - 1972)/10) %>% 
  ungroup() 

# Exclude oversamle of blacks in 1982 and 1987
# see GSS documentation on weigting http://gss.norc.org/documents/codebook/GSS_Codebook_AppendixA.pdf
gss <- gss %>% 
  filter(!str_detect(sample, "oversamp"))

# recode neutral levels that are not in the middle of factor levels to NA
gss <- gss %>% 
  mutate(class = fct_recode(class, NULL = "no class"),
         homosex = fct_recode(homosex, NULL = "other"),
         racchng = fct_recode(racchng, NULL = "wdnt belong"),
         racopen = fct_recode(racopen, NULL = "neither"),
         sexeduc = fct_recode(sexeduc, NULL = "depends")) 

# exclude data for racial items in years where racial questions were asked only to non-blacks 
for(i in c("racmar","racpush", "racopen")) {
  gss[gss$year %in% 1972:1977,i] <- NA
}

gss <- gss %>% droplevels()

# recode issues to binary with 1 indicating agreement to the default position 
# if relevant, the neutral middle category is omited

gss_bin <- gss %>% 
  mutate(pornlaw = ifelse(pornlaw == "legal", 0 , 1)) %>% 
  mutate_at(issues[issues != "pornlaw"], dichotomize)


# change into long format with 74 copies of GSS, one per each issue
# add harm-fainess advantage

gss_bin <- gss_bin %>% 
  gather(issue, opinion, one_of(issues)) %>% 
  left_join(mf_measures %>% select(issue, hf_advantage, lap_advantage))

write_rds(gss_bin, "data/cleaned-gss.rds", compress = "gz")

