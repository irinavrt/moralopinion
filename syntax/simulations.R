library(tidyverse)
library(furrr)

# Formal model ------------------------------------------------------------

# setup
# see section "Methods: Deriving predictions from the model" in the paper for explanaiton
# see figure 2 for results

# Model A

bf <- .5
ba <- .1 # varied parameter
Ll <- .5
Lc <- .5
s <- .5

model_a <- tibble(model = "a",
                  l = .01,
                  c = .01,
                  t = 1:100) 

for(i in 2:nrow(model_a)){
  model_a$l[i] <- model_a$l[i-1] + ((1 - model_a$l[i-1])*(Ll*model_a$l[i-1] + (1 - Ll)*model_a$c[i-1])*bf - 
    model_a$l[i-1]*(Ll*(1 - model_a$l[i-1]) + (1 - Ll)*(1 - model_a$c[i-1]))*ba)
  model_a$c[i] <-  model_a$c[i-1] + s*Lc*(model_a$l[i-1] - model_a$c[i-1])
}

# Model B

ba <- .35

model_b <- tibble(model = "b",
                  l = .01,
                  c = .01,
                  t = 1:100) 

for(i in 2:nrow(model_b)){
  model_b$l[i] <- model_b$l[i-1] + ((1 - model_b$l[i-1])*(Ll*model_b$l[i-1] + (1 - Ll)*model_b$c[i-1])*bf - 
    model_b$l[i-1]*(Ll*(1 - model_b$l[i-1]) + (1 - Ll)*(1 - model_b$c[i-1]))*ba)
  model_b$c[i] <-  model_b$c[i-1] + s*Lc*(model_b$l[i-1] - model_b$c[i-1])
}

bind_rows(model_a, model_b) %>% 
  write_rds("data/formal-model.rds")


# agent based model functions ---------------------------------------------
# see description in Supplementary methods 1

# update position funtion
# devalues arguments that did not work with r rate
update_postion <- function(p, a, r, death_rate, df, 
                           univ_arg_for, univ_arg_against,
                           for_arg, against_arg){
  df$for_opinion_new <- NA_real_
  # death process, replace n*death_rate agents with new agents that have not heard arguments 
  df[sample(rownames(df), nrow(df)*death_rate), c(for_arg, against_arg)] <- 0
  # for each agent
  for(i in 1:nrow(df)){ 
    if(df$liberal[i] == 0){ # if agent is conservative
      if(df$for_opinion[i] == 0){ # if agent holds agains position
        exposed <- rbinom(1, 1, p) # generate if expose based on p = prop of holding liberal position
        if(exposed){
          chosen <- sample(1:length(univ_arg_for), 1) # chose argument 
          p_upd <- a*(r^df[[i,str_c("f", chosen)]]) # update prob of being swade a with deterioration rate^n times arg was heard
          df$for_opinion_new[i] <- rbinom(1, 1, p_upd) # generate new opinion
          if(df$for_opinion_new[i] == 0){ # if argument was not accepted
            df[[i,str_c("f", chosen)]] <- df[[i,str_c("f", chosen)]] + 1 # update how many times argument was heard
          }
        } else {
          df$for_opinion_new[i] <- df$for_opinion[i]
        }
      } else { # if agent holds liberal position for_opinion == 1
        exposed <- rbinom(1, 1, 1-p) # generate if expose based on 1-p = prop of holding conservative position
        if(exposed){
          chosen <- sample(1:length(univ_arg_against), 1) # chose argument
          # update prob of being swade a with deterioration rate^n times arg was heard
          p_upd <- a*(r^df[[i,str_c("a", chosen)]])
          df$for_opinion_new[i] <- rbinom(1, 1, (1-p_upd)) # generate new opinion with prob = not to change (stay 1)
          if(df$for_opinion_new[i] == 1){ # if argument was not accepted
            df[[i,str_c("a", chosen)]] <- df[[i,str_c("a", chosen)]] + 1 # update how many times argument was heard
          }
        }else {
          df$for_opinion_new[i] <- df$for_opinion[i]
        }
      }
    }else{ # if agent is liberal
      if(df$for_opinion[i] == 0){ # if agent holds conservative position
        exposed <- rbinom(1, 1, p) # generate if exposed based on p = prop of holding liberal position
        if(exposed){
          chosen <- sample(1:length(univ_arg_for), 1) # chose argument 
          # update prob of being swade a with deterioration rate^n times arg was heard
          # and if the argument is liberal
          p_upd <- a*(r^df[[i,str_c("f", chosen)]])*univ_arg_for[chosen] 
          df$for_opinion_new[i] <- rbinom(1, 1, p_upd) # generate new opinion
          if(df$for_opinion_new[i] == 0){ # if argument was not accepted
            df[[i,str_c("f", chosen)]] <- df[[i,str_c("f", chosen)]] + 1 # update how many times argument was heard
          }
        } else {
          df$for_opinion_new[i] <- df$for_opinion[i]
        }
      } else { # if agent holds liberal position for_opinion == 1
        exposed <- rbinom(1, 1, 1-p) # generate if expose based on p = prop of holding conservative position
        if(exposed){
          chosen <- sample(1:length(univ_arg_against), 1) # chose argument
          # update prob of being swade a with deterioration rate^n times arg was heard
          # and if the argument is liberal
          p_upd <- a*(r^df[[i,str_c("a", chosen)]])*univ_arg_against[chosen]
          df$for_opinion_new[i] <- rbinom(1, 1, (1-p_upd)) # generate new opinion with prob = not to change (stay 1)
          if(df$for_opinion_new[i] == 1){ # if argument was not accepted
            df[[i,str_c("a", chosen)]] <- df[[i,str_c("a", chosen)]] + 1 # update how many times argument was heard
          }
        }else {
          df$for_opinion_new[i] <- df$for_opinion[i]
        }
      }
    }
  }
  df %>% mutate(for_opinion = for_opinion_new) %>% select(-for_opinion_new)
}


run_sim <- function(pf, pa, n, a, r, Ll, 
                    death_rate, data, init_prop){
  # agents
  initial_agents <- tibble(liberal = rbinom(1000, size = 1, prob = .50),
                           for_opinion = rbinom(1000, size = 1, prob = init_prop))
  # arguments the agents listen to
  for_arg <- str_c("f", 1:n)
  against_arg <- str_c("a", 1:n)
  initial_agents[, c(for_arg, against_arg)] <- 0
  
  time_dinamic <- data %>% 
    mutate(p_cons = NA_real_, p_lib = NA_real_) 
  
  univ_arg_for <- rep(0, n)  
  univ_arg_for[1:(n*pf)] <- 1
  
  univ_arg_against <- rep(0, n)  
  univ_arg_against[1:(n*pa)] <- 1
  
  # agregate opinion at time 0  
  time_dinamic[1, c("p_cons", "p_lib")] <- initial_agents %>% 
    group_by(liberal) %>% 
    summarise(p = mean(for_opinion)) %>% 
    pull(p)
  
  for(i in 2:nrow(time_dinamic)){
    
    p <- Ll*time_dinamic$p_lib[i-1] + (1 - Ll)*time_dinamic$p_cons[i-1]
    
    initial_agents <- update_postion(p, a, r, 
                                     death_rate,
                                     initial_agents,
                                     univ_arg_for, 
                                     univ_arg_against,
                                     for_arg, against_arg)
    
    time_dinamic[i, c("p_cons", "p_lib")] <- initial_agents %>% 
      group_by(liberal) %>% 
      summarise(p = mean(for_opinion)) %>% 
      pull(p)
  }
  time_dinamic
}

# argument deterioration model --------------------------------------------

# see description in Supplementary methods 1

# proporition of universal arguments
pf <- 1
#pa <- c(.2, .7) # parameter to vary
# probability of being convinced by an argument one listens to
# multiplied with pf and pa, this results in parametes of main model bf = .5, ba = c(.1, .35) and s = .5
a <- .5
# argument deteriation rate 
r <- .5
# prob to encounter liberal 
Ll <- .5
# rounds
t <- 200
# number of arguments
n <- 10
death_rate <- 0

init_prop <- .02

parameters <- expand.grid(pf = pf, 
                               pa = .2, 
                               a = a, 
                               r = .5, 
                               Ll = Ll, 
                               death_rate = death_rate,
                               n = n,
                               init_prop = init_prop,
                               t = 0:t) %>% 
  bind_rows(expand.grid(pf = pf, 
                        pa = .6, 
                        a = a, 
                        r = .5, 
                        Ll = Ll, 
                        death_rate = death_rate,
                        n = n,
                        init_prop = init_prop,
                        # less hf advantage takes longer to converge
                        t = 0:1000)) %>% 
  as_tibble()

by_varied_param <- parameters %>% 
  group_by(pf, pa, a, r, Ll, death_rate, n, init_prop) %>% 
  nest()

# 100 simulations for each model
by_varied_param <- map_df(seq_len(100), ~by_varied_param)

# parallel processing
plan(multiprocess)
by_varied_upd <- by_varied_param %>%  
              mutate(sim = future_pmap(., run_sim, .progress = TRUE))

full_sim <- by_varied_upd %>%
  unnest(sim)

aggr_sim <- full_sim %>% 
  group_by(pa, t) %>% 
  summarise(p_cons = mean(p_cons),
            p_lib = mean(p_lib)) %>% 
  ungroup()

# check for convergence
# check if change rate below a threshold
aggr_sim <- aggr_sim %>% 
  mutate(cons_change = p_cons - lag(p_cons, default = 0),
         lib_change = p_lib - lag(p_lib, default = 0),
         small = abs(cons_change) < .0005 & abs(lib_change) < .0005,
         step = 0) 

for(i in 2:nrow(aggr_sim)){
  aggr_sim$step[i] <- ifelse(aggr_sim$small[i] & aggr_sim$small[i-1],
                             aggr_sim$step[i - 1] + 1,
                             0)
}

write_rds(aggr_sim, "data/sim-argument-deterioration.rds")

# introducing birth and death model ---------------------------------------

death_rate <- c(.01, .03, .05)
pa <- c(.2, .6)

parameters <- expand.grid(pf = pf, 
                          pa = pa, 
                          a = a, 
                          r = 0, 
                          Ll = Ll, 
                          death_rate = death_rate,
                          n = n,
                          init_prop = init_prop,
                          t = 0:t) %>% as_tibble()

by_varied_param <- parameters %>% 
  group_by(pf, pa, a, r, Ll, death_rate, n, init_prop) %>% 
  nest()

# run 100 simulations
by_varied_param <- map_df(seq_len(100), ~by_varied_param)

plan(multiprocess)
by_varied_upd <- by_varied_param %>%  
  mutate(sim = future_pmap(., run_sim, .progress = TRUE))

full_sim <- by_varied_upd %>% 
  unnest(sim)  

aggr_sim <- full_sim %>% 
  group_by(pa, death_rate, t) %>% 
  summarise(p_cons = mean(p_cons),
            p_lib = mean(p_lib)) %>% 
  ungroup()

write_rds(aggr_sim, "data/sim-death-process.rds")


