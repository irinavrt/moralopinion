---
title: 'Reproducible code for results in the text of the article "The connection
  between moral position and moral argument drives opinion change"'
output:
  html_document:
    keep_md: TRUE
---




```r
# estimate if default position is more popular among liberals

mf_data <- gss_bin %>% 
  filter(polviews %in% c("liberal", "conservative")) %>% 
  group_by(issue, hf_advantage, lap_advantage,  polviews) %>% 
  summarise(mean_opinion = weighted.mean(opinion, wtssall, na.rm = TRUE)) %>% 
  spread(polviews, mean_opinion) %>% 
  mutate(lib_position = liberal > conservative) %>% 
  select(-liberal, -conservative)
  

# estimate genenaral rate of opinion change 

by_issue <- gss_bin %>% 
  mutate(year_10 = (year - 1972)/10) %>% 
  group_by(issue) %>% 
  nest()

year_est <- function(data) {
  lm(opinion ~ year_10, data, weights = wtssall) %>% 
    tidy(quick = TRUE) %>% 
    filter(term == "year_10") %>% 
    pull(estimate)
}

change <- by_issue %>% 
  mutate(change_rate = map_dbl(data, year_est)) %>% 
  select(-data)

# estimate rate of opinion change separately for liberals and conservatives

by_polv <- gss_bin %>% 
  mutate(year_10 = (year - 1972)/10) %>% 
  filter(polviews %in% c("liberal", "conservative")) %>% 
  group_by(issue, polviews) %>% 
  nest()

polv_change <- by_polv %>% 
  mutate(change_rate = map_dbl(data, year_est)) %>% 
  select(-data) %>% 
  spread(polviews, change_rate) %>% 
  rename(lib_change_rate = liberal,
         cons_change_rate = conservative)

# combine

mf_data <- mf_data %>% 
  left_join(change, by = "issue") %>% 
  left_join(polv_change, by = "issue") %>% 
  ungroup()
```


```r
# liberal liberal positions have larger harm-fairness advantage
hf_adv_lib_side <- (mf_data$hf_advantage > 0 & mf_data$lib_position)|
  (mf_data$hf_advantage < 0 & !mf_data$lib_position)
  
lib_side_prop <- prop.test(rev(table(hf_adv_lib_side)))
lib_side_chi <- chisq.test(mf_data$hf_advantage > 0,  mf_data$lib_position)

# public opinion moves towards positions with larger harm-fairness advantage
hf_adv_trend <- (mf_data$hf_advantage > 0 & mf_data$change_rate > 0)|
  (mf_data$hf_advantage < 0 & mf_data$change_rate < 0)

trend_prop <- prop.test(rev(table(hf_adv_trend)))
trend_chi <- chisq.test(mf_data$hf_advantage > 0, mf_data$change_rate > 0)

# the same is true for liberals

hf_adv_lib_trend <- (mf_data$hf_advantage > 0 & mf_data$lib_change_rate > 0)|
  (mf_data$hf_advantage < 0 & mf_data$lib_change_rate < 0)

lib_trend_prop <- prop.test(rev(table(hf_adv_lib_trend)))
lib_trend_chi <- chisq.test(mf_data$hf_advantage > 0, mf_data$lib_change_rate > 0)

# and conservatives

hf_adv_cons_trend <- (mf_data$hf_advantage > 0 & mf_data$cons_change_rate > 0)|
  (mf_data$hf_advantage < 0 & mf_data$cons_change_rate < 0)

cons_trend_prop <- prop.test(rev(table(hf_adv_cons_trend)))
cons_trend_chi <- chisq.test(mf_data$hf_advantage > 0, mf_data$cons_change_rate > 0)

# correlation between trends among liberals and conservatives

polv_change_cor <- cor.test(mf_data$lib_change_rate, mf_data$cons_change_rate)

# correlation between opinion change rate and harm-fairness advantage

change_vs_hf_adv <- cor.test(mf_data$change_rate, mf_data$hf_advantage)

# the same correlation for 50 issues both coders agreed upon
both_coders <- gss_items$issue[gss_items$coders == 2]
mf_data_50 <- mf_data %>% filter(issue %in% both_coders)
change_vs_hf_adv_50 <- cor.test(mf_data_50$change_rate, mf_data_50$hf_advantage)


# correlation with opinion change among liberals

lib_change_vs_hf_adv <- cor.test(mf_data$lib_change_rate, mf_data$hf_advantage)

# correlation with opinion change among conservatives

cons_change_vs_hf_adv <- cor.test(mf_data$cons_change_rate, mf_data$hf_advantage)

# effect of harm-fairness advantage controlling for binding foundations
mf_data_scaled <- mf_data %>% 
  mutate_at(vars(change_rate, hf_advantage, lap_advantage), funs(scale(.)[,1]))

mod_bind <- lm(change_rate ~ hf_advantage + lap_advantage, mf_data_scaled)

# nest non-in tependent issues in groups 
mf_data_gr <- mf_data_scaled %>% 
  left_join(gss_items %>% select(issue, group)) 


mod_gr <- lmer(change_rate ~ hf_advantage + (1|group), data = mf_data_gr)
set.seed(1)
m_ci <- confint(mod_gr, method="boot", nsim = 1000)
```


# Results
## Testing the predictions on GSS data

**Prediction 1** 

The liberal position has larger harm-fairness connection advantage than conservative position: 64 out of 74 issues. This is a predictive accuracy of 0.86, 95% CI [0.76, 0.93], ($\chi^2$(1) = 37.1, p < 0.001).
 
**Prediction 2** 

Public opinion trended toward the position with harm-fairness advantage for 57 out of 74 issues, an accuracy of 0.77, 95% CI [0.66, 0.86], $\chi^2$(1) = 19.4, p < 0.001. Similar results were obtained when looking only at trends among liberals (0.86, 95% CI [0.76, 0.93], $\chi^2$(1) = 39.7, p < 0.001) and trends among conservatives (0.76, 95% CI [0.64, 0.85], $\chi^2$(1) = 17.4, p < 0.001). The correlation between trends among liberals and conservatives: *r* (72) = 0.90, 95% CI [0.85, 0.94], p < 0.001. 

**Prediction 3**

The correlation between the opinion change rate and the harm-fairness connection advantage:
*r* (72) = 0.72, 95% CI [0.59, 0.82], p < 0.001, (r^2 = 0.52).
The same correlation on a subset of 50 issues that both independent coders judged as moral issues: *r* (48) = 0.73, 95% CI [0.57, 0.84], p < 0.001.


**Robustness of prediction 3**

The correlation between the opinion change rate among liberals and the harm-fairness connection advantage: *r* (72) = 0.74, 95% CI [0.61, 0.83], p < 0.001

The correlation between the opinion change rate among conservatives and the harm-fairness connection advantage: *r* (72) = 0.66, 95% CI [0.51, 0.77], p < 0.001

The effect of harm-fairness advantage, controlling for binding foundations advantage: 
$\beta$ = 0.56, 95% CI [0.34, 0.77], t(71) = 5.1, p < 0.001, whereas there was no positive effect of positions having an advantage with respect to binding foundations ($\beta$ = -0.24, 95% CI [-0.46, -0.03], t(71) = -2.3, p = 0.03).

We also addressed the fact that GSS items cannot necessarily be regarded as independent. We categorised the 74 items into 26 groups based on common themes and averaged the harm-fairness connection advantage within each item group. Hierarchical linear model estimate = 0.77, t (69.9, approximated with Satterthwaite's method) = 9.74 p <0.001, 95% bootstrap confidence interval = [0.61, 0.92].


```r
politics <- mt %>% 
  mutate(polit = cut(politics, c(-1, 0, 3, 4, 7))) %>% 
  count(polit) %>% 
  drop_na() %>% 
  transmute(prop = n/sum(n)) %>% 
  pull(prop) 

politics <- round(politics*100, 1) %>% str_c("%")
```


# Methods
## Mturk survey

In total, we registered 13017 responses with the average respondent answering 30.8 questions. This amounted to 175.9 responses per each of the 74 GSS issues on average. Since each response constitutes a data point, we present the demographics with respect to responses rather than individuals. The mean age of respondents per response was 34.9 years. The share of responses from female respondents was 55.8%. The distribution with respect to political ideology was as follows: 6% responses came from libertarians, 37.4% from liberals, 22.1% from moderates, and 34.5% from conservative respondents. 



```r
lib_cons_cor <- cor.test(mf_measures$lib_hf_adv, mf_measures$cons_hf_adv)
position_cor <- cor.test(mf_measures$default_hf_adv, mf_measures$opposite_hf_adv)
collection_cor <- cor.test(mf_measures$hf_adv_1, mf_measures$hf_adv_2)
```

## Estimation of the harm-fairness connection advantage

The realised range was from -0.48 to +0.48. For instance, the item “Homosexual couples should have the right to marry one another” had a relatively large harm-fairness connection advantage of 0.40, reflecting that arguments in favour of gay marriage tend to be based on fairness, whereas arguments against gay marriage tend to be based on authority and purity.

We checked the reliability of the measure in three ways. As a first check, we calculated the harm-fairness connection advantage separately among liberals and conservatives. These measures were extremely strongly correlated, pearson’s *r* (72) = 0.94, 95% CI [0.91, 0.96], p < 0.001. As a second check, we calculated the harm-fairness connection advantage separately among those who favoured the default position and those who favoured the opposite position. These two measures were also strongly correlated across items,*r* (72) = 0.85, 95% CI [0.78, 0.91], p < 0.001. Finally, we checked that the results are the same in the first and the second part of the data collection, *r* (72) = 0.97, 95% CI [0.96, 0.98], p < 0.001.  Thus, consistent with our analysis of the general agreement on arguments, the measure of harm-fairness connection advantage was not very sensitive to the sample on which we based it.


