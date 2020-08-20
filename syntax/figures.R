library(tidyverse)
library(broom)
library(ggstance)
library(cowplot)
library(ggplotify)

# set font size in figures to 6
theme_set(theme_classic(base_size = 6))
text_size <- 6 * 0.35 # to convert in inches

# load data that is use for several figures -------------------------------

# GSS data with added harm-fairness advantage for each issue.
gss_bin <- read_rds("data/cleaned-gss.rds")


# Figure 1 argument consensus --------------------------------

mf_rpt <- read_rds("data/arguments-agreement-rpt-models.rds")

# ICC point estimate  
mf_R <- mf_rpt %>% 
  mutate(R = map(rpt_model, ~.$R["R_link",])) %>% 
  unnest(R) %>% 
  select(-rpt_model) %>% 
  gather(term, R, -mf, -fixed_eff)

# ICC 95% CI
mf_ci <- mf_rpt %>% 
  mutate(ci = map(rpt_model, ~.$CI_emp$CI_link %>% rownames_to_column(var = "term"))) %>% 
  unnest(ci)


icc_data <- mf_R %>%
  left_join(mf_ci) %>%
  mutate(group = ifelse(term == "Fixed", 2, 1),
         term = factor(term, 
                       levels = c("Fixed", "workerid", "issue", "position"),
                       labels = c("Opinion and\npolitical ideology", "Individual", "Issue",  "Position")),
         mf = factor(mf, 
                     levels = c("harm", "fair", "ingr", "auth", "pure"),
                     labels = c("Harm", "Fairness", "Loyalty", "Authority", "Purity")),
         fixed_eff = factor(fixed_eff, labels = c("baseline model", "with beliefs about opinion change")))

pl <- icc_data %>% 
  ggplot(aes(R, term, xmin = `2.5%`, xmax = `97.5%`, color = fixed_eff)) + 
  geom_linerangeh(position = ggstance::position_dodgev(height = 0.3),
                            size = .2) +
  geom_point(position = position_dodgev(height = 0.3),
             size = .4) +
  geom_vline(xintercept = 0, size = .1, color = "grey70") +
  scale_colour_grey(start = 0.2, end = 0.8)+
  facet_grid(group ~ mf, scales = "free_y", space = "free") +
  labs(x = "proportion of variance", y = NULL, color = NULL) + 
  theme_classic(base_size = 7) +
  theme(strip.background = element_blank(), 
        strip.text.y = element_blank(),
        axis.text.x = element_text(size = 5),
        # panel.grid.major.y = element_blank(),
        # panel.grid.major.x = element_blank(),
        legend.position = "bottom")+
  scale_x_continuous(breaks = c(0, .15, .3), limits = c(-.02, .35))

g <- ggplotGrob(pl)

g[[1]][[23]]$children$axis$grobs[[1]]$children[[1]]$label <- c("Opinion,\npolitical ideology", "and beliefs about\nopinion change")
g[[1]][[23]]$children$axis$grobs[[1]]$children[[1]]$gp$col <- c("grey30", "grey70")
g[[1]][[23]]$children$axis$grobs[[1]]$children[[1]]$vjust <- c(0, 1)
g[[1]][[13]]$children[[1]]$just <- "left"
g[[1]][[23]]$children$axis$grobs[[1]]$children[[1]]$x <- unit(c(1, 1), "npc")
g[[1]][[23]]$children$axis$grobs[[1]]$children[[1]]$y <- unit(c(.51, .45), "npc")


pl <- as.ggplot(g)

pl_upd <- ggdraw(pl, xlim = c(-.02, 1)) + 
  geom_hline(yintercept = .455, size = .2)+
  draw_label("ICC", hjust = 0, x = -.015, y = .92, size = 5)+
  draw_label(expression(marginal ~ r^2), 
             hjust = 0,
             x = -.015, y = .43, size = 5)


ggsave("figures/fig1_argument-applicability.pdf",
       pl_upd,
       width = 160, height = 69, units = "mm")


# Figure 2 How liberal and conservative opinion moves in the model-----------

sim <- read_rds("data/formal-model.rds")

pl <- sim %>% 
  gather(polviews, prop, l, c) %>%
  unite("measure", model, polviews) %>% 
  mutate(measure = factor(measure, 
                          levels = c("a_l", "a_c","b_l", "b_c"),
                          labels = c("Liberals, model A", "Conservatives, model A", 
                                     "Liberals, model B", "Conservatives, model B"))) %>%  
  ggplot(aes(x = t, y = prop, color = measure, linetype = measure))+
  geom_line(size = .5) + 
  labs(x = "Time", y = "Proportion") +
  scale_colour_manual(name = NULL,
                      values = c( "#1a5694", "#c9445a", "#1a5694", "#c9445a")) +   
  scale_linetype_manual(name = NULL,
                        values = c(1, 1, 2, 2))+
  theme(panel.grid.major =  element_blank(), 
        legend.position = c(0.8, .29),
        legend.text = element_text(size = 5))


ggsave("figures/fig2_liberal-and-conservative-movement-in-model.pdf", 
       pl,
       width = 88, height = 65, units = "mm")


# Figure 3 time trends ----------------------------------------------------------------

gss_lib_side <- gss_bin %>% 
  # recode issues so that "yes" position is the position with larger harm-fairness advantage
  mutate(opinion = ifelse(hf_advantage < 0, 1 - opinion, opinion),
         hf_group = ifelse(abs(hf_advantage) > .2, "HF advantage > 0.2", "HF advantage < 0.2"),
         # group variables
         birth_cohorts = cut(birth_year,
                             breaks = c(1883, seq(1910, 1980, by = 10), 1998),
                             labels = c("<1910", 
                                        "1910-20", 
                                        "1920-30", 
                                        "1930-40", 
                                        "1940-50", 
                                        "1950-60", 
                                        "1960-70", 
                                        "1970-80", 
                                        "1980>")),
         education = fct_collapse(degree, 
                                  `College degree` = c("junior college", 
                                                       "bachelor",
                                                       "graduate"),
                                  `Below high school` = "lt high school",
                                  `High school` = "high school"))

# 3a general time trends for high  and low HF advantage -------------------


# model general time trends

hf_trend <- function(data) {
  glm(opinion ~ year, data, weights = wtssall, family = binomial())  
}


by_issue <- gss_lib_side %>% 
  group_by(issue, hf_group) %>% 
  nest()

newdata <- tibble(year = 1972:2016)

trend_pred <- by_issue %>% 
  mutate(model = map(data, hf_trend), 
         predicted_trends = map(model, 
                                ~mutate(newdata, 
                                        pred = predict(.x, newdata, type = "response")))) %>% 
  unnest(predicted_trends)


hf_trends <- trend_pred %>% 
  mutate(hf_group = factor(hf_group, levels = c("HF advantage > 0.2", "HF advantage < 0.2"))) %>% 
  group_by(hf_group, year) %>% 
  summarise(value = mean(pred))

hf_pl <- hf_trends %>% 
  ggplot(aes(year, value, color = hf_group)) +
  geom_line(size = .3) +
  labs(x = "Year", y = "Proportion", color = "HF advantage") +
  scale_color_manual(values = c("#023858", "#2B95CE")) +  
  geom_text(data = . %>% filter(year == max(year)), 
            aes(label = hf_group), 
            hjust = 0,
            nudge_x = 1,
            size = text_size) +
  theme(legend.position = "none") +
  scale_x_continuous(breaks = c(1980, 2000, 2020), limits = c(1972, 2030))+
  ylim(c(0, .9))

# 3b-3c high HF advantage time trends in different groups -----------------

by_issue_high_hf <- by_issue %>% 
  # select issues with high harm-fairness advantage
  filter(hf_group == "HF advantage > 0.2")

# time trends by ideology

polv_trend <- function(data) {
  glm(opinion ~ year*polviews, data, weights = wtssall, family = binomial())  
}

# time trends by education

edu_trend <- function(data) {
  glm(opinion ~ year*education,  data, weights = wtssall, family = binomial())  
}

# time trends by birth cohorts

cohort_trend <- function(data) {
  glm(opinion ~ year*birth_cohorts, data, weights = wtssall, family = binomial())  
}


# polviews were asked in 1974 for the fist time
polv_newdata <- expand.grid(year = 1974:2016,
                            polviews = c("liberal", "conservative"))

edu_newdata <- expand.grid(year = 1972:2016,
                           education = levels(gss_lib_side$education)) 

# only use cohorts that have long or complete year range in the data and select the years within the range
cohort_newdata <-  gss_lib_side %>%
  filter(birth_cohorts %in% c("1920-30", 
                              "1930-40", 
                              "1940-50", 
                              "1950-60", 
                              "1960-70")) %>% 
  select(year, birth_cohorts) %>% 
  distinct()

polv_pred <- by_issue_high_hf %>%
  mutate(model = map(data, ~polv_trend(.x)),
         predicted_trends = map(model, 
                                ~mutate(polv_newdata, 
                                        pred = predict(.x, polv_newdata, type = "response")))) %>% 
  unnest(predicted_trends) %>% 
  mutate(model = "polviews")

edu_pred <- by_issue_high_hf %>% 
  mutate(model = map(data, ~edu_trend(.x)),
         predicted_trends = map(model, 
                                ~mutate(edu_newdata, 
                                        pred = predict(.x, edu_newdata, type = "response")))) %>% 
  unnest(predicted_trends) %>% 
  mutate(model = "education")

cohort_pred <- by_issue_high_hf %>% 
  mutate(model = map(data, ~cohort_trend(.x)),
         predicted_trends = map(model, 
                                ~mutate(cohort_newdata, 
                                        pred = predict(.x, cohort_newdata, type = "response")))) %>% 
  unnest(predicted_trends) %>% 
  mutate(model = "cohorts")

# combine different models and aggregate trens across all variables with high harm-fairness advantage 
pred_for_plot <- polv_pred %>% 
  rename(group = polviews) %>% 
  bind_rows(edu_pred %>% 
              rename(group = education)) %>% 
  bind_rows(cohort_pred %>% 
              rename(group = birth_cohorts)) %>% 
  group_by(year, model, group) %>%
  summarise(value = mean(pred)) %>% 
  ungroup() %>% 
  mutate(group = str_replace_all(group, 
                                 c("liberal" = "Liberals",
                                   "conservative" = "Conservatives"))) 

# create auxiliary data to extend the y-axis for annotating trends with group labels
extend_panel <- expand.grid(model = c("Ideology", "Education", "Birth cohorts"),
                            year = c(1972, 2018),
                            value = c(0, .9),
                            group = "Liberals") %>% 
  mutate(year = case_when(
    year == 2018 & model == "Ideology" ~ 2030,
    year == 2018 & model == "Education" ~ 2038,
    year == 2018 & model == "Birth cohorts" ~ 2024,
    # remove first 2 year, not available for polviews
    year == 1972 & model == "Ideology" ~ 1974, 
    TRUE ~ year
  ))


bottom_row <- pred_for_plot %>%
  mutate(model = factor(model, 
                        levels = c("polviews", "education", "cohorts"),
                        labels = c("Ideology", "Education", "Birth cohorts")),
         group = factor(group, 
                        levels = c("Liberals", 
                                   "Conservatives", 
                                   "Below high school",
                                   "High school",
                                   "College degree",
                                   "1920-30", 
                                   "1930-40", 
                                   "1940-50", 
                                   "1950-60", 
                                   "1960-70"))) %>% 
  ggplot(aes(year, value, color = group)) +
  geom_line(size = .3) +
  labs(x = "Year", y = "Proportion") +
  scale_color_manual(values = c("#1a5694", "#c9445a", #polviews
                                "#004529", "#1A7340", "#34A357", #education
                                "#150E37FF", "#36106BFF", "#5A167EFF", "#7D2382FF", "#A1307EFF")) + # cohorts
  geom_text(data = . %>% 
              filter(year == max(year)) %>% 
              mutate(value = ifelse(group == "1960-70", value + .04, value),
                     value = ifelse(group == "1940-50", value - .01, value)),
            aes(label = group),
            hjust = 0,
            nudge_y = .01,
            nudge_x = 1,
            size = text_size) +
  geom_blank(data = extend_panel) +
  scale_x_continuous(breaks=c(1980, 2000, 2020))+
  theme(legend.position = "none",
        strip.background = element_blank())+
  facet_wrap(~model, scales = "free", shrink = TRUE)

bottom_row

# combine and annotate panels
top_row <- plot_grid(hf_pl, NULL, rel_widths = c(1, 1))

bottom_row <- ggdraw(bottom_row) + 
  draw_text(c("b", "c", "d"), 
            x = c(.02, .35, .67), 
            y = .97, 
            size = 7,
            fontface = "bold")


final_pl <- plot_grid(top_row, bottom_row, 
                      labels = c('a', ''), 
                      nrow = 2, 
                      label_size = 7,
                      hjust = -1,
                      vjust = 2)

ggsave("figures/fig3_opinion-change-hf-advantage.pdf", 
       final_pl,
       width = 180, height = 120, units = "mm")

# figure 4 main correlation -----------------------------------------------
# The correlation between harm-fairness connection advantage and change in public opinion

# estimate linear trends

by_issue <- gss_bin %>% 
  mutate(year_10 = (year - 1972)/10) %>% 
  group_by(issue, hf_advantage) %>% 
  nest()

year_est <- function(data) {
  lm(opinion ~ year_10, data, weights = wtssall) %>% 
    tidy(quick = TRUE) %>% 
    filter(term == "year_10") %>% 
    pull(estimate)
}

mf_adv_vs_change <- by_issue %>% 
  mutate(change_rate = map_dbl(data, year_est)) %>% 
  select(-data)

pl <- mf_adv_vs_change %>% 
  # in percentage
  mutate(change_perc = change_rate*100) %>% 
  ggplot(aes(hf_advantage, change_perc))+
  geom_point(size = 1.5, shape = 19, color = "#12213B", alpha = .5) +
  geom_smooth(size = .7, method="lm", fullrange = TRUE, color = "grey10")+
  #  geom_text(label = eq , x = .42, y = -15, size = text_size, parse = TRUE)+
  labs(x = "Harm-fairness connection advantage", y = "Public opinion change")

ggsave("figures/fig4_correlation-bw-harm-fairness-advantage-and-change.pdf",
       pl,
       width = 88, height = 65, units = "mm")




