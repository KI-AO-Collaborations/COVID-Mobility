library(tidyverse)
library(estimatr)
library(haven)
library(cowplot)
library(lubridate)

setwd("G:/Shared drives/Econ21320/data/clean/")


# Clean Policy Controls ---------------------------------------------------
policy = read_csv("../raw/state_policy.csv")

# Remove irrelevant coluns
dstart = mdy("01/22/2020")
dend = mdy("04/22/2020")
policy = select(policy, -location_id, -StateFIPS, -StatePostal, 
                -PolicyCodingNotes, -PolicySource, -LastUpdated, -LastUpdatedNotes) %>%
  rename(State = StateName) %>%
  mutate(DateIssued = ymd(DateIssued),
         DateEnacted = ymd(DateEnacted))
get_enactdate <- function(policyname, statename)
{
  df = filter(policy, StatePolicy == policyname, Mandate == 1, State == statename) %>%
    select(DateEnacted) %>%
    summarise(DateEnacted = min(DateEnacted, na.rm = TRUE))
  if(df[[1]] == Inf)
    return(-99)
  else
    return(df[[1]])
}
make_panel <- function(enactdate, dstart, dend)
{
  if(enactdate == -99)
  {
    tibble(Date = seq.Date(dstart, dend, by = "day"),
           Implemented = 0)
  }
  else
  {
    tibble(Date = seq.Date(dstart, dend, by = "day"),
           Implemented = ifelse(Date < enactdate, 0, 1))
  }
}
state_panel <- function(policyname, statename)
{
  panel = make_panel(get_enactdate(policyname, statename), dstart, dend)
  mutate(panel,
         State = statename,
         Policy = policyname)
}
states_panel <- function(policyname)
{
  map(.x = distinct(policy, State)[[1]],
      .f = {function(state) state_panel(policyname, state)}) %>%
    bind_rows()
}

panel = map(.x = distinct(policy, StatePolicy)[[1]],
            .f = states_panel) %>%
  bind_rows() %>%
  pivot_wider(names_from = Policy, values_from = Implemented) %>%
  select(-"NA")

write_dta(panel, "policy_panel.dta")

# First Stage -------------------------------------------------------------

alldata = read_dta("rain_merged_weekly.dta") %>%
  filter(!(state %in% c("Puerto Rico", "American Samoa", "Diamond Princess",
                      "Grand Princess", "Virgin Islands"))) %>%
  group_by(state) %>%
  arrange(state, date) %>%
  mutate(d_avg_precip = avg_precip - lag(avg_precip),
         d_una_distance = una_distance - lag(una_distance),
         d_dl_mobility = dl_mobility - lag(dl_mobility),
         d_una_visitation = una_visitation - lag(una_visitation),
         d_twitter_social_distancing = twitter_social_distancing - lag(twitter_social_distancing),
         covid_mortality_pc = COVID_mortality / population,
         covid_mortality_pc_l1 = lag(covid_mortality_pc),
         d_covid_mortality_pc = covid_mortality_pc - covid_mortality_pc_l1,
         d_covid_mortality_pc_l1 = lag(d_covid_mortality_pc),
         d_covid_mortality_pc_l2 = lag(d_covid_mortality_pc_l1),
         d_covid_mortality_pc_l3 = lag(d_covid_mortality_pc_l2),
         d_covid_mortality_pc_l4 = lag(d_covid_mortality_pc_l3),
         d_covid_mortality_pc_l5 = lag(d_covid_mortality_pc_l4))

filter(ungroup(alldata), !is.na(una_distance)) %>%
  summarise(min(date))

# residualize first stage to policy implementation and state fixed effects
make_resid <- function(distvar)
{
  lhs = paste0("d_", distvar, "~")
  rhs = "GathRestrictAny + EmergDec + StayAtHome + d_covid_mortality_pc_l1 + as.factor(state_id) +
         as.factor(state_id):week + as.factor(state_id):I(week^2)"
  formula = as.formula(paste0(lhs, rhs))
  distance_model = lm(formula,
                      data = alldata) %>%
    augment() %>%
    select(.rownames, .resid)
  names(distance_model) = c(".rownames", paste0(".", distvar, "_resid"))
  return(distance_model)
}
una_dist_resid = make_resid("una_distance")
una_visit_resid = make_resid("una_visitation")
dl_resid = make_resid("dl_mobility")
twitter_resid = make_resid("twitter_social_distancing")
rain_resid = make_resid("avg_precip")


regdata = full_join(rain_resid, una_dist_resid) %>%
  full_join(una_visit_resid) %>%
  full_join(dl_resid) %>%
  full_join(twitter_resid)

# First Stage Plots
p_twitter = ggplot(regdata, aes(x = .avg_precip_resid, 
                                y = .twitter_social_distancing_resid)) +
  geom_point() +
  geom_smooth(method = lm, formula = y ~ x) +
  labs(x = "Average Precipitation Change", y = "Twitter Social Distancing Measure Change",
       title = "Twitter Data") +
  theme_bw()
p_una_visit = ggplot(regdata, aes(x = .avg_precip_resid, 
                                  y = .una_visitation_resid)) +
  geom_point() + 
  geom_smooth(method = lm) +
  labs(x = "Average Precipitation Change", y = "Unacast Visitation Measure Change",
       title = "Unacast Visitation") +
  theme_bw()
p_una_distance = ggplot(regdata, aes(x = .avg_precip_resid, 
                                     y = .una_distance_resid)) +
  geom_point() + 
  geom_smooth(method = lm) +
  labs(x = "Average Precipitation Change", y = "Unacast Distance Measure Chhange",
       title = "Unacast Distance") +
  theme_bw() +
  xlim(-0.45, 0.45)
p_dl = ggplot(regdata, aes(x = .avg_precip_resid, 
                           y = .dl_mobility_resid)) +
  geom_point() +
  geom_smooth(method = lm) +
  labs(x = "Average Precipitation Change", y = "Descartes Labs Measure Change",
       title = "Descartes Labs") +
  theme_bw() +
  xlim(-0.45, 0.45)
p_firststage = plot_grid(p_dl, p_una_distance)


# Regression First Stages
firststage_una_distance = lm_robust(d_una_distance ~ d_avg_precip +
                                      GathRestrictAny + EmergDec + 
                                      StayAtHome + d_covid_mortality_pc_l1,
                                    data = alldata,
                                    fixed_effects = ~state_id,
                                    clusters = state_id,
                                    se_type = "stata")
# firststage_una_visit = lm_robust(d_una_visitation ~ d_avg_precip, +
#                                    GathRestrictAny + EmergDec + StayAtHome,
#                                  data = alldata,
#                                  fixed_effects = ~state_id,
#                                  clusters = state_id,
#                                  se_type = "stata")
firststage_twitter = lm_robust(d_twitter_social_distancing ~ d_avg_precip +
                                 GathRestrictAny + EmergDec + StayAtHome +
                                 d_covid_mortality_pc,
                               data = alldata,
                               fixed_effects = ~state_id,
                               clusters = state_id,
                               se_type = "stata")
firststage_dl = lm_robust(d_dl_mobility ~ d_avg_precip +
                            GathRestrictAny + EmergDec + StayAtHome +
                            d_covid_mortality_pc,
                          data = alldata,
                          fixed_effects = ~state_id,
                          clusters = state_id,
                          se_type = "stata")


p_measures = ggplot(alldata, aes(x = dl_mobility, y = una_distance)) +
  geom_point() +
  geom_smooth(method = lm) +
  theme_bw() +
  labs(x = "Descartes Lab Measure",
       y = "Unacast Distance Measure",
       title = "Social Distancing Measure Comparison")

p_measures = ggpairs(select(ungroup(alldata), dl_mobility, una_distance,
                            twitter_social_distancing) %>%
                       rename(Twitter = twitter_social_distancing,
                              Descartes = dl_mobility,
                              Unacast = una_distance)) +
  labs(title = "Correlation Plot Between Distance Measures") +
  theme_bw()

# Time Series in Texas
texasdata = filter(alldata, state == "Texas") %>%
  arrange(date)


datecol = select(ungroup(texasdata), date) %>%
  mutate(.rownames = 1:nrow(texasdata),
         .rownames = as.character(.rownames))
texas_precip = lm(d_avg_precip ~ GathRestrictAny + EmergDec + StayAtHome + d_covid_mortality_pc_l1,
               data = texasdata) %>%
  augment() %>%
  select(.rownames, .resid) %>%
  rename(.avg_precip_resid = .resid)
texas_mob = lm(d_dl_mobility ~ GathRestrictAny + EmergDec + StayAtHome + d_covid_mortality_pc_l1,
               data = texasdata) %>%
  augment() %>%
  select(.rownames, .resid) %>%
  rename(.dl_mobility_resid = .resid)
texas_resid = full_join(datecol, texas_precip) %>%
  full_join(texas_mob)

val_plot = ggplot(texasdata, aes(x = date)) +
  geom_line(aes(y = d_avg_precip, col = "Precipitation Change"), size = 1) +
  geom_point(aes(y = d_avg_precip, col = "Precipitation Change")) +
  geom_line(aes(y = d_dl_mobility, col = "Mobility Change"), size = 1) +
  geom_point(aes(y = d_dl_mobility, col = "Mobility Change")) +
  theme_bw() +
  labs(x = "Date", y = "Value", title = "Raw Data") +
  theme(legend.position = "bottom")

resid_plot = ggplot(texas_resid, aes(x = date)) +
  geom_line(aes(y = .avg_precip_resid, col = "Precipitation Change"), size = 1) +
  geom_point(aes(y = .avg_precip_resid, col = "Precipitation Change")) +
  geom_line(aes(y = .dl_mobility_resid, col = "Mobility Change"), size = 1) +
  geom_point(aes(y = .dl_mobility_resid, col = "Mobility Change")) +
  theme_bw() +
  labs(x = "Date", y = "Value", title = "Residualized Data") +
  theme(legend.position = "bottom")

plot_grid(val_plot, resid_plot)



# SumStats ----------------------------------------------------------------
texasdata_sum = filter(alldata, state == "Texas")
restrictdate = tibble(date = mdy("03/18/2020"))
stayathome = tibble(date = mdy("04/01/2020"))

ggplot(texasdata_sum, aes(x = date)) +
  geom_line(aes(y = d_covid_mortality_pc*100000, col = "Change in Mortality per 100k"),
            size = 1) +
  geom_line(aes(y = d_avg_precip, col = "Change in Rainfall"), size = 1) +
  geom_line(aes(y = d_una_distance, col = "Change in Unacast Mobility"),
            size = 1) +
  theme_bw() +
  labs(x = "Date", y = "Value") +
  geom_vline(data = restrictdate, aes(xintercept = date,col = "Gather Restriction, Emergency Declaration"), 
             linetype = "dashed") +
  geom_vline(data = stayathome, aes(xintercept = date, col = "Stay At Home"),
             linetype = "dashed") +
  guides(linetype = FALSE)

# summarise(alldata, mean(d_avg_precip, na.rm = TRUE), mean(d_una_distance, na.rm = TRUE), mean(d_dl_mobility, na.rm = TRUE), mean(COVID_mortality, na.rm = TRUE))


# Anticipation ------------------------------------------------------------
daily_data = read_dta("rain_merged_daily.dta") %>%
  filter(state == "Texas") %>%
  select(date, dl_mobility, una_distance, GathRestrictAny, StayAtHome, EmergDec) %>%
  mutate(d_dist = una_distance - lag(una_distance),
         d_dl = dl_mobility - lag(dl_mobility)) %>%
  filter(date >= mdy("02/14/2020"))

policydates = tibble(date = c(mdy("03/21/2020"), mdy("04/02/2020"), mdy("03/13/2020")),
                     policy = c("Gather Restriction", "Stay At Home", "Emergency Declaration"))

ggplot(daily_data, aes(x = date)) +
  geom_line(aes(y = una_distance, col = "Unacast Mobility"), size = 1) +
  geom_vline(data = policydates, aes(xintercept = date, col = policy),
             linetype = "dashed") +
  theme_bw() +
  labs(x = "Date", y = "Unacast Mobility Index")
  



# Lasso Estimation --------------------------------------------------------

# Step 1: Projection onto the instruments
dl_hat = lm(d_dl_mobility ~ d_avg_precip + EmergDec + GathRestrictAny  +
              #d_covid_mortality_pc_l1 +
              as.factor(state_id) + as.factor(state_id):week + 
              as.factor(state_id):I(week^2),
            data = alldata) %>%
  augment() %>%
  select(.rownames, .fitted) %>%
  rename(d_dl_mobility_hat = .fitted)
alldata = mutate(ungroup(alldata), 
                 .rownames = 1:nrow(alldata),
                 .rownames = as.character(.rownames))
alldata = full_join(alldata, dl_hat)

# Step 2: Residualize from the controls
dl_hat_resid = lm(d_dl_mobility_hat ~ EmergDec + GathRestrictAny + 
                    #d_covid_mortality_pc_l1 + d_covid_mortality_pc_l2 + d_covid_mortality_pc_l3 +
                    #d_covid_mortality_pc_l4 + d_covid_mortality_pc_l5 + 
                    as.factor(state) + as.factor(state):week +
                  as.factor(state):I(week^2),
                  data = alldata) %>%
  augment() %>%
  select(.rownames, .resid) %>%
  rename(dl_hat_resid = .resid)

alldata = full_join(alldata, dl_hat_resid) %>%
  group_by(state) %>%
  arrange(state, date) %>%
  mutate(dl_hat_resid_l1 = lag(dl_hat_resid),
         dl_hat_resid_l2 = lag(dl_hat_resid, 2),
         dl_hat_resid_l3 = lag(dl_hat_resid, 3),
         dl_hat_resid_l4 = lag(dl_hat_resid, 4))

# Step 2: Lasso Estimation of number of lags
library(glmnet)

# select the relevant data for the lasso
# de-mean the y
# standardize the x
lassodata = select(ungroup(alldata), dl_hat_resid_l1, dl_hat_resid_l2, 
                   dl_hat_resid_l3, dl_hat_resid_l4, covid_mortality_pc) %>%
  filter(!is.na(dl_hat_resid_l1), !is.na(dl_hat_resid_l2), !is.na(dl_hat_resid_l3),
         !is.na(dl_hat_resid_l4)) %>%
  mutate_at(.vars = vars("dl_hat_resid_l1", "dl_hat_resid_l2", "dl_hat_resid_l3",
                         "dl_hat_resid_l4"),
            ~((. - mean(.))/sd(.))) %>%
  mutate(covid_mortality_pc = covid_mortality_pc - mean(covid_mortality_pc))

X = select(lassodata, -covid_mortality_pc) %>%
  as.matrix()
y = select(lassodata, covid_mortality_pc) %>%
  as.matrix()
lasso_output = cv.glmnet(X, y,
                         alpha = 1, intercept = FALSE)
lasso_fit2 = glmnet(X, y, alpha = 1, intercept = FALSE)


lasso_fit = glmnet(X, y, alpha = 1, lambda = lasso_output$lambda.1se,
                   intercept = FALSE)

coef(lasso_fit)

# coefficient value plot
coefdata = tibble(dl_hat_resid_l1 = lasso_fit2$beta[1,],
                  dl_hat_resid_l2 = lasso_fit2$beta[2,],
                  dl_hat_resid_l3 = lasso_fit2$beta[3,],
                  dl_hat_resid_l4 = lasso_fit2$beta[4,],
                  lambda = lasso_fit2$lambda)
coefvalue = ggplot(coefdata, aes(x = lambda)) +
  geom_line(aes(y = dl_hat_resid_l1, col = "Lag 1"), size = 1) +
  geom_line(aes(y = dl_hat_resid_l2, col = "Lag 2"), size = 1) +
  geom_line(aes(y = dl_hat_resid_l3, col = "Lag 3"), size = 1) +
  geom_line(aes(y = dl_hat_resid_l4, col = "Lag 4"), size = 1) +
  labs(x = "Lambda", y = "Coefficient Value") +
  theme_bw()

