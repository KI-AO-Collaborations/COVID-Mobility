library(tidyverse)
library(haven)
library(estimatr)
library(lubridate)
library(broom)

setwd("G:/Shared drives/Econ21320/data/")

# State Level Weather -----------------------------------------------------
# Import and clean the state level weather data
state_weather = read_csv("raw/tavg_50states.csv")
state_weather = mutate(state_weather, 
                       Value = na_if(Value, -99),
                       Date = paste0(str_sub(Date,1,4), "/", str_sub(Date,5,6), "/", "01"),
                       Date = ymd(Date)) %>%
  select(Location, Date, Value) %>%
  rename(Tavg = Value,
         State = Location)


# State Level Influenza ---------------------------------------------------
# Import and clean the influenza data
influenza_us = read_tsv("raw/influenza_us.txt")
influenza_us = select(influenza_us, State, `Month Code`, Deaths) %>%
  rename(Date = `Month Code`) %>%
  mutate(Date = paste0(Date, "/01"),
         Date = ymd(Date),
         Deaths = na_if(Deaths, "Suppressed"),
         Deaths = as.numeric(Deaths))

# aggregate to the state-year level
influenza_us = mutate(influenza_us, Year = year(Date)) %>%
  group_by(State, Year) %>%
  summarise(Influenza_Deaths = sum(Deaths, na.rm = TRUE))

# State Level Swine Flu ---------------------------------------------------
# Import and clean the state level swine flu data
swineflu_us = read_csv("raw/swineflu_us.csv")
for (i in 1:130)
{
  swineflu_us = mutate(swineflu_us,
                    Cases = str_remove_all(Cases, coll(paste0("[", i, "]"))),
                    Deaths = str_remove_all(Cases, coll(paste0("[", i, "]")))) 
}
swineflu_us = mutate(swineflu_us, 
                     Cases = gsub("[^0-9]", "", Cases),
                     Deaths = gsub("[^0-9]", "", Deaths)) %>%
  select(State, Cases, Deaths) %>%
  mutate(Cases = as.numeric(Cases),
         Deaths = as.numeric(Deaths)) %>%
  rename(Swineflu_Cases = Cases,
         Swineflu_Deaths = Deaths)


# Census Data -------------------------------------------------------------
# Import and clean the population data
state_pop = map(.x = 2010:2018,
                .f = {function(yr) read_tsv(paste0("raw/state_pop", yr, ".txt"))}) %>%
  bind_rows()

state_pop = select(state_pop, State, `Age Group Code`, `Gender Code`, Race, 
                   `Yearly July 1st Estimates Code`, Population) %>%
  rename(AgeGroup = `Age Group Code`,
         Gender = `Gender Code`,
         Year = `Yearly July 1st Estimates Code`)
totpop = group_by(state_pop, State, Year) %>%
  summarise(TotPop = sum(Population))
state_pop_share = state_pop %>%
  inner_join(totpop) %>%
  mutate(PopShare = Population / TotPop)


# Corona Data -------------------------------------------------------------
# Import and clean corona death rates data
corona_deaths = read_csv("raw/time_series_covid19_deaths_US.csv")
corona_cases = read_csv("raw/time_series_covid19_confirmed_US.csv")

# Swap to long format
corona_deaths = select(corona_deaths, -c("UID", "iso2", "iso3", "code3", "FIPS",
                                         "Admin2", "Country_Region", "Lat", "Long_")) %>%
  pivot_longer(-c("Province_State", "Combined_Key", "Population"),
                             names_to = "Date", values_to = "Deaths")
corona_cases = select(corona_cases, -c("UID", "iso2", "iso3", "code3", "FIPS",
                                         "Admin2", "Country_Region", "Lat", "Long_")) %>%
  pivot_longer(-c("Province_State", "Combined_Key"),
                             names_to = "Date", values_to = "Cases")
# Aggregate to the state level
corona_deaths = mutate(corona_deaths, Date = mdy(Date)) %>%
  group_by(Province_State, Date) %>%
  summarise(Deaths = sum(Deaths), Population = sum(Population)) %>%
  ungroup()
corona_cases = mutate(corona_cases, Date = mdy(Date)) %>%
  group_by(Province_State, Date) %>%
  summarise(Cases = sum(Cases)) %>%
  ungroup()

corona_case_death = full_join(corona_deaths, corona_cases)
rm(corona_deaths, corona_cases)

first_case <- function(state)
{
  data = filter(corona_case_death, Cases > 0, Province_State == state)
  firstcase = min(data$Date)
  return(firstcase)
}
first_death <- function(state)
{
  data = filter(corona_case_death, Deaths > 0, Province_State == state)
  firstdeath = min(data$Date)
  return(firstdeath)
}
early_cases <- function(state, ndays)
{
  firstcase = first_case(state)
  if (firstcase == Inf) # no first cases found 
    return(0)
  data = filter(corona_case_death, Date %within% interval(firstcase, firstcase + days(ndays)))
  return(max(data$Cases, na.rm = TRUE))
}
early_deaths <- function(state, ndays)
{
  firstdeath = first_death(state)
  if (firstdeath == Inf) # no first cases found 
    return(0)
  data = filter(corona_case_death, Date %within% interval(firstdeath, firstdeath + days(ndays)))
  return(max(data$Deaths, na.rm = TRUE))
}

# Compute date of first case, first death, # early cases, # early deaths (early = first 14 days)
corona_case_death_early = mutate(corona_case_death,
                           First_Case_Date = map(.x = Province_State,
                                                 .f = first_case) %>%
                             as_vector() %>%
                             as_date(),
                           First_Death_Date = map(.x = Province_State,
                                                  .f = first_death) %>%
                             as_vector() %>%
                             as_date(),
                           Early_Deaths = map_dbl(.x = Province_State,
                                              .f = {function(state) early_deaths(state, 30)}),
                           Early_Cases = map_dbl(.x = Province_State,
                                                 .f = {function(state) early_cases(state, 30)})) %>%
  rename(State = Province_State) %>%
  select(-Deaths, -Cases, -Date) %>%
  distinct(State, .keep_all = TRUE)
corona_case_death_total = group_by(corona_case_death, Province_State) %>%
  summarise(Corona_Cases = sum(Cases),
            Corona_Deaths = sum(Deaths)) %>%
  rename(State = Province_State)

corona_case_death = inner_join(corona_case_death_early, corona_case_death_total) %>%
  mutate(Early_Deaths_Share = Early_Deaths/Population,
         Early_Cases_Share = Early_Cases/Population,
         Corona_Cases_Share = Early_Cases/Population,
         Corona_Deaths_Share = Corona_Deaths/Population)


# Policy Strength ---------------------------------------------------------
# Delay Index: How long after the first case the policy was implemented
# Indicator: Did the state implement the policy at all?

# Construction of Delay indicies for each policy, indicator for whether policy was implemented
policy = read_csv("raw/state_policy.csv")

# Remove unimportant columns and fix date formats
policy = select(policy, -location_id, -StateFIPS, -StatePostal, 
                -PolicyCodingNotes, -PolicySource, -LastUpdated, -LastUpdatedNotes) %>%
  rename(State = StateName) %>%
  mutate(DateIssued = ymd(DateIssued),
         DateEnacted = ymd(DateEnacted))
state_dates = select(corona_case_death, State, First_Case_Date, First_Death_Date) %>%
  distinct(State, .keep_all = TRUE)
policy = full_join(policy, state_dates)
rm(state_dates)

# Produces a delay index for a given policy
delay_index <- function(policyname, policydata)
{
  policydata = filter(policydata, StatePolicy %in% policyname, Mandate == 1, StateWide == 1) %>%
    group_by(State, StatePolicy) %>%
    filter(DateIssued == min(DateIssued))
  policydata = mutate(policydata,
                      Ndays_First_Case = interval(First_Case_Date, DateIssued)/days(1),
                      Ndays_First_Death = interval(First_Death_Date, DateIssued)/days(1),
                      Ndays_First_USCase = interval(ymd("2020-01-22"), DateIssued)/days(1),
                      Ndays_First_USDeath = interval(ymd("2020-02-29"), DateIssued)/days(1))
  return(policydata)
}

policies = c("SchoolClose", "GathRestrictAny", "NEBusinessClose", "OtherBusinessClose")
delay_index_schoolclose = delay_index(policies, policy)
delay_index_mean = group_by(delay_index_schoolclose, State) %>%
  summarise(Ndays_First_Case = mean(Ndays_First_Case),
            Ndays_First_Death = mean(Ndays_First_Death),
            Ndays_First_USCase = mean(Ndays_First_USCase),
            Ndays_First_USDeath = mean(Ndays_First_USDeath))

ggplot(delay_index_schoolclose, aes(x = State, y = Ndays_First_Case)) +
  geom_bar(stat = "identity") + 
  theme(axis.text.y = element_text(size = 7)) +
  labs(y = "Number of Days since First Case",
       title = "Average Number of Days since First Case for Each State") +
  coord_flip()
hist(delay_index_schoolclose$Ndays_First_Case)

# Social Distancing Data --------------------------------------------------
# Unacast Data
sd_data1 = read_csv("raw/sds-v3-full-state.csv")

sd_data1 = select(sd_data1, -state_population, -state_fips, -state_code, -last_updated,
                 -weekday, -grade_total, -n_grade_total, -grade_distance, 
                 -n_grade_distance, -grade_visitation, -n_grade_visitation,
                 -grade_encounters, -n_grade_encounters) %>%
  rename(State = state_name, Date = date)

# convert daily distance diff and visitation diffs to z-scores (use country mean and sd)
sd_data1 = mutate_at(sd_data1, 
                     .vars = vars(c("daily_distance_diff", "daily_visitation_diff", "encounters_rate")),
                     ~((.-mean(.))/sd(.)))

# Descartes Lab Data (first only use state level)
sd_data2 = read_csv("raw/DL-us-mobility-daterow.csv")
sd_data2 = filter(sd_data2, admin_level == 1, country_code == "US") %>%
  select(-country_code, -admin_level, -admin2, -fips, -samples, -m50) %>%
  rename(Date = date, State = admin1)

# convert to z-scores (use country mean and sd)
sd_data2 = mutate(sd_data2, m50_index = (m50_index - mean(m50_index))/sd(m50_index))

# merge these social distancing measures
all_sd = full_join(sd_data1, sd_data2)

## Data Checks (ensure they are actually measuring the same thing)
# 1. Plot time series of both for new york
nyplot = ggplot(filter(all_sd, State == "New York"), aes(x = Date)) +
  geom_point(aes(y = daily_distance_diff, col = "Unacast Distance")) +
  geom_point(aes(y = daily_visitation_diff, col = "Unacast Visitation")) +
  geom_point(aes(y = encounters_rate, col = "Unacast Encounter")) +
  geom_point(aes(y = m50_index, col = "DL Index"))

# 2. Plot unacast index against DL index (aggregated)
regplot = ggplot(all_sd, aes(x = m50_index, y = daily_distance_diff)) +
  geom_point() + 
  geom_smooth(method = "loess", formula = y ~ x)
model = lm_robust(daily_distance_diff ~ m50_index, data = all_sd)

# 3. Regress unacast against DL within each state
reg_state <- function(state)
{
  statedata = filter(all_sd, State == state)
  model = lm_robust(daily_distance_diff ~ m50_index, data = statedata)
  df = tidy(model) %>%
    filter(term == "m50_index") %>%
    select(estimate, std.error, statistic) %>%
    mutate(State = state)
  return(df)
}
state_regs = map(.x = unique(all_sd$State),
                 .f = reg_state) %>%
  bind_rows()

# regression coefficients
state_regplot = ggplot(state_regs, aes(x = estimate)) +
  geom_histogram(bins = 100, fill = "darkblue") +
  geom_vline(xintercept = mean(state_regs$estimate, na.rm = TRUE), 
             linetype = "dashed", col ="red")
# t stats
hist(state_regs$statistic)



# Regressions 1 (Past Virus) ----------------------------------------------
age_share = state_pop_share %>%
  group_by(State, AgeGroup, Year) %>%
  summarise(TotPop = TotPop[1], PopShare = sum(PopShare)) %>%
  pivot_wider(names_from = AgeGroup, values_from = PopShare) %>%
  select(-"NA") %>%
  filter(Year == 2010) %>%
  select(-Year) %>%
  mutate(`65+` = `65-69` + `70-74` + `75-79` + `80-84` + `85+`)

swineflu_share = inner_join(swineflu_us, totpop) %>%
  mutate(Swineflu_Case_Share = Swineflu_Cases/TotPop,
         Swineflu_Deaths_Share = Swineflu_Deaths/TotPop) %>%
  select(-TotPop) %>%
  filter(Year == 2010) %>%
  select(-Year)

swineflu_dates = interval(mdy("04/17/2009"), mdy("08/11/2009"))
weather_swineflu = filter(state_weather, Date %within% swineflu_dates) %>%
  group_by(State) %>%
  summarise(Swineflu_Tavg = mean(Tavg, na.rm = TRUE))
rm(swineflu_dates)

influenza_share = inner_join(influenza_us, totpop) %>%
  filter(Year == 2018) %>%
  mutate(Influenza_Deaths_Share = Influenza_Deaths/TotPop) %>%
  select(-TotPop, -Year) 

weather_influenza = filter(state_weather, year(Date) == 2018) %>%
  group_by(State) %>%
  summarise(Influenza18_Tavg = mean(Tavg, na.rm = TRUE))

# Look at the mean social distancing score 2 weeks after the first case
all_sd_agg = inner_join(select(corona_case_death, State, First_Case_Date),
                        all_sd) %>%
  filter(Date <= First_Case_Date + days(14)) %>%
  group_by(State) %>%
  summarise(daily_distance_diff = mean(daily_distance_diff, na.rm = TRUE),
            daily_visitation_diff = mean(daily_visitation_diff, na.rm = TRUE),
            encounters_rate = mean(encounters_rate, na.rm = TRUE),
            m50_index = mean(m50_index, na.rm = TRUE))

alldata = inner_join(weather_swineflu, age_share) %>%
  inner_join(weather_influenza) %>%
  inner_join(swineflu_share) %>%
  inner_join(influenza_share) %>%
  inner_join(delay_index_mean) %>%
  inner_join(all_sd_agg) %>%
  inner_join(select(corona_case_death, -First_Case_Date, -First_Death_Date))

## Regressions for Policy Strength
# plot of first stage
ggplot(alldata, aes(x = Swinflu_Deaths_Share, y = Ndays_First_USCase)) +
  geom_smooth(formula = y ~ x + I(x^2), method = lm) +
  geom_text(aes(label = State), size = 2.5) +
  scale_x_log10() +
  scale_y_log10() +
  labs(x = "Influenza Death Rate (log)", y = "Delay Index (log)") +
  theme_bw()

# plot of reduced form
ggplot(alldata, aes(x = Influenza_Deaths_Share, y = Corona_Deaths_Share)) +
  geom_smooth(formula = y ~ x + I(x^2) + I(x^3), method = lm) +
  geom_text(aes(label = State), size = 2.5) +
  labs(x = "Influenza Death Rate", y = "Corona Death Rate") +
  theme_bw()

# associated regressions
firststage = lm_robust(log(Ndays_First_USCase) ~ log(Influenza_Deaths_Share) + I(log(Influenza_Deaths_Share)^2) + 
                         Influenza18_Tavg + Population + `65+`, 
                       alldata,
                       se_type = "stata")
reducedform = lm_robust(Early_Deaths_Share ~ log(Influenza_Deaths_Share) + I(log(Influenza_Deaths_Share)^2) +
                          Influenza18_Tavg + Population + `65+`,
                        alldata,
                        se_type = "stata")
ivreg = iv_robust(Early_Deaths_Share ~ log(Ndays_First_USCase) + Influenza18_Tavg + Population + `65+`
                    | log(Influenza_Deaths_Share) + I(log(Influenza_Deaths_Share)^2) + Influenza18_Tavg + Population + `65+`,
                  data = alldata,
                  se_type = "stata",
                  diagnostics = TRUE)
endog = lm_robust(Corona_Deaths_Share ~ log(Ndays_First_USCase) + Influenza18_Tavg + Population + `65+`,
                  alldata,
                  se_type = "stata")

## Regressions for Social Distancing
# first stage regression
firststage_sd = lm_robust(m50_index ~ log(Influenza_Deaths_Share) +
                            Influenza18_Tavg + Population + `65+`,
                          alldata,
                          se_type = "stata")

endog_sd = lm_robust(Early_Deaths_Share ~ m50_index + Influenza18_Tavg + Population + `65+`,
                     alldata,
                     se_type = "stata")
endog_sd2 = iv_robust(Early_Deaths_Share ~ m50_index + Influenza18_Tavg + Population + `65+`
                      | daily_distance_diff + Influenza18_Tavg + Population + `65+`,
                      alldata,
                      se_type = "stata")

# Regressions 2 (Weather) -------------------------------------------------




# Clean Data --------------------------------------------------------------
# # Merge Weather data with Swine Flu Data
# swineflu = read_csv("raw/swineflu.csv")
# 
# # Remove random non-number characters from the data
# for (i in 1:100)
# {
#   swineflu = mutate(swineflu,
#                     Cases = str_remove_all(Cases, coll(paste0("[", i, "]"))),
#                     Deaths = str_remove_all(Cases, coll(paste0("[", i, "]")))) 
# }
# swineflu = mutate(swineflu, 
#                   Cases = str_remove(Cases, coll("(")),
#                   Cases = str_remove(Cases, coll(")")),
#                   Deaths = str_remove(Deaths, coll("(")),
#                   Deaths = str_remove(Deaths, coll(")")))
# 
# # Import and clean the country level weather data
# keys = read_csv("raw/keys.csv")
# keys = replace_na(keys, list(X4 = "", X5 = "", X6 = "", X7 = "", X8 = "", X9 = "", X10 = ""))
# keys = mutate(keys, 
#               Country = paste(Country, X4, X5, X6, X7, X8, X9, X10),
#               Country = str_trim(Country)) %>%
#   select(Key, Country)
# 
# temps = read_csv("raw/tavg_countries.csv")
# temps = rename(temps, Key = ISO_3DIGIT) %>%
#   left_join(keys)
# 
# # join 
# alldata = full_join(swineflu, select(temps, Annual_temp, Country, Key))

# # First Stage- Swine Flu
# reg_stage1 = lm_robust(log(Case_Share) ~ log(Tavg), data = alldata, se_type = "stata")
# stage1 = ggplot(alldata, aes(x = log(Tavg), y = log(Case_Share))) +
#   geom_smooth(method = lm) +
#   geom_text(aes(label = State), size = 2.5) +
#   labs(x = "Log(Temperature), F", y = "Log(Case/Population)",
#        title = "IV First Stage: Temperature on Swine Flu Cases") +
#   theme_bw()
# 
# # IV Regression on Corona Deaths
# reg_final = iv_robust(log(Corona_Early_Deaths_Share) ~ log(Case_Share) + TotPop | 
#                         log(Tavg) + TotPop,
#                       data = alldata,
#                       se_type = "stata",
#                       diagnostics = TRUE)
# 
# reg_final = iv_robust(log(Corona_Deaths_Share) ~ log(Case_Share) | log(Tavg),
#                       data = alldata,
#                       se_type = "stata",
#                       diagnostics = TRUE)
# reg_endog = lm_robust(log(Corona_Early_Deaths_Share) ~ log(Case_Share) + log(TotPop), data = alldata,
#                       se_type = "stata")

# IV Regression on Corona Cases


# First Stage- Influenza