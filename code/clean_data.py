'''
ECON 21320 - Applications of Econometrics and Data Science Methods
Ali Hortacsu
Project 
'''
import numpy as np
import pandas as pd
import os

cur_dir = '/Volumes/GoogleDrive/Shared drives/Econ21320/data'
os.chdir(cur_dir)

#######################################
##### Historical Infection Deaths #####
#######################################

for year in [2000, 2010, 2015, 2016]:
    epi_data = pd.read_excel('raw/GHE2016_Deaths_' + str(year) + '-country.xls', sheet_name='Deaths All ages', header=6)
    epi_data = epi_data[epi_data.index <= 218] # Only keep data for total population
    epi_data = epi_data.drop([0, 1], axis=0)
    epi_data = epi_data[epi_data['GHE code'].isin([20, 380, np.nan])] # Only aggregate data for communicable diseases
    epi_data = epi_data.reset_index(drop=True)
    epi_data = epi_data.iloc[:3]

    epi_data = epi_data.append(epi_data.iloc[1] + epi_data.iloc[2], ignore_index=True)
    epi_data = epi_data.drop(['Sex', 'GHE code', 'GHE cause', 'Unnamed: 2', 'Unnamed: 4', 'Unnamed: 5', 'Member State\n(See Notes for explanation of colour codes)'], axis=1)
    epi_data = epi_data.drop([1, 2], axis=0)
    epi_data = epi_data.reset_index(drop=True)
    epi_data = epi_data.T
    epi_data = epi_data.reset_index()
    epi_data = epi_data.rename({'index': 'country', 0: 'pop_thousands', 1: 'infection_deaths'}, axis=1)
    epi_data['pop_thousands'] = epi_data['pop_thousands'].astype(float)
    epi_data['infection_deaths'] = epi_data['infection_deaths'].astype(float) * 1000

    try:
        all_epi_data['pop_thousands'] += epi_data['pop_thousands']
        all_epi_data['infection_deaths'] += epi_data['infection_deaths']
    except:
        all_epi_data = epi_data.copy()

all_epi_data['pop_thousands'] /= 4
all_epi_data['infection_deaths'] = all_epi_data['infection_deaths'] / (all_epi_data['pop_thousands'] * 1000)

all_epi_data.to_stata('clean/epidemic_rates.dta', write_index=False)

##############################
##### Healthcare Quality #####
##############################

healthcare_data = pd.read_csv('raw/IHME_GBD_2016_HAQ_INDEX_1990_2016_SCALED_CAUSE_VALUES/IHME_GBD_2016_HAQ_INDEX_1990_2016_SCALED_CAUSE_VALUES_Y2018M05D23.csv')

healthcare_data = healthcare_data[healthcare_data['indicator_name'] == 'Healthcare Access and Quality Index']
healthcare_data = healthcare_data[healthcare_data['year_id'] == 2016]
healthcare_data = healthcare_data[['location_name', 'val']]
healthcare_data = healthcare_data.rename({'location_name': 'country', 'val': 'hqi'}, axis=1)

healthcare_data.to_stata('clean/healthcare.dta', write_index=False)

###########################
##### COVID-19 Safety #####
###########################

euro_data = pd.read_csv('raw/euro_ratings.txt', delimiter=r'\t', header=None)
asia_data = pd.read_csv('raw/asia_ratings.txt', delimiter=r'\t', header=None)

covid_safety = pd.concat([euro_data, asia_data], axis=0)
covid_safety = covid_safety.drop(0, axis=1)
covid_safety = covid_safety.rename({1: 'country', 2: 'covid_safety'}, axis=1)

covid_safety.to_stata('clean/covid_safety.dta', write_index=False)

##########################################
##### COVID-19 Infections and Deaths #####
##########################################

##################
##### Global #####
##################

covid_inf_data = pd.read_csv('raw/time_series_covid19_confirmed_global.csv')
covid_mort_data = pd.read_csv('raw/time_series_covid19_deaths_global.csv')

covid_inf_data = covid_inf_data[pd.isnull(covid_inf_data['Province/State'])] # Only use entire country
covid_inf_data = covid_inf_data[['Country/Region', '4/18/20']]
covid_inf_data = covid_inf_data.rename({'Country/Region': 'country', '4/18/20': 'covid_infections'}, axis=1)
covid_inf_data.to_stata('clean/covid_infections_global.dta', write_index=False)

covid_mort_data = covid_mort_data[pd.isnull(covid_mort_data['Province/State'])] # Only use entire country
covid_mort_data = covid_mort_data[['Country/Region', '4/18/20']]
covid_mort_data = covid_mort_data.rename({'Country/Region': 'country', '4/18/20': 'covid_deaths'}, axis=1)
covid_mort_data.to_stata('clean/covid_deaths_global.dta', write_index=False)

####################
##### All Days #####
####################

covid_inf_data = pd.read_csv('raw/time_series_covid19_confirmed_US.csv')
covid_mort_data = pd.read_csv('raw/time_series_covid19_deaths_US.csv')

covid_inf_data = covid_inf_data[[covid_inf_data.columns[6]] + list(covid_inf_data.columns[11:])]
covid_inf_data = covid_inf_data.rename({'Province_State': 'state'}, axis=1)
covid_inf_data = covid_inf_data.melt(id_vars='state', var_name='date', value_name='COVID_infections')
covid_inf_data.to_stata('clean/covid_infections_us.dta', write_index=False)

pop_data = covid_mort_data[['Province_State', 'Population']]
pop_data = pop_data.rename({'Province_State': 'state', 'Population': 'population'}, axis=1)
pop_data.to_stata('clean/pop_us.dta', write_index=False)

covid_mort_data = covid_mort_data[[covid_mort_data.columns[6]] + list(covid_mort_data.columns[12:])]
covid_mort_data = covid_mort_data.rename({'Province_State': 'state'}, axis=1)
covid_mort_data = covid_mort_data.melt(id_vars='state', var_name='date', value_name='COVID_mortality')
covid_mort_data.to_stata('clean/covid_deaths_us.dta', write_index=False)

####################
##### GDP Data #####
####################

gdp_data = pd.read_csv('raw/gdp.csv')
gdp_data = gdp_data[['Country Name', '2018']]
gdp_data = gdp_data.rename({'Country Name': 'country', '2018': 'gdp_per_capita'}, axis=1)
gdp_data['country'] = gdp_data['country'].astype(str).str.encode(encoding='utf-8', errors='ignore').astype(str).str[2:-1]
gdp_data['gdp_per_capita'] = gdp_data['gdp_per_capita'].astype(float)
gdp_data.to_stata('clean/gdp_per_capita.dta', write_index=False)

#############################################
##### Weekly State-Level Mortality Data #####
#############################################

mort_data = pd.read_csv('raw/Excess_Deaths_Associated_with_COVID-19.csv', usecols=['Week Ending Date', 'State', 'Observed Number', 'Excess', 'Type', 'Outcome', 'Suppress'], parse_dates=['Week Ending Date'])
# Make sure only using data of interest
# I don't know what suppress means so I just drop if it suppresses
mort_data = mort_data[(mort_data['Type'] == 'Predicted (weighted)') & (mort_data['Outcome'] == 'All causes') & (mort_data['Suppress'] != 'Suppressed (counts 1-9)')]
mort_data = mort_data.drop(['Type', 'Outcome', 'Suppress'], axis=1)
mort_data = mort_data.rename({'Week Ending Date': 'date', 'State': 'state', 'Observed Number': 'total_mortality', 'Excess': 'COVID_mortality'}, axis=1)

mort_data['COVID_mortality_frac'] = mort_data['COVID_mortality'] / mort_data['total_mortality']

# Provide a column of values for entire US for national-level industry shares
mort_data = mort_data.sort_values(by=['state', 'date'])
us_aggregate_update = mort_data[mort_data['state'] == 'United States']
mort_data['US_total_mortality'] = np.nan
mort_data['US_COVID_mortality'] = np.nan
mort_data['US_COVID_mortality_frac'] = np.nan
for unique_date in us_aggregate_update['date'].unique():
    mort_at_date = mort_data['date'] == unique_date
    agg_at_date = us_aggregate_update['date'] == unique_date
    # Use reset_index(drop=True).iloc[0] to extract the value
    mort_data.loc[mort_at_date, 'US_total_mortality'] = us_aggregate_update.loc[agg_at_date, 'total_mortality'].reset_index(drop=True).iloc[0]
    mort_data.loc[mort_at_date, 'US_COVID_mortality'] = us_aggregate_update.loc[agg_at_date, 'COVID_mortality'].reset_index(drop=True).iloc[0]
    mort_data.loc[mort_at_date, 'US_COVID_mortality_frac'] = us_aggregate_update.loc[agg_at_date, 'COVID_mortality_frac'].reset_index(drop=True).iloc[0]

mort_data.to_stata('clean/all_mortality_by_state.dta', write_index=False)

###############################################
##### Twitter State-Level Social Mobility #####
###############################################

states = {
        'AK': 'Alaska',
        'AL': 'Alabama',
        'AR': 'Arkansas',
        'AS': 'American Samoa',
        'AZ': 'Arizona',
        'CA': 'California',
        'CO': 'Colorado',
        'CT': 'Connecticut',
        'DC': 'District of Columbia',
        'DE': 'Delaware',
        'FL': 'Florida',
        'GA': 'Georgia',
        'GU': 'Guam',
        'HI': 'Hawaii',
        'IA': 'Iowa',
        'ID': 'Idaho',
        'IL': 'Illinois',
        'IN': 'Indiana',
        'KS': 'Kansas',
        'KY': 'Kentucky',
        'LA': 'Louisiana',
        'MA': 'Massachusetts',
        'MD': 'Maryland',
        'ME': 'Maine',
        'MI': 'Michigan',
        'MN': 'Minnesota',
        'MO': 'Missouri',
        'MP': 'Northern Mariana Islands',
        'MS': 'Mississippi',
        'MT': 'Montana',
        'NA': 'National',
        'NC': 'North Carolina',
        'ND': 'North Dakota',
        'NE': 'Nebraska',
        'NH': 'New Hampshire',
        'NJ': 'New Jersey',
        'NM': 'New Mexico',
        'NV': 'Nevada',
        'NY': 'New York',
        'OH': 'Ohio',
        'OK': 'Oklahoma',
        'OR': 'Oregon',
        'PA': 'Pennsylvania',
        'PR': 'Puerto Rico',
        'RI': 'Rhode Island',
        'SC': 'South Carolina',
        'SD': 'South Dakota',
        'TN': 'Tennessee',
        'TX': 'Texas',
        'UT': 'Utah',
        'VA': 'Virginia',
        'VI': 'Virgin Islands',
        'VT': 'Vermont',
        'WA': 'Washington',
        'WI': 'Wisconsin',
        'WV': 'West Virginia',
        'WY': 'Wyoming'
}

states['Unnamed: 0'] = 'date'
states[' AK'] = 'Alaska'

twitter_state = pd.read_csv('raw/social_mobility_index.longitudinal.20200330.csv', parse_dates=['Unnamed: 0'])
twitter_state = twitter_state.rename(states, axis=1)

twitter_state = twitter_state.melt(id_vars='date', var_name='state', value_name='twitter_social_distancing')

twitter_state.to_stata('clean/twitter_social_distancing_state.dta', write_index=False)

###############################################
##### Unacast State-Level Social Mobility #####
###############################################

unacast_state = pd.read_csv('raw/sds-v3-full-state.csv', usecols=['state_name', 'state_population', 'date', 'daily_distance_diff', 'daily_visitation_diff'], parse_dates=['date'])

unacast_state = unacast_state.rename({'state_name': 'state'}, axis=1)

unacast_state = unacast_state.sort_values(by=['state', 'date'])

unacast_state.to_stata('clean/unacast_social_distancing_state.dta', write_index=False)

#####################################################
##### Descartes Lab State-Level Social Mobility #####
#####################################################

dl_state = pd.read_csv('raw/DL-us-mobility-daterow.csv')
dl_state = dl_state[dl_state['admin_level'] == 1] # Only use state-level data
dl_state = dl_state[['date', 'admin1', 'm50', 'm50_index']]
dl_state['m50_index'] /= 100
dl_state = dl_state.rename({'admin1': 'state', 'm50': 'dl_mobility', 'm50_index': 'dl_mobility_relative_to_normal'}, axis=1)

dl_state.to_stata('clean/dl_social_distancing_state.dta', write_index=False)

#########################
##### Rainfall Data #####
#########################

rain_data = pd.read_csv('raw/rainfall.csv')
rain_data = rain_data[['Date', 'Avg_Precip', 'State']]
rain_data = rain_data.rename({'Date': 'date', 'Avg_Precip': 'avg_precip', 'State': 'state'}, axis=1)

rain_data.to_stata('clean/rain_data.dta', write_index=False)
