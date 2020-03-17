# Mapping the burden of COVID-2019.
#### This is a rapidly developing projected aimed at projecting and mapping the burden of COVID-2019. Our current efforts have focused on the U.S. but we hope to expand our analyses to the rest of the world.
## Overview
#### Currently, our estimates of severe case burden are cumulative. We are currently working incoroporate predictions from an age structured epidemiological model in order to add a time course to our prediction. 
#### We calculated the total number of severe cases (those requiring intensive care) in each US county as the sum of severe cases across age classes (0-9,10-19,...,70-79,80+).
#### For each county, we calculated the cumulative number of severe cases in each age class as the product of the number of individuals in that age class, the cumulative infection rate, the case fatality rate for that age class, and the ratio of severe cases to severe cases resulting in death.
#### We assumed a cumulative infection rate of 40% in all age classes and in all locations. The 40% cumulative infeciton rate is a 'ballpark' estimate, that in the short term (next several months) is perhaps pessimistic, but in the long term (beyond) is perhaps optimistic. 
#### We used published estimates of age class specific case fatality rates. See data section below for more details.
#### We used a published estimate of the percent of severe cases resulting in a fatality (49%) to calculate the ratio of severe cases to severe cases resulting in death as (1/0.49).
#### We used our estimates of cumulative severe case numbers, along with demographic data and data on hospital bed availibility to predict the spaital distribution of expected cumulative severe COVID-19 cases in the US.
## Preliminary results
### Relative number of severe cases in the US
#### Colors show the base-10 log of the ratio of severe cases in each county to the average number of severe cases per county in the U.S.
![alt text](https://github.com/ianfmiller/covid19-burden-mapping/blob/master/README.figs/rel.severe.cases.png)
### ICU beds per severe case
## Code
#### Code for all data analysis and plotting is contained in <a href="https://github.com/ianfmiller/covid19-burden-mapping/blob/master/mapping.R">mapping.R</a>
## Data sources
#### We used publically available <a href="https://factfinder.census.gov/faces/tableservices/jsf/pages/productview.xhtml?src=bkmk">2018 demography data from the U.S. Census</a>
#### We obtained data on hospital location and bed number from the American Hospital Association 2018 annual survey. We aggregated the data by county in accordance with American Hospital Associaiton date use policy.
#### We used estimates of age specificic case fatality rate from <a href="https://www.medrxiv.org/content/10.1101/2020.03.04.20031104v1">Riou et al. (2020) Adjusted age-specific case fatality ratio during the COVID-19 epidemic in Hubei, China, January and February 2020. medRxiv.</a>
#### We used an estimate of the % severe cases resulting in death from <a href="https://jamanetwork.com/journals/jama/article-abstract/2762130">Wu and McGoogan (2020). Characteristics of and important lessons from the coronavirus disease 2019 (COVID-19) outbreak in China: summary of a report of 72 314 cases from the Chinese Center for Disease Control and Prevention. JAMA.</a>
