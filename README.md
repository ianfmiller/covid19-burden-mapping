# Mapping the burden of COVID-2019.
### This is a rapidly developing projected aimed at projecting and mapping the burden of COVID-2019. Our current efforts have focused on the U.S. but we hope to expand our analyses to the rest of the world.
### Currently, our estimates of case burden are based off of population age structure, age specific case fatality rates, and an assumption of a cumulative infection rate of 40% in all age classes. Thus, our estimate of the number of severe cases is also cumulative. The 40% cumulative infeciton rate is a 'ballpark' estimate, that in the short term (next several months) is perhaps pessimistic, but in the long term (beyond) is perhaps optimistic. 
### Code
#### Code for all data analysis and plotting is contained in <a href="https://github.com/ianfmiller/covid19-burden-mapping/blob/master/mapping.R">mapping.R</a>
### Data sources
#### We used publically available <a href="https://factfinder.census.gov/faces/tableservices/jsf/pages/productview.xhtml?src=bkmk">2018 demography data from the U.S. Census</a>
#### We obtained data on hospital location and bed number from the American Hospital Association 2018 annual survey. We aggregated the data by county in accordance with American Hospital Associaiton date use policy.
#### We used estimates of age specificic case fatality rate from <a href="https://www.medrxiv.org/content/10.1101/2020.03.04.20031104v1">Riou et al. (2020) Adjusted age-specific case fatality ratio during the COVID-19 epidemic in Hubei, China, January and February 2020. medRxiv.</a>
#### We used an estimate of the %severe cases resulting in death from <a href="https://jamanetwork.com/journals/jama/article-abstract/2762130">Wu and McGoogan (2020). Characteristics of and important lessons from the coronavirus disease 2019 (COVID-19) outbreak in China: summary of a report of 72 314 cases from the Chinese Center for Disease Control and Prevention. JAMA.</a>
