# Mapping the burden of COVID-2019.
#### This is a rapidly developing project aimed at projecting and mapping the burden of COVID-2019 based on population age distribution and healthcare system capacity. Our current efforts have focused on the U.S.
## Important Caveats
#### Currently, our estimates of severe cases per ICU bed assume a cumulative 40% infection rate across all age classes, and homogeneous spread across spacce. The 40% cumulative infeciton rate is a 'ballpark' estimate, that in the short term (next several months) is perhaps pessimistic, but in the long term (beyond) is perhaps optimistic. Our assumption of homogeneous spread is certainly incorrect for the immediate future, but may become more appropriate as the outbreak grows.
#### We modeled heath care system capacity (number of ICU beds) at the county level. We distributed cases originating within a county to the healthcare systems of that county and of other counties based on distance and relative ICU bed count.
#### See methodological details below for more detailed information.
## Preliminary results
### Severe cases per ICU bed
#### Colors show log10 ratio of severe cases in each county to the number of ICU beds in each county after cases have been distributed to healthcare systems. Counties without ICU beds are shaded grey.
![alt text](https://github.com/ianfmiller/covid19-burden-mapping/blob/master/README.figs/Severe.cases.per.icu.bed.png)
### Relative number of severe cases in the US, after distribution to healthcare systems. 
#### Colors show log10 ratio of severe cases in each county to the average number of severe cases per county in the U.S. Counties without ICU beds are shaded grey.
![alt text](https://github.com/ianfmiller/covid19-burden-mapping/blob/master/README.figs/rel.severe.cases.post.alloc.png)
### Relative number of severe cases in the US, prior to distribution to healthcare systems.
#### Colors show log10 ratio of severe cases in each county to the average number of severe cases per county in the U.S.
![alt text](https://github.com/ianfmiller/covid19-burden-mapping/blob/master/README.figs/rel.severe.cases.pre.alloc.png)
### Total ICU beds
#### Colors show the log10 number of ICU beds per county. Grey indicates no ICU beds.
![alt text](https://github.com/ianfmiller/covid19-burden-mapping/blob/master/README.figs/icu.beds.png)
### Total Population
#### Colors show log10 total population.
![alt text](https://github.com/ianfmiller/covid19-burden-mapping/blob/master/README.figs/population.png)
### Total Population
#### Colors show the percentage of individuals over the age of 60.
![alt text](https://github.com/ianfmiller/covid19-burden-mapping/blob/master/README.figs/p.pop.60.plus.png)
## Methodological notes
#### We calculated the total number of severe cases (those requiring intensive care) in each US county as the sum of severe cases across age classes (0-9,10-19,...,70-79,80+).
#### For each county, we calculated the cumulative number of severe cases in each age class as the product of the number of individuals in that age class, the cumulative infection rate, the case fatality rate for that age class, and the ratio of severe cases to severe cases resulting in death.
##### --We assumed a cumulative infection rate of 40% in all age classes and in all locations. The 40% cumulative infeciton rate is a 'ballpark' estimate, that in the short term (next several months) is perhaps pessimistic, but in the long term (beyond) is perhaps optimistic. 
##### --We used published estimates of age class specific case fatality rates. See data section below for more details.
##### --We used a published estimate of the percent of severe cases resulting in a fatality (49%) to calculate the ratio of severe cases to severe cases resulting in death as (1/0.49).
##### --We used our estimates of cumulative severe case numbers, along with demographic data and data on hospital bed availibility to predict the spaital distribution of expected cumulative severe COVID-19 cases in the US.
#### We distributed cases between counties based on distance and the number of ICU beds. See method notes below.
#### Need to add
## Code
#### Code for all data analysis and plotting is contained in <a href="https://github.com/ianfmiller/covid19-burden-mapping/blob/master/mapping.R">mapping.R</a>
## Data sources
#### We used publically available <a href="https://factfinder.census.gov/faces/tableservices/jsf/pages/productview.xhtml?src=bkmk">2018 demography data from the U.S. Census</a>
#### We obtained data on hospital location and bed number from the American Hospital Association 2018 annual survey. We aggregated the data by county in accordance with American Hospital Associaiton date use policy.
#### We used estimates of age specificic case fatality rate from <a href="https://www.medrxiv.org/content/10.1101/2020.03.04.20031104v1">Riou et al. (2020) Adjusted age-specific case fatality ratio during the COVID-19 epidemic in Hubei, China, January and February 2020. medRxiv.</a>
#### We used an estimate of the % severe cases resulting in death from <a href="https://jamanetwork.com/journals/jama/article-abstract/2762130">Wu and McGoogan (2020). Characteristics of and important lessons from the coronavirus disease 2019 (COVID-19) outbreak in China: summary of a report of 72 314 cases from the Chinese Center for Disease Control and Prevention. JAMA.</a>
