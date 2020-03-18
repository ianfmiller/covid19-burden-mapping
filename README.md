# Mapping the burden of COVID-2019
#### This rapidly developing project is aimed at projecting and mapping the burden of COVID-2019 based on population age distribution and healthcare system capacity. All results are preliminary and subject to change as we receive new information. Our current efforts have focused on the United States.
#### Authors: Ian Miller, Alex Becker, Bryan Grenfell, and Jess Metcalf of Princeton University.
#### Healthcare officials: If you would like maps of a specific state or region, please contact Ian Miller at ifmiller(at)princeton(dot)edu
#### Please feel free to adapt, refine, and send corrections.
## Important Caveats
#### These preliminary results should be interpreted as rough estimate of where the burden of COVID-19 may be highest rather than as exact projections. However, they could still be used as a set of guidelines for prioritizing resource allocation. 
#### Currently, our estimates of severe cases per ICU bed assume a cumulative 40% infection rate across all age classes, and homogeneous spread across space. The 40% cumulative infection rate is a 'ballpark' estimate that in the short term (i.e., next several months) is perhaps pessimistic, but in the long term (i.e., beyond) is perhaps optimistic. Our assumption of homogeneous spread is certainly incorrect for the immediate future, but may become more appropriate as the outbreak grows.
#### We modeled heath care system capacity (e.g., number of ICU beds) at the county level. We distributed cases originating within a county to the healthcare systems of that county and of other counties based on distance and relative ICU bed count.
#### See methodological details below for more detailed information.
## Preliminary results
### Severe cases per ICU bed
#### Colors show log10 ratio of estimated severe cases in each county to the number of ICU beds in each county after cases have been distributed to healthcare systems. Counties with 0 ICU beds, and thus no capacity to treat severe cases, are shaded grey. Additionally, regions with zero ICU beds may likely impact case and hospital burden in neighboring counties
![alt text](https://github.com/ianfmiller/covid19-burden-mapping/blob/master/README.figs/Severe.cases.per.icu.bed.png)
### Relative number of severe cases in the US, after distribution to healthcare systems. 
#### Colors show log10 ratio of estimated severe cases in each county to the average number of severe cases per county in the U.S. Counties with 0 ICU beds, and thus no capacity to treat severe cases, are shaded grey.
![alt text](https://github.com/ianfmiller/covid19-burden-mapping/blob/master/README.figs/rel.severe.cases.post.alloc.png)
### Estimated relative number of severe cases in the US, prior to distribution to healthcare systems.
#### Colors show log10 ratio of estimated severe cases in each county to the average number of severe cases per county in the U.S.
![alt text](https://github.com/ianfmiller/covid19-burden-mapping/blob/master/README.figs/rel.severe.cases.pre.alloc.png)
### Total ICU beds
#### Colors show the log10 estimated number of ICU beds per county. Counties with 0 ICU beds are shaded grey.
![alt text](https://github.com/ianfmiller/covid19-burden-mapping/blob/master/README.figs/icu.beds.png)
### Total Population
#### Colors show log10 total population.
![alt text](https://github.com/ianfmiller/covid19-burden-mapping/blob/master/README.figs/population.png)
### % Population over 60
#### Colors show the percentage of individuals over the age of 60.
![alt text](https://github.com/ianfmiller/covid19-burden-mapping/blob/master/README.figs/p.pop.over.60.png)
## Methodological notes
### Severe case estimation
#### We calculated the total number of severe cases (those requiring intensive care) in each US county as the sum of severe cases across age classes (0-9,10-19,...,70-79,80+).
#### For each county, we calculated the cumulative number of severe cases in each age class as the product of the number of individuals in that age class, the cumulative infection rate, the case fatality rate for that age class, and the ratio of severe cases to severe cases resulting in death.
#### We assumed a cumulative infection rate of 40% in all age classes and in all locations.
#### We used published estimates of age class specific case fatality rates. See data section below for more details.
#### We used a published estimate of the percent of severe cases resulting in a fatality (49%) to calculate the ratio of severe cases to severe cases resulting in death as (1/0.49).

### Healthcare system capacity
#### Because we are modeling the burden of severe cases that require intensive care, we only considered numbers of ICU beds.
#### We counted only adult medical/surgical intensive care beds.
#### We aggregated ICU bed data to the county level for simplicity, and to comply with data use policy.

### Case distribution
#### We distributed cases originating in a given county to the healthcare systems of that county and other counties. 
#### Let the county of origin be denoted as <img src="https://render.githubusercontent.com/render/math?math=c_{0}"> 
#### and the potential destinatino counties as <img src="https://render.githubusercontent.com/render/math?math=c_{1}...c_{n}">
#### Let the distance between the county of origin, <img src="https://render.githubusercontent.com/render/math?math=c_{0}">, and each desitnation county, <img src="https://render.githubusercontent.com/render/math?math=c_{i}">, be <img src="https://render.githubusercontent.com/render/math?math=d_{0}(i)">
#### We considered destination counties with <img src="https://render.githubusercontent.com/render/math?math=d_{0}(i) < 400 km">
#### We calculated a distance weight, <img src="https://render.githubusercontent.com/render/math?math=z_{i}">, for each county according to the following formula  <img src="https://render.githubusercontent.com/render/math?math=z_{i} = \frac{1}{20} * e^{\frac{d_{0}(i)}{20}}">
#### We calculated a bed weight, <img src="https://render.githubusercontent.com/render/math?math=b_{i}"> , for each county according to the following formula <img src="https://render.githubusercontent.com/render/math?math=b_{i} = "> number of ICU beds in <img src="https://render.githubusercontent.com/render/math?math=c_{i}">
#### We then calculated a composite weight, <img src="https://render.githubusercontent.com/render/math?math=w_{i}"> , for each county as  <img src="https://render.githubusercontent.com/render/math?math=w_{i} =\frac{z_{i}}{\sum_{j=0}^{n}  z_{j}} * \frac{b_{i}}{\sum_{j=0}^{n}  b_{j}}">
#### And finally a relative weight for each county, <img src="https://render.githubusercontent.com/render/math?math=r_{i}"> , as <img src="https://render.githubusercontent.com/render/math?math=r_{i} = \frac{w_{i}}{\sum_{j=0}^{n}  w_{j}}">
#### The severe cases originating in <img src="https://render.githubusercontent.com/render/math?math=c_{0}"> are distributed to counties <img src="https://render.githubusercontent.com/render/math?math=c_{0}...c_{n}"> in proportion to <img src="https://render.githubusercontent.com/render/math?math=r_{0}...r_{n}">
## Code
#### Code for all data analysis and plotting is contained in <a href="https://github.com/ianfmiller/covid19-burden-mapping/blob/master/mapping.R">mapping.R</a> 
#### Code for generating the hospital bed data file for use in plotting is contained in <a href="https://github.com/ianfmiller/covid19-burden-mapping/blob/master/clean.hosp.data.R">clean.hosp.data.R</a> Unfortunately, American Hospital Association policy prevents us from posting the cleaned and aggregated data that this file generates. If you are able to access the 2018 AHA Survey Database, the raw file needed for this code is "AS2018FULLFILE.csv" in '/2018 AHA survey/COMMA/'
## Data sources
#### We used publicly available <a href="https://factfinder.census.gov/faces/tableservices/jsf/pages/productview.xhtml?src=bkmk">2018 demography data from the U.S. Census</a>
#### We obtained data on hospital location and bed number from the American Hospital Association 2018 annual survey. We aggregated the data by county in accordance with American Hospital Associaiton date use policy. Unfortunately, these data are behind a paywall. 
#### We used estimates of age specificic case fatality rate from <a href="https://www.medrxiv.org/content/10.1101/2020.03.04.20031104v1">Riou et al. (2020) Adjusted age-specific case fatality ratio during the COVID-19 epidemic in Hubei, China, January and February 2020. medRxiv.</a>
#### We used an estimate of the % severe cases resulting in death from <a href="https://jamanetwork.com/journals/jama/article-abstract/2762130">Wu and McGoogan (2020). Characteristics of and important lessons from the coronavirus disease 2019 (COVID-19) outbreak in China: summary of a report of 72 314 cases from the Chinese Center for Disease Control and Prevention. JAMA.</a>
