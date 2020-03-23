# Mapping the burden of COVID-2019
#### This rapidly developing project is aimed at projecting and mapping the burden of COVID-2019 based on population age distribution and healthcare system capacity. All results are preliminary and subject to change as we receive new information. Our current efforts have focused on the United States, but we are aiming to extend our analyses to other parts of the world.
#### Authors: Ian Miller, Alex Becker, Bryan Grenfell, and Jess Metcalf of Princeton University.
#### Healthcare officials: If you would like maps of a specific state or region, please contact Ian Miller at ifmiller(at)princeton(dot)edu
#### Please feel free to adapt, refine, and send corrections.
#### We are currently exploring methods of accounting for differences among susceptibility, transmission, and/or contact patterns between age classes, and for incorporating the effects of comorbidities (hypertension, diabetes, etc).
## Important Caveats
#### These preliminary results should be interpreted as rough estimate of where the burden of COVID-19 may be highest rather than as exact projections. However, they could still be used as a set of guidelines for prioritizing resource allocation. 
#### Currently, our estimates assume:
##### 1) a cumulative 40% population infection rate 
######   This is more so a scenario we seek to investigate than an assumption about final epidemic size. A 40% cumulative infection rate would perhaps be considered pessimistic in the short term (i.e. next several months), but in the long term (i.e., beyond) is perhaps optimistic.
##### 2) equal odds of infection across all age classes, 
######   Currently, cases appear to be distributed non-uniformly across age classes. However, given the multitude of non-exclusive hypotheses about what could account for this pattern, and the dearth of data necessary to test them, we proceed cautiously while making the simplifying assumption of equal risk across age classes. This is an aspect of our methods that we are actively working to improve. 
##### 3) an asymptomatic rate of 20% in each age class,
######   Evidence about the percentage of infections that produce symptoms and the variation or lack thereof across age classes is still scarce. This estimate is consistent with the best availible data (<a href="https://www.medrxiv.org/content/10.1101/2020.03.19.20039107v1">Zhang et al.</a>, but also see <a href="https://www.niid.go.jp/niid/en/2019-ncov-e/9407-covid-dp-fe-01.html">this report</a>). 
##### and 4) homogeneous spread across space. 
######   This assumption, which we make for convenience, is certainly incorrect for the immediate future, but may become more appropriate as the outbreak grows. As we aim to predict long term patterns of disease burden, this assumption is reasonable.

#### We modeled heath care system capacity (e.g., number of hospital and ICU beds) at the county level. We distributed cases originating within a county to the healthcare systems of that county and of other counties based on distance and relative hospital bed/ICU bed count.
#### See methodological details below for more detailed information.
## Preliminary results
### Cumulative ICU admits per ICU bed
#### Colors show log10 ratio of estimated critical cases to ICU beds in each county after cases have been distributed to healthcare systems. Counties with 0 ICU beds, and thus no capacity to treat critical cases, are shaded grey. All cases from regions with 0 ICU beds have been allocated to other counties (see methodological notes below).
#### Interpretation notes: This figure gives a reasonable picture of where the burden of critical cases might be highest relative to healthsystem capacity.
![alt text](https://github.com/ianfmiller/covid19-burden-mapping/blob/master/README.figs/icu.per.bed.png)
### Cumulative hospitalizations per hospital bed
#### Colors show log10 ratio of estimated hospitalizations to hospital beds in each county after cases have been distributed to healthcare systems. Counties with 0 hospital beds, and thus no capacity to treat severe cases, are shaded grey. All cases from regions with 0 beds have been allocated to other counties (see methodological notes below).
#### Interpretation notes: This figure gives a reasonable picture of where the burden of severe cases might be highest relative to healthsystem capacity.
![alt text](https://github.com/ianfmiller/covid19-burden-mapping/blob/master/README.figs/hosp.per.bed.png)
### ICU admits per capita 
#### Colors show the number of ICU admits per person in each county (prior to distribution to healthcare systems).
![alt text](https://github.com/ianfmiller/covid19-burden-mapping/blob/master/README.figs/icu.per.capita.png)
### Hospitalizations per capita 
#### Colors show the number of hospitalizations per person in each county (prior to distribution to healthcare systems).
![alt text](https://github.com/ianfmiller/covid19-burden-mapping/blob/master/README.figs/hosp.per.capita.png)
### Relative ICU admits
#### Colors show the relative number of ICU admits in each county (prior to distribution to healthcare systems).
![alt text](https://github.com/ianfmiller/covid19-burden-mapping/blob/master/README.figs/rel.icu.pre.alloc.png)
### Relative hospitalizations
#### Colors show the relative number of hospitalizations in each county (prior to distribution to healthcare systems).
![alt text](https://github.com/ianfmiller/covid19-burden-mapping/blob/master/README.figs/rel.hosp.pre.alloc.png)
### <a href="https://github.com/ianfmiller/covid19-burden-mapping/blob/master/extended.results.md">More visualizations available here.</a>
## Methodological notes
### Case estimation
#### We calculated the total number of cases requireding hospitalization/ICU admits in each US county as the sum of cases requiring hospitalization/ICU admission across age classes (0-9,10-19,...,70-79,80+).
#### For each county, we calculated the cumulative number of hospitalizations or ICU admits in each age class as the product of the number of individuals in that age class, the cumulative infection rate, and either the hospitalizaiton rate or the hospitalizaiton rate times the ICU admit rate given hospitalization.
#### We assumed a cumulative infection rate of 40% in all age classes and in all locations.
#### We used published estimates of age class specific hospitalization rates and ICU admit rates given hospitalization. See data section below for more details.

### Healthcare system capacity
#### We considered the number of beds in each hospital to be the sum of all beds across departments.
#### We considered only adult medical/surgical intensive care beds as ICU beds.
#### We aggregated bed data at the county level for simplicity, and to comply with data use policy.

### Case distribution
#### We distributed cases originating in a given county to the healthcare systems of that county and other counties. 
#### Let the county of origin be denoted as <img src="https://render.githubusercontent.com/render/math?math=c_{0}"> 
#### and the potential destination counties as <img src="https://render.githubusercontent.com/render/math?math=c_{1}...c_{n}">
#### Let the distance between the county of origin, <img src="https://render.githubusercontent.com/render/math?math=c_{0}">, and each desitnation county, <img src="https://render.githubusercontent.com/render/math?math=c_{i}">, be <img src="https://render.githubusercontent.com/render/math?math=d_{0}(i)">
#### We considered destination counties with <img src="https://render.githubusercontent.com/render/math?math=d_{0}(i) < 400 km">
#### We calculated a distance weight, <img src="https://render.githubusercontent.com/render/math?math=z_{i}">, for each county according to the following formula  <img src="https://render.githubusercontent.com/render/math?math=z_{i} = \frac{1}{20} * e^{\frac{d_{0}(i)}{20}}">
#### We calculated a bed weight, <img src="https://render.githubusercontent.com/render/math?math=b_{i}"> , for each county according to the following formula <img src="https://render.githubusercontent.com/render/math?math=b_{i} = "> number of hospital beds in <img src="https://render.githubusercontent.com/render/math?math=c_{i}">
#### For projections involving ICU beds, we used the number of ICU beds instead of the number of hospital beds.
#### We then calculated a composite weight, <img src="https://render.githubusercontent.com/render/math?math=w_{i}"> , for each county as  <img src="https://render.githubusercontent.com/render/math?math=w_{i} =\frac{z_{i}}{\sum_{j=0}^{n}  z_{j}} * \frac{b_{i}}{\sum_{j=0}^{n}  b_{j}}">
#### And finally a relative weight for each county, <img src="https://render.githubusercontent.com/render/math?math=r_{i}"> , as <img src="https://render.githubusercontent.com/render/math?math=r_{i} = \frac{w_{i}}{\sum_{j=0}^{n}  w_{j}}">
#### The severe cases originating in <img src="https://render.githubusercontent.com/render/math?math=c_{0}"> are distributed to counties <img src="https://render.githubusercontent.com/render/math?math=c_{0}...c_{n}"> in proportion to <img src="https://render.githubusercontent.com/render/math?math=r_{0}...r_{n}">
## Code
#### Code for all data analysis and plotting is contained in <a href="https://github.com/ianfmiller/covid19-burden-mapping/blob/master/mapping.US.R">mapping.US.R</a> 
#### Code for generating the hospital bed data file for use in plotting is contained in <a href="https://github.com/ianfmiller/covid19-burden-mapping/blob/master/clean.hosp.data.R">clean.hosp.data.R</a> Unfortunately, American Hospital Association policy prevents us from posting the cleaned and aggregated data that this file generates. If you are able to access the 2018 AHA Survey Database, the raw file needed for this code is "AS2018FULLFILE.csv" in '/2018 AHA survey/COMMA/'
## Data sources
#### We used publicly available <a href="https://factfinder.census.gov/faces/tableservices/jsf/pages/productview.xhtml?src=bkmk">2018 demography data from the U.S. Census</a>
#### We obtained data on hospital location and bed number from the American Hospital Association 2018 annual survey. We aggregated the data by county in accordance with American Hospital Associaiton date use policy. Unfortunately, these data are behind a paywall. 
#### We used age specific estimates of hospital admission rates and ICU admission rates given hospitalization from   <a href="https://www.imperial.ac.uk/media/imperial-college/medicine/sph/ide/gida-fellowships/Imperial-College-COVID19-NPI-modelling-16-03-2020.pdf">Ferguson et al. (2020) Impact of non-pharmaceutical interventions (NPIs) to reduce COVID19 mortality and healthcare demand.</a>

