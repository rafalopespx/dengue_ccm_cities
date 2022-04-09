---
  title: "Dengue Cases and Climatic drivers on Rio de Janeiro city"
author: "Rafael Lopes Paixão da Silva"
institute: "IFT, Unesp"
date: " `r Sys.Date()`"
output:
  xaringan::moon_reader:
  includes:
  after_body: insert-logo.html
self-contained: true
lib_dir: libs
css: "xaringan-themer.css"
nature:
  highlightStyle: github
highlightLines: true
countIncrementalSlides: false
ratio: 16:9
---
  layout: true

```{r setup, include=FALSE}
options(htmltools.dir.version = FALSE)
knitr::opts_chunk$set(echo = FALSE)
```

```{r xaringan-themer, include = FALSE}
options(htmltools.dir.version = FALSE)
library(xaringanthemer)
library(xaringanExtra)
library(knitr)
library(DT)
library(RefManageR)
library(icons)
duo_accent(
  primary_color = "#A70000",
  secondary_color = "#ff0000",
  white_color = "white",
  header_font_google = google_font("Roboto Condensed"),
  text_font_google   = google_font("Roboto Condensed", "300", "300i"),
  code_font_google   = google_font("Ubuntu Mono"), text_font_size = "30px")
use_extra_styles(
  hover_code_line = TRUE, 
  mute_unhighlighted_code = TRUE)
```

---
  class: left

## Find me at

.left-column[
  
  `r fontawesome::fa("github")` GitHub: [https://github.com/rafalopespx/](https://github.com/rafalopespx/)
  
  `r fontawesome::fa("mail-bulk")` e-Mail: [lopes1313@gmail.com](mailto:lopes1313@gmail.com)
  
  `r fontawesome::fa("twitter")` Twitter: [@rafalpx](www.twitter.com/rafalpx)
  
]

.pull-right[
  
  ![profile](img/profile_pick.jpeg)
  
]

---
  
  ## About me
  
  + Ph.D. Student in Physics, in the Instituto de Física Teóric - IFT/ UNESP-SP

+ I am in the last year of my Ph.D. and planning on defending on end of Abril/beginning of May, 22.

+ With a exchange period, in the Barcelona Institute for Global Health, at Barcelona.

---
  
  ## Objectives and Questions
  
  **How Mean Temperature & Total Preciptation affects the counts of Cases due to Dengue in Brazil, during 2010-2019?**
  
  Objectives: 
  
  -1) To quantify causality by *EDM* of t2m and precip and counts of cases due to Dengue

-2) Understand on which level of aggregation (e.g. Health Regions, States, Macro Regions) this are more stronger

---
  
  ## Dengue Disease
  
  -1) vector borne disease, human-mosquito-human cycle

-2) incubation period of four- to six-day and persists until around the time fever abates, which is typically three to seven days, potential of cryptic transmissions

-3) Hyperendemic disease in Brazil, with anual cycles of outbreaks

---
  
  ## Data sources and definitions:
  
  - We have the Brazil national database of information system of severe notifications (SINAN), filtered for the cases of Dengue, aggregated by municipality, health regions, states, and/or any aggregation that is more suitable to the amount of cases.

- For the climate variable, we used the gridded daily ECVs taken from reanalysis products (ERA5Land) 

- We did a weighted mean area aggregation over the municipality (health regions, states, etc.) extent area, to create matching time series for each aggregation level.

---
  
  ## Characteristics of Dataset
  
  - ~+ 20.9 million of individual cases, with information of Sex, Age, Municipality of Residence, fromm 2000 to 2020

- ~+ 12 millions of individual cases, from 2010, less cases on 2016 to 2018, Zika outbreak

- Sparse data before 2000, due to non obligatory notification

---
  
  ```{r before 2000, echo=FALSE, message=FALSE, warning=FALSE, out.width=800, fig.align='center'}
knitr::include_graphics("img/dengue_cases_before_2000.png")
```
---
  
  ```{r after 2000, echo=FALSE, message=FALSE, warning=FALSE, out.width=800, fig.align='center'}
knitr::include_graphics("img/dengue_cases_after_2000.png")
```

---
  
  ## Characteristics of Dengue epidemics
  
  - Strong seasonality, right after the rainy season, relation with mosquito life cycle

- Hyperendemicity, huge peaks very narrow

- Before Covid-19 was the major public health problem in Brazil, has a low attack rate

---
  
  ```{r attackrate, echo=FALSE, message=FALSE, warning=FALSE, out.width=800, fig.align='center'}
knitr::include_graphics("img/map_after_2000_states.png")
```

---
  
  ## Empirical Dynamic Modelling
  
  Framework to analyse time series seeking for causality between them:
  
  - From the Takens Theorem we can reconstruct any dynamical system by any observation and its delays

---
  
  ```{r takens theorem, echo=FALSE, message=FALSE, warning=FALSE, out.width=800, fig.align='center'}
knitr::include_graphics("img/Takens Theorem.png")
```

---
  
  ## Empirical Dynamic Modelling
  
  Framework to analyse time series seeking for causality between them:
  
  - From the Takens Theorem we can reconstruct any dynamical system by any observation and its delays

- This reconstruction it is called __Shadow-Manifold__, it maps complete the dynamics into the original Manifold

---
  
  ```{r state space, echo=FALSE, message=FALSE, warning=FALSE, out.width=800, fig.align='center'}
knitr::include_graphics("img/State Space Reconstruction.png")
```

---
  
  ## Empirical Dynamic Modelling
  
  Framework to analyse time series seeking for causality between them:
  
  - From the Takens Theorem we can reconstruct any dynamical system by any observation and its delays

- This reconstruction it is called __Shadow-Manifold__, it maps complete the dynamics into the original Manifold

- Any two __Shadow_Manifolds__ that maps each other are __Causal__ linked

---
  
  ```{r causality, echo=FALSE, message=FALSE, warning=FALSE, out.width=800, fig.align='center'}
knitr::include_graphics("img/Causality.png")
```

---
  
  ## First Test
  
  - We tried the framework on the 10 cities with the most cases during the period, 2010-2019

- The criteria for causality is the convergence of the mapping 

- The direction of causality is the inverse of the mapping

- Next plots always __Cases Xmap T2m__, causality from t2m to Cases

---
  
  ```{r test, echo=FALSE, message=FALSE, warning=FALSE, out.width=800, fig.align='center'}
knitr::include_graphics("img/ccm_10_most_cities.png")
```

---
  
  ```{r all 10 most, echo=FALSE, message=FALSE, warning=FALSE, out.width=800, fig.align='center'}
knitr::include_graphics("img/ccm_all_10_most_cities.png")
```

---
  
  ## Cases Xmap t2m and Precipitation
  
  We ran the framework on the state level aggregation to ask for causality between **Cases** and Mean temperature **t2m**, 
**Cases** and Total Precipitation **precip**
  
  - __EDM__ by year and by the whole period

- Optimal Embedding Dimension for each State from **Cases** time-series only

- No attempt yet to see which is the optimal time for prediction

---
  
  ```{r cases_xmap_temp_whole_period, echo=FALSE, message=FALSE, warning=FALSE, out.width=800, fig.align='center'}
knitr::include_graphics("img/cases_xmap_temp_by_year_whole_period.png")
```

---
  
  ```{r cases_xmap_t2m_states, echo=FALSE, message=FALSE, warning=FALSE, out.width=800, fig.align='center'}
knitr::include_graphics("img/cases_xmap_temp_state_whole_period.png")
```


---
  
  ```{r cases_xmap_precip_whole_period, echo=FALSE, message=FALSE, warning=FALSE, out.width=800, fig.align='center'}
knitr::include_graphics("img/cases_xmap_precip_by_year_whole_period.png")
```

---
  
  ```{r cases_xmap_precip_states, echo=FALSE, message=FALSE, warning=FALSE, out.width=800, fig.align='center'}
knitr::include_graphics("img/cases_xmap_precip_state_whole_period.png")
```

---
  
  ## Rio de Janeiro
  
  - Pipeline implemented like as in the Ianeri et al.

- 1) Extract the seasonality of the drivers, 

2) Do the CCM with the Original time-series and 500 Surrogates series with the seasonality pattern

3) If the Original is above the 0.95th percentile of the $\rho$ distribution for the 50o Surrogates time-series, it is significant causal relation

4) [Not implemented yet] Estimate the force of causality for the significant drivers

---
  
  ```{r rj-plot, echo=FALSE, message=FALSE, warning=FALSE, out.width=800, fig.align='center'}
knitr::include_graphics('img/cases_rj.png')
```

---
  
  ```{r rj-temp-season, echo=FALSE, message=FALSE, warning=FALSE, out.width=800, fig.align='center'}
knitr::include_graphics('img/seasonality_temp_mean.png')
```

---
  
  ```{r rj-precip-season, echo=FALSE, message=FALSE, warning=FALSE, out.width=800, fig.align='center'}
knitr::include_graphics('img/seasonality_total_precip_mean.png')
```

---
  
  ```{r rj-boxplot, echo=FALSE, message=FALSE, warning=FALSE, out.width=600, fig.align='center'}
knitr::include_graphics('img/boxplot_var_rj.png')
```

---
  
  ```{r rj-boxplot-tps, echo=FALSE, message=FALSE, warning=FALSE, out.width=800, fig.align='center'}
knitr::include_graphics('img/boxplot_all_tp_type.png')
```

---
  
  ```{r rj-boxplot-sig, echo=FALSE, message=FALSE, warning=FALSE, out.width=800, fig.align='center'}
knitr::include_graphics('img/boxplot_drivers_tp_sig.png')
```

---
  
  ## Next Steps
  
  - ~~Implement the EDM on preciptation time series, causality from preciptation to Cases~~
  
  - ~~Improve the pipeline, review the library sizes and put correlation comparison~~
  
  - ~~Test for seasonality, with surrogating statistical testing~~
  
  - Test others spacial aggregations or other spacial locations(State Capitals?), 
like macro health regions

- ~~Test others time aggregations, like monthly, with states and macro health~~
  
  - Estimate the coefficients of causaility for RJ

---
  
  class: middle, center
# Thank You!
# ¡Gracias!
# Obrigado!

`r fontawesome::fa("github")` GitHub: [https://github.com/rafalopespx/](https://github.com/rafalopespx/)
`r fontawesome::fa("mail-bulk")`  [lopes1313@gmail.com](mailto:lopes1313@gmail.com)
`r fontawesome::fa("twitter")` Twitter: [@rafalpx](www.twitter.com/rafalpx)

---
  
  ```{r cases_xmap_temp_2011, echo=FALSE, message=FALSE, warning=FALSE, out.width=800, fig.align='center'}
knitr::include_graphics("img/cases_xmap_temp_year_2011.png")
```

---
  
  ```{r cases_xmap_temp_2012, echo=FALSE, message=FALSE, warning=FALSE, out.width=800, fig.align='center'}
knitr::include_graphics("img/cases_xmap_temp_year_2012.png")
```

---
  
  ```{r cases_xmap_temp_2013, echo=FALSE, message=FALSE, warning=FALSE, out.width=800, fig.align='center'}
knitr::include_graphics("img/cases_xmap_temp_year_2013.png")
```

---
  
  ```{r cases_xmap_temp_2014, echo=FALSE, message=FALSE, warning=FALSE, out.width=800, fig.align='center'}
knitr::include_graphics("img/cases_xmap_temp_year_2014.png")
```

---
  
  ```{r cases_xmap_temp_2015, echo=FALSE, message=FALSE, warning=FALSE, out.width=800, fig.align='center'}
knitr::include_graphics("img/cases_xmap_temp_year_2015.png")
```

---
  
  ```{r cases_xmap_temp_2016, echo=FALSE, message=FALSE, warning=FALSE, out.width=800, fig.align='center'}
knitr::include_graphics("img/cases_xmap_temp_year_2016.png")
```

---
  
  ```{r cases_xmap_temp_2017, echo=FALSE, message=FALSE, warning=FALSE, out.width=800, fig.align='center'}
knitr::include_graphics("img/cases_xmap_temp_year_2017.png")
```

---
  
  ```{r cases_xmap_temp_2018, echo=FALSE, message=FALSE, warning=FALSE, out.width=800, fig.align='center'}
knitr::include_graphics("img/cases_xmap_temp_year_2018.png")
```

---
  
  ```{r cases_xmap_temp_2019, echo=FALSE, message=FALSE, warning=FALSE, out.width=800, fig.align='center'}
knitr::include_graphics("img/cases_xmap_temp_year_2019.png")
```

---