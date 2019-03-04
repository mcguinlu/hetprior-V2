# hetprior
[![Project Status: WIP – Initial development is in progress, but there has not yet been a stable, usable release suitable for the public.](http://www.repostatus.org/badges/latest/wip.svg)](http://www.repostatus.org/#wip)
[![Build Status](https://travis-ci.org/mcguinlu/hetprior.svg?branch=master)](https://travis-ci.org/mcguinlu/hetprior)


## Background
This R package is designed to be used with the `hetprior` [web app](http://hetprior.info), a tool that allows for easy specifiation of prior conditions and auto-generates the R code needed.





## Installing the `hetprior` R package
First ensure you have the `devtools` package installed:

    install.packages("devtools")

Then to install:

    library(devtools)
    install_github("mcguinlu/hetprior")

To update the package, run the `install_github("mcguinlu/hetprior")` command again.




## Usage
`hetprior` allows users to easily access the descriptive statistics of set 




## Examples
General example:

    priormean <- hetprior(4,"mean")
    priorsd <- hetprior(4,"sd")
     
By adding the `details = TRUE` option:
    
    priormean <- hetprior(4,"mean", details = TRUE)
       
   
## Relevant Publications 

* Turner RM, Davey J, Clarke MJ, Thompson SG, Higgins JP. [Predicting the extent of heterogeneity in meta-analysis, using empirical data from the Cochrane Database of Systematic Reviews](https://doi.org/10.1093/ije/dys041). International Journal of Epidemiology. 2012;41(3):818-827.

* Rhodes KM, Turner RM, Higgins JPT. [Predictive distributions were developed for the extent of heterogeneity in meta-analyses of continuous outcome data](https://doi.org/10.1016/j.jclinepi.2014.08.012). Journal of Clinical Epidemiology. 2015;68(1):52-60.

* Turner RM, Jackson D, Wei Y, Thompson SG, Higgins JPT. [Predictive distributions for between-study heterogeneity and simple methods for their application in Bayesian meta-analysis](https://doi.org/10.1002/sim.6381). Statistics in Medicine. 2015;34(6):984-998.

* Rhodes KM, Turner RM, White IR, Jackson D, Spiegelhalter DJ, Higgins JPT. [Implementing informative priors for heterogeneity in meta‐analysis using meta‐regression and pseudo data](https://doi.org/10.1002/sim.7090). Statistics in Medicine. 2016;35(29):5495-5511.

* Rhodes KM, Turner RM, Higgins JPT. [Empirical evidence about inconsistency among studies in a pair‐wise meta‐analysis](https://doi.org/10.1002/jrsm.1193). Research Synthesis Methods. 2016;7(4):346-370.
