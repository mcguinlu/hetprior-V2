# hetprior
[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![Build Status](https://travis-ci.org/mcguinlu/hetprior.svg?branch=master)](https://travis-ci.org/mcguinlu/hetprior)
[![CRAN\_Release\_Badge](http://www.r-pkg.org/badges/version-ago/hetprior)](https://CRAN.R-project.org/package=hetprior)

## Installing the `hetprior` R package
First ensure you have the `devtools` package installed:

    install.packages("devtools")
    library(devtools)

Then to install:
    
    install_github("mcguinlu/hetprior")
    library(hetprior)

To update the package, run the `install_github("mcguinlu/hetprior")` command again.

## Description
We have written the `hetprior` R package to allow for easy specifiation of prior distributions for use in Bayesian meta-analysis, based on user-specified conditions. The package contains two functions:

### 1. `hetprior()`
Main function. Filters a look-up table of informative prior distributions based on the prior ID supplied. Prior ID can be identified within r using the supporting `get_priorid()` function, or online via a [Shiny webapp](https://mcguinlu.shinyapps.io/shiny/). The specified descriptive statistic (e.g. mean or standard deviation) is then be assigned to a variable for subsequent use in a Bayesian meta-analysis.

### 2. `get_priorid()`
This Shiny gadget is used to interactively select the appropriate prior distribtuion based on user input. 

## Usage
### Example workflow using the `hetprior` package
Launch the Shiny gadget to allow easy selection of appropriate prior based on conditions:

    get_priorid()
    [1] 12
    priorid <- 12
    
> Need screenshot of gadget here, or better yet, a gif.    
    
Use the resulting prior ID to create variables representing the descriptive statistics of that prior distribution:

    priormean <- hetprior(priorid,"mean")
    priorsd <- hetprior(priorid,"sd")
         
Adding the `details = TRUE` option gives more information on the prior specification and descriptive statistics:
    
    priormean <- hetprior(4,"mean", details = TRUE)
    # Prior look-up results: 
    # Input: 
    #   Prior ID:                 4 
    #   Heterogeneity statistic:  Tau-squared 
    #   Data type:                Binary 
    #   Effect measure:           Log odds ratio 
    #   Distribution form:        Log normal 
    #   Type of Intervention:     Pharmacological vs placebo/control 
    #   Nature of outcome:        Subjective 
    #   Medical area:             Any 
    #   Average sample size:      Any 
    # 
    # Output: 
    #   Prior Mean      =   -2.13 
    #   Prior SD        =   1.58 
    #   Prior variance  =   2.4964 
    #   Prior Median    =   0.12 
    #   Low 95% CI      =   0.005 
    #   High 95% CI     =   2.63 
    # 
    # Notes: Fitted distribution reported as log-normal(u,o^2), where u and o are the Mean and SD presented on log scale. Median/range 
    # presented on untransformed scale. Subjective/semi-objective outcomes and non-pharmacoloical interventions defined in Table 2 of
    # the paper. 
    # 
    # Reference: Turner, Rebecca M., Jonathan Davey, Mike J. Clarke, Simon G. Thompson, and Julian PT Higgins. "Predicting the extent of 
    # heterogeneity in meta-analysis, using empirical data from the Cochrane Database of Systematic Reviews." International Journal of
    # Epidemiology 41, no. 3 (2012): 818-827.    
 
### Reproductibility
It may be tempting to assign the value returned by `get_priorid()` directly a variable, for example:

    priorid <- get_priorid()
    
However, for the sake of reproducibility, this approach should be avoided, as in the example above, the value of `priorid` is explicitly defined and recorded within the script.
  
## Relevant Publications 
Details of the informative prior distributions were extracted from the following 5 publications:
* Turner RM, Davey J, Clarke MJ, Thompson SG, Higgins JP. [Predicting the extent of heterogeneity in meta-analysis, using empirical data from the Cochrane Database of Systematic Reviews](https://doi.org/10.1093/ije/dys041). International Journal of Epidemiology. 2012;41(3):818-827.

* Rhodes KM, Turner RM, Higgins JPT. [Predictive distributions were developed for the extent of heterogeneity in meta-analyses of continuous outcome data](https://doi.org/10.1016/j.jclinepi.2014.08.012). Journal of Clinical Epidemiology. 2015;68(1):52-60.

* Turner RM, Jackson D, Wei Y, Thompson SG, Higgins JPT. [Predictive distributions for between-study heterogeneity and simple methods for their application in Bayesian meta-analysis](https://doi.org/10.1002/sim.6381). Statistics in Medicine. 2015;34(6):984-998.

* Rhodes KM, Turner RM, White IR, Jackson D, Spiegelhalter DJ, Higgins JPT. [Implementing informative priors for heterogeneity in meta‐analysis using meta‐regression and pseudo data](https://doi.org/10.1002/sim.7090). Statistics in Medicine. 2016;35(29):5495-5511.

* Rhodes KM, Turner RM, Higgins JPT. [Empirical evidence about inconsistency among studies in a pair‐wise meta‐analysis](https://doi.org/10.1002/jrsm.1193). Research Synthesis Methods. 2016;7(4):346-370.
