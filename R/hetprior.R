#'Informative bayesian prior lookup for heterogenity in meta-analyses
#'
#'\code{hetprior} returns an informative Bayesian prior based on the conditions
#'specified
#'
#'This function uses data from five original studies to provide a repository of
#'informative priors for the heterogeneity observed in meta-analysis of studies
#'in particular areas of health care.
#'
#'@usage hetprior(priorid, statistic, details)
#'
#'@param priorid Prior ID number.
#'@param statistic Descriptive statistic required. Options allowed are "mean" or "sd" for lognormal, log(t), and logit(t) distributions,
#'and "shape" or "scale" for inverse gamma distributions.
#'@param details Outputs information on the selected  console. Default = FALSE.
#'
#'
#'@examples
#'  x <- hetprior(45, "mean")
#'  y <- hetprior(45, "sd")
#'  x <- hetprior(2, "mean", details = TRUE)
#'
#'@return \code{hetprior} returns the specific descriptive statistic for a specified informative prior distribtution for the
#'conditions
#'
#'
#'@seealso This \href{https://mcguinlu.shinyapps.io/shiny/}{Shiny app} allows interactive look-up of priors.
#'
#'@references Turner, Rebecca M., Jonathan Davey, Mike J. Clarke, Simon G.
#'  Thompson, and Julian PT Higgins. "Predicting the extent of heterogeneity in
#'  meta-analysis, using empirical data from the Cochrane Database of Systematic
#'  Reviews." International journal of epidemiology 41, no. 3 (2012): 818-827.
#'
#'  Turner, Rebecca M., Dan Jackson, Yinghui Wei, Simon G. Thompson, and Julian
#'  PT Higgins. "Predictive distributions for between?study heterogeneity and
#'  simple methods for their application in Bayesian meta?analysis." Statistics
#'  in medicine 34, no. 6 (2015): 984-998.
#'
#'  Rhodes, Kirsty M., Rebecca M. Turner, Ian R. White, Dan Jackson, David J.
#'  Spiegelhalter, and Julian PT Higgins. "Implementing informative priors for
#'  heterogeneity in meta?analysis using meta?regression and pseudo
#'  data." Statistics in medicine 35, no. 29 (2016): 5495-5511.
#'
#'  Rhodes, Kirsty M., Rebecca M. Turner, and Julian PT Higgins. "Predictive
#'  distributions were developed for the extent of heterogeneity in
#'  meta-analyses of continuous outcome data." Journal of Clinical
#'  Epidemiology 68, no. 1 (2015): 52-60.
#'
#'  Rhodes, Kirsty M., Rebecca M. Turner, and Julian PT Higgins. "Empirical
#'  evidence about inconsistency among studies in a pair?wise
#'  meta?analysis." Research synthesis methods 7, no. 4 (2016): 346-370.|
#'
#'
#'@author Luke A McGuinness \email{luke.mcguinness@@bristol.ac.uk}
#'
#'@export


hetprior <- function(priorid, statistic, details=FALSE){

# Subset data based on Prior ID ----

  Prior.ID <- NULL
  prior.data <- subset(hetdata, Prior.ID == priorid)

# Prior available? ----

  prioravailable <- prior.data[1, 1]

  if (is.na(prioravailable) == TRUE) {

    stop("No prior available for this specification
          Double check the Prior ID")
  }

# Generate output variables ----

  priorhetstat <- as.character(prior.data[1, 2])
  priordatatype <- as.character(prior.data[1, 3])
  prioreffectmeasure <- as.character(prior.data[1, 4])
  priordistributionform <- as.character(prior.data[1, 5])
  priorinterventiontype <- as.character(prior.data[1, 6])
  prioroutcomenature <- as.character(prior.data[1, 7])
  priormedicalarea <- as.character(prior.data[1, 8])
  priorsamplesize <- as.character(prior.data[1, 9])
  priormean <- prior.data[1, 10]
  priorsd <- prior.data[1, 11]
  priorvar <- priorsd * priorsd
  priormedian <- as.character(prior.data[1, 12])
  priorlow95ci <- as.character(prior.data[1, 13])
  priorhigh95ci <- as.character(prior.data[1, 14])
  Notes <- as.character(prior.data[1, 15])
  Reference <- as.character(prior.data[1, 16])

# Display output for details =TRUE

  if (details == TRUE){
    if (priordistributionform == "Inverse gamma"){
      cat("\n")
      cat("Prior look-up results:", "\n")
      cat("Input:", "\n")
      cat("   Prior ID:                ", priorid, "\n")
      cat("   Heterogeneity statistic: ", priorhetstat, "\n")
      cat("   Data type:               ", priordatatype, "\n")
      cat("   Effect measure:          ", prioreffectmeasure, "\n")
      cat("   Distribution form:       ", priordistributionform, "\n")
      cat("   Type of Intervention:    ", priorinterventiontype, "\n")
      cat("   Nature of outcome:       ", prioroutcomenature, "\n")
      cat("   Medical area:            ", priormedicalarea, "\n")
      cat("   Average sample size:     ", priorsamplesize, "\n")
      cat("Output:", "\n")
      cat("   Prior shape  = ", priormean, "\n")
      cat("   Prior scale  = ", priorsd, "\n")
      cat("\n", "Notes:", Notes, "\n")
      cat("Reference:", Reference, "\n")
    } else {
      cat("\n")
      cat("Prior look-up results:", "\n")
      cat("Input:", "\n")
      cat("   Prior ID:                ", priorid, "\n")
      cat("   Heterogeneity statistic: ", priorhetstat, "\n")
      cat("   Data type:               ", priordatatype, "\n")
      cat("   Effect measure:          ", prioreffectmeasure, "\n")
      cat("   Distribution form:       ", priordistributionform, "\n")
      cat("   Type of Intervention:    ", priorinterventiontype, "\n")
      cat("   Nature of outcome:       ", prioroutcomenature, "\n")
      cat("   Medical area:            ", priormedicalarea, "\n")
      cat("   Average sample size:     ", priorsamplesize, "\n", "\n")
      cat("Output:", "\n")
      cat("   Prior Mean      =  ", priormean, "\n")
      cat("   Prior SD        =  ", priorsd, "\n")
      cat("   Prior variance  =  ", priorvar, "\n")
      cat("   Prior Median    =  ", priormedian, "\n")
      cat("   Low 95% CI      =  ", priorlow95ci, "\n")
      cat("   High 95% CI     =  ", priorhigh95ci, "\n", "\n")
      cat("Notes:", Notes, "\n", "\n")
      cat("Reference:", Reference, "\n")
    }
  }



  # Results ----
  if (statistic == "mean" | statistic == "shape"){
    return(priormean)
    }

  if (statistic == "sd" | statistic == "scale"){
    return(priorsd)
    }
}
