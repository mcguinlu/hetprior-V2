% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hetprior.R
\name{hetprior}
\alias{hetprior}
\title{Informative bayesian prior lookup for heterogenity in meta-analyses}
\usage{
hetprior(priorid, statistic, details)
}
\arguments{
\item{priorid}{Prior ID number.}

\item{statistic}{Descriptive statistic required. Options allowed are "mean" or "sd" for lognormal, log(t), and logit(t) distributions,
and "shape" or "scale" for inverse gamma distributions.}

\item{details}{Outputs information on the selected  console. Default = FALSE.}
}
\value{
\code{hetprior} returns the specific descriptive statistic for a specified informative prior distribtution for the
conditions
}
\description{
\code{hetprior} returns an informative Bayesian prior based on the conditions
specified
}
\details{
This function uses data from five original studies to provide a repository of
informative priors for the heterogeneity observed in meta-analysis of studies
in particular areas of health care.
}
\examples{
 x <- hetprior(45, "mean")
 y <- hetprior(45, "sd")
 x <- hetprior(2, "mean", details = TRUE)

}
\references{
Turner, Rebecca M., Jonathan Davey, Mike J. Clarke, Simon G.
 Thompson, and Julian PT Higgins. "Predicting the extent of heterogeneity in
 meta-analysis, using empirical data from the Cochrane Database of Systematic
 Reviews." International journal of epidemiology 41, no. 3 (2012): 818-827.

 Turner, Rebecca M., Dan Jackson, Yinghui Wei, Simon G. Thompson, and Julian
 PT Higgins. "Predictive distributions for between?study heterogeneity and
 simple methods for their application in Bayesian meta?analysis." Statistics
 in medicine 34, no. 6 (2015): 984-998.

 Rhodes, Kirsty M., Rebecca M. Turner, Ian R. White, Dan Jackson, David J.
 Spiegelhalter, and Julian PT Higgins. "Implementing informative priors for
 heterogeneity in meta?analysis using meta?regression and pseudo
 data." Statistics in medicine 35, no. 29 (2016): 5495-5511.

 Rhodes, Kirsty M., Rebecca M. Turner, and Julian PT Higgins. "Predictive
 distributions were developed for the extent of heterogeneity in
 meta-analyses of continuous outcome data." Journal of Clinical
 Epidemiology 68, no. 1 (2015): 52-60.

 Rhodes, Kirsty M., Rebecca M. Turner, and Julian PT Higgins. "Empirical
 evidence about inconsistency among studies in a pair?wise
 meta?analysis." Research synthesis methods 7, no. 4 (2016): 346-370.|
}
\seealso{
This \href{https://mcguinlu.shinyapps.io/shiny/}{Shiny app} allows interactive look-up of priors.
}
\author{
Luke A McGuinness \email{luke.mcguinness@bristol.ac.uk}
}
