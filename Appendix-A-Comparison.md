---
title: "Appendix A: Feature comparison between \\pkg{Distance} and other packages"

---

There are four packages available for analysis of distance sampling data in \proglang{R} that we are aware of. All are available on CRAN.

Package  | Description  | Version  | Homepage
-------: | -----------: | -------: | ---------:
\pkg{Distance} | package described in the main paper | 0.9.6 | https://github.com/distancedevelopment/Distance
\pkg{Rdistance} | uses a similar approach to \pkg{Distance} | 1.3.2 | https://github.com/tmcd82070/Rdistance
\pkg{unmarked} | uses a heirarchical modelling approach | 0.11.0 | https://sites.google.com/site/unmarkedinfo/
\pkg{mrds}     | on which `Distance` is based  | 2.1.15 | https://github.com/distancedevelopment/mrds

This table provides a feature comparison of the packages:

Feature    |  `Distance` | `Rdistance` | `unmarked` | `mrds`
-------: | --------:  | ---------: | --------: | ----:
Line transects | x | x | x | x
Point transets | x | | x | x
Interval (binned) distances | x | | x | x
Exact distances | x | x | | x
Continuous individual level covariates | x |  | | x
Factor individual level covariates | x | x | | x
Transect level covariates | x |  | x | x
Objects in clusters | x | x | x | x
Left truncation | x  | x | | x
Half-normal key | x | x | x | x
Hazard-rate key | x | x |  x | x
Uniform key | x | x | x | x
Gamma key |  | x |  | x
Negative exponential key |  |  | x  |
Adjustment terms | x | x | | x
AIC adjustment selection | x | x | 
Monotonicity constraints | x |  | | x
Availability bias model | | | x |
Perception bias model | | | | x
Abundance estimation | x | x | x | x
Density estimation | x | x | x | x
User-defined likelihood functions | | x | | 


## Abundance estimation

`Distance` and `mrds` models can be used as part of a density surface model using \pkg{dsm}, which allows abundance to be modelled as a function of spatially varying covariates (such as location, sea depth, altitude etc). See @Miller:2013fq for more information.

\pkg{unmarked} also allows abundance to vary accoring to covariates, via the abundance part of the likelihood. See @Fiske:2011gv for more information on the package and @Royle:2004cd for more information on methodology.

Results \pkg{Rdistance} models can be use in combination with \proglang{R} modelling functions such as \code{lm}, \code{glm} etc to build abundance estimates which vary according to covariates. More information is available on the project wiki.






