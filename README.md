# Description

The ```jumpID``` package is a tool to identify, analyze and predict the location of dispersal jumps in recent biological invasions. It is presented as an open access package with a simulated dataset. You can use this package to identify and analyze jump dispersal in biological invasions with the following steps:

## Clone this project locally

Open your Terminal or git shell, and set the working directory to the folder where you want the project to be stored using cd. Then, type:

```
git clone git@gitlab.com:nbelouard/jumpID.git
```

## (optional) Prepare your R for the analyses

The jumpID package relies on a variety of other R packages to function. They are specified as Imports in the DESCRIPTION file, and will be automatically installed with the jumpID package. If you wish to prepare the R environment for the jumpID package manually, you can do so by pasting this command into R or RStudio.

```
install.packages(c('tidyverse', 'here', 'magrittr', 'rmarkdown', 'ggplot2', 'pkgdown', 'knitr', 'sf', 'maps', 'DescTools', 'geosphere', 'leaflet', 'dplyr', 'gridExtra', 'ape', 'spdep', 'roxygen2'))
```

## Access and reproduce this analysis

If you wish to reproduce the analyses, once the project is cloned locally:
- access the folder and open the jumpID.Rproj file to open the project in Rstudio
- install the package using the R button "install and restart" in the Build tab of Rstudio. 

We do not offer the possibility to execute all vignettes and render a site using pkgdown since (1) most analyses require a long computation (e.g. distances calculation), and (2) you will need to modify the vignettes with your own data.

You can manually access the analyses by opening the vignettes (folder vignettes/), modifying the code and running them.

Vignette 1 generates simulated data to execute Vignette 2 and give users the opportunity to test the functions and see the format required to execute the analysis.

# References

For further information, contact @nbelouard
