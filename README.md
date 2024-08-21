---
title: "jumpID, an R package to differentiate diffusive spread and dispersal jumps in biological invasions"
author: 
- Nadege Belouard^[iEco lab at Temple University, Ecobio lab at the University of Rennes, nadege.belouard@gmail.com]
date: "`r Sys.Date()`"
output: 
  html_document:
    theme: cosmo
    toc: TRUE
    toc_float: TRUE
    toc_depth: 3
---
# jumpID, an R package to differentiate diffusive spread and dispersal jumps in biological invasions

## Description

While most dispersal events occur over short distances and result in a continuous species range, human-assisted dispersal promotes the occurrence of dispersal "jumps", and the establishment of satellite populations away from the core of the invasion. Distinguishing diffusive spread and jump dispersal is important to understand the process of invasion, its evolution, but also to take efficient management measures. Through a directional analysis of species occurrence data, the `jumpID` R package differentiates diffusive spread and dispersal jumps in biological invasions, and identifies secondary diffusion stemming from dispersal jumps. `jumpID` is presented as a GitHub R package applied to the example of the invasion of the spotted lanternfly in the United States.  

<p align = "center">
<img src="man/figures/2. jump_description.jpg" alt = "workflow of jumpID" width = "500"/>  
</p>

The spotted lanternfly, *Lycorma delicatula* (hereafter SLF) is an insect from China that is an invasive pest in the US. Since the initial detection of SLF in Pennsylvania in 2014, large-scale surveys were conducted to trace the progression of the invasion. A unique dataset summarizing SLF presence and absence in the US is available in the `lydemapr` R package and constitutes an opportunity to study the spread of the SLF.  

Use this package to identify and analyze dispersal jumps, diffusive spread and secondary diffusion in other biological invasions.


## Installation

### If you wish to visualize the tutorial only  
2 options:  
* Access the “Articles” tab of the <a href="https://nbelouard.github.io/jumpID">jumpID GitHub website</a>    
* Download the PDF tutorial in the vignettes folder of the <a href="https://github.com/nbelouard/jumpID">jumpID GitHub repository</a>


### If you wish to install the package and execute the tutorial  
In the R terminal tab, cd to the folder where you want to store the project, and type:

```
git clone https://github.com/nbelouard/jumpID.git
```

Access the content of the package by opening the `jumpID.Rproj` file in Rstudio. Install jumpID by typing in the R console:

```r 
devtools::install()
``` 

Finally, open the tutorial file (.Rmd) in the vignettes folder, and execute the code chunks.


### If you wish to use the package functions only, without using the tutorial  
Install the package by typing the following line of code in the R console:

```r 
devtools::install_github("nbelouard/jumpID", dependencies = TRUE)
library(jumpID)
```

The functions are now ready to use, and the help file is accessible by typing ?function_name.


## Citation

N. Belouard, S. De Bona, M.R. Helmus,  I.G. Smith, J.E. Behm, 2024. Identifying jump dispersal locations from occurrence data on invasive species spread: the case of the spotted lanternfly *Lycorma delicatula*. https://ieco-lab.github.io/jumpID 