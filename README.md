# Description

The ```jumpID``` package is a tool to identify, analyze and predict the location of dispersal jumps in recent biological invasions. It is presented as an open access package with a simulated dataset. You can use this package to identify and analyze jump dispersal in biological invasions with the following steps:

## Clone this project locally

Open your Terminal or git shell, and set the working directory to the folder where you want the project to be stored using cd. Then, type:

```
git clone https://github.com/nbelouard/jumpID.git
```

## Access and reproduce this analysis

If you wish to reproduce the analyses, once the project is cloned locally:
- access the folder and open the jumpID.Rproj file to open the project in Rstudio
- install the package using the R button "install and restart" in the Build tab of Rstudio. 

We do not offer the possibility to execute all vignettes and render a site using pkgdown since (1) most analyses require a long computation (e.g. distances calculation), and (2) you will need to modify the vignettes with your own data.

You can manually access and run the analyses by opening the vignettes (folder vignettes/).

Vignette 01_Generate_dataset generates simulated data to execute Vignette 02_Identify_jumps.
Vignette 02_Identify_junps gives users the opportunity to test the functions to identify jumps and rarefy the jump dataset, as well as check the format required to execute the analysis.

# References

For further information, contact @nbelouard
