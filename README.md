
<h1 align="center"> RHESSysML </h1>

<h4 align="center"> A comprehensive workflow to determine and visualize variable importance in RHESSys model output. </h4>

<p align="center">
    <a href="https://github.com/RHESSysML/RHESSysML/commits/main">
    <img src="https://img.shields.io/github/last-commit/RHESSysML/RHESSysML.svg?style=flat-square&logo=github&logoColor=white"
         alt="GitHub last commit">
    <a href="https://github.com/RHESSysML/RHESSysML/issues">
    <img src="https://img.shields.io/github/issues-raw/RHESSysML/RHESSysML.svg?style=flat-square&logo=github&logoColor=white"
         alt="GitHub issues">
    <a href="https://github.com/RHESSysML/RHESSysML/pulls">
    <img src="https://img.shields.io/github/issues-pr-raw/RHESSysML/RHESSysML.svg?style=flat-square&logo=github&logoColor=white"
         alt="GitHub pull requests">
    <img src="https://img.shields.io/github/repo-size/RHESSysML/RHESSysML?style=flat-square"
         alt="GitHub repo size">
</p>
      
<p align="center">
  <a href="#about">About</a> •
  <a href="#how-to-use">How to Use</a> •
  <a href="#choosing-a-model">Choosing a Model</a> •
  <a href="#wiki">Wiki</a> 
</p>

---

## About

<table>
<tr>
<td>
      
RHESSysML provides a template and worked example for quickly exploring and identifying interesting variable relationships in RHESSys output data. This workflow is intended for users after the RHESSys model has been run and calibrated. 

This repository contains the following directories:    
    
- `/R`: R functions used in the workflow.

- `/data`: Folder to place RHESSys data for use in the workflow. Also contains data sets used in the completed example for Sagehen Creek.

- `/notebook_templates`: Blank workflow notebooks for use with new datasets.

- `/notebooks`: Notebooks used in the completed example for Sagehen Creek.

  - `/supporting_docs`: Notebooks supporting key choices made in the workflow. 

- `shiny`: Files and subdirectories associated with the Shiny application for interactive visualization of results.

- `renv`: Files and subdirectories created by the `renv` package.

</td>
</tr>
</table>


## How to Use

1. Fork and clone this repository.     
        
2. Place your RHESSys data in the `data` folder.
        
**If this is your first time using this workflow, we suggest viewing the files within `notebooks` for steps 3-4 for more explanation and an example of a completed analysis.**   
        
3. In `notebook_templates`, use "data_preparation.Rmd" to prepare data. We **suggest aggregating by water year for the best results.** 
        
4. In `notebook_templates`, run "rf_variable_importance.Rmd" or "gb_variable_importance.Rmd". For most use cases, **"rf_variable_importance.Rmd" is preferred**.
        
5. In `shiny`, open "shiny_app.R" using RStudio and hit "Run App". The app can also be run via the command line using `R -e “shiny::runApp(‘/shiny’)”`.
        

## Choosing a Model

|                            | Random Forest      | Gradient Boosting |
| -------------------------- | :----------------: | :-------------: |
| Faster run time            |         ✔️         |        ❌        |
| Less tuning                |         ✔️         |        ❌        |
| Accurate predictive power  |         ✔️         |        ✔️        |
| Better maximum accuracy    |         ❌️         |        ✔️        |


## Wiki

Do you **need some help**? For help specific to this workflow, check the documentation and guidance within `notebooks`. For help with RHESSys, check the _articles_ from the [wiki](https://github.com/RHESSys/RHESSys/wiki/).
