# EHDEN Cancer Survival Study Shiny
========================================================================================================================================================

## Introduction
This is the repository to view the cancer survival results from the EHDEN wp2 study (https://github.com/oxford-pharmacoepi/CancerSurvivalWp2Analysis)

## To run the local version of the shiny app:
1. Put your results folder 'Results_database_name.zip' file in data folder.
2. Open the project in the R session ('Shiny.Rproj')
3. Load all the necessary libraries using renv.
   - renv::activate()
   - renv::restore()
   - y (to accept all the packages that are needed to install, this step can take several minutes)
4. Run the local shiny app
   - shiny::runApp() or open up the UI.R file and click "Run App"
5. Youre local shiny app is ready!

Any problems with the Shiny please contact danielle.newby@ndorms.ox.ac.uk


