# A tutorial on hazard analysis

A set of tutorials on how to perform descriptive and inferential (Bayesian and Frequentist) discrete-time hazard analysis (a.k.a. event history analysis (EHA), survial analysis, duration analysis, transition analysis, failure-time analysis) for time-to-event data from psychological experiments, and discrete-time speed-accuracy tradeoff (SAT) analysis in case of choice RT data. Examples of time-to-event data include response times, saccade latencies, fixation durations, and perceptual dominance durations.

# Intended audience

This is mainly for experimental psychologists and cognitive neuroscientists.
But other folks might also find EHA/SAT useful as a general way to analyse time-to-event data and timed accuracy data.

# Basic components of the workflow

- [renv()](https://rstudio.github.io/renv/articles/renv.html) to manage R package versions
- [git](https://git-scm.com/book/en/v2/Getting-Started-About-Version-Control) for version control
- [GitHub](https://github.com/) for collaborating and sharing coding
- The [Tidyverse](https://www.tidyverse.org/) ecosystem for data wrangling and visualisation 
- [brms](https://paul-buerkner.github.io/brms/) for building Bayesian regression models
- The [papaja()](https://frederikaust.com/papaja_man/) package for writing reproducible manuscripts

# What's the easiest way to access the project?

If you want to see and work with the code, then:

1. Clone or download the project from github (https://github.com/sven-panis/Tutorial_Event_History_Analysis) to your local machine.
2. Open the reproducible_workflow.Rproj file and renv() will automatically bootstrap itself.
3. renv() will then ask if you want use renv::restore() to install all of the packages. Say yes.
4. At this point, you can use the project with the same package versions that were stored in the renv.lock file.

# Important files and folders

- Tutorial_1a.Rmd (creating descriptive statistics when there is a single independent variable).

- Tutorial_1b.Rmd (descriptives for 2 independent variables).

- Tutorial_2a.Rmd (generating inferential statistics : Bayesian discrete-time EHA).

- Tutorial_2b.Rmd (generating inferential statistics : Bayesian discrete-time conditional accuracy analysis).

- Tutorial_3a.Rmd (generating inferential statistics : Frequentist discrete-time EHA).

- Tutorial_3b.Rmd (generating inferential statistics : Frequentist discrete-time conditional accuracy analysis).

- Tutorial_4.Rmd (Simulating data sets, power and precision)

- Folder manuscript 

  * manuscript.Rmd (papaja Template)
  * manuscript.pdf
  * manuscript.tex
  * r-references.bib
  * extrareferences.bib (non-Zotero)
  * extrareferences2.bib
  * folder Suppl_material

- Folder sims (code for Figures 1, 2, and 3)

  * sims.Rmd
  * folder figures

- Folder Tutorial_1_descriptive_stats

  * folder data
  * folder figures
  * folder tables

- Folder Tutorial_2_Bayesian 

  * folder models
  * folder figures
  * folder tables

- Folder Tutorial_3_Frequentist 

- Folder Tutorial_4_planning

  * folder data
  * folder figures
  * folder models
