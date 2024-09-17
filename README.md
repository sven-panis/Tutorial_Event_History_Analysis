# Tutorial_Event_History_Analysis

A tutorial on how to perform descriptive, and inferential (Bayesian and Frequentist) discrete-time Event History Analyses (EHA) for time-to-event data from psychological experiments, such as response times, saccade latencies, fixation durations, perceptual dominance durations, etc.

# Intended audience

This is mainly for experimental psychologists and cognitive neuroscientists.
But other folks might also find it useful as a general way to analyse time-to-event data.

# Basic components of the workflow

- [renv()](https://rstudio.github.io/renv/articles/renv.html) to manage R package versions
- [git](https://git-scm.com/book/en/v2/Getting-Started-About-Version-Control) for version control
- [GitHub](https://github.com/) for collaborating and sharing coding
- The [Tidyverse](https://www.tidyverse.org/) ecosystem for data wrangling and visualisation 
- [brms](https://paul-buerkner.github.io/brms/) for building Bayesian regression models
- The [papaja()](https://frederikaust.com/papaja_man/) package for writing reproducible manuscripts

# What's the easiest way to access the project?

If you want to see and work with the code, then:

1. Clone or download the project from github to your local machine.
2. Open the reproducible_workflow.Rproj file and renv() will automatically bootstrap itself.
3. renv() will then ask if you want use renv::restore() to install all of the packages. Say yes.
4. At this point, you can use the project with the same package versions that were stored in the renv.lock file.

# Important files and folders

- Tutorial_1.Rmd (creating descriptive statistics when there is a single independent variable).
- Tutorial_2.Rmd (generating inferential statistics : Bayesian discrete-time EHA).
- Tutorial_3.Rmd (generating inferential statistics : Frequentist discrete-time EHA).
- Tutorial_4.Rmd (creating descriptive stats when there are two independent variables)

Folder manuscript 
- manuscript.Rmd (papaja Template)
- manuscript.pdf
- manuscript.tex
- r-references.bib
- extrareferences.bib

Folder Tutorial_1_descriptive_stats
- folder data
- folder figures
- folder tables

Folder Tutorial_2_Bayesian 
- folder models
- folder figures

Folder Tutorial_3_frequentist 

Folder Tutorial_4_descriptive_2IV



