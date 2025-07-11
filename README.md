# A tutorial on event history analysis

This project contains a set of tutorials on how to perform descriptive and inferential (Bayesian and Frequentist) discrete-time event history analysis (EHA; a.k.a. hazard analysis, survial analysis, duration analysis, transition analysis, failure-time analysis) for time-to-event data from psychological experiments, and discrete-time speed-accuracy tradeoff (SAT) analysis in case of discrimination data. 
Examples of time-to-event data include response times, saccade latencies, fixation durations, and perceptual dominance durations.

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

Make sure you select the branch "main", which contains the latest version of the manuscript as posted 
on PsyArXiv: https://doi.org/10.31234/osf.io/57bh6

2. Open the Tutorial_EHA.Rproj file and renv() will automatically bootstrap itself.
3. renv() will then ask if you want use renv::restore() to install all of the packages. Say yes.
4. At this point, you can use the project with the same package versions that were stored in the renv.lock file.

# Organization of files and folders

## files

At the top level, there are several files.

- There is one R project file:

**Tutorial_EHA.Rproj**. 

- There are several R markdown files (and corresponding html files):

**Tutorial_1a.Rmd**. This file is used to create descriptive statistics when there is a single independent variable.

**Tutorial_1b.Rmd**. This file is used to create descriptive statistics for 2 independent variables.

**Tutorial_2a.Rmd**. This file is used to perform Bayesian discrete-time EHA.

**Tutorial_2b.Rmd**. This file is used to perform Bayesian discrete-time conditional accuracy analysis.

**Tutorial_3a.Rmd**. This file is used to perform Frequentist discrete-time EHA.

**Tutorial_3b.Rmd**. This file is used to perform Frequentist discrete-time conditional accuracy analysis.

**Tutorial_4.Rmd**. This file used to simulating data sets, power and precision.

**install_packages.Rmd**. This file is used to initially install packages. You can use renv instead of this file.

- There is one renv.lock file

**renv.lock**. This plain text file is produced by renv() and records all package versions.

- There is one R file:

**custom_functions.R**. This file contains our custom function (see section C in the supplemental material).

- There is one .bib file:

**refs_tutorials.bib**. This file contains the BibTeX entries used in the tutorials.




## folders ##

There are also seven folders: 

**/manuscript/**

This is where the manuscript .Rmd file and .pdf files are stored, along with the .bib files that contain references. 
For more information on using papaja() for manuscripts, see the [papaja manual](https://frederikaust.com/papaja_man/)

  * manuscript.Rmd (papaja Template)
  * manuscript.pdf
  * manuscript.tex
  * r-references.bib (BibTex entries for R and packages)
  * refs_manuscript.bib (BibTeX entries used in the manuscript)

The /Suppl_material/ subfolder contains the corresponding .Rmd and .pdf files for supplemental material, a .bib file (refs_suppl_material.bib) and Supplementary Figure 1 (Plot_paradigms.png).

The /reviews/ subfolder contains the corresponding .Rmd and .pdf files for responding to reviewer's comments, and a CreateFiguresReview.Rmd file which creates Supplementary Figure 1.

**/sims/**

This folder contains the file sims.Rmd used to create Figure 1.

The /figures/ subfolder contains Fig1_revision1.jpeg (and other figures from sims.Rmd).


**/renv/**

This folder contains the typical renv files and folders.

**/Tutorial_1_descriptive_stats/**

This folder contains three subfolders:

* The /data/ subfolder contains the original data file of Panis and Schmidt (2016), the wrangled data file, the input files for modeling, and an .Rdata file with the descriptive statistics.
* The /figures/ subfolder contains 12 figures (outputs from Tutorial_1a.Rmd and Tutorial_1b.Rmd)
* The /tables/ subfolder contains a .csv file used to create Table 3.

**/Tutorial_2_Bayesian/**

This folder contains three subfolders:

* The /figures/ subfolder contains various figures (outputs from Tutorial_2a.Rmd and Tutorial_2b.Rmd)
* The /models/ subfolder contains the fitted hazard and conditional accuracy models.
* The /tables/ subfolder contains various .csv files used to create Tables.

**/Tutorial_3_Frequentist/**

This folder contains two .RData files (frequentist models) and 2 Figures (comparing the Bayesian and Frequentist parameter estimates from hazard or conditional accuracy models).

**/Tutorial_4_planning/**

This folder contains three subfolders:

* The /data/ subfolder contains various .csv files (created in Tutorial_4.Rmd).
* The /figures/ subfolder contains various figures (outputs from Tutorial_4.Rmd) and 2 subfolders (sim1 and sim2) containing figures.
* The /models/ subfolder contains a fitted model (see Tutorial_4.Rmd).
