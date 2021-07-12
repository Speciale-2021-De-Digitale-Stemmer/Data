
#-------------------------------------------------------------------------
# INSTALL AND LOAD ALL RELEVANT PACKAGES
#-------------------------------------------------------------------------

### Install pacman to perform p_load
# install.packages("pacman")

## Install Radlibrary to gain access to 
# FB ad library
# devtools::install_github("facebookresearch/Radlibrary")

### Install and load relevant packages
pacman::p_load(tidyverse,            # used for data wrangling
               Radlibrary,           # used for applying functions related to FB ad library
               openxlsx,             # used for reading and writing Excel files
               gridExtra,            # used for creating tables in R 
               extrafont,            # used for loading extra fonts to use in tables and plots
               lubridate,            # used for converting Facebook date variable into YMD format
               koRpus,               # used for LIX function
               quanteda,             # used for text manipulation 
               caret,                # used for supervised machine learning
               ranger,               # used for random forest models specifically
               glmnet,               # used for applying lasso, ridge, and elastic-net regression
               parallel,             # Parallel processing to speed up cross-validation           
               doParallel,           # used for using more than 1 computer core)
               snow,                 # used for calculating number of clusters on computer
               tidytext,             # used for converting model results into tidy text
               stringr,              # used for text wrangling
               topicmodels,          # used for lda and stm
               haven,                # used for reading and writing SPSS files
               sjlabelled,           # used for retrieving variable labels
               irr,                  # used for inter-rater agreement analysis
               stargazer,            # used for creating tables compatible with Latex
               caTools,              # used for computing ROC curves
               survey,               # used for calculating raked weights
               plotly,               # used for interactive plots'
               cjoint,               # used for applying functions for conjoint analysis
               cregg,                # -- "" --
               viridis,              # used for special colors in ggplot2
               lfe,                  # used for wrangling with factor variables
               rvest,                # used for reading html tables directly from website
               ggrepel,              # used for repelling labels in ggplot
               emmeans,              # used for estimating marginal means
               RColorBrewer,         # used for coloring in ggplot      
               zoo,                  # used for filling out blank NA spaces in data
               rio,                  # used for importing all sheets from excel files
               ggpubr,               # used for having several ggplots in one plot
               plotly,               # used for creating interactive plots
               ggpubr,               # --"--
               performance,          # used for testing OLS conditions
               see,                  # used with the performance package
               qqplotr,              # used with the performance package
               reactable)            # used for creating tables

# Sentida package
# if(!require("devtools")) install.packages("devtools")
# devtools::install_github("Guscode/Sentida")
# library(Sentida)


### Load extra font families
# font_import(prompt = FALSE)
# font_install('fontcm')
# library(fontcm) # this package specifically adds 'computer modern' which is the LaTeX font
# loadfonts()

### Set standard colors
# Party colors
party.col <- c("#BF0418", # A
               "#E82583", # B
               "#00571F", # C
               "#004450", # D
               "#F04D46", # F
               "#12213f", # I
               "#E7D01E", # O
               "#005392", # V
               "#C21B3E", # Ø
               "#00FF00"  # Å
               )

# Blok colors
blok.col <- c("red3", "navy")

# Color-palette
standard.col <- c("blue3", "darkorange2", "green4", "firebrick4")

