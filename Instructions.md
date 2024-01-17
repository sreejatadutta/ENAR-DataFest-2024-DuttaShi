Is Obesity Associated with the Worsening of Blood Pressure Control?
==============
**Sreejata Dutta** $^1$ | **Xiasong Shi** $^1$ 

$^1$ University of Kansas Medical Center, Kansas City, Kansas, USA

# Code Repository Instruction
As our codes are reproducible, you do not need to download any data prior to running our code. However, you will need to install R version 4.2.1 or above and SAS 9.4. Please follow the instructions to reproduce our statistical analysis method.

*Step 1:* Open R and ensure you have the following packages installed: "table1", "ggplot2", "RColorBrewer", "gridExtra", "ggpubr", "cardioStatsUSA", "dplyr", "haven", "tidyverse", "nhanesA".

*Step 2:* Run [preprocessing file in R](ENAR-DataFest_DataPreprocessing.R). You may want to change the working library as per your preferences using setwd("path"). This R code will create four CSV files: dat_2011_2012_new.csv, dat_2013_2014_new.csv, dat_2015_2016_new.csv, and dat_2017_2020_new.csv.

*Step 3:* Run [the data analysis](ENAR-DataFest_Obesity&ControlBP.sas). This SAS program will perform propensity score matching and DID analysis on both adjusted systolic and diastolic BP. Ensure you change the path names denoted at the beginning of the program so that the program can read your input from Step 2 and produce corresponding outputs in your preferred path.

*Step 4:* Reproduce [figures and table 1](<ENAR-DataFest_Tables & Graphs.R>) from the report using this R code. 

**Note:** You can read more about our project report by clicking [here](<Is Obesity Associated with the Worsening of Blood Pressure Control_DuttaShi.pdf>) and watch our 5 mins study description using this [link](https://www.youtube.com/watch?v=XAKAy2gXJCE). To go through our slide deck, click [here](<ENAR DataFest_DuttaShi.pptx>).
