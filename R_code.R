# Analysis.R
# Team A53 – xAPI-Edu-Data
# Sections will be added by team members
# =====================================================
# Member 1: Data Loading & Cleaning
# Name: Vadde Ajay
# =====================================================

# ---- 0. Setup ----
packages <- c("tidyverse", "ggplot2", "car", "nnet", "broom", "rstatix", "ggpubr")
installed <- rownames(installed.packages())
for(p in packages){
  if(!(p %in% installed)) install.packages(p, repos = "https://cloud.r-project.org")
}

library(tidyverse)
library(ggplot2)
library(car)
library(nnet)
library(broom)
library(rstatix)
library(ggpubr)
