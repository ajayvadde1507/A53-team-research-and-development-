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

# ---- 1. Load data ----
data_path <- "xAPI-Edu-Data.csv"   # keep relative path
df <- read.csv(data_path, stringsAsFactors = FALSE)

# Quick structure check
str(df)
summary(df) 

# ---- 2. Basic cleaning & factor conversions ----
df <- df %>%
  mutate(across(where(is.character), ~ trimws(.)))

df <- df %>%
  mutate(
    gender = factor(gender),
    StageID = factor(StageID),
    GradeID = factor(GradeID),
    SectionID = factor(SectionID),
    Topic = factor(Topic),
    Semester = factor(Semester),
    Relation = factor(Relation)
  )

df$Class <- toupper(df$Class)
df$Class <- ifelse(df$Class == "L", "Low",
                   ifelse(df$Class == "M", "Medium",
                          ifelse(df$Class == "H", "High", NA)))
df$Class <- factor(df$Class, levels = c("Low","Medium","High"))

activity_vars <- c("raisedhands","VisITedResources",
                   "AnnouncementsView","Discussion")

df[activity_vars] <- lapply(df[activity_vars], function(x)
  as.numeric(as.character(x))) 
