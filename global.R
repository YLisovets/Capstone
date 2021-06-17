# global.R ----------------------------------------------------------------
# Coursera Data Science Capstone Project
# Shiny script for loading data into global environment
library(readr)
library(data.table)

pentadgrams <- setDT(read_rds("data/five_grams_final2.rds"))
setkey(pentadgrams, phrase)

quadrugrams <- setDT(read_rds("data/quadrugrams_final2.rds"))
setkey(quadrugrams, phrase)

trigrams <- setDT(read_rds("data/trigrams_final2.rds"))
setkey(trigrams, phrase)

bigrams <- setDT(read_rds("data/bigrams_final2.rds"))
setkey(bigrams, phrase)

vocabulary <- read_rds("data/vocabulary2.rds")
