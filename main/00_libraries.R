if (!require("pacman")) install.packages("pacman")

pkgs <- c(
  "dm",
  "here",
  "robotoolbox",
  "tidyverse"
)

pacman::p_load(pkgs, character.only = T)
