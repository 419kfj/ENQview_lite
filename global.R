options(shiny.sanitize.errors = FALSE)
library(shiny)
library(tidyverse)
library(gt)
library(gtsummary)
library(vcd)
library(purrr)
library(tibble)
library(showtext)
showtext::showtext_auto(TRUE)

# DESCRIPTIONファイルから情報を読み取る
desc_info <- read.dcf("DESCRIPTION")
app_version <- desc_info[1, "Version"]
