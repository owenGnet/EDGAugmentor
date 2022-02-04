library(shiny)
library(shinyjs)
library(shinyBS)
library(ggplot2)

library(DT)
library(shinydashboard)
library(htmltools)
library(dplyr)
library(stringr)
library(stringi)
library(tidyr)
library(glue)
library(rjson)
library(markdown)

source("utils.R")

tenk_folder <- "./www/tenk/"
tenk_md <- jsonlite::read_json(file.path(tenk_folder, "tenk_metadata_plus.json"), simplifyVector = TRUE)

tenks <- list.files(tenk_folder, pattern = "\\.html$")
name_matcher <- "^10K_([a-zA-Z_]+-\\d{8})(_pretty)?"
df_tenks <- as_tibble(tenks) %>%
  mutate(display_name = str_match(tenks, name_matcher)[, 2]) %>%
  mutate(key = str_match(tenks, name_matcher)[, 1]) %>%
  left_join(tenk_md) %>%
  relocate(filing_period, .before = display_name)

entity_type_choices <- list(
                "PERSON" = "person",
                "NORP" = "norp",
                "FACILITY" = "facility",
                "ORG" = "org",
                "GPE" = "gpe",
                "LOC" = "loc",
                "PRODUCT" = "product",
                "EVENT" = "event",
                "WORK OF ART" = "work_of_art",
                "LAW" = "law",
                "LANGUAGE" = "language",
                "DATE" = "date",
                "TIME" = "time",
                "QUANTITY" = "quantity",
                "ORDINAL" = "ordinal",
                "CARDINAL" = "cardinal",
                "PERCENT" = "percent",
                "MONEY" = "money"
            )

esg_type_choices <- list(
                "ENVIRONMENTAL" = "env",
                "SOCIAL" = "soc",
                "GOVERNANCE" = "gov"
            )
