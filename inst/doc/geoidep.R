## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(geoidep)
library(ggplot2)

## -----------------------------------------------------------------------------
departaments <- get_departaments(show_progress = FALSE)

## -----------------------------------------------------------------------------
names(departaments)

## -----------------------------------------------------------------------------
# Display the first 10 rows of the data frame `departaments`
head(departaments)

## -----------------------------------------------------------------------------
ggplot() + 
  geom_sf(data = departaments) +
  theme_minimal()

