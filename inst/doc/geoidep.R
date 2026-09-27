## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  dpi = 300,
  out.width = "100%" 
)

## -----------------------------------------------------------------------------
library(geoidep)

## ----include=FALSE------------------------------------------------------------
providers <- get_data_sources()
layers_available <- get_providers()

loreto_prov <- get_provinces(show_progress = FALSE) |>
    subset(nombdep == "LORETO")
  
loreto_prov[["ubigeo"]] <- paste0(loreto_prov[["ccdd"]], loreto_prov[["ccpp"]])


## -----------------------------------------------------------------------------
providers

## -----------------------------------------------------------------------------
layers_available

## -----------------------------------------------------------------------------
# Region boundaries download (done once in the setup chunk above)
head(loreto_prov, 3)

## ----out.width='100%', out.height=250-----------------------------------------
library(mapgl)
library(sf)
maplibre_view(data = loreto_prov)

## ----include=FALSE------------------------------------------------------------
probe <- get_forest_loss_data(
  layer = "stock_bosque_perdida_provincia",
  ubigeo = loreto_prov[["ubigeo"]][1],
  show_progress = FALSE
  )


## -----------------------------------------------------------------------------
my_fun <- function(x){
  data <- get_forest_loss_data(
    layer = 'stock_bosque_perdida_provincia',
    ubigeo = loreto_prov[["ubigeo"]][x],
    show_progress = FALSE )
  return(data)
}
historico_list <- lapply(X = 1:nrow(loreto_prov),FUN = my_fun)
historico_df <- do.call(rbind.data.frame,historico_list)

## -----------------------------------------------------------------------------
# The first five rows
head(historico_df)

## ----fig.align='center'-------------------------------------------------------
library(ggplot2)
library(dplyr)

historico_prov <- historico_df |>
  inner_join(y = loreto_prov, by = "ubigeo")

promedio_loreto <- historico_prov |>
  group_by(anio) |>
  summarise(perdida = mean(perdida), .groups = "drop")

## ----fig.align='center', fig.height=7, fig.width=12---------------------------
ggplot(historico_prov, aes(x = anio, y = perdida)) +
  geom_line(aes(group = nombprov, color = "Provincia"), linewidth = 0.6) +
  geom_line(
    data = promedio_loreto,
    aes(color = "Promedio Loreto"),
    linewidth = 0.6,
    linetype = "dashed",
    ) +
  scale_color_manual(name = NULL, values = c("Provincia" = "red", "Promedio Loreto" = "black")) +
  facet_wrap(nombprov ~ .) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom") +
  labs(
    title = "Pérdida de bosque 2001-2025: provincias de Loreto vs. promedio departamental",
    caption = "Fuente: Geobosque",
    x = "",
    y = "")

