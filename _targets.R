# Pipeline reprodutível: tábua de mortalidade brasileira + Lee-Carter
# Execute com: targets::tar_make()

library(targets)
library(tarchetypes)

tar_option_set(
  packages = c(
    "dplyr", "tidyr", "purrr", "arrow", "ggplot2",
    "microdatasus", "brpop", "StMoMo", "forecast", "here"
  ),
  format = "rds"
)

tar_source("R")

list(
  tar_target(config, config_projeto())

  # Etapa 2 — download e tratamento (óbitos SIM + população)
  # Etapa 3 — tábua de mortalidade
  # Etapa 4 — Lee-Carter (ajuste, projeção, diagnóstico, backtesting)
  # Etapa 5 — relatório Quarto
)
