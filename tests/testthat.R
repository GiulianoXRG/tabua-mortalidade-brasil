library(testthat)

# Carrega as funções do projeto (não é um pacote, então usamos source)
invisible(lapply(
  list.files(here::here("R"), pattern = "\\.R$", full.names = TRUE),
  source
))

test_dir(here::here("tests", "testthat"))
