# Pipeline reprodutível: tábua de mortalidade brasileira + Lee-Carter
# Execute com: targets::tar_make()

library(targets)
library(tarchetypes)

tar_option_set(
  packages = c(
    "dplyr", "tidyr", "purrr", "arrow", "ggplot2",
    "microdatasus", "brpop", "readxl", "StMoMo", "forecast", "here"
  ),
  format = "rds"
)

tar_source("R")

cfg <- config_projeto()

list(
  tar_target(config, config_projeto()),

  # Etapa 2 — download e tratamento -----------------------------------------

  ## Óbitos (SIM-DO), um ramo por ano
  tar_target(anos, cfg$anos),
  tar_target(sim_raw, baixar_sim_ano(anos), pattern = map(anos),
             format = "file", deployment = "main"),
  tar_target(obitos_brutos, agregar_sim_ano(sim_raw, cfg$idade_max),
             pattern = map(sim_raw)),
  tar_target(qualidade_sim, resumo_ignorados(obitos_brutos)),
  tar_target(obitos, redistribuir_ignorados(obitos_brutos, cfg$idades) |>
               marcar_covid(cfg$anos_covid)),

  ## População e indicadores oficiais (IBGE, Projeções Revisão 2024)
  tar_target(ibge_pop_raw, baixar_ibge("projecoes_2024_tab1_idade_simples.xlsx"),
             format = "file", deployment = "main"),
  tar_target(ibge_ind_raw, baixar_ibge("projecoes_2024_tab4_indicadores.xlsx"),
             format = "file", deployment = "main"),
  tar_target(populacao, ler_pop_ibge(ibge_pop_raw, cfg$anos) |>
               marcar_covid(cfg$anos_covid)),
  tar_target(indicadores_ibge, ler_indicadores_ibge(ibge_ind_raw, cfg$anos)),
  tar_target(censo_raw, baixar_censo_sidra(), format = "file", deployment = "main"),
  tar_target(censo2022, ler_censo_sidra(censo_raw)),
  tar_target(censo_parquet, salvar_parquet(censo2022, "censo2022_idade_simples"),
             format = "file"),
  tar_target(checagem_brpop, comparar_pop_brpop(populacao)),

  ## Saídas em parquet
  tar_target(obitos_parquet, salvar_parquet(obitos, "obitos_sim"), format = "file"),
  tar_target(pop_parquet, salvar_parquet(populacao, "populacao_ibge"), format = "file"),
  tar_target(ind_parquet, salvar_parquet(indicadores_ibge, "indicadores_ibge"),
             format = "file"),
  tar_target(qualidade_parquet, salvar_parquet(qualidade_sim, "qualidade_sim"),
             format = "file"),

  ## Gráfico de cobertura
  tar_target(fig_cobertura, {
    caminho <- here::here("output", "figures", "cobertura_sim.png")
    ggplot2::ggsave(caminho, grafico_cobertura_sim(obitos, indicadores_ibge),
                    width = 8, height = 4.5, dpi = 150, bg = "white")
    caminho
  }, format = "file")

  # Etapa 3 — tábua de mortalidade
  # Etapa 4 — Lee-Carter (ajuste, projeção, diagnóstico, backtesting)
  # Etapa 5 — relatório Quarto
)
