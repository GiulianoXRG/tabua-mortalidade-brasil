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
  }, format = "file"),

  # Etapa 3 — tábua de mortalidade ------------------------------------------
  tar_target(resultado_tabuas, construir_tabuas(obitos, populacao, cfg)),
  tar_target(tabuas, resultado_tabuas$tabuas),
  tar_target(mx, resultado_tabuas$mx),
  tar_target(ajustes_velhice, resultado_tabuas$ajustes),
  tar_target(comp_e0, comparar_e0_ibge(tabuas, indicadores_ibge)),
  tar_target(comp_censo, comparar_abertura_censo(mx, censo2022, 2022)),

  tar_target(tabuas_parquet, salvar_parquet(tabuas, "tabuas_mortalidade"), format = "file"),
  tar_target(mx_parquet, salvar_parquet(mx, "mx_exposicao"), format = "file"),
  tar_target(comp_e0_parquet, salvar_parquet(comp_e0, "comparacao_e0_ibge"), format = "file"),

  tar_target(fig_mx, salvar_figura(grafico_mx(mx), "mx_por_idade"), format = "file"),
  tar_target(fig_suaviz, salvar_figura(grafico_suavizacao(mx, 2019), "suavizacao_kannisto"),
             format = "file"),
  tar_target(fig_e0_ibge, salvar_figura(grafico_e0_ibge(comp_e0), "e0_vs_ibge"),
             format = "file"),
  tar_target(fig_censo, salvar_figura(grafico_abertura_censo(comp_censo), "abertura_90_censo"),
             format = "file"),

  # Etapa 4 — Lee-Carter ----------------------------------------------------
  tar_target(ibge_proj, ler_indicadores_ibge(ibge_ind_raw, 2000:cfg$horizonte)),
  tar_target(lc, rodar_lee_carter(mx, cfg, nsim = 1000)),
  tar_target(backtest, purrr::map_dfr(cfg$sexos, \(s) backtest_lc(
    mx, tabuas, s, cfg$backtest$ajuste, cfg$backtest$teste))),

  tar_target(lc_e_parquet, salvar_parquet(lc$e, "lc_projecao_e0_e65"), format = "file"),
  tar_target(lc_kt_parquet, salvar_parquet(lc$kt, "lc_kt"), format = "file"),
  tar_target(lc_par_parquet, salvar_parquet(lc$parametros, "lc_ax_bx"), format = "file"),
  tar_target(backtest_parquet, salvar_parquet(backtest, "lc_backtest"), format = "file"),

  tar_target(fig_ax_bx, salvar_figura(grafico_ax_bx(lc$parametros), "lc_ax_bx"),
             format = "file"),
  tar_target(fig_kt, salvar_figura(grafico_kt(lc$kt), "lc_kt"), format = "file"),
  tar_target(fig_e0_proj, salvar_figura(
    grafico_e_projetada(lc$e, tabuas, "e0", ibge_proj), "lc_e0_projetada"),
    format = "file"),
  tar_target(fig_e65_proj, salvar_figura(
    grafico_e_projetada(lc$e, tabuas, "e65"), "lc_e65_projetada"), format = "file"),
  tar_target(fig_residuos, salvar_figura(
    grafico_residuos(lc$residuos, "completo"), "lc_residuos", altura = 5.5),
    format = "file"),
  tar_target(fig_residuos_pre, salvar_figura(
    grafico_residuos(lc$residuos, "pre_covid"), "lc_residuos_pre_covid", altura = 5.5),
    format = "file"),
  tar_target(fig_backtest, salvar_figura(grafico_backtest(backtest), "lc_backtest"),
             format = "file")

  # Etapa 5 — relatório Quarto
)
