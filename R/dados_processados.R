#' Marca os anos de pandemia de COVID-19
#'
#' @param dados Tibble com coluna `ano`.
#' @param anos_covid Anos considerados de COVID.
#' @return `dados` com a coluna lógica `covid`.
#' @export
marcar_covid <- function(dados, anos_covid = 2020:2021) {
  dplyr::mutate(dados, covid = .data$ano %in% anos_covid)
}

#' Grava um data frame em `data/processed/` no formato parquet
#'
#' @param dados Data frame.
#' @param nome Nome do arquivo, sem extensão.
#' @param dir Pasta de destino.
#' @return Caminho do arquivo gravado (para uso com `format = "file"`).
#' @export
salvar_parquet <- function(dados, nome, dir = here::here("data", "processed")) {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  destino <- file.path(dir, paste0(nome, ".parquet"))
  arrow::write_parquet(dados, destino)
  destino
}

#' Gráfico de cobertura do SIM em relação aos óbitos estimados pelo IBGE
#'
#' @param obitos Óbitos redistribuídos (ano x sexo x idade).
#' @param indicadores Saída de [ler_indicadores_ibge()].
#' @return Objeto ggplot.
#' @export
grafico_cobertura_sim <- function(obitos, indicadores) {
  obitos |>
    dplyr::group_by(.data$ano, .data$sexo) |>
    dplyr::summarise(obitos_sim = sum(.data$obitos), .groups = "drop") |>
    dplyr::inner_join(indicadores, by = c("ano", "sexo")) |>
    dplyr::mutate(cobertura = .data$obitos_sim / .data$obitos_ibge) |>
    ggplot2::ggplot(ggplot2::aes(.data$ano, .data$cobertura, colour = .data$sexo)) +
    ggplot2::annotate("rect", xmin = 2019.5, xmax = 2021.5, ymin = -Inf, ymax = Inf,
                      fill = "grey90") +
    ggplot2::geom_hline(yintercept = 1, linetype = "dashed", colour = "grey50") +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::geom_point(size = 1.5) +
    ggplot2::scale_y_continuous(labels = scales::percent) +
    escala_cor_sexo() +
    ggplot2::labs(
      title    = "Cobertura do SIM em relação aos óbitos estimados pelo IBGE",
      subtitle = "Óbitos registrados no SIM / óbitos implícitos nas Projeções IBGE 2024",
      x = "Ano", y = "Cobertura",
      caption  = "Faixa cinza: anos de COVID-19 (2020–2021). Fontes: SIM/DATASUS; IBGE."
    ) +
    tema_projeto()
}
