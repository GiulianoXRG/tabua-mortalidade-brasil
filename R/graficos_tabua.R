#' Curvas de log(mx) por idade em anos selecionados
#'
#' @param mx Saída `mx` de [construir_tabuas()].
#' @param anos Anos exibidos.
#' @return Objeto ggplot.
#' @export
grafico_mx <- function(mx, anos = c(2000, 2010, 2019, 2021, 2023)) {
  mx |>
    dplyr::filter(.data$ano %in% anos) |>
    ggplot2::ggplot(ggplot2::aes(.data$idade, .data$mx,
                                 colour = factor(.data$ano))) +
    ggplot2::geom_line(linewidth = 0.7) +
    ggplot2::scale_y_log10(labels = scales::label_number(drop0trailing = TRUE)) +
    ggplot2::scale_colour_viridis_d(option = "D", end = 0.9) +
    ggplot2::facet_wrap(ggplot2::vars(.data$sexo)) +
    ggplot2::labs(title = "Taxas centrais de mortalidade por idade",
                  subtitle = "Escala logarítmica; idades 80+ suavizadas por Kannisto",
                  x = "Idade", y = "mx (log)",
                  caption = "Fontes: SIM/DATASUS; IBGE, Projeções da População 2024.") +
    tema_projeto()
}

#' Taxas observadas versus modelo de Kannisto nas idades avançadas
#'
#' @param mx Saída `mx` de [construir_tabuas()].
#' @param ano Ano exibido.
#' @return Objeto ggplot.
#' @export
grafico_suavizacao <- function(mx, ano = 2019) {
  d <- dplyr::filter(mx, .data$ano == !!ano, .data$idade >= 60)
  ggplot2::ggplot(d, ggplot2::aes(.data$idade)) +
    ggplot2::annotate("rect", xmin = 80, xmax = 89.5, ymin = 0, ymax = Inf,
                      fill = "grey92") +
    ggplot2::geom_point(ggplot2::aes(y = .data$mx_obs, colour = .data$sexo),
                        size = 1.4, alpha = 0.7) +
    ggplot2::geom_line(ggplot2::aes(y = .data$mx, colour = .data$sexo), linewidth = 0.9) +
    ggplot2::scale_y_log10() +
    escala_cor_sexo() +
    ggplot2::labs(title = sprintf("Suavização das idades avançadas (%d)", ano),
                  subtitle = "Pontos: mx observado; linha: tábua (Kannisto a partir de 80). Faixa cinza: idades do ajuste",
                  x = "Idade", y = "mx (log)") +
    tema_projeto()
}

#' Expectativa de vida ao nascer: calculada versus IBGE
#'
#' @param comp Saída de [comparar_e0_ibge()].
#' @return Objeto ggplot.
#' @export
grafico_e0_ibge <- function(comp) {
  pal <- paleta_projeto()
  comp |>
    dplyr::select("ano", "sexo", Calculada = "e0_calc", IBGE = "e0_ibge") |>
    tidyr::pivot_longer(c("Calculada", "IBGE"), names_to = "fonte", values_to = "e0") |>
    ggplot2::ggplot(ggplot2::aes(.data$ano, .data$e0, colour = .data$fonte)) +
    ggplot2::annotate("rect", xmin = 2019.5, xmax = 2021.5, ymin = -Inf, ymax = Inf,
                      fill = "grey90") +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::scale_colour_manual(values = c(Calculada = pal[["destaque"]],
                                            IBGE = pal[["oficial"]])) +
    ggplot2::facet_wrap(ggplot2::vars(.data$sexo)) +
    ggplot2::labs(title = "Expectativa de vida ao nascer (e0)",
                  subtitle = "Tábua calculada com dados do SIM versus IBGE (Projeções 2024)",
                  x = "Ano", y = "Anos",
                  caption = "Faixa cinza: anos de COVID-19 (2020–2021).") +
    tema_projeto()
}

#' Abertura do grupo 90+: modelo versus Censo 2022
#'
#' @param comp Saída de [comparar_abertura_censo()].
#' @return Objeto ggplot.
#' @export
grafico_abertura_censo <- function(comp) {
  pal <- paleta_projeto()
  ggplot2::ggplot(comp, ggplot2::aes(.data$idade, .data$prop, fill = .data$fonte)) +
    ggplot2::geom_col(position = "dodge", width = 0.8) +
    ggplot2::scale_x_continuous(breaks = 90:100,
                                labels = c(90:99, "100+")) +
    ggplot2::scale_y_continuous(labels = scales::percent) +
    ggplot2::scale_fill_manual(values = c(`Modelo (Kannisto)` = pal[["destaque"]],
                                          `Censo 2022` = pal[["oficial"]])) +
    ggplot2::facet_wrap(ggplot2::vars(.data$sexo)) +
    ggplot2::labs(title = "Distribuição da população de 90 anos ou mais (2022)",
                  subtitle = "População estacionária do modelo de Kannisto versus Censo 2022 (SIDRA 9514)",
                  x = "Idade", y = "% do grupo 90+") +
    tema_projeto()
}

#' Salva um gráfico em output/figures
#'
#' @param grafico Objeto ggplot.
#' @param nome Nome do arquivo sem extensão.
#' @param largura,altura Dimensões em polegadas.
#' @return Caminho do PNG (para `format = "file"`).
#' @export
salvar_figura <- function(grafico, nome, largura = 9, altura = 5) {
  caminho <- here::here("output", "figures", paste0(nome, ".png"))
  ggplot2::ggsave(caminho, grafico, width = largura, height = altura,
                  dpi = 150, bg = "white")
  caminho
}
