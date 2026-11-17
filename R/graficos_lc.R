#' Escala de cor dos cenários do Lee-Carter
#' @param ... Repassado a `scale_*_manual()`.
#' @param aesthetics Estética(s) afetadas.
#' @export
escala_cenario <- function(..., aesthetics = c("colour", "fill")) {
  pal <- paleta_projeto()
  ggplot2::scale_colour_manual(
    values = c(pre_covid = pal[["pre_covid"]], completo = pal[["completo"]]),
    labels = rotulos_cenario(), aesthetics = aesthetics, ...
  )
}

#' Parâmetros ax e bx do Lee-Carter
#'
#' @param parametros Tibble `parametros` de [rodar_lee_carter()].
#' @return Objeto ggplot.
#' @export
grafico_ax_bx <- function(parametros) {
  parametros |>
    tidyr::pivot_longer(c("ax", "bx"), names_to = "param", values_to = "valor") |>
    dplyr::mutate(param = factor(.data$param, c("ax", "bx"),
                                 c("ax (nível médio de log mx)", "bx (sensibilidade a kt)"))) |>
    ggplot2::ggplot(ggplot2::aes(.data$idade, .data$valor, colour = .data$cenario,
                                 linetype = .data$sexo)) +
    ggplot2::geom_line(linewidth = 0.8) +
    ggplot2::facet_wrap(ggplot2::vars(.data$param), scales = "free_y") +
    escala_cenario() +
    ggplot2::labs(title = "Lee-Carter: parâmetros por idade",
                  x = "Idade", y = NULL, linetype = NULL) +
    tema_projeto() +
    ggplot2::theme(legend.box = "vertical")
}

#' Índice kt ajustado e projetado com intervalos
#'
#' @param kt Tibble `kt` de [rodar_lee_carter()].
#' @return Objeto ggplot.
#' @export
grafico_kt <- function(kt) {
  proj <- dplyr::filter(kt, .data$tipo == "Projetado")
  ggplot2::ggplot(kt, ggplot2::aes(.data$ano, .data$kt, colour = .data$cenario)) +
    ggplot2::annotate("rect", xmin = 2019.5, xmax = 2021.5, ymin = -Inf, ymax = Inf,
                      fill = "grey92") +
    ggplot2::geom_ribbon(data = proj, ggplot2::aes(ymin = .data$inf95, ymax = .data$sup95,
                                                   fill = .data$cenario),
                         alpha = 0.15, colour = NA) +
    ggplot2::geom_ribbon(data = proj, ggplot2::aes(ymin = .data$inf80, ymax = .data$sup80,
                                                   fill = .data$cenario),
                         alpha = 0.25, colour = NA) +
    ggplot2::geom_line(ggplot2::aes(linetype = .data$tipo), linewidth = 0.8) +
    ggplot2::scale_linetype_manual(values = c(Ajustado = "solid", Projetado = "dashed"),
                                   guide = "none") +
    ggplot2::facet_wrap(ggplot2::vars(.data$sexo)) +
    escala_cenario() +
    ggplot2::labs(title = "Lee-Carter: índice de mortalidade kt",
                  subtitle = "Passeio aleatório com drift; faixas de 80% e 95%",
                  x = "Ano", y = "kt",
                  caption = "Faixa cinza: anos de COVID-19 (2020–2021).") +
    tema_projeto()
}

#' Expectativa de vida observada e projetada
#'
#' @param e Tibble `e` de [rodar_lee_carter()].
#' @param tabuas Tábuas observadas ([construir_tabuas()]).
#' @param indicador `"e0"` ou `"e65"`.
#' @param ibge Opcional: indicadores do IBGE projetados (coluna `e0_ibge`),
#'   desenhados como referência.
#' @return Objeto ggplot.
#' @export
grafico_e_projetada <- function(e, tabuas, indicador = "e0", ibge = NULL) {
  idade <- if (indicador == "e0") 0 else 65
  obs <- dplyr::filter(tabuas, .data$idade == !!idade) |>
    dplyr::select("ano", "sexo", valor = "ex")
  proj <- dplyr::filter(e, .data$indicador == !!indicador)
  titulo <- if (indicador == "e0") "Expectativa de vida ao nascer (e0)" else
    "Expectativa de vida aos 65 anos (e65)"

  g <- ggplot2::ggplot() +
    ggplot2::annotate("rect", xmin = 2019.5, xmax = 2021.5, ymin = -Inf, ymax = Inf,
                      fill = "grey92") +
    ggplot2::geom_ribbon(data = proj, ggplot2::aes(.data$ano, ymin = .data$inf95,
                                                   ymax = .data$sup95, fill = .data$cenario),
                         alpha = 0.18) +
    ggplot2::geom_line(data = proj, ggplot2::aes(.data$ano, .data$central,
                                                 colour = .data$cenario), linewidth = 0.9) +
    ggplot2::geom_line(data = obs, ggplot2::aes(.data$ano, .data$valor),
                       colour = paleta_projeto()[["observado"]], linewidth = 0.9)
  if (!is.null(ibge) && indicador == "e0") {
    g <- g + ggplot2::geom_line(data = ibge, ggplot2::aes(.data$ano, .data$e0_ibge),
                                colour = paleta_projeto()[["oficial"]],
                                linetype = "dotted", linewidth = 0.8)
  }
  g +
    ggplot2::facet_wrap(ggplot2::vars(.data$sexo)) +
    escala_cenario() +
    ggplot2::labs(title = titulo,
                  subtitle = paste0("Preto: tábua observada; linhas coloridas: Lee-Carter com IC 95%",
                                    if (!is.null(ibge) && indicador == "e0") "; pontilhado: IBGE" else ""),
                  x = "Ano", y = "Anos",
                  caption = "Fontes: SIM/DATASUS; IBGE. Elaboração própria.") +
    tema_projeto()
}

#' Mapa de calor dos resíduos de deviance (idade x ano)
#'
#' @param residuos Tibble `residuos` de [rodar_lee_carter()].
#' @param cenario Cenário exibido.
#' @return Objeto ggplot.
#' @export
grafico_residuos <- function(residuos, cenario = "completo") {
  residuos |>
    dplyr::filter(.data$cenario == !!cenario) |>
    dplyr::mutate(residuo = pmax(pmin(.data$residuo, 6), -6)) |>
    ggplot2::ggplot(ggplot2::aes(.data$ano, .data$idade, fill = .data$residuo)) +
    ggplot2::geom_tile() +
    ggplot2::scale_fill_gradient2(low = "#1F5A96", mid = "white", high = "#C0392B",
                                  midpoint = 0, limits = c(-6, 6),
                                  name = "Resíduo") +
    ggplot2::facet_wrap(ggplot2::vars(.data$sexo)) +
    ggplot2::scale_x_continuous(expand = c(0, 0)) +
    ggplot2::scale_y_continuous(expand = c(0, 0)) +
    ggplot2::labs(title = "Resíduos de deviance do Lee-Carter",
                  subtitle = rotulos_cenario()[[cenario]],
                  x = "Ano", y = "Idade",
                  caption = "Resíduos truncados em ±6 para legibilidade.") +
    tema_projeto() +
    ggplot2::theme(legend.position = "right", legend.title = ggplot2::element_text())
}

#' Backtesting: e0 projetada versus observada
#'
#' @param bt Saída combinada de [backtest_lc()] para os dois sexos.
#' @return Objeto ggplot.
#' @export
grafico_backtest <- function(bt) {
  pal <- paleta_projeto()
  ggplot2::ggplot(bt, ggplot2::aes(.data$ano)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = .data$inf95, ymax = .data$sup95),
                         fill = pal[["destaque"]], alpha = 0.15) +
    ggplot2::geom_line(ggplot2::aes(y = .data$e0_proj, colour = "Projetado (ajuste 2000–2014)"),
                       linewidth = 0.9) +
    ggplot2::geom_point(ggplot2::aes(y = .data$e0_obs, colour = "Observado"), size = 2) +
    ggplot2::scale_colour_manual(values = c(`Projetado (ajuste 2000–2014)` = pal[["destaque"]],
                                            Observado = pal[["observado"]])) +
    ggplot2::facet_wrap(ggplot2::vars(.data$sexo), scales = "free_y") +
    ggplot2::labs(title = "Backtesting: e0 projetada para 2015–2019",
                  subtitle = "Lee-Carter ajustado em 2000–2014; faixa: IC 95%",
                  x = "Ano", y = "e0 (anos)") +
    tema_projeto()
}
