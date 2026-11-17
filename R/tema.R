#' Paleta de cores do projeto
#'
#' Paleta consistente usada em todos os gráficos. Sexo masculino e feminino
#' têm cores fixas; cenários do Lee-Carter também.
#'
#' @return Vetor nomeado de cores hexadecimais.
#' @export
paleta_projeto <- function() {
  c(
    Masculino   = "#1F5A96",
    Feminino    = "#C0392B",
    pre_covid   = "#2E7D6B",
    completo    = "#B9770E",
    observado   = "#2B2B2B",
    oficial     = "#7F8C8D",
    destaque    = "#8E44AD"
  )
}

#' Tema ggplot2 do projeto
#'
#' Tema limpo baseado em `theme_minimal()`, com legenda embaixo e títulos
#' alinhados à esquerda.
#'
#' @param base_size Tamanho base da fonte.
#' @return Objeto `theme` do ggplot2.
#' @export
tema_projeto <- function(base_size = 12) {
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      plot.title.position = "plot",
      plot.title          = ggplot2::element_text(face = "bold"),
      plot.subtitle       = ggplot2::element_text(colour = "grey35"),
      plot.caption        = ggplot2::element_text(colour = "grey45", hjust = 0),
      panel.grid.minor    = ggplot2::element_blank(),
      legend.position     = "bottom",
      legend.title        = ggplot2::element_blank(),
      strip.text          = ggplot2::element_text(face = "bold")
    )
}

#' Escala de cor por sexo
#'
#' @param ... Argumentos repassados a `ggplot2::scale_colour_manual()`.
#' @export
escala_cor_sexo <- function(...) {
  ggplot2::scale_colour_manual(
    values = paleta_projeto()[c("Masculino", "Feminino")], ...
  )
}

#' Escala de preenchimento por sexo
#'
#' @param ... Argumentos repassados a `ggplot2::scale_fill_manual()`.
#' @export
escala_preench_sexo <- function(...) {
  ggplot2::scale_fill_manual(
    values = paleta_projeto()[c("Masculino", "Feminino")], ...
  )
}
