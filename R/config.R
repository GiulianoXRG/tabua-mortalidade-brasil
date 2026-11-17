#' Parâmetros globais do projeto
#'
#' Lista única com os parâmetros usados em todo o pipeline. Centralizar aqui
#' evita "números mágicos" espalhados pelo código.
#'
#' @return Lista nomeada com anos, idades, anos de COVID, horizonte de
#'   projeção e raiz da tábua.
#' @export
config_projeto <- function() {
  list(
    anos           = 2000:2023,
    anos_covid     = 2020:2021,
    idades         = 0:100,          # 100 = grupo aberto 100+
    idade_max      = 100,
    idade_suaviz   = 80,             # início da suavização (Kannisto)
    raiz           = 100000,         # l0
    sexos          = c("Masculino", "Feminino"),
    cenarios_lc    = list(
      pre_covid = 2000:2019,
      completo  = 2000:2023
    ),
    backtest       = list(ajuste = 2000:2014, teste = 2015:2019),
    horizonte      = 2050,
    nivel_ic       = c(80, 95)
  )
}
