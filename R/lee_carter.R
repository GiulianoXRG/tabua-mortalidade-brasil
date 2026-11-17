#' Monta as matrizes de óbitos e exposição para o Lee-Carter
#'
#' Nas idades suavizadas (a partir de `idade_suaviz`), os óbitos usados são
#' \eqn{\hat D_x = \hat m_x E_x}, com \eqn{\hat m_x} da tábua (Kannisto). Isso
#' mantém o Lee-Carter coerente com a tábua e evita o ruído de declaração
#' de idade nas idades muito avançadas.
#'
#' @param mx Saída `mx` de [construir_tabuas()].
#' @param sexo Sexo.
#' @param anos Anos usados.
#' @param idades Idades usadas.
#' @param idade_suaviz Idade a partir da qual os óbitos são os ajustados.
#' @return Lista com as matrizes `Dxt` e `Ext` (idades x anos).
#' @export
matrizes_lc <- function(mx, sexo, anos, idades = 0:100, idade_suaviz = 80) {
  d <- mx |>
    dplyr::filter(.data$sexo == !!sexo, .data$ano %in% anos, .data$idade %in% idades) |>
    dplyr::mutate(obitos_lc = dplyr::if_else(.data$idade >= idade_suaviz,
                                             .data$mx * .data$expostos, .data$obitos)) |>
    dplyr::arrange(.data$ano, .data$idade)
  para_matriz <- function(v) {
    matrix(v, nrow = length(idades), ncol = length(anos),
           dimnames = list(idades, anos))
  }
  list(Dxt = para_matriz(d$obitos_lc), Ext = para_matriz(d$expostos))
}

#' Ajusta o modelo de Lee-Carter (Poisson, link log)
#'
#' \eqn{D_{xt} \sim Poisson(E_{xt} \mu_{xt})}, \eqn{\log \mu_{xt} = a_x + b_x k_t},
#' com as restrições usuais \eqn{\sum b_x = 1} e \eqn{\sum k_t = 0}
#' (`StMoMo::lc(link = "log")`).
#'
#' @inheritParams matrizes_lc
#' @return Objeto `fitStMoMo`.
#' @export
fit_lc <- function(mx, sexo, anos, idades = 0:100) {
  m <- matrizes_lc(mx, sexo, anos, idades)
  suppressWarnings(
    StMoMo::fit(StMoMo::lc(link = "log"), Dxt = m$Dxt, Ext = m$Ext,
                ages = idades, years = anos, verbose = FALSE)
  )
}

#' Extrai os parâmetros do Lee-Carter em formato tidy
#'
#' @param ajuste Objeto `fitStMoMo`.
#' @return Lista com `idade` (`ax`, `bx`) e `kt` (`ano`, `kt`).
#' @export
parametros_lc <- function(ajuste) {
  list(
    idade = tibble::tibble(idade = as.integer(ajuste$ages),
                           ax = as.numeric(ajuste$ax),
                           bx = as.numeric(ajuste$bx)),
    kt = tibble::tibble(ano = as.integer(ajuste$years),
                        kt = as.numeric(ajuste$kt))
  )
}

#' Expectativa de vida a partir de uma matriz de taxas
#'
#' @param taxas Matriz idades x anos de taxas centrais.
#' @param sexo Sexo (para a0).
#' @param idades_e Idades em que a expectativa é extraída.
#' @return Matriz `length(idades_e)` x anos.
#' @export
expectativas_de_taxas <- function(taxas, sexo, idades_e = c(0, 65)) {
  idades <- as.integer(rownames(taxas))
  apply(taxas, 2, function(m) {
    life_table(m, idades, sexo)$ex[match(idades_e, idades)]
  }) |> matrix(nrow = length(idades_e))
}

#' Projeta o Lee-Carter: kt, e0 e e65 com intervalos
#'
#' O índice \eqn{k_t} é projetado por ARIMA(0,1,0) com constante (passeio
#' aleatório com drift). Os intervalos de \eqn{k_t} vêm de
#' `forecast()`; os de e0 e e65 vêm de `nsim` trajetórias simuladas
#' (`simulate()`), que propagam a incerteza de \eqn{k_t} até a tábua.
#'
#' @param ajuste Objeto `fitStMoMo`.
#' @param sexo Sexo.
#' @param ate Último ano projetado.
#' @param nivel Níveis dos intervalos (%).
#' @param nsim Número de simulações.
#' @param semente Semente aleatória.
#' @return Lista com `kt` (histórico + projeção com limites), `e`
#'   (e0 e e65 projetados com mediana e limites) e `taxas` (matriz central).
#' @export
projetar_lc <- function(ajuste, sexo, ate = 2050, nivel = c(80, 95),
                        nsim = 1000, semente = 2024) {
  h <- ate - max(ajuste$years)
  prev <- forecast::forecast(ajuste, h = h, level = nivel,
                             kt.method = "iarima", kt.order = c(0, 1, 0),
                             kt.include.constant = TRUE)
  anos_f <- as.integer(colnames(prev$rates))

  kt_proj <- tibble::tibble(
    ano = anos_f, kt = as.numeric(prev$kt.f$mean),
    inf80 = as.numeric(prev$kt.f$lower[, , "80%"]),
    sup80 = as.numeric(prev$kt.f$upper[, , "80%"]),
    inf95 = as.numeric(prev$kt.f$lower[, , "95%"]),
    sup95 = as.numeric(prev$kt.f$upper[, , "95%"]),
    tipo = "Projetado"
  )
  kt <- dplyr::bind_rows(
    dplyr::mutate(parametros_lc(ajuste)$kt, tipo = "Ajustado"),
    kt_proj
  )

  sim <- simulate(ajuste, nsim = nsim, h = h, seed = semente,
                  kt.method = "iarima", kt.order = c(0, 1, 0),
                  kt.include.constant = TRUE)
  e_sim <- vapply(seq_len(nsim), function(i) {
    expectativas_de_taxas(sim$rates[, , i, drop = TRUE], sexo)
  }, matrix(0, 2, h))
  qs <- c(0.025, 0.10, 0.5, 0.90, 0.975)
  e_q <- apply(e_sim, c(1, 2), stats::quantile, probs = qs)
  e_central <- expectativas_de_taxas(prev$rates, sexo)

  e <- purrr::map_dfr(1:2, function(j) {
    tibble::tibble(
      indicador = c("e0", "e65")[j], ano = anos_f,
      central = e_central[j, ],
      inf95 = e_q[1, j, ], inf80 = e_q[2, j, ], mediana = e_q[3, j, ],
      sup80 = e_q[4, j, ], sup95 = e_q[5, j, ]
    )
  })

  list(kt = kt, e = e, taxas = prev$rates)
}

#' Resíduos de deviance do Lee-Carter em formato tidy
#'
#' @param ajuste Objeto `fitStMoMo`.
#' @return Tibble com `idade`, `ano`, `residuo`.
#' @export
residuos_lc <- function(ajuste) {
  r <- stats::residuals(ajuste)$residuals
  tibble::tibble(
    idade = rep(as.integer(rownames(r)), times = ncol(r)),
    ano = rep(as.integer(colnames(r)), each = nrow(r)),
    residuo = as.numeric(r)
  )
}

#' Backtesting do Lee-Carter
#'
#' Ajusta o modelo no período `ajuste`, projeta os anos de `teste` e compara
#' com a tábua observada: e0 (erro em anos) e \eqn{\log m_x} (RMSE por ano).
#'
#' @param mx Saída `mx` de [construir_tabuas()].
#' @param tabuas Saída `tabuas` de [construir_tabuas()].
#' @param sexo Sexo.
#' @param ajuste,teste Anos de ajuste e de teste.
#' @param nsim Número de simulações para os intervalos.
#' @return Tibble por ano de teste com e0 projetada (e intervalos), e0
#'   observada, erro e RMSE de log mx.
#' @export
backtest_lc <- function(mx, tabuas, sexo, ajuste = 2000:2014, teste = 2015:2019,
                        nsim = 500) {
  aj   <- fit_lc(mx, sexo, ajuste)
  proj <- projetar_lc(aj, sexo, ate = max(teste), nsim = nsim)

  obs_mx <- mx |>
    dplyr::filter(.data$sexo == !!sexo, .data$ano %in% teste) |>
    dplyr::select("ano", "idade", mx_obs = "mx")
  rmse <- tibble::tibble(
    ano = rep(as.integer(colnames(proj$taxas)), each = nrow(proj$taxas)),
    idade = rep(as.integer(rownames(proj$taxas)), times = ncol(proj$taxas)),
    mx_proj = as.numeric(proj$taxas)
  ) |>
    dplyr::inner_join(obs_mx, by = c("ano", "idade")) |>
    dplyr::group_by(.data$ano) |>
    dplyr::summarise(rmse_log_mx = sqrt(mean((log(.data$mx_proj) - log(.data$mx_obs))^2)))

  e0_obs <- tabuas |>
    dplyr::filter(.data$sexo == !!sexo, .data$ano %in% teste, .data$idade == 0) |>
    dplyr::select("ano", e0_obs = "ex")

  proj$e |>
    dplyr::filter(.data$indicador == "e0") |>
    dplyr::select("ano", e0_proj = "central", "inf95", "sup95") |>
    dplyr::inner_join(e0_obs, by = "ano") |>
    dplyr::inner_join(rmse, by = "ano") |>
    dplyr::mutate(sexo = sexo, erro_e0 = .data$e0_proj - .data$e0_obs,
                  dentro_ic95 = .data$e0_obs >= .data$inf95 & .data$e0_obs <= .data$sup95,
                  .before = 1)
}

#' Ajusta e projeta o Lee-Carter para todos os cenários e sexos
#'
#' @param mx Saída `mx` de [construir_tabuas()].
#' @param cfg Lista de [config_projeto()].
#' @param nsim Número de simulações.
#' @return Lista com tibbles `parametros`, `kt`, `e`, `residuos` e a lista
#'   de ajustes (`modelos`), todos identificados por `cenario` e `sexo`.
#' @export
rodar_lee_carter <- function(mx, cfg = config_projeto(), nsim = 1000) {
  chaves <- tidyr::expand_grid(cenario = names(cfg$cenarios_lc), sexo = cfg$sexos)
  res <- purrr::pmap(chaves, function(cenario, sexo) {
    aj   <- fit_lc(mx, sexo, cfg$cenarios_lc[[cenario]], cfg$idades)
    proj <- projetar_lc(aj, sexo, cfg$horizonte, cfg$nivel_ic, nsim)
    rot  <- function(df) dplyr::mutate(df, cenario = cenario, sexo = sexo, .before = 1)
    list(
      modelo     = aj,
      parametros = rot(parametros_lc(aj)$idade),
      kt         = rot(proj$kt),
      e          = rot(proj$e),
      residuos   = rot(residuos_lc(aj))
    )
  })
  nomes <- paste(chaves$cenario, chaves$sexo, sep = "_")
  list(
    modelos    = stats::setNames(purrr::map(res, "modelo"), nomes),
    parametros = purrr::map_dfr(res, "parametros"),
    kt         = purrr::map_dfr(res, "kt"),
    e          = purrr::map_dfr(res, "e"),
    residuos   = purrr::map_dfr(res, "residuos")
  )
}

#' Rótulos dos cenários do Lee-Carter
#' @keywords internal
rotulos_cenario <- function() {
  c(pre_covid = "Ajuste 2000–2019 (pré-COVID)",
    completo  = "Ajuste 2000–2023 (com COVID)")
}
