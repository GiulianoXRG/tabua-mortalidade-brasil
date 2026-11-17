#' Fator de separação a0 de Coale-Demeny
#'
#' Fração média do primeiro ano vivida por quem morre antes de completar 1
#' ano, segundo Coale e Demeny (1983), conforme Preston, Heuveline e
#' Guillot (2001, Tabela 3.3).
#'
#' @param m0 Taxa central de mortalidade na idade 0.
#' @param sexo `"Masculino"` ou `"Feminino"`.
#' @return Valor de a0.
#' @export
a0_coale_demeny <- function(m0, sexo) {
  if (sexo == "Masculino") {
    ifelse(m0 >= 0.107, 0.330, 0.045 + 2.684 * m0)
  } else {
    ifelse(m0 >= 0.107, 0.350, 0.053 + 2.800 * m0)
  }
}

#' Fator de separação a1 com força de mortalidade exponencial no intervalo
#'
#' Supõe que \eqn{\mu} decai exponencialmente entre as idades 1 e 2 à
#' taxa \eqn{k = \log(m_1/m_2)}. Nesse caso, o tempo médio vivido no
#' intervalo por quem morre nele é \eqn{1/k - 1/(e^k - 1)}, que é menor que
#' 0,5 quando a mortalidade cai com a idade (caso típico em 1 ano).
#'
#' @param m1,m2 Taxas centrais nas idades 1 e 2.
#' @return Valor de a1 (0,5 quando `m1 == m2`).
#' @export
a1_exponencial <- function(m1, m2) {
  k <- log(m1 / m2)
  if (!is.finite(k) || abs(k) < 1e-6) return(0.5)
  min(max(1 / k - 1 / (exp(k) - 1), 0.05), 0.95)
}

#' Tábua de mortalidade por idade simples
#'
#' Constrói a tábua de vida completa a partir das taxas centrais de
#' mortalidade \eqn{m_x}. A última idade é tratada como grupo aberto
#' (\eqn{q_\omega = 1}, \eqn{L_\omega = l_\omega / m_\omega}).
#'
#' Fatores de separação \eqn{a_x}: idade 0 por Coale-Demeny
#' ([a0_coale_demeny()]), idade 1 pela aproximação exponencial
#' ([a1_exponencial()]), 0,5 nas demais idades fechadas.
#'
#' @param mx Vetor de taxas centrais de mortalidade, uma por idade.
#' @param idades Vetor de idades simples consecutivas, começando em 0.
#' @param sexo `"Masculino"` ou `"Feminino"` (usado em a0).
#' @param raiz Raiz da tábua (\eqn{l_0}).
#' @return Tibble com `idade`, `mx`, `ax`, `qx`, `px`, `lx`, `dx`, `Lx`,
#'   `Tx`, `ex`.
#' @examples
#' life_table(mx = c(0.012, 0.0008, 0.0004, 0.5), idades = 0:3, sexo = "Feminino")
#' @export
life_table <- function(mx, idades = seq_along(mx) - 1L, sexo, raiz = 1e5) {
  n <- length(mx)
  stopifnot(n == length(idades), n >= 3, idades[1] == 0,
            all(diff(idades) == 1), all(mx > 0, na.rm = FALSE))

  ax <- rep(0.5, n)
  ax[1] <- a0_coale_demeny(mx[1], sexo)
  ax[2] <- a1_exponencial(mx[2], mx[3])
  ax[n] <- 1 / mx[n]

  qx <- mx / (1 + (1 - ax) * mx)
  qx <- pmin(qx, 1)
  qx[n] <- 1

  lx <- raiz * cumprod(c(1, 1 - qx[-n]))
  dx <- lx * qx
  Lx <- c(lx[-1], NA) + ax * dx
  Lx[n] <- lx[n] / mx[n]
  Tx <- rev(cumsum(rev(Lx)))
  ex <- Tx / lx

  tibble::tibble(idade = as.integer(idades), mx = mx, ax = ax, qx = qx,
                 px = 1 - qx, lx = lx, dx = dx, Lx = Lx, Tx = Tx, ex = ex)
}

#' Taxas de mortalidade, suavização das idades avançadas e exposições
#'
#' Para um ano e um sexo:
#' 1. calcula \eqn{m_x = D_x / E_x} de 0 a 89 (população do IBGE em idade
#'    simples);
#' 2. ajusta Kannisto (e Gompertz, para comparação) nas idades 80–89;
#' 3. substitui \eqn{m_x} a partir de `idade_suaviz` pelo modelo escolhido;
#' 4. abre o grupo 90+ da população em 90, ..., 100+ com
#'    [pesos_grupo_aberto()], gerando exposições para o Lee-Carter.
#'
#' @param obitos Óbitos do ano/sexo (`idade` 0..100, `obitos`).
#' @param pop População do ano/sexo (`idade` 0..90, sendo 90 = 90+, `pop`).
#' @param idade_suaviz Idade a partir da qual se usa o modelo ajustado.
#' @param faixa_ajuste Idades usadas no ajuste do modelo.
#' @param modelo Modelo usado na suavização (`"kannisto"` ou `"gompertz"`).
#' @param idade_max Grupo aberto final.
#' @return Lista com `mx` (tibble por idade) e `ajustes` (tibble com
#'   parâmetros e AIC de cada modelo).
#' @export
preparar_mx <- function(obitos, pop, idade_suaviz = 80, faixa_ajuste = 80:89,
                        modelo = "kannisto", idade_max = 100) {
  pop_aberta <- max(pop$idade)
  d <- obitos$obitos[match(faixa_ajuste, obitos$idade)]
  e <- pop$pop[match(faixa_ajuste, pop$idade)]

  ajustes <- lapply(c("kannisto", "gompertz"),
                    function(m) ajustar_velhice(d, e, faixa_ajuste, m))
  names(ajustes) <- c("kannisto", "gompertz")
  aj <- ajustes[[modelo]]

  pesos <- pesos_grupo_aberto(aj, idade_ini = pop_aberta, idade_max = idade_max)
  p_aberto <- pop$pop[pop$idade == pop_aberta]
  expo <- c(pop$pop[pop$idade < pop_aberta], p_aberto * pesos$peso)
  idades <- 0:idade_max

  mx_obs  <- obitos$obitos[match(idades, obitos$idade)] / expo
  mx_mod  <- mu_velhice(idades + 0.5, aj$a, aj$b, aj$modelo, aj$x0)
  mx_mod[idades == idade_max] <- m_aberto_modelo(aj, idade_max)

  list(
    mx = tibble::tibble(
      idade    = idades,
      obitos   = obitos$obitos[match(idades, obitos$idade)],
      expostos = expo,
      mx_obs   = mx_obs,
      mx       = ifelse(idades >= idade_suaviz, mx_mod, mx_obs)
    ),
    ajustes = purrr::map_dfr(ajustes, \(a) tibble::as_tibble(a[c(
      "modelo", "a", "b", "loglik", "aic", "convergiu")]))
  )
}

#' Constrói as tábuas de mortalidade de todos os anos e sexos
#'
#' @param obitos Óbitos redistribuídos (`ano`, `sexo`, `idade`, `obitos`).
#' @param populacao População IBGE (`ano`, `sexo`, `idade`, `pop`).
#' @param cfg Lista de [config_projeto()].
#' @return Lista com `tabuas`, `mx` e `ajustes`, todos tibbles com `ano` e
#'   `sexo`.
#' @export
construir_tabuas <- function(obitos, populacao, cfg = config_projeto()) {
  chaves <- tidyr::expand_grid(ano = cfg$anos, sexo = cfg$sexos)

  res <- purrr::pmap(chaves, function(ano, sexo) {
    o <- obitos[obitos$ano == ano & obitos$sexo == sexo, ]
    p <- populacao[populacao$ano == ano & populacao$sexo == sexo, ]
    prep <- preparar_mx(o, p, idade_suaviz = cfg$idade_suaviz,
                        idade_max = cfg$idade_max)
    tab <- life_table(prep$mx$mx, prep$mx$idade, sexo, cfg$raiz)
    list(
      tabua   = dplyr::mutate(tab, ano = ano, sexo = sexo, .before = 1),
      mx      = dplyr::mutate(prep$mx, ano = ano, sexo = sexo, .before = 1),
      ajustes = dplyr::mutate(prep$ajustes, ano = ano, sexo = sexo, .before = 1)
    )
  })

  list(
    tabuas  = purrr::map_dfr(res, "tabua") |> marcar_covid(cfg$anos_covid),
    mx      = purrr::map_dfr(res, "mx") |> marcar_covid(cfg$anos_covid),
    ajustes = purrr::map_dfr(res, "ajustes")
  )
}

#' Compara a expectativa de vida calculada com a oficial do IBGE
#'
#' @param tabuas Tábuas de [construir_tabuas()].
#' @param indicadores Saída de [ler_indicadores_ibge()].
#' @return Tibble com e0 e e60 calculadas, oficiais e as diferenças.
#' @export
comparar_e0_ibge <- function(tabuas, indicadores) {
  tabuas |>
    dplyr::filter(.data$idade %in% c(0, 60)) |>
    dplyr::select("ano", "sexo", "idade", "ex") |>
    tidyr::pivot_wider(names_from = "idade", values_from = "ex",
                       names_prefix = "e") |>
    dplyr::rename(e0_calc = "e0", e60_calc = "e60") |>
    dplyr::inner_join(indicadores, by = c("ano", "sexo")) |>
    dplyr::mutate(dif_e0 = .data$e0_calc - .data$e0_ibge,
                  dif_e60 = .data$e60_calc - .data$e60_ibge)
}

#' Compara a abertura do grupo 90+ com o Censo 2022
#'
#' @param mx Saída `mx` de [construir_tabuas()].
#' @param censo Saída de [ler_censo_sidra()].
#' @param ano Ano de comparação.
#' @return Tibble com a proporção de cada idade dentro do 90+, pelo método
#'   do modelo e pelo Censo.
#' @export
comparar_abertura_censo <- function(mx, censo, ano = 2022) {
  modelo <- mx |>
    dplyr::filter(.data$ano == !!ano, .data$idade >= 90) |>
    dplyr::group_by(.data$sexo) |>
    dplyr::mutate(prop = .data$expostos / sum(.data$expostos), fonte = "Modelo (Kannisto)") |>
    dplyr::ungroup() |>
    dplyr::select("sexo", "idade", "prop", "fonte")
  censo_p <- censo |>
    dplyr::filter(.data$idade >= 90) |>
    dplyr::group_by(.data$sexo) |>
    dplyr::mutate(prop = .data$pop / sum(.data$pop), fonte = "Censo 2022") |>
    dplyr::ungroup() |>
    dplyr::select("sexo", "idade", "prop", "fonte")
  dplyr::bind_rows(modelo, censo_p)
}

#' Valida Kannisto e Gompertz fora da faixa de ajuste (90–99) com o Censo 2022
#'
#' Ajusta os dois modelos em 80–89 (ano de 2022) e compara as taxas previstas
#' em 90–99 com as observadas, calculadas com os óbitos do SIM e a população
#' do Censo 2022, reescalada para o total 90+ do IBGE.
#'
#' @param obitos Óbitos redistribuídos.
#' @param populacao População IBGE.
#' @param censo Saída de [ler_censo_sidra()].
#' @param ano Ano da validação.
#' @return Tibble com `sexo`, `idade`, `mx_obs`, `kannisto`, `gompertz`.
#' @export
validar_velhice_censo <- function(obitos, populacao, censo, ano = 2022) {
  purrr::map_dfr(c("Masculino", "Feminino"), function(s) {
    o <- obitos[obitos$ano == ano & obitos$sexo == s, ]
    p <- populacao[populacao$ano == ano & populacao$sexo == s, ]
    cs <- censo[censo$sexo == s, ]
    x <- 80:89
    d <- o$obitos[match(x, o$idade)]
    e <- p$pop[match(x, p$idade)]
    k <- ajustar_velhice(d, e, x, "kannisto")
    g <- ajustar_velhice(d, e, x, "gompertz")
    xi <- 90:99
    fator <- p$pop[p$idade == 90] / sum(cs$pop[cs$idade >= 90])
    tibble::tibble(
      sexo = s, idade = xi,
      mx_obs = o$obitos[match(xi, o$idade)] / (cs$pop[match(xi, cs$idade)] * fator),
      kannisto = mu_velhice(xi + 0.5, k$a, k$b, "kannisto"),
      gompertz = mu_velhice(xi + 0.5, g$a, g$b, "gompertz")
    )
  })
}
