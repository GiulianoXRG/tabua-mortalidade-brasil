#' Força de mortalidade dos modelos de idades avançadas
#'
#' Kannisto (logístico): \eqn{\mu(x) = a e^{b(x - x_0)} / (1 + a e^{b(x - x_0)})}.
#' Gompertz: \eqn{\mu(x) = a e^{b(x - x_0)}}.
#'
#' @param x Idades (podem ser fracionárias).
#' @param a,b Parâmetros do modelo (`a > 0`, `b > 0`).
#' @param modelo `"kannisto"` ou `"gompertz"`.
#' @param x0 Idade de referência da parametrização.
#' @return Vetor com \eqn{\mu(x)}.
#' @export
mu_velhice <- function(x, a, b, modelo = c("kannisto", "gompertz"), x0 = 80) {
  modelo <- match.arg(modelo)
  g <- a * exp(b * (x - x0))
  if (modelo == "kannisto") g / (1 + g) else g
}

#' Ajusta Kannisto ou Gompertz por máxima verossimilhança de Poisson
#'
#' Supõe \eqn{D_x \sim Poisson(E_x \mu(x + 0.5))} e maximiza a
#' log-verossimilhança em \eqn{(\log a, \log b)}.
#'
#' @param obitos Óbitos por idade simples.
#' @param expostos Exposição (população no meio do ano) por idade.
#' @param idades Idades correspondentes.
#' @param modelo `"kannisto"` ou `"gompertz"`.
#' @param x0 Idade de referência.
#' @return Lista com `a`, `b`, `modelo`, `x0`, `loglik` e `aic`.
#' @export
ajustar_velhice <- function(obitos, expostos, idades,
                            modelo = c("kannisto", "gompertz"), x0 = 80) {
  modelo <- match.arg(modelo)
  stopifnot(length(obitos) == length(expostos), length(obitos) == length(idades))

  # Valores iniciais: regressão linear em log(mx) (Gompertz) ou logit(mx)
  mx  <- pmax(obitos / expostos, 1e-8)
  y   <- if (modelo == "kannisto") stats::qlogis(pmin(mx, 0.99)) else log(mx)
  ini <- stats::coef(stats::lm(y ~ I(idades + 0.5 - x0)))
  par0 <- c(ini[[1]], log(max(ini[[2]], 0.01)))

  # Verossimilhança de Poisson "contínua" (óbitos redistribuídos não são inteiros)
  nll <- function(p) {
    mu <- mu_velhice(idades + 0.5, exp(p[1]), exp(p[2]), modelo, x0)
    -sum(obitos * log(expostos * mu) - expostos * mu)
  }
  opt <- stats::optim(par0, nll, method = "BFGS")
  ll  <- -opt$value - sum(lgamma(obitos + 1))

  list(a = exp(opt$par[1]), b = exp(opt$par[2]), modelo = modelo, x0 = x0,
       loglik = ll, aic = 2 * 2 - 2 * ll, convergiu = opt$convergence == 0)
}

#' Pesos para abrir um grupo etário aberto em idades simples
#'
#' Distribui a população do grupo aberto (por exemplo, 90+) entre as idades
#' simples segundo a população estacionária implícita no modelo de
#' mortalidade ajustado: \eqn{w_x \propto \int_x^{x+1} S(t)\,dt}, com
#' \eqn{S} a sobrevivência a partir da idade inicial do grupo. A última
#' idade recebe todo o restante (grupo aberto final).
#'
#' @param ajuste Saída de [ajustar_velhice()].
#' @param idade_ini Idade inicial do grupo a ser aberto.
#' @param idade_max Idade do grupo aberto final.
#' @param idade_lim Idade até onde a integral numérica é calculada.
#' @return Tibble com `idade` e `peso` (soma 1).
#' @export
pesos_grupo_aberto <- function(ajuste, idade_ini = 90, idade_max = 100,
                               idade_lim = 130) {
  passo <- 0.01
  t  <- seq(idade_ini, idade_lim, by = passo)
  mu <- mu_velhice(t, ajuste$a, ajuste$b, ajuste$modelo, ajuste$x0)
  S  <- exp(-cumsum(mu) * passo)
  faixa <- pmin(floor(t), idade_max)
  L <- tapply(S * passo, faixa, sum)
  tibble::tibble(idade = as.integer(names(L)), peso = as.numeric(L / sum(L)))
}

#' Taxa central do grupo aberto implícita no modelo
#'
#' Para o grupo aberto \eqn{[\omega, \infty)}, \eqn{m_\omega = 1/e_\omega},
#' com \eqn{e_\omega = \int_\omega^\infty S(t)/S(\omega)\,dt} calculado
#' numericamente sob o modelo ajustado.
#'
#' @inheritParams pesos_grupo_aberto
#' @param idade Idade inicial do grupo aberto.
#' @return Taxa central \eqn{m_\omega}.
#' @export
m_aberto_modelo <- function(ajuste, idade = 100, idade_lim = 130) {
  passo <- 0.01
  t  <- seq(idade, idade_lim, by = passo)
  mu <- mu_velhice(t, ajuste$a, ajuste$b, ajuste$modelo, ajuste$x0)
  S  <- exp(-cumsum(mu) * passo)
  1 / sum(S * passo)
}
