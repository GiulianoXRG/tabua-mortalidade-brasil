# Taxas sintéticas: mortalidade infantil + Gompertz-Makeham simples
mx_teste <- function() {
  x <- 0:100
  m <- 0.0005 + 0.00003 * exp(0.095 * x)
  m[1] <- 0.013
  m[2] <- 0.0009
  m
}

test_that("lx é decrescente e começa na raiz", {
  tab <- life_table(mx_teste(), 0:100, "Feminino")
  expect_equal(tab$lx[1], 1e5)
  expect_true(all(diff(tab$lx) < 0))
})

test_that("soma de dx é igual a l0", {
  tab <- life_table(mx_teste(), 0:100, "Masculino")
  expect_equal(sum(tab$dx), tab$lx[1])
})

test_that("qx está entre 0 e 1 e é 1 no grupo aberto", {
  tab <- life_table(mx_teste(), 0:100, "Masculino")
  expect_true(all(tab$qx >= 0 & tab$qx <= 1))
  expect_equal(tab$qx[101], 1)
})

test_that("Lx e ex são consistentes", {
  tab <- life_table(mx_teste(), 0:100, "Feminino")
  expect_equal(tab$Tx[1], sum(tab$Lx))
  expect_equal(tab$ex[101], 1 / tab$mx[101])
  expect_true(all(tab$Lx <= c(tab$lx[-1], Inf) + tab$dx + 1e-8))
})

test_that("a0 de Coale-Demeny segue a Tabela 3.3 de Preston et al.", {
  expect_equal(a0_coale_demeny(0.02, "Masculino"), 0.045 + 2.684 * 0.02)
  expect_equal(a0_coale_demeny(0.2, "Feminino"), 0.35)
})

test_that("a1 é menor que 0,5 quando a mortalidade cai com a idade", {
  expect_lt(a1_exponencial(0.0009, 0.0005), 0.5)
  expect_equal(a1_exponencial(0.001, 0.001), 0.5)
})

test_that("Kannisto recupera os parâmetros em dados simulados", {
  set.seed(1)
  x <- 80:89
  e <- rep(2e5, length(x))
  mu <- mu_velhice(x + 0.5, a = 0.06, b = 0.11, "kannisto")
  d <- stats::rpois(length(x), e * mu)
  aj <- ajustar_velhice(d, e, x, "kannisto")
  expect_equal(aj$a, 0.06, tolerance = 0.05)
  expect_equal(aj$b, 0.11, tolerance = 0.05)
})

test_that("pesos do grupo aberto somam 1 e decrescem", {
  aj <- list(a = 0.06, b = 0.11, modelo = "kannisto", x0 = 80)
  w <- pesos_grupo_aberto(aj, 90, 100)
  expect_equal(sum(w$peso), 1)
  expect_equal(w$idade, 90:100)
  expect_true(all(diff(w$peso[1:10]) < 0))
})
