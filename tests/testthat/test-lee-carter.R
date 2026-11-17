# Dados sintéticos com estrutura Lee-Carter exata
mx_sintetico <- function(anos = 2000:2015, idades = 0:100) {
  ax <- log(0.0005 + 0.00003 * exp(0.095 * idades))
  bx <- rep(1 / length(idades), length(idades))
  kt <- seq(10, -10, length.out = length(anos))
  grade <- tidyr::expand_grid(ano = anos, idade = idades)
  grade$sexo <- "Feminino"
  grade$expostos <- 1e6
  grade$mx <- exp(ax[grade$idade + 1] + bx[grade$idade + 1] * kt[grade$ano - min(anos) + 1])
  grade$obitos <- grade$mx * grade$expostos
  grade
}

test_that("matrizes_lc gera matrizes idade x ano coerentes", {
  m <- matrizes_lc(mx_sintetico(), "Feminino", 2000:2015)
  expect_equal(dim(m$Dxt), c(101, 16))
  expect_equal(dim(m$Ext), c(101, 16))
  expect_equal(rownames(m$Dxt)[1], "0")
  expect_true(all(m$Dxt > 0))
})

test_that("Lee-Carter recupera as taxas de dados sem ruído", {
  d <- mx_sintetico()
  aj <- fit_lc(d, "Feminino", 2000:2015)
  ajustado <- stats::fitted(aj, type = "rates")
  m <- matrizes_lc(d, "Feminino", 2000:2015)
  expect_lt(max(abs(log(ajustado) - log(m$Dxt / m$Ext))), 1e-3)
  expect_equal(sum(aj$bx), 1, tolerance = 1e-6)
})

test_that("expectativas_de_taxas bate com life_table", {
  d <- mx_sintetico(2000:2001)
  m <- matrizes_lc(d, "Feminino", 2000:2001)
  taxas <- m$Dxt / m$Ext
  e <- expectativas_de_taxas(taxas, "Feminino")
  esperado <- life_table(taxas[, 1], 0:100, "Feminino")$ex[c(1, 66)]
  expect_equal(unname(e[, 1]), unname(esperado))
})
