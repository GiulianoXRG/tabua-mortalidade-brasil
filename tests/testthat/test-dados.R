test_that("decodificar_idade_sim trata unidades, 100+ e ignorados", {
  cod <- c("010", "105", "220", "311", "400", "435", "499", "500", "512", "999", NA, "4x1")
  esp <- c(0L, 0L, 0L, 0L, 0L, 35L, 99L, 100L, 100L, NA, NA, NA)
  expect_identical(decodificar_idade_sim(cod), esp)
})

test_that("redistribuir_ignorados preserva o total e elimina NA", {
  obitos <- tibble::tribble(
    ~ano, ~sexo,       ~idade, ~obitos,
    2020, "Masculino", 0L,     10,
    2020, "Masculino", 1L,     30,
    2020, "Feminino",  0L,     20,
    2020, "Feminino",  1L,     20,
    2020, "Masculino", NA,     8,
    2020, NA,          1L,     5,
    2020, NA,          NA,     2
  )
  res <- redistribuir_ignorados(obitos, idades = 0:1)

  expect_equal(sum(res$obitos), sum(obitos$obitos))
  expect_false(anyNA(res))
  expect_setequal(unique(res$sexo), c("Masculino", "Feminino"))
  # 8 masculinos sem idade: 1/4 vai para idade 0, 3/4 para idade 1
  m0 <- res$obitos[res$sexo == "Masculino" & res$idade == 0]
  expect_gt(m0, 12)
})
