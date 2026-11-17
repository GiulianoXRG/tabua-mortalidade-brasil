#' Baixa as declarações de óbito (SIM-DO) de um ano
#'
#' Faz o download de todas as UFs de um ano via
#' `microdatasus::fetch_datasus()`, mantém apenas as variáveis necessárias e
#' grava o resultado bruto em parquet. Se o arquivo já existir, não baixa de
#' novo.
#'
#' @param ano Ano de ocorrência do óbito (um único valor).
#' @param dir_raw Pasta de destino dos dados brutos.
#' @param sobrescrever Se `TRUE`, baixa novamente mesmo que o arquivo exista.
#' @return Caminho do arquivo parquet gravado.
#' @export
baixar_sim_ano <- function(ano, dir_raw = here::here("data", "raw", "sim"),
                           sobrescrever = FALSE) {
  stopifnot(length(ano) == 1)
  dir.create(dir_raw, recursive = TRUE, showWarnings = FALSE)
  destino <- file.path(dir_raw, sprintf("sim_do_%d.parquet", ano))
  if (file.exists(destino) && !sobrescrever) return(destino)

  dados <- microdatasus::fetch_datasus(
    year_start = ano, year_end = ano, uf = "all",
    information_system = "SIM-DO",
    vars = c("DTOBITO", "IDADE", "SEXO"),
    stop_on_error = TRUE, timeout = 600, quiet = TRUE
  )
  dados <- dplyr::mutate(dados, ano = as.integer(ano), .before = 1)
  arrow::write_parquet(dados, destino)
  destino
}

#' Decodifica a variável IDADE do SIM em anos completos
#'
#' No SIM, `IDADE` tem 3 dígitos: o primeiro é a unidade (0 = minutos,
#' 1 = horas, 2 = dias, 3 = meses, 4 = anos, 5 = anos acima de 100) e os
#' dois seguintes, a quantidade. Valores com unidade 9 ou quantidade
#' inválida são tratados como ignorados (`NA`).
#'
#' @param idade Vetor character com o código `IDADE` original.
#' @param idade_max Idade do grupo aberto final (as idades acima são
#'   truncadas nela).
#' @return Vetor inteiro com a idade em anos completos (0 a `idade_max`)
#'   ou `NA`.
#' @export
decodificar_idade_sim <- function(idade, idade_max = 100L) {
  idade  <- trimws(as.character(idade))
  valido <- !is.na(idade) & grepl("^[0-9]{3}$", idade)
  unid   <- suppressWarnings(as.integer(substr(idade, 1, 1)))
  qtd    <- suppressWarnings(as.integer(substr(idade, 2, 3)))

  anos <- rep(NA_integer_, length(idade))
  menos_1_ano <- valido & unid %in% 0:3
  anos[menos_1_ano] <- 0L
  em_anos <- valido & unid == 4L
  anos[em_anos] <- qtd[em_anos]
  acima_100 <- valido & unid == 5L
  anos[acima_100] <- 100L + qtd[acima_100]

  pmin(anos, as.integer(idade_max))
}

#' Trata um ano de óbitos do SIM
#'
#' Aplica `microdatasus::process_sim()` (rotulagem de sexo e datas),
#' decodifica a idade em anos completos e agrega por ano x sexo x idade,
#' mantendo os ignorados como `NA` para posterior redistribuição.
#'
#' @param caminho Caminho do parquet bruto gerado por [baixar_sim_ano()].
#' @param idade_max Idade do grupo aberto final.
#' @return Tibble com colunas `ano`, `sexo` (Masculino/Feminino/NA),
#'   `idade` (0..idade_max ou NA) e `obitos`.
#' @export
agregar_sim_ano <- function(caminho, idade_max = 100L) {
  bruto <- arrow::read_parquet(caminho)
  ano   <- unique(bruto$ano)
  proc  <- suppressWarnings(microdatasus::process_sim(
    dplyr::select(bruto, -"ano"), municipality_data = FALSE
  ))

  tibble::tibble(
    ano   = as.integer(ano),
    sexo  = as.character(proc$SEXO),
    idade = decodificar_idade_sim(bruto$IDADE, idade_max)
  ) |>
    dplyr::mutate(
      sexo = dplyr::if_else(sexo %in% c("Masculino", "Feminino"), sexo, NA_character_)
    ) |>
    dplyr::count(.data$ano, .data$sexo, .data$idade, name = "obitos")
}

#' Redistribui óbitos com sexo e/ou idade ignorados
#'
#' Procedimento proporcional em duas etapas, dentro de cada ano:
#' 1. óbitos com idade ignorada são distribuídos entre as idades conforme a
#'    distribuição etária dos óbitos com idade conhecida do mesmo sexo (para
#'    sexo ignorado, usa-se a distribuição de ambos os sexos);
#' 2. óbitos com sexo ignorado são distribuídos entre os sexos conforme a
#'    razão de sexo dos óbitos daquela idade.
#'
#' O total de óbitos de cada ano é preservado. O resultado pode ter valores
#' não inteiros.
#'
#' @param obitos Tibble com `ano`, `sexo`, `idade`, `obitos`, como em
#'   [agregar_sim_ano()].
#' @param idades Vetor de idades válidas.
#' @return Tibble com `ano`, `sexo`, `idade`, `obitos`, sem `NA`.
#' @export
redistribuir_ignorados <- function(obitos, idades = 0:100) {
  sexos <- c("Masculino", "Feminino")
  grade <- tidyr::expand_grid(
    ano = unique(obitos$ano), sexo = c(sexos, NA_character_), idade = idades
  )

  # Etapa 1: idade ignorada -> proporcional à distribuição etária
  conhecidos <- dplyr::filter(obitos, !is.na(.data$idade))
  dist_sexo <- conhecidos |>
    dplyr::filter(!is.na(.data$sexo)) |>
    dplyr::group_by(.data$ano, .data$sexo, .data$idade) |>
    dplyr::summarise(obitos = sum(.data$obitos), .groups = "drop")
  dist_ambos <- conhecidos |>
    dplyr::group_by(.data$ano, .data$idade) |>
    dplyr::summarise(obitos = sum(.data$obitos), .groups = "drop") |>
    dplyr::mutate(sexo = NA_character_)
  pesos <- dplyr::bind_rows(dist_sexo, dist_ambos) |>
    dplyr::right_join(grade, by = c("ano", "sexo", "idade")) |>
    dplyr::mutate(obitos = dplyr::coalesce(.data$obitos, 0)) |>
    dplyr::group_by(.data$ano, .data$sexo) |>
    dplyr::mutate(peso = .data$obitos / sum(.data$obitos)) |>
    dplyr::ungroup() |>
    dplyr::select("ano", "sexo", "idade", "peso")

  ign_idade <- obitos |>
    dplyr::filter(is.na(.data$idade)) |>
    dplyr::group_by(.data$ano, .data$sexo) |>
    dplyr::summarise(ign = sum(.data$obitos), .groups = "drop")

  etapa1 <- obitos |>
    dplyr::filter(!is.na(.data$idade)) |>
    dplyr::right_join(grade, by = c("ano", "sexo", "idade")) |>
    dplyr::mutate(obitos = dplyr::coalesce(.data$obitos, 0)) |>
    dplyr::left_join(pesos, by = c("ano", "sexo", "idade")) |>
    dplyr::left_join(ign_idade, by = c("ano", "sexo")) |>
    dplyr::mutate(
      obitos = .data$obitos +
        dplyr::coalesce(.data$ign, 0) * dplyr::coalesce(.data$peso, 0)
    ) |>
    dplyr::select("ano", "sexo", "idade", "obitos")

  # Etapa 2: sexo ignorado -> proporcional à razão de sexo por idade
  ign_sexo <- etapa1 |>
    dplyr::filter(is.na(.data$sexo)) |>
    dplyr::select("ano", "idade", ign = "obitos")

  etapa1 |>
    dplyr::filter(!is.na(.data$sexo)) |>
    dplyr::group_by(.data$ano, .data$idade) |>
    dplyr::mutate(
      tot  = sum(.data$obitos),
      peso = dplyr::if_else(.data$tot > 0, .data$obitos / .data$tot, 0.5)
    ) |>
    dplyr::ungroup() |>
    dplyr::left_join(ign_sexo, by = c("ano", "idade")) |>
    dplyr::mutate(obitos = .data$obitos + dplyr::coalesce(.data$ign, 0) * .data$peso) |>
    dplyr::select("ano", "sexo", "idade", "obitos") |>
    dplyr::arrange(.data$ano, .data$sexo, .data$idade)
}

#' Resume a qualidade da informação de sexo e idade
#'
#' @param obitos Tibble agregado com `NA` (saída de [agregar_sim_ano()]).
#' @return Tibble por ano com total de óbitos e percentuais de idade
#'   ignorada e sexo ignorado.
#' @export
resumo_ignorados <- function(obitos) {
  obitos |>
    dplyr::group_by(.data$ano) |>
    dplyr::summarise(
      total            = sum(.data$obitos),
      pct_idade_ign    = 100 * sum(.data$obitos[is.na(.data$idade)]) / .data$total,
      pct_sexo_ign     = 100 * sum(.data$obitos[is.na(.data$sexo)]) / .data$total,
      .groups = "drop"
    )
}
