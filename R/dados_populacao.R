#' URL base das Projeções da População do IBGE (Revisão 2024)
#' @keywords internal
url_ibge_projecoes <- function() {
  "https://ftp.ibge.gov.br/Projecao_da_Populacao/Projecao_da_Populacao_2024/"
}

#' Baixa uma tabela das Projeções da População do IBGE (Revisão 2024)
#'
#' @param tabela Nome do arquivo, por exemplo
#'   `"projecoes_2024_tab1_idade_simples.xlsx"`.
#' @param dir_raw Pasta de destino.
#' @return Caminho do arquivo baixado.
#' @export
baixar_ibge <- function(tabela, dir_raw = here::here("data", "raw", "ibge")) {
  dir.create(dir_raw, recursive = TRUE, showWarnings = FALSE)
  destino <- file.path(dir_raw, tabela)
  if (!file.exists(destino)) {
    utils::download.file(paste0(url_ibge_projecoes(), tabela), destino,
                         mode = "wb", quiet = TRUE)
  }
  destino
}

#' Lê a população residente por sexo e idade simples (IBGE)
#'
#' Lê a Tabela 1 das Projeções da População (Revisão 2024): população em
#' 1º de julho, por sexo e idade simples de 0 a 89 e grupo aberto 90+, para
#' o Brasil.
#'
#' @param caminho Caminho do xlsx `projecoes_2024_tab1_idade_simples.xlsx`.
#' @param anos Anos desejados.
#' @return Tibble com `ano`, `sexo`, `idade` (90 = 90+) e `pop`.
#' @export
ler_pop_ibge <- function(caminho, anos = 2000:2023) {
  readxl::read_excel(caminho, skip = 5, .name_repair = "minimal") |>
    dplyr::filter(.data$SIGLA == "BR", .data$SEXO %in% c("Homens", "Mulheres")) |>
    dplyr::select("IDADE", "SEXO", dplyr::all_of(as.character(anos))) |>
    tidyr::pivot_longer(-c("IDADE", "SEXO"), names_to = "ano", values_to = "pop") |>
    dplyr::transmute(
      ano   = as.integer(.data$ano),
      sexo  = dplyr::if_else(.data$SEXO == "Homens", "Masculino", "Feminino"),
      idade = as.integer(.data$IDADE),
      pop   = as.numeric(.data$pop)
    ) |>
    dplyr::arrange(.data$ano, .data$sexo, .data$idade)
}

#' Lê os indicadores oficiais do IBGE (e0, e60, óbitos estimados)
#'
#' Lê a Tabela 4 das Projeções da População (Revisão 2024), usada como
#' referência para comparar a e0 calculada e estimar o grau de cobertura do
#' SIM (óbitos registrados / óbitos estimados pelo IBGE).
#'
#' @param caminho Caminho do xlsx `projecoes_2024_tab4_indicadores.xlsx`.
#' @param anos Anos desejados.
#' @return Tibble com `ano`, `sexo`, `e0_ibge`, `e60_ibge`, `obitos_ibge`.
#' @export
ler_indicadores_ibge <- function(caminho, anos = 2000:2023) {
  readxl::read_excel(caminho, skip = 6, .name_repair = "minimal") |>
    dplyr::filter(.data$SIGLA == "BR", .data$ANO %in% anos) |>
    dplyr::select("ANO", "e0_H", "e0_M", "e60_H", "e60_M", "OBT_H", "OBT_M") |>
    dplyr::mutate(dplyr::across(dplyr::everything(), as.numeric)) |>
    tidyr::pivot_longer(-"ANO", names_to = c(".value", "s"), names_sep = "_") |>
    dplyr::transmute(
      ano         = as.integer(.data$ANO),
      sexo        = dplyr::if_else(.data$s == "H", "Masculino", "Feminino"),
      e0_ibge     = .data$e0,
      e60_ibge    = .data$e60,
      obitos_ibge = .data$OBT
    )
}

#' Compara a população do IBGE com a do pacote brpop
#'
#' O `brpop` só disponibiliza grupos quinquenais (até 80+), por isso não é a
#' fonte principal. Esta função agrega a população do IBGE nos mesmos grupos
#' e calcula a diferença relativa, como checagem de consistência.
#'
#' @param pop_ibge Saída de [ler_pop_ibge()].
#' @return Tibble com `ano`, `grupo`, `pop_ibge`, `pop_brpop`, `dif_pct`.
#' @export
comparar_pop_brpop <- function(pop_ibge) {
  rotulo <- function(x) {
    ini <- pmin(5 * (x %/% 5), 80)
    dplyr::if_else(ini == 80, "80+", sprintf("%d-%d", ini, ini + 4))
  }
  brpop_br <- suppressMessages(brpop::uf_pop_age()) |>
    dplyr::filter(.data$age_group != "Total") |>
    dplyr::mutate(
      ini   = as.integer(sub("From (\\d+).*", "\\1", .data$age_group)),
      grupo = rotulo(.data$ini),
      ano   = as.integer(.data$year)
    ) |>
    dplyr::group_by(.data$ano, .data$grupo) |>
    dplyr::summarise(pop_brpop = sum(.data$pop), .groups = "drop")

  pop_ibge |>
    dplyr::mutate(grupo = rotulo(.data$idade)) |>
    dplyr::group_by(.data$ano, .data$grupo) |>
    dplyr::summarise(pop_ibge = sum(.data$pop), .groups = "drop") |>
    dplyr::inner_join(brpop_br, by = c("ano", "grupo")) |>
    dplyr::mutate(dif_pct = 100 * (.data$pop_brpop / .data$pop_ibge - 1))
}

#' Baixa a população do Censo 2022 por sexo e idade simples (SIDRA 9514)
#'
#' Consulta a API do SIDRA (tabela 9514, universo do Censo Demográfico
#' 2022): população residente por sexo e idade, Brasil. Grava o JSON bruto.
#'
#' @param dir_raw Pasta de destino.
#' @return Caminho do arquivo JSON.
#' @export
baixar_censo_sidra <- function(dir_raw = here::here("data", "raw", "ibge")) {
  dir.create(dir_raw, recursive = TRUE, showWarnings = FALSE)
  destino <- file.path(dir_raw, "censo2022_sidra9514.json")
  if (!file.exists(destino)) {
    url <- paste0(
      "https://apisidra.ibge.gov.br/values/t/9514/n1/all/v/93/p/2022",
      "/c2/4,5/c287/all/c286/113635/f/n"
    )
    utils::download.file(url, destino, mode = "wb", quiet = TRUE)
  }
  destino
}

#' Lê a população do Censo 2022 em idade simples (0 a 100+)
#'
#' @param caminho JSON gerado por [baixar_censo_sidra()].
#' @return Tibble com `sexo`, `idade` (100 = 100+) e `pop`.
#' @export
ler_censo_sidra <- function(caminho) {
  bruto <- jsonlite::fromJSON(caminho)[-1, ]
  tibble::tibble(sexo = bruto$D4N, rotulo = bruto$D5N, pop = as.numeric(bruto$V)) |>
    dplyr::filter(grepl("^([0-9]+ anos?( ou mais)?|Menos de 1 ano)$", .data$rotulo)) |>
    dplyr::transmute(
      sexo  = dplyr::if_else(.data$sexo == "Homens", "Masculino", "Feminino"),
      idade = dplyr::if_else(.data$rotulo == "Menos de 1 ano", 0L,
                             suppressWarnings(as.integer(sub(" .*", "", .data$rotulo)))),
      pop   = .data$pop
    ) |>
    dplyr::arrange(.data$sexo, .data$idade)
}
