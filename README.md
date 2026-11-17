# Tábua de mortalidade brasileira + projeção Lee-Carter

> 🚧 Em construção.

Tábua de mortalidade do Brasil (2000–2023), por sexo e idade simples, construída
a partir de dados abertos (SIM/DATASUS e estimativas populacionais), com
projeção da mortalidade até 2050 pelo modelo Lee-Carter.

## Estrutura

```
.
├── _targets.R          # orquestra o pipeline
├── R/                  # funções puras documentadas (roxygen)
├── data/
│   ├── raw/            # dados brutos (não versionados)
│   └── processed/      # dados tratados (parquet)
├── output/figures/     # gráficos gerados
├── report/             # relatório Quarto
├── tests/testthat/     # testes unitários
└── renv.lock           # versões travadas dos pacotes
```

## Como reproduzir

```r
renv::restore()
targets::tar_make()
```

## Licença

[MIT](LICENSE)
