
preparar_cards_acoes <- function(dados_acoes) {

  if (is.null(dados_acoes) || nrow(dados_acoes) == 0) {
    return(list())
  }

  quebrar_multivalor <- function(x, separador = ",") {

    if (is.null(x) || length(x) == 0 || all(is.na(x))) {
      return(character(0))
    }

    x <- as.character(x)
    x <- x[!is.na(x)]

    if (length(x) == 0) {
      return(character(0))
    }

    x <- unlist(strsplit(x, separador, fixed = TRUE))
    x <- trimws(x)
    x <- x[!is.na(x) & x != ""]

    unique(x)
  }

  obter_cores_tema <- function(x) {

    if (is.null(x) || length(x) == 0) {
      return(data.frame())
    }

    if (is.data.frame(x)) {
      cores <- x
    } else if (is.list(x) && length(x) == 1 && is.data.frame(x[[1]])) {
      cores <- x[[1]]
    } else {
      return(data.frame())
    }

    if (nrow(cores) == 0) {
      return(data.frame())
    }

    # Prioriza cor_dark, caso exista.
    # Se não existir, usa cor_bg como alternativa.
    coluna_cor <- if ("cor_dark" %in% names(cores)) {
      "cor_dark"
    } else if ("cor_bg" %in% names(cores)) {
      "cor_bg"
    } else {
      NA_character_
    }

    if (is.na(coluna_cor)) {
      return(data.frame())
    }

    out <- data.frame(
      tema = if ("tema" %in% names(cores)) cores$tema else NA_character_,
      cor = cores[[coluna_cor]],
      stringsAsFactors = FALSE
    )

    out <- out[!is.na(out$cor) & trimws(out$cor) != "", , drop = FALSE]
    unique(out)
  }

  cards <- lapply(seq_len(nrow(dados_acoes)), function(i) {

    linha <- dados_acoes[i, , drop = FALSE]

    list(
      id = linha$ID[1],

      nome_ficha = valor_txt(linha$Nome_ficha[1]),

      cores_tema = obter_cores_tema(
        linha$cores_tema[[1]]
      ),

      indicador_relacionado = quebrar_multivalor(
        linha$Indicador_relacionado[1]
      ),

      estrategia_acao = quebrar_multivalor(
        linha$acao[1]
      ),

      tipologia = quebrar_multivalor(
        linha$tipologia[1]
      ),

      local = quebrar_multivalor(
        linha$local[1]
      )
    )
  })

  cards
}

campo_card_acao_ui <- function(titulo, valores) {

  if (is.null(valores) || length(valores) == 0) {
    valores <- "Sem informação"
  }

  tags$div(
    class = "acao-card-campo",

    tags$div(
      class = "acao-card-campo-titulo",
      titulo
    ),

    tags$div(
      class = "acao-card-campo-valores",

      lapply(valores, function(x) {
        tags$span(
          class = "acao-card-chip",
          x
        )
      })
    )
  )
}

card_acao_ui <- function(card) {

  cores_tema <- card$cores_tema

  tags$div(
    class = "acao-card-novo",

    # 1. Nome da ficha
    tags$h3(
      class = "acao-card-novo-titulo",
      card$nome_ficha
    ),

    # 2. Linhas/tags coloridas dos temas
    tags$div(
      class = "acao-card-temas-linhas",

      if (!is.null(cores_tema) && is.data.frame(cores_tema) && nrow(cores_tema) > 0) {

        lapply(seq_len(nrow(cores_tema)), function(i) {

          tags$span(
            class = "acao-card-tema-linha",
            title = cores_tema$tema[i],
            style = paste0(
              "background:", cores_tema$cor[i], ";"
            )
          )
        })

      } else {

        tags$span(
          class = "acao-card-tema-linha sem-cor"
        )
      }
    ),

    # 3. Indicador de exposição relacionado
    campo_card_acao_ui(
      titulo = "Indicador de exposição relacionado",
      valores = card$indicador_relacionado
    ),

    # 4. Estratégia de atuação da ação
    campo_card_acao_ui(
      titulo = "Estratégia de atuação da ação",
      valores = card$estrategia_acao
    ),

    # 5. Tipologia da ação
    campo_card_acao_ui(
      titulo = "Tipologia da ação",
      valores = card$tipologia
    ),

    # 6. Local da ação
    campo_card_acao_ui(
      titulo = "Local da ação",
      valores = card$local
    )
  )
}

cards_acoes_ui <- function(cards) {

  if (is.null(cards) || length(cards) == 0) {
    return(
      tags$div(
        class = "acoes-sem-resultados",
        "Nenhuma ação encontrada para a seleção realizada."
      )
    )
  }

  tags$div(
    class = "acoes-cards-grid",

    lapply(cards, card_acao_ui)
  )
}
