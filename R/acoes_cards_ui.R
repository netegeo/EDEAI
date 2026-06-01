
preparar_cards_acoes <- function(dados_acoes) {

  if (is.null(dados_acoes) || nrow(dados_acoes) == 0) {
    return(list())
  }

  quebrar_multivalor <- function(x, separador = ",") {

    limpar_opcoes_filtro_acoes(
      x = x,
      separador = separador
    )
  }

  valor_coluna_card <- function(linha, nome_novo, nome_antigo = NULL, vazio = NA_character_) {

    if (nome_novo %in% names(linha)) {
      return(valor_txt(linha[[nome_novo]][1], vazio = vazio))
    }

    if (!is.null(nome_antigo) && nome_antigo %in% names(linha)) {
      return(valor_txt(linha[[nome_antigo]][1], vazio = vazio))
    }

    vazio
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

    coluna_cor <- if ("cor_dark" %in% names(cores)) {
      "cor_dark"
    } else if ("cor" %in% names(cores)) {
      "cor"
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
      id = valor_coluna_card(linha, "id", "ID"),

      nome_ficha = limpar_texto_html(valor_coluna_card(linha, "nome_ficha", "Nome_ficha")),

      cores_tema = obter_cores_tema(
        linha$cores_tema[[1]]
      ),

      indicador_relacionado = quebrar_multivalor(
        valor_coluna_card(
          linha,
          "indicador_de_exposicao_relacionado",
          "Indicador_relacionado"
        )
      ),

      estrategia_acao = quebrar_multivalor(
        valor_coluna_card(
          linha,
          "estrategia_de_atuacao_da_acao",
          "acao"
        )
      ),

      tipologia = quebrar_multivalor(
        valor_coluna_card(
          linha,
          "tipologia_da_acao",
          "tipologia"
        )
      ),

      local = quebrar_multivalor(
        valor_coluna_card(
          linha,
          "local_da_acao",
          "local"
        )
      )
    )
  })

  cards
}

quebrar_valores_card_acao <- function(valores, separador = ",") {

  if (is.null(valores) || length(valores) == 0) {
    return(character(0))
  }

  valores <- as.character(valores)
  valores <- valores[!is.na(valores)]

  if (length(valores) == 0) {
    return(character(0))
  }

  marcador_pm25 <- "__MARCADOR_PM25__"

  # Protege o trecho decimal antes da quebra por vírgula.
  # Preserva os parênteses e o restante do texto.
  valores <- gsub(
    pattern = "PM\\s*2\\s*,\\s*5",
    replacement = marcador_pm25,
    x = valores,
    ignore.case = TRUE
  )

  valores <- unlist(
    strsplit(
      valores,
      separador,
      fixed = TRUE
    )
  )

  valores <- gsub(
    pattern = marcador_pm25,
    replacement = "PM 2,5",
    x = valores,
    fixed = TRUE
  )

  valores <- limpar_espacos_unicode(valores)
  valores <- valores[!is.na(valores) & valores != ""]

  chave <- normalizar_chave_filtro(valores)
  valores <- valores[!duplicated(chave)]

  sort(valores)
}

campo_card_acao_ui <- function(titulo, valores, separador = ",") {

  valores <- limpar_opcoes_filtro_acoes(
    x = valores,
    separador = separador
  )

  if (length(valores) == 0) {
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
    tags$a(
      href = "#",
       class = "acao-card-novo-titulo",
        onclick = sprintf(
        "Shiny.setInputValue('acao_selecionada', '%s', {priority: 'event'}); return false;",
        card$id
      ),
      card$nome_ficha,tags$span(class = "acoes-seta-left")
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
