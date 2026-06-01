# ---------------------------
# Página de políticas públicas
# ---------------------------

preparar_acao_ui <- function(dados_acoes, id_acao, objeto_acoes = NULL) {

  if (is.null(dados_acoes) || nrow(dados_acoes) == 0) {
    return(NULL)
  }

  # Compatibilidade entre nomes antigos e novos
  coluna_id <- if ("ID" %in% names(dados_acoes)) {
    "ID"
  } else if ("id" %in% names(dados_acoes)) {
    "id"
  } else {
    return(NULL)
  }

  linha <- dados_acoes[
    as.character(dados_acoes[[coluna_id]]) == as.character(id_acao),
    ,
    drop = FALSE
  ]

  if (nrow(linha) == 0) {
    return(NULL)
  }

  valor_coluna <- function(nome_novo, nome_antigo = NULL, vazio = NA_character_) {

    if (nome_novo %in% names(linha)) {
      return(valor_txt(linha[[nome_novo]][1], vazio = vazio))
    }

    if (!is.null(nome_antigo) && nome_antigo %in% names(linha)) {
      return(valor_txt(linha[[nome_antigo]][1], vazio = vazio))
    }

    vazio
  }

  valor_lista <- function(nome_novo, nome_antigo = NULL) {

    if (nome_novo %in% names(linha)) {
      return(linha[[nome_novo]][[1]])
    }

    if (!is.null(nome_antigo) && nome_antigo %in% names(linha)) {
      return(linha[[nome_antigo]][[1]])
    }

    list()
  }
  
  # --------- O que Fazer -----------------------
o_que_fazer <- character(0)

if (!is.null(objeto_acoes) &&
    !is.null(objeto_acoes$o_que_fazer)) {

  linha_o_que_fazer <- objeto_acoes$o_que_fazer[
    objeto_acoes$o_que_fazer$id == id_acao,
    ,
    drop = FALSE
  ]

  if (nrow(linha_o_que_fazer) > 0) {

    ordem_num <- suppressWarnings(as.numeric(linha_o_que_fazer$ordem))

    linha_o_que_fazer <- linha_o_que_fazer[
      order(ordem_num, na.last = TRUE),
      ,
      drop = FALSE
    ]

    o_que_fazer <- linha_o_que_fazer$o_que_fazer
  }
}
    
  # --------- Dicas Praticas --------------------
dicas_praticas <- character(0)

if (!is.null(objeto_acoes) &&
    !is.null(objeto_acoes$dicas_praticas)) {

  linha_dicas_praticas <- objeto_acoes$dicas_praticas[
    objeto_acoes$dicas_praticas$id == id_acao,
    ,
    drop = FALSE
  ]

  if (nrow(linha_dicas_praticas) > 0) {

    ordem_num <- suppressWarnings(as.numeric(linha_dicas_praticas$ordem))

    linha_dicas_praticas <- linha_dicas_praticas[
      order(ordem_num, na.last = TRUE),
      ,
      drop = FALSE
    ]

    dicas_praticas <- linha_dicas_praticas$dicas_praticas
  }
}
  
  # ------- Base Tecnica -------------------------
  
  base_tecnica <- data.frame(
      id = character(0),
      id_referencia = character(0),
      titulo = character(0),
      o_que_trata = character(0),
      relacao = character(0),
      stringsAsFactors = FALSE
  )

  if (!is.null(objeto_acoes) &&
      !is.null(objeto_acoes$base_tecnica)) {

       base_tecnica <- objeto_acoes$base_tecnica[
       objeto_acoes$base_tecnica$id == id_acao,
      ,
       drop = FALSE
      ]
   }

  list(
    id 					= limpar_texto_html(valor_coluna("id", "ID")),
    nome_curto 			= limpar_texto_html(valor_coluna("nome_curto", "acao")),
    nome_ficha 			= limpar_texto_html(valor_coluna("nome_ficha", "Nome_ficha")),
    por_que_importante 	= limpar_texto_html(valor_coluna("por_que_importante", "importancia")),
    indicador_de_exposicao_relacionado = limpar_texto_html(valor_coluna(
      "indicador_de_exposicao_relacionado",
      "Indicador_relacionado"
    )),

    estrategia_de_atuacao_da_acao = limpar_texto_html(valor_coluna(
      "estrategia_de_atuacao_da_acao",
      "acao"
    )),

    tipologia_da_acao = limpar_texto_html(valor_coluna(
      "tipologia_da_acao",
      "tipologia"
    )),

    local_da_acao = limpar_texto_html(valor_coluna(
      "local_da_acao",
      "local"
    )),
    
    tags 				= valor_lista("tags"),
    temas 				= valor_lista("temas_lista"),
    cores_tema 			= valor_lista("cores_tema"),
    o_que_fazer 		= o_que_fazer,
    dicas_praticas 		= dicas_praticas,
    base_tecnica 		= base_tecnica
  )
}

acoes_recomendadas_ui <- function() {

  tagList(

    div(
      class = "acoes-page",

      # =========================
      # HERO - MANTIDO
      # =========================
      div(
        class = "acoes-hero",

        div(
          class = "acoes-hero-content",

          # tags$div(
          #   class = "acoes-kicker",
          #   "Ações recomendadas"
          # ),

          tags$h2(
            class = "acoes-hero-title",
            "Recomendações para apoiar a gestão pública"
          ),

          tags$p(
            class = "acoes-hero-text",
            "Esta página conecta os indicadores de exposição ambiental a eixos de ação pública, como saneamento, ambiente escolar, clima e desastres, habitação e planejamento territorial."
          )
        )
      ),

      # =========================
      # NOVA BARRA DE SELEÇÃO
      # =========================
      div(
        class = "acoes-selecao-barra",

        uiOutput("barra_selecao_acoes")
      ),

      # =========================
      # ÁREA DE RESULTADOS
      # =========================
      div(
        class = "acoes-resultados-wrap",

        uiOutput("pagina_acoes")
      )
    )
  )
}

regua_temas_acao_ui <- function(cores_tema) {

  if (is.null(cores_tema) || nrow(cores_tema) == 0) {
    return(
      tags$div(
        class = "acao-card-regua",
        tags$div(
          class = "acao-card-regua-item",
          style = "background:#113131;"
        )
      )
    )
  }

  tags$div(
    class = "acao-card-regua",
    lapply(seq_len(nrow(cores_tema)), function(i) {
      tags$div(
        class = "acao-card-regua-item",
        title = cores_tema$tema[i],
        style = paste0(
          "background:",
          cores_tema$cor_dark[i],
          ";"
        )
      )
    })
  )
}

o_que_fazer_acao_ui <- function(o_que_fazer) {

  if (is.null(o_que_fazer) ||
      length(o_que_fazer) == 0 ||
      all(is.na(o_que_fazer)) ||
      all(trimws(as.character(o_que_fazer)) == "")) {
    return(
      tags$p("Sem informação.")
    )
  }

  o_que_fazer <- as.character(o_que_fazer)

  o_que_fazer <- o_que_fazer[
    !is.na(o_que_fazer) &
      trimws(o_que_fazer) != ""
  ]

  tagList(
    lapply(seq_along(o_que_fazer), function(i) {

      tags$div(
        class = "acao-info-card",

        tags$div(
          class = "acao-info-numero",
          i
        ),

        tags$div(
          class = "acao-info-conteudo",
          limpar_texto_html(valor_txt(o_que_fazer[i]))
        )
      )
    })
  )
}

dicas_praticas_acao_ui <- function(dicas_praticas) {

  if (is.null(dicas_praticas) ||
      length(dicas_praticas) == 0 ||
      all(is.na(dicas_praticas)) ||
      all(trimws(as.character(dicas_praticas)) == "")) {
    return(
      tags$p("Sem informação.")
    )
  }

  dicas_praticas <- as.character(dicas_praticas)

  dicas_praticas <- dicas_praticas[
    !is.na(dicas_praticas) &
      trimws(dicas_praticas) != ""
  ]

  tagList(
    lapply(seq_along(dicas_praticas), function(i) {

      tags$div(
        class = "acao-info-card",

        tags$div(
          class = "acao-info-numero",
          i
        ),

        tags$div(
          class = "acao-info-conteudo",
          limpar_texto_html(valor_txt(dicas_praticas[i]))
        )
      )
    })
  )
}

base_tecnica_acao_ui <- function(base_tecnica) {

  if (is.null(base_tecnica) || nrow(base_tecnica) == 0) {
    return(
      tags$p("Sem informação.")
    )
  }

  tagList(
    lapply(seq_len(nrow(base_tecnica)), function(i) {

      tags$div(
        class = "acao-ref-card",

        tags$div(
          class = "acao-ref-titulo",
          limpar_texto_html(valor_txt(base_tecnica$titulo[i]))
        ),

        tags$div(
          class = "acao-ref-linha",

          tags$div(
            class = "acao-ref-label",
            "O que trata:"
          ),

          tags$div(
            class = "acao-ref-conteudo",
            limpar_texto_html(valor_txt(base_tecnica$o_que_trata[i]))
          )
        ),

        tags$div(
          class = "acao-ref-linha",

          tags$div(
            class = "acao-ref-label",
            "Relação:"
          ),

          tags$div(
            class = "acao-ref-conteudo",
            limpar_texto_html(valor_txt(base_tecnica$relacao[i]))
          )
        )
      )
    })
  )
}

campo_lista_acao_ui <- function(rotulo, valores) {

  valores <- limpar_opcoes_filtro_acoes(valores)

  if (length(valores) == 0) {
    valores <- "Sem informação"
  }

  tags$div(
    class = "acao-info-campo",

    tags$div(
      class = "acao-info-rotulo",
      rotulo
    ),

    tags$div(
      class = "acao-info-valores",

      lapply(valores, function(x) {
        tags$div(
          class = "acao-info-valor",
          x
        )
      })
    )
  )
}

acao_topo_detalhe_ui <- function(acao) {

  tags$div(
    class = "acao-topo-box",

    tags$div(
      class = "acao-topo-titulo-box",
      tags$h1(
        class = "acao-topo-titulo",
        valor_txt(limpar_texto_html(acao$nome_ficha))
      )
    ),

    tags$div(
      class = "acao-topo-importancia",

      tags$strong("Por que isso é importante: "),

      tags$span(
        valor_txt(limpar_texto_html(acao$por_que_importante))
      )
    ),

    tags$div(
      class = "acao-info-grid linha-1",

      campo_lista_acao_ui(
        "Indicador",
        acao$indicador_de_exposicao_relacionado
      ),

      campo_lista_acao_ui(
        "Estratégia",
        acao$estrategia_de_atuacao_da_acao
      )
    ),

    tags$div(
      class = "acao-info-grid linha-2",

      campo_lista_acao_ui(
        "Tipologia",
        acao$tipologia_da_acao
      ),

      campo_lista_acao_ui(
        "Onde acontece",
        acao$local_da_acao
      )
    )
  )
}

acao_detalhe_ui <- function(obj) {

  acao <- obj

  tags$div(
    class = "acao-detalhe-page",

    # =========================
    # TOPO DA AÇÃO
    # =========================
    acao_topo_detalhe_ui(acao),

    # =========================
    # O QUE FAZER
    # =========================
    tags$div(
      class = "acao-section-card",

      tags$h2("O que fazer"),

      o_que_fazer_acao_ui(acao$o_que_fazer)
    ),

    # =========================
    # DICAS PRÁTICAS
    # =========================
    tags$div(
      class = "acao-section-card",

      tags$h2("Dicas práticas"),

      dicas_praticas_acao_ui(acao$dicas_praticas)
    ),

    # =========================
    # BASE TÉCNICA
    # =========================
    tags$div(
      class = "acao-section-card acao-referencias-section",

      tags$h2("Base Técnica"),

      base_tecnica_acao_ui(acao$base_tecnica)
    ),

    # =========================
    # VOLTAR
    # =========================
    tags$div(
      class = "acao-voltar-area",

      actionButton(
        inputId = "voltar_origem_acao",
        label = "← Voltar",
        class = "btn-voltar-acoes"
      )
    )
  )
}
