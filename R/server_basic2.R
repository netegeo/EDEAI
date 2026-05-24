# =========================================================
# Arquivo: R/server_basic2.R
# Finalidade:
#   Controlar a navegação principal do dashboard Shiny,
#   renderizar a página municipal, a página de indicadores,
#   a página de ações recomendadas e as páginas institucionais.
#
# Observações:
#   - Esta versão remove a lógica antiga da aba Ações Recomendadas
#     baseada em filtros laterais, botões "buscar"/"limpar" e
#     uiOutput("filtros") / uiOutput("pagina").
#   - A nova aba Ações Recomendadas passa a usar:
#       uiOutput("barra_selecao_acoes")
#       uiOutput("pagina_acoes")
#   - A função montar_objeto_acoes() deve estar disponível no ambiente.
# =========================================================


# =========================================================
# Funções auxiliares gerais
# =========================================================

valor_txt <- function(x, vazio = "Sem informação") {
  if (length(x) == 0 || is.null(x) || all(is.na(x))) {
    return(vazio)
  }

  x <- as.character(x[1])

  if (is.na(x) || trimws(x) == "") {
    return(vazio)
  }

  x
}

limpar_opcoes_filtro_acoes <- function(x, separador = ",") {

  if (is.null(x) || length(x) == 0) {
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

  sort(unique(x))
}


# =========================================================
# Funções auxiliares da nova barra de seleção de ações
# =========================================================

dropdown_filtro_acoes <- function(id, titulo, escolhas) {

  escolhas <- limpar_opcoes_filtro_acoes(escolhas)

  tags$div(
    class = "acoes-dropdown",

    tags$button(
      type = "button",
      class = "acoes-dropdown-btn",
      titulo
    ),

    tags$div(
      class = "acoes-dropdown-content",

      checkboxGroupInput(
        inputId = id,
        label = NULL,
        choices = escolhas,
        selected = character(0)
      )
    )
  )
}

preparar_acao_ui <- function(dados_acoes, id_acao) {

  linha <- dados_acoes[dados_acoes$ID == id_acao, , drop = FALSE]

  if (nrow(linha) == 0) {
    return(NULL)
  }

  list(
    id = linha$ID[1],
    nome_curto = linha$acao[1],
    nome_ficha = linha$Nome_ficha[1],
    por_que_importante = linha$importancia[1],
    descricao_acao = linha$descricao[1],
    observacao = linha$observacao[1],
    referencias = linha$referencia[[1]],
    tags = linha$tags[[1]],
    temas = linha$temas_lista[[1]],
    cores_tema = linha$cores_tema[[1]]
  )
}

tem_selecao_acoes <- function(selecao) {

  if (is.null(selecao) || length(selecao) == 0) {
    return(FALSE)
  }

  any(vapply(
    selecao,
    function(x) {
      !is.null(x) &&
        length(x) > 0 &&
        any(!is.na(x) & trimws(as.character(x)) != "")
    },
    logical(1)
  ))
}

acoes_resultado_placeholder_ui <- function(obj_acoes) {

  tags$div(
    class = "acoes-resultados-placeholder",

    tags$div(
      class = "acoes-lista-info",
      HTML(
        paste0(
          "<b>",
          obj_acoes$total,
          "</b> ações encontradas"
        )
      )
    ),

    tags$p(
      "A barra de seleção já está conectada ao objeto de ações. A forma final de apresentação das ações será definida na próxima etapa."
    )
  )
}


# =========================================================
# Server
# =========================================================

server <- function(input, output, session) {

  # -------------------------------------------------------
  # Estado de navegação da aplicação
  # -------------------------------------------------------

  pagina_ativa <- reactiveVal("inicio")
  indicador_ativo <- reactiveVal(NULL)

  # -------------------------------------------------------
  # Navegação principal
  # -------------------------------------------------------

  observeEvent(input$menu_inicio, {
    pagina_ativa("inicio")
  })

  observeEvent(input$hero_municipio, {
    pagina_ativa("municipio")
  })

  observeEvent(input$menu_municipio, {
    pagina_ativa("municipio")
  })

  observeEvent(input$menu_acoes_recomendadas, {
    pagina_ativa("acoes")
  })

  observeEvent(input$menu_projeto, {
    pagina_ativa("projeto")
  })

  observeEvent(input$menu_indicadores, {
    pagina_ativa("indicadores")
  })

  observeEvent(input$footer_quem_somos, {
    pagina_ativa("quem_somos")
  })

  observeEvent(input$btn_equipe_projeto, {
    pagina_ativa("equipe_projeto")
  })


  # -------------------------------------------------------
  # Navegação da página de indicadores
  # -------------------------------------------------------

  observeEvent(input$indicador_selecionado, {
    indicador_ativo(input$indicador_selecionado)
    pagina_ativa("indicador_detalhe")
  })

  observeEvent(input$voltar_indicadores, {
    indicador_ativo(NULL)
    pagina_ativa("indicadores")
  })


  # -------------------------------------------------------
  # Renderização da página atual
  # -------------------------------------------------------

  output$pagina_atual <- renderUI({

    pagina <- pagina_ativa()

    if (pagina == "inicio") {
      return(inicio_ui())
    }

    if (pagina == "municipio") {
      return(municipio_ui())
    }

    if (pagina == "acoes") {
      return(acoes_recomendadas_ui())
    }

    if (pagina == "projeto") {
      return(projeto_ui())
    }

    if (pagina == "indicadores") {
      return(indicador_ui(dados_app$info$metadado))
    }

    if (pagina == "indicador_detalhe") {

      req(indicador_ativo())

      df_indicador <- dados_app$info$metadado[
        dados_app$info$metadado$Codigo == indicador_ativo(),
        ,
        drop = FALSE
      ]

      if (nrow(df_indicador) == 0) {
        return(
          tags$div(
            class = "pagina-indicador",
            tags$p("Indicador não encontrado.")
          )
        )
      }

      return(pagina_indicador_ui(df_indicador))
    }

    if (pagina == "quem_somos") {
      return(quem_somos_ui())
    }

    if (pagina == "equipe_projeto") {
      return(equipe_projeto_ui())
    }

    inicio_ui()
  })


  # =======================================================
  # Página: Diagnóstico de Municípios
  # =======================================================

  observeEvent(input$uf, {

    req(input$uf)

    municipios_uf <- dados_app$inbase$nome_mun[
      dados_app$inbase$sigla_uf == input$uf
    ]

    municipios_uf <- sort(unique(municipios_uf))
    municipios_uf <- municipios_uf[!is.na(municipios_uf) & municipios_uf != ""]

    updateSelectInput(
      session = session,
      inputId = "municipio",
      choices = municipios_uf,
      selected = municipios_uf[1]
    )
  }, ignoreInit = TRUE)


  objeto_municipio <- eventReactive(input$carregar_municipio, {

    req(input$uf)
    req(input$municipio)

    montar_objeto_municipio(
      dados_app = dados_app,
      uf = input$uf,
      municipio = input$municipio
    )
  })


  output$card_municipio <- renderUI({

    obj <- objeto_municipio()

    if (is.null(obj)) {
      return(
        tags$div(
          class = "municipio-card-novo2",
          tags$p("Não foi possível carregar as informações do município selecionado.")
        )
      )
    }

    municipio_card_ui(obj)
  })


  # =======================================================
  # Página: Ações Recomendadas
  # Nova lógica
  # =======================================================

  # -------------------------------------------------------
  # Define a base de ações a ser usada pela função
  # montar_objeto_acoes().
  #
  # Preferência:
  #   1. objeto global dados_acao, se existir;
  #   2. info_acoes$dados_acao, se existir.
  # -------------------------------------------------------

  base_acoes <- reactive({

    if (exists("dados_acao_df")) {
      return(dados_acao_df)
    }

    NULL
  })

  # -------------------------------------------------------
  # Barra de seleção das ações recomendadas
  # -------------------------------------------------------

  output$barra_selecao_acoes <- renderUI({

    dados <- base_acoes()

    validate(
      need(
        !is.null(dados),
        "Base de ações não encontrada. Verifique se dados_acao_df foi criado."
      )
    )

  escolhas_tema <- limpar_opcoes_filtro_acoes(
    unlist(dados$temas_lista)
  )

  escolhas_indicador <- limpar_opcoes_filtro_acoes(
    dados$Indicador_relacionado
  )

  escolhas_acao <- limpar_opcoes_filtro_acoes(
    dados$acao
  )

  escolhas_local <- limpar_opcoes_filtro_acoes(
    dados$local
  )

  escolhas_tipologia <- limpar_opcoes_filtro_acoes(
    dados$tipologia
  )

  tags$div(
    class = "acoes-selecao-barra",

    dropdown_filtro_acoes(
      id = "filtro_tema",
      titulo = "Grupo temático",
      escolhas = escolhas_tema
    ),

    dropdown_filtro_acoes(
      id = "filtro_indicador_relacionado",
      titulo = "Indicador de exposição \nrelacionado",
      escolhas = escolhas_indicador
    ),

    dropdown_filtro_acoes(
      id = "filtro_acao",
      titulo = "Estratégia de atuação \nda ação",
      escolhas = escolhas_acao
    ),

    dropdown_filtro_acoes(
      id = "filtro_local",
      titulo = "Local da ação",
      escolhas = escolhas_local
    ),

    dropdown_filtro_acoes(
      id = "filtro_tipologia",
      titulo = "Tipologia da ação",
      escolhas = escolhas_tipologia
    ),

    tags$button(
      id = "limpar_filtros_acoes",
      type = "button",
      class = "acoes-limpar-btn",
      "Limpar seleção",
      onclick = "
        Shiny.setInputValue(
          'limpar_filtros_acoes_click',
          Math.random(),
          {priority: 'event'}
        );
      "
    )
  )
  })


  # -------------------------------------------------------
  # Seleções feitas pelo usuário
  # -------------------------------------------------------

  selecao_acoes <- reactive({

    list(
      tema 					= input$filtro_tema,
      indicador_relacionado = input$filtro_indicador_relacionado,
      acao 					= input$filtro_acao,
      local 				= input$filtro_local,
      tipologia 			= input$filtro_tipologia
    )
  })


  # -------------------------------------------------------
  # Limpa os filtros da nova barra
  # -------------------------------------------------------

  observeEvent(input$limpar_filtros_acoes_click, {

    updateCheckboxGroupInput(
      session = session,
      inputId = "filtro_tema",
      selected = character(0)
    )

    updateCheckboxGroupInput(
      session = session,
      inputId = "filtro_indicador_relacionado",
      selected = character(0)
    )

    updateCheckboxGroupInput(
      session = session,
      inputId = "filtro_acao",
      selected = character(0)
    )

    updateCheckboxGroupInput(
      session = session,
      inputId = "filtro_local",
      selected = character(0)
    )

    updateCheckboxGroupInput(
      session = session,
      inputId = "filtro_tipologia",
      selected = character(0)
    )
  })


  # -------------------------------------------------------
  # Objeto final de ações, já filtrado pela seleção
  # -------------------------------------------------------

  objeto_acoes_filtrado <- reactive({

    dados <- base_acoes()

    validate(
      need(
        !is.null(dados),
        "Base de ações não encontrada. Verifique se dados_acao_df foi carregado."
      )
    )

    montar_objeto_acoes(
      dados_acao = dados,
      selecao = selecao_acoes()
    )
  })


  # -------------------------------------------------------
  # Área de resultados das ações
  #
  # Nesta etapa, ainda é apenas uma saída de validação.
  # A forma final de apresentação será construída depois.
  # -------------------------------------------------------

output$pagina_acoes <- renderUI({

  selecao <- selecao_acoes()

  if (!tem_selecao_acoes(selecao)) {
    return(
      tags$div(
        class = "acoes-mensagem-inicial",
        "Selecione um ou mais filtros na barra acima para visualizar as ações recomendadas."
      )
    )
  }

  obj <- objeto_acoes_filtrado()

  cards <- preparar_cards_acoes(
    obj$dados
  )

  tagList(
    tags$div(
      class = "acoes-resultados-resumo",
      HTML(
        paste0(
          "<b>",
          obj$total,
          "</b> ações encontradas"
        )
      )
    ),

    cards_acoes_ui(cards)
  )
})



}
