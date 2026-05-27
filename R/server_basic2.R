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

padronizar_colunas_acoes <- function(df) {

  names(df) <- names(df) |>
    tolower() |>
    trimws() |>
    gsub("\\s+", "_", x = _) |>
    gsub("\\.", "_", x = _)

  df
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
  acao_ativa <- reactiveVal(NULL)
  origem_acao <- reactiveVal(NULL)

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

 observeEvent(input$btn_equipe_inicio, {
    pagina_ativa("inicio")
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


  observeEvent(input$acao_selecionada, {

  req(input$acao_selecionada)

  acao_ativa(input$acao_selecionada)
  origem_acao(pagina_ativa())

  pagina_ativa("acao_detalhe")

  shinyjs::runjs("
    window.scrollTo({
      top: 0,
      behavior: 'smooth'
    });
  ")
  })
  
  observeEvent(input$voltar_origem_acao, {

  destino <- origem_acao()

  if (is.null(destino) || is.na(destino) || destino == "") {
    destino <- "acoes"
  }

  acao_ativa(NULL)
  pagina_ativa(destino)

  shinyjs::runjs("
    window.scrollTo({
      top: 0,
      behavior: 'smooth'
    });
  ")
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
    
    if (pagina == "acao_detalhe") {

      req(acao_ativa())

      acao <- preparar_acao_ui(
       dados_acoes = objeto_acoes$destaque,
       id_acao = acao_ativa(),
       objeto_acoes = objeto_acoes
       
      )

    if (is.null(acao)) {
      return(
       tags$div(
         class = "acao-detalhe-page",
         tags$p("Ação não encontrada.")
       )
      )
    }

    return(acao_detalhe_ui(acao))
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
  # Base de ações
  #
  # Mantém o nome base_acoes(), mas agora usa o objeto
  # já montado em R/montar_objeto_acoes.R:
  #
  #   objeto_acoes$destaque
  #
  # Não usa mais dados_acao_df.
  # -------------------------------------------------------

  base_acoes <- reactive({

    validate(
      need(
        exists("objeto_acoes") &&
          !is.null(objeto_acoes) &&
          !is.null(objeto_acoes$destaque),
        "Objeto de ações não encontrado. Verifique se objeto_acoes foi montado em R/montar_objeto_acoes.R."
      )
    )

    objeto_acoes$destaque
  })


    # -------------------------------------------------------
  # Função de compatibilidade para os cards atuais
  #
  # Ela evita mudar preparar_cards_acoes() e cards_acoes_ui().
  # Cria apelidos com nomes antigos caso os cards ainda usem:
  #   ID, Nome_ficha, importancia, Indicador_relacionado,
  #   acao, local, tipologia, temas_lista, cores_tema.
  # -------------------------------------------------------

  compatibilizar_acoes_para_cards <- function(dados) {

    if (is.null(dados) || nrow(dados) == 0) {
      return(dados)
    }

    if (!"ID" %in% names(dados) && "id" %in% names(dados)) {
      dados$ID <- dados$id
    }

    if (!"Nome_ficha" %in% names(dados) && "nome_da_ficha" %in% names(dados)) {
      dados$Nome_ficha <- dados$nome_da_ficha
    }

    if (!"importancia" %in% names(dados) &&
        "por_que_isso_e_importante" %in% names(dados)) {
      dados$importancia <- dados$por_que_isso_e_importante
    }

    if (!"Indicador_relacionado" %in% names(dados) &&
        "indicador_de_exposicao_relacionado" %in% names(dados)) {
      dados$Indicador_relacionado <- dados$indicador_de_exposicao_relacionado
    }

    if (!"acao" %in% names(dados) &&
        "estrategia_de_atuacao_da_acao" %in% names(dados)) {
      dados$acao <- dados$estrategia_de_atuacao_da_acao
    }

    if (!"local" %in% names(dados) &&
        "local_da_acao" %in% names(dados)) {
      dados$local <- dados$local_da_acao
    }

    if (!"tipologia" %in% names(dados) &&
        "tipologia_da_acao" %in% names(dados)) {
      dados$tipologia <- dados$tipologia_da_acao
    }

    if (!"temas_lista" %in% names(dados) &&
        "grupo_tematico" %in% names(dados)) {
      dados$temas_lista <- lapply(
        dados$grupo_tematico,
        limpar_opcoes_filtro_acoes,
        separador = ";"
      )
    }

    if (!"cores_tema" %in% names(dados) &&
        all(c("grupo_tematico", "cor_tema_dark") %in% names(dados))) {

      dados$cores_tema <- lapply(seq_len(nrow(dados)), function(i) {
        data.frame(
          tema = dados$grupo_tematico[i],
          cor_dark = dados$cor_tema_dark[i],
          stringsAsFactors = FALSE
        )
      })
    }

    dados
  }

  # -------------------------------------------------------
  # Barra de seleção das ações recomendadas
  #
  # Mantém:
  #   output$barra_selecao_acoes
  #   filtro_tema
  #   filtro_indicador_relacionado
  #   filtro_acao
  #   filtro_local
  #   filtro_tipologia
  # -------------------------------------------------------

  output$barra_selecao_acoes <- renderUI({

    dados <- base_acoes()

    escolhas_tema <- limpar_opcoes_filtro_acoes(
      dados$grupo_tematico,
      separador = ","
    )

    escolhas_indicador <- limpar_opcoes_filtro_acoes(
      dados$indicador_de_exposicao_relacionado
    )

    escolhas_acao <- limpar_opcoes_filtro_acoes(
      dados$estrategia_de_atuacao_da_acao
    )

    escolhas_local <- limpar_opcoes_filtro_acoes(
      dados$local_da_acao
    )

    escolhas_tipologia <- limpar_opcoes_filtro_acoes(
      dados$tipologia_da_acao
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
  #
  # Mantém o nome selecao_acoes() e mantém os inputIds.
  # Só muda o nome interno dos campos para conversar com
  # filtrar_acoes_recomendadas().
  # -------------------------------------------------------

  selecao_acoes <- reactive({

    list(
      grupo_tematico = input$filtro_tema,
      indicador_de_exposicao_relacionado = input$filtro_indicador_relacionado,
      estrategia_de_atuacao_da_acao = input$filtro_acao,
      local_da_acao = input$filtro_local,
      tipologia_da_acao = input$filtro_tipologia
    )
  })


  # -------------------------------------------------------
  # Limpa os filtros da nova barra
  #
  # Mantém exatamente os mesmos inputIds.
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
  #
  # Mantém o nome objeto_acoes_filtrado().
  # Agora usa filtrar_acoes_recomendadas().
  # -------------------------------------------------------

  objeto_acoes_filtrado <- reactive({

    dados <- base_acoes()

    filtrar_acoes_recomendadas(
      destaque = dados,
      selecao = selecao_acoes(),
      regra = "OR"
    )
  })


  # -------------------------------------------------------
  # Área de resultados das ações
  #
  # Mantém output$pagina_acoes.
  # Mantém preparar_cards_acoes() e cards_acoes_ui().
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

    dados_cards <- compatibilizar_acoes_para_cards(
      obj$dados
    )

    cards <- preparar_cards_acoes(
      dados_cards
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
