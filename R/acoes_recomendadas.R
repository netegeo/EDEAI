# ---------------------------
# Página de políticas públicas
# ---------------------------

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

          tags$div(
            class = "acoes-kicker",
            "Ações recomendadas"
          ),

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




acao_detalhe_ui <- function(obj) {

  tags$div(
    class = "acao-detalhe-page",

    # =========================
    # BREADCRUMB
    # =========================
    tags$div(
      class = "acao-breadcrumb",

      actionLink(
        inputId = "voltar_lista_acoes_topo",
        label = "Ações Recomendadas",
        class = "acao-breadcrumb-link"
      ),

      tags$span(
        class = "acao-breadcrumb-sep",
        "›"
      ),

      tags$span(
        class = "acao-breadcrumb-current",
        acao$nome_curto
      )
    ),

    # =========================
    # CARD PRINCIPAL
    # =========================
    tags$div(
      class = "acao-card-principal",

      tags$div(
        class = "acao-card-barra"
      ),

      tags$div(
        class = "acao-card-conteudo",

        tags$div(
          class = "acao-topo",

          tags$h1(
            class = "acao-titulo",
            acao$nome_ficha
          ),
        ),

        tags$div(
          class = "acao-tags",

          lapply(acao$tags, function(x) {
            tags$span(
              class = "acao-tag",
              x
            )
          })
        ),

        tags$div(
          class = "acao-importancia-box",

          tags$strong(
            "Por que isso é importante: "
          ),

          tags$span(
            acao$por_que_importante
          )
        )
      )
    ),

    # =========================
    # DESCRIÇÃO
    # =========================
    tags$div(
      class = "acao-section-card",

      tags$h2("Descrição da ação"),

      tags$p(
        acao$descricao_acao
      )
    ),

    # =========================
    # OBSERVAÇÃO
    # =========================
    tags$div(
      class = "acao-section-card",

      tags$h2("Observação"),

      tags$p(
        acao$observacao
      )
    ),

    # =========================
    # REFERÊNCIAS
    # =========================


    tags$div(
      class = "acao-section-card acao-referencias-section",

      tags$h2("Referências bibliográficas"),

      lapply(acao$referencias, function(ref) {

        tags$div(
          class = "acao-ref-card",

          tags$div(
            class = "acao-ref-texto",

            tags$div(
              class = "acao-ref-titulo",
              ref$titulo
            ),

tags$div(
  class = "acao-ref-linha",

  tags$div(
    class = "acao-ref-label",
    "O que trata:"
  ),

  tags$div(
    class = "acao-ref-conteudo",
    ref$o_que_trata
  )
),

tags$div(
  class = "acao-ref-linha",

  tags$div(
    class = "acao-ref-label",
    "Relação com a ação:"
  ),

  tags$div(
    class = "acao-ref-conteudo",
    ref$relacao
  )
)
          )
        )
      })
    ),

    # =========================
    # VOLTAR
    # =========================
    tags$div(
      class = "acao-voltar-area",

      actionButton(
        "voltar_lista_acoes",
        "← Voltar para lista",
        class = "btn-voltar-acoes"
      )
    )
  )
}