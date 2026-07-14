# =========================================================
# Arquivo: iu_basic-T.R
# Finalidade:
#   Definir a interface principal do dashboard Shiny.
#
# Estrutura geral:
#   - carrega componentes auxiliares de interface
#   - define o objeto global 'ui'
#   - cria as páginas internas acessadas por navegação
#
# Páginas definidas neste arquivo:
#   - início
#   - Diagnóstico Municipio
#   - Ações Recomendadas  
#   - Sobre o projeto 
#       - O Painel
#       - Metodologia
#       - Quem Somos
#
# Componentes internos:
#   - Inicio
#   - municipio_ui.R
#
# Observações:
#   - a página de município foi modularizada em arquivo próprio
#   - há blocos comentados que representam versões anteriores
#   - parte do conteúdo metodológico ainda está provisória
# =========================================================

# ---------------------------
# Interface principal do app
# ---------------------------
# Define a estrutura fixa da aplicação:
#   - topo com logos
#   - barra de navegação
#   - área dinâmica de conteúdo
#   - rodapé

ui <- fluidPage(
  shinyjs::useShinyjs(),
   theme 		= bslib::bs_theme(
    version 	= 5,
    bootswatch 	= "flatly",
    primary 	= "#0B5CAD",
    secondary 	= "#1E3A5F",
    bg 			= "#F8F4ED",
    fg 			= "#183153"
  ),
  
  tags$head(
    #tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
	tags$link(rel = "stylesheet", type = "text/css", href = "custom2.css"),
    tags$script(src = "js/html2pdf.bundle.min.js")
  ),
  
  div(
    class = "menu-bar",
    div(
      class = "menu-inner",
      actionButton("menu_inicio", "Início", class = "menu-btn"),
      actionButton("menu_municipio", "Diagnóstico de Municípios", class = "menu-btn"),
      actionButton("menu_acoes_recomendadas", "Ações Recomendadas", class = "menu-btn"), 
      div(class = "menu-dropdown",
        tags$button(
        class = "menu-btn dropdown-toggle", "Sobre o Projeto"),
            div(
                class = "menudrop-content",
                actionButton("menu_projeto","O Painel",
                class = "menudrop-item" ),
                actionButton("menu_indicadores","Metodologia",
                class = "menudrop-item"),
                actionButton("footer_quem_somos","Quem somos",
                class = "menudrop-item")
                )
        ) 
      )
    ),
  uiOutput("pagina_atual"),
  footer_ui()
)

# ---------------------------
# Página inicial
# ---------------------------
# Apresenta o objetivo do painel e orienta a leitura geral.

inicio_ui <- function() {
tagList(
  div(
    class = "inicio-page",

    div(
      class = "inicio-section",

      div(
        class = "hero-card inicio-vertical",

        img(
          src = "icons/SAMI_assinatura_RGB_080526-01.svg",
          class = "inicio-img"
        ),

        div(
          class = "inicio-text",

          div(
            class = "inicio-subtitle",
            "Uma ferramenta que reúne dados e informações para garantir o direito de crianças e adolescentes a viver em um ambiente limpo, saudável e seguro."
          ),

          div(
            class = "inicio-actions",

            actionButton(
              "hero_municipio",
              "Explore a situação do seu município",
              class = "btn-municipio"
            )
          ),
          div(
            class = "inicio-actions",
            tags$a(
              href = "files/NotaTecnica-PainelCriancasMeioAmbiente.pdf",
              target = "_blank",
                tags$button(
                type = "button",
                class = "btn-municipio",
                "Acesse a nota técnica dos resultados")
            )
          )
        )
      )
    ),
    div(
      class = "card-wrap",
      div(class = "card-title", card_inicio$title),
      div(
        class = "cards-grid",
        div(
          class = "info-card",
          div(class = "cards-icon", tags$img(  
            src = info_card$icons[info_card$name_card == "diagnostico"], height = "120px")),
          div(class = "cards-title", 
              info_card$title_card[info_card$name_card == "diagnostico"]),
          div(class = "cards-text", 
              info_card$text_card[info_card$name_card == "diagnostico"])
        ),
        div(
          class = "info-card",
          div(class = "cards-icon", tags$img(
            src = info_card$icons[info_card$name_card == "dados"], height = "120px")),
          div(class = "cards-title", 
              info_card$title_card[info_card$name_card == "dados"]),
          div(class = "cards-text", 
              info_card$text_card[info_card$name_card == "dados"])
        ),
        div(
          class = "info-card",
          div(class = "cards-icon", tags$img(            
            src = info_card$icons[info_card$name_card == "açoes"], height = "120px")),
          div(class = "cards-title", 
              info_card$title_card[info_card$name_card == "açoes"]),
          div(class = "info-text", 
              info_card$text_card[info_card$name_card == "açoes"])
        ),
        div(
          class = "info-card",
          div(class = "cards-icon", tags$img(            
            src = info_card$icons[info_card$name_card == "criancas"], height = "120px")),
          div(class = "cards-title", 
              info_card$title_card[info_card$name_card == "criancas"]),
          div(class = "cards-text", 
              info_card$text_card[info_card$name_card == "criancas"])
        )
      )
    )
  ))
}


# ---------------------------
# Página de leitura/metodologia
# ---------------------------


projeto_ui <- function() {
  tagList(
      div(class =  "projeto-page",

div(
  class = "projeto-hero",

  div(
    class = "projeto-hero-text",
    h1("O que é o Painel?"),
  div(
    class = "linhas-proj",
    span(),
    span(),
    span(),
    span()
    ),
    p("Uma ferramenta estratégica que permite identificar, mensurar e comunicar quão adequados são os ambientes em que vivem crianças e adolescentes nos municípios brasileiros, bem como propor ações para auxiliar os gestores públicos na melhoria da qualidade do ambiente em que vivem meninos e meninas.")
  ),

  div(
    class = "projeto-metricas",

    div(
      class = "projeto-metrica-card",
      div(class = "projeto-metrica-numero", "5.571"),
      div(class = "projeto-metrica-label", "municípios \n analisados")
    ),

    div(
      class = "projeto-metrica-card",
      div(class = "projeto-metrica-numero", "53 milhões"),
      div(class = "projeto-metrica-label", "de crianças e adolecentes")
    )
  )
),

      div(
        class = "projeto-cards",

        projeto_card(
          titulo = "Como funciona",
          icone = "chart-bar",
          texto = "O painel reúne 14 indicadores de exposição
          ambiental, distribuídos em 4 áreas temáticas, que afetam o bem-estar e a saúde de crianças e adolescentes no Brasil. A ferramenta gera diagnósticos e recomendações para os municípios com base no seu desempenho em cada indicador."
        ),

        projeto_card(
          titulo = "Objetivo",
          icone = "bullseye",          
          texto = "Oferecer uma ferramenta de apoio aos gestores públicos municipais para qualificar a análise do território, subsidiar decisões mais preventivas e promover o direito das crianças e adolescentes a um ambiente limpo, saudável, seguro e protegido contra riscos ambientais previsíveis e evitáveis."
        ),

        projeto_card(
          titulo = "Por que analisar?",          
          icone = "children",
          texto = "Crianças e adolescentes são particularmente sensíveis às condições do ambiente ao seu redor e vulneráveis aos impactos da poluição, de eventos climáticos extremos, e de doenças causadas por condições inadequadas nos locais em que vivem e frequentam."
        ),

        projeto_card(
          titulo = "Por que importa?",          
          icone = "clipboard-list",
          texto = "Um ambiente seguro, saudável e sustentável não é apenas um direito de toda criança e adolescente, mas também um elemento essencial para garantir outros direitos, reduzir desigualdades e promover o desenvolvimento integral na infância e na adolescência."
        )
      )
    )
  )
}


projeto_card <- function(icone, titulo, texto) {
  div(
    class = "projeto-card",
    div(
      class = "projeto-card-icon",
      icon(icone)
    ),
    h3(titulo),
    div(class = "projeto-card-line"),
    p(texto),
  )
}


