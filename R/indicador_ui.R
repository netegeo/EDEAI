# =========================
# DADOS
# =========================
metadados <- readRDS("data/informacoes_20260510.rds")$metadados


# garante ordenação
metadados <- metadados[order(metadados$n_apr), ]

# =========================
# BLOCO DE TEMA
# =========================
criar_bloco_tema <- function(df_tema) {
  
  titulo_tema <- unique(df_tema$tema)[1]
  desc_tema   <- unique(df_tema$desc_tema)[1]
  cor_bg      <- unique(df_tema$cor_box)[1]
  tom_dark      <- unique(df_tema$cor_dark)[1]

  div(
    class = "indicador-box-dinamico",
    style = paste0(
      "background:", cor_bg,"33", ";",
      "border-left: 10px solid ", tom_dark,";", 
      "border-top: 2px solid",tom_dark,";",   
      "border-radius: 16px;",
      "padding: 18px;"
    ),
    
    div(
      class = "indicador-box-head",
      onclick = "toggleTema(this.parentElement)",
      
      div(class = "indicador-box-titulo", titulo_tema),
      div(class = "indicador-box-desc", desc_tema)
    ),
    
    div(
      class = "indicador-lista hidden",
      
      lapply(seq_len(nrow(df_tema)), function(i) {
        div(
          class = "indicador-item",
          
          tags$a(
            href = "#",
            class = "indicador-item-titulo-link",
            onclick = sprintf(
              "Shiny.setInputValue('indicador_selecionado', '%s', {priority: 'event'}); return false;",
              df_tema$Codigo[i]
            ),
            df_tema$Título[i]
          ),
          
          div(
            class = "indicador-item-desc",
            HTML(paste0("<b>Descrição do indicador: </b>", df_tema$Descrição[i],
            "</br> <b>Unidade: </b>",df_tema$Unidade[i],
            "</br> <b>Fonte: </b>",df_tema$Fonte[i],
            "</br> <b>Série histórica: </b>",df_tema$Série.histórica.ou.ano[i])
            )
#            df_tema$Nova.Descrição[i]
          )
        )
      })
    )
  )
}

# =========================
# PÁGINA PRINCIPAL
# =========================
indicador_ui <- function(metadados) {
  
  metadados <- metadados[order(metadados$n_apr), ]
  temas <- unique(metadados$tema)
  n_temas <- length(temas)
  
  blocos <- lapply(temas, function(t) {
    df_tema <- metadados[metadados$tema == t, ]
    criar_bloco_tema(df_tema)
  })

  tagList(
    div(class =  "metodo-page",
    div(class = "metodo-hero",
      div( class = "metodo-hero-text",
    h1("Que dados compõem o Painel?"),
    div(
    class = "linhas-decorativas",
    span(),
    span(),
    span(),
    span()
    ),  
    div(
    class = "metodologia-section",
    div(
      class = "metodo-metricas",
    div(
      class = "metodo-metrica-card",
      div(class = "metodo-metrica-numero", "14"),
      div(class = "metodo-metrica-label", "indicadores")
    ),

    div(
      class = "metodo-metrica-card",
      div(class = "metodo-metrica-numero", "4"),
      div(class = "metodo-metrica-label", "áreas temáticas")
    )
  ),
      div(
        class = "metodologia-intro",
        "O Painel trabalha com 14 indicadores de exposição ambiental que visam refletir os principais elementos que impactam na saúde e bem-estar das crianças e adolescentes brasileiras para os quais temos informações. Os dados foram obtidos de bases públicas confiáveis e municipalizadas, utilizando os registros mais recentes disponíveis.",
        tags$button(
        id = "baixar_diagnostico_pdf",
        type = "button",
        class = "btn-baixar-metodo",
        "Acesse a metodologia completa"
      )),
      ))),
    div(
      class = "indicador-section",
      
      h2(
        "Descrição dos indicadores por área temática",
        class = "indicadores-titulo"
      ),
      p("Clique em uma área para explorar os indicadores correspondentes"),
      
      div(
        class = "indicadores-layout-dinamico",
        style = paste0(
          "display:grid; ",
          "gap: 15px; ",
          "align-items: start;"
        ),
        blocos
      )
    ),
    
    tags$script(HTML("
      function toggleTema(el) {
        const lista = el.querySelector('.indicador-lista');
        if (lista) {
          lista.classList.toggle('hidden');
        }
      }
    ")),
    div(
    class = "metodologia--box-fim",
    tags$img(src = "icons/alerta.svg", height = "200px"),
    tags$p("Há outros elementos relacionados ao ambiente escolar, aos eventos climáticos extremos, à infraestrutura ambiental e urbana e à qualidade do ar, assim como outros temas que impactam a exposição de crianças e adolescentes a riscos ambientais, para os quais não existem dados disponíveis ou cujas informações não estão acessíveis no nível municipal. Embora existam diversos outros temas que impactam o bem-estar de crianças e adolescentes, os 14 indicadores apresentados oferecem uma visão ampla para orientar a atuação de gestores públicos."),
  )))
}



pagina_indicador_ui <- function(df) {

  nome_arquivo_pdf <- paste0(
    gsub("[^A-Za-z0-9_-]", "_", df$Título[1]),
    "_ficha_indicador.pdf"
  )

  div(
    class = "pagina-indicador-export-wrapper",

    div(
      id = "indicador_pdf",
      class = "pagina-indicador",

      div(
        class = "indicador-hero",

        div(
          class = "pagina-indicador-titulo",
          df$Título[1]
        ),

        div(
          class = "pagina-indicador-bloco",
          style = paste0(
            "background:", df$cor_dark[1], ";",
            "color:#ffffff;",
            "font-weight:700;"
          ),
          HTML(paste0(
            "<b style='color:#ffffff;'>Tema:</b> ",
            "<span class='tema-texto'>",
            df$tema[1],
            "</span>"
          ))
        ),
    #         div(
    #   class = "pagina-indicador-download-area no-export",
    #   tags$button(
    #     id = "baixar_indicador_pdf",
    #     type = "button",
    #     class = "btn-baixar-indicador",
    #     "Baixar ficha do indicador em PDF"
    #   )
    # ),

        div(
          class = "pagina-indicador-bloco",
          style = paste0(
            "background:#F8F4ED;",
            "color:#1e523c;",
            "font-weight:700;"
          ),
          HTML(paste0(
            "<b>Descrição do indicador: </b>",
            df$Descrição[1]
          ))
        )
      ),

      div(
        class = "pagina-indicador-bloco",
        HTML(paste0("<b>Unidade: </b>", df$Unidade[1]))
      ),

      div(
        class = "pagina-indicador-bloco",
        HTML(paste0("<b>Periodicidade: </b>", df$Periodicidade[1]))
      ),

      div(
        class = "pagina-indicador-bloco",
        HTML(paste0("<b>Série histórica/ano: </b>", df$Série.histórica.ou.ano[1]))
      ),

      div(
        class = "pagina-indicador-bloco",
        HTML(paste0("<b>Fonte: </b>", df$Fonte[1]))
      ),

      div(
        class = "pagina-indicador-bloco bloco-calculo",
        HTML(paste0("<b>Cálculo: </b>", df$Cálculo[1]))
      )
    ),

    actionButton(
      "voltar_indicadores",
      "Voltar para indicadores",
      class = "btn-voltar-ind no-export"
    ),

tags$script(HTML(paste0("
  document.getElementById('baixar_indicador_pdf').addEventListener('click', function() {

    const elemento = document.getElementById('indicador_pdf');

    if (!elemento) {
      alert('Não foi possível localizar o conteúdo do indicador.');
      return;
    }

    const largura = elemento.scrollWidth;
    const altura = elemento.scrollHeight;

    const opcoes = {
      margin: 0,
      filename: '", nome_arquivo_pdf, "',
      image: {
        type: 'jpeg',
        quality: 0.98
      },
      html2canvas: {
        scale: 2,
        useCORS: true,
        backgroundColor: '#F8F4ED',
        scrollY: 0
      },
      jsPDF: {
        unit: 'px',
        format: [largura, altura],
        orientation: 'portrait'
      },
      pagebreak: {
        mode: []
      }
    };

    html2pdf()
      .set(opcoes)
      .from(elemento)
      .save();
  });
")))
  )
}


# pagina_indicador_ui <- function(df) {

#   nome_arquivo_pdf <- paste0(
#     gsub("[^A-Za-z0-9_-]", "_", df$Novo.Título[1]),
#     "_indicador.pdf"
#   )

#   div(
#     class = "pagina-indicador-export-wrapper",

#     div(
#       class = "pagina-indicador-download-area no-export",
#       tags$button(
#         id = "baixar_indicador_pdf",
#         type = "button",
#         class = "btn-baixar-indicador",
#         "Baixar ficha do indicador"
#       )
#     ),

#     div(
#       id = "indicador_pdf",
#       class = "pagina-indicador",

#    div(
#     class = "indicador-hero",   
#     div(class = "pagina-indicador-titulo", df$Título[1]),
    
#     div(class = "pagina-indicador-bloco", 
#         style = paste0(
#           "background:", df$cor_dark,";",
#           "color: #ffffff;",
#           "font-weight: 700;"),
#     HTML( paste0(
#             "<b style='color:#ffffff;'>Tema:</b> ",
#             "<span class='tema-texto'>",
#             df$tema[1],
#             "</span>"
#             )
#         )),

#       div(
#         class = "pagina-indicador-bloco",
#         HTML(paste0(
#           "<b>Descrição do indicador: </b>",
#           df$Descrição[1]
#         ))
#       ),

#       div(
#         class = "pagina-indicador-bloco",
#         HTML(paste0("<b>Unidade: </b>", df$Unidade[1]))
#       ),

#       div(
#         class = "pagina-indicador-bloco",
#         HTML(paste0("<b>Granularidade: </b>", df$Granularidade[1]))
#       ),

#       div(
#         class = "pagina-indicador-bloco",
#         HTML(paste0("<b>Periodicidade: </b>", df$Periodicidade[1]))
#       ),

#       div(
#         class = "pagina-indicador-bloco",
#         HTML(paste0("<b>Série histórica/ano: </b>", df$Série.histórica.ou.ano[1]))
#       ),

#       div(
#         class = "pagina-indicador-bloco",
#         HTML(paste0("<b>Cálculo: </b>", df$Cálculo[1]))
#       ),

#       div(
#         class = "pagina-indicador-bloco",
#         HTML(paste0("<b>Conceituação: </b>", df$Conceituação[1]))
#       ),

#       div(
#         class = "pagina-indicador-bloco",
#         HTML(paste0("<b>Fonte: </b>", df$Fonte[1]))
#       ),

#       div(
#         class = "pagina-indicador-bloco",
#         HTML(paste0("<b>Categorias de análise: </b>", df$Categorias.de.análise[1]))
#       ),

#       div(
#         class = "pagina-indicador-bloco",
#         HTML(paste0("<b>Interpretação e Uso: </b>", df$Interpretação.e.Uso[1]))
#       )
#     ),

#     actionButton(
#       "voltar_indicadores",
#       "Voltar para indicadores",
#       class = "btn-voltar-ind no-export"
#     ),

#     tags$script(HTML(paste0("
#       document.getElementById('baixar_indicador_pdf').addEventListener('click', function() {
#         const elemento = document.getElementById('indicador_pdf');

#         if (!elemento) {
#           alert('Não foi possível localizar o conteúdo do indicador.');
#           return;
#         }

#         const largura = elemento.scrollWidth;
#         const altura = elemento.scrollHeight;

#         const opcoes = {
#           margin: 0,
#           filename: '", nome_arquivo_pdf, "',
#           image: {
#             type: 'jpeg',
#             quality: 0.98
#           },
#           html2canvas: {
#             scale: 2,
#             useCORS: true,
#             backgroundColor: '#F8F4ED',
#             scrollY: 0
#           },
#           jsPDF: {
#             unit: 'px',
#             format: [largura, altura],
#             orientation: 'portrait'
#           },
#           pagebreak: {
#             mode: []
#           }
#         };

#         html2pdf()
#           .set(opcoes)
#           .from(elemento)
#           .save();
#       });
#     ")))
#   ))
# }