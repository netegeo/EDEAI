# =========================
# DADOS
# =========================

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
      "background: #ffffff;",
      "border: 2px solid ", tom_dark,";", 
      "border-top: 2px solid",tom_dark,";",   
      "border-radius: 30px;",
      "padding: 18px;"
    ),
    
    div(
      class = "indicador-box-head",
      onclick = "toggleTema(this.parentElement)",
      
      div(class = "indicador-box-titulo", titulo_tema,tags$span(class = "indicador-seta-dropdown")),
      div( class = "linhas-indicador",
           style = paste0(
              "background:",tom_dark,";",
              "width: 100px;"),
          span()),
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
            onclick = sprintf("
                        setTimeout(function() {
                          window.scrollTo({
                          top: 0,
                          behavior: 'smooth'});
                          }, 50);
                        Shiny.setInputValue(
                          'indicador_selecionado',
                            '%s',
                          {priority: 'event'}
                          );
                        return false;",
              df_tema$Codigo[i]),
              df_tema$Título[i],tags$span(class = "indicador-seta-left")),
          
          div(
            class = "indicador-item-desc",
            HTML(paste0("<b>Descrição do indicador: </b>", df_tema$Descrição[i],
            "</br> <b>Unidade: </b>",df_tema$Unidade[i],
            "</br> <b>Fonte: </b>",df_tema$Fonte[i],
            "</br> <b>Série histórica: </b>",df_tema$Série.histórica.ou.ano[i])
            )
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
  
  metadados <- metadados[order(metadados$tema), ]
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
            div( class = "linhas-decorativas",
                span(),
                span(),
                span(),
                span()
          )),  
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

  # NOVA COLUNA
  div(
    class = "metodo-texto-area",

    div(
      class = "metodologia-intro",
      "O Painel trabalha com 14 indicadores de exposição ambiental que visam refletir os principais elementos que impactam na saúde e bem-estar das crianças e adolescentes brasileiras para os quais temos informações. Os dados foram obtidos de bases públicas confiáveis e municipalizadas, utilizando os registros mais recentes disponíveis."
    ),

div(
  class = "metodo-botoes",

tags$a(
  href = "files/Relatorio_Metodologico_Projeto.pdf",
  target = "_blank",

  tags$button(
    type = "button",
    class = "btn-baixar-metodo",
    "Acesse a metodologia completa"
  )
),
tags$a(
  href = "files/Relatorio_Metodologico_Projeto.pdf",
  target = "_blank",
  tags$button(
    id = "baixar_metadados",
    type = "button",
    class = "btn-baixar-metodo",
    "Baixar metadados"
  )
))
  )
))),

  ### Segundo Bloco 

    div(
      class = "indicador-section",
      
      h2(
        "Descrição dos indicadores por área temática",
        class = "indicadores-h2"
      ),
      p("Clique em uma área para explorar os indicadores correspondentes",
        class = "indicadores-t3"),
      
      div(
        class = "indicadores-layout-dinamico",
        style = paste0(
          "display:grid; ",
          "gap: 15px; ",
          "align-items: start;",
          "cursor: pointer;",
          "background: #ffffff"
          
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
    class = "metodologia-box-fim",
    div(class = "metodo-icon",
    tags$img(src = "icons/alerta.svg", height = "200px")),
    tags$p("Há outros elementos relacionados ao ambiente escolar, aos eventos climáticos extremos, à infraestrutura ambiental e urbana e à qualidade do ar, assim como outros temas que impactam a exposição de crianças e adolescentes a riscos ambientais, para os quais não existem dados disponíveis ou cujas informações não estão acessíveis no nível municipal. Embora existam diversos outros temas que impactam o bem-estar de crianças e adolescentes, os 14 indicadores apresentados oferecem uma visão ampla para orientar a atuação de gestores públicos.")
  ))
}

selecionar_acoes_por_indicador <- function(df, objeto_acoes) {

  if (is.null(df) || nrow(df) == 0) {
    return(data.frame(
      id_acao = character(0),
      acao = character(0),
      stringsAsFactors = FALSE
    ))
  }

  if (is.null(objeto_acoes) ||
      is.null(objeto_acoes$destaque) ||
      nrow(objeto_acoes$destaque) == 0) {
    return(data.frame(
      id_acao = character(0),
      acao = character(0),
      stringsAsFactors = FALSE
    ))
  }

  destaque <- objeto_acoes$destaque

  titulo_indicador <- limpar_espacos_unicode(as.character(df$Título[1]))

  codigo_indicador <- NA_character_

  if ("Codigo" %in% names(df)) {
    codigo_indicador <- limpar_espacos_unicode(as.character(df$Codigo[1]))
  } else if ("codigo" %in% names(df)) {
    codigo_indicador <- limpar_espacos_unicode(as.character(df$codigo[1]))
  }

  # -------------------------------------------------------
  # Compatibilidade com nomes possíveis das colunas
  # -------------------------------------------------------

  coluna_id <- intersect(
    c("id", "ID", "id_acao", "ID_acao"),
    names(destaque)
  )[1]

  coluna_acao <- intersect(
    c("acao", "Nome_ficha", "nome_ficha", "titulo", "ação"),
    names(destaque)
  )[1]

  coluna_indicador <- intersect(
    c(
      "indicador",
      "indicador_relacionado",
      "Indicador_relacionado",
      "Indicador Relacionado",
      "indicador_de_exposicao_relacionado",
      "Indicador_de_exposicao_relacionado",
      "indicadores",
      "Título",
      "titulo_indicador"
    ),
    names(destaque)
  )[1]

  if (is.na(coluna_id) || is.na(coluna_acao) || is.na(coluna_indicador)) {
    stop(
      paste0(
        "Não foi possível identificar as colunas necessárias em objeto_acoes$destaque. ",
        "Verifique se existem colunas de ID da ação, nome da ação e indicador relacionado."
      )
    )
  }

  destaque$id_acao_tmp 		<- limpar_espacos_unicode(as.character(destaque[[coluna_id]]))
  destaque$acao_tmp 		<- limpar_texto_html(as.character(destaque[[coluna_acao]]))
  destaque$indicador_tmp 	<- limpar_espacos_unicode(as.character(destaque[[coluna_indicador]]))

  # -------------------------------------------------------
  # Seleção das ações associadas ao indicador
  # -------------------------------------------------------
  # Primeiro tenta por código, se existir.
  # Depois tenta pelo título do indicador.

  if (!is.na(codigo_indicador) && codigo_indicador != "") {

    sel_acoes <- destaque[
      grepl(
        pattern = codigo_indicador,
        x = destaque$indicador_tmp,
        fixed = TRUE
      ),
      ,
      drop = FALSE
    ]

  } else {

    sel_acoes <- destaque[0, , drop = FALSE]
  }

  if (nrow(sel_acoes) == 0) {

    sel_acoes <- destaque[
      grepl(
        pattern = titulo_indicador,
        x = destaque$indicador_tmp,
        fixed = TRUE
      ),
      ,
      drop = FALSE
    ]
  }

  if (nrow(sel_acoes) == 0) {
    return(data.frame(
      id_acao = character(0),
      acao = character(0),
      stringsAsFactors = FALSE
    ))
  }

  sel_acoes <- data.frame(
    id_acao = sel_acoes$id_acao_tmp,
    acao = sel_acoes$acao_tmp,
    stringsAsFactors = FALSE
  )

  sel_acoes <- sel_acoes[
    !is.na(sel_acoes$id_acao) &
      sel_acoes$id_acao != "" &
      !is.na(sel_acoes$acao) &
      sel_acoes$acao != "",
    ,
    drop = FALSE
  ]

  sel_acoes <- sel_acoes[
    !duplicated(sel_acoes$id_acao),
    ,
    drop = FALSE
  ]

  rownames(sel_acoes) <- NULL

  sel_acoes
}

pagina_indicador_ui <- function(df, objeto_acoes) {

  sel_acoes <- selecionar_acoes_por_indicador(
    df = df,
    objeto_acoes = objeto_acoes
  )

  df_acoes <- if (nrow(sel_acoes) == 0) {

    tags$p("Não há ações recomendadas associadas a este indicador.")

  } else {

    tagList(
      lapply(seq_len(nrow(sel_acoes)), function(i) {

        tagList(

          tags$a(
            href = "#",
            onclick = sprintf(
              "Shiny.setInputValue('acao_selecionada', '%s', {priority: 'event'}); return false;",
              sel_acoes$id_acao[i]
            ),
            class = "linha-acao-link",
            limpar_texto_html(valor_txt(sel_acoes$acao[i])),
            tags$span(class = "acoes-seta-left")
          ),

          tags$br()
        )
      })
    )
  }

  div(
    class = "indicador-page",

    div(
      class = "indicador-hero",

      div(
        class = "indicador-page-titulo",
        df$Título[1]
      ),

      div(
        class = "indicador-tema",
        div(class = "indicador-temah1", "Tema:"),
        div(class = "indicador-temap", df$tema[1])
      ),

      div(
        class = "indicador-tema",
        div(class = "indicador-temah1", "Descrição do indicador:"),
        div(class = "indicador-temap", df$Descrição[1])
      ),

      div(
        class = "indicador-tema",
        div(class = "indicador-temah1", "Interpretação e Uso:"),
        div(class = "indicador-temap", df$Interpretação.e.Uso[1])
      )
    ),

    div(
      class = "indicador-grid",

      div(
        class = "indicador-coluna",
        div(class = "indicador-temah1", "Unidade:"),
        div(class = "indicador-temap", df$Unidade[1])
      ),

      div(
        class = "indicador-coluna",
        div(class = "indicador-temah1", "Periodicidade:"),
        div(class = "indicador-temap", df$Periodicidade[1])
      ),

      div(
        class = "indicador-coluna",
        div(class = "indicador-temah1", "Série histórica/ano:"),
        div(class = "indicador-temap", df$Série.histórica.ou.ano[1])
      ),

      div(
        class = "indicador-coluna",
        div(class = "indicador-temah1", "Fonte:"),
        div(class = "indicador-temap", df$Fonte[1])
      )
    ),

    div(
      class = "indicador-info",

      div(
        class = "indicador-bloco",
        tags$b("Cálculo:"),
        tags$div(
          class = "lista-acoes-indicador",
          df$Método.de.Cálculo[1]
        )
      ),

      div(
        class = "indicador-bloco",
        tags$b("Ações Recomendadas:"),
        tags$div(
          class = "lista-acoes-indicador",
          df_acoes
        )
      )
    ),

    actionButton(
      "voltar_indicadores",
      "Voltar",
      class = "btn-voltar-ind no-export",
      onclick = "setTimeout(function() {
        window.scrollTo({
          top: 0,
          behavior: 'smooth'
        });
      }, 50);"
    )
  )
}

