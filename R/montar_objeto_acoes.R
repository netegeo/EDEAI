# =========================================================
# Arquivo: R/montar_objeto_acoes.R
# Finalidade:
#   Montar um objeto consolidado de acoes dado a selecao efetuada,
#   reunindo:
#     - informações básicas do município
#     - indicadores em formato longo
#     - valores brutos
#     - valores classificados
#     - cores de fundo e texto
#
# Entradas:
#   - dados_app: lista global com as bases já carregadas
#   - uf: sigla da unidade federativa
#   - municipio: nome do município
#
# Saída:
#   Uma lista com:
#     - basico: informações gerais do município
#     - indicadores: tabela longa única com todos os indicadores
#     - temas: lista de data.frames separados por tema
#
# Observações:
#   - A ordenação dos temas é fixa
#   - A ordenação dos indicadores dentro do tema é feita da
#     maior classificação para a menor (3, 2, 1)
#   - O título exibido do indicador é sempre o campo
#     'Novo.Título'
# =========================================================
print("montar_objeto_acoes.R carregado com sucesso")


  # -----------------------------------------------------
  # Função para formatar referencias das ações 
  # -----------------------------------------------------

 formatar_referencias <- function(texto_ref) {

  if (is.null(texto_ref) || is.na(texto_ref) || texto_ref == "") {
    return(list())
  }

  blocos <- unlist(strsplit(texto_ref, "\\n\\s*\\n"))

  lapply(blocos, function(bloco) {

    linhas <- trimws(unlist(strsplit(bloco, "\\n")))
    linhas <- linhas[linhas != ""]

    titulo <- linhas[1]

    o_que_trata <- linhas[grepl("^O que trata:", linhas)]
    relacao <- linhas[grepl("^Relação com a ação:", linhas)]

    o_que_trata <- gsub("^O que trata:\\s*", "", o_que_trata)
    relacao <- gsub("^Relação com a ação:\\s*", "", relacao)

    list(
      titulo = titulo,
      o_que_trata = ifelse(length(o_que_trata) > 0, o_que_trata, ""),
      relacao = ifelse(length(relacao) > 0, relacao, "")
    )
  })
 }
 
enriquecer_acoes_com_temas <- function(dados_acao, tabela_cores) {

  cores_nivel3 <- tabela_cores[
    c("tema", "Codigo", "Título", "cor_dark")
  ]

  obter_temas_acao <- function(indicadores_txt) {

    if (is.null(indicadores_txt) || is.na(indicadores_txt) || trimws(indicadores_txt) == "") {
      return(character(0))
    }

    indicadores <- trimws(unlist(strsplit(indicadores_txt, ",")))

    temas <- unique(cores_nivel3$tema[
      cores_nivel3$Codigo %in% indicadores |
      cores_nivel3$Título %in% indicadores
    ])

    temas[!is.na(temas) & trimws(temas) != ""]
  }

  temas_lista <- lapply(dados_acao$Indicador_relacionado, obter_temas_acao)

  dados_acao$temas_lista <- temas_lista

  dados_acao$tema <- vapply(
    temas_lista,
    function(x) {
      if (length(x) == 0) {
        return(NA_character_)
      }

      paste(unique(x), collapse = "; ")
    },
    character(1)
  )

  dados_acao$cores_tema <- lapply(temas_lista, function(temas) {
    unique(cores_nivel3[
      cores_nivel3$tema %in% temas,
      c("tema", "cor_dark")
    ])
  })

  dados_acao
} 

montar_objeto_acoes <- function(dados_acao, selecao = list()) {

  dados <- dados_acao

  # -----------------------------------------------------
  # 1. Validação da seleção
  # -----------------------------------------------------

  if (is.null(selecao)) {
    selecao <- list()
  }

  campos_validos <- c(
    "tema",
    "indicador_relacionado",
    "acao",
    "local",
    "tipologia"
  )

  selecao <- selecao[names(selecao) %in% campos_validos]

  selecao <- lapply(selecao, function(x) {
    x <- unique(trimws(as.character(x)))
    x[!is.na(x) & x != ""]
  })

  selecao <- selecao[lengths(selecao) > 0]

  # -----------------------------------------------------
  # 2. Se não houver seleção, retorna todas as ações
  # -----------------------------------------------------

  if (length(selecao) == 0) {
    dados_filtrados <- dados
  } else {

    # ---------------------------------------------------
    # 3. Regra OR global:
    #    entra se atender a qualquer seleção feita
    # ---------------------------------------------------

    manter <- rep(FALSE, length(dados$ID))

    # Tema
    if ("tema" %in% names(selecao)) {
      manter <- manter | vapply(
        dados$temas_lista,
        function(x) {
          any(trimws(as.character(x)) %in% selecao$tema)
        },
        logical(1)
      )
    }

    # Indicador relacionado
    if ("indicador_relacionado" %in% names(selecao)) {
  manter <- manter | vapply(
    dados$Indicador_relacionado,
    function(x) {
      indicadores <- limpar_opcoes_filtro_acoes(x)
      any(indicadores %in% selecao$indicador_relacionado)
    },
    logical(1)
  )
}

    if ("acao" %in% names(selecao)) {
  manter <- manter | vapply(
    dados$acao,
    function(x) {
      valores <- limpar_opcoes_filtro_acoes(x)
      any(valores %in% selecao$acao)
    },
    logical(1)
  )
}

if ("local" %in% names(selecao)) {
  manter <- manter | vapply(
    dados$local,
    function(x) {
      valores <- limpar_opcoes_filtro_acoes(x)
      any(valores %in% selecao$local)
    },
    logical(1)
  )
}

if ("tipologia" %in% names(selecao)) {
  manter <- manter | vapply(
    dados$tipologia,
    function(x) {
      valores <- limpar_opcoes_filtro_acoes(x)
      any(valores %in% selecao$tipologia)
    },
    logical(1)
  )
}

    #dados_filtrados <- dados[manter, ]
    dados_filtrados <- dados_acao_df[manter, ]
  }

  # -----------------------------------------------------
  # 4. Remove duplicidades por ID
  # -----------------------------------------------------

  dados_filtrados <- dados_filtrados[!duplicated(dados_filtrados$ID), ]

  # -----------------------------------------------------
  # 5. Organiza campos para uso na interface
  # -----------------------------------------------------

  dados_filtrados$nome_curto 			<- dados_filtrados$acao
  dados_filtrados$nome_ficha 			<- dados_filtrados$Nome_ficha
  dados_filtrados$por_que_importante 	<- dados_filtrados$importancia
  dados_filtrados$descricao_acao 		<- dados_filtrados$descricao
  dados_filtrados$referencias 			<- dados_filtrados$referencia

  dados_filtrados$tags 					<- lapply(seq_len(nrow(dados_filtrados)), function(i) {

    tags <- c(
      unlist(dados_filtrados$temas_lista[i]),
      dados_filtrados$Indicador_relacionado[i],
      dados_filtrados$local[i],
      dados_filtrados$tipologia[i]
    )

    tags <- unique(trimws(as.character(tags)))
    tags[!is.na(tags) & tags != ""]
  })

  # -----------------------------------------------------
  # 6. Retorno final
  # -----------------------------------------------------

  list(
    selecao = selecao,
    total = nrow(dados_filtrados),
    ids = dados_filtrados$ID,
    dados = dados_filtrados
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


# -------------------------------------------------------------------- 

dados_acao <- list(ID 						= acao_objetos$dados$ID,
                   Nome_ficha 				= acao_objetos$dados$Nome_ficha,
                   Campo 					= acao_objetos$dados$Campo,
                   Valor 					= acao_objetos$dados$Valor,
                   tema 					= acao_objetos$dados$tema,
                   Indicador_relacionado 	= acao_objetos$dados$Indicador_relacionado,
                   importancia 				= acao_objetos$dados$importancia,
                   acao 					= acao_objetos$dados$acao,
                   tipologia 				= acao_objetos$dados$tipologia,
                   local 					= acao_objetos$dados$local,
                   descricao 				= acao_objetos$dados$descricao,
                   observacao 				= acao_objetos$dados$observacao,
                   referencia 				= lapply(acao_objetos$dados$referencia,formatar_referencias))

dados_acao <- enriquecer_acoes_com_temas(dados_acao,dados_app$info$metadado)

dados_acao_df <- data.frame(
  ID                    = dados_acao$ID,
  Nome_ficha            = dados_acao$Nome_ficha,
  Campo                 = dados_acao$Campo,
  Valor                 = dados_acao$Valor,
  tema                  = dados_acao$tema,
  Indicador_relacionado = dados_acao$Indicador_relacionado,
  importancia           = dados_acao$importancia,
  acao                  = dados_acao$acao,
  tipologia             = dados_acao$tipologia,
  local                 = dados_acao$local,
  descricao             = dados_acao$descricao,
  observacao            = dados_acao$observacao,
  stringsAsFactors      = FALSE
)

dados_acao_df$referencia  <- dados_acao$referencia
dados_acao_df$temas_lista <- dados_acao$temas_lista
dados_acao_df$cores_tema  <- dados_acao$cores_tema

# Para testes

#obj_acoes <- montar_objeto_acoes(
#  dados_acao,
#  selecao = list(
#    tema = c("Ambiente Escolar","Qualidade do Ar"),
#    local = c("Cidade","Institucional"),
#    tipologia = c("Física ou tecnológica","Baseada na natureza"),
#    acao = c("Transformar"),
#    indicador_relacionado =c("Secas")
#  )
#)

#obj_acoes$total
#dim(obj_acoes$dados)
#names(obj_acoes$dados)
