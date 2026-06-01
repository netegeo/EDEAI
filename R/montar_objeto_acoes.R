# =========================================================
# Arquivo: R/montar_objeto_acoes.R
# Finalidade:
#   Montar e manipular o objeto de ações recomendadas,
#   com foco inicial no bloco "destaque", utilizado na
#   página "Ações Recomendadas".
#
# Estrutura esperada do destaque:
#   - id
#   - nome_da_ficha
#   - por_que_isso_e_importante
#   - indicador_de_exposicao_relacionado
#   - grupo_tematico
#   - estrategia_de_atuacao_da_acao
#   - tipologia_da_acao
#   - local_da_acao
#
# Saída principal:
#   objeto_acoes <- list(
#     destaque = destaque,
#     filtros  = filtros
#   )
#
# Observação:
#   - A filtragem por seleção do usuário é feita em função
#     separada: filtrar_acoes_recomendadas().
# =========================================================

print("montar_objeto_acoes.R carregado com sucesso")


# =========================================================
# 1. Funções auxiliares gerais
# =========================================================

limpar_texto_html <- function(x) {
  if (is.null(x)) return(x)

  x <- as.character(x)

  # Entidades HTML e espaços especiais
  x <- gsub("&nbsp;", " ", x, fixed = TRUE)
  x <- gsub("&#160;", " ", x, fixed = TRUE)
  x <- gsub("\u00A0", " ", x, fixed = TRUE)
  x <- gsub("\u202F", " ", x, fixed = TRUE)
  x <- gsub("\u2007", " ", x, fixed = TRUE)

  # Caracteres invisíveis
  x <- gsub("\u200B", "", x, fixed = TRUE)
  x <- gsub("\u200C", "", x, fixed = TRUE)
  x <- gsub("\u200D", "", x, fixed = TRUE)
  x <- gsub("\uFEFF", "", x, fixed = TRUE)

  # Hífens invisíveis / hifenização suave
  x <- gsub("\u00AD", "", x, fixed = TRUE)
  x <- gsub("&shy;", "", x, fixed = TRUE)

  # Quebras HTML ou textuais que podem ter vindo da base
  x <- gsub("<br>", " ", x, fixed = TRUE)
  x <- gsub("<br/>", " ", x, fixed = TRUE)
  x <- gsub("<br />", " ", x, fixed = TRUE)
  x <- gsub("\n", " ", x, fixed = TRUE)
  x <- gsub("\r", " ", x, fixed = TRUE)
  x <- gsub("\t", " ", x, fixed = TRUE)

  # Normaliza múltiplos espaços
  x <- gsub("[[:space:]]+", " ", x)

  trimws(x)
}

limpar_espacos_unicode <- function(x) {

  x <- as.character(x)

  # Troca espaços não separáveis por espaço comum
  x <- gsub("\u00A0", " ", x, fixed = TRUE)

  # Troca outros espaços Unicode comuns por espaço comum
  x <- gsub("[\u2000-\u200B\u202F\u205F\u3000]", " ", x)

  # Reduz múltiplos espaços para um único espaço
  x <- gsub("\\s+", " ", x)

  # Remove espaços no começo e no fim
  trimws(x)
}

normalizar_chave_filtro <- function(x) {

  x <- limpar_espacos_unicode(x)
  x <- tolower(x)

  x <- iconv(
    x,
    from = "",
    to = "ASCII//TRANSLIT"
  )

  x <- gsub("'", "", x)
  x <- limpar_espacos_unicode(x)

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

  marcador_pm25 <- "__PM25_DECIMAL__"

  x <- gsub(
    pattern = "PM\\s*2\\s*,\\s*5",
    replacement = marcador_pm25,
    x = x,
    ignore.case = TRUE
  )

  x <- unlist(strsplit(x, separador, fixed = TRUE))

  x <- gsub(
    pattern = marcador_pm25,
    replacement = "PM 2,5",
    x = x,
    fixed = TRUE
  )

  x <- limpar_espacos_unicode(x)

  x <- x[!is.na(x) & x != ""]

  chave <- normalizar_chave_filtro(x)

  x <- x[!duplicated(chave)]

  sort(x)
}

texto_valido_acao <- function(x) {

  if (is.null(x) || length(x) == 0) {
    return(FALSE)
  }

  x <- as.character(x[1])

  !is.na(x) && trimws(x) != ""
}

valor_texto_acao <- function(x, vazio = NA_character_) {

  if (!texto_valido_acao(x)) {
    return(vazio)
  }

  trimws(as.character(x[1]))
}

comparar_opcoes_filtro_acoes <- function(valores, selecao, separador = ",") {

  valores <- limpar_opcoes_filtro_acoes(
    valores,
    separador = separador
  )

  selecao <- limpar_espacos_unicode(selecao)
  selecao <- selecao[!is.na(selecao) & selecao != ""]

  if (length(valores) == 0 || length(selecao) == 0) {
    return(FALSE)
  }

  valores_norm <- normalizar_chave_filtro(valores)
  selecao_norm <- normalizar_chave_filtro(selecao)

  any(valores_norm %in% selecao_norm)
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
# 2. Cores dos temas
# =========================================================

montar_tabela_cores_tema <- function(tabela_cores) {

  if (is.null(tabela_cores)) {
    return(NULL)
  }

  tabela_cores <- padronizar_colunas_acoes(tabela_cores)

  colunas_necessarias <- c("tema", "cor_box", "cor_dark")

  if (!all(colunas_necessarias %in% names(tabela_cores))) {
    return(NULL)
  }

  cores_tema <- unique(tabela_cores[, colunas_necessarias])

  names(cores_tema) <- c(
    "grupo_tematico",
    "cor_tema",
    "cor_tema_dark"
  )

  cores_tema$grupo_tematico <- trimws(as.character(cores_tema$grupo_tematico))
  cores_tema$cor_tema <- trimws(as.character(cores_tema$cor_tema))
  cores_tema$cor_tema_dark <- trimws(as.character(cores_tema$cor_tema_dark))

  cores_tema
}

enriquecer_destaque_com_cores <- function(destaque, tabela_cores = NULL) {

  cores_tema <- montar_tabela_cores_tema(tabela_cores)

  if (!is.null(cores_tema)) {

    destaque <- merge(
      destaque,
      cores_tema,
      by = "grupo_tematico",
      all.x = TRUE,
      sort = FALSE
    )

  } else {

    destaque$cor_tema <- NA_character_
    destaque$cor_tema_dark <- NA_character_
  }

  destaque$cor_tema[
    is.na(destaque$cor_tema) | destaque$cor_tema == ""
  ] <- "#E2ECE8"

  destaque$cor_tema_dark[
    is.na(destaque$cor_tema_dark) | destaque$cor_tema_dark == ""
  ] <- "#113131"

  destaque
}

montar_cores_tema_acao <- function(grupo_tematico, tabela_cores) {

  temas <- limpar_opcoes_filtro_acoes(
    grupo_tematico,
    separador = ","
  )

  if (length(temas) == 0 || is.null(tabela_cores)) {
    return(
      data.frame(
        tema = "Sem tema",
        cor_dark = "#113131",
        stringsAsFactors = FALSE
      )
    )
  }

  tabela_cores <- padronizar_colunas_acoes(tabela_cores)

  if (!all(c("tema", "cor_dark") %in% names(tabela_cores))) {
    return(
      data.frame(
        tema = temas,
        cor_dark = "#113131",
        stringsAsFactors = FALSE
      )
    )
  }

  tabela_aux <- unique(tabela_cores[, c("tema", "cor_dark")])

  tabela_aux$tema_norm <- normalizar_chave_filtro(tabela_aux$tema)
  temas_norm <- normalizar_chave_filtro(temas)

  cores <- tabela_aux[
    tabela_aux$tema_norm %in% temas_norm,
    c("tema", "cor_dark"),
    drop = FALSE
  ]

  if (nrow(cores) == 0) {
    cores <- data.frame(
      tema = temas,
      cor_dark = "#113131",
      stringsAsFactors = FALSE
    )
  }

  cores <- cores[!duplicated(normalizar_chave_filtro(cores$tema)), ]

  rownames(cores) <- NULL

  cores
}

inferir_grupo_tematico_por_indicador <- function(indicadores_txt, metadados) {

  if (is.null(indicadores_txt) ||
      length(indicadores_txt) == 0 ||
      is.na(indicadores_txt) ||
      trimws(as.character(indicadores_txt)) == "") {
    return(NA_character_)
  }

  indicadores <- limpar_opcoes_filtro_acoes(indicadores_txt)

  metadados_aux <- metadados
  metadados_aux$Codigo_norm <- normalizar_chave_filtro(metadados_aux$Codigo)
  metadados_aux$Titulo_norm <- normalizar_chave_filtro(metadados_aux$Título)

  indicadores_norm <- normalizar_chave_filtro(indicadores)

  temas <- unique(metadados_aux$tema[
    metadados_aux$Codigo_norm %in% indicadores_norm |
      metadados_aux$Titulo_norm %in% indicadores_norm
  ])

  temas <- limpar_espacos_unicode(temas)
  temas <- temas[!is.na(temas) & temas != ""]

  if (length(temas) == 0) {
    return(NA_character_)
  }

  paste(unique(temas), collapse = "; ")
}


# =========================================================
# 3. Montagem do destaque
# =========================================================

montar_destaque_acoes <- function(destaque_raw, tabela_cores = NULL) {

  destaque_raw <- padronizar_colunas_acoes(destaque_raw)

  # -------------------------------------------------------
  # Compatibilização de nomes alternativos, caso existam
  # -------------------------------------------------------

  if ("campo_tematico" %in% names(destaque_raw) &&
      !"grupo_tematico" %in% names(destaque_raw)) {
    names(destaque_raw)[names(destaque_raw) == "campo_tematico"] <- "grupo_tematico"
  }
  
  if ("indicador_relacionado" %in% names(destaque_raw) &&
      !"indicador_de_exposicao_relacionado" %in% names(destaque_raw)) {
    names(destaque_raw)[names(destaque_raw) == "indicador_relacionado"] <- "indicador_de_exposicao_relacionado"
  }

  if ("estrategia_de_atuacao" %in% names(destaque_raw) &&
      !"estrategia_de_atuacao_da_acao" %in% names(destaque_raw)) {
    names(destaque_raw)[names(destaque_raw) == "estrategia_de_atuacao"] <- "estrategia_de_atuacao_da_acao"
  }

  if ("tipologia" %in% names(destaque_raw) &&
      !"tipologia_da_acao" %in% names(destaque_raw)) {
    names(destaque_raw)[names(destaque_raw) == "tipologia"] <- "tipologia_da_acao"
  }

  if ("local" %in% names(destaque_raw) &&
      !"local_da_acao" %in% names(destaque_raw)) {
    names(destaque_raw)[names(destaque_raw) == "local"] <- "local_da_acao"
  }

  # -------------------------------------------------------
  # Validação das colunas obrigatórias
  # -------------------------------------------------------

  colunas_obrigatorias <- c(
    "id",
    "nome_da_ficha",
    "por_que_isso_e_importante",
    "indicador_de_exposicao_relacionado",
    "grupo_tematico",
    "estrategia_de_atuacao_da_acao",
    "tipologia_da_acao",
    "local_da_acao"
  )

  colunas_faltantes <- setdiff(colunas_obrigatorias, names(destaque_raw))

  if (length(colunas_faltantes) > 0) {
    stop(
      paste0(
        "As seguintes colunas estão ausentes no destaque das ações: ",
        paste(colunas_faltantes, collapse = ", ")
      )
    )
  }

  destaque <- destaque_raw[, colunas_obrigatorias]
  
  destaque[] <- lapply(destaque, function(x) {
    trimws(as.character(x))
  })

  # -------------------------------------------------------
  # Remove ações sem ID ou sem nome
  # -------------------------------------------------------

  destaque <- destaque[
    !is.na(destaque$id) &
      destaque$id != "" &
      !is.na(destaque$nome_da_ficha) &
      destaque$nome_da_ficha != "",
  ]

  # -------------------------------------------------------
  # Remove duplicidade por ID
  # -------------------------------------------------------

  destaque <- destaque[!duplicated(destaque$id), ]

  # -------------------------------------------------------
  # Adiciona campos úteis para a interface atual
  # -------------------------------------------------------

  destaque$nome_curto <- destaque$nome_da_ficha
  destaque$nome_ficha <- destaque$nome_da_ficha
  destaque$por_que_importante <- destaque$por_que_isso_e_importante

  destaque$tags <- lapply(seq_len(nrow(destaque)), function(i) {

    tags <- c(
      limpar_opcoes_filtro_acoes(destaque$grupo_tematico[i], separador = ","),
      limpar_opcoes_filtro_acoes(destaque$indicador_de_exposicao_relacionado[i]),
      limpar_opcoes_filtro_acoes(destaque$estrategia_de_atuacao_da_acao[i]),
      limpar_opcoes_filtro_acoes(destaque$local_da_acao[i]),
      limpar_opcoes_filtro_acoes(destaque$tipologia_da_acao[i])
    )

    tags <- unique(trimws(as.character(tags)))
    tags[!is.na(tags) & tags != ""]
  })

  destaque <- enriquecer_destaque_com_cores(
    destaque = destaque,
    tabela_cores = tabela_cores
  )
  
  destaque$cores_tema <- lapply(
     destaque$grupo_tematico,
     montar_cores_tema_acao,
     tabela_cores = tabela_cores
  )

  destaque <- destaque[order(destaque$id), ]

  rownames(destaque) <- NULL
  
  idx_sem_tema <- is.na(destaque$grupo_tematico) | destaque$grupo_tematico == ""

  if (any(idx_sem_tema) && !is.null(tabela_cores)) {

    destaque$grupo_tematico[idx_sem_tema] <- vapply(
      destaque$indicador_de_exposicao_relacionado[idx_sem_tema],
      inferir_grupo_tematico_por_indicador,
      metadados = tabela_cores,
      FUN.VALUE = character(1)
    )
  }

  destaque
}


# =========================================================
# 4. Montagem dos filtros
# =========================================================

montar_filtros_acoes <- function(destaque) {

  list(
    grupo_tematico = limpar_opcoes_filtro_acoes(
      destaque$grupo_tematico,
      separador = ","
    ),

    indicador_de_exposicao_relacionado = limpar_opcoes_filtro_acoes(
      destaque$indicador_de_exposicao_relacionado
    ),

    estrategia_de_atuacao_da_acao = limpar_opcoes_filtro_acoes(
      destaque$estrategia_de_atuacao_da_acao
    ),

    local_da_acao = limpar_opcoes_filtro_acoes(
      destaque$local_da_acao
    ),

    tipologia_da_acao = limpar_opcoes_filtro_acoes(
      destaque$tipologia_da_acao
    )
  )
}


# =========================================================
# 5. Montagem do objeto geral de ações
# =========================================================

montar_objeto_acoes <- function(acao_objetos, tabela_cores = NULL) {

  destaque <- montar_destaque_acoes(
    destaque_raw = acao_objetos$destaque,
    tabela_cores = tabela_cores
  )

  filtros <- montar_filtros_acoes(destaque)

  o_que_fazer <- montar_o_que_fazer_acoes(
    o_que_fazer_raw = acao_objetos$o_que_fazer
  )
  
  dicas_praticas <- montar_dicas_praticas_acoes(
    dicas_praticas_raw = acao_objetos$dicas_praticas
  )
  
  base_tecnica <- montar_base_tecnica_acoes(
    base_tecnica_raw = acao_objetos$base_tecnica
  )

  list(
    destaque = destaque,
    filtros = filtros,
    o_que_fazer = o_que_fazer,
    dicas_praticas = dicas_praticas,
    base_tecnica  = base_tecnica
  )
}

montar_objeto_acao <- function(objeto_acoes, id_acao) {

  if (is.null(objeto_acoes) || is.null(id_acao)) {
    return(NULL)
  }

  destaque <- objeto_acoes$destaque[
    objeto_acoes$destaque$id == id_acao,
    ,
    drop = FALSE
  ]

  if (nrow(destaque) == 0) {
    return(NULL)
  }

  o_que_fazer <- objeto_acoes$o_que_fazer[
    objeto_acoes$o_que_fazer$id == id_acao,
    ,
    drop = FALSE
  ]

  list(
    id = id_acao,
    destaque = destaque,
    o_que_fazer = o_que_fazer
  )
}

# =========================================================
# 6. Filtragem das ações recomendadas
# =========================================================

filtrar_acoes_recomendadas <- function(destaque, selecao = list(), regra = "OR") {

  dados <- destaque

  if (is.null(selecao)) {
    selecao <- list()
  }

  regra <- toupper(regra)

  if (!regra %in% c("OR", "AND")) {
    stop("A regra de filtragem deve ser 'OR' ou 'AND'.")
  }

  campos_validos <- c(
    "grupo_tematico",
    "indicador_de_exposicao_relacionado",
    "estrategia_de_atuacao_da_acao",
    "local_da_acao",
    "tipologia_da_acao"
  )

  selecao <- selecao[names(selecao) %in% campos_validos]

  selecao <- lapply(selecao, function(x) {
    x <- unique(trimws(as.character(x)))
    x[!is.na(x) & x != ""]
  })

  selecao <- selecao[lengths(selecao) > 0]

  if (length(selecao) == 0) {

    dados_filtrados <- dados

  } else {

    if (regra == "OR") {
      manter <- rep(FALSE, nrow(dados))
    } else {
      manter <- rep(TRUE, nrow(dados))
    }

    aplicar_regra <- function(vetor_logico, condicao) {
      if (regra == "OR") {
        vetor_logico | condicao
      } else {
        vetor_logico & condicao
      }
    }

    if ("grupo_tematico" %in% names(selecao)) {

      condicao <- vapply(
        dados$grupo_tematico,
         function(x) {
          comparar_opcoes_filtro_acoes(
           valores = x,
            selecao = selecao$grupo_tematico,
            separador = ","
        )
      },
      logical(1)
     )

     manter <- aplicar_regra(manter, condicao)
    }

    if ("indicador_de_exposicao_relacionado" %in% names(selecao)) {

      condicao <- vapply(
        dados$indicador_de_exposicao_relacionado,
        function(x) {
          comparar_opcoes_filtro_acoes(
             valores = x,
             selecao = selecao$indicador_de_exposicao_relacionado
          )
        },
        logical(1)
      )

      manter <- aplicar_regra(manter, condicao)
    }

    if ("estrategia_de_atuacao_da_acao" %in% names(selecao)) {

      condicao <- vapply(
        dados$estrategia_de_atuacao_da_acao,
        function(x) {
          comparar_opcoes_filtro_acoes(
               valores = x,
              selecao = selecao$estrategia_de_atuacao_da_acao
          )
        },
        logical(1)
      )

      manter <- aplicar_regra(manter, condicao)
    }

    if ("local_da_acao" %in% names(selecao)) {

      condicao <- vapply(
        dados$local_da_acao,
        function(x) {
          comparar_opcoes_filtro_acoes(
              valores = x,
              selecao = selecao$local_da_acao
          )
        },
        logical(1)
      )

      manter <- aplicar_regra(manter, condicao)
    }

    if ("tipologia_da_acao" %in% names(selecao)) {

      condicao <- vapply(
        dados$tipologia_da_acao,
        function(x) {
          comparar_opcoes_filtro_acoes(
              valores = x,
              selecao = selecao$tipologia_da_acao
          )
        },
        logical(1)
      )

      manter <- aplicar_regra(manter, condicao)
    }

    dados_filtrados <- dados[manter, ]
  }

  dados_filtrados <- dados_filtrados[!duplicated(dados_filtrados$id), ]

  rownames(dados_filtrados) <- NULL

  list(
    selecao = selecao,
    regra = regra,
    total = nrow(dados_filtrados),
    ids = dados_filtrados$id,
    dados = dados_filtrados
  )
}

montar_o_que_fazer_acoes <- function(o_que_fazer_raw) {

  if (is.null(o_que_fazer_raw)) {
    return(data.frame(
      id = character(0),
      o_que_fazer = character(0),
      stringsAsFactors = FALSE
    ))
  }

  o_que_fazer_raw <- padronizar_colunas_acoes(o_que_fazer_raw)

  # Compatibilidade com nomes possíveis
  if ("ID" %in% names(o_que_fazer_raw) &&
      !"id" %in% names(o_que_fazer_raw)) {
    names(o_que_fazer_raw)[names(o_que_fazer_raw) == "ID"] <- "id"
  }

  colunas_obrigatorias <- c(
    "id",
    "ordem",
    "o_que_fazer"
  )

  colunas_faltantes <- setdiff(
    colunas_obrigatorias,
    names(o_que_fazer_raw)
  )

  if (length(colunas_faltantes) > 0) {
    stop(
      paste0(
        "As seguintes colunas estão ausentes em o_que_fazer: ",
        paste(colunas_faltantes, collapse = ", ")
      )
    )
  }

  o_que_fazer <- o_que_fazer_raw[, colunas_obrigatorias]

  o_que_fazer[] <- lapply(o_que_fazer, function(x) {
    limpar_espacos_unicode(as.character(x))
  })

  o_que_fazer <- o_que_fazer[
    !is.na(o_que_fazer$id) &
      o_que_fazer$id != "",
  ]


  rownames(o_que_fazer) <- NULL

  o_que_fazer
}

montar_dicas_praticas_acoes <- function(dicas_praticas_raw) {

  if (is.null(dicas_praticas_raw)) {
    return(data.frame(
      id = character(0),
      ordem = character(0),
      dicas_praticas = character(0),
      stringsAsFactors = FALSE
    ))
  }

  dicas_praticas_raw <- padronizar_colunas_acoes(dicas_praticas_raw)

  # Compatibilidade com nomes possíveis
  if ("ID" %in% names(dicas_praticas_raw) &&
      !"id" %in% names(dicas_praticas_raw)) {
    names(dicas_praticas_raw)[names(dicas_praticas_raw) == "ID"] <- "id"
  }

  colunas_obrigatorias <- c(
    "id",
    "ordem",
    "dicas_praticas"
  )

  colunas_faltantes <- setdiff(
    colunas_obrigatorias,
    names(dicas_praticas_raw)
  )

  if (length(colunas_faltantes) > 0) {
    stop(
      paste0(
        "As seguintes colunas estão ausentes em dicas_praticas: ",
        paste(colunas_faltantes, collapse = ", ")
      )
    )
  }

  dicas_praticas <- dicas_praticas_raw[, colunas_obrigatorias]

  dicas_praticas[] <- lapply(dicas_praticas, function(x) {
    limpar_espacos_unicode(as.character(x))
  })

  dicas_praticas <- dicas_praticas[
    !is.na(dicas_praticas$id) &
      dicas_praticas$id != "",
  ]

  dicas_praticas <- dicas_praticas[
    !is.na(dicas_praticas$dicas_praticas) &
      dicas_praticas$dicas_praticas != "",
  ]

  ordem_num <- suppressWarnings(as.numeric(dicas_praticas$ordem))

  dicas_praticas <- dicas_praticas[
    order(
      dicas_praticas$id,
      ordem_num,
      na.last = TRUE
    ),
  ]

  rownames(dicas_praticas) <- NULL

  dicas_praticas
}

montar_base_tecnica_acoes <- function(base_tecnica_raw) {

  if (is.null(base_tecnica_raw)) {
    return(data.frame(
      id = character(0),
      id_referencia = character(0),
      titulo = character(0),
      o_que_trata = character(0),
      relacao = character(0),
      stringsAsFactors = FALSE
    ))
  }

  base_tecnica_raw <- padronizar_colunas_acoes(base_tecnica_raw)

  colunas_obrigatorias <- c(
    "id",
    "id_referencia",
    "titulo",
    "o_que_trata",
    "relacao"
  )

  colunas_faltantes <- setdiff(
    colunas_obrigatorias,
    names(base_tecnica_raw)
  )

  if (length(colunas_faltantes) > 0) {
    stop(
      paste0(
        "As seguintes colunas estão ausentes em base_tecnica: ",
        paste(colunas_faltantes, collapse = ", ")
      )
    )
  }

  base_tecnica <- base_tecnica_raw[, colunas_obrigatorias]

  base_tecnica[] <- lapply(base_tecnica, function(x) {
    limpar_espacos_unicode(as.character(x))
  })

  base_tecnica <- base_tecnica[
    !is.na(base_tecnica$id) &
      base_tecnica$id != "",
  ]

  base_tecnica <- base_tecnica[
    order(base_tecnica$id, base_tecnica$id_referencia),
  ]

  rownames(base_tecnica) <- NULL

  base_tecnica
}

# =========================================================
# 7. Montagem do objeto de ações da aplicação
# =========================================================

objeto_acoes <- montar_objeto_acoes(
  acao_objetos = acao_objetos,
  tabela_cores = dados_app$info$metadado
)
