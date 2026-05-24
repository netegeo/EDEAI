# =========================================================
# Arquivo: R/montar_objeto_municipio.R
# Finalidade:
#   Montar um objeto consolidado do município selecionado,
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
print("montar_objeto_municipio.R carregado com sucesso")

# Temporario

dados_app$info$metadado$Título[dados_app$info$metadado$Título=="Dias com poluição do ar MP 2,5 acima do limite"]						<-"Dias com partículas finas acima do limite (PM2.5)"
dados_app$info$metadado$Título[dados_app$info$metadado$Título=="População sem água tratada"]											<-"Estimativa da População sem água tratada"
dados_app$info$metadado$Título[dados_app$info$metadado$Título=="Crianças e adolescentes sem acesso à água via rede de abastecimento"]	<-"Crianças e adolescentes sem água da rede geral"


montar_objeto_municipio <- function(dados_app, uf, municipio) {

  # -------------------------------------------------------
  # 1. Recupera as bases necessárias
  # -------------------------------------------------------
  inbase 				<- dados_app$inbase
  porte_pop 			<- dados_app$porte_pop

  metadado 				<- dados_app$info$metadado
  dados_brutos 			<- dados_app$info$dados_brutos
  dados_classificados 	<- dados_app$info$dados_classificados
  cores 				<- dados_app$info$cores

  # -------------------------------------------------------
  # 2. Filtra o município selecionado nas bases
  # -------------------------------------------------------
  base_mun 	<- inbase[
    inbase$sigla_uf == uf & inbase$nome_mun == municipio,
  ]

  bruto_mun <- dados_brutos[
    dados_brutos$sigla_uf == uf & dados_brutos$nome_mun == municipio,
  ]
  
  # Apenas para teste - Assim que aparecer o dado unicef trocar
  base_mun$selo_unicef<-NA  
  
  class_mun <- dados_classificados[
    dados_classificados$sigla_uf == uf & dados_classificados$nome_mun == municipio,
  ]

  # Se alguma base não retornar linha, interrompe
  if (NROW(base_mun) == 0 || NROW(bruto_mun) == 0 || NROW(class_mun) == 0) {
    return(NULL)
  }

  # -------------------------------------------------------
  # 3. Monta o bloco de informações básicas
  # -------------------------------------------------------
  porte_label <- porte_pop$limites[
    match(base_mun$porte[1], porte_pop$porte)
  ]

  basico 	<- list(
    geocodigo 			= base_mun$geocodigo[1],
    nome_mun 			= base_mun$nome_mun[1],
    sigla_uf 			= base_mun$sigla_uf[1],
    nm_regiao 			= base_mun$nm_regiao[1],
    populacao 			= base_mun$populacao[1],
    porte 				= base_mun$porte[1],
    porte_label 		= porte_label[1],
    perc_pop_infantoj 	= base_mun$perc_pop_infantoj[1],
    pop_top				= as.character(base_mun$ppopij[1]),
    mun_selo 			= verificar_selo_unicef(base_mun$selo_unicef[1])
  )

  # -------------------------------------------------------
  # 4. Ordem fixa dos temas
  # -------------------------------------------------------
  ordem_temas <- c(
    "Ambiente Escolar",
    "Eventos Climáticos Extremos",
    "Infraestrutura Ambiental e Urbana",
    "Qualidade do Ar"    
  )

  # -------------------------------------------------------
  # 5. Monta tabela longa dos indicadores
  # -------------------------------------------------------
  lista_indicadores <- lapply(seq_len(NROW(metadado)), function(i) {

    codigo 			<- metadado$Codigo[i]
    tema 			<- metadado$tema[i]
    nome_indicador 	<- metadado$Título[i]

    # Valor bruto do indicador para o município
    valor_bruto 	<- if (codigo %in% names(bruto_mun)) {
      round(bruto_mun[[codigo]][1],2)
    } else {
      NA
    }
    
	polaridade <- if ("polaridade" %in% names(metadado)) {
		metadado$polaridade[i]
	} else {
		NA
	}

	if (!is.na(valor_bruto) &&
		!is.na(polaridade) &&
		tolower(trimws(polaridade)) == "positiva") {
		valor_bruto <- round(100 - valor_bruto, 2)
	}

    # Valor classificado do indicador para o município
    valor_classificado <- if (codigo %in% names(class_mun)) {
      class_mun[[codigo]][1]
    } else {
      NA
    }
    
    valor_classificado_num <- to_numeric_safe(valor_classificado)
    
    # Unidade do Indicador
    unidade <- if ("Unidade" %in% names(metadado)) {
      metadado$Unidade[i]
    } else {
      NA
    }
	
	# Descricao Curta do Indicador
    descricao_curta <- if ("Descricao_curta" %in% names(metadado)) {
      metadado$Descricao_curta[i]
    } else {
      NA
    }
    
    # Busca de cores com base em:
    # tema + sigla + nível
    cor_sel 		<- cores[
      cores$tema == tema &
      cores$sigla == codigo &
      as.character(cores$nivel) == as.character(valor_classificado),
    ]

    cor_bg 			<- if (NROW(cor_sel) > 0) cor_sel$cor_bg[1] else NA
    cor_tex 		<- if (NROW(cor_sel) > 0) cor_sel$cor_tex[1] else NA

    data.frame(
      tema 					= tema,
      codigo 				= codigo,
      indicador 			= nome_indicador,
      valor_bruto 			= valor_bruto,
      unidade				= unidade,
      valor_classificado 	= valor_classificado_num,
      descricao_curta       = descricao_curta,
      cor_bg 				= cor_bg,
      cor_tex 				= cor_tex,
      stringsAsFactors 		= FALSE
    )
  })

  indicadores 		<- do.call(rbind, lista_indicadores)

  # -------------------------------------------------------
  # 6. Ajusta tema como fator com ordem fixa
  # -------------------------------------------------------
  indicadores$tema 	<- factor(indicadores$tema, levels = ordem_temas)

  # -------------------------------------------------------
  # 7. Ordena dentro do tema:
  #    classificação 3 -> 2 -> 1
  # -------------------------------------------------------
  indicadores 		<- indicadores[order(
    indicadores$tema,
    -indicadores$valor_classificado,
    indicadores$indicador
  ), ]

  rownames(indicadores) <- NULL

  # -------------------------------------------------------
  # 8. Separa os indicadores por tema
  # -------------------------------------------------------
  temas_ui <- lapply(ordem_temas, function(tema_atual) {

    df_tema <- indicadores[indicadores$tema == tema_atual, ]

    if (NROW(df_tema) == 0) {
      return(NULL)
    }

    meta_tema <- metadado[metadado$tema == tema_atual, ]

    if (NROW(meta_tema) == 0) {
      return(NULL)
    }

    list(
      tema 			= tema_atual,
      desc_tema 	= meta_tema$desc_tema[1],
      cor_bg_tema 	= meta_tema$cor_bg[1],
      cor_box 		= meta_tema$cor_box[1],
      indicadores 	= df_tema
    )
  })

  # Remove temas nulos
  temas_ui <- temas_ui[!vapply(temas_ui, is.null, logical(1))]

  # -------------------------------------------------------
  # 9. Retorna o objeto final do município
  # -------------------------------------------------------
  list(
    basico = basico,
    indicadores = indicadores,
    temas_ui = temas_ui
  )
}


# =========================================================
# Função auxiliar:
#   Converte classificação para número, preservando NA
# =========================================================
to_numeric_safe <- function(x) {
  if (length(x) == 0 || is.null(x) || is.na(x)) {
    return(NA_real_)
  }

  y <- suppressWarnings(as.numeric(x))

  if (is.na(y)) {
    return(NA_real_)
  }

  y
}

verificar_selo_unicef <- function(x) {
  if (length(x) == 0 || is.null(x) || is.na(x)) {
    return("Não")
  }

  x <- suppressWarnings(as.numeric(x[1]))

  if (!is.na(x) && x == 1) {
    return("Sim")
  }
}
