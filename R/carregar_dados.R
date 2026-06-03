# =========================================================
# Arquivo: global.R
# Finalidade:
#   Carregar, na inicialização da aplicação Shiny, os dados
#   estáticos e objetos de uso global.
#
# Papel deste arquivo:
#   - Executado uma vez no início da aplicação
#   - Deixa disponíveis, para ui e server, objetos que não
#     precisam ser relidos a cada interação do usuário
#
# Estrutura esperada do projeto:
#   - app.R
#   - global.R
#   - data/        -> arquivos .rds
#   - R/           -> subrotinas e componentes
#   - www/         -> css, fontes, logos
#
# Observações:
#   - Como os arquivos .rds são atualizados apenas por ano
#     ou por conjunto de anos, eles podem ser tratados como
#     dados estáticos da aplicação
#   - A leitura centralizada reduz redundância no server e
#     melhora a organização do projeto
# =========================================================

print("carregar_dados.R carregado com sucesso")

texto_valido <- function(x) {
  !is.na(x) & trimws(as.character(x)) != ""
}

limpar_base_municipal <- function(df) {
  df[
    texto_valido(df$sigla_uf) &
    texto_valido(df$nome_mun),
  ]
}

# ---------------------------------------------------------
# Carregamento das bases estáticas da aplicação
# ---------------------------------------------------------
# Cada elemento da lista 'dados_app' representa uma base
# carregada uma única vez na inicialização do app.
#
# Sugestão:
#   Use nomes curtos, claros e estáveis, pois esses objetos
#   serão referenciados depois no server, por exemplo:
#   dados_app$inbase
#   dados_app$porte_pop
# ---------------------------------------------------------

info_raw <- readRDS("data/informacoes_20260603.rds")

# Objeto responsavel por trazer as informações de ações públicas recomendadas
acao_objetos <- readRDS("data/objeto_acao_20260531.rds")

# Objeto responsavel por carregar as informações dos cards de apresentação da pagina inicial 
card_inicio <- readRDS("data/cards_inicio_20260524.rds")
info_card <- card_inicio$info_card_inicio

dados_app <- list(

  # Base principal com os dados municipais
  # Espera-se que contenha campos como:
  # geocodigo, nome_mun, sigla_uf, nm_regiao,
  # porte, populacao
  inbase = limpar_base_municipal(readRDS("data/inbase.rds")),

  # Tabela auxiliar de porte populacional
  # Usada para traduzir/categorizar o porte associado
  # ao município selecionado.
  porte_pop = readRDS("data/porte_populacao.rds"),
  
  # Base que contem o metadado, dados brutos, dados classificados
  # tabela de cores
  
  info = list(metadado				= info_raw$metadados,
			  dados_brutos 			= limpar_base_municipal(info_raw$brutos),
			  dados_classificados 	= limpar_base_municipal(info_raw$classe),
			  cores 				= info_raw$tabela_cores,
        selo_unicef   = info_raw$selo_unicef)
  
  # -------------------------------------------------------
  # Exemplos de futuras bases a serem adicionadas
  # -------------------------------------------------------
  # temas = readRDS("data/temas.rds"),
  # indicadores = readRDS("data/indicadores.rds"),
  # descricoes_tema = readRDS("data/descricoes_tema.rds"),
  # descricoes_indicador = readRDS("data/descricoes_indicador.rds"),
  # classificacoes = readRDS("data/classificacoes.rds"),
  # cores = readRDS("data/cores.rds")
)

# ---------------------------------------------------------
# Validações iniciais simples
# ---------------------------------------------------------
# Essas verificações ajudam a identificar problemas logo
# na inicialização, em vez de descobrir apenas durante o
# uso da aplicação.
# ---------------------------------------------------------

# Verifica se a base principal foi carregada corretamente
stopifnot(!is.null(dados_app$inbase))

# Verifica se a base auxiliar de porte populacional foi carregada
stopifnot(!is.null(dados_app$porte_pop))

# Verifica se a base de informações foi carregada
stopifnot(!is.null(dados_app$info))


# ---------------------------------------------------------
# Observação para manutenção futura
# ---------------------------------------------------------
# À medida que novas bases .rds forem incorporadas ao app,
# recomenda-se:
#   1. carregá-las nesta lista 'dados_app'
#   2. documentar brevemente sua finalidade
#   3. manter nomes padronizados
#
# Isso facilitará a etapa posterior de construção do objeto
# consolidado do município.
# ---------------------------------------------------------




                    
