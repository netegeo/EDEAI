# =========================================================
# Arquivo: app.R
# Finalidade:
#   Arquivo principal de inicialização do dashboard Shiny.
#   Este script carrega os pacotes necessários, importa os
#   arquivos de interface e servidor, e inicia a aplicação.
#
# Estrutura esperada:
#   - O arquivo "R/iu_basic-T.R" deve criar o objeto 'ui'
#   - O arquivo "R/server_basic.R" deve criar o objeto 'server'
#
# Observações:
#   - Este arquivo atua como ponto de entrada do app
#   - A lógica principal não está aqui, mas nos scripts
#     carregados por source()
# =========================================================

# -------------------------------------
# Importação dos scripts principais do app
# -------------------------------------
library(shiny)
library(bslib)
library(htmltools)
library(shinyjs)
library(shinymanager)
library(dplyr)
library(readr)
library(DT)

# ---------------------------------------------------------
# 1. Dados globais e funções auxiliares gerais
# ---------------------------------------------------------

source("R/carregar_dados.R")

print("Após carregar global.R:")
print(exists("acao_objetos"))
print(ls())

# ---------------------------------------------------------
# 2. Funções de montagem de objetos/dados
#    Devem vir antes da UI e do server.
# ---------------------------------------------------------
source("R/montar_objeto_municipio.R")
source("R/montar_objeto_acoes.R")

# ---------------------------------------------------------
# 3. Componentes visuais específicos
#    Devem vir antes da UI principal se forem chamados nela
#    ou no server.
# ---------------------------------------------------------
source("R/footer_ui.R")
source("R/municipio_ui.R")
source("R/municipio_card_ui.R")
source("R/indicador_ui.R")
source("R/quem_somos_ui.R")
source("R/acoes_recomendadas.R")
source("R/acoes_cards_ui.R")

# ---------------------------------------------------------
# 4. Interface principal
#    Deve vir depois dos componentes visuais.
# ---------------------------------------------------------
source("R/iu_basic-T.R")

# ---------------------------------------------------------
# 5. Server
#    Deve vir por último, pois usa dados, funções e UI.
# ---------------------------------------------------------
source("R/server_basic2.R")

# ---------------------------------------------------------
# 6. Execução local
# ---------------------------------------------------------
options(browser = "firefox")

shinyApp(ui, server)
