# =========================================================
# Arquivo: municipio_ui.R
# Finalidade:
#   Definir a interface da página de seleção municipal no
#   dashboard Shiny.
#
# Função principal:
#   - Permitir ao usuário escolher a UF e o município
#   - Acionar o carregamento do perfil municipal
#   - Reservar o espaço onde o card do município será exibido
#
# Observações:
#   - A lista de municípios é atualizada dinamicamente no server
#   - O card final é renderizado em output$card_municipio
# =========================================================

municipio_ui <- function() {
  tagList(
    div(
      class = "municipio-panel",

      # Título da seção de seleção territorial
      div(class = "municipio-title", "Selecionar Município"),

      fluidRow(

        # Seleção da Unidade da Federação (UF)
        column(
          width = 4,
          selectInput(
            "uf", "UF",
            choices = c(
              "AC", "AL", "AM", "AP", "BA", "CE", "DF", "ES", "GO", "MA",
              "MG", "MS", "MT", "PA", "PB", "PE", "PI", "PR", "RJ", "RN",
              "RO", "RR", "RS", "SC", "SE", "SP", "TO"
            ),
            selected = "DF"
          )
        ),

        # Seleção do município
        # Observação: as opções são atualizadas no servidor
        # conforme a UF selecionada.
        column(
          width = 5,
          selectInput(
            "municipio", "Município",
            choices = "Brasília",
            selected = "Brasília"
          )
        ),

        # Botão para disparar o carregamento do perfil municipal
        column(
          width = 3,
          actionButton(
            "carregar_municipio",
            "Carregar perfil",
            class = "btn-carregar"
          )
        )
      ),
      
      div(
        class = "municipio-texto-intro",
        "Nesta página, você encontra o diagnóstico do seu município de acordo com os 14 indicadores de exposição ambiental que compõem o painel. A ordem e a intensidade da cor dos indicadores apontam o nível de prioridade com que o tema deve ser tratado pelo município, considerando o seu desempenho comparado com outras cidades de mesmo porte."
      ),

      # Área onde será exibido o card do município selecionado
      uiOutput("card_municipio")
    )
  )
}
