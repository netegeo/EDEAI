# =========================================================
# Arquivo: quem_somos_ui.R
# Finalidade:
#   Definir a interface da página "Quem somos" e da página
#   complementar "Equipe do projeto".
#
# Observação:
#   Ajuste os nomes dos arquivos SVG conforme os arquivos
#   existentes em www/icons/.
# =========================================================

quem_somos_ui <- function() {

instituicao_bloco <- function(
    logo_src,
    logo_alt,
    texto,
    link = NULL,
    invertido = FALSE,
    max_width = "320px",
    max_height = "180px",
    width_p = "100%") 
    {

  logo_img <- tags$img(
    src = logo_src,
    alt = logo_alt,
    class = "quem-logo-img",
    style = paste0(
      "max-width:", max_width, ";",
      "max-height:", max_height, ";",
      "width:", width_p, ";"
    )
  )

  logo_box <- tags$div(
    class = "quem-logo-box",
    if (!is.null(link)) {
      tags$a(
        href = link,
        target = "_blank",
        logo_img
      )
    } else {
      logo_img
    }
  )

texto_box <- tags$div(
  class = "quem-texto-box",

  tags$p(texto),

  if (!is.null(link)) {
    tags$div(
      class = "quem-link-site",

      tags$strong("Acesse o site:"),
      tags$br(),

      tags$a(
        href = link,
        target = "_blank",
        link
      )
    )
  }
)

  tags$div(
    class ="quem-instituicao-bloco",
      tagList(logo_box, texto_box)
  )
}

  tagList(
    tags$section(
      class = "quem-hero",
      tags$div(
        class = "quem-hero-inner",
        tags$h1("Quem somos"),
          div(
    class = "linhas-decorativas",
    span(),
    span(),
    span(),
    span()
    ),
        tags$p(
          "O Ministério do Meio Ambiente e Mudança do Clima firmou parceria com o UNICEF e a Vital Strategies para a criação desta plataforma, que disponibiliza informações para toda a sociedade, em especial para gestores públicos dos municípios. Esse projeto reúne dados e informações para garantir o direito de crianças e adolescentes a um ambiente limpo, saudável e seguro."
        )
      )
    ),

    tags$section(
      class = "quem-section",
      tags$div(
        class = "quem-section-inner",

        instituicao_bloco(
          logo_src = "icons/ReguaDeLogos-MMA2.svg",
          logo_alt = "Ministério do Meio Ambiente e Mudança do Clima",
          texto = "O Ministério do Meio Ambiente e Mudança do Clima (MMA) é o órgão federal brasileiro responsável por formular, implementar e coordenar políticas públicas ambientais e climáticas. Sua missão é proteger o meio ambiente, promover o desenvolvimento socioeconômico sustentável, o uso racional dos recursos naturais, a biodiversidade e a transição para uma economia de baixas emissões./n",
          link = "https://www.gov.br/mma/pt-br",
          invertido = FALSE,
          width_p = "120%"
        ),

        instituicao_bloco(
          logo_src = "icons/Logo-Brazil-Unicef.svg",
          logo_alt = "UNICEF",
          texto = "O UNICEF, Fundo das Nações Unidas para a Infância, trabalha para proteger os direitos de cada criança e adolescente, em todos os lugares, especialmente os mais vulneráveis, nos locais mais remotos. Em mais de 190 países e territórios, fazemos o que for preciso para ajudar crianças e adolescentes a sobreviver, prosperar e alcançar seu pleno potencial. Em 2025, o UNICEF comemorou 75 anos no Brasil. O trabalho do UNICEF é financiado inteiramente por contribuições voluntárias.\n",
          link = "https://www.unicef.org/brazil/",
          invertido = FALSE,
          max_width = "200px",
          max_height = "180px",
          width_p = "1300%"
        ),

        instituicao_bloco(
          logo_src = "icons/Vital_Strategies_Logo_screen_blue_RGB.svg",
          logo_alt = "Vital Strategies",
          texto = "A Vital Strategies é uma organização global de saúde presente em mais de 80 países. No Brasil desde 2017, trabalha em parceria com governos e sociedade civil para influenciar políticas, práticas e pessoas no enfrentamento dos maiores desafios de saúde pública do país. O trabalho foca em soluções de políticas públicas baseadas em evidências e capazes de gerar resultados duradouros, sustentáveis e de alto impacto.\n",
          link = "https://www.vitalstrategies.org/sao-paulo-brasil/",
          invertido = FALSE,
          width_p = "110%"
        ),

        tags$div(
          class = "quem-equipe-cta",
          actionButton(
            "btn_equipe_projeto",
            "Conheça a equipe do projeto",
            class = "btn-equipe-projeto"
          )
        )
      )
    )
  )
}


equipe_projeto_ui <- function() {

  membro <- function(cargo, nome) {
    tagList(
      div(class = "equipe-item",  
      tags$p(class = "equipe-cargo", cargo),
      tags$p(class = "equipe-nome", nome)
    ))
  }

  card_equipe <- function(titulo, conteudo) {
    tags$div(
      class = "equipe-card",
      tags$h2(titulo),
      conteudo
    )
  }

  tagList(
    tags$section(
      class = "equipe-page",

      tags$div(
        class = "equipe-header",
        tags$h1("Conheça a equipe do projeto"),
        tags$div(
          class = "equipe-linhas",
          tags$span(class = "linha linha-1"),
          tags$span(class = "linha linha-2"),
          tags$span(class = "linha linha-3"),
          tags$span(class = "linha linha-4")
        )
      ),



div(
  class = "equipe-grid",

div(
  class = "equipe-card dupla",

  div(
    class = "equipe-instituicao mma",
    tags$h2("MINISTÉRIO DO MEIO AMBIENTE E MUDANÇA DO CLIMA"),
    div(class = "equipe-lista",
      membro("Diretor do Departamento de Meio Ambiente Urbano", "Carlos Maurício da Fonseca Guerra"),
      membro("Coordenador-Geral de Cidades Sustentáveis", "Salomar Mafaldo de Amorim Junior"),
      membro("Coordenadora-Geral de Adaptação de Ambientes Urbanos à Mudança do Clima", "Ana Luísa Teixeira de Campos"),
      membro("Coordenador-Geral de Tecnologia da Informação", "Jonas Jeske"),
      membro("Coordenador-Geral de Tecnologia da Informação", "Diego Rodrigues Cavalcanti"),
      membro("Analistas Ambientais", "Célia Regina Miranda Melo"),
      membro("Analistas Ambientais", "Leonardo Mendonça de Lima")
    )
  ),

  div(
    class = "equipe-instituicao unicef",
    tags$h2("FUNDO DAS NAÇÕES UNIDAS PARA A INFÂNCIA (UNICEF)"),
    div(class = "equipe-lista",
            membro("Representante do UNICEF no Brasil", "Youssouf Abdel-Jelil"),
            membro("Representante-Adjunta de Programas", "Layla Saad"),
            membro("Chefe de Comunicação e Parcerias", "Sonia Yeo"),
            membro("Chefe de Água, Saneamento e Higiene, e de Clima e Meio Ambiente", "Gregory Bulit"),
            membro("Chefe de Saúde e Nutrição", "Luciana Phebo"),
            membro("Especialista em Clima e Meio Ambiente", "Danilo Moura"),
            membro("Especialista em Comunicação Digital", "Camilo Leon"),
            membro("Oficial de Comunicação", "Elisa Meirelles Reis"),
            membro("Oficial de Monitoramento e Avaliação", "Anderson Macedo"),
            membro("Oficial de Saúde", "Gerson da Costa Filho")
    )
  )
)),


div(
  class = "equipe-grid",

div(
  class = "equipe-card dupla",

  div(
    class = "equipe-instituicao mma",
    tags$h2("VITAL STRATEGIES"),
    div(class = "equipe-lista",
            membro("Diretor-Executivo", "Pedro de Paula"),
            membro("Diretora Adjunta, Doenças Crônicas Não Transmissíveis", "Luciana Vasconcelos Sardinha"),
            membro("Diretora Adjunta de Comunicação Institucional", "Luiza Borges"),
            membro("Gerente de Recursos Financeiros", "Juliana Mendes"),
            membro("Gestão do Projeto", "Izabel Ferreira"),
            membro("Coordenadora de Comunicação", "Beatriz Bethlem"),
            membro("Analista de Comunicação", "Ana Furtado"),
            membro("Design e Diagramação", "Beatriz Ferreira"),
            membro("Coleta de Dados", "Erik Santos")
    )
  ),

  div(
    class = "equipe-instituicao unicef",
    tags$h2("Equipe de indicadores e Políticas Públicas"),
    div(class = "equipe-lista",

            membro("Coordenação Geral", "Júlia Alves Menezes"),
            membro("Coordenação de Pesquisa e Desenvolvimento de Indicadores", "Júlia Alves Menezes"),
            membro("Coordenação de pesquisa e desenvolvimento de políticas públicas", "Mariana Gutierres Arteiro da Paz"),
            membro("Processamento de Dados, Análise Estatística, Desenvolvimento do Dashboard", "Naurinete de Jesus da Costa Barreto"),
            membro("Processamento de Dados, Análise Estatística, Desenvolvimento do Dashboard", "George Ulguim Pedra"),
            membro("Coleta e Processamento de Dados", "Jocilene Dantas Barros"),
            membro("Consultoria técnica", "Cássia Maria Gama Lemos"),
            membro("Consultoria técnica", "Mariana Gutierres Arteiro da Paz"),
            membro("Consultoria técnica", "Júlia Alves Menezes")
    )
  )
))

    )
  )
}