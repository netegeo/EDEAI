# =========================================================
# Arquivo: municipio_card_ui.R
# Finalidade:
#   Renderizar a aba Município a partir do objeto consolidado
#   retornado por montar_objeto_municipio().
#
# Entrada esperada:
#   - obj$basico
#   - obj$temas_ui
#
# Estrutura visual:
#   - Botão de download do diagnóstico em PDF
#   - Bloco 1: texto introdutório
#   - Bloco 2: informações básicas do município
#   - Bloco 3: legenda dos níveis de prioridade
#   - Bloco 4: blocos temáticos com indicadores em formato tabular
#
# Campos esperados em cada indicador:
#   - indicador
#   - valor_bruto
#   - unidade
#   - valor_classificado
#   - descricao_curta
#   - cor_bg
#   - cor_tex
# =========================================================

municipio_card_ui <- function(obj) {

  # -------------------------------------------------------
  # 1. Validação mínima
  # -------------------------------------------------------
  if (is.null(obj)) {
    return(NULL)
  }

  if (is.null(obj$basico) || is.null(obj$temas_ui)) {
    return(NULL)
  }

  basico <- obj$basico
  temas_ui <- obj$temas_ui

  # -------------------------------------------------------
  # 2.1. Alerta para população infantojuvenil expressiva
  # -------------------------------------------------------

  tem_alerta_popij <- !is.null(basico$pop_top) &&
     !is.na(basico$pop_top) &&
     basico$pop_top %in% c("P25", "P10")

  classe_alerta <- if (tem_alerta_popij && basico$pop_top == "P25") {
     "alerta-popij-p25"
  } else if (tem_alerta_popij && basico$pop_top %in% c("P10")) {
     "alerta-popij-p10"
  } else {
     NULL
  }

  nivel_alerta <- if (tem_alerta_popij && basico$pop_top == "P25") {
     "Top 25%"
  } else if (tem_alerta_popij && basico$pop_top %in% c("P10")) {
     "Top 10%"
  } else {
     NULL
  }

  mensagem_alerta <- if (tem_alerta_popij && basico$pop_top == "P25") {
    "Sua cidade está entre os 25% de municípios com mais crianças e adolescentes na população, proporcionalmente."
  } else if (tem_alerta_popij && basico$pop_top %in% c("P10")) {
    "Sua cidade está entre os 10% de municípios com mais crianças e adolescentes na população, proporcionalmente. "
  } else {
    NULL
  }

  frase_prioridade <- if (tem_alerta_popij && basico$pop_top == "P25") {
    "Isso significa que ações voltadas para proteger meninos e meninas são ainda mais urgentes e necessárias."
  } else if (tem_alerta_popij && basico$pop_top %in% c("P10")) {
    "Isso significa que ações voltadas para proteger meninos e meninas são ainda mais urgentes e necessárias."
  } else {
    NULL
  }


  # -------------------------------------------------------
  # 2. Funções auxiliares simples de exibição
  # -------------------------------------------------------

  valor_txt <- function(x, vazio = "Sem informação") {
    if (length(x) == 0 || is.null(x) || all(is.na(x))) {
      return(vazio)
    }

    x <- as.character(x[1])

    if (is.na(x) || trimws(x) == "") {
      return(vazio)
    }

    x
  }
  
	formatar_populacao <- function(x, vazio = "Sem informação") {
		if (length(x) == 0 || is.null(x) || all(is.na(x))) {
			return(vazio)
		}

		x <- suppressWarnings(as.numeric(x[1]))

		if (is.na(x)) {
			return(vazio)
		}

		if (x < 1000) {
			return(paste0(
			format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE),
			" habitantes"
			))
		}

		if (x < 1000000) {
			return(paste0(
			format(round(x / 1000, 1), decimal.mark = ",", nsmall = 1),
			" mil habitantes"
			))
		}

		paste0(
			format(round(x / 1000000, 1), decimal.mark = ",", nsmall = 1),
			" milhões de habitantes"
		)
	}
	
	formatar_porte_populacional <- function(x, vazio = "Sem informação") {
		if (length(x) == 0 || is.null(x) || all(is.na(x))) {
			return(vazio)
		}

		pop <- suppressWarnings(as.numeric(x[1]))

		if (is.na(pop)) {
			return(vazio)
		}

		if (pop < 5000) {
			return("menor que 5 mil habitantes")
		}

		if (pop < 10000) {
			return("entre 5 mil e 10 mil habitantes")
		}

		if (pop < 20000) {
			return("entre 10 mil e 20 mil habitantes")
		}

		if (pop < 50000) {
			return("entre 20 mil e 50 mil habitantes")
		}

		if (pop < 100000) {
			return("entre 50 mil e 100 mil habitantes")
		}

		if (pop < 500000) {
			return("entre 100 mil e 500 mil habitantes")
		}

		"maior que 500 mil habitantes"
	}
	
  campo_ui <- function(rotulo, valor, extra_class = NULL) {
    tags$div(
      class = paste("municipio-campo", extra_class),
      tags$div(class = "municipio-campo-rotulo", rotulo),
      tags$div(class = "municipio-campo-valor", valor_txt(valor))
    )
  }

  # -------------------------------------------------------
  # 3. Nome do arquivo PDF
  # -------------------------------------------------------

  nome_arquivo_pdf <- paste0(
    gsub("[^A-Za-z0-9_-]", "_", basico$nome_mun),
    "_",
    gsub("[^A-Za-z0-9_-]", "_", basico$sigla_uf),
    "_",
    gsub("[^A-Za-z0-9_-]", "_", basico$geocodigo),
    ".pdf"
  )

  # -------------------------------------------------------
  # 4. Bloco principal
  # -------------------------------------------------------

  tags$div(
    class = "municipio-export-wrapper",

    # =====================================================
    # BOTÃO DE DOWNLOAD
    # =====================================================
    # Este botão fica fora da área exportada para o PDF.

    tags$div(
      class = "municipio-download-area no-export",
      tags$button(
        id = "baixar_diagnostico_pdf",
        type = "button",
        class = "btn-baixar-diagnostico",
        "Baixe o diagnóstico completo do seu município"
      )
    ),

    # =====================================================
    # ÁREA QUE SERÁ EXPORTADA PARA PDF
    # =====================================================
    # Tudo que estiver dentro desta div será capturado
    # pelo html2pdf.js.

    tags$div(
      id = "diagnostico_municipio_pdf",
      class = "municipio-card-novo2",

      # =====================================================
      # TEXTO INTRODUTÓRIO DA PÁGINA MUNICIPAL
      # =====================================================

      #tags$div(
      #  class = "municipio-texto-intro",
      #  "Nesta página, você encontra o diagnóstico do seu município de acordo com os 14 indicadores de exposição ambiental que compõem o painel. A ordem e a intensidade da cor dos indicadores apontam o nível de prioridade com que o tema deve ser tratado pelo município, considerando o seu desempenho comparado com outras cidades de mesmo porte."
      #),

      # =====================================================
      # BLOCO 1 - INFORMAÇÕES BÁSICAS
      # =====================================================

      tags$div(
		class = "municipio-cabecalho",

		tags$div(
			class = "municipio-basico-grid linha-1",
			campo_ui("Município", basico$nome_mun, "span-2"),
			campo_ui("UF", basico$sigla_uf)
		),

		tags$div(
			class = "municipio-basico-grid linha-2",
			campo_ui("População", formatar_populacao(basico$populacao)),
			campo_ui("Porte Populacional", formatar_porte_populacional(basico$populacao)),
			campo_ui("Região", basico$nm_regiao)
		),

		tags$div(
			class = "municipio-basico-grid linha-3",
			campo_ui("População Infantojuvenil (%)", basico$perc_pop_infantoj),
			campo_ui("Inscrito no Selo UNICEF (2025-2028)", basico$mun_selo)
		),

		if (tem_alerta_popij) {
			tags$div(
			class = paste("alerta-popij", classe_alerta),

			tags$div(
				class = "alerta-popij-icone",
				tags$img(src = "icons/alerta.svg", height = "50px")
			),

		tags$div(
			class = "alerta-popij-conteudo",
			tags$div(class = "alerta-popij-nivel", nivel_alerta),
			tags$p(class = "alerta-popij-msg", mensagem_alerta),
			tags$strong(class = "alerta-popij-prioridade", frase_prioridade)
			)
		)
		}
		),
      # =====================================================
      # BLOCO 2 - LEGENDA GERAL SOBRE OS NÍVEIS
      # =====================================================

      tags$div(
        class = "municipio-legenda-geral",

        tags$div(
          class = "municipio-legenda-titulo",
          "Legenda dos níveis de prioridade"
        ),

        tags$ul(
          class = "municipio-legenda-lista",

          tags$li(
            tags$strong("Nível 3 - Nível de atenção alto (cor escura): "),
            "representam indicadores de alta prioridade, que demandam ação imediata e priorizada."
          ),

          tags$li(
            tags$strong("Nível 2 - Nível de atenção médio (cor intermediária): "),
            "representam indicadores de média prioridade, para os quais recomenda-se planejamento e adoção progressiva de medidas de melhoria no curto a médio prazo."
          ),

          tags$li(
            tags$strong("Nível 1 - Nível de atenção baixo (cor clara): "),
            "representam indicadores de baixa prioridade, para os quais recomenda-se monitoramento contínuo e manutenção das condições atuais, sem necessidade de intervenção imediata ou emergencial."
          )
        )
      ),

      # =====================================================
      # BLOCO 3 - TEMAS E INDICADORES
      # =====================================================

      lapply(temas_ui, function(tu) {

        if (is.null(tu) || is.null(tu$indicadores) || NROW(tu$indicadores) == 0) {
          return(NULL)
        }

        df_tema <- tu$indicadores

        # Fundo principal do tema
        estilo_tema <- if (!is.null(tu$cor_bg_tema) &&
                   !is.na(tu$cor_bg_tema) &&
                   trimws(tu$cor_bg_tema) != "") {
		paste0(
			"background:#FFFFFF;",
			"border:2px solid ", tu$cor_bg_tema, ";"
		)
		} else {
			"background:#FFFFFF;border:1px solid rgba(24,49,83,0.12);"
		}

        tags$div(
          class = "tema-bloco",
          style = estilo_tema,

          # -----------------------------------------------
          # Título do tema
          # -----------------------------------------------
          tags$div(
            class = "tema-titulo",
            tu$tema
          ),

          # -----------------------------------------------
          # Descrição do tema
          # -----------------------------------------------
          tags$div(
            class = "tema-descricao-inline",
            valor_txt(tu$desc_tema)
          ),

          # -----------------------------------------------
          # Tabela visual de indicadores
          # -----------------------------------------------
          tags$div(
            class = "indicadores-tabela",

            # Cabeçalho do tema
            tags$div(
              class = "indicadores-cabecalho",

              tags$div(
                class = "indicadores-col indicador-col-nome",
                "Indicador"
              ),

              tags$div(
                class = "indicadores-col indicador-col-valor",
                "Valor"
              ),

              tags$div(
                class = "indicadores-col indicador-col-unidade",
                "Unidade"
              ),

              tags$div(
                class = "indicadores-col indicador-col-nivel",
                "Descrição Curta"
              ),

              tags$div(
                class = "indicadores-col indicador-col-descricao",
                "Nível de \natenção"
              )
            ),

            # Linhas dos indicadores
            lapply(seq_len(NROW(df_tema)), function(i) {

              linha <- df_tema[i, ]

              estilo_indicador <- paste0(
					"background:#FFFFFF;",
					"color:#000000;",
					if (!is.na(linha$cor_bg) && trimws(linha$cor_bg) != "") {
					paste0("border:2px solid ", linha$cor_bg, ";")
					} else {
						"border:1px solid rgba(24,49,83,0.12);"
					}
				)
              
              estilo_nivel <- paste0(
				if (!is.na(linha$cor_bg) && trimws(linha$cor_bg) != "") {
					paste0("background:", linha$cor_bg, ";")
				} else {
					"background:#E5E7EB;"
				},
				if (!is.na(linha$valor_classificado) && linha$valor_classificado == 3) {
					"color:#FFFFFF;"
				} else if (!is.na(linha$cor_tex) && trimws(linha$cor_tex) != "") {
					paste0("color:", linha$cor_tex, ";")
				} else {
					"color:#183153;"
				}
			 )

              tags$div(
                class = "indicadores-linha",
                style = estilo_indicador,

                tags$div(
                  class = "indicadores-col indicador-col-nome",

                  tags$a(
                   href = "#",
                   class = "indicador-municipio-link",
                    onclick = sprintf(
                    "Shiny.setInputValue('indicador_selecionado', '%s', {priority: 'event'}); return false;",
                      linha$codigo
                     ),
                     valor_txt(linha$indicador)
                   )
                 ),

                tags$div(
                  class = "indicadores-col indicador-col-valor",
                  valor_txt(linha$valor_bruto)
                ),

                tags$div(
                  class = "indicadores-col indicador-col-unidade",
                  valor_txt(linha$unidade)
                ),

                tags$div(
                  class = "indicadores-col indicador-col-descricao",
                  valor_txt(linha$descricao_curta)
                ),
                
                tags$div(
					class = "indicadores-col indicador-col-nivel",
						tags$span(
							class = "indicador-nivel-tag",
							style = estilo_nivel,
						valor_txt(linha$valor_classificado)
						)
				)
              )
            })
          )
        )
      })
    ),

    # =====================================================
    # SCRIPT PARA GERAR PDF SEM QUEBRA DE PÁGINA
    # =====================================================

    tags$script(HTML(paste0("
  document.getElementById('baixar_diagnostico_pdf').addEventListener('click', function() {
    const elemento = document.getElementById('diagnostico_municipio_pdf');

    if (!elemento) {
      alert('Não foi possível localizar o conteúdo do diagnóstico.');
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

# tags$script(
#   HTML(
#     paste0("
# document
#   .getElementById('baixar_diagnostico_pdf')
#   .addEventListener('click', async function() {

#     const elemento = document.getElementById(
#       'diagnostico_municipio_pdf'
#     );

#     if (!elemento) {
#       alert(
#         'Não foi possível localizar o conteúdo do diagnóstico.'
#       );
#       return;
#     }

#     /*
#      * Carrega uma imagem e a converte
#      * para PNG em Base64.
#      */
#     function carregarImagemBase64(caminho) {

#       return new Promise(function(resolve, reject) {

#         const imagem = new Image();

#         imagem.onload = function() {

#           const larguraOriginal =
#             imagem.naturalWidth || imagem.width;

#           const alturaOriginal =
#             imagem.naturalHeight || imagem.height;

#           const canvas =
#             document.createElement('canvas');

#           canvas.width = larguraOriginal;
#           canvas.height = alturaOriginal;

#           const contexto =
#             canvas.getContext('2d');

#           contexto.drawImage(
#             imagem,
#             0,
#             0,
#             larguraOriginal,
#             alturaOriginal
#           );

#           resolve({
#             base64: canvas.toDataURL('image/png'),
#             largura: larguraOriginal,
#             altura: alturaOriginal
#           });
#         };

#         imagem.onerror = function() {
#           reject(
#             new Error(
#               'Não foi possível carregar a imagem: ' +
#               caminho
#             )
#           );
#         };

#         imagem.src = caminho;
#       });
#     }

#     const botao =
#       document.getElementById(
#         'baixar_diagnostico_pdf'
#       );

#     try {

#       if (botao) {
#         botao.disabled = true;
#       }

#       /*
#        * Imagem do cabeçalho.
#        * Arquivo localizado em:
#        * www/icons/SAMI_16052026.png
#        */
#       const cabecalho =
#         await carregarImagemBase64(
#           'icons/SAMI_16052026.png'
#         );

#       /*
#        * Imagem do rodapé.
#        * Arquivo localizado em:
#        * www/icons/ReguaDeLogos.png
#        */
#       const logoRodape =
#         await carregarImagemBase64(
#           'icons/ReguaDeLogos.png'
#         );

#       const opcoes = {

#         /*
#          * Margens em milímetros:
#          *
#          * superior: 48 mm
#          * esquerda: 12 mm
#          * inferior: 30 mm
#          * direita: 12 mm
#          *
#          * A margem superior reserva espaço
#          * para o cabeçalho de 31 mm.
#          */
#         margin: [30, 1, 30, 30],

#         filename: '", nome_arquivo_pdf, "',

#         image: {
#           type: 'jpeg',
#           quality: 0.75
#         },

#         html2canvas: {
#           scale: 1.2,
#           useCORS: true,
#           allowTaint: false,
#           backgroundColor: '#F8F4ED',
#           scrollX: 0,
#           scrollY: 0,
#           windowWidth: elemento.scrollWidth
#         },

#         jsPDF: {
#           unit: 'mm',
#           format: 'a4',
#           orientation: 'portrait'
#         },

#         pagebreak: {
#           mode: ['css', 'legacy'],
#           avoid: [
#             '.card',
#             '.indicador',
#             '.grafico',
#             '.tabela'
#           ]
#         }
#       };

#       await html2pdf()
#         .set(opcoes)
#         .from(elemento)
#         .toPdf()
#         .get('pdf')
#         .then(function(pdf) {

#           const totalPaginas =
#             pdf.internal.getNumberOfPages();

#           const larguraPagina =
#             pdf.internal.pageSize.getWidth();

#           const alturaPagina =
#             pdf.internal.pageSize.getHeight();

#           const margemLateral = 5;

#           /*
#            * ---------------------------------
#            * CONFIGURAÇÃO DO CABEÇALHO
#            * ---------------------------------
#            */

#           const larguraCabecalho = 90;
#           const alturaCabecalho = 25;

#           const posicaoCabecalhoX =
#             (larguraPagina - larguraCabecalho) / 2;

#           const posicaoCabecalhoY = 4;

#           /*
#            * Linha abaixo do cabeçalho.
#            */
#           const linhaCabecalhoY =
#             posicaoCabecalhoY +
#             alturaCabecalho +
#             4;

#           /*
#            * ---------------------------------
#            * CONFIGURAÇÃO DO RODAPÉ
#            * ---------------------------------
#            */

#           const larguraMaximaRodape = 150;
#           const alturaMaximaRodape = 15;

#           const proporcaoRodape =
#             logoRodape.largura /
#             logoRodape.altura;

#           let larguraRodape =
#             larguraMaximaRodape;

#           let alturaRodape =
#             larguraRodape /
#             proporcaoRodape;

#           /*
#            * Impede que o rodapé ultrapasse
#            * a altura máxima, preservando
#            * sua proporção.
#            */
#           if (
#             alturaRodape >
#             alturaMaximaRodape
#           ) {

#             alturaRodape =
#               alturaMaximaRodape;

#             larguraRodape =
#               alturaRodape *
#               proporcaoRodape;
#           }

#           const posicaoRodapeX =
#             (larguraPagina - larguraRodape) / 2;

#           const posicaoRodapeY =
#             alturaPagina -
#             alturaRodape -
#             7;

#           const linhaRodapeY =
#             posicaoRodapeY - 3;

#           /*
#            * ---------------------------------
#            * INSERÇÃO EM TODAS AS PÁGINAS
#            * ---------------------------------
#            */

#           for (
#             let pagina = 1;
#             pagina <= totalPaginas;
#             pagina++
#           ) {

#             pdf.setPage(pagina);

#             /*
#              * Cabeçalho com tamanho exato:
#              * 129 mm × 31 mm.
#              */
#             pdf.addImage(
#               cabecalho.base64,
#               'PNG',
#               posicaoCabecalhoX,
#               posicaoCabecalhoY,
#               larguraCabecalho,
#               alturaCabecalho
#             );

#             /*
#              * Linha abaixo do cabeçalho.
#              */
#             pdf.setDrawColor(
#               180,
#               180,
#               180
#             );

#             pdf.setLineWidth(0.25);

#             pdf.line(
#               margemLateral,
#               linhaCabecalhoY,
#               larguraPagina -
#                 margemLateral,
#               linhaCabecalhoY
#             );

#             /*
#              * Linha acima do rodapé.
#              */
#             pdf.line(
#               margemLateral,
#               linhaRodapeY,
#               larguraPagina -
#                 margemLateral,
#               linhaRodapeY
#             );

#             /*
#              * Régua de logos no rodapé.
#              */
#             pdf.addImage(
#               logoRodape.base64,
#               'PNG',
#               posicaoRodapeX,
#               posicaoRodapeY,
#               larguraRodape,
#               alturaRodape
#             );

#             /*
#              * Numeração das páginas.
#              */
#             pdf.setFontSize(7);

#             pdf.setTextColor(
#               80,
#               80,
#               80
#             );

#             pdf.text(
#               'Página ' +
#                 pagina +
#                 ' de ' +
#                 totalPaginas,
#               larguraPagina -
#                 margemLateral,
#               alturaPagina - 4,
#               {
#                 align: 'right'
#               }
#             );
#           }
#         })
#         .save();

#     } catch (erro) {

#       console.error(
#         'Erro ao gerar o PDF:',
#         erro
#       );

#       alert(
#         'Não foi possível gerar o PDF.'
#       );

#     } finally {

#       if (botao) {
#         botao.disabled = false;
#       }
#     }
#   });
# ")
#   )
# )
  )
}
