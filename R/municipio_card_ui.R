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
			campo_ui("UF", basico$sigla_uf) #,        
      # tags$img(
      #     src = "icons/SAMI_16052026.png",
      #     class = "municipio-sami"
      #   )
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
			campo_ui("Inscrito no Selo UNICEF (2025-2028)", basico$mun_selo) #,        
      # tags$img(
      #     src = "icons/ReguaDeLogos.png",
      #     class = "municipio-logos"
      #   )
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
tags$script(
  HTML(
    paste0("

function carregarImagemBase64(caminho) {

  return new Promise(function(resolve, reject) {

    const imagem = new Image();

    imagem.onload = function() {

      const canvas =
        document.createElement('canvas');

      canvas.width =
        imagem.naturalWidth;

      canvas.height =
        imagem.naturalHeight;

      const contexto =
        canvas.getContext('2d');

      contexto.drawImage(
        imagem,
        0,
        0
      );

      resolve({
        base64:
          canvas.toDataURL('image/png'),

        largura:
          imagem.naturalWidth,

        altura:
          imagem.naturalHeight
      });
    };

    imagem.onerror = function() {

      reject(
        new Error(
          'Não foi possível carregar a imagem: ' +
          caminho
        )
      );
    };

    imagem.src = caminho;
  });
}


function aguardarImagens(elemento) {

  const imagens =
    Array.from(
      elemento.querySelectorAll('img')
    );

  return Promise.all(
    imagens.map(function(imagem) {

      if (
        imagem.complete &&
        imagem.naturalWidth > 0
      ) {
        return Promise.resolve();
      }

      return new Promise(function(resolve) {

        imagem.onload = resolve;

        imagem.onerror = function() {

          console.warn(
            'Uma imagem interna não foi carregada:',
            imagem.src
          );

          resolve();
        };
      });
    })
  );
}


document
  .getElementById('baixar_diagnostico_pdf')
  .addEventListener(
    'click',
    async function() {

      const elemento =
        document.getElementById(
          'diagnostico_municipio_pdf'
        );

      if (!elemento) {

        alert(
          'Não foi possível localizar o conteúdo do diagnóstico.'
        );

        return;
      }

      try {

        /*
         * Aguarda o carregamento das fontes.
         */
        if (
          document.fonts &&
          document.fonts.ready
        ) {
          await document.fonts.ready;
        }

        /*
         * Aguarda as imagens existentes
         * dentro do conteúdo do diagnóstico.
         */
        await aguardarImagens(elemento);

        /*
         * Carrega as imagens adicionadas
         * diretamente ao rodapé do PDF.
         */
        const logoRodape =
          await carregarImagemBase64(
            'icons/SAMI_16052026.png'
          );

        const logoRegua =
          await carregarImagemBase64(
            'icons/ReguaDeLogos.png'
          );

        /*
         * Aguarda um ciclo de renderização
         * antes de medir o conteúdo.
         */
        await new Promise(function(resolve) {

          requestAnimationFrame(function() {

            requestAnimationFrame(resolve);

          });

        });

        /*
         * Math.ceil evita que valores
         * fracionários sejam arredondados
         * para baixo.
         */
        const largura =
          Math.ceil(
            elemento.scrollWidth
          );

        const alturaConteudo =
          Math.ceil(
            elemento.scrollHeight
          );

        /*
         * Espaços reservados para
         * cabeçalho e rodapé.
         */
        const margemSuperior = 0;

        const margemInferior = 90;

        /*
         * Pequena folga para diferenças
         * de renderização entre ambientes.
         */
const alturaPdf =
  alturaConteudo +
  margemSuperior +
  margemInferior +
  2;

        /*
         * Proporção da imagem SAMI.
         */
        const proporcaoLogo =
          logoRodape.largura /
          logoRodape.altura;

        let larguraLogo = 275;

        let alturaLogo =
          larguraLogo /
          proporcaoLogo;

        /*
         * Proporção da régua de logos.
         */
        const proporcaoRegua =
          logoRegua.largura /
          logoRegua.altura;

        let larguraRegua = 275;

        let alturaRegua =
          larguraRegua /
          proporcaoRegua;

        /*
         * Limita a altura das imagens para
         * que elas permaneçam dentro do rodapé.
         */
        const alturaMaximaImagem = 55;

        if (
          alturaLogo >
          alturaMaximaImagem
        ) {

          alturaLogo =
            alturaMaximaImagem;

          larguraLogo =
            alturaLogo *
            proporcaoLogo;
        }

        if (
          alturaRegua >
          alturaMaximaImagem
        ) {

          alturaRegua =
            alturaMaximaImagem;

          larguraRegua =
            alturaRegua *
            proporcaoRegua;
        }

        const opcoes = {

          margin: [
            margemSuperior,
            0,
            margemInferior,
            0
          ],

          filename:
            '", nome_arquivo_pdf, "',

          image: {
            type: 'jpeg',
            quality: 0.98
          },

          html2canvas: {
            scale: 2,
            useCORS: true,
            allowTaint: false,
            backgroundColor: '#F8F4ED',
            scrollX: 0,
            scrollY: 0,
            width: largura,
            windowWidth: largura,
            logging: false
          },

          jsPDF: {
            unit: 'px',
            format: [
              largura,
              alturaPdf
            ],
            orientation: 'portrait',
            compress: true
          },

          pagebreak: {
            mode: []
          }
        };

        html2pdf()
          .set(opcoes)
          .from(elemento)
          .toPdf()
          .get('pdf')
          .then(function(pdf) {

            const totalPaginas =
  pdf.internal.getNumberOfPages();

if (totalPaginas > 1) {

  for (
    let pagina = totalPaginas;
    pagina > 1;
    pagina--
  ) {
    pdf.deletePage(pagina);
  }
}

            const larguraPagina =
              pdf.internal.pageSize.getWidth();

            const alturaPagina =
              pdf.internal.pageSize.getHeight();

            /*
             * ===========================
             * RODAPÉ
             * ===========================
             */

            const alturaRodape = 90;

            const inicioRodape =
              alturaPagina -
              alturaRodape;

            pdf.setFillColor(
              242,
              243,
              245
            );

            pdf.rect(
              0,
              inicioRodape,
              larguraPagina,
              alturaRodape,
              'F'
            );

            /*
             * Logo SAMI à esquerda.
             */
            const posicaoLogoX = 25;

            const posicaoLogoY =
              inicioRodape +
              (
                alturaRodape -
                alturaLogo
              ) / 2;

            pdf.addImage(
              logoRodape.base64,
              'PNG',
              posicaoLogoX,
              posicaoLogoY,
              larguraLogo,
              alturaLogo
            );

            /*
             * Régua de logos à direita.
             */
            const posicaoReguaX =
              larguraPagina -
              larguraRegua -
              25;

            const posicaoReguaY =
              inicioRodape +
              (
                alturaRodape -
                alturaRegua
              ) / 2;

            pdf.addImage(
              logoRegua.base64,
              'PNG',
              posicaoReguaX,
              posicaoReguaY,
              larguraRegua,
              alturaRegua
            );

            /*
             * Texto central do rodapé.
             */
            pdf.setFont(
              'helvetica',
              'bold'
            );

            pdf.setFontSize(24);

            pdf.setTextColor(
              15,
              49,
              49
            );

            pdf.text(
              'www.criancasemeioambiente.org.br',
              larguraPagina / 2,
              inicioRodape +
                alturaRodape / 2,
              {
                align: 'center',
                baseline: 'middle'
              }
            );
          })
          .save()
          .catch(function(erro) {

            console.error(
              'Erro ao gerar o PDF:',
              erro
            );

            alert(
              'Ocorreu um erro durante a geração do PDF.'
            );
          });

      } catch (erro) {

        console.error(
          'Erro ao preparar o PDF:',
          erro
        );

        alert(
          'Não foi possível preparar as imagens ou o conteúdo do PDF.'
        );
      }
    }
  );
")
  )
)


)
}
