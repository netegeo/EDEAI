# Código da aplicação

Esta pasta contém os componentes carregados por `app.R`. É código de uma aplicação Shiny; não constitui um pacote R instalável.

## Arquivos e responsabilidades

| Arquivo | Função |
| --- | --- |
| `carregar_dados.R` | Lê as bases RDS e cria `dados_app`, `acao_objetos` e `card_inicio` |
| `montar_objeto_municipio.R` | Consolida informações básicas, valores, classes e cores do município |
| `montar_objeto_acoes.R` | Prepara o objeto de ações, opções e funções de filtragem |
| `footer_ui.R` | Rodapé |
| `municipio_ui.R` | Seletores e área de diagnóstico |
| `municipio_card_ui.R` | Renderização municipal e exportação em PDF |
| `indicador_ui.R` | Consulta às fichas dos indicadores |
| `quem_somos_ui.R` | Instituições e equipe |
| `acoes_recomendadas.R` | Interface de consulta às ações |
| `acoes_cards_ui.R` | Apresentação dos cards de ações |
| `iu_basic-T.R` | Objeto `ui`, página inicial e apresentação do painel |
| `server_basic2.R` | Navegação, reatividade, seleção municipal e filtros |

Alguns comentários do código usam nomes antigos, como `global.R` e `server_basic.R`. Os caminhos efetivamente executados são os chamados em `app.R`.

## Ordem de carregamento

`app.R` lê as bases, carrega a montagem de objetos, os componentes visuais, a interface principal e, por último, o servidor. Executar os arquivos isoladamente pode falhar por falta de objetos globais ou funções auxiliares.

Iniciar pela raiz:

```r
shiny::runApp(".", launch.browser = FALSE)
```

## Contratos de dados

- `dados_app$inbase`: identificação e características municipais.
- `dados_app$porte_pop`: rótulos de porte populacional.
- `dados_app$info`: metadados, valores brutos, classes, cores e Selo UNICEF.
- `acao_objetos`: dados de entrada para a montagem das ações.
- `card_inicio`: conteúdo da página inicial.

`montar_objeto_municipio()` seleciona linhas por `sigla_uf` e `nome_mun`. Bases com grafias divergentes podem impedir a montagem do diagnóstico. Os códigos de indicadores também devem permanecer coerentes entre metadados e tabelas de valores.

O servidor utiliza `regra = "OR"` na filtragem de ações. Não presumir que a combinação de filtros representa uma interseção obrigatória.

## Manutenção

Preservar os nomes de funções e os identificadores de entrada e saída usados entre UI e servidor. Mudanças no layout devem considerar [www/custom2.css](../www/custom2.css), a folha ativa.

Este diretório apresenta dados classificados; não contém o pipeline integral de cálculo estatístico dos indicadores. Para interpretar a metodologia, consultar [www/files](../www/files/README.md).
