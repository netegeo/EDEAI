# Dados utilizados pelo painel

Esta pasta reúne objetos preparados para a aplicação, armazenados principalmente em formato RDS. As descrições seguem as operações de leitura do código; não substituem a inspeção interna dos arquivos nem a planilha de metadados.

## Inventário

| Arquivo | Uso no código |
| --- | --- |
| `inbase.rds` | Base municipal lida para `dados_app$inbase` |
| `porte_populacao.rds` | Tabela de correspondência dos portes e seus rótulos |
| `informacoes_20260715.rds` | Objeto com `metadados`, `brutos`, `classe`, `tabela_cores` e `selo_unicef` |
| `objeto_acao_20260722.rds` | Conteúdo de ações recomendado, lido como `acao_objetos` |
| `cards_inicio_20260524.rds` | Título e conteúdo dos cards da página inicial |
| `controle_maiusculas_base_tecnica.csv` | Arquivo auxiliar de controle textual; não é lido em `carregar_dados.R` |

Os sufixos de data dos nomes identificam arquivos versionados; não demonstram, por si só, o período de observação dos indicadores.

## Inspeção no R

A partir da raiz do projeto:

```r
base <- readRDS("data/inbase.rds")
info <- readRDS("data/informacoes_20260715.rds")

str(base)
names(info)
str(info, max.level = 1)
names(info$metadados)
```

A base municipal é utilizada com campos como `geocodigo`, `nome_mun`, `sigla_uf`, `nm_regiao`, `porte`, `populacao`, `perc_pop_infantoj` e `ppopij`. Os rótulos de porte utilizam `porte` e `limites`. Confirmar os tipos e os demais campos diretamente nos objetos.

## Compatibilidade e atualização

A rotina de carregamento remove linhas com UF ou nome municipal ausente ou vazio em algumas bases. Essa operação não deve ser interpretada como validação completa da qualidade dos dados.

Para atualizar uma base:

1. preservar identificadores e documentar alterações de cobertura;
2. manter códigos e títulos compatíveis entre indicadores, classes e metadados;
3. registrar fontes, unidades, períodos de referência, valores ausentes e critérios de classificação;
4. atualizar os caminhos em `R/carregar_dados.R` quando o arquivo mudar de nome;
5. conferir os diagnósticos afetados antes da publicação.

Não converter ausência de informação em zero: zero e dado ausente possuem significados diferentes.

## Fontes, períodos e reuso

Consultar [Metadados-PainelCriancasMeioAmbiente.xlsx](../www/files/Metadados-PainelCriancasMeioAmbiente.xlsx) e os [documentos técnicos](../www/files/README.md) para as fontes e definições de cada indicador.

Os arquivos disponibilizados são insumos processados do painel. As séries originais e as rotinas completas de produção não estão reunidas neste diretório. A documentação não declara uma licença única para essas bases; verificar os termos das fontes e as permissões de redistribuição antes de incluí-las no depósito.
