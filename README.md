# EDEAI — Painel de exposição ambiental infantojuvenil

Aplicação web em **R/Shiny** para consultar indicadores municipais de exposição ambiental, suas classificações e ações públicas recomendadas, com foco nas condições de vida de crianças e adolescentes.

Este repositório reúne o código do painel, bases preparadas para sua execução e materiais técnicos. A interface apresenta o projeto como **Painel Crianças e Meio Ambiente** e utiliza a identidade visual SAMI. EDEAI é o nome deste repositório; documentos de desenvolvimento também utilizam a denominação Novo ISAI.

## Objetivo e funcionalidades

Apoiar a leitura territorial e o planejamento municipal por meio de:

- seleção de município e unidade federativa;
- apresentação de valores dos indicadores e níveis de prioridade;
- consulta às fichas metodológicas;
- consulta e filtragem de ações recomendadas;
- geração do diagnóstico municipal em PDF no navegador;
- acesso à nota técnica, ao relatório metodológico e à planilha de metadados.

A interface informa 14 indicadores em quatro áreas: Ambiente Escolar, Eventos Climáticos Extremos, Infraestrutura Ambiental e Urbana e Qualidade do Ar. A disponibilidade e o período de cada indicador devem ser consultados nos metadados; os indicadores não devem ser tratados como uma base com um único ano de referência.

## Escopo e interpretação

O painel apresenta **exposições ambientais e condições do território**. Sua leitura não equivale à mensuração direta de desfechos de saúde nem a uma estimativa individual de risco.

A documentação de desenvolvimento descreve classificação relativa por porte populacional, em tercis e sete portes. Os documentos preliminares registram também experimentos com outras configurações. Para interpretar a versão distribuída, consultar os [materiais técnicos](www/files/README.md) e os metadados associados aos dados.

As classes são posições relativas no grupo de referência: prioridade elevada não significa, por si só, ultrapassagem de limite normativo. Indicadores com referências técnicas próprias exigem leitura complementar.

**Limite de reprodutibilidade:** o código carrega valores e classificações previamente preparados em arquivos RDS. As rotinas completas de aquisição dos dados de origem, cálculo dos indicadores, tratamento estatístico e classificação não estão nesta árvore do repositório. Executar o painel reproduz a consulta às bases distribuídas, mas não refaz toda a cadeia científica a partir das fontes originais.

## Estrutura

| Caminho | Conteúdo |
| --- | --- |
| [app.R](app.R) | Inicialização, pacotes e carregamento dos componentes |
| [R/](R/README.md) | Leitura de dados, montagem de objetos, interface e servidor |
| [data/](data/README.md) | Bases serializadas utilizadas pela aplicação |
| [www/](www/README.md) | CSS, fontes, imagens e biblioteca de exportação |
| [www/files/](www/files/README.md) | Documentos metodológicos e metadados |
| [manifest.json](manifest.json) | Ambiente e dependências registrados para implantação |
| [docs/](docs/README.md) | Preparação da versão para depósito no Zenodo |

## Executar localmente

O `manifest.json` registra **R 4.5.2**. Essa é a referência do ambiente de implantação, não uma versão mínima testada. Recomenda-se começar com a mesma versão. RStudio é opcional.

Clonar o repositório:

```bash
git clone https://github.com/netegeo/EDEAI.git
cd EDEAI
```

No console do R, com o diretório de trabalho na raiz do projeto:

```r
install.packages(c(
  "shiny", "bslib", "htmltools", "shinyjs", "shinymanager",
  "dplyr", "readr", "DT", "later"
))
shiny::runApp(".", launch.browser = FALSE)
```

Abrir no navegador o endereço informado pelo Shiny. Usar `launch.browser = FALSE` evita depender da configuração de Firefox presente em `app.R`.

O comando de instalação obtém as versões disponíveis no momento. Para comparar com o ambiente registrado, consultar as versões em `manifest.json`. Não há `renv.lock` nesta versão; o manifesto não deve ser apresentado como um ambiente já restaurado e validado.

### Verificação prática

Após iniciar a aplicação, verificar:

1. seleção de UF e município e carregamento do diagnóstico;
2. apresentação dos indicadores e abertura das fichas;
3. filtros das ações recomendadas e retorno das páginas de detalhe;
4. exportação municipal em PDF;
5. abertura dos PDFs técnicos e da planilha de metadados.

A documentação foi conferida contra os caminhos e o código disponível. Não representa um teste de execução da aplicação.

## Atualização e implantação

As bases são carregadas em `R/carregar_dados.R`. Ao mudar um nome de arquivo, atualizar a referência correspondente e manter a compatibilidade entre códigos municipais, indicadores, classes e cores.

Para implantação com rsconnect, configurar a conta no ambiente de publicação e, quando necessário, gerar novamente o manifesto:

```r
install.packages("rsconnect")
rsconnect::writeManifest(appDir = ".")
```

Credenciais de implantação não devem ser incluídas no código ou no arquivo de documentação. O serviço hospedado e esta cópia do repositório podem evoluir de forma independente.

## Créditos

Os créditos abaixo seguem a ficha técnica incluída em `R/quem_somos_ui.R`:

| Participação | Pessoas ou instituições |
| --- | --- |
| Parceria institucional | UNICEF e Vital Strategies |
| Coordenação geral | Júlia Alves Menezes |
| Coordenação de pesquisa e desenvolvimento de políticas públicas | Mariana Gutierres Arteiro da Paz |
| Processamento de dados, análise estatística e desenvolvimento do dashboard | Naurinete de J. da C. Barreto e George Ulguim Pedra |
| Coleta e processamento de dados | Jocilene Dantas Barros |
| Consultoria técnica | Cássia Maria Gama Lemos |


A ficha técnica da aplicação contém os demais créditos institucionais. A manutenção desta cópia não implica autoria exclusiva do projeto, dos dados, dos documentos ou da identidade visual.

## Citação e Zenodo

Ainda não há DOI ou versão de depósito declarados nesta documentação. Para citar esta cópia antes do depósito, informar o nome do painel, o endereço do repositório e o commit consultado, preservando os créditos pertinentes.

As orientações de preparação e publicação estão em [docs/README.md](docs/README.md). Após o depósito, utilizar a referência gerada pelo Zenodo para a versão arquivada.

## Licença e materiais de terceiros

Não foi identificado arquivo `LICENSE` na árvore examinada. Esta documentação não atribui uma licença nova ao projeto. Antes da publicação da versão, definir os termos aplicáveis ao código e verificar separadamente as permissões de redistribuição dos dados, documentos, fontes e marcas.

## Contribuições

Relatar problemas pelas [Issues](https://github.com/netegeo/EDEAI/issues), indicando o commit ou a versão, a página afetada e como reproduzir o problema. Alterações podem ser propostas por pull request, preservando os créditos e a documentação dos dados.
