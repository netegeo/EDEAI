# Preparação do EDEAI para o Zenodo

O depósito deve identificar a **versão do software arquivada**, seu conteúdo e seus autores. A autoria do código, dos indicadores, dos documentos e a participação institucional precisam ser descritas conforme o material efetivamente depositado.

## Descrição sugerida

> EDEAI — código do Painel Crianças e Meio Ambiente. Aplicação desenvolvida em R/Shiny para consulta de indicadores municipais de exposição ambiental infantojuvenil, apresentação de classificações relativas e acesso a ações públicas recomendadas. O repositório inclui os componentes da interface e do servidor, bases preparadas em formato RDS, recursos visuais e documentos técnicos. O painel foi desenvolvido no contexto da parceria entre UNICEF e Vital Strategies. A versão arquivada permite executar a consulta às bases distribuídas; não inclui todas as rotinas de aquisição, cálculo e classificação dos indicadores a partir das fontes originais.

## Metadados a preparar

| Campo | Orientação |
| --- | --- |
| Tipo de recurso | Software |
| Título | EDEAI — código do Painel Crianças e Meio Ambiente |
| Versão | Usar a versão efetivamente adotada na release |
| Autores | Confirmar a lista e a ordem de autoria do software depositado |
| Contribuidores | Registrar as demais contribuições pertinentes ao conteúdo |
| Afiliações e ORCID | Informar apenas identificadores e vínculos confirmados |
| Descrição | Usar o texto acima, ajustando ao conteúdo final |
| Palavras-chave | exposição ambiental; infância; adolescência; indicadores municipais; R; Shiny |
| Repositório relacionado | https://github.com/netegeo/EDEAI |
| Licença | Definir conforme os direitos aplicáveis ao código e demais materiais |

A ficha técnica registra Naurinete de J. da C. Barreto e George Ulguim Pedra no processamento de dados, análise estatística e desenvolvimento do dashboard. Preservar esse crédito e os demais papéis da equipe, sem presumir que a lista de autores do depósito esteja automaticamente definida.

## Pendências antes da release

- [ ] Definir a versão e o commit a arquivar.
- [ ] Confirmar os autores, a ordem, as afiliações e os contribuidores.
- [ ] Definir a licença do código e registrá-la em `LICENSE`.
- [ ] Conferir os termos de redistribuição de bases, PDFs, planilha, fontes, logos e JavaScript.
- [ ] Executar o painel no ambiente de referência e verificar as funcionalidades descritas no README.
- [ ] Registrar o ambiente efetivamente testado; o manifesto atual informa R 4.5.2.
- [ ] Declarar nas notas da release o limite de reprodutibilidade das rotinas científicas.
- [ ] Conferir os arquivos da versão para evitar a inclusão de credenciais ou materiais privados.

Não foram criados DOI, número de versão ou licença fictícios nesta documentação.

## Metadados no repositório

Após confirmar a autoria e a licença, preparar `CITATION.cff` ou `.zenodo.json`. O Zenodo aceita ambos; quando os dois estão presentes, prioriza `.zenodo.json` e ignora os metadados de `CITATION.cff` na integração.

Referência: [Describe software — Zenodo](https://help.zenodo.org/docs/github/describe-software/).

## Publicação pela integração GitHub–Zenodo

1. Revisar e incorporar a documentação na branch que será lançada.
2. No Zenodo, conectar a conta GitHub correspondente e habilitar `netegeo/EDEAI`.
3. Criar uma release no GitHub com tag, título, versão e notas do conteúdo arquivado.
4. Aguardar o processamento e verificar o registro e o DOI no Zenodo.
5. Conferir autores, licença, descrição, arquivos e vínculos do registro.
6. Acrescentar ao README e ao arquivo de citação o DOI real do depósito.

Criar somente um commit ou uma branch não equivale a publicar uma release. Este trabalho de documentação não publica um depósito.

Referências oficiais: [Enable a repository](https://help.zenodo.org/docs/github/enable-repository/) e [Archive a release from GitHub](https://help.zenodo.org/docs/github/archive-software/github-upload/).

## Notas de versão sugeridas

> Esta versão reúne o código da aplicação Shiny, as bases preparadas utilizadas pelo painel e os materiais técnicos distribuídos no repositório. A documentação descreve estrutura, execução, contratos de dados e créditos. O depósito preserva esta cópia e este commit; não representa necessariamente a versão em produção de outro serviço. As rotinas completas de produção estatística dos indicadores não estão incluídas.

## Citação após o depósito

Utilizar a referência fornecida pelo Zenodo e conferir os nomes dos autores. Para reproduzir resultados, informar também a versão e o DOI correspondente ao conteúdo utilizado. Não atribuir ao depósito uma autoria exclusiva do projeto institucional nem uma atualização contínua de versões mantidas em outros repositórios.
