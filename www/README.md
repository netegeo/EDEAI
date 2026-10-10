# Recursos estáticos

O Shiny serve os arquivos desta pasta ao navegador. Por exemplo, `www/custom2.css` é referenciado na interface como `custom2.css`, sem o prefixo `www/`.

| Caminho | Uso |
| --- | --- |
| `custom2.css` | Folha de estilo ativa em `R/iu_basic-T.R` |
| `custom.css` | Folha alternativa; sua inclusão está comentada na interface principal |
| `fonts/` | Fontes Rawline distribuídas com a aplicação |
| `icons/` | Logotipos, ícones e elementos de identidade visual |
| `js/html2pdf.bundle.min.js` | Biblioteca utilizada para a exportação do diagnóstico no navegador |
| [files/](files/README.md) | PDFs técnicos e planilha de metadados |

## Alterações visuais

Conferir os seletores CSS usados pelos componentes em `R/`. Manter caminhos e capitalização dos arquivos: sistemas Linux diferenciam letras maiúsculas e minúsculas.

Após alterar o CSS ou as imagens, conferir a navegação, os cards municipais e o PDF exportado, inclusive com textos longos. O resultado da exportação depende da renderização e do carregamento de imagens no navegador.

## Direitos e dependências

Logos e identidade visual devem manter os créditos da ficha técnica. A presença de fontes, marcas e biblioteca JavaScript neste repositório não significa que todas compartilhem os mesmos termos de uso. Conferir as licenças dos recursos de terceiros antes do depósito no Zenodo; o repositório não contém uma declaração consolidada dessas permissões.
