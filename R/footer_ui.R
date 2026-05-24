footer_ui <- function() {

footer_nav_link <- function(label, input_id) {

  tags$a(
    href = "#",
    class = "footer-nav-link",

    onclick = sprintf(
      "
      window.scrollTo({
        top: 0,
        behavior: 'smooth'
      });

      Shiny.setInputValue('%s', Math.random(), {
        priority: 'event'
      });

      return false;
      ",
      input_id
    ),

    label
  )
}

  div(
    class = "footer",

    div(
      class = "footer-content",

      # Lado esquerdo de quem olha: páginas
      div(
        class = "footer-left footer-pages",
        tags$nav(
          class = "footer-nav",
          footer_nav_link("Início", "menu_inicio"),
          footer_nav_link("Diagnóstico de municípios", "menu_municipio"),
          footer_nav_link("Ações recomendadas", "menu_acoes_recomendadas"),          
          footer_nav_link("Sobre o projeto", "menu_projeto"),

          footer_nav_link("Metodologia", "menu_indicadores"),

          footer_nav_link("Quem somos", "footer_quem_somos")
        )
      ),

      # Lado direito de quem olha: régua de logos
      div(
        class = "footer-right footer-logos",
        tags$img(
          src = "icons/ReguaDeLogos.svg",
          class = "footer-rega-logos",
          alt = "Logos institucionais"
        )
      )
    ),

    div(
      class = "footer-bottom",
      "Copyright © 2026 Vital Strategies"
    )
  )
}
