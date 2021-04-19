library(leaflet)
library(magrittr)
library(shiny)
library(shinycssloaders)
library(shinydashboard)

# Data -------------------------------------------------------------------------
criancas <- readr::read_rds("data/criancas.rds")
n_criancas <- nrow(criancas)

pais <- readr::read_rds("data/pais.rds") %>%
  dplyr::slice_sample(n = 10)
n_pais = nrow(pais)

tempos_entre_chegadas <- "data/tempos_entre_chegadas.rds" %>%
  readr::read_rds() %>%
  sort()

#-------------------------------------------------------------------------------
# Minhas escolhas
escolhas_sexo <-  c("Feminino" = "F", "Masculino" = "M")
escolhas_raca <-  c(
  "Amarela" = "Amarela",
  "Branca" = "Branca",
  "Negra" = "Preta",
  "Parda" = "Parda",
  "Indígena" = "Indigena"
)

# ui ---------------------------------------------------------------------------
ui <-
  fluidPage(titlePanel("Selecione o perfil da criança Desejada"),
            fluidRow(column(
              3,
              wellPanel(
                h4("Selecione o perfil desejado"),
                sliderInput(
                  inputId = "idade",
                  label = "Faixa etária",
                  0,
                  18,
                  value = c(1, 7)
                ),
                # selecionar sexo
                checkboxGroupInput("sexo", "Sexo", escolhas_sexo, selected = escolhas_sexo),
                # selecionar raça
                checkboxGroupInput("cor", "Raça:", escolhas_raca, selected = escolhas_raca),
                actionButton("action", label = "Iniciar"),
              )
            ),
            column(9, wellPanel(
              span(
                "Tempo médio de espera na fila para adotar uma criança com esse perfil (em dias):",
                textOutput("t_adocao_m") %>% withSpinner()
              )
            ))))

# Server -----------------------------------------------------------------------
server <- function(input, output, session) {
  perfil_pai = reactive({
    data.frame(
      idade_minima = input$idade[1],
      idade_maxima = input$idade[2],
      sexo_feminino = "F" %in% input$sexo,
      sexo_masculino = "M" %in% input$sexo,
      cor_branca = "Branca" %in% input$cor,
      cor_preta = "Preta" %in% input$cor,
      cor_amarela = "Amarela" %in% input$cor,
      cor_parda = "Parda" %in% input$cor,
      cor_indigena = "Indígena" %in% input$cor
    )
  })
  tempo = eventReactive(input$action, {
    perfil_pai() %>% tempo_adocao_m(criancas, pais, tempos_entre_chegadas, n_sim = 100)
  })
  output$t_adocao_m <- renderPrint({
    (tempo())
  })
}
# Run the aplication
shinyApp(ui = ui, server = server)
