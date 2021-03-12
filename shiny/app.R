library(leaflet)
library(shiny)
library(shinydashboard)

criancas <- readr::read_rds("../data/criancas.rds")
n_criancas <- nrow(criancas)

pais <- readr::read_rds("../data/pais.rds") %>%
  dplyr::slice_sample(n = 10)
n_pais = nrow(pais)

tempos_entre <- "../data/tempos_entre_chegadas.rds" %>%
  readr::read_rds() %>%
  sort()

quer_adotar <- function(pai, crianca)
{
  falhas = c(
    crianca$anos_completos < pai$idade_minima,
    crianca$anos_completos > pai$idade_maxima,
    crianca$raca_cor == "Indigena" & !pai$cor_indigena,
    crianca$raca_cor == "Parda" & !pai$cor_parda,
    crianca$raca_cor == "Branca" & !pai$cor_branca,
    crianca$raca_cor == "Preta" & !pai$cor_amarela,
    crianca$raca_cor == "Amarela" & !pai$cor_amarela,
    crianca$sexo == "M" & !pai$sexo_masculino,
    crianca$sexo == "F" & !pai$sexo_feminino
  )
  prod(!falhas)
}

tempo_ate_crianca <- function() sample(tempos_entre, 1)
gera_crianca <- function() dplyr::slice_sample(criancas, n = 1)
tempo_adocao_sim <- function(pai)
{
  adotou <- 0
  tempo_total <- 0
  n_pais = nrow(pais)
  fila_pais <- rep(1, n_pais)
  while(!adotou)
  {
    tempo_total <- tempo_total + tempo_ate_crianca()
    crianca <- gera_crianca()
    adotada <- 0
    for(ii in 1:n_pais)
    {
      if(quer_adotar(pais[ii,], crianca) & fila_pais[ii])
      {
        fila_pais[ii] = 0
        adotada <- 1
        break
      }
    }
    if(!adotada & quer_adotar(pai, crianca)) adotou <- 1
  }
  tempo_total
}

tempo_adocao_m <- function(pai, n_sim = 100)
{
  tempos_sim <- rep(NA, n_sim)
  for(ii in 1:n_sim) tempos_sim[ii] <- tempo_adocao_sim(pai)
  mean(tempos_sim)
}

ui <- fluidPage(
  titlePanel("Selecione o perfil da criança Desejada"),

  fluidRow(
    column(3,
           wellPanel(
             h4("Selecione o perfil desejado"),
             sliderInput(inputId = "idade",
                         label = "Faixa etária", 0, 18, value=c(1,7)),
             selectInput("sexo",
                         "Sexo",
                         c("Todos" = "all",
                           "Feminino" = "F",
                           "Masculino" = "M")),
             selectInput("cor",
                         "Raça:",
                         c("Todos" = "all",
                           "Amarela" = "Amarela",
                           "Branca" = "Branca",
                           "Negra" = "Preta",
                           "Parda" = "Parda",
                           "Indígena" = "Indigena"))
           )
    ),
    column(9,
           wellPanel(
             span("Tempo médio de espera na fila para adotar uma criança com esse perfil (em dias):",
                  textOutput("t_adocao_m")))
    )
  )
)

server <- function(input, output, session) {
  perfil_pai = reactive({
    data.frame(
      idade_minima = input$idade[1],
      idade_maxima = input$idade[2],
      sexo_feminino = input$sexo == "F" | input$sexo == "all",
      sexo_masculino = input$sexo == "M" | input$sexo == "all",
      cor_branca = input$cor == "Branca" | input$cor == "all",
      cor_preta = input$cor == "Preta" | input$cor == "all",
      cor_amarela = input$cor == "Amarela" | input$cor == "all",
      cor_parda = input$cor == "Parda" | input$cor == "all",
      cor_indigena = input$cor == "Indigena" | input$cor == "all"
    )
  })

  tempo = reactive({
    perfil_pai() %>% tempo_adocao_m()
  })

  output$t_adocao_m <- renderText({(tempo())})
}

shinyApp(ui=ui, server = server)
