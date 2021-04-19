# Funções bases para o app

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

tempo_ate_crianca <- function(tempos_entre_chegadas)
  sample(tempos_entre_chegadas, 1)

gera_crianca <- function(criancas)
  dplyr::slice_sample(criancas, n = 1)

tempo_adocao_sim <- function(pai, criancas, pais, tempos_entre_chegadas)
{
  adotou <- 0
  tempo_total <- 0
  n_pais = nrow(pais)
  fila_pais <- rep(1, n_pais)
  while (!adotou)
  {
    tempo_total <- tempo_total +
      tempo_ate_crianca(tempos_entre_chegadas)
    crianca <- gera_crianca(criancas)
    adotada <- 0
    for (ii in 1:n_pais)
    {
      if (quer_adotar(pais[ii,], crianca) & fila_pais[ii])
      {
        fila_pais[ii] = 0
        adotada <- 1
        break
      }
    }
    if (!adotada & quer_adotar(pai, crianca))
      adotou <- 1
  }
  tempo_total
}

tempo_adocao_m <- function(pai, criancas, pais, tempos_entre_chegadas, n_sim = 100)
{
  tempos_sim <- rep(NA, n_sim)
  for (ii in 1:n_sim)
    tempos_sim[ii] <- tempo_adocao_sim(pai, criancas, pais, tempos_entre_chegadas)
  mean(tempos_sim)
}
