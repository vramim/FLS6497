
########## DEFINIR DIRETORIO DE TRABALHO ############

setwd("C:/Users/vramim/Documents/Mestrado/FLS6497/Aula02")


########## INSTALAR E CARREGAR PACOTES ############

if(!require(pacman))
  install.packages("pacman")
library(pacman)

# A funcao p_load do pacote pacman faz o mesmo que foi feito acima para os demais pacotes necessarios.

pacman::p_load(mrl3, tensorflow, keras, readr, tidyverse)

# Instalacao do TensorFlow

library(tensorflow)
install_tensorflow()


########## BAIXAR OS DADOS ############

link <- "https://raw.githubusercontent.com/jacobwright32/Web_Scraper_AI_Core_Project/bb4865ae568e23ab8fadb6ea58cf117df2164ef3/web%20scraping/Cleaned%20Data/Brazil_Sao%20Bernardo%20Do%20Campo_Cleaned.csv"
dados <- readr::read_csv(link)


########## EXERCICIO a) ############

nuvens_temp <- ggplot(dados, aes(x = cloud_coverage, y = maximum_temprature)) +
  geom_point() +
  theme_bw() +
  geom_smooth()

nuvens_temp

# Os dados indicam que quanto menor for a cobertura de nuvens, maior tende a ser a temperatura máxima.


########## EXERCICIOS b) e c) ############

grafico <- dados %>%
  select_if(is.numeric) %>% # Seleciona se a variavel eh numerica
  pivot_longer(-maximum_temprature) %>% # Derretimento da base
  ggplot(aes(x = maximum_temprature, y = value)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~ name, scales = "free") # Gera varios metodos ao inves de apenas 1

ggsave(grafico, file = paste0("grafico.png"))



########## EXERCICIO d) ############

modelo1 <- lm(maximum_temprature ~ minimum_temprature + cloud_coverage + humidity + pressure, data = dados)
dados$predicao1 <- predict(modelo1)
summary(modelo1)

grafico1a <- ggplot(dados, aes(x = maximum_temprature, y = predicao1)) +
  geom_point() +
  theme_bw()
grafico1a

grafico1b <- dados %>%
  select(date, maximum_temprature, predicao1) %>%
  pivot_longer(-date) %>%
  ggplot(aes(x = date, y = value, color = name)) +
  geom_line()
grafico1b

grafico1c <- dados %>%
  arrange(maximum_temprature) %>%
  mutate(id = 1:n()) %>%
  select(id, maximum_temprature, predicao1) %>%
  pivot_longer(-id) %>%
  mutate(name = fct_relevel(name, "predicao1")) %>%
  ggplot(aes(x = id, y = value, color = name)) +
  geom_line()
grafico1c


########## EXERCICIO 2a) e 2b) ############

nova_funcao <- function(df){
  df %>%
  sample_n(1000) %>%
  summarise(humidity = mean(humidity)) %>%
  pluck("humidity")
}

par(mfrow = c(1,1), mai=c(1, 1, 1, 1))
media_umidade <- NULL
for (i in 1:1000){
  media_umidade <- append(media_umidade, nova_funcao(dados))
}

hist(media_umidade)

########## EXERCICIO 2c) ############

ultima_funcao <- function(df){
  df %>%
  sample_n(1000) %>%
  lm(humidity ~ minimum_temprature + cloud_coverage + pressure, data = .) %>%
  summary() %>%
  pluck("r.squared")
}

par(mfrow = c(1,1), mai=c(1, 1, 1, 1))
r2 <- NULL
for (i in 1:1000){
  r2 <- append(r2, ultima_funcao(dados))
}

hist(r2)
