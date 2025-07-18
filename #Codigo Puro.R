#Codigo Puro

#Instalação e Inicialização dos Pacotes
library(tidyverse) #Para manipulação de dados
library(scales) #Para formatação de números
library(janitor) #Para limpeza de nomes de colunas
library(reactable) #Para tabelas mais bonitas e interativas.
#Bancos de Dados
BR2019 <- read.csv("C:/Users/DamaLaptoper/Desktop/Dmas_code/Dados_sobre_Dados/Bancos_de_Dados/df2019.csv") %>% janitor::clean_names()
BR2021 <- read.csv("C:/Users/DamaLaptoper/Desktop/Dmas_code/Dados_sobre_Dados/Bancos_de_Dados/df2021.csv") %>%  janitor::clean_names()
BR2022 <- read.csv("C:/Users/DamaLaptoper/Desktop/Dmas_code/Dados_sobre_Dados/Bancos_de_Dados/df2022.csv") %>%  janitor::clean_names()
BR2023 <- read.csv("C:/Users/DamaLaptoper/Desktop/Dmas_code/Dados_sobre_Dados/Bancos_de_Dados/df2023.csv") %>%  janitor::clean_names()
BR2024 <- read.csv("C:/Users/DamaLaptoper/Desktop/Dmas_code/Dados_sobre_Dados/Bancos_de_Dados/df2024.csv") %>%  janitor::clean_names()

#Nesse ponto é possivel juntar os bancos de dados, mas necessitaria de um entendimento mais profundo dos dados, 
#renomear manualmente utilizando LLMs, mas ainda não tenho total controle sobre alucinações, 
#e ou confundimento então preferi deixar separado, tentarei deixar o codigo o mais legivel possível mesmo assim.

