#Codigo Puro

#Instalação e Inicialização dos Pacotes
library(tidyverse) #Para manipulação de dados
library(scales) #Para formatação de números
library(janitor) #Para limpeza de nomes de colunas
library(reactable) #Para tabelas mais bonitas e interativas.
library(reactablefmtr)
#Bancos de Dados
br2019 <- read.csv("C:/Users/DamaLaptoper/Desktop/Dmas_code/Dados_sobre_Dados/Bancos_de_Dados/df2019.csv") %>% janitor::clean_names()
br2021 <- read.csv("C:/Users/DamaLaptoper/Desktop/Dmas_code/Dados_sobre_Dados/Bancos_de_Dados/df2021.csv") %>%  janitor::clean_names()
br2022 <- read.csv("C:/Users/DamaLaptoper/Desktop/Dmas_code/Dados_sobre_Dados/Bancos_de_Dados/df2022.csv") %>%  janitor::clean_names()
br2023 <- read.csv("C:/Users/DamaLaptoper/Desktop/Dmas_code/Dados_sobre_Dados/Bancos_de_Dados/df2023.csv") %>%  janitor::clean_names()
br2024 <- read.csv("C:/Users/DamaLaptoper/Desktop/Dmas_code/Dados_sobre_Dados/Bancos_de_Dados/df2024.csv") %>%  janitor::clean_names()

#Nesse ponto é possivel juntar os bancos de dados, mas necessitaria de um entendimento mais profundo dos dados, 
#renomear manualmente utilizando LLMs, mas ainda não tenho total controle sobre alucinações, 
#e ou confundimento então preferi deixar separado, tentarei deixar o codigo o mais legivel possível mesmo assim.


# =============================================================================
# ANÁLISE SALARIAL - STATE OF DATA BRAZIL
# =============================================================================

# -----------------------------------------------------------------------------
# 2. CONFIGURAÇÃO DAS FAIXAS SALARIAIS
# -----------------------------------------------------------------------------

# Faixas completas (2021-2024)
faixas_completas <- c(
  "Menos de R$ 1.000/mês",
  "de R$ 1.001/mês a R$ 2.000/mês",
  "de R$ 2.001/mês a R$ 3.000/mês",
  "de R$ 3.001/mês a R$ 4.000/mês",
  "de R$ 4.001/mês a R$ 6.000/mês",
  "de R$ 6.001/mês a R$ 8.000/mês",
  "de R$ 8.001/mês a R$ 12.000/mês",
  "de R$ 12.001/mês a R$ 16.000/mês",
  "de R$ 16.001/mês a R$ 20.000/mês",
  "de R$ 20.001/mês a R$ 25.000/mês",
  "de R$ 25.001/mês a R$ 30.000/mês",
  "de R$ 30.001/mês a R$ 40.000/mês",
  "Acima de R$ 40.001/mês"
)

# Faixas limitadas 2019 (máximo até 25.001)
faixas_2019 <- c(
  "Menos de R$ 1.000/mês",
  "de R$ 1.001/mês a R$ 2.000/mês",
  "de R$ 2.001/mês a R$ 3.000/mês",
  "de R$ 3.001/mês a R$ 4.000/mês",
  "de R$ 4.001/mês a R$ 6.000/mês",
  "de R$ 6.001/mês a R$ 8.000/mês",
  "de R$ 8.001/mês a R$ 12.000/mês",
  "de R$ 12.001/mês a R$ 16.000/mês",
  "de R$ 16.001/mês a R$ 20.000/mês",
  "de R$ 20.001/mês a R$ 25.000/mês",
  "Acima de R$ 25.001/mês"  # Limitação do ano 2019
)

# -----------------------------------------------------------------------------
# 3. FUNÇÕES DE FORMATAÇÃO
# -----------------------------------------------------------------------------

formatar_dados_salario <- function(dados, coluna_salario, faixas_ordem) {
  dados %>%
    select(all_of(coluna_salario)) %>%
    na.omit() %>%
    rename(faixa_salarial = 1) %>%
    mutate(faixa_salarial = factor(faixa_salarial, levels = faixas_ordem)) %>%
    count(faixa_salarial) %>%
    arrange(faixa_salarial) %>%
    rename(`Faixa Salarial` = faixa_salarial, Quantidade = n) %>%
    mutate(
      Relativo = Quantidade / sum(Quantidade),
      Acumulado = cumsum(Relativo),
      # Formatação em percentual
      Relativo_fmt = percent(Relativo, accuracy = 0.1),
      Acumulado_fmt = percent(Acumulado, accuracy = 0.1)
    )
}

# -----------------------------------------------------------------------------
# 4. CONFIGURAÇÃO DO TEMA
# -----------------------------------------------------------------------------

configurar_tema <- function() {
  tema <- espn()
  tema$titleStyle <- list(
    fontFamily = "'Bebas Neue', sans-serif", 
    fontSize = "2rem", 
    fontWeight = "700"
  )
  tema$subtitleStyle <- list(
    fontFamily = "'Bebas Neue', sans-serif", 
    fontSize = "1.25rem", 
    fontWeight = "400", 
    color = "#666"
  )
  return(tema)
}

# -----------------------------------------------------------------------------
# 5. FUNÇÃO PARA CRIAR TABELA INTERATIVA
# -----------------------------------------------------------------------------

criar_tabela_salarial <- function(dados_formatados, titulo, ano) {
  
  meu_tema <- configurar_tema()
  
  dados_formatados %>%
    mutate(Icones_Representacao = Quantidade) %>%
    reactable(
      striped = TRUE,
      pagination = FALSE,
      theme = meu_tema,
      showSortIcon = FALSE,
      searchable = TRUE,
      language = reactableLang(
        searchPlaceholder = "BUSCAR POR FAIXA..."
      ),
      columns = list(
        "Faixa Salarial" = colDef(
          name = "Faixa Salarial", 
          maxWidth = 200
        ),
        
        Icones_Representacao = colDef(
          name = "",
          align = "center",
          maxWidth = 150,
          cell = icon_assign(
            data = ., 
            icon = "user", 
            fill_color = "#555555",
            buckets = 5,
            show_values = 'none' 
          )
        ),
        
        Quantidade = colDef(
          name = "Quantidade",
          maxWidth = 100,
          style = color_scales(
            data = .,
            colors = c("#edf8e9", "#74c476", "#005a32")
          )
        ),
        
        Relativo_fmt = colDef(
          name = "Percentual",
          cell = data_bars(
            data = .,
            fill_color = "#3fc1c9",
            background = "#f2f2f2",
            text_position = "outside-end"
          )
        ),
        
        Acumulado_fmt = colDef(
          name = "Acumulado",
          maxWidth = 120
        ),
        
        # Ocultar colunas auxiliares
        Relativo = colDef(show = FALSE),
        Acumulado = colDef(show = FALSE)
      )
    ) %>% 
    add_title(paste("Distribuição Salarial", titulo)) %>% 
    add_source(paste("State of Data Brazil", ano))
}

# -----------------------------------------------------------------------------
# 6. ANÁLISE PARA 2024 (EXEMPLO DE USO)
# -----------------------------------------------------------------------------

# Processar dados de 2024
tabela_2024 <- formatar_dados_salario(
  dados = br2024,
  coluna_salario = "x2_h_faixa_salarial",
  faixas_ordem = faixas_completas
)

# Criar tabela interativa
tabela_interativa_2024 <- criar_tabela_salarial(
  dados_formatados = tabela_2024,
  titulo = "2024",
  ano = "2024-2025"
)

# Exibir tabela
tabela_interativa_2024

# -----------------------------------------------------------------------------
# 7. EXEMPLO PARA ANÁLISE HISTÓRICA (2019)
# -----------------------------------------------------------------------------

# Para comparar com 2019 (limitação nas faixas)
tabela_2019 <- formatar_dados_salario(
  dados = br2019,
  coluna_salario = "x_p16_salary_range", # Ajustar nome da coluna
  faixas_ordem = faixas_2019
)

# Criar tabela para 2019
tabela_interativa_2019 <- criar_tabela_salarial(
  dados_formatados = tabela_2019,
  titulo = "2019 (Dados Limitados)",
  ano = "2019"
)