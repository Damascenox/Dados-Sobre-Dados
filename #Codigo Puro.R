#Codigo Puro

#Instalação e Inicialização dos Pacotes
library(tidyverse) #Para manipulação de dados
library(scales) #Para formatação de números
library(janitor) #Para limpeza de nomes de colunas
library(reactable) #Para tabelas mais bonitas e interativas.
library(reactablefmtr) #Para simplificar a formatação de tabelas
#Bancos de Dados
br2019 <- read.csv("C:/Users/DamaLaptoper/Desktop/Dmas_code/Dados_sobre_Dados/Bancos_de_Dados/df2019.csv") %>% janitor::clean_names()
br2021 <- read.csv("C:/Users/DamaLaptoper/Desktop/Dmas_code/Dados_sobre_Dados/Bancos_de_Dados/df2021.csv") %>%  janitor::clean_names()
br2022 <- read.csv("C:/Users/DamaLaptoper/Desktop/Dmas_code/Dados_sobre_Dados/Bancos_de_Dados/df2022.csv") %>%  janitor::clean_names()
br2023 <- read.csv("C:/Users/DamaLaptoper/Desktop/Dmas_code/Dados_sobre_Dados/Bancos_de_Dados/df2023.csv") %>%  janitor::clean_names()
br2024 <- read.csv("C:/Users/DamaLaptoper/Desktop/Dmas_code/Dados_sobre_Dados/Bancos_de_Dados/df2024.csv") %>%  janitor::clean_names()

#Nesse ponto é possivel juntar os bancos de dados, mas necessitaria de um entendimento mais profundo dos dados, 
#renomear manualmente utilizando LLMs, mas ainda não tenho total controle sobre alucinações, 
#e ou confundimento então preferi deixar separado, tentarei deixar o codigo o mais legivel possível mesmo assim.


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
#OBS: No banco de 2019, as faixas salariais só vão até "Acima de R$ 25.001/mês"

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

#' Configura o tema visual para tabelas interativas reactable.
#'
#' Esta função define estilos personalizados para títulos e subtítulos,
#' retornando um objeto de tema que pode ser utilizado em tabelas interativas.
#' @return Um objeto de tema reactable com estilos personalizados.

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
    mutate(Icones_Representacao = Quantidade) %>% # Adiciona coluna de ícones relativos a quantidade.
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
# ANÁLISE PARA 2024 
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
# ANÁLISE PARA 2023
# -----------------------------------------------------------------------------

# Processar dados de 2023
tabela_2023 <- formatar_dados_salario(
  dados = br2023,
  coluna_salario = "x_p2_h_faixa_salarial",
  faixas_ordem = faixas_completas
)

# Criar tabela interativa
tabela_interativa_2023 <- criar_tabela_salarial(
  dados_formatados = tabela_2023,
  titulo = "2023",
  ano = "2023-2024"
)

# Exibir tabela
tabela_interativa_2023


# -----------------------------------------------------------------------------
# ANÁLISE PARA 2022
# -----------------------------------------------------------------------------

# Processar dados de 2022
tabela_2022 <- formatar_dados_salario(
  dados = br2022,
  coluna_salario = "x_p2_h_faixa_salarial",
  faixas_ordem = faixas_completas
)

# Criar tabela interativa
tabela_interativa_2022 <- criar_tabela_salarial(
  dados_formatados = tabela_2022,
  titulo = "2023",
  ano = "2022-2023"
)

# Exibir tabela
tabela_interativa_2022


# -----------------------------------------------------------------------------
# ANÁLISE PARA 2021
# -----------------------------------------------------------------------------

# Processar dados de 2021
tabela_2021 <- formatar_dados_salario(
  dados = br2021,
  coluna_salario = "x_p2_h_faixa_salarial",
  faixas_ordem = faixas_completas
)

# Criar tabela interativa
tabela_interativa_2021 <- criar_tabela_salarial(
  dados_formatados = tabela_2021,
  titulo = "2022",
  ano = "2021-2022"
)

# Exibir tabela
tabela_interativa_2021

# -----------------------------------------------------------------------------
# EXEMPLO PARA ANÁLISE HISTÓRICA (2019)
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
# -----------------------------------------------------------------------------
# 7. FUNÇÕES PARA GRÁFICOS EVOLUTIVOS
# -----------------------------------------------------------------------------

combinar_dados_historicos <- function() {
  # Processar cada ano individualmente
  dados_2019 <- formatar_dados_salario(bancoBR19, "coluna_salario_2019", faixas_2019) %>%
    mutate(Ano = "2019")
  
  dados_2021 <- formatar_dados_salario(bancoBR21, "coluna_salario_2021", faixas_completas) %>%
    mutate(Ano = "2021")
  
  dados_2022 <- formatar_dados_salario(bancoBR22, "coluna_salario_2022", faixas_completas) %>%
    mutate(Ano = "2022")
  
  dados_2023 <- formatar_dados_salario(bancoBR23, "coluna_salario_2023", faixas_completas) %>%
    mutate(Ano = "2023")
  
  dados_2024 <- formatar_dados_salario(bancoBR24, "2.h_faixa_salarial", faixas_completas) %>%
    mutate(Ano = "2024")
  
  # Padronizar faixas para comparação (usar apenas faixas presentes em 2019)
  faixas_padronizadas <- intersect(faixas_2019, faixas_completas)
  
  # Combinar e filtrar apenas faixas comparáveis
  dados_combinados <- bind_rows(dados_2019, dados_2021, dados_2022, dados_2023, dados_2024) %>%
    filter(`Faixa Salarial` %in% faixas_padronizadas) %>%
    mutate(
      Ano = factor(Ano, levels = c("2019", "2021", "2022", "2023", "2024")),
      `Faixa Salarial` = factor(`Faixa Salarial`, levels = faixas_padronizadas)
    )
  
  return(dados_combinados)
}

criar_grafico_evolutivo_linhas <- function(dados_combinados) {
  ggplot(dados_combinados, aes(x = Ano, y = Relativo, color = `Faixa Salarial`, group = `Faixa Salarial`)) +
    geom_line(size = 1.2, alpha = 0.8) +
    geom_point(size = 3, alpha = 0.9) +
    facet_wrap(~`Faixa Salarial`, scales = "free_y", ncol = 3) +
    scale_y_continuous(labels = percent_format(accuracy = 0.1)) +
    scale_color_viridis_d(option = "plasma", guide = "none") +
    labs(
      title = "Evolução das Faixas Salariais na Área de Dados (2019-2024)",
      subtitle = "Proporção de profissionais em cada faixa salarial ao longo do tempo",
      y = "Proporção de Profissionais",
      x = "Ano",
      caption = "Nota: Dados de 2020 não disponíveis. Faixas limitadas às presentes em 2019 para comparabilidade."
    ) +
    theme_minimal() +
    theme(
      strip.text = element_text(size = 9, face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
      plot.subtitle = element_text(hjust = 0.5, size = 11),
      plot.caption = element_text(size = 8, color = "gray50"),
      panel.grid.minor = element_blank()
    )
}

criar_grafico_barras_comparativo <- function(dados_combinados) {
  ggplot(dados_combinados, aes(x = `Faixa Salarial`, y = Relativo, fill = Ano)) +
    geom_col(position = position_dodge(width = 0.8), alpha = 0.8) +
    scale_y_continuous(labels = percent_format(accuracy = 0.1)) +
    scale_fill_brewer(
      palette = "Set2", 
      name = "Ano",
      guide = guide_legend(title.position = "top", title.hjust = 0.5)
    ) +
    labs(
      title = "Distribuição Salarial por Ano na Área de Dados",
      subtitle = "Comparação da proporção de profissionais em cada faixa salarial (2019-2024)",
      x = "Faixa Salarial",
      y = "Proporção de Profissionais",
      caption = "Nota: Dados de 2020 não disponíveis. Análise baseada em dados comparáveis entre os anos."
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
      legend.position = "bottom",
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
      plot.subtitle = element_text(hjust = 0.5, size = 11),
      plot.caption = element_text(size = 8, color = "gray50"),
      panel.grid.minor.y = element_blank(),
      panel.grid.major.x = element_blank()
    ) +
    guides(fill = guide_legend(nrow = 1, title.position = "top"))
}

# Função para criar gráfico de tendências gerais
criar_grafico_tendencias <- function(dados_combinados) {
  # Criar categorias simplificadas para análise de tendências
  dados_tendencias <- dados_combinados %>%
    mutate(
      Categoria_Salario = case_when(
        str_detect(`Faixa Salarial`, "1.000|2.000|3.000") ~ "Até R$ 3.000",
        str_detect(`Faixa Salarial`, "4.000|6.000") ~ "R$ 3.001 - R$ 6.000",
        str_detect(`Faixa Salarial`, "8.000|12.000") ~ "R$ 6.001 - R$ 12.000",
        str_detect(`Faixa Salarial`, "16.000|20.000|25.000") ~ "R$ 12.001 - R$ 25.000",
        TRUE ~ "Acima de R$ 25.000"
      )
    ) %>%
    group_by(Ano, Categoria_Salario) %>%
    summarise(Relativo_Agrupado = sum(Relativo), .groups = "drop") %>%
    mutate(Categoria_Salario = factor(
      Categoria_Salario, 
      levels = c("Até R$ 3.000", "R$ 3.001 - R$ 6.000", "R$ 6.001 - R$ 12.000", 
                 "R$ 12.001 - R$ 25.000", "Acima de R$ 25.000")
    ))
  
  ggplot(dados_tendencias, aes(x = Ano, y = Relativo_Agrupado, fill = Categoria_Salario)) +
    geom_area(alpha = 0.7, position = "stack") +
    scale_y_continuous(labels = percent_format()) +
    scale_fill_viridis_d(option = "viridis", name = "Faixa Salarial") +
    labs(
      title = "Evolução da Estrutura Salarial na Área de Dados",
      subtitle = "Distribuição proporcional por grandes faixas salariais (2019-2024)",
      x = "Ano",
      y = "Proporção Acumulada",
      caption = "Faixas agrupadas para melhor visualização das tendências gerais"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
      plot.subtitle = element_text(hjust = 0.5, size = 11),
      plot.caption = element_text(size = 8, color = "gray50"),
      legend.position = "bottom",
      panel.grid.minor = element_blank()
    ) +
    guides(fill = guide_legend(nrow = 2, title.position = "top"))
}

# -----------------------------------------------------------------------------
# 8. EXECUÇÃO DA ANÁLISE HISTÓRICA
# -----------------------------------------------------------------------------

# Combinar dados históricos
dados_historicos <- combinar_dados_historicos()

# Criar gráficos
grafico_linhas <- criar_grafico_evolutivo_linhas(dados_historicos)
grafico_barras <- criar_grafico_barras_comparativo(dados_historicos)
grafico_tendencias <- criar_grafico_tendencias(dados_historicos)
