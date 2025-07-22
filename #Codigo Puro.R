#Instalação e Inicialização dos Pacotes
library(tidyverse) #Para manipulação de dados
library(scales) #Para formatação de números
library(janitor) #Para limpeza de nomes de colunas
install.packages("reactablefmtr")
library(reactable) #Para tabelas mais bonitas e interativas.
library(reactablefmtr)
library(stringr)
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
# CONFIGURAÇÃO DAS FAIXAS SALARIAIS
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
  "Acima de R$ 25.001/mês"
)

# -----------------------------------------------------------------------------
# PROCESSAMENTO DOS DADOS INDIVIDUAIS
# -----------------------------------------------------------------------------

# Dados 2019
dados_2019 <- br2019 %>%
  select(x_p16_salary_range) %>%
  na.omit() %>%
  rename(faixa_salarial = x_p16_salary_range) %>%
  mutate(faixa_salarial = factor(faixa_salarial, levels = faixas_2019)) %>%
  count(faixa_salarial) %>%
  arrange(faixa_salarial) %>%
  rename(`Faixa Salarial` = faixa_salarial, Quantidade = n) %>%
  mutate(
    Relativo = Quantidade / sum(Quantidade),
    Acumulado = cumsum(Relativo),
    Relativo_fmt = percent(Relativo, accuracy = 0.1),
    Acumulado_fmt = percent(Acumulado, accuracy = 0.1),
    Ano = "2019"
  )

# Dados 2021
dados_2021 <- br2021 %>%
  select(x_p2_h_faixa_salarial) %>%
  na.omit() %>%
  rename(faixa_salarial = x_p2_h_faixa_salarial) %>%
  mutate(faixa_salarial = factor(faixa_salarial, levels = faixas_completas)) %>%
  count(faixa_salarial) %>%
  arrange(faixa_salarial) %>%
  rename(`Faixa Salarial` = faixa_salarial, Quantidade = n) %>%
  mutate(
    Relativo = Quantidade / sum(Quantidade),
    Acumulado = cumsum(Relativo),
    Relativo_fmt = percent(Relativo, accuracy = 0.1),
    Acumulado_fmt = percent(Acumulado, accuracy = 0.1),
    Ano = "2021"
  )

# Dados 2022
dados_2022 <- br2022 %>%
  select(x_p2_h_faixa_salarial) %>%
  na.omit() %>%
  rename(faixa_salarial = x_p2_h_faixa_salarial) %>%
  mutate(faixa_salarial = factor(faixa_salarial, levels = faixas_completas)) %>%
  count(faixa_salarial) %>%
  arrange(faixa_salarial) %>%
  rename(`Faixa Salarial` = faixa_salarial, Quantidade = n) %>%
  mutate(
    Relativo = Quantidade / sum(Quantidade),
    Acumulado = cumsum(Relativo),
    Relativo_fmt = percent(Relativo, accuracy = 0.1),
    Acumulado_fmt = percent(Acumulado, accuracy = 0.1),
    Ano = "2022"
  )

# Dados 2023
dados_2023 <- br2023 %>%
  select(x_p2_h_faixa_salarial) %>%
  na.omit() %>%
  rename(faixa_salarial = x_p2_h_faixa_salarial) %>%
  mutate(faixa_salarial = factor(faixa_salarial, levels = faixas_completas)) %>%
  count(faixa_salarial) %>%
  arrange(faixa_salarial) %>%
  rename(`Faixa Salarial` = faixa_salarial, Quantidade = n) %>%
  mutate(
    Relativo = Quantidade / sum(Quantidade),
    Acumulado = cumsum(Relativo),
    Relativo_fmt = percent(Relativo, accuracy = 0.1),
    Acumulado_fmt = percent(Acumulado, accuracy = 0.1),
    Ano = "2023"
  )

# Dados 2024
dados_2024 <- br2024 %>%
  select(x2_h_faixa_salarial) %>%
  na.omit() %>%
  rename(faixa_salarial = x2_h_faixa_salarial) %>%
  mutate(faixa_salarial = factor(faixa_salarial, levels = faixas_completas)) %>%
  count(faixa_salarial) %>%
  arrange(faixa_salarial) %>%
  rename(`Faixa Salarial` = faixa_salarial, Quantidade = n) %>%
  mutate(
    Relativo = Quantidade / sum(Quantidade),
    Acumulado = cumsum(Relativo),
    Relativo_fmt = percent(Relativo, accuracy = 0.1),
    Acumulado_fmt = percent(Acumulado, accuracy = 0.1),
    Ano = "2024"
  )

# -----------------------------------------------------------------------------
# TABELAS INTERATIVAS INDIVIDUAIS
# -----------------------------------------------------------------------------

# Configurar tema
tema <- espn()
tema$titleStyle <- list(fontFamily = "'Bebas Neue', sans-serif", fontSize = "2rem", fontWeight = "700")
tema$subtitleStyle <- list(fontFamily = "'Bebas Neue', sans-serif", fontSize = "1.25rem", fontWeight = "400", color = "#666")

# Tabela 2024 (mais recente)
tabela_2024 <- dados_2024 %>%
  mutate(Icones_Representacao = Quantidade) %>%
  reactable(
    striped = TRUE,
    pagination = FALSE,
    theme = tema,
    showSortIcon = FALSE,
    searchable = TRUE,
    language = reactableLang(searchPlaceholder = "BUSCAR POR FAIXA..."),
    columns = list(
      "Faixa Salarial" = colDef(name = "Faixa Salarial", maxWidth = 200),
      Icones_Representacao = colDef(
        name = "", align = "center", maxWidth = 150,
        cell = icon_assign(data = ., icon = "user", fill_color = "#555555", buckets = 5, show_values = 'none')
      ),
      Quantidade = colDef(
        name = "Quantidade", maxWidth = 100,
        style = color_scales(data = ., colors = c("#edf8e9", "#74c476", "#005a32"))
      ),
      Relativo_fmt = colDef(name = "Percentual", maxWidth = 150, align = "left"),
      Acumulado_fmt = colDef(name = "Acumulado", maxWidth = 120),
      Relativo = colDef(show = FALSE),
      Acumulado = colDef(show = FALSE),
      Ano = colDef(show = FALSE)
    )
  ) %>% 
  add_title("Distribuição Salarial 2024") %>% 
  add_source("State of Data Brazil 2024")

# Exibir tabelas

print(tabela_2024)


# Tabela 2019
tabela_2019 <- dados_2019 %>%
  mutate(Icones_Representacao = Quantidade) %>%
  reactable(
    striped = TRUE,
    pagination = FALSE,
    theme = tema,
    showSortIcon = FALSE,
    searchable = TRUE,
    language = reactableLang(searchPlaceholder = "BUSCAR POR FAIXA..."),
    columns = list(
      "Faixa Salarial" = colDef(name = "Faixa Salarial", maxWidth = 200),
      Icones_Representacao = colDef(
        name = "", align = "center", maxWidth = 150,
        cell = icon_assign(data = ., icon = "user", fill_color = "#555555", buckets = 5, show_values = 'none')
      ),
      Quantidade = colDef(
        name = "Quantidade", maxWidth = 100,
        style = color_scales(data = ., colors = c("#edf8e8", "#74c476", "#005a32"))
      ),
      Relativo_fmt = colDef(name = "Percentual", maxWidth = 150, align = "left"),
      Acumulado_fmt = colDef(name = "Acumulado", maxWidth = 120),
      Relativo = colDef(show = FALSE),
      Acumulado = colDef(show = FALSE),
      Ano = colDef(show = FALSE)
    )
  ) %>% 
  add_title("Distribuição Salarial 2019") %>% 
  add_source("State of Data Brazil 2019")

dados_2019_expandido <- dados_2019 %>%
  mutate(`Faixa Salarial` = ifelse(`Faixa Salarial` == "Acima de R$ 25.001/mês", 
                                    "Acima de R$ 40.001/mês", 
                                    as.character(`Faixa Salarial`))) %>%
  mutate(`Faixa Salarial` = factor(`Faixa Salarial`, levels = faixas_completas))

# Combinar dados históricos usando todas as faixas completas
dados_historicos <- bind_rows(dados_2019_expandido, dados_2021, dados_2022, dados_2023, dados_2024) %>%
  mutate(
    Ano = factor(Ano, levels = c("2019", "2021", "2022", "2023", "2024")),
    `Faixa Salarial` = factor(`Faixa Salarial`, levels = faixas_completas)
  ) %>%
  # Preencher com 0 para faixas não existentes em 2019
  complete(Ano, `Faixa Salarial`, fill = list(Quantidade = 0, Relativo = 0, Acumulado = 0, 
                                               Relativo_fmt = "0.0%", Acumulado_fmt = "0.0%"))

# -----------------------------------------------------------------------------
# GRÁFICO 1: EVOLUÇÃO POR LINHAS
# -----------------------------------------------------------------------------

grafico_linhas <- ggplot(dados_historicos, aes(x = Ano, y = Relativo, color = `Faixa Salarial`, group = `Faixa Salarial`)) +
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
    caption = "Nota: Dados de 2020 não disponíveis. Faixa 'Acima de R$ 40.001/mês' em 2019 inclui todos os valores acima de R$ 25.001/mês."
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

print(grafico_linhas)


# -----------------------------------------------------------------------------
# GRÁFICO 3: TENDÊNCIAS GERAIS (TODOS OS ANOS COM FAIXAS COMPLETAS)
# -----------------------------------------------------------------------------

# Criar categorias salariais agrupadas para todos os anos
dados_tendencias <- bind_rows(dados_2019_expandido, dados_2021, dados_2022, dados_2023, dados_2024) %>%
  mutate(
    Categoria_Salario = case_when(
      str_detect(`Faixa Salarial`, "1.000|2.000|3.000") ~ "Até R$ 3.000",
      str_detect(`Faixa Salarial`, "4.000|6.000") ~ "R$ 3.001 - R$ 6.000", 
      str_detect(`Faixa Salarial`, "8.000|12.000") ~ "R$ 6.001 - R$ 12.000",
      str_detect(`Faixa Salarial`, "16.000|20.000|25.000") ~ "R$ 12.001 - R$ 25.000",
      str_detect(`Faixa Salarial`, "30.000|40.000") ~ "R$ 25.001 - R$ 40.000",
      str_detect(`Faixa Salarial`, "Acima") ~ "Acima de R$ 40.000",
      TRUE ~ "Outras faixas"
    )
  ) %>%
  group_by(Ano, Categoria_Salario) %>%
  summarise(Relativo_Agrupado = sum(Relativo), .groups = "drop") %>%
  mutate(
    Ano = factor(Ano, levels = c("2019", "2021", "2022", "2023", "2024")),
    Categoria_Salario = factor(
      Categoria_Salario, 
      levels = c("Até R$ 3.000", "R$ 3.001 - R$ 6.000", "R$ 6.001 - R$ 12.000", 
                 "R$ 12.001 - R$ 25.000", "R$ 25.001 - R$ 40.000", "Acima de R$ 40.000")
    )
  )

grafico_tendencias <- ggplot(dados_tendencias, aes(x = Ano, y = Relativo_Agrupado, fill = Categoria_Salario)) +
  geom_area(alpha = 0.7, position = "stack") +
  scale_y_continuous(labels = percent_format()) +
  scale_fill_viridis_d(option = "viridis", name = "Faixa Salarial") +
  labs(
    title = "Evolução da Estrutura Salarial na Área de Dados",
    subtitle = "Distribuição proporcional por faixas salariais agrupadas (2019, 2021-2024)",
    x = "Ano",
    y = "Proporção Acumulada",
    caption = "Análise incluindo todos os anos disponíveis. Faixa 'Acima de R$ 40.000' em 2019 inclui todos os valores acima de R$ 25.001/mês."
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 11),
    plot.caption = element_text(size = 8, color = "gray50"),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

print(grafico_tendencias)

akjjjjjjjjjshkjdhdkdkskdsdkjsdkshdskdksdhskdskdsdhskdsjdhskdsjdhskjdhsdkjshksjdhskdjhsdkshdksjdhskdhsdkjshdskjdhskjshdksjdhskdhskshkjdhskdh

library(reactable)
library(reactablefmtr)
library(dplyr)
library(DT)
library(tidyr)

# =============================================================================
# ANÁLISE SALARIAL POR NÍVEL DE EDUCAÇÃO - STATE OF DATA BRAZIL
# =============================================================================

# -----------------------------------------------------------------------------
# PREPARAÇÃO DOS DADOS 2024
# -----------------------------------------------------------------------------

# Assumindo que br2024 já está carregado
# Vamos usar a coluna de educação e criar faixas salariais baseadas na coluna existente

# Preparar dados de educação (ajustar nome da coluna conforme necessário)
# Substitua 'coluna_educacao' pelo nome real da coluna de educação no seu dataset
dados_educacao_2024 <- br2024 %>%
  select(educacao = x1_l_nivel_de_ensino, salario = x2_h_faixa_salarial) %>%
  na.omit()

# Definir níveis de educação em ordem hierárquica
dados_educacao_2024$educacao <- factor(dados_educacao_2024$educacao, levels = c(
"Doutorado ou Phd",
"Mestrado",
"Pós-graduação",
"Graduação/Bacharelado",
"Estudante de Graduação",
"Não tenho graduação formal",
"Prefiro não informar"))

# Criar faixas salariais simplificadas baseadas nas existentes
dados_educacao_2024 <- dados_educacao_2024 %>%
  mutate(faixasalarial = case_when(
    str_detect(salario, "Menos de R\\$ 1.000") ~ "0-1k",
    str_detect(salario, "1.001.*2.000") ~ "1-2k", 
    str_detect(salario, "2.001.*3.000") ~ "2-3k",
    str_detect(salario, "3.001.*4.000") ~ "3-4k",
    str_detect(salario, "4.001.*6.000") ~ "4-6k",
    str_detect(salario, "6.001.*8.000") ~ "6-8k",
    str_detect(salario, "8.001.*12.000") ~ "8-12k",
    str_detect(salario, "12.001.*16.000") ~ "12-16k",
    str_detect(salario, "16.001.*20.000") ~ "16-20k",
    str_detect(salario, "20.001.*25.000") ~ "20-25k",
    str_detect(salario, "25.001.*30.000") ~ "25-30k",
    str_detect(salario, "30.001.*40.000") ~ "30-40k",
    str_detect(salario, "Acima de R\\$ 40.001") ~ "40k+",
    TRUE ~ "Outros"
  ))

# Definir ordem das faixas salariais
dados_educacao_2024$faixasalarial <- factor(dados_educacao_2024$faixasalarial, 
                                           levels = c("0-1k", "1-2k", "2-3k", "3-4k", 
                                                     "4-6k", "6-8k", "8-12k", "12-16k",
                                                     "16-20k", "20-25k", "25-30k", 
                                                     "30-40k", "40k+"))

# -----------------------------------------------------------------------------
# CRIAÇÃO DAS TABELAS DE FREQUÊNCIA
# -----------------------------------------------------------------------------

# Frequência absoluta
tabela_freq <- dados_educacao_2024 %>%
  count(educacao, faixasalarial) %>%
  pivot_wider(names_from = faixasalarial, values_from = n, values_fill = 0)

# Frequência relativa por nível de educação
tabela_relativa <- tabela_freq %>%
  mutate(Total = rowSums(across(-educacao))) %>%
  mutate(across(-c(educacao, Total), ~ round(.x / Total * 100, 1))) %>%
  select(-Total)

# Renomear primeira coluna
colnames(tabela_freq)[1] <- "Nível de Educação"
colnames(tabela_relativa)[1] <- "Nível de Educação"

# -----------------------------------------------------------------------------
# TABELA INTERATIVA COM DT (FREQUÊNCIAS ABSOLUTAS)
# -----------------------------------------------------------------------------

tabela_dt_absoluta <- datatable(
  tabela_freq, 
  options = list(
    pageLength = 15, 
    dom = 't',
    scrollX = TRUE
  ),
  caption = 'Distribuição Absoluta: Faixa Salarial (em milhares de R$) por Nível de Educação',
  class = 'stripe hover cell-border order-column compact',
  style = "bootstrap"
) %>%
  formatStyle(
    columns = colnames(tabela_freq)[-1],
    backgroundColor = styleInterval(
      c(5, 15, 30, 50), 
      values = c('#f7fbff', '#c6dbef', '#6baed6', '#3182bd', '#08519c')
    ),
    textAlign = 'center'
  ) %>%
  formatStyle(
    columns = "Nível de Educação",
    fontWeight = 'bold',
    textAlign = 'left'
  )

print(tabela_dt_absoluta)

# -----------------------------------------------------------------------------
# TABELA INTERATIVA COM DT (FREQUÊNCIAS RELATIVAS - PERCENTUAIS)
# -----------------------------------------------------------------------------

tabela_dt_relativa <- datatable(
  tabela_relativa, 
  options = list(
    pageLength = 15, 
    dom = 't',
    scrollX = TRUE
  ),
  caption = 'Distribuição Percentual: Faixa Salarial (em milhares de R$) por Nível de Educação (%)',
  class = 'stripe hover cell-border order-column compact',
  style = "bootstrap"
) %>%
  formatStyle(
    columns = colnames(tabela_relativa)[-1],
    backgroundColor = styleInterval(
      c(10, 25, 40), 
      values = c('#f7fbff', '#c6dbef', '#6baed6', '#2171b5')
    ),
    textAlign = 'center'
  ) %>%
  formatStyle(
    columns = "Nível de Educação",
    fontWeight = 'bold',
    textAlign = 'left'
  )

print(tabela_dt_relativa)

# -----------------------------------------------------------------------------
# TABELA REACTABLE MODERNA (ALTERNATIVA)
# -----------------------------------------------------------------------------

tabela_reactable <- tabela_relativa %>%
  reactable(
    pagination = FALSE,
    striped = TRUE,
    highlight = TRUE,
    searchable = TRUE,
    theme = reactableTheme(
      borderColor = "#ddd",
      stripedColor = "#f6f8fa",
      highlightColor = "#f0f0f0"
    ),
    columns = list(
      `Nível de Educação` = colDef(
        name = "Nível de Educação",
        minWidth = 200,
        style = list(fontWeight = "bold")
      )
    ),
    defaultColDef = colDef(
      align = "center",
      minWidth = 80,
      cell = function(value) {
        if (is.numeric(value)) paste0(value, "%") else value
      },
      style = function(value) {
        if (is.numeric(value)) {
          color <- if (value >= 40) {
            "#2171b5"
          } else if (value >= 25) {
            "#6baed6"
          } else if (value >= 10) {
            "#c6dbef"
          } else {
            "#f7fbff"
          }
          list(backgroundColor = color, color = if (value >= 25) "white" else "black")
        }
      }
    )
  ) %>%
  add_title("Distribuição Salarial por Nível de Educação (%)") %>%
  add_source("State of Data Brazil 2024")

print(tabela_reactable)

# -----------------------------------------------------------------------------
# ANÁLISE ESTATÍSTICA COMPLEMENTAR
# -----------------------------------------------------------------------------

# Resumo por nível de educação
resumo_educacao <- dados_educacao_2024 %>%
  group_by(educacao) %>%
  summarise(
    total_respondentes = n(),
    .groups = "drop"
  ) %>%
  arrange(desc(total_respondentes))

print("=== RESUMO POR NÍVEL DE EDUCAÇÃO ===")
print(resumo_educacao)

# Análise das faixas salariais mais altas por educação
faixas_altas <- dados_educacao_2024 %>%
  filter(faixasalarial %in% c("25-30k", "30-40k", "40k+")) %>%
  group_by(educacao) %>%
  summarise(
    salarios_altos = n(),
    .groups = "drop"
  ) %>%
  arrange(desc(salarios_altos))

print("=== PROFISSIONAIS COM SALÁRIOS MAIS ALTOS POR EDUCAÇÃO ===")
print(faixas_altas)

# -----------------------------------------------------------------------------
# GRÁFICO COMPLEMENTAR
# -----------------------------------------------------------------------------

library(ggplot2)

grafico_educacao_salario <- ggplot(dados_educacao_2024, aes(x = educacao, fill = faixasalarial)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_viridis_d(option = "plasma", name = "Faixa Salarial\n(milhares R$)") +
  labs(
    title = "Distribuição Salarial por Nível de Educação",
    subtitle = "Proporção de profissionais em cada faixa salarial por nível educacional",
    x = "Nível de Educação",
    y = "Proporção",
    caption = "State of Data Brazil 2024"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 11),
    legend.position = "right"
  )

print(grafico_educacao_salario)


sfkajfklagldjhasldhagsdladjhagdkasdjhsgdkashgdkajsghdkashdg


# -----------------------------------------------------------------------------
# ANÁLISE DE SALÁRIOS POR NÍVEL DE EXPERIÊNCIA - DADOS BRASILEIROS
# -----------------------------------------------------------------------------

library(ggplot2)
library(dplyr)
library(scales)

# -----------------------------------------------------------------------------
# PREPARAÇÃO DOS DADOS PARA ANÁLISE
# -----------------------------------------------------------------------------

# Função para converter faixa salarial em valor médio (para ordenação)
converter_faixa_para_valor <- function(faixa) {
  case_when(
    faixa == "Menos de R$ 1.000/mês" ~ 500,
    faixa == "de R$ 1.001/mês a R$ 2.000/mês" ~ 1500,
    faixa == "de R$ 2.001/mês a R$ 3.000/mês" ~ 2500,
    faixa == "de R$ 3.001/mês a R$ 4.000/mês" ~ 3500,
    faixa == "de R$ 4.001/mês a R$ 6.000/mês" ~ 5000,
    faixa == "de R$ 6.001/mês a R$ 8.000/mês" ~ 7000,
    faixa == "de R$ 8.001/mês a R$ 12.000/mês" ~ 10000,
    faixa == "de R$ 12.001/mês a R$ 16.000/mês" ~ 14000,
    faixa == "de R$ 16.001/mês a R$ 20.000/mês" ~ 18000,
    faixa == "de R$ 20.001/mês a R$ 25.000/mês" ~ 22500,
    faixa == "de R$ 25.001/mês a R$ 30.000/mês" ~ 27500,
    faixa == "de R$ 30.001/mês a R$ 40.000/mês" ~ 35000,
    faixa == "Acima de R$ 40.001/mês" ~ 50000,
    faixa == "Acima de R$ 25.001/mês" ~ 30000, # Para dados de 2019
    TRUE ~ NA_real_
  )
}

# -----------------------------------------------------------------------------
# ANÁLISE 1: DADOS DE 2024 (MAIS RECENTE)
# -----------------------------------------------------------------------------

# Preparar dados 2024 com nível de experiência
dados_2024_exp <- br2024 %>%
  select(x2_h_faixa_salarial, x2_g_nivel) %>%
  filter(
    !is.na(x2_h_faixa_salarial) & x2_h_faixa_salarial != "" & x2_h_faixa_salarial != "NA",
    !is.na(x2_g_nivel) & x2_g_nivel != "" & x2_g_nivel != "NA"
  ) %>%
  rename(
    faixa_salarial = x2_h_faixa_salarial,
    nivel_experiencia = x2_g_nivel
  ) %>%
  mutate(
    faixa_salarial = factor(faixa_salarial, levels = faixas_completas),
    valor_medio = converter_faixa_para_valor(faixa_salarial)
  ) %>%
  filter(!is.na(valor_medio)) # Remove casos onde a conversão retornou NA

# Gráfico 1: Boxplot com valores médios das faixas
grafico_01_2024 <- ggplot(dados_2024_exp, aes(x = nivel_experiencia, y = valor_medio)) +
  geom_boxplot(fill = "steelblue", alpha = 0.7) +
  labs(
    title = "Distribuição de Salários por Nível de Experiência - Brasil 2024",
    subtitle = "Baseado nos valores médios das faixas salariais",
    x = "Nível de Experiência",
    y = "Salário Mensal (R$)"
  ) +
  scale_y_continuous(labels = scales::dollar_format(prefix = "R$ ", big.mark = ".")) +
  scale_x_discrete(na.translate = FALSE) + # Remove NA do eixo X
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Gráfico 2: Distribuição por faixas (heatmap)
distribuicao_2024 <- dados_2024_exp %>%
  count(nivel_experiencia, faixa_salarial) %>%
  group_by(nivel_experiencia) %>%
  mutate(prop = n / sum(n))

grafico_02_2024 <- ggplot(distribuicao_2024, aes(x = nivel_experiencia, y = faixa_salarial, fill = prop)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "darkblue", labels = percent) +
  labs(
    title = "Distribuição de Faixas Salariais por Nível de Experiência - Brasil 2024",
    x = "Nível de Experiência",
    y = "Faixa Salarial",
    fill = "Proporção"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text.y = element_text(size = 8)
  )

# -----------------------------------------------------------------------------
# ANÁLISE 2: COMPARAÇÃO ENTRE ANOS (2021-2024)
# -----------------------------------------------------------------------------

# Combinar dados de todos os anos
dados_combinados <- bind_rows(
  br2021 %>%
    select(x_p2_h_faixa_salarial, x_p2_g_nivel) %>%
    filter(
      !is.na(x_p2_h_faixa_salarial) & x_p2_h_faixa_salarial != "" & x_p2_h_faixa_salarial != "NA",
      !is.na(x_p2_g_nivel) & x_p2_g_nivel != "" & x_p2_g_nivel != "NA"
    ) %>%
    rename(faixa_salarial = x_p2_h_faixa_salarial, nivel_experiencia = x_p2_g_nivel) %>%
    mutate(ano = "2021"),
  
  br2022 %>%
    select(x_p2_h_faixa_salarial, x_p2_g_nivel) %>%
    filter(
      !is.na(x_p2_h_faixa_salarial) & x_p2_h_faixa_salarial != "" & x_p2_h_faixa_salarial != "NA",
      !is.na(x_p2_g_nivel) & x_p2_g_nivel != "" & x_p2_g_nivel != "NA"
    ) %>%
    rename(faixa_salarial = x_p2_h_faixa_salarial, nivel_experiencia = x_p2_g_nivel) %>%
    mutate(ano = "2022"),
  
  br2023 %>%
    select(x_p2_h_faixa_salarial, x_p2_g_nivel) %>%
    filter(
      !is.na(x_p2_h_faixa_salarial) & x_p2_h_faixa_salarial != "" & x_p2_h_faixa_salarial != "NA",
      !is.na(x_p2_g_nivel) & x_p2_g_nivel != "" & x_p2_g_nivel != "NA"
    ) %>%
    rename(faixa_salarial = x_p2_h_faixa_salarial, nivel_experiencia = x_p2_g_nivel) %>%
    mutate(ano = "2023"),
  
  br2024 %>%
    select(x2_h_faixa_salarial, x2_g_nivel) %>%
    filter(
      !is.na(x2_h_faixa_salarial) & x2_h_faixa_salarial != "" & x2_h_faixa_salarial != "NA",
      !is.na(x2_g_nivel) & x2_g_nivel != "" & x2_g_nivel != "NA"
    ) %>%
    rename(faixa_salarial = x2_h_faixa_salarial, nivel_experiencia = x2_g_nivel) %>%
    mutate(ano = "2024")
) %>%
  mutate(
    faixa_salarial = factor(faixa_salarial, levels = faixas_completas),
    valor_medio = converter_faixa_para_valor(faixa_salarial)
  ) %>%
  filter(!is.na(valor_medio)) # Remove casos onde a conversão retornou NA

# Gráfico 3: Evolução temporal por nível
grafico_03_evolucao <- ggplot(dados_combinados, aes(x = ano, y = valor_medio, fill = nivel_experiencia)) +
  geom_boxplot() +
  facet_wrap(~ nivel_experiencia, scales = "free_y") +
  labs(
    title = "Evolução dos Salários por Nível de Experiência (2021-2024)",
    x = "Ano",
    y = "Salário Mensal (R$)",
    fill = "Nível de Experiência"
  ) +
  scale_y_continuous(labels = scales::dollar_format(prefix = "R$ ", big.mark = ".")) +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    legend.position = "none"
  )

# -----------------------------------------------------------------------------
# ANÁLISE 3: ESTATÍSTICAS RESUMO
# -----------------------------------------------------------------------------

# Resumo por nível de experiência (2024)
resumo_2024 <- dados_2024_exp %>%
  group_by(nivel_experiencia) %>%
  summarise(
    n_respostas = n(),
    salario_mediano = median(valor_medio, na.rm = TRUE),
    q1 = quantile(valor_medio, 0.25, na.rm = TRUE),
    q3 = quantile(valor_medio, 0.75, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(salario_mediano))

# Tabela formatada
print("Resumo dos Salários por Nível de Experiência - 2024:")
print(resumo_2024)

# -----------------------------------------------------------------------------
# VISUALIZAÇÃO DOS GRÁFICOS
# -----------------------------------------------------------------------------

# Exibir gráficos
print(grafico_01_2024)
print(grafico_02_2024)
print(grafico_03_evolucao)

# -----------------------------------------------------------------------------
# ANÁLISE ADICIONAL: TOP FAIXAS POR NÍVEL
# -----------------------------------------------------------------------------

# Faixa salarial mais comum por nível de experiência
top_faixas_2024 <- dados_2024_exp %>%
  count(nivel_experiencia, faixa_salarial) %>%
  group_by(nivel_experiencia) %>%
  slice_max(n, n = 1) %>%
  arrange(desc(n))

print("Faixa salarial mais comum por nível de experiência (2024):")
print(top_faixas_2024)

alsdjalksjdhalskdjalskjdhalksjdhalkjsdlas

# -----------------------------------------------------------------------------
# ANÁLISE DE SALÁRIOS POR TAMANHO DA EMPRESA - DADOS BRASILEIROS
# -----------------------------------------------------------------------------

library(ggplot2)
library(dplyr)
library(scales)

# -----------------------------------------------------------------------------
# CONFIGURAÇÃO DAS FAIXAS DE TAMANHO DA EMPRESA
# -----------------------------------------------------------------------------

# Definir níveis ordenados para tamanho da empresa
niveis_empresa <- c(
  "de 1 a 5", 
  "de 6 a 10",
  "de 11 a 50", 
  "de 51 a 100",
  "de 101 a 500", 
  "de 501 a 1.000", 
  "de 1.001 a 3.000",
  "Acima de 3.000"
)

# -----------------------------------------------------------------------------
# FUNÇÃO PARA CONVERTER FAIXA SALARIAL EM VALOR MÉDIO (já definida anteriormente)
# -----------------------------------------------------------------------------

converter_faixa_para_valor <- function(faixa) {
  case_when(
    faixa == "Menos de R$ 1.000/mês" ~ 500,
    faixa == "de R$ 1.001/mês a R$ 2.000/mês" ~ 1500,
    faixa == "de R$ 2.001/mês a R$ 3.000/mês" ~ 2500,
    faixa == "de R$ 3.001/mês a R$ 4.000/mês" ~ 3500,
    faixa == "de R$ 4.001/mês a R$ 6.000/mês" ~ 5000,
    faixa == "de R$ 6.001/mês a R$ 8.000/mês" ~ 7000,
    faixa == "de R$ 8.001/mês a R$ 12.000/mês" ~ 10000,
    faixa == "de R$ 12.001/mês a R$ 16.000/mês" ~ 14000,
    faixa == "de R$ 16.001/mês a R$ 20.000/mês" ~ 18000,
    faixa == "de R$ 20.001/mês a R$ 25.000/mês" ~ 22500,
    faixa == "de R$ 25.001/mês a R$ 30.000/mês" ~ 27500,
    faixa == "de R$ 30.001/mês a R$ 40.000/mês" ~ 35000,
    faixa == "Acima de R$ 40.001/mês" ~ 50000,
    faixa == "Acima de R$ 25.001/mês" ~ 30000, # Para dados de 2019
    TRUE ~ NA_real_
  )
}

# -----------------------------------------------------------------------------
# ANÁLISE 1: DADOS DE 2024 (MAIS RECENTE)
# -----------------------------------------------------------------------------

# Preparar dados 2024 com tamanho da empresa
dados_2024_empresa <- br2024 %>%
  select(x2_h_faixa_salarial, x2_c_numero_de_funcionarios) %>%
  filter(
    !is.na(x2_h_faixa_salarial) & x2_h_faixa_salarial != "" & x2_h_faixa_salarial != "NA",
    !is.na(x2_c_numero_de_funcionarios) & x2_c_numero_de_funcionarios != "" & x2_c_numero_de_funcionarios != "NA"
  ) %>%
  rename(
    faixa_salarial = x2_h_faixa_salarial,
    tamanho_empresa = x2_c_numero_de_funcionarios
  ) %>%
  mutate(
    faixa_salarial = factor(faixa_salarial, levels = faixas_completas),
    tamanho_empresa = factor(tamanho_empresa, levels = niveis_empresa),
    valor_medio = converter_faixa_para_valor(faixa_salarial)
  ) %>%
  filter(!is.na(valor_medio) & !is.na(tamanho_empresa)) # Remove NAs

# Gráfico 1: Boxplot por tamanho da empresa (2024)
grafico_09_2024 <- ggplot(dados_2024_empresa, aes(x = tamanho_empresa, y = valor_medio)) +
  geom_boxplot(fill = "yellow", alpha = 0.7) +
  labs(
    title = "Distribuição de Salários por Tamanho da Empresa - Brasil 2024",
    subtitle = "Baseado nos valores médios das faixas salariais",
    x = "Tamanho da Empresa (nº de funcionários)",
    y = "Salário Mensal (R$)"
  ) +
  scale_y_continuous(labels = scales::dollar_format(prefix = "R$ ", big.mark = ".")) +
  scale_x_discrete(na.translate = FALSE) + # Remove NA do eixo X
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# -----------------------------------------------------------------------------
# ANÁLISE 2: COMPARAÇÃO ENTRE ANOS (2021-2024)
# -----------------------------------------------------------------------------

# Combinar dados de todos os anos
dados_empresa_combinados <- bind_rows(
  br2021 %>%
    select(x_p2_h_faixa_salarial, x_p2_c_numero_de_funcionarios) %>%
    filter(
      !is.na(x_p2_h_faixa_salarial) & x_p2_h_faixa_salarial != "" & x_p2_h_faixa_salarial != "NA",
      !is.na(x_p2_c_numero_de_funcionarios) & x_p2_c_numero_de_funcionarios != "" & x_p2_c_numero_de_funcionarios != "NA"
    ) %>%
    rename(faixa_salarial = x_p2_h_faixa_salarial, tamanho_empresa = x_p2_c_numero_de_funcionarios) %>%
    mutate(ano = "2021"),
  
  br2022 %>%
    select(x_p2_h_faixa_salarial, x_p2_c_numero_de_funcionarios) %>%
    filter(
      !is.na(x_p2_h_faixa_salarial) & x_p2_h_faixa_salarial != "" & x_p2_h_faixa_salarial != "NA",
      !is.na(x_p2_c_numero_de_funcionarios) & x_p2_c_numero_de_funcionarios != "" & x_p2_c_numero_de_funcionarios != "NA"
    ) %>%
    rename(faixa_salarial = x_p2_h_faixa_salarial, tamanho_empresa = x_p2_c_numero_de_funcionarios) %>%
    mutate(ano = "2022"),
  
  br2023 %>%
    select(x_p2_h_faixa_salarial, x_p2_c_numero_de_funcionarios) %>%
    filter(
      !is.na(x_p2_h_faixa_salarial) & x_p2_h_faixa_salarial != "" & x_p2_h_faixa_salarial != "NA",
      !is.na(x_p2_c_numero_de_funcionarios) & x_p2_c_numero_de_funcionarios != "" & x_p2_c_numero_de_funcionarios != "NA"
    ) %>%
    rename(faixa_salarial = x_p2_h_faixa_salarial, tamanho_empresa = x_p2_c_numero_de_funcionarios) %>%
    mutate(ano = "2023"),
  
  br2024 %>%
    select(x2_h_faixa_salarial, x2_c_numero_de_funcionarios) %>%
    filter(
      !is.na(x2_h_faixa_salarial) & x2_h_faixa_salarial != "" & x2_h_faixa_salarial != "NA",
      !is.na(x2_c_numero_de_funcionarios) & x2_c_numero_de_funcionarios != "" & x2_c_numero_de_funcionarios != "NA"
    ) %>%
    rename(faixa_salarial = x2_h_faixa_salarial, tamanho_empresa = x2_c_numero_de_funcionarios) %>%
    mutate(ano = "2024")
) %>%
  mutate(
    faixa_salarial = factor(faixa_salarial, levels = faixas_completas),
    tamanho_empresa = factor(tamanho_empresa, levels = niveis_empresa),
    valor_medio = converter_faixa_para_valor(faixa_salarial)
  ) %>%
  filter(!is.na(valor_medio) & !is.na(tamanho_empresa)) # Remove NAs

# Gráfico 2: Evolução temporal por tamanho da empresa
grafico_empresa_evolucao <- ggplot(dados_empresa_combinados, aes(x = ano, y = valor_medio, fill = tamanho_empresa)) +
  geom_boxplot() +
  facet_wrap(~ tamanho_empresa, scales = "free_y", ncol = 4) +
  labs(
    title = "Evolução dos Salários por Tamanho da Empresa (2021-2024)",
    x = "Ano",
    y = "Salário Mensal (R$)",
    fill = "Tamanho da Empresa"
  ) +
  scale_y_continuous(labels = scales::dollar_format(prefix = "R$ ", big.mark = ".", scale = 0.001, suffix = "k")) +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    legend.position = "none",
    strip.text = element_text(size = 8)
  )

# -----------------------------------------------------------------------------
# ANÁLISE 3: HEATMAP - DISTRIBUIÇÃO POR FAIXAS
# -----------------------------------------------------------------------------

# Distribuição por faixas salariais e tamanho da empresa (2024)
distribuicao_empresa_2024 <- dados_2024_empresa %>%
  count(tamanho_empresa, faixa_salarial) %>%
  group_by(tamanho_empresa) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup() %>%
  filter(!is.na(faixa_salarial) & !is.na(tamanho_empresa))

# Gráfico 3: Heatmap da distribuição
grafico_heatmap_empresa <- ggplot(distribuicao_empresa_2024, aes(x = tamanho_empresa, y = faixa_salarial, fill = prop)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "orange", labels = percent) +
  labs(
    title = "Distribuição de Faixas Salariais por Tamanho da Empresa - Brasil 2024",
    x = "Tamanho da Empresa (nº de funcionários)",
    y = "Faixa Salarial",
    fill = "Proporção"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text.y = element_text(size = 8)
  )

# -----------------------------------------------------------------------------
# ANÁLISE 4: ESTATÍSTICAS RESUMO
# -----------------------------------------------------------------------------

# Resumo por tamanho da empresa (2024)
resumo_empresa_2024 <- dados_2024_empresa %>%
  group_by(tamanho_empresa) %>%
  summarise(
    n_respostas = n(),
    salario_mediano = median(valor_medio, na.rm = TRUE),
    salario_medio = mean(valor_medio, na.rm = TRUE),
    q1 = quantile(valor_medio, 0.25, na.rm = TRUE),
    q3 = quantile(valor_medio, 0.75, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(salario_mediano))

# Faixa salarial mais comum por tamanho de empresa
top_faixas_empresa_2024 <- dados_2024_empresa %>%
  count(tamanho_empresa, faixa_salarial) %>%
  group_by(tamanho_empresa) %>%
  slice_max(n, n = 1) %>%
  arrange(tamanho_empresa)

# -----------------------------------------------------------------------------
# VISUALIZAÇÃO DOS GRÁFICOS
# -----------------------------------------------------------------------------

# Exibir gráficos
print(grafico_09_2024)
print(grafico_empresa_evolucao)
print(grafico_heatmap_empresa)

# Exibir estatísticas
print("Resumo dos Salários por Tamanho da Empresa - 2024:")
print(resumo_empresa_2024)

print("Faixa salarial mais comum por tamanho de empresa (2024):")
print(top_faixas_empresa_2024)

iahdlkahsdlkajshdlajkshdlaksjdhlakjsdhlakjsdhalksjdhalksjdhalksjdh

 #Processar dados de 2024
salarios_2024 <- br2024 %>%
  filter(!is.na(x2_f_cargo_atual) & !is.na(x2_h_faixa_salarial)) %>%
  mutate(salario_numerico = converter_faixa_para_valor(x2_h_faixa_salarial)) %>%
  filter(!is.na(salario_numerico)) %>%
  group_by(cargo = x2_f_cargo_atual) %>%
  summarise(
    salario_medio = mean(salario_numerico, na.rm = TRUE),
    n_obs = n(),
    .groups = "drop"
  ) %>%
 filter(n_obs >= 10 & cargo != "NA" & !is.na(cargo)) %>%
  mutate(ano = 2024)

# Processar dados de 2023
salarios_2023 <- br2023 %>%
  filter(!is.na(x_p2_f_cargo_atual) & !is.na(x_p2_h_faixa_salarial)) %>%
  mutate(salario_numerico = converter_faixa_para_valor(x_p2_h_faixa_salarial)) %>%
  filter(!is.na(salario_numerico)) %>%
  group_by(cargo = x_p2_f_cargo_atual) %>%
  summarise(
    salario_medio = mean(salario_numerico, na.rm = TRUE),
    n_obs = n(),
    .groups = "drop"
  ) %>%
  filter(n_obs >= 10 & cargo != "NA" & !is.na(cargo)) %>%
  mutate(ano = 2023)

# Processar dados de 2022
salarios_2022 <- br2022 %>%
  filter(!is.na(x_p2_f_cargo_atual) & !is.na(x_p2_h_faixa_salarial)) %>%
  mutate(salario_numerico = converter_faixa_para_valor(x_p2_h_faixa_salarial)) %>%
  filter(!is.na(salario_numerico)) %>%
  group_by(cargo = x_p2_f_cargo_atual) %>%
  summarise(
    salario_medio = mean(salario_numerico, na.rm = TRUE),
    n_obs = n(),
    .groups = "drop"
  ) %>%
  filter(n_obs >= 10 & cargo != "NA" & !is.na(cargo)) %>%
  mutate(ano = 2022)

# Processar dados de 2021
salarios_2021 <- br2021 %>%
  filter(!is.na(x_p2_f_cargo_atual) & !is.na(x_p2_h_faixa_salarial)) %>%
  mutate(salario_numerico = converter_faixa_para_valor(x_p2_h_faixa_salarial)) %>%
  filter(!is.na(salario_numerico)) %>%
  group_by(cargo = x_p2_f_cargo_atual) %>%
  summarise(
    salario_medio = mean(salario_numerico, na.rm = TRUE),
    n_obs = n(),
    .groups = "drop"
  ) %>%
 filter(n_obs >= 10 & cargo != "NA" & !is.na(cargo)) %>%
  mutate(ano = 2021)

# Combinar todos os dados
dados_completos <- bind_rows(salarios_2021, salarios_2022, salarios_2023, salarios_2024)

# Calcular média geral por cargo (para ranking)
ranking_cargos <- dados_completos %>%
  group_by(cargo) %>%
  summarise(
    salario_medio_geral = mean(salario_medio, na.rm = TRUE),
    anos_presentes = n(),
    .groups = "drop"
  ) %>%
  filter(anos_presentes >= 2) %>%  # Cargos presentes em pelo menos 2 anos
  arrange(desc(salario_medio_geral))

# Selecionar top 6 e bottom 6
top_6_cargos <- head(ranking_cargos, 6)$cargo
bottom_6_cargos <- tail(ranking_cargos, 6)$cargo

# Filtrar dados para os cargos selecionados
dados_filtrados <- dados_completos %>%
  filter(cargo %in% c(top_6_cargos, bottom_6_cargos)) %>%
  mutate(
    grupo = ifelse(cargo %in% top_6_cargos, "Top 6 Salários", "Bottom 6 Salários"),
    cargo_curto = case_when(
      nchar(cargo) > 30 ~ paste0(substr(cargo, 1, 27), "..."),
      TRUE ~ cargo
    )
  )

# Gráfico dos Top 6 salários
p1 <- dados_filtrados %>%
  filter(grupo == "Top 6 Salários") %>%
  ggplot(aes(x = reorder(cargo_curto, salario_medio), y = salario_medio, fill = factor(ano))) +
  geom_col(position = "dodge", alpha = 0.8) +
  coord_flip() +
  scale_fill_viridis_d(name = "Ano") +
  scale_y_continuous(labels = scales::dollar_format(prefix = "R$ ", big.mark = ".")) +
  labs(
    title = "Top 6 Cargos - Maiores Salários Médios",
    subtitle = "Comparação entre 2021-2024",
    x = "Cargo",
    y = "Salário Médio (R$)",
    caption = "Fonte: Dados internos"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.text.y = element_text(size = 10),
    legend.position = "bottom"
  )

# Gráfico dos Bottom 6 salários
p2 <- dados_filtrados %>%
  filter(grupo == "Bottom 6 Salários") %>%
  ggplot(aes(x = reorder(cargo_curto, salario_medio), y = salario_medio, fill = factor(ano))) +
  geom_col(position = "dodge", alpha = 0.8) +
  coord_flip() +
  scale_fill_viridis_d(name = "Ano") +
  scale_y_continuous(labels = scales::dollar_format(prefix = "R$ ", big.mark = ".")) +
  labs(
    title = "Bottom 6 Cargos - Menores Salários Médios",
    subtitle = "Comparação entre 2021-2024",
    x = "Cargo",
    y = "Salário Médio (R$)",
    caption = "Fonte: Dados internos"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.text.y = element_text(size = 10),
    legend.position = "bottom"
  )

# Exibir os gráficos
print(p1)
print(p2)

# Tabela resumo dos Top 6
cat("\n=== TOP 6 CARGOS (MAIORES SALÁRIOS) ===\n")
top_6_resumo <- dados_filtrados %>%
  filter(grupo == "Top 6 Salários") %>%
  select(cargo, ano, salario_medio) %>%
  pivot_wider(names_from = ano, values_from = salario_medio, names_prefix = "Ano_") %>%
  arrange(desc(rowMeans(select(., starts_with("Ano_")), na.rm = TRUE)))

print(top_6_resumo)

# Tabela resumo dos Bottom 6
cat("\n=== BOTTOM 6 CARGOS (MENORES SALÁRIOS) ===\n")
bottom_6_resumo <- dados_filtrados %>%
  filter(grupo == "Bottom 6 Salários") %>%
  select(cargo, ano, salario_medio) %>%
  pivot_wider(names_from = ano, values_from = salario_medio, names_prefix = "Ano_") %>%
  arrange(rowMeans(select(., starts_with("Ano_")), na.rm = TRUE))

print(bottom_6_resumo)

# Estatísticas gerais
cat("\n=== ESTATÍSTICAS GERAIS ===\n")
cat("Total de cargos únicos analisados:", length(unique(dados_completos$cargo)), "\n")
cat("Total de observações por ano:\n")
print(table(dados_completos$ano))

kjsdhkashdkahksdajsdhadjkasdhakshdka

# Carregar bibliotecas necessárias
library(dplyr)
library(ggplot2)
library(tidyr)
library(janitor)

# Sua função de conversão de faixa salarial
converter_faixa_para_valor <- function(faixa) {
  case_when(
    faixa == "Menos de R$ 1.000/mês" ~ 500,
    faixa == "de R$ 1.001/mês a R$ 2.000/mês" ~ 1500,
    faixa == "de R$ 2.001/mês a R$ 3.000/mês" ~ 2500,
    faixa == "de R$ 3.001/mês a R$ 4.000/mês" ~ 3500,
    faixa == "de R$ 4.001/mês a R$ 6.000/mês" ~ 5000,
    faixa == "de R$ 6.001/mês a R$ 8.000/mês" ~ 7000,
    faixa == "de R$ 8.001/mês a R$ 12.000/mês" ~ 10000,
    faixa == "de R$ 12.001/mês a R$ 16.000/mês" ~ 14000,
    faixa == "de R$ 16.001/mês a R$ 20.000/mês" ~ 18000,
    faixa == "de R$ 20.001/mês a R$ 25.000/mês" ~ 22500,
    faixa == "de R$ 25.001/mês a R$ 30.000/mês" ~ 27500,
    faixa == "de R$ 30.001/mês a R$ 40.000/mês" ~ 35000,
    faixa == "Acima de R$ 40.001/mês" ~ 50000,
    faixa == "Acima de R$ 25.001/mês" ~ 30000, # Para dados de 2019
    TRUE ~ NA_real_
  )
}

# Processar dados de 2024
salarios_2024 <- br2024 %>%
  filter(!is.na(x2_f_cargo_atual) & !is.na(x2_h_faixa_salarial) & 
         x2_f_cargo_atual != "" & x2_h_faixa_salarial != "") %>%
  mutate(salario_numerico = converter_faixa_para_valor(x2_h_faixa_salarial)) %>%
  filter(!is.na(salario_numerico) & !is.na(x2_f_cargo_atual)) %>%
  group_by(cargo = x2_f_cargo_atual) %>%
  summarise(
    salario_medio = mean(salario_numerico),
    n_obs = n(),
    .groups = "drop"
  ) %>%
  filter(n_obs >= 10 & cargo != "NA" & !is.na(cargo)) %>%
  arrange(desc(salario_medio))

# Selecionar top 6 e bottom 6
top_6_cargos <- head(salarios_2024, 6)
bottom_6_cargos <- tail(salarios_2024, 6)

# Combinar dados para visualização
dados_viz <- bind_rows(
  top_6_cargos %>% mutate(grupo = "Top 6 Salários"),
  bottom_6_cargos %>% mutate(grupo = "Bottom 6 Salários")
) %>%
  mutate(
    cargo_curto = case_when(
      nchar(cargo) > 30 ~ paste0(substr(cargo, 1, 27), "..."),
      TRUE ~ cargo
    )
  )

# Gráfico dos Top 6 salários
p1 <- top_6_cargos %>%
  mutate(cargo_curto = case_when(
    nchar(cargo) > 30 ~ paste0(substr(cargo, 1, 27), "..."),
    TRUE ~ cargo
  )) %>%
  ggplot(aes(x = reorder(cargo_curto, salario_medio), y = salario_medio)) +
  geom_col(fill = "#1f77b4", alpha = 0.8, width = 0.7) +
  coord_flip() +
  scale_y_continuous(labels = scales::dollar_format(prefix = "R$ ", big.mark = ".")) +
  labs(
    title = "Top 6 Cargos - Maiores Salários Médios (2024)",
    x = "Cargo",
    y = "Salário Médio (R$)",
    caption = "Fonte: Dados internos - 2024"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.text.y = element_text(size = 10),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank()
  )

# Gráfico dos Bottom 6 salários
p2 <- bottom_6_cargos %>%
  mutate(cargo_curto = case_when(
    nchar(cargo) > 30 ~ paste0(substr(cargo, 1, 27), "..."),
    TRUE ~ cargo
  )) %>%
  ggplot(aes(x = reorder(cargo_curto, salario_medio), y = salario_medio)) +
  geom_col(fill = "#ff7f0e", alpha = 0.8, width = 0.7) +
  coord_flip() +
  scale_y_continuous(labels = scales::dollar_format(prefix = "R$ ", big.mark = ".")) +
  labs(
    title = "Bottom 6 Cargos - Menores Salários Médios (2024)",
    x = "Cargo",
    y = "Salário Médio (R$)",
    caption = "Fonte: Dados internos - 2024"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.text.y = element_text(size = 10),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank()
  )

# Exibir os gráficos
print(p1)
print(p2)

# Tabela resumo dos Top 6
cat("\n=== TOP 6 CARGOS - MAIORES SALÁRIOS (2024) ===\n")
top_6_resumo <- top_6_cargos %>%
  mutate(salario_formatado = paste0("R$ ", format(salario_medio, big.mark = ".", decimal.mark = ",", nsmall = 0))) %>%
  select(Cargo = cargo, `Salário Médio` = salario_formatado, `Nº Observações` = n_obs)

print(top_6_resumo)

# Tabela resumo dos Bottom 6
cat("\n=== BOTTOM 6 CARGOS - MENORES SALÁRIOS (2024) ===\n")
bottom_6_resumo <- bottom_6_cargos %>%
  mutate(salario_formatado = paste0("R$ ", format(salario_medio, big.mark = ".", decimal.mark = ",", nsmall = 0))) %>%
  select(Cargo = cargo, `Salário Médio` = salario_formatado, `Nº Observações` = n_obs)

print(bottom_6_resumo)

laksjdlaksjdlakjsldjalsjdlajs



empresas <- br2024 %>%
  filter(x2_k_satisfeito_atualmente == TRUE & x2_c_numero_de_funcionarios == "Acima de 3.000" | x2_c_numero_de_funcionarios == "de 1.001 a 3.000" )

view(empresas)


ggplot(br2024, aes((y = x2_c_numero_de_funcionarios ))) + geom_bar()
92837492374982749237429482374


# Análise: Ambiente de Trabalho em Empresas de Tecnologia de Grande Porte
# Dataset: br2024

# Carregar bibliotecas necessárias
library(dplyr)
library(ggplot2)
library(tidyr)
library(corrplot)
library(RColorBrewer)
library(gridExtra)
library(knitr)

# Assumindo que o dataset já está carregado como 'br2024'
# Se não estiver, descomente a linha abaixo:
# br2024 <- read.csv("seu_arquivo.csv")

# ===============================================
# 1. FILTROS E PREPARAÇÃO DOS DADOS
# ===============================================

# Filtrar empresas de grande porte e cargos relevantes
dados_tech <- br2024 %>%
  filter(
    # Empresas de grande porte
    x2_c_numero_de_funcionarios %in% c("de 1.001 a 3.000", "Acima de 3.000"),
    # Filtrar por cargos relacionados a dados/tech (ajuste conforme necessário)
    grepl("Data|Dados|Software|Engenharia|Engineer|Analytics|Science|Tech|TI", 
          x2_f_cargo_atual, ignore.case = TRUE) |
    grepl("Data|Dados|Software|Engenharia|Engineer|Analytics|Science|Tech|TI", 
          x2_b_setor, ignore.case = TRUE)
  )

# Verificar tamanho da amostra
cat("Tamanho da amostra após filtros:", nrow(dados_tech), "\n")
cat("Distribuição por tamanho de empresa:\n")
table(dados_tech$x2_c_numero_de_funcionarios)

# ===============================================
# 2. ANÁLISE DE SATISFAÇÃO GERAL
# ===============================================

# Função para converter escalas de satisfação em numérico (ajuste conforme sua escala)
convert_satisfaction <- function(x) {
  case_when(
    x %in% c("Muito insatisfeito", "1") ~ 1,
    x %in% c("Insatisfeito", "2") ~ 2,
    x %in% c("Neutro", "Indiferente", "3") ~ 3,
    x %in% c("Satisfeito", "4") ~ 4,
    x %in% c("Muito satisfeito", "5") ~ 5,
    TRUE ~ NA_real_
  )
}

# Aplicar conversão nas colunas de satisfação
satisfaction_cols <- c("x2_l_1_remuneracao_salario", "x2_l_2_beneficios", 
                      "x2_l_3_proposito_do_trabalho_e_da_empresa",
                      "x2_l_4_flexibilidade_de_trabalho_remoto",
                      "x2_l_5_ambiente_e_clima_de_trabalho",
                      "x2_l_6_oportunidade_de_aprendizado_e_trabalhar_com_referencias",
                      "x2_l_7_oportunidades_de_crescimento",
                      "x2_l_8_maturidade_da_empresa_em_termos_de_tecnologia_e_dados",
                      "x2_l_9_relacao_com_os_gestores_e_lideres",
                      "x2_l_10_reputacao_que_a_empresa_tem_no_mercado")

dados_tech_processed <- dados_tech %>%
  mutate(across(all_of(satisfaction_cols), convert_satisfaction))

# ===============================================
# 3. ANÁLISE POR TAMANHO DE EMPRESA
# ===============================================

# Satisfação média por tamanho de empresa
satisfacao_por_tamanho <- dados_tech_processed %>%
  group_by(x2_c_numero_de_funcionarios) %>%
  summarise(
    n = n(),
    ambiente_clima = mean(x2_l_5_ambiente_e_clima_de_trabalho, na.rm = TRUE),
    gestores_lideres = mean(x2_l_9_relacao_com_os_gestores_e_lideres, na.rm = TRUE),
    maturidade_tech = mean(x2_l_8_maturidade_da_empresa_em_termos_de_tecnologia_e_dados, na.rm = TRUE),
    oportunidades_crescimento = mean(x2_l_7_oportunidades_de_crescimento, na.rm = TRUE),
    remuneracao = mean(x2_l_1_remuneracao_salario, na.rm = TRUE),
    flexibilidade = mean(x2_l_4_flexibilidade_de_trabalho_remoto, na.rm = TRUE),
    .groups = 'drop'
  )

print("Satisfação Média por Tamanho de Empresa:")
print(satisfacao_por_tamanho)

# ===============================================
# 4. VISUALIZAÇÕES
# ===============================================

# Gráfico 1: Satisfação por dimensão e tamanho de empresa
dados_viz <- dados_tech_processed %>%
  select(x2_c_numero_de_funcionarios, all_of(satisfaction_cols)) %>%
  pivot_longer(cols = -x2_c_numero_de_funcionarios, 
               names_to = "dimensao", values_to = "satisfacao") %>%
  mutate(
    dimensao_label = case_when(
      dimensao == "x2_l_1_remuneracao_salario" ~ "Remuneração",
      dimensao == "x2_l_2_beneficios" ~ "Benefícios",
      dimensao == "x2_l_3_proposito_do_trabalho_e_da_empresa" ~ "Propósito",
      dimensao == "x2_l_4_flexibilidade_de_trabalho_remoto" ~ "Flexibilidade",
      dimensao == "x2_l_5_ambiente_e_clima_de_trabalho" ~ "Ambiente/Clima",
      dimensao == "x2_l_6_oportunidade_de_aprendizado_e_trabalhar_com_referencias" ~ "Aprendizado",
      dimensao == "x2_l_7_oportunidades_de_crescimento" ~ "Crescimento",
      dimensao == "x2_l_8_maturidade_da_empresa_em_termos_de_tecnologia_e_dados" ~ "Maturidade Tech",
      dimensao == "x2_l_9_relacao_com_os_gestores_e_lideres" ~ "Gestores",
      dimensao == "x2_l_10_reputacao_que_a_empresa_tem_no_mercado" ~ "Reputação"
    )
  ) %>%
  filter(!is.na(satisfacao))

p1 <- ggplot(dados_viz, aes(x = dimensao_label, y = satisfacao, 
                           fill = x2_c_numero_de_funcionarios)) +
  geom_boxplot() +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Satisfação por Dimensão e Tamanho da Empresa",
       x = "Dimensões", y = "Satisfação (1-5)",
       fill = "Tamanho da Empresa") +
  scale_fill_brewer(type = "qual", palette = "Set2")

# Gráfico 2: Satisfação média por dimensão
satisfacao_media <- dados_viz %>%
  group_by(x2_c_numero_de_funcionarios, dimensao_label) %>%
  summarise(satisfacao_media = mean(satisfacao, na.rm = TRUE), .groups = 'drop')

p2 <- ggplot(satisfacao_media, aes(x = dimensao_label, y = satisfacao_media, 
                                  fill = x2_c_numero_de_funcionarios)) +
  geom_col(position = "dodge") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Satisfação Média por Dimensão",
       x = "Dimensões", y = "Satisfação Média",
       fill = "Tamanho da Empresa") +
  scale_fill_brewer(type = "qual", palette = "Set2") +
  geom_hline(yintercept = 3, linetype = "dashed", color = "red", alpha = 0.7)

# ===============================================
# 5. ANÁLISE DE ROTATIVIDADE
# ===============================================

# Indicadores de rotatividade
rotatividade <- dados_tech %>%
  group_by(x2_c_numero_de_funcionarios) %>%
  summarise(
    n = n(),
    perc_entrevistas_6m = mean(x2_m_participou_de_entrevistas_ultimos_6m == "Sim", na.rm = TRUE) * 100,
    perc_planos_mudanca = mean(x2_n_planos_de_mudar_de_emprego_6m == "Sim", na.rm = TRUE) * 100,
    perc_layoff = mean(x2_q_empresa_passou_por_layoff_em_2024 == "Sim", na.rm = TRUE) * 100,
    perc_quer_outra_area = mean(x2_l_11_gostaria_de_trabalhar_em_outra_area == "Sim", na.rm = TRUE) * 100,
    .groups = 'drop'
  )

print("Indicadores de Rotatividade:")
print(rotatividade)

# Gráfico 3: Indicadores de rotatividade
rotatividade_viz <- rotatividade %>%
  select(-n) %>%
  pivot_longer(cols = -x2_c_numero_de_funcionarios, 
               names_to = "indicador", values_to = "percentual") %>%
  mutate(
    indicador_label = case_when(
      indicador == "perc_entrevistas_6m" ~ "Entrevistas nos\núltimos 6 meses",
      indicador == "perc_planos_mudanca" ~ "Planos de\nmudança",
      indicador == "perc_layoff" ~ "Empresa teve\nlayoff em 2024",
      indicador == "perc_quer_outra_area" ~ "Quer trabalhar\nem outra área"
    )
  )

p3 <- ggplot(rotatividade_viz, aes(x = indicador_label, y = percentual, 
                                  fill = x2_c_numero_de_funcionarios)) +
  geom_col(position = "dodge") +
  theme_minimal() +
  labs(title = "Indicadores de Rotatividade por Tamanho de Empresa",
       x = "Indicadores", y = "Percentual (%)",
       fill = "Tamanho da Empresa") +
  scale_fill_brewer(type = "qual", palette = "Set2")

# ===============================================
# 6. ANÁLISE POR NÍVEL DE SENIORIDADE
# ===============================================

if("x2_g_nivel" %in% names(dados_tech_processed)) {
  satisfacao_por_nivel <- dados_tech_processed %>%
    filter(!is.na(x2_g_nivel)) %>%
    group_by(x2_g_nivel, x2_c_numero_de_funcionarios) %>%
    summarise(
      n = n(),
      ambiente_clima = mean(x2_l_5_ambiente_e_clima_de_trabalho, na.rm = TRUE),
      gestores = mean(x2_l_9_relacao_com_os_gestores_e_lideres, na.rm = TRUE),
      maturidade_tech = mean(x2_l_8_maturidade_da_empresa_em_termos_de_tecnologia_e_dados, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    filter(n >= 3)  # Mínimo 3 respostas por grupo
  
  print("Satisfação por Nível de Senioridade:")
  print(satisfacao_por_nivel)
}

# ===============================================
# 7. CORRELAÇÕES ENTRE DIMENSÕES
# ===============================================

# Matriz de correlação
cor_data <- dados_tech_processed %>%
  select(all_of(satisfaction_cols)) %>%
  cor(use = "complete.obs")

# Gráfico de correlação
png("correlacao_satisfacao.png", width = 800, height = 800)
corrplot(cor_data, method = "color", type = "upper", 
         tl.cex = 0.8, tl.col = "black", tl.srt = 45,
         title = "Correlação entre Dimensões de Satisfação")
dev.off()

# ===============================================
# 8. ANÁLISE DE TRABALHO REMOTO
# ===============================================

if(all(c("x2_r_modelo_de_trabalho_atual", "x2_s_modelo_de_trabalho_ideal") %in% names(dados_tech))) {
  trabalho_remoto <- dados_tech %>%
    group_by(x2_c_numero_de_funcionarios) %>%
    summarise(
      n = n(),
      atual_remoto = mean(grepl("remoto|home", x2_r_modelo_de_trabalho_atual, ignore.case = TRUE), na.rm = TRUE) * 100,
      ideal_remoto = mean(grepl("remoto|home", x2_s_modelo_de_trabalho_ideal, ignore.case = TRUE), na.rm = TRUE) * 100,
      .groups = 'drop'
    )
  
  print("Análise de Trabalho Remoto:")
  print(trabalho_remoto)
}

# ===============================================
# 9. PRINCIPAIS MOTIVOS DE INSATISFAÇÃO
# ===============================================

if("x2_l_motivo_insatisfacao" %in% names(dados_tech)) {
  motivos_insatisfacao <- dados_tech %>%
    filter(!is.na(x2_l_motivo_insatisfacao) & x2_l_motivo_insatisfacao != "") %>%
    group_by(x2_c_numero_de_funcionarios, x2_l_motivo_insatisfacao) %>%
    summarise(n = n(), .groups = 'drop') %>%
    group_by(x2_c_numero_de_funcionarios) %>%
    mutate(prop = n/sum(n) * 100) %>%
    arrange(x2_c_numero_de_funcionarios, desc(n))
  
  print("Principais Motivos de Insatisfação:")
  print(motivos_insatisfacao)
}

# ===============================================
# 10. RELATÓRIO RESUMO
# ===============================================

cat("\n" , "="*50, "\n")
cat("RESUMO EXECUTIVO DA ANÁLISE\n")
cat("="*50, "\n")

cat("Amostra Total:", nrow(dados_tech), "profissionais de tech em empresas 1000+ funcionários\n\n")

cat("PRINCIPAIS ACHADOS:\n")
cat("- Distribuição da amostra:\n")
print(table(dados_tech$x2_c_numero_de_funcionarios))

if(nrow(satisfacao_por_tamanho) > 0) {
  cat("\n- Dimensões com MAIOR satisfação:\n")
  top_satisfaction <- satisfacao_por_tamanho %>%
    select(-n) %>%
    pivot_longer(cols = -x2_c_numero_de_funcionarios, names_to = "dimensao", values_to = "score") %>%
    group_by(dimensao) %>%
    summarise(score_medio = mean(score, na.rm = TRUE), .groups = 'drop') %>%
    arrange(desc(score_medio)) %>%
    head(3)
  print(top_satisfaction)
  
  cat("\n- Dimensões com MENOR satisfação:\n")
  bottom_satisfaction <- satisfacao_por_tamanho %>%
    select(-n) %>%
    pivot_longer(cols = -x2_c_numero_de_funcionarios, names_to = "dimensao", values_to = "score") %>%
    group_by(dimensao) %>%
    summarise(score_medio = mean(score, na.rm = TRUE), .groups = 'drop') %>%
    arrange(score_medio) %>%
    head(3)
  print(bottom_satisfaction)
}

# Exibir gráficos
grid.arrange(p1, p2, ncol = 1)
print(p3)

cat("\nAnálise concluída! Verifique os gráficos gerados.\n")


