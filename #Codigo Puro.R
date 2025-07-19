#Instalação e Inicialização dos Pacotes
library(tidyverse) #Para manipulação de dados
library(scales) #Para formatação de números
library(janitor) #Para limpeza de nomes de colunas
install.packages("reactablefmtr")
library(reactable) #Para tabelas mais bonitas e interativas.
library(reactablefmtr)
library(stringr)
#Bancos de Dados
br2019 <- read.csv("/kaggle/input/data2025/df2019.csv") %>% janitor::clean_names()
br2021 <- read.csv("/kaggle/input/data2025/df2021.csv") %>%  janitor::clean_names()
br2022 <- read.csv("/kaggle/input/data2025/df2022.csv") %>%  janitor::clean_names()
br2023 <- read.csv("/kaggle/input/data2025/df2023.csv") %>%  janitor::clean_names()
br2024 <- read.csv("/kaggle/input/data2025/df2024.csv") %>%  janitor::clean_names()

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
