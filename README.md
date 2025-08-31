# Conectando Dados em Primeira Pessoa

## Description
Este projeto é uma refatoração do código original do desafio "Dados em Primeira Pessoa" da comunidade Data Hackers. O objetivo principal é organizar e otimizar o código, melhorar a acessibilidade visual dos gráficos e aprofundar o entendimento das técnicas de análise de dados, utilizando os dados das pesquisas State of Data Brazil de 2019 e 2021 a 2024. A análise busca responder a perguntas frequentes da comunidade brasileira de dados com base em evidências empíricas.

## Data Source
Os dados utilizados nesta análise são provenientes das edições de 2019, 2021, 2022, 2023 e 2024 da pesquisa anual State of Data Brazil. Esses conjuntos de dados contêm informações valiosas sobre o perfil dos profissionais de dados no Brasil, incluindo faixas salariais, tecnologias utilizadas, níveis de experiência e satisfação profissional.

## Analysis Steps
O projeto segue as seguintes etapas de análise:
1.  **Carregamento e Preparação dos Dados:** Os datasets de cada ano são carregados e padronizados utilizando a função `clean_names` do pacote `janitor`. Dados de diferentes anos são combinados e tratados para análises comparativas.
2.  **Análise Salarial:** Exploração da distribuição salarial ao longo dos anos através de tabelas interativas (`reactable`) e gráficos de barras agrupadas (`ggplot2`).
3.  **Heatmaps Salariais:** Visualização da distribuição salarial por nível de experiência e tamanho da empresa em 2024, e a evolução dessas distribuições ao longo dos anos utilizando heatmaps.
4.  **Generalista vs. Especialista:** Classificação dos respondentes em perfis generalista ou especialista com base em múltiplos critérios (função, cargo, atividades, formação, etc.). Análise comparativa da distribuição salarial, senioridade, cargos típicos e representação no mercado entre esses perfis.
5.  **Análise de Uso de Python:** Investigação do percentual de uso da linguagem Python por cargo, experiência em dados e faixa etária, utilizando gráficos de barras empilhadas e gráficos de linha.
6.  **Perfil do Cientista de Dados Júnior:** Identificação dos conhecimentos teóricos, responsabilidades, linguagens, bancos de dados, ferramentas de BI e plataformas de nuvem mais comuns entre Cientistas de Dados com nível júnior.
7.  **Análise de Background de Cientistas de Dados:** Comparação da distribuição salarial e de senioridade entre Cientistas de Dados com background em Estatística/Matemática versus Computação/TI.
8.  **Análise de Correlação:** Cálculo e visualização da matriz de correlação entre variáveis numéricas e categóricas mapeadas para identificar relacionamentos entre diferentes aspectos da carreira em dados.

## Key Findings
Alguns dos principais insights obtidos com a análise incluem:
*   Houve uma evolução nas faixas salariais ao longo dos anos, com maior concentração em faixas intermediárias e superiores em anos mais recentes.
*   Profissionais classificados como **Especialistas** tendem a ocupar faixas salariais mais altas e ter maior presença em níveis de senioridade Pleno e Sênior em comparação com os **Generalistas**.
*   O uso de **Python** é predominante em cargos como Cientista de Dados e Engenheiro de Dados, e o percentual de uso aumenta significativamente com o tempo de experiência na área de dados.
*   Cientistas de Dados **Júnior** focam em atividades como coleta e limpeza de dados e desenvolvimento de ML para produção, utilizando principalmente **Python**, **SQL**, **Databricks**, **PostgreSQL** e **Microsoft PowerBI**.
*   Cientistas de Dados com background em **Computação/TI** representam a maioria (cerca de 80%) e tendem a ter maior proporção em níveis de senioridade mais altos (Sênior) em comparação com aqueles com background em **Estatística/Matemática**, embora a distribuição salarial pareça similar entre os grupos de Plenos e Sêniores.
*   A matriz de correlação revela relações positivas entre experiência, faixa salarial e o uso de certas ferramentas/linguagens.

