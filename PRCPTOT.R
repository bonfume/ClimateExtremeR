#Autora: Gabriela Carreiro

# PRCPTOT

## Esse script é destinado para o cálculo do Índice PRCPTOT, que representam: 
## Link: https://ftp.cptec.inpe.br/pesquisa/grpeta/petamdl/Projetos/Projeta/indices.pdf
## PRCPTOT: Precipitação anual total dos dias úmidos (mm). Fornece a precipitação acumulada durante o ano.

# Instalar pacotes necessários 
install.packages(c("dplyr", "lubridate", "readr", "ggplot2"))

# Carregar as bibliotecas necessárias
library(dplyr)
library(lubridate)
library(readr)
library(ggplot2)

# Carregar os dados de um arquivo .txt
dados <- read_delim("C:/Users/bonfu/Downloads/Paraty_CHIRPS_limpo.txt", delim = " ", col_names = FALSE)

# Renomear as colunas para ano, mês, dia e precipitação
colnames(dados) <- c("year", "month", "day", "precipitation")

# Criar uma coluna de data a partir de year, month e day
dados <- dados %>%
  mutate(date = make_date(year, month, day))

# Calcular o PRCPTOT para cada ano
prcptot <- dados %>%
  filter(precipitation > 0) %>%  # Filtrar dias chuvosos
  group_by(year) %>%             # Agrupar por ano
  summarise(PRCPTOT = sum(precipitation, na.rm = TRUE))  # Somar a precipitação dos dias chuvosos

# Imprimir os resultados
print(prcptot)

# Criar um gráfico para visualizar o PRCPTOT ao longo dos anos
ggplot(prcptot, aes(x = year, y = PRCPTOT)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(title = "Total de Precipitação em Dias Chuvosos (PRCPTOT) por Ano",
       x = "Ano",
       y = "Total de Precipitação (mm)") +
  theme_minimal() +
  scale_y_continuous(limits = c(0, max(prcptot$PRCPTOT, na.rm = TRUE) * 1.1)) # Ajustar limites do eixo y

# Exportar os resultados para CSV
write_csv(prcptot, "C:/Users/bonfu/Downloads/PRCPTOT_Paraty.csv")
