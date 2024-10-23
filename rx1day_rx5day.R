#Autora: Gabriela Carreiro

# RX1DAY E RX5DAY 

## Esse script é destinado para o cálculo dos Índices RX1day e RX5day, que representam: 
## Link: https://ftp.cptec.inpe.br/pesquisa/grpeta/petamdl/Projetos/Projeta/indices.pdf
### RX1day: Máxima precipitação anual em 1 dia (mm). Precipitação diária mais intensa ocorrida no ano. O índice reflete a intensidade de chuva que pode causar inundações bruscas.
### RX5day: Máxima precipitação anual em 5 dias consecutivos (mm). Precipitação acumulada em 5 dias, máxima ocorrida em um ano. É um indicador para possibilidade de deslizamento.

# Instalar pacotes necessários 
install.packages(c("dplyr", "zoo", "lubridate", "readr", "ggplot2" ))

# Carregar pacotes
library(dplyr)
library(zoo)       # Para calcular a soma em janelas móveis (rolling window)
library(lubridate) # Para manipulação de datas
library(readr)     # Para ler arquivos com delimitadores
library(ggplot2)   # Para criar gráficos 

# Ler o arquivo CSV ou TXT com dados de precipitação
# Como o delimitador é um espaço, usamos read_table() ou read_delim() com o delimitador correto
dados <- read_delim("C:/Users/bonfu/Downloads/Paraty_CHIRPS_limpo.txt", delim = " ", col_names = FALSE)

# Renomear as colunas para algo mais significativo (Ano, Mês, Dia, Precipitação)
colnames(dados) <- c("year", "month", "day", "precipitation")

# Criar uma coluna de data a partir de year, month e day
dados <- dados %>%
  mutate(date = make_date(year, month, day))

# Remover linhas onde a data não pôde ser convertida ou onde a precipitação está faltando
dados <- dados %>%
 filter(!is.na(date))

# Certificar que a coluna de precipitação seja numérica
dados <- dados %>%
  mutate(precipitation = as.numeric(precipitation))

# Verificar a estrutura dos dados
str(dados)

# Função para calcular RX1day (máxima precipitação diária em cada ano)
calculate_rx1day <- function(data) {
  data %>%
    group_by(year) %>%      # Agrupa os dados por ano
    summarise(
      RX1day = max(precipitation, na.rm = TRUE),  # Máxima precipitação em um dia
      RX1day_date = date[which.max(precipitation)] # Data da máxima precipitação
    )
}

# Função para calcular RX5day (máxima precipitação acumulada em 5 dias consecutivos)
calculate_rx5day <- function(data) {
  data %>%
    group_by(year) %>%    # Agrupa os dados por ano
    summarise(
      RX5day = max(rollsum(precipitation, 5, fill = NA, align = "right"), na.rm = TRUE),  # Acumulação máxima de precipitação em 5 dias
      RX5day_date = date[which.max(rollsum(precipitation, 5, fill = NA, align = "right"))] # Data final dos 5 dias com máxima precipitação
    )
}

# Calcular RX1day e RX5day
rx1day_results <- calculate_rx1day(dados)
rx5day_results <- calculate_rx5day(dados)

# Unir os resultados de RX1day e RX5day
resultado_rx1rx5 <- left_join(rx1day_results, rx5day_results, by = "year")

# Visualizar os resultados
print(resultado_rx1rx5)

# Exportar os resultados para CSV
write_csv(resultado_rx1rx5, "C:/Users/bonfu/Downloads/RX1day_RX5day_Paraty.csv")

# Gráfico de RX1day (máxima precipitação diária por ano)
#ggplot(rx1day_results, aes(x = year, y = RX1day)) +
#  geom_line(color = "blue", size = 1) +           # Linha azul
#  geom_point(color = "blue", size = 2) +          # Pontos nos anos
#  labs(title = "Máxima Precipitação Diária (RX1day)", 
#       x = "Ano", y = "RX1day (mm)") +            # Rótulos
#  theme_minimal()                                 # Tema minimalista

# Gráfico de RX5day (máxima precipitação acumulada em 5 dias consecutivos por ano)
#ggplot(rx5day_results, aes(x = year, y = RX5day)) +
#  geom_line(color = "red", size = 1) +            # Linha vermelha
#  geom_point(color = "red", size = 2) +           # Pontos nos anos
#  labs(title = "Máxima Precipitação Acumulada em 5 dias (RX5day)", 
#       x = "Ano", y = "RX5day (mm)") +            # Rótulos
#  theme_minimal()                                 # Tema minimalista


