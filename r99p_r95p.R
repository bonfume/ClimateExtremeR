#Autora: Gabriela Carreiro

# R95p e R99p

## Esse script é destinado para o cálculo dos Índices R95p e R99p, que representam: 
## Link: https://ftp.cptec.inpe.br/pesquisa/grpeta/petamdl/Projetos/Projeta/indices.pdf
## R95p: Precipitação anual total dos dias em que RR > percentil 95 (mm). Indica precipitação de intensidade extrema.
## R99p: Precipitação anual total dos dias em que RR > percentil 99 (mm). Indica precipitação de intensidade muito extrema.

# Instalar pacotes necessários 
install.packages(c("dplyr", "lubridate", "readr"))

# Carregar as bibliotecas necessárias
library(dplyr)
library(lubridate)
library(readr)

# Carregar os dados de um arquivo .txt
dados <- read_delim("C:/Users/bonfu/Downloads/Paraty_CHIRPS_limpo.txt", delim = "\t", col_names = FALSE)

# Renomear as colunas para ano, mês, dia e precipitação
colnames(dados) <- c("year", "month", "day", "precipitation")

# Criar uma coluna de data a partir de year, month e day
dados <- dados %>%
  mutate(date = make_date(year, month, day))

# Remover linhas onde a data não pôde ser convertida ou onde a precipitação está faltando
dados <- dados %>%
  filter(!is.na(date))

# Filtrar dias chuvosos (precipitação > 1 mm)
dias_chuvosos <- dados %>%
  filter(precipitation > 1)

# Calcular os percentis 95% (R95p) e 99% (R99p) da precipitação
limiar_r95p <- quantile(dias_chuvosos$precipitation, 0.95, na.rm = TRUE)
limiar_r99p <- quantile(dias_chuvosos$precipitation, 0.99, na.rm = TRUE)

print(paste("Limiar 95º Percentil (R95p):", limiar_r95p, "mm"))
print(paste("Limiar 99º Percentil (R99p):", limiar_r99p, "mm"))

# Função para calcular R95p e R99p por ano
calcular_r95p_r99p <- function(precip_data, limiar) {
  # Retorna a quantidade total de precipitação acima do limiar
  return(sum(precip_data[precip_data > limiar], na.rm = TRUE))
}

# Inicializar data frame para armazenar os resultados_r95p_r99p anuais
resultados_r95p_r99p <- data.frame(
  Ano = integer(),
  R95p = numeric(),
  R99p = numeric()
)

# Identificar os anos únicos no conjunto de dados
anos_unicos <- unique(dados$year)

# Calcular R95p e R99p para cada ano
for (ano in anos_unicos) {
  dados_ano <- filter(dados, year == ano)
  
  # Calcular R95p (precipitação total acima do 95º percentil)
  r95p <- calcular_r95p_r99p(dados_ano$precipitation, limiar_r95p)
  
  # Calcular R99p (precipitação total acima do 99º percentil)
  r99p <- calcular_r95p_r99p(dados_ano$precipitation, limiar_r99p)
  
  # Armazenar os resultados para o ano
  resultados_r95p_r99p <- rbind(resultados_r95p_r99p, data.frame(
    Ano = ano,
    R95p = r95p,
    R99p = r99p
  ))
}

# Exibir os resultados
print(resultados_r95p_r99p)

# Plotar os índices R95p e R99p por ano
library(ggplot2)

# Gráfico para R95p
ggplot(resultados_r95p_r99p, aes(x = Ano, y = R95p)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(title = "Precipitação anual total dos dias em que RR > percentil 95 (mm)", x = "Ano", y = "R95p (mm)") +
  theme_minimal()

# Gráfico para R99p
ggplot(resultados_r95p_r99p, aes(x = Ano, y = R99p)) +
  geom_bar(stat = "identity", fill = "red") +
  labs(title = "Precipitação anual total dos dias em que RR > percentil 99 (mm)", x = "Ano", y = "R99p (mm)") +
  theme_minimal()

# Exportar os resultados para CSV
write_csv(resultados_r95p_r99p, "C:/Users/bonfu/Downloads/R95p_R99p_Paraty.csv")
