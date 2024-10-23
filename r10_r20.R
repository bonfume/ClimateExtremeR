#Autora: Gabriela Carreiro

# R10mm e R20mm

## Esse script é destinado para o cálculo dos Índices R10mm e R20mm, que representam: 
## Link: https://ftp.cptec.inpe.br/pesquisa/grpeta/petamdl/Projetos/Projeta/indices.pdf
## R10mm: Número de dias no ano com RR >= 10 mm (dias). Este índice reflete frequência de eventos de chuva >10mm/dia.
## R20mm: Número de dias no ano com RR >= 20 mm (dias). Este índice reflete frequência de eventos de chuva >20mm/dia.

# Instalar pacotes necessários 
install.packages(c("dplyr", "lubridate", "readr", "ggplot2" ))

# Carregar as bibliotecas necessárias
library(dplyr)
library(lubridate)
library(readr)
library(ggplot2)

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

# Definir os limiares para R10 e R20
limiar_r10 <- 10  # Precipitação > 10 mm
limiar_r20 <- 20  # Precipitação > 20 mm

# Função para calcular R10 e R20 por ano
calcular_r10_r20 <- function(precip_data, limiar) {
  # Retorna o número de dias com precipitação superior ao limiar
  return(sum(precip_data > limiar, na.rm = TRUE))
}

# Inicializar data frame para armazenar os resultados_r10_r20 anuais
resultados_r10_r20 <- data.frame(
  Ano = integer(),
  R10 = integer(),
  R20 = integer()
)

# Identificar os anos únicos no conjunto de dados
anos_unicos <- unique(dados$year)

# Calcular R10 e R20 para cada ano
#for (ano in anos_unicos) {
#  dados_ano <- filter(dados, year == ano)
  
  # Calcular R10 (dias com precipitação > 10 mm)
#  r10 <- calcular_r10_r20(dados_ano$precipitation, limiar_r10)
  
  # Calcular R20 (dias com precipitação > 20 mm)
#  r20 <- calcular_r10_r20(dados_ano$precipitation, limiar_r20)
  
  # Armazenar os resultados para o ano
#  resultados_r10_r20 <- rbind(resultados_r10_r20, data.frame(
#    Ano = ano,
#    R10 = r10,
#    R20 = r20
#  ))
#}

# Filtrar os dias com precipitação acima de 10 mm (R10) e 20 mm (R20)
dias_r10 <- dados %>%
  filter(precipitation > limiar_r10)

dias_r20 <- dados %>%
  filter(precipitation > limiar_r20)

# Exibir os primeiros dias de precipitação acima de 10 mm
print("Dias com precipitação > 10 mm:")
print(head(dias_r10))

# Exibir os primeiros dias de precipitação acima de 20 mm
print("Dias com precipitação > 20 mm:")
print(head(dias_r20))

# Adicionar colunas para identificar se o dia é R10 ou R20
dados_unificado <- dados %>%
  mutate(
    R10 = ifelse(precipitation > limiar_r10, 1, 0),
    R20 = ifelse(precipitation > limiar_r20, 1, 0)
  )

# Filtrar apenas os dias que atendem a pelo menos um dos limiares (R10 ou R20)
dados_unificado_filtrado <- dados_unificado %>%
  filter(R10 == 1 | R20 == 1)

# Exibir os primeiros registros dos dias filtrados
print("Dias com precipitação > 10 mm ou > 20 mm:")
print(head(dados_unificado_filtrado))

# Exportar os resultados para um único CSV
write_csv(dados_unificado_filtrado, "C:/Users/bonfu/Downloads/R10_R20_Paraty.csv")


