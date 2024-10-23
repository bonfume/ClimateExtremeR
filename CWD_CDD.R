#Autora: Gabriela Carreiro

# CWD e CDD

## Esse script é destinado para o cálculo dos Índices CWD e CDD, que representam: 
## Link: https://ftp.cptec.inpe.br/pesquisa/grpeta/petamdl/Projetos/Projeta/indices.pdf
## CWD: Número máximo de dias consecutivos com chuva no ano (dias). Este índice reflete a duração de períodos chuvosos.
## CDD: Número máximo de dias consecutivos sem chuva no ano (dias). Este índice reflete a duração de períodos de estiagem.

# Instalar pacotes necessários 
install.packages(c("dplyr", "lubridate", "readr", "ggplot2" ))

# Carregar as bibliotecas necessárias
library(dplyr)
library(lubridate)
library(ggplot2)
library(readr)

# Carregar os dados de um arquivo .txt
dados <- read_delim("C:/Users/bonfu/Downloads/Angra_CHIRPS_teste.txt", delim = " ", col_names = FALSE)

# Renomear as colunas para ano, mês, dia e precipitação
colnames(dados) <- c("year", "month", "day", "precipitation")

# Verificar as primeiras linhas para garantir a leitura correta
print(head(dados))

# Criar uma coluna de data a partir de year, month e day
dados <- dados %>%
  mutate(date = make_date(year, month, day))

# Remover linhas onde a data não pôde ser convertida ou onde a precipitação está faltando
dados <- dados %>%
  filter(!is.na(date))

# Definir o limiar de precipitação para dias chuvosos (WetDay) e secos (DryDay)
WetDay <- 1  # Precipitação >= 1 mm é considerado dia chuvoso
DryDay <- 1  # Precipitação < 1 mm é considerado dia seco

# Identificar dias chuvosos e secos
dados <- dados %>%
  mutate(
    is_wet = precipitation >= WetDay,
    is_dry = precipitation < DryDay
  )

# Função para calcular a maior sequência de dias consecutivos de TRUE e retornar o início da sequência
calcular_maior_sequencia <- function(vetor_logico, datas) {
  rle_result <- rle(vetor_logico)
  
  # Verificar se existem valores TRUE no vetor
  if (any(rle_result$values == TRUE)) {
    duracao_max <- max(rle_result$lengths[rle_result$values == TRUE], na.rm = TRUE)
    indice_inicio <- which(rle_result$lengths == duracao_max & rle_result$values == TRUE)[1]
    
    # Verificar se índice_inicio é válido
    if (!is.na(indice_inicio) && indice_inicio > 1) {
      inicio_sequencia <- sum(rle_result$lengths[1:(indice_inicio - 1)]) + 1
      return(list(duracao = duracao_max, data_inicio = datas[inicio_sequencia]))
    } else {
      return(list(duracao = duracao_max, data_inicio = datas[1]))
    }
  } else {
    return(list(duracao = NA, data_inicio = NA))
  }
}

# Inicializar data frame para armazenar os resultados_cwd_cdd anuais
resultados_cwd_cdd <- data.frame(
  Ano = integer(),
  CWD = integer(),
  Data_Inicio_CWD = as.Date(character()),
  CDD = integer(),
  Data_Inicio_CDD = as.Date(character())
)

# Calcular CWD e CDD para cada ano
anos_unicos <- unique(dados$year)

for (ano in anos_unicos) {
  dados_ano <- filter(dados, year == ano)
  
  # CWD: Consecutive Wet Days (dias chuvosos consecutivos)
  resultado_cwd <- calcular_maior_sequencia(dados_ano$is_wet, dados_ano$date)
  
  # CDD: Consecutive Dry Days (dias secos consecutivos)
  resultado_cdd <- calcular_maior_sequencia(dados_ano$is_dry, dados_ano$date)
  
  # Armazenar os resultados para o ano
  resultados_cwd_cdd <- rbind(resultados_cwd_cdd, data.frame(
    Ano = ano,
    CWD = resultado_cwd$duracao,
    Data_Inicio_CWD = resultado_cwd$data_inicio,
    CDD = resultado_cdd$duracao,
    Data_Inicio_CDD = resultado_cdd$data_inicio
  ))
}

# Exibir os resultados
print(resultados_cwd_cdd)

# Plotar a duração do CWD por ano
ggplot(resultados_cwd_cdd, aes(x = Ano, y = CWD)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(title = "Duração do CWD (Consecutive Wet Days) por Ano", x = "Ano", y = "Dias Chuvosos Consecutivos") +
  theme_minimal()

# Plotar a duração do CDD por ano
ggplot(resultados_cwd_cdd, aes(x = Ano, y = CDD)) +
  geom_bar(stat = "identity", fill = "red") +
  labs(title = "Duração do CDD (Consecutive Dry Days) por Ano", x = "Ano", y = "Dias Secos Consecutivos") +
  theme_minimal()

# Exportar os resultados para CSV
write_csv(resultados_cwd_cdd, "C:/Users/bonfu/Downloads/cwd_cdd_Angra.csv")
