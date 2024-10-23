#Autora: Gabriela Carreiro

# PRCPTOT

## Esse script é destinado para o cálculo do Índice PRCPTOT, que representam: 
## Link: https://ftp.cptec.inpe.br/pesquisa/grpeta/petamdl/Projetos/Projeta/indices.pdf
## Anomalia de Precipitação: Precipitação anual total dos dias úmidos (mm). Fornece a precipitação acumulada durante o ano.

# Instalar pacotes necessários 
install.packages(c("dplyr", "openxlsx"))

# Carregar bibliotecas necessárias
library(openxlsx)
library(dplyr)

# Função para calcular a norma climática (média e desvio padrão)
calcular_norma_climatica_txt <- function(precipitacao_mensal) {
  # Calcular a média e o desvio padrão da precipitação histórica mensal
  media_norma <- mean(precipitacao_mensal, na.rm = TRUE) # Ignorar valores NA
  dp_norma <- sd(precipitacao_mensal, na.rm = TRUE)
  
  return(list(media = media_norma, desvio_padrao = dp_norma))
}

# Função para calcular anomalia
calcular_anomalia_txt <- function(precipitacao_atual, media_norma) {
  # Calcular a anomalia (diferença entre o valor atual e a média da norma)
  anomalia <- precipitacao_atual - media_norma
  return(anomalia)
}

# Função para exportar resultados em Excel
exportar_para_excel_txt <- function(municipio, media_norma, dp_norma, anomalias, save_dir) {
  # Criar data frame com os resultados
  resultados <- data.frame(
    Mes = 1:length(anomalias),  # Assumindo que cada linha representa um mês
    Media_Norma = rep(media_norma, length(anomalias)),
    DP_Norma = rep(dp_norma, length(anomalias)),
    Anomalia = anomalias
  )
  
  # Criar e salvar arquivo Excel
  excel_file <- paste0(save_dir, "resultados_anomalia_", municipio, ".xlsx")
  write.xlsx(resultados, excel_file, overwrite = TRUE)  # Salvar com overwrite = TRUE
  
  cat("Resultados exportados para Excel para o município", municipio, ".\n")
}

# Função principal para processar os dados do município a partir de um arquivo TXT
processar_dados_municipio <- function() {
  
  # DEFINIR CAMINHOS
  # Defina o caminho do arquivo TXT contendo os dados de precipitação (ALTERE)
  arquivo_txt <- "C:/Users/bonfu/Downloads/Paraty_CHIRPS_limpo.txt"
  
  # Defina o diretório para salvar os resultados (ALTERE)
  save_dir <- "C:/Users/bonfu/Downloads"   
  
  
  # Espera-se que o arquivo tenha 4 colunas sem cabeçalho: ano, mês, dia e precipitação
  dados_txt <- read.table(arquivo_txt, header = FALSE)
  
  # Nomear as colunas do arquivo
  colnames(dados_txt) <- c("Ano", "Mes", "Dia", "Precipitacao")
  
  
  # Nome do município para ser usado nos arquivos de saída (ALTERE)
  municipio <- "teste"  
  
  # FILTRAR PERÍODOS 
  # Filtrar para o período de norma climática: 1981-2020
  dados_historicos <- dados_txt %>%
    filter(Ano >= 1981 & Ano <= 2020)
  
  # Filtrar para o período de análise da anomalia: 01 de setembro de 2022 a 30 de setembro de 2024
  dados_atual <- dados_txt %>%
    filter((Ano == 2022 & Mes >= 9) | (Ano == 2023) | (Ano == 2024 & Mes <= 9))
  
  # AGREGAR PRECIPITAÇÃO MENSAL 
  # Agrupar os dados por Ano e Mes e somar a precipitação diária para obter precipitação mensal
  precipitacao_mensal_historica <- dados_historicos %>%
    group_by(Ano, Mes) %>%
    summarise(Precipitacao_Mensal = sum(Precipitacao, na.rm = TRUE))
  
  precipitacao_mensal_atual <- dados_atual %>%
    group_by(Ano, Mes) %>%
    summarise(Precipitacao_Mensal = sum(Precipitacao, na.rm = TRUE))
  
  # Extrair apenas os valores de precipitação histórica
  precipitacao_historica <- precipitacao_mensal_historica %>% pull(Precipitacao_Mensal)
  
  # Extrair apenas os valores de precipitação atual (período de 2022-2024)
  precipitacao_atual <- precipitacao_mensal_atual %>% pull(Precipitacao_Mensal)
  
  # CÁLCULOS 
  # Calcular a norma climática (média e desvio padrão) para a precipitação histórica
  norma_climatica <- calcular_norma_climatica_txt(precipitacao_historica)
  
  # Calcular as anomalias (diferença entre a precipitação atual e a norma)
  anomalias <- calcular_anomalia_txt(precipitacao_atual, norma_climatica$media)
  
  # EXPORTAR RESULTADOS 
  # Exportar os resultados para Excel
  exportar_para_excel_txt(municipio, norma_climatica$media, norma_climatica$desvio_padrao, anomalias, save_dir)
  
  cat("Processamento concluído para o município:", municipio, "\n")
}

# Executar o processamento para um município
processar_dados_municipio()
