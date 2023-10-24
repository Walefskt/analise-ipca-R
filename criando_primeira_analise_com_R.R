################################################################################
################# Criando a primeira análise de dados com o R ##################

### Pacotes
library(tidyverse)
library(sidrar)

### Coleta
ipca_raw <- sidrar::get_sidra(api = '/t/1737/n1/all/v/2265/p/all/d/v2265%202')

### Limpando
dplyr::glimpse(ipca_raw)

ipca <- ipca_raw |>
  dplyr::select('data' = 'Mês (Código)',
                'ipca' = 'Valor') |>
  dplyr::mutate(data = lubridate::ym(data)) |>
  dplyr::filter(data >= '2004-01-01') |>
  dplyr::as_tibble()

### Análise Exploratória

# Gráfico de linha
ggplot(ipca) +
  aes(x = data, y = ipca) +
  geom_line()

# Os cinco números
summary(ipca)

# Gráfico de boxplot
ggplot(ipca)+
  aes(y = ipca)+
  geom_boxplot()

#Gráfico de histograma
ggplot(ipca)+
  aes(x = ipca)+
  geom_histogram()

### Taxa de desocupação
desocupacao_raw <- sidrar::get_sidra(api = '/t/6381/n1/all/v/4099/p/all/d/v4099%201')

### Limpando
desocupacao <- desocupacao_raw |>
  select('data' = 'Trimestre Móvel (Código)',
         'desocupacao' = 'Valor') |>
  mutate(data = lubridate::ym(data)) |>
  as_tibble()

# Juntando os data frames
df_dados <- ipca |>
  inner_join(desocupacao, by = 'data')

# Comparando as duas variáveis
df_dados |>
  ggplot()+
  aes(x = data)+
  geom_line(aes(y = desocupacao, color = 'Taxa de desocupação'))+
  geom_line(aes(y = ipca, color = 'IPCA'))+
  scale_color_manual(values = c('#282f6b', '#b22200'))

# IPCA em função do desemprego
modelo_phillips <- lm(ipca ~ desocupacao, data = df_dados)

summary(modelo_phillips)
