library(tidyverse)
library(fable)
library(tsibble)
library(readxl)
library(readr)
library(ggplot2)
install.packages(c( "feasts"))
library(dplyr)
install.packages("plotly")
library(plotly)
install.packages("e1071")
library(e1071)
install.packages("ggExtra")
library(ggExtra)
install.packages("EnvStats")
library(EnvStats)
install.packages("imputeTS")
library(imputeTS)
install.packages(tidyquant)
library(tidyquant)

data=read_csv("Indicadores.csv")
head(data)
data <- data %>% rename(Precios = "Precios de Inflacion")
data$Periodos = yearmonth(data$Periodos)
as_tsibble(data) %>% 
  autoplot(`Precios`)+
  labs(y ="MXN", x= "Fecha",title ="Inflacion 2008-2023")

head(data)

print(paste('Inflacion menor: ', min(data$Precios)))
print(paste('Inflacion máxima: ', max(data$Precios)))
summary(data[, 'Precios'])

boxplot = data %>% 
  mutate(year = year(Periodos)) %>% 
  ggplot(aes(x = as.factor(year), y = Precios)) + 
  geom_boxplot() + 
  xlab('Periodo') + 
  ylab('Inflación')

ggplotly(boxplot)

sd(data$Precios)
var(data$Precios)

skewness(data$Precios)

p <- ggplot(data, aes(x=Periodos, y=Precios)) + 
  geom_hline(yintercept =2) + 
  geom_hline(yintercept =6) +
  geom_point() + 
  ggtitle('Inflacion por mes') + ylab('Mxn') + xlab('Periodos')

ggMarginal(p, type='histogram', margins = 'y')

histogram = ggplot(data, aes(x = Precios)) +
  geom_histogram( bins = 20, fill = "black", color = "black", alpha = 0.5) +
  labs(title = "Histograma",
       x = "Precios",
       y = "Densidad")

ggplotly(histogram)

p <- data %>% as_tibble %>% group_by(years=year(Periodos)) %>%
  summarise(Inflacion=sum(Precios)) %>%
  arrange(desc(years))%>%
  mutate(change = (Inflacion/lead(Inflacion) - 1) * 100) %>% 
  filter(years > 2008) %>% 
  filter(years < 2023)

mean_growth <- data %>% as_tibble %>% group_by(years=year(Periodos)) %>%
  summarise(Inflacion=sum(Precios)) %>%
  arrange(desc(years))%>%
  mutate(change = (Inflacion/lead(Inflacion) - 1) * 100) %>% 
  filter(years > 2008) %>% 
  filter(years < 2023) %>%
  summarise(mean(change))

mean_growth <- mean_growth$`mean(change)`

ggplot(p, aes(x=years, y=change)) +
  geom_line() +
  geom_hline(yintercept=mean_growth) +
  geom_hline(yintercept=0) +
  ggtitle('Cambio porcentual por año') + ylab('%') + xlab('Mes')

data = as_tsibble(data, index=Periodos, regular=TRUE)
head(data, 2)

train <- data %>% select(Precios) %>% filter_index("2008 Jan " ~ "2023 Jan")
test <- data %>% select(Precios) %>% filter_index("2023 Jan " ~ "2023 Dec ")
tstng_prds <- 12
frcst_prds <- 12


models_fit <- train %>% 
  model(`Seasonal naive` = SNAIVE(Precios))
models_tst <- models_fit %>% forecast(h = tstng_prds)
mape_sn <- (models_fit %>% forecast(h = tstng_prds) %>% accuracy(test))$MAPE
snaive_plot <- models_tst %>% autoplot(filter_index(data, "2015 Jan" ~ .)) +
  ggtitle('Seasonal Naive') + ylab('Inflacion') + xlab('Mes')

snaive_plot

qqnorm(data$Precios)
qqline(data$Precios)

bc <- EnvStats::boxcox(data$Precios, lambda=c(-2, 2), optimize=TRUE, objective.name='Log-Likelihood')
bc_data <- EnvStats::boxcoxTransform(data$Precios, bc$lambda)

skewness(bc_data)
qqnorm(bc_data)
qqline(bc_data)

data <- data %>% mutate('Inflacion_trn' = bc_data)
stl_model = data %>% dplyr::select(Inflacion_trn) %>% stl(s.window = 'per')
plot(stl_model,main = 'Descomposicón de la serie con STL')

datos=read_csv("Circulacion.csv")
head(datos)
datos$Fecha = yearmonth(datos$Fecha)
datos = as_tsibble(datos, index=Fecha, regular=TRUE)

train_gdp <- datos %>% select(Circulacion) %>% filter_index("2008 Jan" ~ "2023 Jan")
test_gdp <- datos %>% select(Circulacion) %>% filter_index("2023 Jan" ~ "2023 Dec")

train_gdp %>% ggplot(aes(x = Circulacion, y = log(value))) +
  labs(y = "Inflacion)",
       x = "Circulacion)") +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)