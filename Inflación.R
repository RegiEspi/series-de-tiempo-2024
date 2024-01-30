library(tidyverse)
library(fable)
library(tsibble)
library(readxl)
library(readr)
library(ggplot2)

data=read_csv("Indicadores.csv")
head(data)

as_tsibble(data) %>% 
  autoplot(`Precios de Inflacion`)+
  labs(y ="mxn",title ="Inflacion 2008-2023")

