options(scipen = 999)
library(ordinal)
library(tidyverse)
library(pomcheckr)
library(gofcat)
library(sjPlot)


# Datos -------------------------------------------------------------------

data(wine)
summary(wine)

# Distribución de los ratings
wine %>% 
  ggplot(aes(x = rating)) +
  geom_bar()

# Distribución de los ratings según temperatura
wine %>% 
  count(rating, temp) %>% 
  ggplot(aes(x = rating, y = temp, fill = n)) +
  geom_tile()




# Modelo ------------------------------------------------------------------


# Ajustamos el modelo usando la funcion clm 
# modelo <- clm(respuesta ~ predictores, data = dataframe)

modelo <- clm(rating ~ temp, data = wine)
summary(modelo)
confint(modelo)
?wine

# Coeficientes ------------------------------------------------------------

# Obtenemos los OR (odds ratio)
exp(modelo$beta)

sjPlot::tab_model(modelo, file = "vino1.doc")

sjPlot::plot_model(modelo)

# Supuestos ---------------------------------------------------------------


# Analisis del supuesto de odds proporcionales
nominal_test(modelo)
brant.test(modelo)

################################## Contacto ####################
# Distribución de los ratings
wine %>% 
  ggplot(aes(x = rating)) +
  geom_bar()

# Distribución de los ratings según temperatura
wine %>% 
  count(rating, contact) %>% 
  ggplot(aes(x = rating, y = contact, fill = n)) +
  geom_tile()




# Modelo ------------------------------------------------------------------


# Ajustamos el modelo usando la funcion clm 
# modelo <- clm(respuesta ~ predictores, data = dataframe)

modelo2 <- clm(rating ~ contact, data = wine)
summary(modelo2)
confint(modelo2)
?wine

# Coeficientes ------------------------------------------------------------

# Obtenemos los OR (odds ratio)
exp(modelo2$beta)

sjPlot::tab_model(modelo2, file = "vino2.doc")

sjPlot::plot_model(modelo2)

# Supuestos ---------------------------------------------------------------


# Analisis del supuesto de odds proporcionales
nominal_test(modelo2)
brant.test(modelo2)

#################
# Distribución de los ratings
wine %>% 
  ggplot(aes(x = rating)) +
  geom_bar()

# Distribución de los ratings según temperatura
wine %>% 
  count(rating, temp) %>% 
  ggplot(aes(x = rating, y = temp, fill = n)) +
  geom_tile()




# Modelo ------------------------------------------------------------------


# Ajustamos el modelo usando la funcion clm 
# modelo <- clm(respuesta ~ predictores, data = dataframe)

modelo3 <- clm(rating ~ temp + contact, data = wine)
summary(modelo3)
confint(modelo3)
?wine

# Coeficientes ------------------------------------------------------------

# Obtenemos los OR (odds ratio)
exp(modelo3$beta)

sjPlot::tab_model(modelo3, file = "vino3.doc")

sjPlot::plot_model(modelo3)

# Supuestos ---------------------------------------------------------------


# Analisis del supuesto de odds proporcionales
nominal_test(modelo3)
brant.test(modelo)

######## Datos nuevos ####
# Datos -------------------------------------------------------------------

data(ologit)
summary(ologit)
?ologit
# Distribución de los ratings
ologit %>% 
  ggplot(aes(x = apply)) +
  geom_bar()

# Distribución de los ratings según temperatura
ologit %>% 
  count(apply, pared) %>% 
  ggplot(aes(x = apply, y = pared, fill = n)) +
  geom_tile()




# Modelo ------------------------------------------------------------------


# Ajustamos el modelo usando la funcion clm 
# modelo <- clm(respuesta ~ predictores, data = dataframe)

modelo4 <- clm(apply ~ pared + public, data = ologit)
summary(modelo4)
confint(modelo4)
?wine

# Coeficientes ------------------------------------------------------------

# Obtenemos los OR (odds ratio)
exp(modelo4$beta)

sjPlot::tab_model(modelo, file = "vino1.doc")

sjPlot::plot_model(modelo4)

# Supuestos ---------------------------------------------------------------


# Analisis del supuesto de odds proporcionales
nominal_test(modelo4)
brant.test(modelo)
