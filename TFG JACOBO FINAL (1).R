install.packages("readxl")
install.packages("dplyr")
install.packages("lubridate")
install.packages("ggplot2")

library(readxl)
library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyr)

remove.packages("cli")
install.packages("cli")

# Especifica las rutas de los archivos
file_path_clientes <- "/Users/jacobogrossfernandez/Downloads/TFG Usuarios.xlsx"
file_path_pedidos <- "/Users/jacobogrossfernandez/Downloads/TFG Pedidos.xlsx"

# Leer la primera hoja del archivo Excel
TFG_Usuarios <- read_excel(file_path_clientes)
TFG_Pedidos <- read_excel(file_path_pedidos)

# Verificar nombres de las columnas
colnames(TFG_Pedidos)
colnames(TFG_Usuarios)

# Mostrar las primeras filas de cada DataFrame
head(TFG_Pedidos)
head(TFG_Usuarios)

# Información general de cada DataFrame
str(TFG_Pedidos)
str(TFG_Usuarios)

# Vector con los nombres de las columnas relevantes para TFG_Usuarios
relevant_columns_usuarios <- c("ID", "Fecha_registro", "Planes", "Puntos", 
                               "Teléfono móvil", "Tipo_de_cuenta", "Patrocinador", 
                               "Sexo", "Fecha_nacimiento", "Pedidos", 
                               "Primer pedido", "Último_pedido", "Dirección", 
                               "Código_Postal", "Población", "Provincia", "País")


# Seleccionar solo las columnas relevantes en el DataFrame TFG_Usuarios
TFG_Usuarios <- TFG_Usuarios %>% select(one_of(relevant_columns_usuarios))


# Verificar las primeras filas del DataFrame resultante
head(TFG_Usuarios)
str(TFG_Usuarios)

# Vector con los nombres de las columnas relevantes para TFG_Pedidos
relevant_columns_pedidos <- c("Orden_de_pago", "Fecha", "Estado", "Total_ofertas", 
                              "Saldo_usado", "Descuento", "Total_pagado", 
                              "Comisión(%)", "Comisión", "Margen", "Cantidad", 
                              "Nombre", "Comprador", "Plataforma_de_pago")

# Seleccionar solo las columnas relevantes en el DataFrame TFG_Pedidos
TFG_Pedidos <- TFG_Pedidos %>% select(one_of(relevant_columns_pedidos))

# Verificar las primeras filas del DataFrame resultante
head(TFG_Pedidos)
str(TFG_Pedidos)

# Anonimizar datos sensibles
TFG_Usuarios <- TFG_Usuarios %>%
  mutate(`Teléfono móvil` = ifelse(`Teléfono móvil` != "", "****", ""),
         Dirección = ifelse(Dirección != "", "****", ""))

# Convertir las fechas al formato Date
TFG_Usuarios <- TFG_Usuarios %>%
  mutate(Fecha_registro = as.Date(Fecha_registro, format="%Y-%m-%d"),
         Fecha_nacimiento = as.Date(Fecha_nacimiento, format="%Y-%m-%d"),
         `Primer pedido` = as.Date(`Primer pedido`, format="%Y-%m-%d"),
         Último_pedido = as.Date(Último_pedido, format="%Y-%m-%d"))

TFG_Pedidos <- TFG_Pedidos %>%
  mutate(Fecha = as.Date(Fecha, format="%Y-%m-%d"))



# Manejo de valores NA
TFG_Usuarios <- TFG_Usuarios %>%
  mutate(across(where(is.character), ~ replace_na(.x, "No especificado")),
         across(where(is.numeric), ~ replace_na(.x, 0)))

# Verificar los tipos de datos de las columnas
str(TFG_Usuarios)

# Convertir las fechas al formato Date
TFG_Usuarios <- TFG_Usuarios %>%
  mutate(Fecha_nacimiento = as.Date(Fecha_nacimiento, format="%d/%m/%Y"))

# Verificar de nuevo las primeras filas y el resumen
head(TFG_Usuarios$Fecha_nacimiento)
summary(TFG_Usuarios$Fecha_nacimiento)

# Contar cuántas celdas no están vacías en la columna Fecha_nacimiento
non_na_birth_dates <- sum(!is.na(TFG_Usuarios$Fecha_nacimiento))
total_birth_dates <- nrow(TFG_Usuarios)
percentage_filled_birth_dates <- (non_na_birth_dates / total_birth_dates) * 100

# Mostrar la cantidad y el porcentaje de celdas rellenas
non_na_birth_dates
percentage_filled_birth_dates

# Definir un rango razonable para las fechas de nacimiento
fecha_minima_valida <- as.Date("1923-01-01")
fecha_maxima_valida <- Sys.Date()  # La fecha actual

# Filtrar fechas de nacimiento no válidas********
TFG_Usuarios <- TFG_Usuarios %>%
  filter(Fecha_nacimiento >= fecha_minima_valida & Fecha_nacimiento <= fecha_maxima_valida | is.na(Fecha_nacimiento))

# Verificar las primeras filas para asegurarnos de que la columna Fecha_nacimiento se ha filtrado correctamente
head(TFG_Usuarios$Fecha_nacimiento)

# Crear una columna de Edad a partir de la fecha de nacimiento
TFG_Usuarios <- TFG_Usuarios %>%
  mutate(Edad = as.integer(format(Sys.Date(), "%Y")) - as.integer(format(Fecha_nacimiento, "%Y")))

# Filtrar edades no válidas (por ejemplo, menos de 10 años) en un nuevo dataframe
TFG_Usuarios_edad_valida <- TFG_Usuarios %>%
  filter(Edad > 10)

# Verificar las primeras filas para asegurarnos de que la columna Edad se ha creado correctamente
head(TFG_Usuarios$Edad)

# Crear un histograma de la distribución de la edad
ggplot(TFG_Usuarios_edad_valida, aes(x = Edad)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Distribución de la Edad de los Clientes", x = "Edad", y = "Frecuencia") +
  theme_minimal()

#PLATAFORMA DE PAGO
# Filtrar los datos para incluir solo BIZUM y REDSYS
TFG_Pedidos <- TFG_Pedidos %>%
  filter(Plataforma_de_pago %in% c("BIZUM", "REDSYS"))

# Verificar las categorías restantes
unique(TFG_Pedidos$Plataforma_de_pago)

# Calcular la frecuencia de cada método de pago
pago_frecuencia <- TFG_Pedidos %>%
  count(Plataforma_de_pago)

# Calcular la proporción
pago_frecuencia <- pago_frecuencia %>%
  mutate(proporcion = n / sum(n) * 100)

# Mostrar la tabla con frecuencia y proporción
print(pago_frecuencia)

# Crear un gráfico de barras para la plataforma de pago
ggplot(pago_frecuencia, aes(x = Plataforma_de_pago, y = n, fill = Plataforma_de_pago)) +
  geom_bar(stat = "identity", color = "black", alpha = 0.7) +
  geom_text(aes(label = paste0(round(proporcion, 1), "%")), vjust = -0.5, color = "black", size = 3.5) +
  labs(title = "Distribución de las plataformas de pago utilizadas por los clientes", 
       x = "Plataforma de pago", 
       y = "Número de pedidos") +
  theme_minimal() +
  scale_fill_manual(values = c("BIZUM" = "blue", "REDSYS" = "red"))



#VARIABLE ESTADO
# Verificar todos los valores únicos en la columna Estado
unique(TFG_Pedidos$Estado)

# Calcular la frecuencia de cada estado
estado_frecuencia <- TFG_Pedidos %>%
  count(Estado)

# Mostrar la tabla con la frecuencia de cada estado
print(estado_frecuencia)

# Crear un gráfico de barras para el estado de los pedidos
ggplot(estado_frecuencia, aes(x = Estado, y = n, fill = Estado)) +
  geom_bar(stat = "identity", color = "black", alpha = 0.7) +
  geom_text(aes(label = n), vjust = -0.5, color = "black", size = 3.5) +
  labs(title = "Distribución de los estados de los pedidos", 
       x = "Estado", 
       y = "Número de pedidos") +
  theme_minimal()

# Calcular la proporción de cada estado
estado_frecuencia <- estado_frecuencia %>%
  mutate(proporcion = n / sum(n) * 100)

# Mostrar la tabla con la proporción de cada estado
print(estado_frecuencia)

# Crear un gráfico de barras para la proporción de los estados de los pedidos, aunque sea el mismo gráfico que antes
ggplot(estado_frecuencia, aes(x = Estado, y = proporcion, fill = Estado)) +
  geom_bar(stat = "identity", color = "black", alpha = 0.7) +
  geom_text(aes(label = paste0(round(proporcion, 2), "%")), vjust = -0.5, color = "black", size = 3.5) +
  labs(title = "Proporción de los estados de los pedidos", 
       x = "Estado", 
       y = "Proporción (%)") +
  theme_minimal()

#SEXO
# Verificar los valores únicos en la columna "Sexo"
unique(TFG_Usuarios$Sexo)


# Calcular la frecuencia de todos los valores en la columna Sexo
sexo_frecuencia <- TFG_Usuarios %>%
  count(Sexo)

# Calcular la proporción
sexo_frecuencia <- sexo_frecuencia %>%
  mutate(proporcion = n / sum(n) * 100)

# Mostrar los resultados
print(sexo_frecuencia)

# Crear un gráfico de barras para la distribución del sexo
ggplot(sexo_frecuencia, aes(x = Sexo, y = n, fill = Sexo)) +
  geom_bar(stat = "identity", color = "black", alpha = 0.7) +
  geom_text(aes(label = paste0(round(proporcion, 1), "%")), vjust = -0.5, color = "black", size = 3.5) +
  labs(title = "Distribución del Sexo entre los clientes", 
       x = "Sexo", 
       y = "Número de clientes") +
  theme_minimal()

# Números dados
hombres <- 465
mujeres <- 353
no_especificado <- 19959

# Calcular el total de especificados
total_especificado <- hombres + mujeres

# Calcular la proporción de hombres y mujeres
hombres_proporcion <- (hombres / total_especificado) * 100
mujeres_proporcion <- (mujeres / total_especificado) * 100

# Mostrar las proporciones calculadas
hombres_proporcion
mujeres_proporcion

# Datos ajustados para la gráfica
sexo_frecuencia_ajustada <- data.frame(
  Sexo = c("Hombre", "Mujer"),
  n = c(hombres, mujeres),
  proporcion = c(hombres_proporcion, mujeres_proporcion)
)

# Crear la gráfica de barras
ggplot(sexo_frecuencia_ajustada, aes(x = Sexo, y = n, fill = Sexo)) +
  geom_bar(stat = "identity", color = "black", alpha = 0.7) +
  geom_text(aes(label = paste0(round(proporcion, 2), "%")), vjust = -0.5, color = "black", size = 3.5) +
  labs(title = "Distribución de sexo de los clientes especificados",
       x = "Sexo",
       y = "Número de clientes") +
  theme_minimal() +
  scale_fill_manual(values = c("Hombre" = "blue", "Mujer" = "pink"))

#Patrocinador cambiar a 0 y 1
TFG_Usuarios$Patrocinador <- ifelse(TFG_Usuarios$Patrocinador == "No especificado", 0, 1)
table(TFG_Usuarios$Patrocinador)

# Crear un data frame para los datos
patrocinador_frecuencia <- data.frame(
  Patrocinador = c("Sin Patrocinador", "Con Patrocinador"),
  n = c(4140, 16637)
)

# Calcular la proporción
patrocinador_frecuencia <- patrocinador_frecuencia %>%
  mutate(proporcion = n / sum(n) * 100)

# Crear el gráfico de barras
ggplot(patrocinador_frecuencia, aes(x = Patrocinador, y = n, fill = Patrocinador)) +
  geom_bar(stat = "identity", color = "black", alpha = 0.7) +
  geom_text(aes(label = paste0(round(proporcion, 2), "%")), vjust = -0.5, color = "black", size = 3.5) +
  labs(title = "Distribución de Usuarios con y sin patrocinador",
       x = "Tipo de usuario",
       y = "Número de usuarios") +
  theme_minimal() +
  scale_fill_manual(values = c("Sin Patrocinador" = "red", "Con Patrocinador" = "green"))



# Asegurarse de que la columna Fecha_registro esté en formato Date
TFG_Usuarios$Fecha_registro <- as.Date(TFG_Usuarios$Fecha_registro, format = "%Y-%m-%d")

# Filtrar los datos para el rango de tiempo desde marzo 2022 hasta octubre 2023
TFG_Usuarios_filtrado <- TFG_Usuarios %>%
  filter(Fecha_registro >= as.Date("2022-03-01") & Fecha_registro <= as.Date("2023-10-31"))

# Extraer el año y el mes de la columna Fecha_registro
TFG_Usuarios_filtrado <- TFG_Usuarios_filtrado %>%
  mutate(Año = year(Fecha_registro),
         Mes = month(Fecha_registro, label = TRUE, abbr = TRUE))

# Crear una columna de fechas combinando Año y Mes
TFG_Usuarios_filtrado <- TFG_Usuarios_filtrado %>%
  mutate(Fecha_Mes = as.Date(paste(Año, sprintf("%02d", month(Fecha_registro)), "01", sep = "-"), format = "%Y-%m-%d"))

# Contar el número de registros por mes
usuarios_por_mes <- TFG_Usuarios_filtrado %>%
  group_by(Fecha_Mes) %>%
  summarise(Usuarios = n(), .groups = 'drop') %>%
  arrange(Fecha_Mes)

# Contar el número acumulado de registros por año
usuarios_por_año_acumulado <- TFG_Usuarios_filtrado %>%
  arrange(Fecha_registro) %>%
  mutate(Acumulado = row_number()) %>%
  group_by(Año) %>%
  summarise(Usuarios_Acumulados = max(Acumulado), .groups = 'drop')

# Gráfico de usuarios por mes
ggplot(usuarios_por_mes, aes(x = Fecha_Mes, y = Usuarios)) +
  geom_line(color = "blue") +
  geom_point(color = "blue") +
  labs(title = "Número de usuarios registrados por mes",
       x = "Mes",
       y = "Número de usuarios") +
  theme_minimal() +
  scale_x_date(date_labels = "%b-%Y", date_breaks = "1 month") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Crear una columna de acumulado de usuarios por mes
usuarios_por_mes_acumulado <- usuarios_por_mes %>%
  arrange(Fecha_Mes) %>%
  mutate(Usuarios_Acumulados = cumsum(Usuarios))

# Gráfico acumulado de usuarios por mes
ggplot(usuarios_por_mes_acumulado, aes(x = Fecha_Mes, y = Usuarios_Acumulados)) +
  geom_line(color = "red") +
  geom_point(color = "red") +
  labs(title = "Número acumulado de usuarios registrados por mes",
       x = "Mes",
       y = "Número acumulado de usuarios") +
  theme_minimal() +
  scale_x_date(date_labels = "%b-%Y", date_breaks = "1 month") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



#PEDIDOS

# Filtrar los pedidos en estado "Enviado"
pedidos_enviados <- TFG_Pedidos %>%
  filter(Estado == "Enviado")

# Calcular el precio medio de los pedidos enviados
precio_medio <- mean(pedidos_enviados$Total_pagado, na.rm = TRUE)

# Contar cuántos pedidos tienen descuento y cuántos no
descuento_frecuencia <- pedidos_enviados %>%
  mutate(Tiene_Descuento = ifelse(Descuento > 0, "Con Descuento", "Sin Descuento")) %>%
  count(Tiene_Descuento)

# Calcular la proporción de pedidos con y sin descuento
descuento_frecuencia <- descuento_frecuencia %>%
  mutate(Proporción = n / sum(n) * 100)

# Mostrar resultados
print(precio_medio)
print(descuento_frecuencia)

# Crear un gráfico de barras para visualizar los pedidos con y sin descuento
ggplot(descuento_frecuencia, aes(x = Tiene_Descuento, y = n, fill = Tiene_Descuento)) +
  geom_bar(stat = "identity", color = "black", alpha = 0.7) +
  geom_text(aes(label = paste0(round(Proporción, 1), "%")), vjust = -0.5, color = "black", size = 3.5) +
  labs(title = "Distribución de pedidos con y sin descuento",
       x = "Tipo de Pedido",
       y = "Número de Pedidos") +
  theme_minimal() +
  scale_fill_manual(values = c("Con Descuento" = "blue", "Sin Descuento" = "red"))

# Contar cuántos pedidos usaron saldo/puntos y cuántos no
saldo_frecuencia <- pedidos_enviados %>%
  mutate(Usa_Saldo = ifelse(Saldo_usado > 0, "Con Saldo", "Sin Saldo")) %>%
  count(Usa_Saldo)

# Calcular la proporción de pedidos con y sin saldo/puntos
saldo_frecuencia <- saldo_frecuencia %>%
  mutate(Proporción = n / sum(n) * 100)

# Mostrar resultados de saldo/puntos
print(saldo_frecuencia)

# Crear un gráfico de barras para visualizar los pedidos con y sin saldo/puntos
ggplot(saldo_frecuencia, aes(x = Usa_Saldo, y = n, fill = Usa_Saldo)) +
  geom_bar(stat = "identity", color = "black", alpha = 0.7) +
  geom_text(aes(label = paste0(round(Proporción, 1), "%")), vjust = -0.5, color = "black", size = 3.5) +
  labs(title = "Distribución de pedidos con y sin uso de Saldo/Puntos",
       x = "Tipo de Pedido",
       y = "Número de Pedidos") +
  theme_minimal() +
  scale_fill_manual(values = c("Con Saldo" = "blue", "Sin Saldo" = "red"))

#Oferta fisios (Nombre)
unique(TFG_Pedidos$Nombre)

# Contar la frecuencia de cada nombre
frecuencia_nombres <- TFG_Pedidos %>%
  count(Nombre)

# Ordenar por frecuencia descendente
frecuencia_nombres <- frecuencia_nombres %>%
  arrange(desc(n))

# Mostrar las frecuencias
print(frecuencia_nombres)

# Filtrar los nombres que se han usado al menos 10 veces
frecuencia_nombres_filtrado <- frecuencia_nombres %>%
  filter(n >= 10) %>%
  arrange(desc(n))

# Crear el gráfico de barras
ggplot(frecuencia_nombres_filtrado, aes(x = reorder(Nombre, -n), y = n, fill = Nombre)) +
  geom_bar(stat = "identity", color = "black", alpha = 0.7) +
  labs(title = "Frecuencia de Nombres en TFG_Pedidos (Más de 10 veces)",
       x = "Nombre",
       y = "Frecuencia") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  guides(fill = FALSE)


#ANALISIS DEL TIEMPO DE CONVERSION

# Filtrar los usuarios que se han convertido en clientes
clientes <- TFG_Usuarios %>%
  filter(!is.na(`Primer pedido`))

# Calcular el tiempo de conversión
clientes <- clientes %>%
  mutate(Tiempo_conversion = as.numeric(`Primer pedido` - Fecha_registro))

# Verificar las primeras filas para asegurarnos de que los cálculos son correctos
head(clientes)

# Crear un histograma del tiempo de conversión
ggplot(clientes, aes(x = Tiempo_conversion)) +
  geom_histogram(binwidth = 10, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Distribución del tiempo de conversión de usuarios a clientes",
       x = "Días hasta la Conversión",
       y = "Frecuencia") +
  theme_minimal()

summary(clientes$Tiempo_conversion)

# Crear un histograma del tiempo de conversión segmentado por patrocinador
ggplot(clientes, aes(x = Tiempo_conversion, fill = factor(Patrocinador))) +
  geom_histogram(binwidth = 10, color = "black", alpha = 0.7, position = "dodge") +
  labs(title = "Distribución del tiempo de conversión de usuarios a clientes por patrocinador",
       x = "Días hasta la Conversión",
       y = "Frecuencia",
       fill = "Patrocinador") +
  theme_minimal()

# Calcular la proporción de conversiones por patrocinador
conversion_por_patrocinador <- clientes %>%
  group_by(Patrocinador) %>%
  summarise(Conversiones = n(), .groups = 'drop') %>%
  mutate(Proporcion = Conversiones / sum(Conversiones) * 100)

# Mostrar la tabla con la proporción de conversiones por patrocinador
print(conversion_por_patrocinador)

#Analisis de conversiones
# Calcular el número total de usuarios
total_usuarios <- nrow(TFG_Usuarios)

# Calcular el número de conversiones
total_conversiones <- nrow(clientes)

# Proporción de conversiones
proporcion_conversiones <- total_conversiones / total_usuarios * 100

# Número de usuarios con y sin patrocinador
usuarios_con_patrocinador <- sum(TFG_Usuarios$Patrocinador == 1)
usuarios_sin_patrocinador <- sum(TFG_Usuarios$Patrocinador == 0)

# Tasa de conversión para usuarios con patrocinador
conversiones_con_patrocinador <- nrow(clientes %>% filter(Patrocinador == 1))
tasa_conversion_con_patrocinador <- conversiones_con_patrocinador / usuarios_con_patrocinador * 100

# Tasa de conversión para usuarios sin patrocinador
conversiones_sin_patrocinador <- nrow(clientes %>% filter(Patrocinador == 0))
tasa_conversion_sin_patrocinador <- conversiones_sin_patrocinador / usuarios_sin_patrocinador * 100

# Crear un data frame para las tasas de conversión
conversion_tasas <- data.frame(
  Grupo = c("Con Patrocinador", "Sin Patrocinador"),
  Tasa_Conversion = c(tasa_conversion_con_patrocinador, tasa_conversion_sin_patrocinador)
)

# Visualizar la proporción de conversiones
ggplot(conversion_tasas, aes(x = Grupo, y = Tasa_Conversion, fill = Grupo)) +
  geom_bar(stat = "identity", color = "black", alpha = 0.7) +
  geom_text(aes(label = paste0(round(Tasa_Conversion, 2), "%")), vjust = -0.5, color = "black", size = 3.5) +
  labs(title = "Tasa de conversión de usuarios a clientes por patrocinador",
       x = "Grupo",
       y = "Tasa de Conversión (%)") +
  theme_minimal() +
  scale_fill_manual(values = c("Con Patrocinador" = "blue", "Sin Patrocinador" = "red"))

# Crear un data frame para la proporción de conversiones
conversion_proporciones <- data.frame(
  Grupo = c("Con Patrocinador", "Sin Patrocinador"),
  Conversiones = c(conversiones_con_patrocinador, conversiones_sin_patrocinador)
)

# Visualizar la proporción de conversiones
ggplot(conversion_proporciones, aes(x = Grupo, y = Conversiones, fill = Grupo)) +
  geom_bar(stat = "identity", color = "black", alpha = 0.7) +
  geom_text(aes(label = Conversiones), vjust = -0.5, color = "black", size = 3.5) +
  labs(title = "Número de conversiones de usuarios a clientes por patrocinador",
       x = "Grupo",
       y = "Número de Conversiones") +
  theme_minimal() +
  scale_fill_manual(values = c("Con Patrocinador" = "blue", "Sin Patrocinador" = "red"))

# Mostrar resultados adicionales
cat("Número total de usuarios:", total_usuarios, "\n")
cat("Número total de conversiones:", total_conversiones, "\n")
cat("Proporción de conversiones:", round(proporcion_conversiones, 2), "%\n")
cat("Tasa de conversión para usuarios con patrocinador:", round(tasa_conversion_con_patrocinador, 2), "%\n")
cat("Tasa de conversión para usuarios sin patrocinador:", round(tasa_conversion_sin_patrocinador, 2), "%\n")


# Crear histogramas y densidades superpuestas del tiempo de conversión
ggplot(clientes, aes(x = Tiempo_conversion, fill = factor(Patrocinador))) +
  geom_histogram(aes(y = ..density..), binwidth = 30, color = "black", alpha = 0.5, position = "identity") +
  geom_density(alpha = 0.7) +
  labs(title = "Distribución del tiempo de conversión de usuarios a clientes por patrocinador",
       x = "Días hasta la Conversión",
       y = "Densidad",
       fill = "Patrocinador") +
  theme_minimal() +
  scale_fill_manual(values = c("0" = "red", "1" = "blue"))

# Crear un data frame para el gráfico de barras acumulado
clientes_acumulado <- clientes %>%
  mutate(Intervalo = cut(Tiempo_conversion, breaks = seq(0, max(Tiempo_conversion, na.rm = TRUE), by = 30), right = FALSE)) %>%
  group_by(Intervalo, Patrocinador) %>%
  summarise(Conversiones = n(), .groups = 'drop')
# Crear el gráfico de barras acumulado
ggplot(clientes_acumulado, aes(x = Intervalo, y = Conversiones, fill = factor(Patrocinador))) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.7) +
  labs(title = "Número de conversiones por intervalo de tiempo y patrocinador",
       x = "Días hasta la conversión (Intervalos de 30 días)",
       y = "Número de conversiones",
       fill = "Patrocinador") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("0" = "red", "1" = "blue"))


#VAMOS CON LOS MODELOS

#CLUSTERS

# Instalación y carga de las librerías necesarias
install.packages("tidyverse")
install.packages("factoextra")

library(tidyverse)
library(factoextra)
if (!require("smotefamily")) install.packages("smotefamily", dependencies=TRUE)
if (!require("randomForest")) install.packages("randomForest", dependencies=TRUE)
if (!require("caret")) install.packages("caret", dependencies=TRUE)
if (!require("dplyr")) install.packages("dplyr", dependencies=TRUE)
if (!require("ggplot2")) install.packages("ggplot2", dependencies=TRUE)

library(smotefamily)
library(randomForest)
library(caret)
library(dplyr)
library(ggplot2)

# Filtrar clientes que han realizado al menos un pedido
clientes <- TFG_Usuarios %>% filter(Pedidos > 0)

# Seleccionar variables relevantes
clientes_seleccion <- clientes %>%
  select(Edad, Fecha_registro, Puntos, Patrocinador, Sexo, Pedidos, `Primer pedido`, `Último_pedido`)

# Crear variables de tiempo de conversión y tiempo desde el último pedido
clientes_seleccion <- clientes_seleccion %>%
  mutate(Tiempo_conversion = as.numeric(difftime(`Primer pedido`, Fecha_registro, units = "days")),
         Tiempo_desde_ultimo_pedido = as.numeric(difftime(Sys.Date(), `Último_pedido`, units = "days")))

# Convertir la variable 'Sexo' a numérica (0 para Hombre, 1 para Mujer) y eliminar filas con NA o 'No especificado'
clientes_seleccion <- clientes_seleccion %>%
  mutate(Sexo = ifelse(Sexo == "Hombre", 0, ifelse(Sexo == "Mujer", 1, NA))) %>%
  filter(!is.na(Sexo))

# Asegurarse de que todas las columnas necesarias son numéricas
clientes_seleccion <- clientes_seleccion %>%
  mutate(Edad = as.numeric(Edad),
         Puntos = as.numeric(Puntos),
         Pedidos = as.numeric(Pedidos),
         Tiempo_conversion = as.numeric(Tiempo_conversion),
         Tiempo_desde_ultimo_pedido = as.numeric(Tiempo_desde_ultimo_pedido),
         Patrocinador = as.numeric(Patrocinador))

# Manejar posibles NA introducidos por la coerción
clientes_seleccion <- clientes_seleccion %>%
  mutate(across(c(Edad, Puntos, Pedidos, Tiempo_conversion, Tiempo_desde_ultimo_pedido, Patrocinador), ~ ifelse(is.na(.x), 0, .x)))

# Seleccionar las variables para el K-Means
variables_kmeans <- clientes_seleccion %>%
  select(Edad, Puntos, Patrocinador, Sexo, Pedidos, Tiempo_conversion, Tiempo_desde_ultimo_pedido)

# Normalizar las variables numéricas
variables_kmeans_normalizadas <- variables_kmeans %>%
  mutate(across(c(Edad, Puntos, Pedidos, Tiempo_conversion, Tiempo_desde_ultimo_pedido), ~ scale(.x, center = TRUE, scale = TRUE)))

# Encontrar el número óptimo de clusters usando el método del codo
fviz_nbclust(variables_kmeans_normalizadas, kmeans, method = "wss")

#OPTIMO PARECE SER QUE ES 4

# Asegurarnos de que el dataframe y los nombres de columnas son correctos
clientes_seleccion <- clientes_seleccion %>%
  filter(!is.na(Edad) & !is.na(Puntos) & !is.na(Pedidos) & !is.na(Tiempo_conversion) & !is.na(Tiempo_desde_ultimo_pedido) & !is.na(Sexo) & !is.na(Patrocinador))

# Verificar el número de filas después del filtrado
nrow(clientes_seleccion)

#normalizar los datos
variables_kmeans <- clientes_seleccion %>%
  select(Edad, Puntos, Pedidos, Tiempo_conversion, Tiempo_desde_ultimo_pedido, Sexo, Patrocinador)

variables_kmeans_normalizadas <- variables_kmeans %>%
  mutate(across(c(Edad, Puntos, Pedidos, Tiempo_conversion, Tiempo_desde_ultimo_pedido), scale))

# Verificar el número de filas después de la normalización
nrow(variables_kmeans_normalizadas)

#Aplicar K-Means Clustering
set.seed(123)
kmeans_result <- kmeans(variables_kmeans_normalizadas, centers = 4, nstart = 25)

# Verificar el número de clusters
length(kmeans_result$cluster)



# Añadir los clusters al dataframe original filtrado
clientes_seleccion$Cluster <- kmeans_result$cluster

# Ver las primeras filas del dataframe con los clusters
head(clientes_seleccion)


# Ver resumen de los clusters
summary(clientes_seleccion$Cluster)

# Contar el número de clientes en cada cluster
table(clientes_seleccion$Cluster)

#ANALISIS DE LOS CLUSTERS
library(ggplot2)

# Edad por cluster
ggplot(clientes_seleccion, aes(x = factor(Cluster), y = Edad, fill = factor(Cluster))) +
  geom_boxplot() +
  labs(title = "Distribución de la Edad por Cluster", x = "Cluster", y = "Edad") +
  theme_minimal()

# Puntos por cluster
ggplot(clientes_seleccion, aes(x = factor(Cluster), y = Puntos, fill = factor(Cluster))) +
  geom_boxplot() +
  labs(title = "Distribución de los Puntos por Cluster", x = "Cluster", y = "Puntos") +
  theme_minimal()

# Pedidos por cluster
ggplot(clientes_seleccion, aes(x = factor(Cluster), y = Pedidos, fill = factor(Cluster))) +
  geom_boxplot() +
  labs(title = "Distribución de los Pedidos por Cluster", x = "Cluster", y = "Pedidos") +
  theme_minimal()

# Tiempo de conversión por cluster
ggplot(clientes_seleccion, aes(x = factor(Cluster), y = Tiempo_conversion, fill = factor(Cluster))) +
  geom_boxplot() +
  labs(title = "Distribución del Tiempo de Conversión por Cluster", x = "Cluster", y = "Tiempo de Conversión (días)") +
  theme_minimal()

# Tiempo desde el último pedido por cluster
ggplot(clientes_seleccion, aes(x = factor(Cluster), y = Tiempo_desde_ultimo_pedido, fill = factor(Cluster))) +
  geom_boxplot() +
  labs(title = "Distribución del Tiempo desde el Último Pedido por Cluster", x = "Cluster", y = "Tiempo desde el Último Pedido (días)") +
  theme_minimal()


# Calcular los centroides de cada cluster

# Verificar las primeras filas del dataframe con los clusters
head(variables_kmeans_normalizadas)

# Verificar las primeras filas del dataframe original
head(clientes_seleccion)

# Calcular los centroides de cada cluster
centroides <- variables_kmeans %>%
  group_by(clientes_seleccion$Cluster) %>%
  summarise(across(everything(), mean, na.rm = TRUE))

# Ver los centroides
print(centroides)



#MODELO

# Suponiendo que ya has cargado y limpiado los datos, y has calculado la variable 'Edad' en el script previo

# Filtrar solo los usuarios que han realizado al menos un pedido
clientes_activos <- TFG_Usuarios %>% filter(Pedidos > 0)

# Calcular la diferencia entre el primer y último pedido
clientes_activos <- clientes_activos %>%
  mutate(Periodo_actividad = as.numeric(difftime(`Último_pedido`, `Primer pedido`, units = "days")))

# Crear la variable objetivo 'churn'
# Consideramos churn si el periodo de actividad es mayor a 6 meses y no ha hecho un pedido en los últimos 6 meses
clientes_activos <- clientes_activos %>%
  mutate(churn = ifelse(Periodo_actividad > 180 & `Último_pedido` < (Sys.Date() - months(6)), 1, 0))

# Convertir 'Sexo' y 'churn' a factores
clientes_activos$Sexo <- as.factor(clientes_activos$Sexo)
clientes_activos$churn <- as.factor(clientes_activos$churn)

# Manejo de valores nulos
# Imputar valores nulos en 'Edad' y 'Periodo_actividad' con la media de cada columna
clientes_activos$Edad[is.na(clientes_activos$Edad)] <- mean(clientes_activos$Edad, na.rm = TRUE)
clientes_activos$Periodo_actividad[is.na(clientes_activos$Periodo_actividad)] <- mean(clientes_activos$Periodo_actividad, na.rm = TRUE)

# Dividir los datos en conjuntos de entrenamiento y prueba
set.seed(42)
trainIndex <- createDataPartition(clientes_activos$churn, p = 0.7, list = FALSE)
trainData <- clientes_activos[trainIndex,]
testData <- clientes_activos[-trainIndex,]

# Instalar y cargar la librería ROSE
if (!require(ROSE)) {
  install.packages("ROSE")
}
library(ROSE)

# Balancear los datos de entrenamiento
trainData_balanced <- ovun.sample(churn ~ Edad + Periodo_actividad + Sexo, data = trainData, method = "over", N = sum(trainData$churn == 0)*2)$data

# Entrenar el modelo de Random Forest con los datos balanceados
library(randomForest)
modelo_rf_balanced <- randomForest(churn ~ Edad + Periodo_actividad + Sexo, data = trainData_balanced, importance = TRUE, ntree = 100)

# Hacer predicciones
predictions_balanced <- predict(modelo_rf_balanced, testData)

# Evaluar el modelo
library(caret)
confusionMatrix(predictions_balanced, testData$churn)

# Mostrar la importancia de las variables
varImpPlot(modelo_rf_balanced)







