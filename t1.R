##########################################################################
#
# Creditos: Cristobal Gallardo
# Evaluación - Mineria de Datos de datos en alimentos 
# nombre del archivo: evaluacion_CG.R
# Fecha: 2025-05-26
#
###########################################################################
#Funciones
# 1. Comparación por restaurantes
comparacion_por_restaurantes <- function(df, grupo_var = "restaurant") {
  # Calcular promedios
  resumen <- aggregate(cbind(calories, total_fat, sodium) ~ get(grupo_var), data = df, FUN = mean, na.rm = TRUE)
  
  colnames(resumen)[1] <- grupo_var
  
  variable <- rep(c("Calorías", "Grasa Total", "Sodio"), each = nrow(resumen))
  valor <- c(resumen$calories, resumen$total_fat, resumen$sodium)
  grupo <- rep(resumen[[grupo_var]], times = 3)
  
  datos_largos <- data.frame(
    grupo = grupo,
    variable = variable,
    valor = valor
  )
  
  colnames(datos_largos)[1] <- grupo_var
  
  return(datos_largos)
}
# 2. Es Saludable
es_saludable_func <- function(df) {
  with(df,
       calories < 500 &
       total_fat < 20 &
       sat_fat < 5 &
       sugar < 10 &
       fiber > 3 &
       protein > 15
  )
}
# 3. Contador de productos saludables de cada restaurante
conteo_saludables <- function(df, grupo_var = "restaurant") {
  # Evaluar si cada ítem es saludable
  df$es_saludable <- es_saludable_func(df)
  
  # Crear tabla de conteo por grupo
  tabla <- aggregate(es_saludable ~ get(grupo_var), data = df, FUN = function(x) c(saludables = sum(x), no_saludables = sum(!x)))
  
  # Separar la lista en dos columnas
  conteo <- do.call(data.frame, tabla)
  colnames(conteo) <- c(grupo_var, "saludables", "no_saludables")
  
  # Reestructurar a formato largo
  variable <- rep(c("saludables", "no_saludables"), each = nrow(conteo))
  valor <- c(conteo$saludables, conteo$no_saludables)
  grupo <- rep(conteo[[grupo_var]], times = 2)
  
  datos_largos <- data.frame(grupo = grupo, tipo = variable, cantidad = valor)
  colnames(datos_largos)[1] <- grupo_var
  
  return(datos_largos)
}
# 4. Promedio de por_calorias_grasa, indice_salud y sodio_por_proteina por categoria
promedio_por_categoria <- function(df, grupo_var = "categoria") {
  # Calcular promedios por categoria
  resumen <- aggregate(cbind(porc_calorias_grasa, indice_salud, sodio_por_proteina) ~ categoria, data = df, FUN = mean, na.rm = TRUE)

  # Renombrar la columna de grupo para que siempre se llame igual
  colnames(resumen)[1] <- grupo_var
  
  # Crear formato largo manualmente
  variable <- rep(c("% Calorías x grasas", "Indice Salud", "Sodio x Proteina"), each = nrow(resumen))
  valor <- c(resumen$porc_calorias_grasa, resumen$indice_salud, resumen$sodio_por_proteina)
  grupo <- rep(resumen[[grupo_var]], times = 3)
  
  # Crear dataframe largo
  datos_largos <- data.frame(
    grupo = grupo,
    variable = variable,
    valor = valor
  )
  colnames(datos_largos)[1] <- grupo_var
  
  return(datos_largos)
}
# Funciones para calculos chicos
porc_calorias_grasa <- function(total_fat, calories) {
  (total_fat * 9) / calories * 100
}
indice_salud <- function(fiber, protein, sat_fat, sugar) {
  fiber + protein - sat_fat - sugar
}
sodio_por_proteina <- function(sodium, protein) {
  sodium / protein
}
############################################################################################################
#Carga de datos externos con extensión csv
valores<-read.csv("fastfood_calories.csv")

# Mostrar la cantidad de columnas
ncol(valores)
cat("\n")
print("La cantidad de columnas es:")
print(ncol(valores))

# Mostrar la estructura de la data
cat("\n")
print("La estructura de la data es:")
print(str(valores))
names(valores)[names(valores) == "X"] <- "ID"
# Limpiar los datos
valores <- valores[, -c(5,15, 16, 17, 18)]

summary(valores)
colSums(is.na(valores)) 
print("Cantidad de NA por columna:")
print(colSums(is.na(valores)))

# Eliminar filas con NA
valores <- na.omit(valores)
row.names(valores) <- NULL
print("Cantidad de NA por columna después de eliminar filas con NA:")
print(colSums(is.na(valores)))

# Comprobar si hay duplicados
duplicados <- duplicated(valores$Item)
any(duplicados)
print("¿Hay duplicados en la columna Item?")
print(any(duplicados))  

# Crear columnas adicionales
valores$porc_calorias_grasa <- porc_calorias_grasa(valores$total_fat, valores$calories)
valores$indice_salud <- indice_salud(valores$fiber, valores$protein, valores$sat_fat, valores$sugar)
valores$sodio_por_proteina <- sodio_por_proteina(valores$sodium, valores$protein)

###########################################################################################################
# Dataframes creado desde Vectores adicionales
# 1 - Clasificación de los items
items <- unique(valores$item) # vector de items únicos
es_saludable <- logical(length(items))
for (i in seq_along(items)) {
  subset_item <- valores[valores$item == items[i], ]
  # Verificamos si todas las filas cumplen la condición
  es_saludable[i] <- all(es_saludable_func(subset_item))
}
categoria <- ifelse(es_saludable, "Saludable", "No saludable")
# Crear dataframe desde vectores
clasificacion_items <- data.frame(
  item = items,
  categoria = categoria,
  stringsAsFactors = FALSE
)
cat("\n")
# print(clasificacion_items)

# 2 - Clasificación de los restaurantes
nombres_rest <- c("Mcdonalds", "Chick Fil-A", "Sonic", "Arbys", "Burger King", "Dairy Queen", "Subway", "Taco Bell")
ciudad_origen <- c("San Bernardino, CA", "Hapeville, GA", "Shawnee, OK", "Boardman, OH", "Miami, FL", "Joliet, IL", "Bridgeport, CT", "Downey, CA")
año_fundacion <- c(1940, 1946, 1953, 1964, 1954, 1940, 1965, 1962)

# Crear dataframe desde vectores
info_restaurantes <- data.frame(
  restaurant = nombres_rest,
  ciudad_origen = ciudad_origen,
  año_fundacion = año_fundacion
)

# Enlazar con tu dataset principal
valores_enriquecido <- merge(valores, info_restaurantes, by = "restaurant", all.x = TRUE)
valores_clasificados <- merge(valores, clasificacion_items, by = "item", all.x = TRUE)
valores_clasificados_simple <- valores_clasificados[, c("item", "categoria")]
# Dataframe final
valores_final <- merge(valores_enriquecido, valores_clasificados_simple, by = "item", all.x = TRUE)
cat("\n")
# print("Dataframe final:")
# print(valores_final)
print(str(valores_final))

# Generación de gráficos
library(ggplot2)

# 1. Gráfico Promedio de calorías, sodio y grasas por restaurante
datos_para_grafico <- comparacion_por_restaurantes(valores_final)

grafico <- ggplot(datos_para_grafico, aes(x = reorder(restaurant, -valor), y = valor, fill = variable)) +
  geom_col(position = "dodge") +
  labs(title = "Promedios nutricionales por restaurante",
       x = "Restaurante",
       y = "Valor promedio",
       fill = "Variable") +
  theme_minimal() +
  coord_flip()

# print(grafico)

# 2. Gráfico de Cantidad de items saludables por restaurante
saludables_por_restaurante <- conteo_saludables(valores_final)
grafico2 <-  ggplot(saludables_por_restaurante, aes(x = reorder(restaurant, -cantidad), y = cantidad, fill = tipo)) +
  geom_col(position = "dodge") +
  labs(title = "Conteo de ítems saludables y no saludables por restaurante",
       x = "Restaurante",
       y = "Cantidad de items") +
  scale_fill_manual(values = c("saludables" = "forestgreen", "no_saludables" = "firebrick")) +
  theme_minimal() +
  coord_flip()

#print(grafico2)

# 3. Gráfico de Porcentaje de calorías de grasa por categoría
tabla_categoriaS <- promedio_por_categoria(valores_final)
# print(tabla_categoriaS)
grafico3 <- ggplot(tabla_categoriaS, aes(x = reorder(categoria, -valor), y = valor, fill = variable)) +
  geom_col(position = "dodge") +
  labs(title = "Promedios nutricionales por restaurante",
       x = "Restaurante",
       y = "Valor promedio",
       fill = "Variable") +
  theme_minimal() +
  coord_flip()

# print(grafico3)

# Graficos a guardar
ggsave("grafico_promedio_restaurantes.png", plot = grafico, width = 10, height = 6)
ggsave("grafico_cant_items_saludables.png", plot = grafico2, width = 10, height = 6)
ggsave("grafico_promedios_nutricionales.png", plot = grafico3, width = 10, height = 6)
# Guardar el dataframe final
write.csv(valores_final, "valores_final.csv", row.names = FALSE)
# Guardar el dataframe clasificado
write.csv(clasificacion_items, "clasificacion_items.csv", row.names = FALSE)
# Guardar el dataframe de restaurantes
write.csv(info_restaurantes, "info_restaurantes.csv", row.names = FALSE)

