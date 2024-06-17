
# Limpiar el entorno
rm(list = ls())


# - -----------------------------------------------------------------------

# En este script en R se va a realizar una limpieza de datos de un dataset relativo
# a características y precios de viviendas de idealista extraídos del siguiente
# enlace:
# https://www.kaggle.com/datasets/mirbektoktogaraev/madrid-real-estate-market


# - -----------------------------------------------------------------------



# ---------------- 1. PREPARACIÓN DEL ENTORNO ----------------------------------

# Instalar librerías -----------------------------------------------------------

#install.packages("dplyr")
#install.packages("readr")
#install.packages("psych")
#install.packages("ggplot2")
#install.packages("purrr")

# Importar librerías -----------------------------------------------------------

library(dplyr)
library(readr)
library(psych)
library(purrr)
library(ggplot2)


# Cargar datos -----------------------------------------------------------------

data <- read_csv(file = "C:/Users/david/Downloads/houses_Madrid1.csv")


# ---------------------------- 2. EDA ------------------------------------------

unique(data$subtitle)


# Consultar primeros datos
data %>% head()


# Consultar últimos datos
data %>% tail()


# Comprobar columnas
colnames(data)


# Comprobar dimensión
dim(data)


# Comprobar duplicados
duplicated_data <- duplicated(data)
suma_duplicados <- sum(duplicated_data)
suma_duplicados


# Comprobar nulos# Compduplicated_datarobar nulos
columnas_con_nulos <- colnames(data)[colSums(is.na(data)) > 0]

# Número total de filas
total_filas <- nrow(data)

# Imprime el porcentaje de valores nulos por columna
if (length(columnas_con_nulos) > 0) {
  for (col in columnas_con_nulos) {
    porcentaje_nulos <- (sum(is.na(data[[col]])) / total_filas) * 100
    cat(col, round(porcentaje_nulos, 2), "% de nulos\n")
  }
} else {
  cat("No hay columnas con valores nulos\n")
}


# Datos estadísticos
describe(data)

# -------------------------- 3. LIMPIEZA ---------------------------------------

# ELIMINAR COLUMNAS CON MUCHOS NULOS

# Nombres de las columnas a eliminar
columnas_a_eliminar <- c('sq_mt_useful', 'n_floors', 'sq_mt_allotment', 'latitude', 'longitude', 'raw_address',
                         'street_number', 'street_name', 'portal', 'door', 'rent_price_by_area', 'house_type_id',
                         'has_central_heating', 'has_individual_heating', 'are_pets_allowed', 'has_fitted_wardrobes',
                         'has_garden', 'is_furnished', 'is_kitchen_equipped', 'has_private_parking', 'has_public_parking',
                         'is_parking_included_in_price', 'parking_price', 'is_orientation_north', 'is_orientation_west',
                         'is_orientation_south', 'is_orientation_east')

# Eliminar las columnas > 50% de nulos
data <- data[, !(names(data) %in% columnas_a_eliminar)]

data %>% head()

# Mostrar columnas actualizadas
colnames(data)


## FILTRAMOS LA COLUMNA TITLE PARA QUEDARNOS CON PISOS Y ESTUDIOS

data$title

# Filtrar el dataframe para mantener solo las filas donde la columna 'title' contiene 'Piso' o 'Estudio'
data <- data[grepl("Piso|Estudio", data$title, ignore.case = TRUE) & !is.na(data$title), ]



## ELIMINAR COLUMNAS QUE NO NOS INTERESAN
columnas_a_eliminar <- c('Unnamed.0', 'id', 'title', 'is_exact_address_hidden', 'is_floor_under', 'neighborhood_id',
                         'operation', 'rent_price', 'is_rent_price_known', 'buy_price_by_area', 'is_buy_price_known',
                         'is_exterior', 'energy_certificate')

# Eliminar columnas con información no necesaria
data <- data[, !(names(data) %in% columnas_a_eliminar)]


# Mostrar columnas actualizadas
colnames(data)



## SEPARAMOS SUBTITLE PARA QUEDARNOS CON DISTRITO

# Dividir la columna 'subtitle' en varias columnas usando la coma como delimitador
data_dividido <- as.data.frame(do.call(rbind, strsplit(as.character(data$subtitle), ",", fixed=TRUE)))

# Asignar nuevos nombres a las columnas resultantes
names(data_dividido) <- c("zona", "nada")

# Agregar la columna 'zona' al dataframe original
data$zona <- data_dividido$zona

# Contar el número de valores únicos en la columna 'zona'
n_distinct(data_dividido$zona)



## RELLENAR VARIABLES FALTANTES

### BAÑOS

# Ver valores únicos
unique(data$n_bathrooms)


ggplot(data=data, aes(x=n_bathrooms)) +
  geom_bar(
    color="#FFBD59"
  ) +
  labs(title = "Número de baños")




# Definir la función fill_bathrooms
fill_bathrooms <- function(row) {
  if (is.na(row$n_bathrooms)) {
    if (row$n_rooms <= 1) {
      return(1)
    } else {
      return(row$n_rooms - 1)
    }
  } else {
    return(row$n_bathrooms)
  }
}


# Aplicar la función a cada fila del data frame
data <- data %>%
  rowwise() %>%
  mutate(n_bathrooms = fill_bathrooms(cur_data())) %>%
  ungroup()

# Convertir a número entero
data$n_bathrooms <- as.integer(round(data$n_bathrooms))

unique(data$n_bathrooms)



### PISOS

# Comprobar valores únicos
unique(data$floor)



# Mapear los valores de la columna 'floor'
data$floor <- factor(data$floor, levels = c('Bajo', 'Entreplanta', 'Entreplanta exterior', 'Entreplanta interior',
                                            'Semi-sótano', 'Semi-sótano exterior', 'Semi-sótano interior',
                                            'Sótano', 'Sótano exterior', 'Sótano interior', '3', '4', '1', '2',
                                            '7', '6', '5', '9', '8', 'nan'),
                     labels = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 2, 1, 1, 2, 2, 2, 2, 2, 'NaN'))

# Convertir la columna 'floor' a numérica
data$floor <- as.numeric(as.character(data$floor))

# Calcular la mediana del piso por subtítulo
median_floor_by_subtitle <- tapply(data$floor, data$subtitle, median, na.rm = TRUE)


# Definir la función para rellenar los NA con la mediana por subtítulo
fill_na_with_median <- function(row) {
  if (is.na(row$floor)) {
    return(median_floor_by_subtitle[[row$subtitle]])
  } else {
    return(row$floor)
  }
}

# Aplicar la función para rellenar los NA en 'floor'
data$floor <- unlist(lapply(split(data, seq(nrow(data))), fill_na_with_median))


### Mapear los valores de la columna 'floor'
data$floor <- factor(data$floor, levels = c('1', '2', '0'),
                     labels = c('Planta_intermedia','Planta_alta', 'Planta_baja'))




### AIRE ACONDICIONADO
# Rellenar valores faltantes con False (se asume)
data$has_ac[is.na(data$has_ac)] <- FALSE

ggplot(data, aes(x = as.factor(has_ac), y = buy_price)) +
  geom_bar(stat = "summary", fun = "mean") +
  labs(title = "Promedio de precio por Aire Acondicionado",
       x = "Aire Acondicionado",
       y = "Precio de compra")



### ASCENSOR

# Comprobar valores únicos
unique(data$has_lift)

# Calcular la proporción de ascensores por zona
proporcion_con_ascensor <- tapply(data$has_lift, data$zona, mean, na.rm = TRUE)

# Determinar el valor para rellenar NaN según la proporción
zona_a_valor <- proporcion_con_ascensor > 0.5

# Rellenar los NaN en 'has_lift' según la proporción calculada
data$has_lift <- ifelse(is.na(data$has_lift), zona_a_valor[data$zona], data$has_lift)


ggplot(data, aes(x = as.factor(has_lift), y = buy_price)) +
  geom_bar(stat = "summary", fun = "mean") +
  labs(title = "Promedio de precio por Ascensor",
       x = "Ascensor",
       y = "Precio de compra") +
  theme_minimal()



### PISCINA, TERRAZA, BALCÓN, TRASTERO, ACCESIBILIDAD, ZONAS VERDES Y AIRE ACONDICIONADO

# Pasar de valores NaN a False
data[c('has_pool', 'has_terrace', 'has_balcony', 'has_storage_room', 'is_accessible','has_green_zones',
       'has_ac')][is.na(data[c('has_pool', 'has_terrace', 'has_balcony', 'has_storage_room', 'is_accessible',
                               'has_green_zones', 'has_ac')])] <- FALSE



### AÑO DE CONSTRUCCIÓN

# Calcular la moda
moda_año_construccion <- as.numeric(names(sort(table(data$built_year),
                                               decreasing = TRUE)[1]))

# Rellenar NaN con la moda
data$built_year[is.na(data$built_year)] <- moda_año_construccion

# Convertir a número entero
#data['built_year'] = data['built_year'].astype(int)


años_agrupados <- data %>%
  group_by(built_year)

ggplot(data, aes(x = as.factor(built_year), y = buy_price)) +
  geom_bar(stat = "summary", fun = "mean", color="orange") +
  labs(title = "Promedio de precio por año de construcción",
       x = "Año de construcción",
       y = "Precio de compra") +
  theme_minimal()



ggplot(data = data, aes(x = built_year)) +
  geom_bar(color="orange", bin)




### NUEVA CONSTRUCCIÓN

# Rellenar valores nulos en 'is_new_development' teniendo en cuenta 'year_built' (10 años o menos)
data$is_new_development <- sapply(seq(nrow(data)), function(i) {
  row <- data[i, ]
  if (!is.na(row$built_year)) {
    if (2014 <= row$built_year & row$built_year <= 2024) {
      return(TRUE)
    } else if (row$built_year < 2014) {
      return(FALSE)
    }
  }
  return(row$is_new_development)
})

ggplot(data = data) +
  geom_point(mapping = aes(x=is_new_development, y = buy_price))



### MAPEAR ZONAS

data$zona

# Crear un vector de correspondencia entre las zonas y los códigos
zonas_codigos <- c(
  'San Cristóbal' = 17,
  'Los Ángeles' = 17,
  'San Andrés' = 17,
  'Los Rosales' = 17,
  'Villaverde' = 17,
  'Butarque' = 17,
  'Vicálvaro' = 19,
  'Ambroz' = 19,
  'Casco Histórico de Vicálvaro' = 19,
  'El Cañaveral - Los Berrocales' = 19,
  'Valdebernardo - Valderribas' = 19,
  'Casco Histórico de Vallecas' = 18,
  'Villa de Vallecas' = 18,
  'Ensanche de Vallecas - La Gavia' = 18,
  'Santa Eugenia' = 18,
  'Orcasitas' = 12,
  'Usera' = 12,
  'San Fermín' = 12,
  'Pradolongo' = 12,
  'Zofío' = 12,
  'Almendrales' = 12,
  'Moscardó' = 12,
  '12 de Octubre-Orcasur' = 12,
  'Tetuán' = 6,
  'Valdeacederas' = 6,
  'Berruguete' = 6,
  'Cuatro Caminos' = 6,
  'Cuzco-Castillejos' = 6,
  'Bellas Vistas' = 6,
  'Ventilla-Almenara' = 6,
  'Retiro' = 3,
  'Adelfas' = 3,
  'Ibiza' = 3,
  'Pacífico' = 3,
  'Niño Jesús' = 3,
  'Jerónimos' = 3,
  'Estrella' = 3,
  'Puente de Vallecas' = 13,
  'Palomeras Bajas' = 13,
  'San Diego' = 13,
  'Palomeras sureste' = 13,
  'Numancia' = 13,
  'Entrevías' = 13,
  'Portazgo' = 13,
  'Aravaca' = 9,
  'Argüelles' = 9,
  'Valdezarza' = 9,
  'Moncloa' = 9,
  'Ciudad Universitaria' = 9,
  'Fontarrón' = 14,
  'Moratalaz' = 14,
  'Vinateros' = 14,
  'Marroquina' = 14,
  'Media Legua' = 14,
  'Pavones' = 14,
  'Horcajo' = 14,
  'Puerta del Ángel' = 10,
  'Latina' = 10,
  'Los Cármenes' = 10,
  'Aluche' = 10,
  'Águilas' = 10,
  'Lucero' = 10,
  'Campamento' = 10,
  'Cuatro Vientos' = 10,
  'Valdemarín' = 9,
  'Casa de Campo' = 9,
  'El Plantío' = 9,
  'Fuencarral' = 8,
  'Las Tablas' = 8,
  'La Paz' = 8,
  'Peñagrande' = 8,
  'Tres Olivos - Valverde' = 8,
  'Montecarmelo' = 8,
  'Pilar' = 8,
  'Mirasierra' = 8,
  'Arroyo del Fresno' = 8,
  'Fuentelarreina' = 8,
  'El Pardo' = 8,
  'Sanchinarro' = 16,
  'Hortaleza' = 16,
  'Virgen del Cortijo - Manoteras' = 16,
  'Valdebebas - Valdefuentes' = 16,
  'Palomas' = 16,
  'Pinar del Rey' = 16,
  'Conde Orgaz-Piovera' = 16,
  'Canillas' = 16,
  'Apóstol Santiago' = 16,
  'Chamberí' = 7,
  'Almagro' = 7,
  'Trafalgar' = 7,
  'Nuevos Ministerios-Ríos Rosas' = 7,
  'Vallehermoso' = 7,
  'Gaztambide' = 7,
  'Arapiles' = 7,
  'Ventas' = 15,
  'Pueblo Nuevo' = 15,
  'Atalaya' = 15,
  'Quintana' = 15,
  'San Juan Bautista' = 15,
  'Costillares' = 15,
  'Ciudad Lineal' = 15,
  'Concepción' = 15,
  'Colina' = 15,
  'San Pascual' = 15,
  'Chamartín' = 5,
  'El Viso' = 5,
  'Prosperidad' = 5,
  'Nueva España' = 5,
  'Castilla' = 5,
  'Bernabéu-Hispanoamérica' = 5,
  'Ciudad Jardín' = 5,
  'Lavapiés-Embajadores' = 1,
  'Opañel' = 11,
  'Comillas' = 11,
  'Abrantes' = 11,
  'San Isidro' = 11,
  'Carabanchel' = 11,
  'Puerta Bonita' = 11,
  'Vista Alegre' = 11,
  'Pau de Carabanchel' = 11,
  'Buena Vista' = 11,
  'Huertas-Cortes' = 1,
  'Malasaña-Universidad' = 1,
  'Chueca-Justicia' = 1,
  'Palacio' = 1,
  'Centro' = 1,
  'Sol' = 1,
  'Barrio de Salamanca' = 4,
  'Goya' = 4,
  'Lista' = 4,
  'Guindalera' = 4,
  'Fuente del Berro' = 4,
  'Castellana' = 4,
  'Recoletos' = 4,
  'Imperial' = 2,
  'Chopera' = 2,
  'Acacias' = 2,
  'Delicias' = 2,
  'Palos de Moguer' = 2,
  'Legazpi' = 2,
  'Arganzuela' = 2,
  'Casco Histórico de Barajas' = 20,
  'Alameda de Osuna' = 20,
  'Timón' = 20,
  'Campo de las Naciones-Corralejos' = 20,
  'Barajas' = 20
)

# Reemplazar los nombres de las zonas con sus códigos
data$zona <- zonas_codigos[data$zona]

unique(data$zona)


ggplot(data = data) +
  geom_point(
    mapping = aes(x = zona, y = buy_price),
    size = 1,
    color = "orange")



### RENOMBRAR COLUMNAS Y COMPROBAR NULOS

colnames(data) <- c('zona',
                    'metros_cuadrados',
                    'numero_habitaciones',
                    'numero_baños',
                    'altura_piso',
                    'precio_venta',
                    'necesita_reforma',
                    'año_construccion',
                    'aire_acondicionado',
                    'ascensor',
                    'piscina',
                    'terraza',
                    'balcon',
                    'accesibilidad',
                    'zonas_verdes',
                    'trastero',
                    'parking',
                    'nueva_construccion')


# Consultar nulos
colSums(is.na(data))


### MOSTRAR DATOS LIMPIOS
data %>% View()

