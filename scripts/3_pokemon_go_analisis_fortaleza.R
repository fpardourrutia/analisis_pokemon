library("plyr")
library("dplyr")
library("tidyr")
library("httr")
library("ggplot2")
library("png") # para leer imágenes PNG
library("grid") # para plotear PNG's (rasterGrob())
library("readr")

# Leyendo datos de Pokémon Go:
status_base_pokemon_go_1_251 <- readRDS("../datos/status_base_pokemon_go_1_251.RData")

# La siguiente función recibe una tabla con números de pokémon y descarga los 
# sprites asociados en una carpeta adecuada. Argumentos:
#   tabla_base: una tabla con una columna de "no" donde se encuentran los números
#   nacionales de los pokémon de interés
#   ruta_carpeta: carpeta donde se almacenarán las imágenes de pokémon.
# La función almacena los sprites en la ruta deseada.
almacena_sprites <- function(tabla_base, ruta_carpeta){
  
  # Almacenando las imágenes en la carpeta deseada
  l_ply(tabla_base$no, function(i){
    
    # Generando las URL's a partir de la columna "no" de "tabla_base
    url <- paste0("https://raw.githubusercontent.com/PokeAPI/sprites/master/sprites/pokemon/", i, ".png")
    
    # Generando ruta del archivo
    ruta_archivo <- paste0(ruta_carpeta, "/", i, ".png")
    
    # Almacenando el archivo en la ruta adecuada
    GET(url, write_disk(ruta_archivo))
  })
}

ruta_carpeta <- "../sprites/1_251"
# almacena_sprites(status_base_pokemon_go_1_251, ruta_carpeta)

### Calculando los índices a plotear, de acuerdo a mi conocimiento experto
indices_pokemon_go_1_251 <- status_base_pokemon_go_1_251 %>%
  gather("aux", "type", type_1, type_2) %>%
  select(-aux) %>%
  filter(!is.na(type)) %>%
  mutate(
    resistencia = sqrt(defense_pokemon_go * hp_pokemon_go),
    ataque = attack_pokemon_go
  ) %>%
  select(
    no,
    nombre = name,
    tipo = type,
    resistencia,
    ataque
  )

### Ploteando con los sprites de pokémon:

# Función que prepara una imagen para ser ploteada como un raster en una gráfica
# de ggplot:
# ruta_sprite: ruta del sprite a dibujar
# x: coordenada x del centro de la imagen
# y: coordenada y del centro de la imagen
# semitamanio: mitad del tamanio de la imagen a plotear
prepara_sprite <- function(ruta_sprite, x, y, semitamanio) {
  ruta_sprite %>%
    readPNG() %>%
    rasterGrob() %>%
    annotation_custom(
      xmin = x - semitamanio, xmax = x + semitamanio, 
      ymin = y - semitamanio, ymax = y + semitamanio
    )
}

# Ploteando para cada tipo:
# l_ply(unique(indices_pokemon_go_1_251$tipo), function(x){
#   
#   # Filtrando pokémon de un solo tipo:
#   indices_pokemon_go_1_251_filtrado <- indices_pokemon_go_1_251 %>%
#     filter(tipo == x)
#   
#   ggplot(data = indices_pokemon_go_1_251_filtrado, aes(x = resistencia, y = ataque)) +
#     # Para que ggplot centre las coordenadas, necesitamos geom_point()
#     geom_point(alpha = 0) +
#     apply(indices_pokemon_go_1_251_filtrado, 1, function(x){
#       ruta_sprite <- paste0(ruta_carpeta, "/", as.numeric(x[["no"]]), ".png")
#       prepara_sprite(ruta_sprite, as.numeric(x["resistencia"]), as.numeric(x["ataque"]), 10)
#     }) +
#     theme_bw() +
#     labs(x = "Resistencia", y = "Ataque") +
#     # Todas con los mismos límites para facilitar la interpretación
#     xlim(0, 350) +
#     ylim(0, 330)
#     
#   # Guardando plot
#   #ggsave(paste0("../graficas/", x, ".jpg"))
# })
# 
# # Ploteando todos
# 
# ggplot(data = indices_pokemon_go_1_251, aes(x = resistencia, y = ataque)) +
#   # Para que ggplot centre las coordenadas, necesitamos geom_point()
#   geom_point(alpha = 0) +
#   apply(indices_pokemon_go_1_251, 1, function(x){
#     ruta_sprite <- paste0(ruta_carpeta, "/", as.numeric(x[["no"]]), ".png")
#     prepara_sprite(ruta_sprite, as.numeric(x["resistencia"]), as.numeric(x["ataque"]), 10)
#   }) +
#   theme_bw() +
#   labs(x = "Resistencia", y = "Ataque") +
#   # Todas con los mismos límites para facilitar la interpretación
#   xlim(0, 350) +
#   ylim(0, 330)

# Guardando plot
#ggsave("../graficas/todos.jpg")

####

# Sin embargo, creo que conviene estandarizar porque la resistencia parece estar
# subvaluada (ver Lugia y Ho-oh):
indices_pokemon_go_1_251_estandarizado <- indices_pokemon_go_1_251 %>%
  mutate(
    # Por el tamaño de los sprites hay que hacer un ajuste
    ataque_indice = as.numeric(10 * scale(ataque)),
    resistencia_indice = as.numeric(10 * scale(resistencia))
  )
# Para ver el tamaño de los ejes
summary(indices_pokemon_go_1_251_estandarizado)

# Ploteando todos

ggplot(data = indices_pokemon_go_1_251_estandarizado,
  aes(x = resistencia_indice, y = ataque_indice)) +
  # Para que ggplot centre las coordenadas, necesitamos geom_point()
  geom_point(alpha = 0) +
  apply(indices_pokemon_go_1_251_estandarizado, 1, function(x){
    ruta_sprite <- paste0(ruta_carpeta, "/", as.numeric(x[["no"]]), ".png")
    prepara_sprite(ruta_sprite, as.numeric(x["resistencia_indice"]), as.numeric(x["ataque_indice"]), 1.5)
  }) +
  theme_bw() +
  labs(x = "Resistencia", y = "Ataque") +
  # Todas con los mismos límites para facilitar la interpretación
  xlim(-23, 47) +
  ylim(-27, 33)

# Guardando plot
#ggsave("../graficas/todos_estandarizado.jpg")


# Ahora se creará un PCA para encontrar resúmenes más informativos de las diferencias
# entre los estatus base de los pokémon. Es obvio que el reescalamiento afecta al PCA
# puesto que los ejes de la "elipse" cambian de ángulo, dando distintos índices.
pca <- prcomp(~ ataque_indice + resistencia_indice, data = indices_pokemon_go_1_251_estandarizado)

# Obvio que los dos componentes capturan el 100% de la varianza.
screeplot(pca, type = "lines", main = "Scree plot")

# Graficando el biplot, donde básicamente se plotean:
# 1. Los componentes principales como ejes.
# 2. Los pokémon con sus coordenadas en los componentes principales
# 3. Las variables originales como vectores, básicamente se plotea el ángulo entre
# ellas y los componentes.
biplot(pca, cex = c(0.6, 0.85), arrow.len = 0.05,
  xlab = "PC1 - fuerza total",
  ylab = "PC2 - defensa sobre ataque")

# Podemos ver que el CP1 está positivamente correlacionado tanto con resistencia
# como con ataque, por lo que puede ser un índice general de poder.
# El CP2 está positivamente correlacionado con resistencia, pero negativamente
# con ataque, por lo que entre más alto sea este índice para una especie, más
# está orientada a la defensa.

# Graficando:

pokemon_go_1_251_pca <- indices_pokemon_go_1_251_estandarizado %>%
  cbind(pca$x) %>%
  rename(
    pc1_fuerza_total = PC1,
    pc2_defensa_sobre_ataque = PC2
  )
summary(pokemon_go_1_251_pca)

ggplot(data = pokemon_go_1_251_pca, aes(x = pc1_fuerza_total, y = pc2_defensa_sobre_ataque)) +
  # Para que ggplot centre las coordenadas, necesitamos geom_point()
  geom_point(alpha = 0) +
  apply(pokemon_go_1_251_pca, 1, function(x){
    ruta_sprite <- paste0(ruta_carpeta, "/", as.numeric(x[["no"]]), ".png")
    prepara_sprite(ruta_sprite, as.numeric(x["pc1_fuerza_total"]), as.numeric(x["pc2_defensa_sobre_ataque"]), 1.5)
  }) +
  theme_bw() +
  labs(x = "PC1: Fuerza total", y = "PC2: Defensa sobre ataque") +
  # Todas con los mismos límites para facilitar la interpretación
  xlim(-30, 34) +
  ylim(-17, 39)

# Guardando plot
#ggsave("../graficas/pca.jpg")

### Revisando la correlación entre CP máximo y PC1

source("2_pokemon_go_calcular_estadisticas_individuales_pokemon.R")

# Calculando HP, ataque, defensa  y CP máximo de cada Pokémon:
estadisticas_maximas_pokemon_go <- ldply(unique(pokemon_go_1_251_pca$nombre), function(x){
  calcula_estadisticas_individuales(x, 15, 15, 15, 40)
}) %>%
  select(
    no,
    contains("maxim")
  )

# Uniendo data frames:
pokemon_go_1_251_pca_estadisticas_maximas <- pokemon_go_1_251_pca %>%
  inner_join(estadisticas_maximas_pokemon_go, by = c("no"))

# Ploteando:
ggplot(data = pokemon_go_1_251_pca_estadisticas_maximas,
  aes(x = pc1_fuerza_total, y = pc2_defensa_sobre_ataque, colour = cp_maximo)) +
  geom_point() +
  scale_color_gradientn(colours = rainbow(5))

ggplot(data = pokemon_go_1_251_pca_estadisticas_maximas,
  aes(x = pc1_fuerza_total, y = cp_maximo)) +
  geom_point() +
  geom_smooth()
# Podemos ver cómo diferencias en CP tienden a sobreestimar diferencias en poder
# especialmente para pokémon poderosos. Podemos razonar que esta tendencia se da
# también pra individuos de la misma especie pero distintos IV's.


# Se ve una gran correlación entre PC1_fuerza_total y CP. Revisándola:
pokemon_go_1_251_pca_estadisticas_maximas %>%
  select(
    pc1_fuerza_total,
    pc2_defensa_sobre_ataque,
    cp_maximo) %>%
  cor() %>%
  round(5)
# Hay una gran correlación entre cp máximo y PC1_fuerza_total.

# Finalmente, generando la tabla de uso práctico:
pokemon_go_1_251_tabla_uso_practico <- pokemon_go_1_251_pca_estadisticas_maximas %>%
  # Sólo me voy a quedar con los que son mñas poderosos que kadabra: la preevolución
  # más poderosa
  filter(pc1_fuerza_total > pc1_fuerza_total[nombre == "kadabra"]) %>%
  ddply(.variables = .(tipo), function(df){
    resultado <- df %>%
      arrange(desc(pc1_fuerza_total)) %>%
      select(
        no,
        nombre,
        pc1_fuerza_total,
        pc2_defensa_sobre_ataque,
        cp_maximo
      ) %>%
      head(10)
    
    return(resultado)
  })
#write_csv(pokemon_go_1_251_tabla_uso_practico, "../datos/pokemon_go_1_251_tabla_uso_practico.csv")







