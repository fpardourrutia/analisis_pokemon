source("../funciones_auxiliares.R")
library("ggplot2")

status_base_pokemon_1_721 <- readRDS("../../datos/serie_original/status_base_pokemon_1_721.RData")
summary(status_base_pokemon_1_721)
# Se ven más o menos balanceados.

# Ahora se calcularán variables adicionales (de experto), para los pokémon de interés (1-493):
datos_procesados_pokemon_1_493 <- status_base_pokemon_1_721 %>%
  rename(
    nombre = name,
    tipo_1 = type_1,
    tipo_2 = type_2,
    ataque = attack,
    defensa = defense,
    ataque_especial = special_attack,
    defensa_especial = special_defense,
    rapidez = speed
  ) %>%
  mutate(
    
    # Como un pokémon puede ser generalista en cuanto a ataque, calcular su
    # ataque como el máximo entre el normal y el especial:
    capacidad_ofensiva = pmax(ataque, ataque_especial),
    
    # La defensa considerada será la media geométrica de ambas defensas, ya que
    # preferimos pokémon balanceados en este ámbito.
    defensa_considerada =  sqrt(defensa * defensa_especial),
    
    # Capacidad defensiva será la media geométrica de la defensa considerada y el
    # HP. Notar que al HP se le da el doble de peso que a cada defensa en particular,
    # porque éste importa para cualquier tipo de ataque, sea físico o especial:
    capacidad_defensiva = sqrt(defensa_considerada * hp),
    
    # Se modelará que la rapidez es de importancia decreciente:
    rapidez_util = sqrt(rapidez)
    ) %>%
  filter(no <= 493)

# Ahora se calculará un PCA utilizando:
# 1. Capacidad ofensiva
# 2. Capacidad defensiva
# 3. Rapidez
# Para darnos una idea general de cómo están distribuídos los Pokémon en estos 3
# status:

pca_capacidad_rapidez <- prcomp(
  ~ capacidad_ofensiva + capacidad_defensiva + rapidez_util,
  data = datos_procesados_pokemon_1_493,
  center = TRUE, scale. = TRUE)
summary(pca_capacidad_rapidez)

screeplot(pca_capacidad_rapidez, type = "lines", main = "Scree plot")
# De acuerdo con la regla del codo, podemos utilizar los dos primeros componente

# Graficando el biplot, donde básicamente se plotean:
# 1. Los componentes principales como ejes.
# 2. Los pokémon con sus coordenadas en los componentes principales
# 3. Las variables originales como vectores, básicamente se plotea el ángulo entre
# ellas y los componentes.
biplot(pca_capacidad_rapidez, cex = c(0.6, 0.85), arrow.len = 0.05,
  xlab = "PC1 - índice de fuerza total",
  ylab = "PC2 - sprinter")

# Podemos ver que el CP1 correlaciona positivamente con las 3 variables: capacidad
# ofensiva, defensiva y rapidez útil, positivamente, por lo que puede ser un
# índice general de
# poder.
# El CP2 está positivamente correlacionado con rapidez, pero negativamente
# con capacidad defensiva, por lo que una especie con este CP elevado, para tener
# esperanza en batalla, debe tener buena capacidad ofensiva (para ser un sweeper).
# Si no ataca primero y lo matan en un segundo (como Farfetch'd).

ruta_carpeta <- "../../sprites/1_721"

pca_pokemon_1_493 <- datos_procesados_pokemon_1_493 %>%
  cbind(pca_capacidad_rapidez$x) %>%
  mutate(
    pc1_fuerza_total = PC1 * 10,
    pc2_sprinter = PC2 * 10,
    pc3 = PC3 * 10
  ) %>%
  select(-PC1, -PC2, -PC3)

# Revisando algunas correlaciones
pca_pokemon_1_493 %>%
  select(
    ataque:rapidez,
    capacidad_ofensiva,
    defensa_considerada,
    capacidad_defensiva,
    pc1_fuerza_total,
    pc2_sprinter,
    pc3
  ) %>%
  cor()

# Graficando

ruta_carpeta_sprites <- "../../sprites/1_721/"

d_ply(pca_pokemon_1_493, .(tipo_1), function(df){
  ggplot(data = df, aes(x = pc1_fuerza_total, y = pc2_sprinter)) +
    # Para que ggplot centre las coordenadas, necesitamos geom_point()
    geom_point(alpha = 0) +
    apply(df, 1, function(x){
      ruta_sprite <- paste0(ruta_carpeta_sprites, "/", as.numeric(x[["no"]]), ".png")
      prepara_sprite(ruta_sprite, as.numeric(x["pc1_fuerza_total"]), as.numeric(x["pc2_sprinter"]), 1.5)
    }) +
    theme_bw() +
    labs(x = "Fuerza total", y = "Sweeper sobre tanque") +
    # Todas con los mismos límites para facilitar la interpretación
    xlim(-30, 40) +
    ylim(-30, 35)
  
  ruta_grafica <- paste0("../../graficas/serie_original/", unique(df$tipo), ".jpg")
  ggsave(ruta_grafica)
})


ggplot(data = pca_pokemon_1_493, aes(x = pc1_fuerza_total, y = pc2_sprinter)) +
  # Para que ggplot centre las coordenadas, necesitamos geom_point()
  geom_point(alpha = 0) +
  apply(pca_pokemon_1_493, 1, function(x){
    ruta_sprite <- paste0(ruta_carpeta_sprites, "/", as.numeric(x[["no"]]), ".png")
    prepara_sprite(ruta_sprite, as.numeric(x["pc1_fuerza_total"]), as.numeric(x["pc2_sprinter"]), 1.5)
  }) +
  theme_bw() +
  labs(x = "Fuerza total", y = "Sweeper sobre tanque") +
  # Todas con los mismos límites para facilitar la interpretación
  xlim(-30, 40) +
  ylim(-30, 35)

ruta_grafica <- "../../graficas/serie_original/pca.jpg"
ggsave(ruta_grafica)


  
# Sin embargo, esta combinación de PC1 y PC2 no es tan intuitiva porque, entre
# más alto PC2, más alto queremos que sea PC1 (puesto que queremos que el pokémon
# sea un sweeper y no un sprinter débil). Una corrección intuitiva es tomar a
# Blastoise como el nuevo centro y el vector director de Blastoise a Charizard
# como el nuevo eje Y (reproyectar todos los pokémon a dichas coordenadas)
# Ver la gráfica siguiente si es necesario:

# ggplot(data = pca_pokemon_1_493[1:9,], aes(x = pc1_fuerza_total, y = pc2_sprinter)) +
#   # Para que ggplot centre las coordenadas, necesitamos geom_point()
#   geom_point(alpha = 0) +
#   apply(pca_pokemon_1_493[1:9,], 1, function(x){
#     ruta_sprite <- paste0(ruta_carpeta, "/", as.numeric(x[["no"]]), ".png")
#     prepara_sprite(ruta_sprite, as.numeric(x["pc1_fuerza_total"]), as.numeric(x["pc2_sprinter"]), 1.5)
#   }) +
#   theme_bw() +
#   labs(x = "PC1: Fuerza total", y = "PC2: Sprinter") +
#   # Todas con los mismos límites para facilitar la interpretación
#   xlim(-30, 40) +
#   ylim(-30, 35)

vector_blastoise <- pca_pokemon_1_493 %>%
  filter(nombre == "blastoise") %>%
  select(
    pc1_fuerza_total,
    pc2_sprinter
  ) %>%
  as.numeric()

vector_charizard <- pca_pokemon_1_493 %>%
  filter(nombre == "charizard") %>%
  select(
    pc1_fuerza_total,
    pc2_sprinter
  ) %>%
  as.numeric()

vector_blastoise_charizard <- vector_charizard - vector_blastoise

vector_normal_blastoise_charizard <- c(
  vector_blastoise_charizard[2],
  -vector_blastoise_charizard[1]
  )

indicadores_fortaleza_pokemon <- pca_pokemon_1_493 %>%
  rowwise() %>%
  do(
    no = .$no,
    # La proyección ortogonal de la diferencia entre el pokémon y Blastoise
    # en la dirección del vector de Blastoise a Charizard será el eje Y:
    coordenadas_centro_blastoise = c(
      .$pc1_fuerza_total - vector_blastoise[1],
      .$pc2_sprinter - vector_blastoise[2])
  ) %>%
  do(
    no = .$no,
    # Proyectando sobre "vector_blastoise_charizard". Notar que sólo nos interesan
    # las coordenadas, a final de cuentas, estandarizaremos los resultados de nuevo
    # y cambiará el centro y la dispersión de los datos.
    eje_y_sweeper_sobre_tanque_aux = (.$coordenadas_centro_blastoise %*% vector_blastoise_charizard) /
      (vector_blastoise_charizard %*% vector_blastoise_charizard),
    
    # Proyectando sobre vector_normal_blastoise_charizard
    eje_x_fuerza_corregida_aux = (.$coordenadas_centro_blastoise %*% vector_normal_blastoise_charizard) /
        (vector_normal_blastoise_charizard %*% vector_normal_blastoise_charizard)
  ) %>%
  ungroup() %>%
  mutate(
    no = as.numeric(no),
    eje_x_fuerza_corregida_aux = as.numeric(eje_x_fuerza_corregida_aux),
    eje_y_sweeper_sobre_tanque_aux = as.numeric(eje_y_sweeper_sobre_tanque_aux)
  ) %>%
  
  # Si se calcula la media y sd en el mismo mutate, la calcula mal por alguna razón
  mutate(
    # Escalando
    eje_x_fuerza_corregida = (eje_x_fuerza_corregida_aux - mean(eje_x_fuerza_corregida_aux)) * 10 /
      sd(eje_x_fuerza_corregida_aux),
    eje_y_sweeper_sobre_tanque = (eje_y_sweeper_sobre_tanque_aux - mean(eje_y_sweeper_sobre_tanque_aux)) * 10 /
      sd(eje_y_sweeper_sobre_tanque_aux)
  ) %>%
  
  # Uniendo para obtener las columnas originales
  inner_join(pca_pokemon_1_493, by = "no") %>%
  
  # Agrupando los tipos en una columna para graficar por tipo de pokémon (se
  # usará un d_ply porque facet_wrap() no funciona en este caso:
  gather(key = "aux", value = "tipo", tipo_1, tipo_2) %>%
  # Quitando artefactos del gather
  filter(!is.na(tipo)) %>%
  
  # Seleccionando columnas de interés
  select(
    no,
    nombre,
    generacion,
    tipo,
    ataque,
    defensa,
    hp,
    ataque_especial,
    defensa_especial,
    rapidez,
    tipo_atacante,
    tipo_defensor,
    capacidad_ofensiva,
    defensa_considerada,
    capacidad_defensiva,
    resistencia_fisica,
    resistencia_especial,
    eje_x_fuerza_corregida,
    eje_y_sweeper_sobre_tanque
  )

# Calculando correlaciones entre variables:
indicadores_fortaleza_pokemon %>%
  select(
    ataque,
    defensa,
    hp,
    ataque_especial,
    defensa_especial,
    rapidez,
    eje_x_fuerza_corregida,
    eje_y_sweeper_sobre_tanque
  ) %>%
  cor()

# Están correlacionadas las variables "eje_x_fuerza_corregida" y "eje_y_sweeper_sobre_tanque".
# Creí que convenía hacer un PCA adicional para "enderezar los ejes", sin embargo,
# me di cuenta que esta es la clave para tener un indicador con una muestra:
# si puedo delinear ejes que miden lo que me gustaría medir, con algunos ejemplos
# particulares (en este caso, determiné que Charizard y Blastoise tienen la misma
# fuerza total, )



# Graficando al fin

ruta_carpeta_sprites <- "../../sprites/1_721/"

d_ply(indicadores_fortaleza_pokemon, .(tipo), function(df){
  ggplot(data = df, aes(x = eje_x_fuerza_corregida, y = eje_y_sweeper_sobre_tanque)) +
    # Para que ggplot centre las coordenadas, necesitamos geom_point()
    geom_point(alpha = 0) +
    apply(df, 1, function(x){
      ruta_sprite <- paste0(ruta_carpeta_sprites, "/", as.numeric(x[["no"]]), ".png")
      prepara_sprite(ruta_sprite, as.numeric(x["eje_x_fuerza_corregida"]), as.numeric(x["eje_y_sweeper_sobre_tanque"]), 1.5)
    }) +
    theme_bw() +
    labs(x = "Fuerza total", y = "Sweeper sobre tanque") +
    # Todas con los mismos límites para facilitar la interpretación
    xlim(-30, 40) +
    ylim(-30, 35)
  
  ruta_grafica <- paste0("../../graficas/serie_original/", unique(df$tipo), ".jpg")
  ggsave(ruta_grafica)
})

options(expressions=10000)

ggplot(data = indicadores_fortaleza_pokemon, aes(x = eje_x_fuerza_corregida, y = eje_y_sweeper_sobre_tanque)) +
  # Para que ggplot centre las coordenadas, necesitamos geom_point()
  geom_point(alpha = 0) +
  apply(indicadores_fortaleza_pokemon, 1, function(x){
    ruta_sprite <- paste0(ruta_carpeta_sprites, "/", as.numeric(x[["no"]]), ".png")
    prepara_sprite(ruta_sprite, as.numeric(x["eje_x_fuerza_corregida"]), as.numeric(x["eje_y_sweeper_sobre_tanque"]), 1.5)
  }) +
  theme_bw() +
  labs(x = "Fuerza total", y = "Sweeper sobre tanque") +
  # Todas con los mismos límites para facilitar la interpretación
  xlim(-30, 40) +
  ylim(-30, 35)

ruta_grafica <- "../../graficas/serie_original/todos.jpg"
ggsave(ruta_grafica)




