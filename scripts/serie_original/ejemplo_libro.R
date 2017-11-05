source("../funciones_auxiliares.R")
library("ggplot2")

# Obteniendo status y sprites de los pokémon de interés:
# status_base_pokemon_1_721 <- obtener_status(1:721)
# almacena_sprites(1:721, "../../sprites/1_721/")

# Nota: para pokémon con múltiples formas (como Wormadam), la poke-api sólo da
# info de una de ellas.
#saveRDS(status_base_pokemon_1_721, "../../datos/serie_original/status_base_pokemon_1_721.RData")

status_base_pokemon_1_721 <- readRDS("../../datos/serie_original/status_base_pokemon_1_721.RData")
summary(status_base_pokemon_1_721)
# Se ven más o menos balanceados.

######################################
#### Calculando variables adicionales
######################################

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
    generacion = case_when(
      no <= 151 ~ 1,
      151 < no & no <= 251 ~ 2,
      251 < no & no <= 386 ~ 3,
      386 < no & no <= 493 ~ 4,
      493 < no & no <= 649 ~ 5,
      650 < no & no <= 721 ~ 6
      #721 < no & no <= 802 ~ 7 Aún no están en la poke-api.
    ),
    
    # Viendo si son atacantes físicos, neutros o especiales
    tipo_atacante = case_when(
      ataque - ataque_especial > 10 ~ "físico",
      abs(ataque - ataque_especial) <= 10 ~ "neutro",
      ataque - ataque_especial < -10 ~ "especial"
    ),
    
    # Viendo si son defensores físicos, neutros o especiales. Cabe destacar que,
    # en general, un individuo sí puede ser selectivo en cuanto a ataque (físico
    # o especial), pero en cuanto a defensa conviene que sea generalista.
    tipo_defensor = case_when(
      defensa - defensa_especial > 10 ~ "físico",
      abs(defensa - defensa_especial) <= 10 ~ "neutro",
      defensa - defensa_especial < -10 ~ "especial"
    ),
    
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
    
    # Otras estadísticas cuyo uso es más estratégico (en ciertos casos, por ejemplo,
    # al hacer un cambio de Pokémon en una batalla):
    resistencia_fisica = sqrt(defensa * hp),
    resistencia_especial = sqrt(defensa_especial * hp),
    
    # Variables de experto. Se usa la media geométrica porque son variables que
    # no se pueden comparar entre si:
    fuerza_total = (capacidad_ofensiva * capacidad_defensiva * sqrt(rapidez)) ^ (1/3),
    sweeper_sobre_tanque = sqrt(rapidez / capacidad_defensiva),
    # Se saca la raíz cuadrada porque Shedinja no deja visualizar bien los datos
    
    fuerza_total_estandarizada = estandariza(fuerza_total) * 10,
    sweeper_sobre_tanque_estandarizada = estandariza(sweeper_sobre_tanque) * 10
    ) %>%
  filter(no <= 493)

summary(datos_procesados_pokemon_1_493)
# Bastante no correlacionadas, súper bien!!
cor(
  datos_procesados_pokemon_1_493$fuerza_total_estandarizada,
  datos_procesados_pokemon_1_493$sweeper_sobre_tanque_estandarizada)

# saveRDS(datos_procesados_pokemon_1_493, "../../datos/serie_original/datos_procesados_pokemon_1_493.RData")

################################
#### Ejemplo de PCA exploratorio
################################

# Este análisis fue el que motivó la creación de las variables: fuerza total y
# sweeper sobre tanque.

# Se calculará un PCA utilizando:
# 1. Capacidad ofensiva
# 2. Capacidad defensiva
# 3. Rapidez
# Para darnos una idea general de cómo están distribuídos los Pokémon en estos 3
# status:

pca <- prcomp(
  ~ capacidad_ofensiva + capacidad_defensiva + sqrt(rapidez),
  data = datos_procesados_pokemon_1_493,
  center = TRUE, scale. = TRUE)

screeplot(pca, type = "lines", main = "Scree plot")
# De acuerdo con la regla del codo, podemos utilizar los dos primeros componentes

# Graficando el biplot, donde básicamente se plotean:
# 1. Los componentes principales como ejes.
# 2. Los pokémon con sus coordenadas en los componentes principales
# 3. Las variables originales como vectores, básicamente se plotea el ángulo entre
# ellas y los componentes.
biplot(pca, cex = c(0.6, 0.85), arrow.len = 0.05,
  xlab = "PC1 - índice de fuerza total",
  ylab = "PC2 - sprinter")

# Viendo los loadings:
pca$rotation

# Podemos ver que el CP1 correlaciona con las 3 variables: capacidad ofensiva,
# defensiva y rapidez positivamente, por lo que puede ser un índice general de
# poder.

# El CP2 está positivamente correlacionado con rapidez, pero negativamente
# con capacidad defensiva, por lo que una especie con este CP elevado, tiende a
# ser un sweeper para su nivel de poder: suponiendo CP1 constante, a mayor CP2,
# menor defensa (y por lo tanto, mayor porcentaje de la fuerza total dedicada al
# ataque), y más velocidad.

# El PCA captura la variabilidad de los datos, y es una muy útil primera aproximación
# a obtener índices, pero puede no ajustarse completamente a la interpretación
# que deseamos. Por ejemplo, tal vez el CP1 le da más importancia a la rapidez de
# lo que se quisiera.

# Debido a lo anterior, en el data frame "datos_procesados_pokemon_1_493" se
# incluyeron las variables: "fuerza_total" y "sweeper_sobre_tanque". Basadas
# en los resultados del PCA pero

################################
#### Graficando
################################

datos_plot_pokemon_1_493 <- datos_procesados_pokemon_1_493 %>%
  gather(key = "aux", value = "tipo", tipo_1, tipo_2)  %>%
  select(-aux) %>%
  filter(!is.na(tipo))
ruta_carpeta <- "../../sprites/1_721"

# Graficando al fin: gráficas por tipo
d_ply(datos_plot_pokemon_1_493, .(tipo), function(df){
  ggplot(data = df, aes(x = fuerza_total_estandarizada, y = sweeper_sobre_tanque_estandarizada)) +
    # Para que ggplot centre las coordenadas, necesitamos geom_point()
    geom_point(alpha = 0) +
    apply(df, 1, function(x){
      ruta_sprite <- paste0(ruta_carpeta, "/", as.numeric(x[["no"]]), ".png")
      prepara_sprite(
        ruta_sprite, as.numeric(x["fuerza_total_estandarizada"]),
        as.numeric(x["sweeper_sobre_tanque_estandarizada"]), 1.5)
    }) +
    theme_bw() +
    labs(x = "Fuerza total", y = "Sweeper sobre tanque") +
    # Todas con los mismos límites para facilitar la interpretación
    xlim(-30, 30) +
    ylim(-32, 65)
  
  ruta_grafica <- paste0("../../graficas/serie_original/", unique(df$tipo), ".jpg")
  ggsave(ruta_grafica)
})

# Dejar anidad más expresiones
options(expressions=10000)

# Y gráfica general:
ggplot(datos_plot_pokemon_1_493, aes(x = fuerza_total_estandarizada, y = sweeper_sobre_tanque_estandarizada)) +
  # Para que ggplot centre las coordenadas, necesitamos geom_point()
  geom_point(alpha = 0) +
  apply(datos_plot_pokemon_1_493, 1, function(x){
    ruta_sprite <- paste0(ruta_carpeta, "/", as.numeric(x[["no"]]), ".png")
    prepara_sprite(ruta_sprite, as.numeric(x["fuerza_total_estandarizada"]), as.numeric(x["sweeper_sobre_tanque_estandarizada"]), 1.5)
  }) +
  theme_bw() +
  labs(x = "Fuerza total", y = "Sweeper sobre tanque") +
  # Todas con los mismos límites para facilitar la interpretación
  xlim(-30, 30) +
  ylim(-32, 65)

ruta_grafica <- "../../graficas/serie_original/todos.jpg"
ggsave(ruta_grafica)




