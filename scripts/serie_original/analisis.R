# Leyendo el archivo de configuración
source("config.R")
source(ruta_archivo_funciones_auxiliares)

# Obteniendo status y sprites de los pokémon de interés:
# status_base_pokemon_1_721 <- obtener_status(1:721)
# almacena_sprites(1:721, ruta_carpeta_sprites)
# Nota: para pokémon con múltiples formas (como Wormadam), la poke-api sólo da
# info de una de ellas.

#saveRDS(status_base_pokemon_1_721, ruta_archivo_datos_status_base)

status_base_pokemon_1_721 <- readRDS(ruta_archivo_datos_status_base)
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
    
    # Como un pokémon puede ser especialista en cuanto a ataque, calcular su
    # capacidad ofensiva como el máximo entre el normal y el especial. Sin embargo,
    # la rapidez también es importante, aunque en menor medida. Por ello se
    # hace una media geométrica ponderada.
    capacidad_ofensiva = (pmax(ataque, ataque_especial) ^ 2 * rapidez) ^ (1/3), 

    # Capacidad defensiva será la media geométrica de:
    # La suma entre la defensa física y la especial (no queremos penalizar a
    # pokémon con discrepancias en estas características, ya que los consideramos
    # de uso estratégico muy útil).
    # El HP, que importa para ambas defensas
    # HP. Notar que al HP se le da el doble de peso que a cada defensa en particular,
    # porque éste importa para cualquier tipo de ataque, sea físico o especial:
    capacidad_defensiva = sqrt(hp * (defensa + defensa_especial) / 2),

    # Otras estadísticas cuyo uso es más estratégico (en ciertos casos, por ejemplo,
    # al hacer un cambio de Pokémon en una batalla):
    resistencia_fisica = sqrt(defensa * hp),
    resistencia_especial = sqrt(defensa_especial * hp),
    
    # Y para saber si un Pokémon es atacante físico o especial:
    ofensiva_fisica = (ataque ^ 2 * rapidez) ^ (1/3),
    ofensiva_especial = (ataque_especial ^ 2 * rapidez) ^ (1/3)
    
  ) %>%
  filter(no <= 493)
summary(datos_procesados_pokemon_1_493)

#######################################
#### PCA capacidad ofensiva/ defensiva
#######################################

# Este PCA sirve para darnos una idea general de cómo están distribuídos los
# Pokémon considerando estos dos status:

pca_capacidad_ofensiva_defensiva <- prcomp(
  ~ capacidad_ofensiva + capacidad_defensiva,
  data = datos_procesados_pokemon_1_493,
  center = TRUE, scale = TRUE)

# Viendo los eigenvalores y eigenvectores de la matriz de correlación:
pca_capacidad_ofensiva_defensiva$rotation
pca_capacidad_ofensiva_defensiva$sdev
# Que los loadings den el mismo peso a ambas variables es una consecuencia de
# diagonalizar una matriz de correlación de 2 variables.

# Graficando el biplot, donde básicamente se plotean:
# 1. Los componentes principales como ejes.
# 2. Los pokémon con sus coordenadas en los componentes principales
# 3. Las variables originales como vectores, básicamente se plotea el ángulo entre
# ellas y los componentes.
biplot(pca_capacidad_ofensiva_defensiva, cex = c(0.6, 0.85), arrow.len = 0.05,
  xlab = "PC1 - índice de fuerza total",
  ylab = "PC2 - tanque sobre sweeper")

# Podemos ver que el CP1 correlaciona con ambas variables, por lo que puede ser
# un índice general de poder.

# El CP2 está positivamente correlacionado con capacidad defensiva, pero
# negativamente relacionado con capacidad ofensiva, por lo que una especie con
# este CP elevado, tiende a ser un tanque para su nivel de poder.

#######################################
#### PCA resistencia física / especial
#######################################

pca_resistencia_fisica_especial <- prcomp(
  ~ resistencia_fisica + resistencia_especial,
  data = datos_procesados_pokemon_1_493,
  center = TRUE, scale = TRUE)

biplot(pca_resistencia_fisica_especial, cex = c(0.6, 0.85), arrow.len = 0.05,
  xlab = "PC1 - resistencia total",
  ylab = "PC2 - resistencia física sobre especial")

# Lo esperado, nada más hay que agregar las variables al data frame

#######################################
#### PCA ofensiva física / especial
#######################################

pca_ofensiva_fisica_especial <- prcomp(
  ~ ofensiva_fisica + ofensiva_especial,
  data = datos_procesados_pokemon_1_493,
  center = TRUE, scale = TRUE)

biplot(pca_ofensiva_fisica_especial, cex = c(0.6, 0.85), arrow.len = 0.05,
  xlab = "PC1 - ofensiva total",
  ylab = "PC2 - ofensiva física sobre especial")

# Lo esperado, nada más hay que agregar las variables al data frame

######################################################
#### Agregando variables de PCA (y otras) a la tabla
######################################################

indices_pokemon_1_493 <- datos_procesados_pokemon_1_493 %>%
  
  # Agregando columnas del primer PCA:
  cbind(as_data_frame(pca_capacidad_ofensiva_defensiva$x)) %>%
  mutate(
    pc1_fuerza_total = PC1 * 10 ,
    pc2_sweeper_sobre_tanque = -PC2 * 10,
    
    # Variables de experto. Se usa la media geométrica porque son variables que
    # no se pueden comparar entre si:
    fuerza_total_experto = (sqrt(capacidad_ofensiva * capacidad_defensiva) %>%
      estandariza()) * 10,
    
    # Elevado a la 1/4 en lugar de a la 1/2 porque shedinja no deja visualizar los
    # datos
    sweeper_sobre_tanque_experto = ((capacidad_ofensiva / capacidad_defensiva)^(1/4) %>%
      estandariza()) * 10
    # Se saca la raíz cuadrada porque Shedinja no deja visualizar bien los datos
  ) %>%
  select(
    -PC1,
    -PC2
  ) %>%
  
  # Agregando columnas del segundo PCA:
  cbind(as_data_frame(pca_resistencia_fisica_especial$x)) %>%
  mutate(
    pc1_resistencia_total = PC1 * 10 ,
    pc2_resistencia_fisica_sobre_especial = PC2 * 10
  ) %>%
  select(
    -PC1,
    -PC2
  ) %>%
  
  # Agregando columnas del tercer PCA:
  cbind(as_data_frame(pca_ofensiva_fisica_especial$x)) %>%
  mutate(
    pc1_ofensiva_total = PC1 * 10 ,
    pc2_ofensiva_fisica_sobre_especial = PC2 * 10
  ) %>%
  select(
    -PC1,
    -PC2
  )

# Correlaciones:
indices_pokemon_1_493 %>%
  select(
    pc1_fuerza_total,
    pc2_sweeper_sobre_tanque,
    fuerza_total_experto,
    sweeper_sobre_tanque_experto
  ) %>%
  cor()
# Se ven más o menos bien. Están tan correlacionados que para términos prácticos
# usaré el de PCA.

# En realidad, para hacer más robustas las variables obtenidas mediante PCA,
# son tan buenas que se pueden definir las variables de experto basadas en ellas
# como ellas mismas (esto se refleja mediante hardcodeo de ellas en el código,
# sin embargo, teóricamente es una profunda diferencia)

###################################################
#### Graficando PCA capacidad ofensiva / defensiva
###################################################

# summary(datos_plot_pokemon_1_493)
datos_plot_pokemon_1_493 <- indices_pokemon_1_493 %>%
  gather(key = "aux", value = "tipo", tipo_1, tipo_2)  %>%
  select(-aux) %>%
  filter(!is.na(tipo))

### Gráficas por tipo:
d_ply(datos_plot_pokemon_1_493, .(tipo), function(df){
  grafica_pokemon(
    df = df,
    x = "pc1_fuerza_total",
    y = "pc2_sweeper_sobre_tanque",
    x_lab = "PC1 - Fuerza total",
    y_lab = "PC2 - Sweeper sobre tanque",
    x_lim = c(-30, 35),
    y_lim = c(-32, 30),
    no = "no",
    ruta_carpeta_sprites = ruta_carpeta_sprites
  )
    
  ruta_grafica <- paste0(
    ruta_carpeta_graficas_serie_original,
    "/fuerza_total/", unique(df$tipo), ".jpg")
  
  ggsave(ruta_grafica, width = 11, height = 7, units = "in")
})

# Dejar anidar más expresiones antes de crear la gráfica general
options(expressions=10000)

# Y gráfica general:
grafica_pokemon(
  df = indices_pokemon_1_493,
  x = "pc1_fuerza_total",
  y = "pc2_sweeper_sobre_tanque",
  x_lab = "PC1 - Fuerza total",
  y_lab = "PC2 - Sweeper sobre tanque",
  x_lim = c(-30, 35),
  y_lim = c(-32, 30),
  no = "no",
  ruta_carpeta_sprites = ruta_carpeta_sprites
)

ruta_grafica <- paste0(ruta_carpeta_graficas_serie_original, "/fuerza_total/todos.jpg")
ggsave(ruta_grafica, width = 11, height = 7, units = "in")

###################################################
#### Graficando PCA resistencia física / especial
###################################################

# summary(indices_pokemon_1_493)
### Gráficas por tipo:
d_ply(datos_plot_pokemon_1_493, .(tipo), function(df){
  grafica_pokemon(
    df = df,
    x = "pc1_resistencia_total",
    y = "pc2_resistencia_fisica_sobre_especial",
    x_lab = "Resistencia total",
    y_lab = "Escudo sobre espejo",
    x_lim = c(-45, 45),
    y_lim = c(-45, 20),
    no = "no",
    ruta_carpeta_sprites = ruta_carpeta_sprites
  )
  
  ruta_grafica <- paste0(
    ruta_carpeta_graficas_serie_original,
    "/resistencia/", unique(df$tipo), ".jpg")
  
  ggsave(ruta_grafica, width = 11, height = 7, units = "in")
})

grafica_pokemon(
  df = indices_pokemon_1_493,
  x = "pc1_resistencia_total",
  y = "pc2_resistencia_fisica_sobre_especial",
  x_lab = "Resistencia total",
  y_lab = "Escudo sobre espejo",
  x_lim = c(-45, 45),
  y_lim = c(-45, 20),
  no = "no",
  ruta_carpeta_sprites = ruta_carpeta_sprites
)
  
ruta_grafica <- paste0(ruta_carpeta_graficas_serie_original, "/resistencia/todos.jpg")
ggsave(ruta_grafica, width = 11, height = 7, units = "in")

###################################################
#### Graficando PCA ofensiva física / especial
###################################################

# summary(indices_pokemon_1_493)
### Gráficas por tipo:
d_ply(datos_plot_pokemon_1_493, .(tipo), function(df){
  grafica_pokemon(
    df = df,
    x = "pc1_ofensiva_total",
    y = "pc2_ofensiva_fisica_sobre_especial",
    x_lab = "Ofensiva total",
    y_lab = "Espada sobre magia",
    x_lim = c(-40, 51),
    y_lim = c(-23, 20),
    no = "no",
    ruta_carpeta_sprites = ruta_carpeta_sprites
  )
  
  ruta_grafica <- paste0(
    ruta_carpeta_graficas_serie_original,
    "/ofensiva/", unique(df$tipo), ".jpg")
  
  ggsave(ruta_grafica, width = 11, height = 7, units = "in")
})

grafica_pokemon(
  df = indices_pokemon_1_493,
  x = "pc1_ofensiva_total",
  y = "pc2_ofensiva_fisica_sobre_especial",
  x_lab = "Ofensiva total",
  y_lab = "Espada sobre magia",
  x_lim = c(-40, 51),
  y_lim = c(-23, 20),
  no = "no",
  ruta_carpeta_sprites = ruta_carpeta_sprites
)

ruta_grafica <- paste0(ruta_carpeta_graficas_serie_original, "/ofensiva/todos.jpg")
ggsave(ruta_grafica, width = 11, height = 7, units = "in")
