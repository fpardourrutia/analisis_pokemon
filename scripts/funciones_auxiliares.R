library("plyr")
library("dplyr")
library("tidyr")
library("httr")
library("purrr")
library("png") # para leer imágenes PNG
library("grid") # para plotear PNG's (rasterGrob())
library("ggplot2")

# La siguiente función recibe un vector con números de pokémon y descarga su
# nombre, tipo(s) y status en formato de data frame:
# numeros_pokemon: un vector cuyas entradas son los números nacionales de los
# pokémon de interés
# La función consulta la Pokeapi: http://pokeapi.co y regresa un data frame que
# contiene:
#   - El número de pokémon
#   - El nombre del pokémon
#   - El tipo(s) de pokémon
#   - Los valores base del pokémon: hp, ataque, defensa, ataque especial, defensa
#   especial, rapidez.
obtener_status <- function(numeros_pokemon){
  # Generando URL's a consultar:
  urls <- paste0("http://pokeapi.co/api/v2/pokemon/", numeros_pokemon)
  
  # Nombrando urls para que salga el número de Pokémon en el resultado
  names(urls) <- numeros_pokemon
  
  # Ejemplo:
  # x <- "http://pokeapi.co/api/v2/pokemon/100"
  # GET(x) %>%
  # content("parsed") %>%
  # str(max.level = 1)
  
  # Consultando las URL's y extrayendo datos de interés:
  resultado <- ldply(urls, function(x){
    
    # Notificación de URL visitada
    print(x)
    
    # Obteniendo JSON de un pokémon (URL) y parseándolo como lista
    contenido_respuesta <-  GET(x) %>%
      content("parsed")
    
    # Generando DF de status para un pokémon
    df_status <- ldply(contenido_respuesta$stats, function(x){
      datos_status <- data_frame(
        stat_name = x$stat$name,
        base_power = x$base_stat
      )
    })
    
    # Generando DF de tipos para un pokémon
    # Nombrando tipos en orden de aparición
    names(contenido_respuesta$types) <- map(contenido_respuesta$types, function(x) x$slot)
    
    # Generando vector de tipos
    df_tipos <- ldply(contenido_respuesta$types, function(x){
      datos_tipo <- data_frame(type = x$type$name)
    }) %>%
      arrange(.id)
    
    # Generando resultado para un pokémon
    resultado <- data_frame(
      name = contenido_respuesta$name,
      type_1 = df_tipos$type[1],
      type_2 = df_tipos$type[2]) %>%
      cbind(df_status)
  }) %>%
    mutate(
      no = as.numeric(.id)
    ) %>%
    select(-.id) %>%
    spread(key = stat_name, value = base_power) %>%
    rename(
      special_attack = `special-attack`,
      special_defense = `special-defense`
    ) %>%
    arrange(no) %>%
    select(
      no,
      everything()
    )

  return(resultado)
}

# La siguiente función recibe un vector con números de pokémon y descarga los 
# sprites asociados en una carpeta adecuada. Argumentos:
# numeros_pokemon: un vector cuyas entradas son los números nacionales de los
#   pokémon de interés
# ruta_carpeta: carpeta donde se almacenarán las imágenes de pokémon.
# La función almacena los sprites en la ruta deseada.
almacena_sprites <- function(numeros_pokemon, ruta_carpeta){
  
  # Almacenando las imágenes en la carpeta deseada
  l_ply(numeros_pokemon, function(i){
    
    # Generando las URL's a partir de la columna "no" de "tabla_base
    url <- paste0("https://raw.githubusercontent.com/PokeAPI/sprites/master/sprites/pokemon/", i, ".png")
    
    # Generando ruta del archivo
    ruta_archivo <- paste0(ruta_carpeta, "/", i, ".png")
    
    # Almacenando el archivo en la ruta adecuada
    GET(url, write_disk(ruta_archivo))
  })
}

# La siguiente función prepara una imagen para ser ploteada como un raster en una
# gráfica de ggplot:
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

# La siguiente función grafica dos variables correspondientes a especies de
# Pokémon utilizando el sprite de cada especie:
# df: el data frame que continene las variables a graficar, más una columna
#   que contiene el número nacional de Pokémon.
# x: nombre de la variable a graficar en el eje x
# y: nombre de la variable a graficar en el eje y
# x_lab: etiqueta de la variable a graficar en el eje x
# y_lab: etiqueta de la variable a graficar en el eje y
# x_lim: vector con el valor mínimo y máximo del eje x
# y_lim: vector con el valor mínimo y máximo del eje y
# no: nombre de la variable con el número nacional de pokémon
# ruta_carpeta_sprites: ruta a la carpeta de sprites donde se encontrarán los
#   archivos (.png) nombrados de acuerdo a "no".
# Nota: si se usan muchos Pokémon, posiblemente sea necesario correr la siguiente
# línea antes de graficar: options(expressions=10000)
grafica_pokemon <- function(df, x, y, x_lab, y_lab, x_lim, y_lim, no, ruta_carpeta_sprites){

  # El eje de menor tamanio entre 30
  tamanio_sprites <- min(c(max(x_lim) - min(x_lim), max(y_lim) - min(y_lim))) / 30
  
  ggplot(data = df, aes_string(x = x, y = y)) +
    # Para que ggplot centre las coordenadas, necesitamos geom_point()
    geom_point(alpha = 0) +
    apply(df, 1, function(renglon){
      ruta_sprite <- paste0(ruta_carpeta_sprites, "/", as.numeric(renglon[[no]]), ".png")
      prepara_sprite(
        ruta_sprite,
        as.numeric(renglon[x]),
        as.numeric(renglon[y]),
        tamanio_sprites
      )
    }) +
    theme_bw() +
    labs(x = x_lab, y = y_lab) +
    xlim(x_lim) +
    ylim(y_lim)
}

# La siguiente función recibe un vector numérico y lo estandariza (media 0 y sd 1)
# var = vector numérico a estandarizar.
estandariza <- function(var){
  resultado <- (var - mean(var))/sd(var)
}