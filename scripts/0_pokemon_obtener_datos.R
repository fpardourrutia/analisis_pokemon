library("plyr")
library("dplyr")
library("tidyr")
library("httr")
library("purrr")

# Función para obtener la lista de los primeros n pokémon.
# n: número máximo de pokémon a obtener.
# La función consulta la Pokeapi: http://pokeapi.co y regresa un data frame que
# contiene:
#   - El número de pokémon
#   - El nombre del pokémon
#   - Los valores base del pokémon: hp, ataque, defensa, ataque especial, defensa
#   especial, rapidez.
obtener_status <- function(n){
  # Generando URL's a consultar:
  urls <- paste0("http://pokeapi.co/api/v2/pokemon/", 1:n)
  
  # Nombrando urls para que salga el número de Pokémon en el resultado
  names(urls) <- 1:n
  
  # Ejemplo:
  # x <- "http://pokeapi.co/api/v2/pokemon/100"
  # GET(x) %>%
  # content("parsed") %>%
  # str(max.level = 1)
  
  # Consultando las URL's y extrayendo datos de interés:
  resultado <- ldply(urls, function(x){
    
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
}

status_base_pokemon_1_251 <- obtener_status(251)
summary(status_base_pokemon_1_251)
#saveRDS(status_base_pokemon_1_251, "../datos/status_base_pokemon_1_251.RData")