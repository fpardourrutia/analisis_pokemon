library("plyr")
library("dplyr")
library("tidyr")

# Leyendo datos de Pokémon Go:
status_base_pokemon_go_1_251 <- readRDS("../../datos/pokemon_go/status_base_pokemon_go_1_251.RData")
multiplicadores_cp_nivel <- readRDS("../../datos/pokemon_go/multiplicadores_cp_nivel.RData")

# La siguiente función, calcula, dados:
#   no_pokemon: el número nacional de pokémon
#   iv_ataque: el iv de ataque de un pokémon
#   iv_defensa: el iv de defensa de un pokémon
#   iv_hp: el iv de hp de un pokémon
#   nivel: el nivel de un pokémon
# Un vector con los siguientes datos:
#   ataque/defensa/hp/cp_real: los puntos de ataque/defensa/hp/cp del pokémon en curso
#   ataque/defensa/hp/cp_maximo: los puntos de ataque/defensa/hp/cp máximos (IV's perfectos)
#   proporcion_ataque/defensa/hp/cp: porcentaje de los puntos de ataque/defensa/hp/cp
#     reales con respecto a los máximos.
# Fuente de las fórmulas:
#   https://pokemongo.gamepress.gg/damage-mechanics
#   https://pokegocomplete.com/about
calcula_estadisticas_individuales <- function(especie, iv_ataque, iv_defensa, iv_hp, nivel){
  
  # Encontrando el multiplicador de nivel en la tabla correspondiente
  multiplicador_cp <- multiplicadores_cp_nivel %>%
    filter(level == nivel) %>%
    pull(cp_multiplier)
  
  stats_pokemon <- status_base_pokemon_go_1_251 %>%
    filter(
      name == tolower(especie)
    ) %>%
    mutate(
      ataque_real = floor((attack_pokemon_go + iv_ataque) * multiplicador_cp),
      defensa_real = floor((defense_pokemon_go + iv_defensa) * multiplicador_cp),
      hp_real = floor((hp_pokemon_go + iv_hp) * multiplicador_cp),
      cp_real = floor(
        (attack_pokemon_go + iv_ataque) *
        sqrt(defense_pokemon_go + iv_defensa) *
        sqrt(hp_pokemon_go + iv_hp) * multiplicador_cp^2 / 10),
      
      ataque_maximo = floor((attack_pokemon_go + 15) * multiplicador_cp),
      defensa_maxima = floor((defense_pokemon_go + 15) * multiplicador_cp),
      hp_maximo = floor((hp_pokemon_go + 15) * multiplicador_cp),
      cp_maximo = floor(
        (attack_pokemon_go + 15) *
        sqrt(defense_pokemon_go + 15) *
        sqrt(hp_pokemon_go + 15) * multiplicador_cp^2 / 10),
      
      proporcion_ataque = (ataque_real / ataque_maximo) * 100,
      proporcion_defensa = (defensa_real / defensa_maxima) * 100,
      proporcion_hp = (hp_real / hp_maximo) * 100,
      proporcion_cp = (cp_real / cp_maximo) * 100
    ) %>%
    select(
      no,
      nombre = name,
      ataque_real,
      ataque_maximo,
      proporcion_ataque,
      defensa_real,
      defensa_maxima,
      proporcion_defensa,
      hp_real,
      hp_maximo,
      proporcion_hp,
      cp_real,
      cp_maximo,
      proporcion_cp
    )
  
  return(stats_pokemon)
}

# calcula_estadisticas_individuales("articuno", 15, 10, 12, 40)
# Mi articuno es perfecto...

# Revisando máximas estadísticas de los pokémon a nivel 29:
# status_base_pokemon_go_1_251 %>%
#   pull(name) %>%
#   ldply(function(x) calcula_estadisticas_individuales(x, 15, 15, 15, 29)) %>%
#   select(
#     no,
#     nombre,
#     contains("max")
#   ) %>%
#   View()