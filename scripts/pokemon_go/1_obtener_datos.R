source("config.R")
source(ruta_archivo_funciones_auxiliares)
library("ggplot2")

# Obteniendo datos de los pokémon de interés
status_base_pokemon_1_251 <- obtener_status(251)
summary(status_base_pokemon_1_251)
#saveRDS(status_base_pokemon_1_251, "../../datos/pokemon_go/status_base_pokemon_1_251.RData")

# Paquete para Web scrapping
library("rvest")

# Escrapeando los multiplicadores de CP:
multiplicadores_cp_nivel <- read_html("https://pokemongo.gamepress.gg/cp-multiplier") %>%
  # Extraer los elementos que corresponden al selector de CSS "table", me da una lista
  html_nodes("table") %>%
  # Extraer la primera tabla (que es la que me interesa)
  `[[`(1) %>%
  # Convertir a DF cada elemento de la lista anterior
  html_table() %>%
  rename(
    level = Level,
    cp_multiplier = `CP Multiplier`
  )
#saveRDS(multiplicadores_cp_nivel, "../../datos/pokemon_go/multiplicadores_cp_nivel.RData")

# La siguiente función calcula, dada una tabla de status de pokémon (tabla base)
# que contiene las siguientes columnas:
# no: número de pokémon
# name: nombre del pokémon
# type_1, type_2: tipos del pokémon (type_2 puede ser NA)
# attack, defense, hp, special_attack, special_defense, speed: base stats del pokemon
# Una tabla que contiene las siguientes:
# no: número de pokémon
# name: nombre del pokémon
# type_1, type_2: tipos del pokémon (type_2 puede ser NA)
# hp_pokemon_go, attack_pokemon_go, defense_pokemon_go: base stats del pokémon en
# pokémon go.
# Las fórmulas se obtuvieron de:
# https://pokegocomplete.com/about

calcula_stats_pokemon_go <- function(tabla_base){
  tabla_base_pokemon_go <- tabla_base %>%
    mutate(
      hp_pokemon_go = 2 * hp,
      
      attack_pokemon_go_aux = 2 * (
        pmax(attack, special_attack) * (7/8) +
        pmin(attack, special_attack) * (1/8)),
      
      attack_pokemon_go = round(attack_pokemon_go_aux * (0.85 + speed / 500)),
      
      defense_pokemon_go_aux = 2 * (
        pmax(defense, special_defense) * (7/8) +
        pmin(defense, special_defense) * (1/8)),
      
      defense_pokemon_go = round(defense_pokemon_go_aux * (0.85 + speed / 500))
    ) %>%
    select(
      no,
      name,
      type_1,
      type_2,
      attack_pokemon_go,
      defense_pokemon_go,
      hp_pokemon_go
    )
}

# Leyendo datos:
status_base_pokemon_1_251 <- readRDS("../../datos/status_base_pokemon_1_251.RData")

# Calculando status de Pokémon Go
status_base_pokemon_go_1_251 <- calcula_stats_pokemon_go(status_base_pokemon_1_251)
#saveRDS(status_base_pokemon_go_1_251, "../../datos/pokemon_go/status_base_pokemon_go_1_251.RData")
summary(status_base_pokemon_go_1_251)

# Graficando los status por número de Pokémon
status_base_pokemon_go_1_251_plot <- status_base_pokemon_go_1_251 %>%
  select(
    no,
    attack_pokemon_go,
    defense_pokemon_go,
    hp_pokemon_go
  ) %>%
  gather("stat", "value", attack_pokemon_go:hp_pokemon_go)

ggplot(status_base_pokemon_go_1_251_plot,
  aes(x = no, y = value, colour = stat, group = stat)) +
  geom_line()
# No hay tendencias, todo bien!