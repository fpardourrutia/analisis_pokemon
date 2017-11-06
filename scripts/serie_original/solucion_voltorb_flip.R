library("plyr")
library("dplyr")
library("lpSolve")

# La siguiente función encuentra una solución factible para el juego de Voltorb
# Flip (https://bulbapedia.bulbagarden.net/wiki/Voltorb_Flip), con las variables
# nombradas como:
# ui = suma de los números en el i-ésimo renglón o columna
# vi = número de Voltorbs en el i-ésimo renglón o columna
# Donde i es como sigue:
# x11 x12 x13 x14 x15  1
# x21 x22 x23 x24 x25  2
# x31 x32 x33 x34 x35  3
# x41 x42 x43 x44 x45  4
# x51 x52 x53 x54 x55  5
# 6   7   8   9   10

# Entradas:
# U: vector con las entradas u1...u10
# V: vector con las entradas v1...v10.

# Adicionalmente a las entradas anteriores, se pueden agregar como restricciones
# adicionales, valores de x ya conocidos (en orden de x11, x12, ..., hasta x55),
# como sigue:

# usar_warmstart: booleano que indica si se utilizarán valores de x ya conocidos.
# valores_warmstart: vector de tamaño 25 que tiene NA en los valores desconocidos
# de x y su valor (1 a 3) en los conocidos

# Salidas:
# La función regresa una matriz de 5 x 5 que representa una solución factible (no
# se de unicidad) al problema anterior, donde la casilla (i,j) representa el
# valor de dicha casilla en la solución propuesta (y 0 si es Voltorb).

encuentra_solucion_voltorb_flip <- function(
  u, v, usar_warmstart = FALSE, valores_warmstart = NULL){
  
  # Sea xij el valor numérico de la casilla ij
  # Sea yij = 0 si hay voltorb (el valor de xij es 0) y 1 e.o.c.
  
  # Entonces el problema a resolver es
  # (ver https://cs.stackexchange.com/questions/12102/express-boolean-logic-operations-in-zero-one-integer-linear-programming-ilp):
  
  # min cte
  # s.a
  # xi1 + xi2 + xi3 + xi4 + xi5 = ui, 1<=i<=5
  # x1j + x2j + x3j + x4j + x5j = u(j+5), 1<=j<=5
  # xij >= 0 (default en LP Solve)
  # xij enteros
  # yi1 + yi2 + yi3 + yi4 + yi5 = 5 - vi, 1<=i<=5
  # y1j + y2j + y3j + y4j + y5j = 5 - v(j+5), 1<=j<=5
  # xij - yij >= 0 
  # -xij + 3yij >= 0
  # Las dos condiciones anteriores implican (xi = 0 <=> yi = 0)
  # yij binarias
  

  # Y el resultado es la matriz:
  # ((xij))
  
  ###########################################################
  # Resolviendo el anterior problema de programación lineal:
  ###########################################################
  # Notar que el problema tiene 25 variables xi y 25 variables yi, que serán
  # numeradas como:
  # x11, x12, ..., x55, y11, y12, ..., y55
  
  #### coeficientes de la función objetivo:
  coeficientes_func_objetivo <- rep(0, 50)
  
  #### Creando las restricciones:
  
  # xi1 + xi2 + xi3 + xi4 + xi5 = ui, 1<=i<=5
  coeficientes_restricciones_suma_renglones_xij <- Reduce(rbind, list(
    c(rep(1,5), rep(0, 45)),
    c(rep(0,5), rep(1,5), rep(0, 40)),
    c(rep(0,10), rep(1,5), rep(0, 35)),
    c(rep(0,15), rep(1,5), rep(0, 30)),
    c(rep(0,20), rep(1,5), rep(0, 25))
  ))

  direcciones_restricciones_suma_renglones_xij <- rep("==", 5)
  lados_derechos_restricciones_suma_renglones_xij <- u[1:5]
  
  # x1j + x2j + x3j + x4j + x5j = u(j+5), 1<=j<=5
  coeficientes_restricciones_suma_columnas_xij <- Reduce(rbind, list(
    c(rep(c(1, 0, 0, 0, 0),5), rep(0, 25)),
    c(rep(c(0, 1, 0, 0, 0),5), rep(0, 25)),
    c(rep(c(0, 0, 1, 0, 0),5), rep(0, 25)),
    c(rep(c(0, 0, 0, 1, 0),5), rep(0, 25)),
    c(rep(c(0, 0, 0, 0, 1),5), rep(0, 25))
  ))
  
  direcciones_restricciones_suma_columnas_xij <- rep("==", 5)
  lados_derechos_restricciones_suma_columnas_xij <- u[6:10]
  
  # xij >= 0 (default)
  
  # xij enteros se especifica después

  # yi1 + yi2 + yi3 + yi4 + yi5 = 5 - vi, 1<=i<=5
  coeficientes_restricciones_suma_renglones_yij <- Reduce(rbind, list(
    c(rep(0, 25), rep(1,5), rep(0, 20)),
    c(rep(0,30), rep(1,5), rep(0, 15)),
    c(rep(0,35), rep(1,5), rep(0, 10)),
    c(rep(0,40), rep(1,5), rep(0, 5)),
    c(rep(0,45), rep(1,5))
  ))

  direcciones_restricciones_suma_renglones_yij <- rep("==", 5)
  lados_derechos_restricciones_suma_renglones_yij <- 5 - v[1:5]
  
  # y1j + y2j + y3j + y4j + y5j = 5 - v(j+5), 1<=j<=5
  coeficientes_restricciones_suma_columnas_yij <- Reduce(rbind, list(
    c(rep(0, 25), rep(c(1, 0, 0, 0, 0),5)),
    c(rep(0, 25), rep(c(0, 1, 0, 0, 0),5)),
    c(rep(0, 25), rep(c(0, 0, 1, 0, 0),5)),
    c(rep(0, 25), rep(c(0, 0, 0, 1, 0),5)),
    c(rep(0, 25), rep(c(0, 0, 0, 0, 1),5))
  ))
  
  direcciones_restricciones_suma_columnas_yij <- rep("==", 5)
  lados_derechos_restricciones_suma_columnas_yij <- 5 - v[6:10]

  # xij - yij >= 0
  coeficientes_restricciones_diferencias_xij_yij <- cbind(diag(25), -diag(25))
  
  direcciones_restricciones_diferencias_xij_yij <- rep(">=", 25)
  lados_derechos_restricciones_diferencias_xij_yij <- rep(0, 25)
  
  # -xij + 3yij >= 0
  coeficientes_restricciones_diferencias_3yij_xij <- cbind(-diag(25), 3 * diag(25))
  
  direcciones_restricciones_diferencias_3yij_xij <- rep(">=", 25)
  lados_derechos_restricciones_diferencias_3yij_xij <- rep(0, 25)
  
  # yij binarias se especifica después
  
  #### Lidiando con valores de warmstart
  
  # 1. Se actualizarán los lados derechos de las restricciones que dependen de
  # varias variables.
  # 2. Se marcarán las restricciones que sólo dependen de las variables ya conocidas
  # 3. Se eliminarán los coeficientes de las variables ya conocidas en todas las
  # restricciones y la función objetivo (siempre teniendo el control de qué
  # variables se conocen y cuáles se están calculando).
  # 4. Se eliminarán las restricciones que dependen sólo de variables ya conocidas.
  
  if(usar_warmstart){
    if(length(valores_warmstart) != 25){
      stop("valores_warmstart debe ser un vector de longitud 25")
    }
    
    for(i in 1:25){
      # Si valores_warmstart[i] es NA, se ignora todo lo que sigue
      if(!is.na(valores_warmstart[i])){
        
        ## 1. Restricciones sobre los renglones:
        
        # Dependiendo del número de variable son las restricciones a modificar.
        # Esto lo podemos saber por su cociente al dividirla entre 5.
        # Por ejemplo, si son las variables x6, ..., x10:
        # i %/% 5 == 1 ó (i %/% 5 == 2 y 1 %% 5 == 0), esto afecta a las
        # restricciones sobre los renglones:
        # x21 + x22 + x23 + x24 + x25 = u2, (segunda restricción de su tipo)
        # y21 + y22 + y23 + y24 + y25 = 5 - v2 (segunda restricción de su tipo)
        # Y así sucesivamente:
        # Si i %/% 5 == n, ésto afecta a las (n+1)-ésimas restricciones (a
        # excepción de i%/% 5 == n e i %% 5 == 0, que afecta a las n-ésimas).
        
        if(i %% 5 == 0){
          
          # xi1 + xi2 + xi3 + xi4 + xi5 = ui, 1<=i<=5
          lados_derechos_restricciones_suma_renglones_xij[i %/% 5] <-
            lados_derechos_restricciones_suma_renglones_xij[i %/% 5] - valores_warmstart[i]
          
          # yi1 + yi2 + yi3 + yi4 + yi5 = 5 - vi, 1<=i<=5
          lados_derechos_restricciones_suma_renglones_yij[i %/% 5] <-
            lados_derechos_restricciones_suma_renglones_yij[i %/% 5] - 1
          
        } else{
          
          # xi1 + xi2 + xi3 + xi4 + xi5 = ui, 1<=i<=5
          lados_derechos_restricciones_suma_renglones_xij[(i %/% 5) + 1] <-
            lados_derechos_restricciones_suma_renglones_xij[(i %/% 5) + 1] - valores_warmstart[i]
          
          # yi1 + yi2 + yi3 + yi4 + yi5 = 5 - vi, 1<=i<=5
          lados_derechos_restricciones_suma_renglones_yij[(i %/% 5) + 1] <-
            lados_derechos_restricciones_suma_renglones_yij[(i %/% 5) + 1] - 1
          
        }
        
        ## 2. Restricciones sobre columnas:
        
        # Dependiendo del número de variable son las restricciones a modificar.
        # Esto lo podemos saber por su residuo al dividirla entre 5.
        # Si son las variables x1, x6, x11, x16, x21, (i %% 5 == 1) esto afecta
        # a las restricciones sobre columnas: 
        # x11 + x21 + x31 + x41 + x51 = u6, (primera restricción de su tipo)
        # y11 + y21 + y31 + y41 + y51 = 5 - v6 (primera restricción de su tipo)
        # Y así sucesivamente:
        # Si i %% 5 == n, ésto afecta a las n-ésimas restricciones (a excepción
        # de n == 0, que afecta a las quintas restricciones)
        
        if(i %% 5 > 0){
          
          # x1j + x2j + x3j + x4j + x5j = u(j+5), 1<=j<=5
          lados_derechos_restricciones_suma_columnas_xij[i %% 5] <-
            lados_derechos_restricciones_suma_columnas_xij[i %% 5] - valores_warmstart[i]
          
          # y1j + y2j + y3j + y4j + y5j = 5 - v(j+5), 1<=j<=5
          lados_derechos_restricciones_suma_columnas_yij[i %% 5] <-
            lados_derechos_restricciones_suma_columnas_yij[i %% 5] - 1
          
        } else{
          
          # x1j + x2j + x3j + x4j + x5j = u(j+5), 1<=j<=5
          lados_derechos_restricciones_suma_columnas_xij[5] <-
            lados_derechos_restricciones_suma_columnas_xij[5] - valores_warmstart[i]
          
          # y1j + y2j + y3j + y4j + y5j = 5 - v(j+5), 1<=j<=5
          lados_derechos_restricciones_suma_columnas_yij[5] <-
            lados_derechos_restricciones_suma_columnas_yij[5] - 1
        }
        
        ## 2. Marcando lados derechos de restricciones no necesarias: se pondrán 
        ## todas como NA's pues de si las elimino una por una tendré problemas con
        ## los índices de eliminación.
        
        # xij - yij >= 0
        lados_derechos_restricciones_diferencias_xij_yij[i] <- NA
        
        # -xij + 3yij >= 0
        lados_derechos_restricciones_diferencias_3yij_xij[i] <- NA
      }
    }
  }
  
  #### Definiendo los elementos del problema de programación lineal (esto es
  #### independientemente de si se usa warmstart o no)
  
  objective.in <- coeficientes_func_objetivo
  
  const.mat <- rbind(
    coeficientes_restricciones_suma_renglones_xij,
    coeficientes_restricciones_suma_columnas_xij,
    
    coeficientes_restricciones_suma_renglones_yij,
    coeficientes_restricciones_suma_columnas_yij,
    
    coeficientes_restricciones_diferencias_xij_yij,
    coeficientes_restricciones_diferencias_3yij_xij
  )
  
  const.dir <- c(
    direcciones_restricciones_suma_renglones_xij,
    direcciones_restricciones_suma_columnas_xij,
    
    direcciones_restricciones_suma_renglones_yij,
    direcciones_restricciones_suma_columnas_yij,
    
    direcciones_restricciones_diferencias_xij_yij,
    direcciones_restricciones_diferencias_3yij_xij
  )
  
  const.rhs <- c(
    lados_derechos_restricciones_suma_renglones_xij,
    lados_derechos_restricciones_suma_columnas_xij,
    
    lados_derechos_restricciones_suma_renglones_yij,
    lados_derechos_restricciones_suma_columnas_yij,
    
    lados_derechos_restricciones_diferencias_xij_yij,
    lados_derechos_restricciones_diferencias_3yij_xij
  )
  
  if(usar_warmstart){
    
    ## 3. Eliminando variables conocidas, tanto de la función objetivo como de las
    ## restricciones.

    # Encontrando indices de las variables xi a calcular y de las conocidas.
    # Notar que en ningún momento se cambia el órden de las variables (columnas),
    # por lo que los valores de las variables se calculan en el mismo orden.
    indices_variables_xi_conocidas <- (1:25)[!is.na(valores_warmstart)]
    indices_variables_xi_calcular <- (1:25)[is.na(valores_warmstart)]
    
    # Encontrando índices de las variables xi y yi conocidas:
    indices_variables_yi_conocidas <- indices_variables_xi_conocidas + 25
    
    indices_variables_xi_yi_conocidas <- c(
      indices_variables_xi_conocidas,
      indices_variables_yi_conocidas
    )
    
    # Eliminando dichas variables de la función objetivo y restricciones
    objective.in <- objective.in[-indices_variables_xi_yi_conocidas]
    const.mat <- const.mat[,-indices_variables_xi_yi_conocidas]
      
    ## 4. Eliminando restricciones no necesarias al usar warmstart, recordando
    ## que se marcaron sus correspondientes lados derechos con NA.
    
    const.mat <- const.mat[complete.cases(const.rhs),]
    const.dir <- const.dir[complete.cases(const.rhs)]
    const.rhs <- const.rhs[complete.cases(const.rhs)]
    
    # Eliminando restricciones adicionales si es el caso (por ejemplo, si todas
    # las variables en un restricción de suma de renglón o columna son conocidas:
    
    restricciones_redundantes <- rowSums(abs(const.mat)) == 0
    const.mat <- const.mat[!restricciones_redundantes,]
    const.dir <- const.dir[!restricciones_redundantes]
    const.rhs <- const.rhs[!restricciones_redundantes]
    
    # Definiendo variables enteras y binarias, tomando en cuenta las nuevas posiciones de
    # las variables al eliminar algunas
    indices_variables_enteras <- 1:length(indices_variables_xi_calcular)
    indices_variables_binarias <- (length(indices_variables_xi_calcular) + 1):
      (2 * length(indices_variables_xi_calcular))
    
  } else{
    
    # Si no se usó warmstart, los índices de las variables enteras son las de las
    # xij (las yij se vuelven enteras por optimización).
    indices_variables_enteras <- 1:25
    indices_variables_binarias <- 26:50
  }

  # Encontrando la solución al problema:
  resultado_optimizacion <- lp(
    direction = "min",
    objective.in = objective.in,
    const.mat = const.mat,
    const.dir = const.dir,
    const.rhs = const.rhs,
    int.vec = indices_variables_enteras,
    binary.vec = indices_variables_binarias
  )
  
  # Poniendo la solución al problema en forma de matriz:
  if(usar_warmstart){
    
    solucion <- data_frame(
        indice_xi = indices_variables_xi_conocidas,
        valor_xi = valores_warmstart[indices_variables_xi_conocidas]
      ) %>%
      rbind(data_frame(
        indice_xi = indices_variables_xi_calcular,
        valor_xi = resultado_optimizacion$solution[indices_variables_enteras]
      )) %>%
      arrange(indice_xi) %>%
      pull(valor_xi) %>%
      matrix(byrow = TRUE, nrow = 5)
    
  } else{
    
    solucion <- resultado_optimizacion$solution[1:25] %>%
      matrix(byrow = TRUE, nrow = 5)
  }
  
  # Creando diagnóstico para ver si la optimización es correcta:
  print(resultado_optimizacion)
  
  if(sum(rowSums(solucion) == u[1:5]) == 5 & sum(colSums(solucion) == u[6:10]) == 5)
    print("u: TRUE")
  else
    print("u: FALSE")
  
  if(sum(rowSums(solucion == 0) == v[1:5]) == 5 & sum(colSums(solucion == 0) == v[6:10]) == 5){
    print("v: TRUE")
  } else{
    print("v: FALSE")
  }
  
  return(solucion)
}

# La siguiente función encuentra la probabilidad de que cada casilla sea un número
# (no un Voltorb). Esto se hace únicamente con la información de cantidad de
# Voltorbs por renglón y columna, puesto que lpSolve sólo puede obtener todas
# las soluciones de un problema de Programación binaria. Variables:
# vi: número de Voltorbs en el i-ésimo renglón o columna
# Donde i es como sigue:
# x11 x12 x13 x14 x15  v1
# x21 x22 x23 x24 x25  v2
# x31 x32 x33 x34 x35  v3
# x41 x42 x43 x44 x45  v4
# x51 x52 x53 x54 x55  v5
# v6  v7  v8  v9  v10

# Entradas:
# v: vector con las variables v1...v10

# Adicionalmente a las entradas anteriores, se pueden agregar como restricciones
# adicionales, lugares donde ya sabemos que no hay Voltorbs.

# usar_warmstart: booleano que indica si se utilizará información de lugares donde
# seguro no hay Voltorbs.
# valores_warmstart: vector con los índices de las casillas donde seguro no hay
# Voltorbs.

# Salidas:
# La función regresa una matriz de 5x5 donde cada casilla contiene un índice de
# la confianza que tenemosde que su valor sea un número. (Entre más alto dicho
# valor, más seguros que al seleccionar esa casilla, no perderemos).

encuentra_confianza_de_numero <- function(
  v, usar_warmstart = FALSE, indices_posiciones_conocidas_numeros = NULL){
  
  # Sea yij = 1 si hay número en la casilla ij y 0 e.o.c.
  
  # Entonces el problema a resolver es:

  # min cte
  # s.a
  # yi1 + yi2 + yi3 + yi4 + yi5 = 5 - vi, 1<=i<=5
  # y1j + y2j + y3j + y4j + y5j = 5 - v(j+5), 1<=j<=5
  # yij binarias
  
  # Se obtendrán todas las soluciones a dicho problema binario, y, para cada casilla,
  # se sumarán sus valores sobre todas las soluciones. Entre más alto sea el
  # valor encontrado, más soluciones tienen a dicha casilla como número y por lo
  # tranto, mayor confianza tenemos que en esa casilla efectivamente hay un número
  
  ###########################################################
  # Resolviendo el anterior problema de programación lineal:
  ###########################################################
  # Notar que el problema tiene 25 variables yi, que serán numeradas como:
  # y11, y12, ..., y55
  
  #### coeficientes de la función objetivo:
  coeficientes_func_objetivo <- rep(0, 25)
  
  #### Creando las restricciones:
  
  # yi1 + yi2 + yi3 + yi4 + yi5 = 5 - vi, 1<=i<=5
  coeficientes_restricciones_suma_renglones_yij <- Reduce(rbind, list(
    c(rep(1,5), rep(0, 20)),
    c(rep(0,5), rep(1,5), rep(0, 15)),
    c(rep(0,10), rep(1,5), rep(0, 10)),
    c(rep(0,15), rep(1,5), rep(0, 5)),
    c(rep(0,20), rep(1,5))
  ))
  
  direcciones_restricciones_suma_renglones_yij <- rep("==", 5)
  lados_derechos_restricciones_suma_renglones_yij <- 5 - v[1:5]
  
  # y1j + y2j + y3j + y4j + y5j = 5 - v(j+5), 1<=j<=5
  coeficientes_restricciones_suma_columnas_yij <- Reduce(rbind, list(
    rep(c(1, 0, 0, 0, 0),5),
    rep(c(0, 1, 0, 0, 0),5),
    rep(c(0, 0, 1, 0, 0),5),
    rep(c(0, 0, 0, 1, 0),5),
    rep(c(0, 0, 0, 0, 1),5)
  ))
  
  direcciones_restricciones_suma_columnas_yij <- rep("==", 5)
  lados_derechos_restricciones_suma_columnas_yij <- 5 - v[6:10]
  
  #### Lidiando con valores de warmstart
  
  # 1. Se actualizarán los lados derechos de las restricciones que dependen de
  # varias variables.
  # 2. Se marcarán las restricciones que sólo dependen de las variables ya conocidas
  # 3. Se eliminarán los coeficientes de las variables ya conocidas en todas las
  # restricciones y la función objetivo (siempre teniendo el control de qué
  # variables se conocen y cuáles se están calculando).
  # 4. Se eliminarán las restricciones que dependen sólo de variables ya conocidas.
  
  if(usar_warmstart){
    if(max(indices_posiciones_conocidas_numeros) > 25 | min(indices_posiciones_conocidas_numeros) < 1){
      stop("indices_numeros debe especificar los índices de las casillas donde
        sabemos que hay número (1-25)")
    }
    
    for(i in indices_posiciones_conocidas_numeros){
      
      ## 1. Restricciones sobre los renglones:
      
      # Dependiendo del número de variable son las restricciones a modificar.
      # Esto lo podemos saber por su cociente al dividirla entre 5.
      # Por ejemplo, si son las variables y6, ..., y10:
      # i %/% 5 == 1 ó (i %/% 5 == 2 y 1 %% 5 == 0), esto afecta a la
      # restricción sobre los renglones:
      # y21 + y22 + y23 + y24 + y25 = 5 - v2 (segunda restricción de su tipo)
      # Y así sucesivamente:
      # Si i %/% 5 == n, ésto afecta a la (n+1)-ésima restricción (a
      # excepción de i%/% 5 == n e i %% 5 == 0, que afecta a la n-ésima).
      
      if(i %% 5 == 0){
        
        # yi1 + yi2 + yi3 + yi4 + yi5 = 5 - vi, 1<=i<=5
        lados_derechos_restricciones_suma_renglones_yij[i %/% 5] <-
          lados_derechos_restricciones_suma_renglones_yij[i %/% 5] - 1
        
      } else{
        
        # yi1 + yi2 + yi3 + yi4 + yi5 = 5 - vi, 1<=i<=5
        lados_derechos_restricciones_suma_renglones_yij[(i %/% 5) + 1] <-
          lados_derechos_restricciones_suma_renglones_yij[(i %/% 5) + 1] - 1
        
      }
      
      ## 2. Restricciones sobre columnas:
      
      # Dependiendo del número de variable son las restricciones a modificar.
      # Esto lo podemos saber por su residuo al dividirla entre 5.
      # Si son las variables y1, y6, y11, y16, y21, (i %% 5 == 1) esto afecta
      # a la restricción sobre columnas: 
      # y11 + y21 + y31 + y41 + y51 = 5 - v6 (primera restricción de su tipo)
      # Y así sucesivamente:
      # Si i %% 5 == n, ésto afecta a la n-ésima restricción (a excepción
      # de n == 0, que afecta a la quinta restricción)
      
      if(i %% 5 > 0){
        
        # y1j + y2j + y3j + y4j + y5j = 5 - v(j+5), 1<=j<=5
        lados_derechos_restricciones_suma_columnas_yij[i %% 5] <-
          lados_derechos_restricciones_suma_columnas_yij[i %% 5] - 1
        
      } else{
        
        # y1j + y2j + y3j + y4j + y5j = 5 - v(j+5), 1<=j<=5
        lados_derechos_restricciones_suma_columnas_yij[5] <-
          lados_derechos_restricciones_suma_columnas_yij[5] - 1
      }
    }
  }

#### Definiendo los elementos del problema de programación lineal (esto es
#### independientemente de si se usa warmstart o no)
  
  objective.in <- coeficientes_func_objetivo
  
  const.mat <- rbind(
    coeficientes_restricciones_suma_renglones_yij,
    coeficientes_restricciones_suma_columnas_yij
  )
  
  const.dir <- c(
    direcciones_restricciones_suma_renglones_yij,
    direcciones_restricciones_suma_columnas_yij
  )
  
  const.rhs <- c(
    lados_derechos_restricciones_suma_renglones_yij,
    lados_derechos_restricciones_suma_columnas_yij
  )
  
  if(usar_warmstart){
    
    ## 3. Eliminando variables conocidas, tanto de la función objetivo como de las
    ## restricciones.
    
    # Encontrando indices de las variables yi a calcular.
    # Notar que en ningún momento se cambia el órden de las variables (columnas),
    # por lo que los valores de las variables se calculan en el mismo orden.
    indices_variables_yi_conocidas <- indices_posiciones_conocidas_numeros
    
    # Eliminando dichas variables de la función objetivo y restricciones
    objective.in <- objective.in[-indices_variables_yi_conocidas]
    const.mat <- const.mat[,-indices_variables_yi_conocidas]
    
    ## 4. Eliminando restricciones adicionales si es el caso (por ejemplo, si todas
    ## las variables en un restricción de suma de renglón o columna son conocidas:
    
    restricciones_redundantes <- rowSums(abs(const.mat)) == 0
    const.mat <- const.mat[!restricciones_redundantes,]
    const.dir <- const.dir[!restricciones_redundantes]
    const.rhs <- const.rhs[!restricciones_redundantes]
  }
  
  numero_soluciones <- 50
  
  ##### Encontrando la solución al problema:
  resultado_optimizacion <- lp(
    direction = "min",
    objective.in = objective.in,
    const.mat = const.mat,
    const.dir = const.dir,
    const.rhs = const.rhs,
    all.bin = TRUE,
    num.bin.solns = numero_soluciones,
    use.rw = TRUE # por robustez (ver ?lp)
  )
  # Si uso 100 soluciones, resultado_optimizacion$solution será
  # un vector de 100 * 25 = 2500 entradas, a interpretar.
  
  #### Creando el output:
  
  if(usar_warmstart){
    
    # Si se usó warmstart, hay que tomar en cuenta que sólo se calcularon mediante
    # optimización los valores de las variables no conocidas:
    
    data_frame_auxiliar <- data_frame(
      # Creando data frame con soluciones calculadas mediante optimización
      solucion = rep(1:numero_soluciones, 25 - length(indices_variables_yi_conocidas)) %>%
        sort(),
      casilla = rep(setdiff(1:25, indices_variables_yi_conocidas), numero_soluciones),
      # Le quitamos el último valor que siempre es -1
      valor = resultado_optimizacion$solution[-length(resultado_optimizacion$solution)]
    ) %>%
      
    # Y pegándole el de los valores de las variables que conocemos
    rbind(data_frame(
      solucion = rep(1:numero_soluciones, length(indices_variables_yi_conocidas)) %>%
        sort(),
      casilla = rep(indices_variables_yi_conocidas, numero_soluciones),
      valor = 1 # Siempre es 1 porque sabemos que es un número
    )) %>%
      arrange(solucion, casilla)
    
  } else{
    data_frame_auxiliar <- data_frame(
      solucion = rep(1:numero_soluciones, 25) %>%
        sort(),
      casilla = rep(1:25, numero_soluciones),
      # Le quitamos el último valor que siempre es -1
      valor = resultado_optimizacion$solution[-length(resultado_optimizacion$solution)]
    )
  }
  
  ## Creando lista con las soluciones que cumplen con todas las restricciones:
  lista_soluciones_reales <- ddply(data_frame_auxiliar, .(solucion), function(df){
    
    # Definiendo la matriz de la solución
    solucion <- matrix(df$valor, byrow = TRUE, nrow = 5)
    
    # Creando lista de matrices que cumplen las restricciones impuestas:
    if(
      sum(rowSums(solucion == 0) == v[1:5]) == 5 &
        sum(colSums(solucion == 0) == v[6:10]) == 5 &
        # A veces da -1 en algunas soluciones, y no se por qué
        sum(solucion >= 0) == 25){
      resultado <- data_frame(satisface_restricciones = TRUE)
    } else{
      resultado <- data_frame(satisface_restricciones = FALSE)
    }
  }) %>%
    # Filtrando sólo soluciones que satisfacen todas las restricciones
    filter(satisface_restricciones) %>%
    pull(solucion)
  
  ## Creando resultado:
  matriz_confianzas <- data_frame_auxiliar %>%
    # Quedándonos sólo con las soluciones reales
    filter(solucion %in% lista_soluciones_reales) %>%
    # Calculando confianza por casilla
    group_by(casilla) %>%
    summarise(
      confianza = sum(valor)
    ) %>%
    pull(confianza) %>%
    matrix(byrow = TRUE, nrow = 5)
  
  # Imprimiendo número de soluciones encontradas:
  print(paste0("Número de soluciones: ", length(lista_soluciones_reales)))
  return(matriz_confianzas)
}

# Ejemplo de uso

# Definiendo los valores de u y v:
u <- c(6,8,6,2,4,6,6,7,4,3)
v <- c(1,1,1,4,3,1,3,1,3,2)

# Definiendo parámetros para el warmstart
usar_warmstart <- TRUE
valores_warmstart <- rep(NA, 25)
valores_warmstart[c(1,3,6,11,13)] <- 1
#valores_warmstart[c(10,14)] <- 2
valores_warmstart[c(8)] <- 3

# Revisando valores warmstart:
matrix(valores_warmstart, byrow = TRUE, nrow = 5)

# Definiendo "indices_posiciones_conocidas_numeros" a partir de "valores_warmstart"
indices_posiciones_conocidas_numeros <- which(!is.na(valores_warmstart))

# Llamando a funciones anteriores
encuentra_confianza_de_numero(v, usar_warmstart, indices_posiciones_conocidas_numeros)
encuentra_solucion_voltorb_flip(u, v, usar_warmstart, valores_warmstart)

# Estrategia: Como la solución no es única, jugar, destapando las casillas con
# menor probabilidad de Voltorb para ganar evidencia (warmstart). A veces hay que
# arriesgar pero entre más informativa sea la evidencia (arriesgarse demasiado) y
# ver que no hay Voltorb en determinada casilla, más probable es que se de la unicidad
# de la solución.
