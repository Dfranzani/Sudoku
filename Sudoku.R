# SUDOKU  

# Enumeración por bloque de 3x3
r1 = cbind(matrix(rep(1,9), ncol = 3),matrix(rep(2,9), ncol = 3),matrix(rep(3,9), ncol = 3))
r2 = cbind(matrix(rep(4,9), ncol = 3),matrix(rep(5,9), ncol = 3),matrix(rep(6,9), ncol = 3))
r3 = cbind(matrix(rep(7,9), ncol = 3),matrix(rep(8,9), ncol = 3),matrix(rep(9,9), ncol = 3))
identidad = rbind(r1,r2,r3)

# Análisis de soluciones posibles en una casilla
posible = function(sudoku,i,j){
  h = c(sudoku[i,])
  v = c(sudoku[,j])
  pos = which(identidad == identidad[i,j], arr.ind = T)
  s = c()
  for (k in 1:dim(pos)[1]){
    s = c(s, sudoku[pos[k,1], pos[k,2]])
  }
  all = sort(unique(c(h,v,s)))
  if(all[1] == 0){
    all = all[-1]
  }
  ideal = seq(1,9,1)
  sol = setdiff(ideal,all)
  return(sol)
}

# Recursividad, se cambia de camino al momento de encontrar una inconsistencia
solucion1 = function(sudoku){
  for (i in 1:9) {
    for (j in 1:9) {
      if(sudoku[i,j] == 0){
        for (k in 1:9) {
          if(is.element(k,posible(sudoku,i,j))){
            sudoku[i,j] = k
            solucion1(sudoku)
            sudoku[i,j] = 0
          }
        }
        return("Fin")
      }
    }
  }
  print(sudoku)
}

# Variante de la primer solución:
# Las posibilidades se recorren sin detenerse en una inconsistencia
solucion2 = function(sudoku){
  while(sum(sudoku) < 9*sum(1:9)){
    suma = sum(sudoku)
    for (i in 1:9) {
      for (j in 1:9) {
        if(sudoku[i,j] == 0){
          sol = posible(sudoku,i,j)
          if(length(sol) == 1){
            sudoku[i,j] = sol
          }
        }
      }
    }
    # Nos detenemos al encontrar la primera solución
    if(sum(sudoku) == 9*(sum(1:9))) {print(sudoku)}
    
    if(suma == sum(sudoku)) {void = solve2(sudoku); break()}
  }
  return("Fin")
}

# Matriz de ejemplo
M = matrix(c(9,7,2,3,0,0,0,0,0,
             1,0,0,7,0,0,2,0,5,
             0,0,0,0,2,0,8,0,0,
             0,0,0,2,0,0,0,5,0,
             0,9,7,6,0,3,4,8,0,
             0,3,0,0,0,4,0,0,0,
             0,0,3,0,9,0,0,0,0,
             8,0,1,0,0,2,0,0,6,
             0,0,0,0,0,6,0,1,0),
           ncol = 9, nrow = 9, byrow = T)

solucion1(M)
solucion2(M)




