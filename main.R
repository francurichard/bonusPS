library(fpCompare) #pentru precizie la comparatii 

decide_dependent <- function(){
  rand = runif(1)
  dependent = TRUE
  if (1 - rand > rand) {
    dependent = FALSE
  }
  return(dependent)
}

solve_element <- function(A, r, c, R, C) {
  # verific daca numarul elementelor de pe aceeasi linie este egal cu C - 3, unde C este numarul coloanelor repartitiei comune
  # daca da, inseamna ca elementul ca elementul se poate afla, folosind faptul ca suma elementelor de pe aceeasi linie este egala
  # cu probabilitatea variabilei X corespunzatoare liniei respective
  # daca nu, atunci fac acelasi lucru si pe coloana 
  # daca nici acum nu am aflat elementul, returnez -1
  no_elem_line = 0
  sum_line = 0
  no_elem_col = 0
  sum_col = 0
  for (col in 2:(C - 1)) {
    if (A[r, col] != -1) {
      no_elem_line = 1 + no_elem_line
      sum_line = sum_line + A[r, col]
    }
  }
  if (no_elem_line == C - 3) {
    ans = A[r, C] - sum_line
    return(ans)
  }
  for (row in 2:(R - 1)) {
    if (A[row, c] != -1) {
      no_elem_col = 1 + no_elem_col
      sum_col = sum_col + A[row, c]
    }
  }
  if (no_elem_col == R - 3) {
    ans = A[R, c] - sum_col
    return(ans)
  }
  ans = -1
  cov_x
  return(ans)
}


generate_random_joint_distribution <- function(n, m)
{
  x = c(sample(1:(2 * n), n, rep=FALSE))
  y = c(sample(1:(2 * m), m, rep=FALSE))
  x = sort(x)
  y = sort(y)
  p = c(runif(n, 1, 10 * n)) 
  q = c(runif(m, 1, 10 * n))
  sum_p = sum(p)
  sum_q = sum(q)
  normalised_p = c(p / sum_p)
  normalised_q = c(q / sum_q)
  normalised_p = c(floor(normalised_p * 1000) / 1000) # probabilitati doar cu 3 zecimale
  normalised_q = c(floor(normalised_q * 1000) / 1000) # probabilitati doar cu 3 zecimale
  # verific daca suma probabilitatilor este 1 in urma transformarii la 3 zecimale, daca nu adaug restul pana la 1 la prima valoare
  if (sum(normalised_p) != 1.000) {
    rest = 1.000 - sum(normalised_p)
    normalised_p[1] = (normalised_p[1] + rest)
  }
  if (sum(normalised_q) != 1.000) {
    rest = 1.000 - sum(normalised_q)
    normalised_q[1] = (normalised_q[1] + rest)
  }
  x_variable = matrix(0, nrow = 2, ncol = n)
  y_variable = matrix(0, nrow = 2, ncol = m)
  
  for (col in 1:ncol(x_variable)) {
    x_variable[1, col] = x[col]
    x_variable[2, col] = normalised_p[col]
  }
  for (col in 1:ncol(y_variable)) {
    y_variable[1, col] = y[col]
    y_variable[2, col] = normalised_q[col]
  }
  
  joint_distribution = matrix(0, nrow = (n + 2), ncol = (m + 2))
  colnames_joint_distribution = c(1:m)
  rownames_joint_distribution = c(1:n)
  for (i in 1:m) {
    colnames_joint_distribution[i] = paste(c("y", colnames_joint_distribution[i]), collapse = "", sep = "")
  }
  for (i in 1:n) {
  rownames_joint_distribution[i] = paste(c("x", rownames_joint_distribution[i]), collapse = "", sep = "")
  }
  rownames(joint_distribution) = c("Y", rownames_joint_distribution, "q")
  colnames(joint_distribution) = c("X", colnames_joint_distribution, "p")
  for (col in 1:ncol(x_variable)) {
    joint_distribution[col + 1, m + 2] = x_variable[2, col]
    joint_distribution[col + 1, 1] = x_variable[1, col]
  }
  for (col in 1:ncol(y_variable)) {
    joint_distribution[n + 2, col + 1] = y_variable[2, col]
    joint_distribution[1, col + 1] = y_variable[1, col]
  }
  for (row in 2:(nrow(joint_distribution) - 1)) {
    for (col in 2:(ncol(joint_distribution) - 1)) {
      joint_distribution[row, col] = x_variable[2, row - 1] * y_variable[2, col - 1]
    }
  }
  if (decide_dependent()) {
    # aleg minimul dintre joint_distribution[2, 2]; [2, 3]; [3, 2]; [3, 3]
    # scad din el jumate, jumate din el adaug in elementul de pe aceeasi linie 
    # si coloana din acest patrat, si jumate din el scad de pe cel din diagonala
    # astfel nu voi mai avea variabilele independente, produsul probabilitatilor p1 si q1
    # va fi diferit de valoarea ce se afla corespunzatoare acestuia in repartitia comuna si anume [2, 2]
    mini = 1
    posx = 1
    posy = 1
    for (row in 2:3) {
      for (col in 2:3) {
        if (mini > joint_distribution[row, col]) {
          mini = joint_distribution[row, col]
          posx = row
          posy = col
        }  
      }
    }
    half_of_mini = mini / 2
    joint_distribution[posx, posy] = half_of_mini # elementul care contine minimul
    joint_distribution[posx + ((-1) ^ posx), posy + ((-1) ^ posy)] = joint_distribution[posx + ((-1) ^ posx), posy + ((-1) ^ posy)] - half_of_mini # elementul de pe diagonala
    joint_distribution[posx, posy + ((-1) ^ posy)] = joint_distribution[posx, posy + ((-1) ^ posy)] + half_of_mini # elementul de pe aceeasi linie
    joint_distribution[posx + ((-1) ^ posx), posy] = joint_distribution[posx + ((-1) ^ posx), posy] + half_of_mini # elementul de pe aceeasi coloana 
  }
  return(joint_distribution)
}

erase_some_values <- function(joint_distribution) 
{
    # Voi sterge elementele de pe diagonala principala si primele doua elemente de pe prima linie
    # pozitiile de pe care elementele au fost sterse sunt marcate cu -1
    joint_distribution[2, 2] = -1
    joint_distribution[2, 3] = -1
    mini = min(nrow(joint_distribution), ncol(joint_distribution)) - 1
    for (p in 3:mini) {
      joint_distribution[p, p] = -1
    }
    return(joint_distribution)
}

frepcomgen <- function(n, m)
{
  joint_distribution = generate_random_joint_distribution(n, m)
#  joint_distribution_table = as.table(joint_distribution)
  print("Repartitia comuna completa")
  print(joint_distribution)
  partial_joint_distribution = erase_some_values(joint_distribution = joint_distribution)
  print("Repartitia comuna incompleta")
  print(partial_joint_distribution)
  return(partial_joint_distribution)
}

fcomplrepcom <- function(A) 
{
  R = nrow(A)
  C = ncol(A)
  ok = TRUE
  while (ok == TRUE) {
    ok = FALSE
    for (row in 2:(R - 1)) {
      for (col in 2:(C - 1)) {
        if (A[row, col] == -1) {
          ans = solve_element(A, row, col, R, C)
          if (ans != -1) {
            ok = TRUE
            A[row, col] = ans
          }
        }
      }
    }
  }
  return(A)
}

variance_discrete_variable <- function(discrete_var) {
  mean_var = 0
  mean_var2 = 0
  for (col in ncol(discrete_var)) {
    mean_var2 = (mean_var2 + (discrete_var[1, col] ^ 2) * discrete_var[2, col])
    mean_var = (mean_var + discrete_var[1, col] * discrete_var[2, col])
  }
  res = mean_var2 - (mean_var ^ 2)
  return(res)
}

covariance_discrete_variables <- function(joint_distribution) {
  mean_xy = 0
  for (row in 2:(nrow(joint_distribution) - 1)) {
    for (col in 2:(ncol(joint_distribution) - 1)) {
      mean_xy = (mean_xy + (joint_distribution[row, col] * joint_distribution[1, col] * joint_distribution[row, 1]))
    }
  }
  mean_x = 0
  mean_y = 0
  for (row in 2:(nrow(joint_distribution) - 1)) {
    mean_x = (joint_distribution[row, 1] * joint_distribution[row, ncol(joint_distribution)] + mean_x)
  }
  for (col in 2:(ncol(joint_distribution) - 1)) {
    mean_y = (joint_distribution[1, col] * joint_distribution[nrow(joint_distribution), col] + mean_y)
  }
  print("Mean xy")
  print(mean_xy)
  print("Mean x")
  print(mean_x)
  print("Mean y")
  print(mean_y)
  res = (mean_xy - (mean_x * mean_y) )
  return(res)
}

covariance_5x_3y <- function(joint_distribution) 
{
  ans = covariance_discrete_variables(joint_distribution)
  ans = (ans * (-3) * (5))
  print("Cov(5X,-3Y)")
  print(ans)
} 

fverind <- function(joint_distribution) {
  print(joint_distribution)
  R = nrow(joint_distribution)
  C = ncol(joint_distribution)
  
  for (row in 2:(R - 1)){
    for (col in 2:(C - 1)) {
      if (joint_distribution[row, col] %!=% (joint_distribution[row, C] * joint_distribution[R, col])) {
          print("Variabilele sunt dependente")
        }
      }
  }
  print("Variabilele sunt independente")
  return(TRUE)
}

fvernecor <- function(joint_distribution) {
  # extrag variabilele dim repartitia comuna
  R = nrow(joint_distribution)
  C = ncol(joint_distribution)
  X = matrix(0, nrow = 2, ncol = R - 2)
  Y = matrix(0, nrow = 2, ncol = C - 2)
  for (col in 1:ncol(X)) {
    X[1, col] = joint_distribution[col + 1, 1]
    X[2, col] = joint_distribution[col + 1, C]
  }
  for (col in 1:ncol(Y)) {
    Y[1, col] = joint_distribution[1, col + 1]
    Y[2, col] = joint_distribution[R, col + 1]
  }
  variance_X = variance_discrete_variable(X)
  variance_Y = variance_discrete_variable(Y)
  cov_xy = covariance_discrete_variables(joint_distribution)
  print("Varianta lui X")
  print(variance_X)
  print("Varianta lui Y")
  print(variance_Y)
  print("Covarianta")
  print(cov_xy)
  fi_xy = (cov_xy / ((sqrt(variance_X) * sqrt(variance_Y))))
  print("φ(x, y):")
  print(fi_xy)
  if (abs(fi_xy) >= 0.75 && abs(fi_xy) <= 1) {
    print("Puternic corelate")
  } else if (abs(fi_xy) >= 0.25 && abs(fi_xy) < 0.75) {
    print("Corelate")
  } else if (abs(fi_xy) > 0 && abs(fi_xy) < 0.25) {
    print("Slab corelate")
  } else if (abs(fi_xy) == 0) {
    print("Necorelate")
  }
}

calcP <- function(joint_distribution) {
  s1 = 0
  s2 = 0
  for (row in 2:(nrow(joint_distribution) - 1)){
    for (col in 2:(ncol(joint_distribution) - 1)) {
      if (joint_distribution[1, col] > 2) {
        s2 = s2 + joint_distribution[row, col]
        if (joint_distribution[row, 1] > 0 && joint_distribution[row, 1] < 3) {
          s1 = s1 + joint_distribution[row, col]
        }
      } 
    }
  }
  res1 = s1 / s2
  print("P(0<X<3/Y>2)")
  print(res1)
  res2 = 0
  for (row in 2:(nrow(joint_distribution) - 1)) {
    for (col in 2:(ncol(joint_distribution) - 1)) {
      if (joint_distribution[row, 1] > 6 && joint_distribution[1, col] < 7) {
        res2 = res2 + joint_distribution[row, col]
      }
    }
  }
  print("P(X>6,Y<7)")
  print(res2)
}

partial_joint_distribution = frepcomgen(2,3)
completed_joint_distribution = fcomplrepcom(partial_joint_distribution)
print("Repartitia comuna completata:")
print(completed_joint_distribution)
covariance_5x_3y(completed_joint_distribution)
fvernecor(completed_joint_distribution)
calcP(completed_joint_distribution)
