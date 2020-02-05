library(fpCompare) #pentru precizie la comparatii 

erase_elements <- function(joint_distribution) {
  R = nrow(joint_distribution)
  C = ncol(joint_distribution)
  erase = matrix(TRUE, nrow = R + 1, ncol = C + 1)
  for (i in 1:nrow(erase)) {
    erase[i, 1] = FALSE
    erase[i, ncol(erase)] = FALSE
  }
  for (i in 1:ncol(erase)) {
    erase[1, i] = FALSE
    erase[nrow(erase), i] = FALSE
  }
  ok = TRUE
  posx = 2
  posy = 2
  d = 1 # 1 - merg in dreapta
        # 2 - merg in jos
        # 3 - merg in stanga
        # 4 - merg in sus
  while(ok) {
    if (d == 1) {
      if (erase[posx, posy + 1] == TRUE){ # daca e liber in dreapta merg
        erase[posx, posy] = FALSE
        posy = posy + 1
      } else if (erase[posx + 1, posy] == TRUE) { # daca nu e liber in dreapta merg in jos
        joint_distribution[posx, posy] = -1
        d = 2
      } else {
        joint_distribution[posx, posy] = -1 # daca nu pot nici in jos atunci ma opresc
        ok = FALSE
      }
    } else if (d == 2) {
      if (erase[posx + 1, posy] == TRUE) {
        erase[posx, posy] = FALSE
        posx = posx + 1
      } else if (erase[posx, posy - 1] == TRUE) {
        joint_distribution[posx, posy] = -1
        d = 3
      } else {
        joint_distribution[posx, posy] = -1
        ok = FALSE
      }
    } else if (d == 3) {
      if (erase[posx, posy - 1] == TRUE) {
        erase[posx, posy] = FALSE
        posy = posy - 1
      } else if (erase[posx - 1, posy] == TRUE) {
        joint_distribution[posx, posy] = -1
        d = 4
      } else {
        joint_distribution[posx, posy] = -1
        ok = FALSE
      }
    } else if (d == 4) {
      if (erase[posx - 1, posy] == TRUE) {
        erase[posx, posy] = FALSE
        posx = posx - 1
      } else if (erase[posx, posy + 1] == TRUE) {
        joint_distribution[posx, posy] = -1
        d = 1
      } else {
        joint_distribution[posx, posy] = -1
        ok = FALSE
      }
    }
  }
  joint_distribution[R, C] = 0
  return(joint_distribution)
}

solve_element <- function(A, r, c, R, C) {
  # verific daca numarul elementelor de pe aceeasi linie este egal cu C - 3, unde C este numarul coloanelor repartitiei comune
  # daca da, inseamna ca elementul ca elementul se poate afla, folosind faptul ca suma elementelor de pe aceeasi linie este egala
  # cu probabilitatea variabilei X corespunzatoare liniei respective
  # daca nu, atunci fac acelasi lucru si pe coloana 
  # daca nici acum nu am aflat elementul, returnez -1
  if (r != R && c != C)
  {
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
    if (no_elem_line == C - 3 && A[r, C] != -1) {
      ans = A[r, C] - sum_line
      return(ans)
    }
    for (row in 2:(R - 1)) {
      if (A[row, c] != -1) {
        no_elem_col = 1 + no_elem_col
        sum_col = sum_col + A[row, c]
      }
    }
    if (no_elem_col == R - 3 && A[R, c] != -1) {
      ans = A[R, c] - sum_col
      return(ans)
    }
  } else if (r == R) {
    no_elem_line = 0
    no_elem_col = 0
    sum_line = 0
    sum_col = 0
    for (col in 2:C) {
      if (A[r, col] != -1) {
        no_elem_line = no_elem_line + 1
        sum_line = sum_line + A[r, col]
      }
    }
    if (no_elem_line == C - 2) {
      ans = 1 - sum_line
      return(ans)
    }
    for (row in 2:R) {
      if (A[row, c] != -1) {
        no_elem_col = no_elem_col + 1
        sum_col = sum_col + A[row, c]
      }
    }
    if (no_elem_col == R - 2) {
      ans = sum_col
      return(ans)
    }
  } else if (c == C) {
    no_elem_line = 0
    no_elem_col = 0
    sum_line = 0
    sum_col = 0
    for (col in 2:C) {
      if (A[r, col] != -1) {
        no_elem_line = no_elem_line + 1
        sum_line = sum_line + A[r, col]
      }
    }
    if (no_elem_line == C - 2) {
      ans = sum_line
      return(ans)
    }
    for (row in 2:R) {
      if (A[row, c] != -1) {
        no_elem_col = no_elem_col + 1
        sum_col = sum_col + A[row, c]
      }
    }
    if (no_elem_col == R - 2) {
      ans = 1 - sum_col
      return(ans)
    }
  }
  ans = -1
  return(ans)
}


generate_random_joint_distribution <- function(n, m)
{
  x = c(sample(1:(10 * n), n, rep=FALSE))
  y = c(sample(1:(10 * m), m, rep=FALSE))
  x = sort(x)
  y = sort(y)
  aux = matrix(sample(1:(n*m), n * m, replace = TRUE), nrow = n, ncol = m)
  joint_distribution = matrix(0, nrow = n + 2, ncol = m + 2)
  for (i in 1:n) {
    joint_distribution[i + 1, 1] = x[i]
    joint_distribution[i + 1, m + 2] = rowSums(aux)[i]
  }
  for (i in 1:m) {
    joint_distribution[1, i + 1] = y[i]
    joint_distribution[n + 2, i + 1] = colSums(aux)[i]
  }
  for (i in 2:(n + 1)) {
    for (j in 2:(m + 1)) {
      joint_distribution[i, j] = aux[i - 1,j - 1]
    }
  }
  for (i in 2:(n + 2)) {
    for (j in 2:(m + 2)) {
      joint_distribution[i, j] = joint_distribution[i, j] / sum(aux)
    }
  }
  
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
  return(joint_distribution)
}

frepcomgen <- function(n, m)
{
  joint_distribution = generate_random_joint_distribution(n, m)
  print("Repartitia comuna completa")
  print(joint_distribution)
  partial_joint_distribution = erase_elements(joint_distribution = joint_distribution)
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
    for (row in 2:R) {
      for (col in 2:C) {
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
  R = nrow(joint_distribution)
  C = ncol(joint_distribution)
  
  for (row in 2:(R - 1)){
    for (col in 2:(C - 1)) {
      if (joint_distribution[row, col] %!=% (joint_distribution[row, C] * joint_distribution[R, col])) {
          print("Variabilele sunt dependente")
          return()
        }
      }
  }
  print("Variabilele sunt independente")
  return()
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

partial_joint_distribution = frepcomgen(4,3)
completed_joint_distribution = fcomplrepcom(partial_joint_distribution)
print("Repartitia comuna completata:")
print(completed_joint_distribution)
covariance_5x_3y(completed_joint_distribution)
fverind(completed_joint_distribution)
fvernecor(completed_joint_distribution)
calcP(completed_joint_distribution)
