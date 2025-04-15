#' Vérifie l'absence de triplets (000 ou 111) dans une grille Takuzu
#'
#' @param grid Une matrice Takuzu avec des 0, 1 ou NA
#' @return TRUE si aucun triplet n'est présent, FALSE sinon
#' @export
check_no_triplets <- function(grid) {
  has_triplet <- function(vec) {
    str <- paste(vec, collapse = "")
    grepl("000|111", str)
  }
  
  rows <- apply(grid, 1, function(row) !has_triplet(na.omit(row)))
  cols <- apply(grid, 2, function(col) !has_triplet(na.omit(col)))
  
  all(rows) && all(cols)
}

#' Vérifie l'équilibre des 0 et 1 dans chaque ligne et colonne
#'
#' @param grid Une matrice Takuzu
#' @return TRUE si chaque ligne/colonne contient au plus la moitié de 0 ou de 1
#' @export
check_balance <- function(grid) {
  half <- ncol(grid) / 2
  
  check_line <- function(x) {
    t <- table(factor(x, levels = c(0, 1)))
    all(t[1] <= half, t[2] <= half)
  }
  
  all(apply(grid, 1, check_line)) && all(apply(grid, 2, check_line))
}

#' Vérifie que toutes les lignes et colonnes sont uniques
#'
#' @param grid Une matrice Takuzu
#' @return TRUE si toutes les lignes et colonnes sont différentes (ignorant les lignes/colonnes incomplètes)
#' @export
check_unique_rows_cols <- function(grid) {
  complete_rows <- apply(grid, 1, function(row) !any(is.na(row)))
  complete_cols <- apply(grid, 2, function(col) !any(is.na(col)))
  
  unique_rows <- nrow(unique(grid[complete_rows, , drop = FALSE])) == sum(complete_rows)
  unique_cols <- ncol(unique(t(grid[, complete_cols, drop = FALSE]))) == sum(complete_cols)
  
  unique_rows && unique_cols
}

#' Vérifie que la grille ne contient pas de valeurs manquantes (NA)
#'
#' @param grid Une matrice Takuzu
#' @return TRUE si aucune case n'est vide, FALSE sinon
#' @export
check_no_na <- function(grid) {
  !any(is.na(grid))
}