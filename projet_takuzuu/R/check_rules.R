# Vérifie qu’il n’y a pas de 000 ou 111
check_no_triplets <- function(grid) {
  check_row <- apply(grid, 1, function(row) {
    !grepl("000|111", paste(row, collapse = ""))
  })
  check_col <- apply(grid, 2, function(col) {
    !grepl("000|111", paste(col, collapse = ""))
  })
  all(check_row) && all(check_col)
}

# Vérifie qu’il n’y a pas plus de la moitié de 0 ou de 1 par ligne et colonne
check_balance <- function(grid) {
  n <- nrow(grid)
  check_row <- apply(grid, 1, function(row) {
    sum(row == 0, na.rm = TRUE) <= n / 2 &&
      sum(row == 1, na.rm = TRUE) <= n / 2
  })
  check_col <- apply(grid, 2, function(col) {
    sum(col == 0, na.rm = TRUE) <= n / 2 &&
      sum(col == 1, na.rm = TRUE) <= n / 2
  })
  all(check_row) && all(check_col)
}

# Vérifie que toutes les lignes et colonnes sont uniques (pas de doublons)
check_unique_rows_cols <- function(grid) {
  rows <- apply(grid, 1, paste, collapse = "")
  cols <- apply(grid, 2, paste, collapse = "")

  !any(duplicated(rows[!grepl("NA", rows)])) &&
    !any(duplicated(cols[!grepl("NA", cols)]))
}

# Vérifie qu'il n'y a aucune case vide (NA) dans la grille
check_no_na <- function(grid) {
  all(!is.na(grid))
}

# Vérifie toutes les règles du Takuzu
check_takuzu_valid <- function(grid) {
  check_no_triplets(grid) &&
    check_balance(grid) &&
    check_unique_rows_cols(grid) &&
    check_no_na(grid)
}

