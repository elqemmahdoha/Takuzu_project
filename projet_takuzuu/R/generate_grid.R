#' Génère une grille de Takuzu jouable avec solution
#'
#' Cette fonction génère une grille complète de Takuzu (également appelé Binairo),
#' puis masque une proportion de cases pour créer une grille jouable. La solution
#' complète est également retournée. Le Takuzu est un jeu de logique avec les règles suivantes :
#' - Chaque ligne et colonne doit contenir autant de 0 que de 1
#' - Pas plus de deux 0 ou deux 1 consécutifs
#' - Aucune ligne ou colonne identique n'est autorisée
#'
#' @param taille Un entier : taille de la grille (doit être 4, 6 ou 8)
#' @param proportion_visible Une valeur entre 0 et 1 : proportion de cases visibles dans la grille jouable
#'
#' @return Une liste contenant :
#' \describe{
#'   \item{grille_visible}{Matrice de taille \code{taille x taille} avec des valeurs 0, 1 ou NA (cases à remplir)}
#'   \item{solution}{Matrice complète représentant la solution de la grille}
#' }
#'
#' @examples
#' jeu <- generer_takuzu_jouable(taille = 6, proportion_visible = 0.4)
#' print(jeu$grille_visible)
#' print(jeu$solution)
#'
#' @export

generer_takuzu_jouable <- function(taille = 8, proportion_visible = 0.3) {
  if (!(taille %in% c(4, 6, 8))) stop("La taille doit être 4, 6 ou 8")

  # VALIDATION RAPIDE
  valider_case <- function(grid, i, j) {
    check_no_triplets <- function(vec) {
      for (k in 1:(length(vec) - 2)) {
        if (!any(is.na(vec[k:(k+2)])) && length(unique(vec[k:(k+2)])) == 1)
          return(FALSE)
      }
      TRUE
    }

    check_balance <- function(vec) {
      n0 <- sum(vec == 0, na.rm = TRUE)
      n1 <- sum(vec == 1, na.rm = TRUE)
      max_val <- length(vec) / 2
      n0 <= max_val && n1 <= max_val
    }

    row <- grid[i, ]
    col <- grid[, j]
    check_no_triplets(row) && check_no_triplets(col) &&
      check_balance(row) && check_balance(col)
  }

  lignes_colonnes_uniques <- function(grid) {
    lignes <- apply(grid, 1, paste0, collapse = "")
    colonnes <- apply(grid, 2, paste0, collapse = "")
    length(unique(lignes)) == nrow(grid) && length(unique(colonnes)) == ncol(grid)
  }

  # BACKTRACKING OPTIMISÉ
  backtrack <- function(grid, i = 1, j = 1) {
    if (i > taille) {
      if (lignes_colonnes_uniques(grid)) return(grid)
      else return(NULL)
    }

    next_i <- if (j == taille) i + 1 else i
    next_j <- if (j == taille) 1 else j + 1

    if (!is.na(grid[i, j])) return(backtrack(grid, next_i, next_j))

    for (val in sample(0:1)) {
      grid[i, j] <- val
      if (valider_case(grid, i, j)) {
        sol <- backtrack(grid, next_i, next_j)
        if (!is.null(sol)) return(sol)
      }
    }

    return(NULL)
  }

  # GÉNÉRATION SOLUTION
  grille_vide <- matrix(NA, nrow = taille, ncol = taille)
  solution <- backtrack(grille_vide)

  if (is.null(solution)) stop("Échec de la génération de la grille complète.")

  # MASQUER DES CASES
  total_cases <- taille^2
  nb_visible <- ceiling(total_cases * proportion_visible)
  indices <- sample(1:total_cases, nb_visible)

  grille_visible <- matrix(NA, nrow = taille, ncol = taille)
  grille_visible[indices] <- solution[indices]

  return(list(
    grille_visible = grille_visible,
    solution = solution
  ))
}
