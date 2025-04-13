#' Génère une grille Takuzu valide et la rend jouable
#'
#' @param taille Taille de la grille (4, 6 ou 8)
#' @param proportion_visible Proportion de cases visibles (ex: 0.35)
#' @return Une liste avec deux éléments : grille_visible et solution
#' @export
generer_takuzu_jouable <- function(taille = 8, proportion_visible = 0.3) {
  if (!(taille %in% c(4, 6, 8))) stop("La taille doit être 4, 6 ou 8")

  # ==== FONCTIONS DE VALIDATION ====
  est_valide <- function(grid) {
    check_no_triplets <- function(vec) {
      if (any(is.na(vec))) return(TRUE)
      all(rle(vec)$lengths <= 2)
    }

    check_balance <- function(vec) {
      n0 <- sum(vec == 0, na.rm = TRUE)
      n1 <- sum(vec == 1, na.rm = TRUE)
      total <- length(vec)
      max_val <- total / 2
      n0 <= max_val && n1 <= max_val &&
        (n0 + n1 == total || TRUE)
    }

    lignes_uniques <- function(mat) {
      lignes_completes <- mat[complete.cases(mat), , drop = FALSE]
      !any(duplicated(apply(lignes_completes, 1, paste0, collapse = "")))
    }

    for (i in 1:nrow(grid)) {
      row <- grid[i, ]
      col <- grid[, i]
      if (!check_no_triplets(row) || !check_no_triplets(col)) return(FALSE)
      if (!check_balance(row) || !check_balance(col)) return(FALSE)
    }
    lignes_uniques(grid) && lignes_uniques(t(grid))
  }

  # ==== BACKTRACKING POUR REMPLIR LA GRILLE ====
  backtrack <- function(grid, i = 1, j = 1) {
    if (i > taille) return(grid)

    next_i <- if (j == taille) i + 1 else i
    next_j <- if (j == taille) 1 else j + 1

    if (!is.na(grid[i, j])) return(backtrack(grid, next_i, next_j))

    for (val in sample(0:1)) {
      grid[i, j] <- val
      if (est_valide(grid)) {
        sol <- backtrack(grid, next_i, next_j)
        if (!is.null(sol)) return(sol)
      }
    }

    return(NULL)
  }

  # ==== ÉTAPE 1 : Génération de la grille complète ====
  grille_vide <- matrix(NA, nrow = taille, ncol = taille)
  solution <- backtrack(grille_vide)

  if (is.null(solution)) stop("Échec de la génération de la grille complète.")

  # ==== ÉTAPE 2 : Création de la grille jouable ====
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
#exemple pour tester la fonction :
#jeu <- generer_takuzu_jouable(taille = 6, proportion_visible = 0.35)

#cat("🎯 Grille Jouable :\n")
#print(jeu$grille_visible)

#cat("\n✅ Solution Complète :\n")
#print(jeu$solution)
