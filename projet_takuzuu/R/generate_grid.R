#' Génère une grille Takuzu partiellement remplie sans triplets
#'
#' @param size Choix de la taille de la grille : 4, 6 ou 8 (par défaut 8)
#' @param filled_cases Nombre de cases à remplir aléatoirement (doit être raisonnable)
#'
#' @return Une matrice n x n avec des 0, 1 et NA pour les cases vides
#' @export
generate_grid <- function(size = 8, filled_cases = 20) {
  if (!(size %in% c(4, 6, 8))) stop("La taille de la grille doit être 4, 6 ou 8.")
  if (size %% 2 != 0) stop("La taille de la grille doit être paire.")
  
  n <- size
  grid <- matrix(NA, nrow = n, ncol = n)
  count <- 0
  max_attempts <- 500
  
  while (count < filled_cases && max_attempts > 0) {
    i <- sample(1:n, 1)
    j <- sample(1:n, 1)
    
    if (is.na(grid[i, j])) {
      value <- sample(c(0, 1), 1)
      grid[i, j] <- value
      
      if (check_no_triplets(grid)) {
        count <- count + 1
      } else {
        grid[i, j] <- NA  # remet à vide si ça crée un triplet
      }
      max_attempts <- max_attempts - 1
    }
  }
  
  # S'assurer que les cases sont bien 0, 1 ou NA
  grid <- apply(grid, c(1, 2), function(x) if (is.na(x)) NA else as.integer(x))
  
  return(grid)
}