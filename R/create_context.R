#' Create a context object for a correction set
#'
#' @description
#' Create a list that contains context for a given correction set (must be
#' recalculated when a different correction set/project is selected).
#'
#' @param correction
#' @param base_corr_dir
#' @param base_templ_dir
#' @param base_repos_dir
#' @param repositories
#' @param assignments
#' @param repo
#' @param app
#' @param github_url
#' @param branch
#'
#' @return
#' @export
#'
#' @examples
create_context <- function(correction, base_corr_dir, base_templ_dir,
  base_repos_dir, repositories, assignments, repo, app, github_url, branch) {
  # Note: for now, it is hardcoded in default_correction at the top of the file!
  # Populate a list of correction folders available
  course_dirs <- fs::dir_ls(base_corr_dir, type = "directory")
  if (!length(course_dirs))
    stop("Il n'y a pas encore de grilles de correction disponibles. ",
      "Utilisez le script '04create_assessments3.R' pour les créer...")
  courses <- basename(course_dirs)

  corr_dirs <- fs::dir_ls(course_dirs, type = "directory")
  if (!length(corr_dirs))
    stop("Il n'y a pas encore de grilles de correction disponibles. ",
      "Utilisez le script '04create_assessments3.R' pour les créer...")
  corrections <- basename(corr_dirs)

  if (is.null(correction) || !length(correction) || correction == "")
    stop("Correction set non fourni, ou opération annulée.")
  if (!correction %in% corrections)
    stop("Correction set '", correction, "' introuvable. Vérifiez sa valeur...")

  # Get corr_dir for this correction + get course, assignment and corr_date
  corr_dir <- corr_dirs[corrections == correction]
  course <- basename(dirname(corr_dir))
  corr_parts <- strsplit(correction, "_", fixed = TRUE)[[1]]
  if (length(corr_parts) < 4)
    stop("Il y a un problème avec le set de corrections '", correction,
      "'  : il devrait être quelque chose comme A00Ia_21M_titre_2022-01-01")
  corr_date  <- corr_parts[length(corr_parts)]
  assignment <- paste(corr_parts[-length(corr_parts)], collapse = "_")
  rm(corr_parts)

  # Get dirs, files and more for this correction set
  templ_file <- file_path_check(base_templ_dir, course,
    paste0(assignment, ".csv"))
  repos_dir <- dir_path_check(base_repos_dir, course, assignment)
  corr_files <- fs::dir_ls(corr_dir, glob = '*.csv')
  if (!length(corr_files))
    stop("Correction files not found in '", corr_dir, "' !")
  n <- length(corr_files)
  repos_names <- sub("\\.csv$", "", basename(corr_files))
  # PhG: adapted for urchins in Q2
  repos_names2 <- paste0(substring(repo, 1L, 3L), substring(repos_names, 4L))

  repos_dirs <- path(repos_dir, repos_names)
  repos_dirs2 <- path(repos_dir, repos_names2)
  user_logins <- substring(repos_names, nchar(assignment) + 2)

  # General assignment infos from assign_file
  assign_infos <- as.data.frame(assignments)[tolower(assignments$assignment) ==
      tolower(assignment), ]
  # Also filter on app, if provided
  if (!is.null(app))
    assign_infos <- assign_infos[tolower(assign_infos$app) == tolower(app), ]
  if (!NROW(assign_infos)) {
    stop("L'exercice sélectionné (", assignment, ") n'est pas trouvé dans ",
      "la table des exercices.")
  }

  if (NROW(assign_infos) > 1) {
    warning("L'exercice sélectionné (", assignment,
      ") est trouvé dans la table ",
      " des exercices en plusieurs exemplaires. Utilisation du premier.")
    assign_infos <- assign_infos[1, ]
  }

  # Read information from the template file
  templ_corrs <- read(templ_file)
  if (NROW(templ_corrs) < 1)
    stop("No correction items found in '", templ_file,
      "', or error reading the file.")

  # Eliminate user-specific items (starting with !{login} in the template)
  templ_corrs <-
    templ_corrs[substring(templ_corrs$criterion, 1, 8) != "!{login}", ]

  # The context object (list) contains info required to populate tables
  context <- list(
    assignment = assignment,     # The corresponding assignment (project)
    templ_corrs = templ_corrs,   # The content of the correction grid template
    corr_files = corr_files,     # The correction grids files (CSV)
    assign_infos = assign_infos, # Informations about the assignment
    github_url = github_url,     # The base URL for GitHub links
    branch = branch,             # The GitHub branch
    user_logins = user_logins,   # The users logins (teams for group projects)
    repos_names = repos_names,   # The names of repositories
    repos_names2 = repos_names2, # The names of repos if different from apps
    repos_dirs = repos_dirs,     # The paths to the repositories
    repos_dirs2 = repos_dirs2    # The paths to repos if different from apps
  )
  context
}
