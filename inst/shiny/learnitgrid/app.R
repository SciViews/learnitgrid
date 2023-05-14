# TODO: rework this to make it a proper R package Shiny app
library(learnitgrid)

# For now, you must hardcode the correction folder to use here...
## Special case: default_correction <- "A01Ga_22M_biology_challenge_2022-10-09"
#default_correction <- "A01Ia_22M_covid19_2022-10-07"
#default_correction <- "A02Ga_22M_describe_charts_2022-10-17"
#default_correction <- "A02Ia_22M_scatterplot_2022-10-17"
#default_correction <- "A04Ia_22M_graphe_avance_2022-02-13"
#default_correction <- "A03Ga_22M_urchin_2022-02-18"
#default_correction <- "B01Ib_22M_abalone_2022-10-17"
#default_correction <- "B02Ib_22M_achatina_2022-02-11"
#default_correction <- "B02Ga_22M_urchin_2022-02-22"
#default_correction <- "B03Ia_22M_ovocyte_2022-02-19"
#default_correction <- "B04Gb_22M_bacterial_growth_2022-03-22"
#default_correction <- "B05Ia_22M_abies_balsamea_2022-12-07"
#default_correction <- "C01Ia_22M_ml1_2022-02-22"
#default_correction <- "C05Gb_22M_ts_adv_2023-01-10"
# Q2
default_correction <- "A08Ia_22M_pea_2023-03-09"
#default_correction <- "A08Ga_22M_urchin_2022-06-17"
#default_correction <- "A09Ia_22M_cardiovascular_2022-05-31"
#default_correction <- "A11Ia_22M_anova2_2022-05-31"
#default_correction <- "A12Ia_22M_correlation_2022-05-31"
#default_correction <- "B05Ia_22M_ligurian_sea_2022-05-31"
#default_correction <- "B06Ia_22M_fish_market_2022-05-31"
#default_correction <- "B07Ia_22M_acp_afc_2022-05-31"
#default_correction <- "B08Ia_22M_mfa_2022-05-31"
app <- NULL
#app <- "A08Ia_pea"
repo <- default_correction # Could possibly be different
message("Correction Science des Données Biologiques démarrée... pour '",
  default_correction, "', soyez patient.")

# This section is here to ease debugging...
#setwd("/Volumes/Cuttlefish3/sdd-projects/") # If not debugging, leave commented
default_item <- "" # Only change this for quicker debugging purposes
default_grid <- "" # Only change this for quicker debugging purposes
# Note that during the execution of the app, these two variables contain the
# context for, respectively, the table by criterion (default_item), or the table
# by grid (default_grid)


# Global constants --------------------------------------------------------

github_url        <- "https://github.com/BioDataScience-Course" # GitHub URL
branch            <- "master" # The branch to use in the git repo
max_lines         <- 20 # Maximum number of lines to place in content

root_dir          <- dir_path_check("www")
base_corr_dir     <- dir_path_check(root_dir, "corrections")
base_templ_dir    <- dir_path_check(root_dir, "templates")
base_repos_dir    <- dir_path_check(root_dir, "repos")

# TODO: this could be located possibly elsewhere and with a different name
repos_file        <- file_path_check(root_dir, "repositories_22.csv")
assign_file       <- file_path_check(root_dir, "assignments_22.csv")

default_highlight <- FALSE # This is nice, but slow => better to leave it FALSE


# Global objects ----------------------------------------------------------

repositories      <- read(repos_file)
assignments       <- read(assign_file)
order             <- NULL # The order for the table by criterion
context           <- create_context(correction = default_correction,
  base_corr_dir = base_corr_dir, base_templ_dir = base_templ_dir,
  base_repos_dir = base_repos_dir, repositories = repositories,
  assignments = assignments, repo = repo, app = app,
  github_url = github_url, branch = branch) # Context object for
# selected project/correction set
# TODO: get login of user if app is run in RStudio Connect
default_evaluator <-  try(gh_whoami()$login, silent = TRUE)
if (inherits(default_evaluator, 'try-error') || is.null(default_evaluator)) {
  default_evaluator <- ""
} else {
  default_evaluator <- as.character(default_evaluator)[1]
}

# Initial values for default_item and default_grid
# If default_item is already defined (debugging purpose), check it is correct
if (default_item != "") {
  if (!default_item %in% context$templ_corrs$criterion)
    stop("Item '", default_item, "' non trouvé dans la grille d'évaluation.")
} else {# Use first item by default
  default_item <- context$templ_corrs$criterion[1]
}
# If default_grid is already defined (debugging purpose), check it is correct
if (default_grid != "") {
  if (!default_grid %in% context$repos_names)
    stop("Item '", default_grid, "' non trouvé dans les grilles de correction.")
} else {# Use first item by default
  default_grid <- context$repos_names[1]
}

# Sort the table according to content similarities
# Note: depends on a global variable `order` initialized to `NULL`!
# For this reason, it cannot be relocated in learnitgrid_functions.R
sort_table <- function(x, is_content = attr(x, "is_content")) {
  # Calculate a dissimilarity matrix with string distances ('osa' method)
  # TODO: try other methods as well
  if (isTRUE(is_content)) {
    n <- nrow(x)
    dissim_mat <- matrix(0, nrow = n, ncol = n)
    content <- x$content
    stringsim <- stringdist::stringsim
    for (i in 1:n)
      dissim_mat[i, ] <- 1 - stringsim(content, content[i], method = "osa")
    # Create a dist object by keeping only the lower triangle
    dissim <- as.dist(dissim_mat)
    # Cluster this dist object, in order to sort items by dissimilarities
    cl <- flashClust::hclust(dissim, method = "complete")
    #plot(cl) # TODO: we could display this, e.g., below the table or in a tab?
    # We are only interested by order in cl
    order <- cl$order
    # Reorder x accordingly
    x <- x[order, ]

  } else {
    order <- 1:nrow(x) # Initial order simply
  }
  order <<- order # TODO: do this differently!
  attr(x, "order") <- order
  x
}

# In debug mode, run until here and test populate_table()
# By criterion:
#populate_table(items = default_item, grids = "all", context = context)
# By grid:
#populate_table(items = "all", grids = default_grid, context = context)


# The Shiny application ---------------------------------------------------

shinyUI <- fluidPage(
  #shinyFeedback::useShinyFeedback(),
  #titlePanel(paste("Correction de", context$assignment)),
  #sidebarLayout(position = "right",
  #  sidebarPanel("Sidepanel items"),
  #mainPanel(

  fluidRow(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "highlight.css")
    ),
    column(12,
      h3(paste("Correction de", context$assignment)),
      textInput("evaluator", "Identifiant (login GitHub) :",
        value = default_evaluator, width = "70%",
        placeholder = "Votre login GitHub comme évaluateur"),
      checkboxInput("highlight", "Syntaxe colorée pour le code R (lent)",
        default_highlight),
      #p("Type de projet:", strong(assign_infos$type),
      #  paste0("(", assign_infos$acad_year, " ", assign_infos$term, ")")),
      tabsetPanel(
        tabPanel("Par critère",
          selectInput("item", "Critère :", context$templ_corrs$criterion,
            multiple = FALSE, selectize = TRUE, size = NULL, width = '70%',
            selected = default_item),
          br(),
          verbatimTextOutput("last_edited"),
          br(),
          DT::dataTableOutput('correction_table_criterion', width = "100%")
        ),
        tabPanel("Par grille",
          selectInput("grid", "Grille critériée :", context$repos_names,
            multiple = FALSE, selectize = TRUE, size = NULL, width = '70%',
            selected = default_grid),
          br(),
          verbatimTextOutput("last_edited_grid"),
          br(),
          DT::dataTableOutput('correction_table_grid', width = "100%")
        )
      )#,

      # Not used yet!
      #br(),
      #actionButton("reset", "Reset cell edited"),
      #actionButton("viewBtn", "View"),
      #actionButton("saveBtn", "Save")
    )
  )

  #)
  #)
)

# The modal dialog box prompting for evaluator login and correction set
#modal_startup <- modalDialog(
#  "Veuillez vérifier votre identité et sélectionner un set de correction",
#  title = "Correction Science des Données Biologiques",
#  footer = tagList(
#    actionButton("cancel", "Annuler"),
#    actionButton("ok", "Continuer", class = "btn btn-primary btn-lg")
#  )
#)

#js <- c(
#  "correction_table_criterion.on('key',",
#  "  function(e, datatable, key, cell, originalEvent){",
#  "  var targetName = originalEvent.target.localName;",
#  "  if(key == 13 && targetName == 'body'){",
#  "    $(cell.node()).trigger('dblclick.dt');",
#  "  }",
#  "});",
#  "correction_table_criterion.on('keydown', function(e){",
#  "  var keys = [9,13,37,38,39,40];",
#  "  if(e.target.localName == 'input' && keys.indexOf(e.keyCode) > -1){",
#  "    $(e.target).trigger('blur');",
#  "  }",
#  "});",
#  "correction_table_criterion.on('key-focus',",
#  "  function(e, datatable, cell, originalEvent){",
#  "  var targetName = originalEvent.target.localName;",
#  "  var type = originalEvent.type;",
#  "  if(type == 'keydown' && targetName == 'input'){",
#  "    if([9,37,38,39,40].indexOf(originalEvent.keyCode) > -1){",
#  "      $(cell.node()).trigger('dblclick.dt');",
#  "    }",
#  "  }",
#  "});"
#)

shinyServer <- function(input, output) {
  #showModal(modal_startup)

  #observeEvent(input$ok, {
  #  showNotification("Récupération+
  #  3, des données...")
  #  removeModal()
  #})
  #observeEvent(input$cancel, {
  #  removeModal()
  #  stopApp(1)
  #})

  output$correction_table_criterion <- DT::renderDataTable({
    DT::formatStyle(
      DT::datatable(
        sort_table(populate_table(items = input$item, grids = "all",
          context = context, reorder = TRUE, highlight = input$highlight,
          max_lines = max_lines)),
        colnames = c('Max', 'Score&nbsp;Commentaire', 'Critère', 'Contenu',
          'Graphique', 'Liens', 'Evaluateur', 'Étudiant/groupe'),
        rownames = FALSE,
        selection = "none",
        escape = FALSE,
        #callback = DT::JS(js),
        extensions = c("Buttons", "KeyTable"),
        options = list(
          paging = FALSE,
          searching = TRUE,
          #fixedColumns = TRUE,
          autoWidth = TRUE,
          autoFill = TRUE,
          ordering = FALSE,
          dom = 'Bfrtip',
          buttons = c('csv', 'excel'),
          language = list(search = 'Filtrer :'),
          columnDefs = list(
            list(width = '300px', targets = 1), # score_comment
            list(visible = FALSE, targets = 2), # criterion (don't show it here)
            list(width = '1000px', targets = 3) # content
          )
        ),
        editable = list(target = "cell", disable = list(columns = c(0, 2:10))),
        class = "display"
      ), 'score_comment', backgroundColor = "lightgrey"
    )
  })

  output$correction_table_grid <- DT::renderDataTable({
    DT::formatStyle(
      DT::datatable(
        populate_table(items = "all", grids = input$grid, context = context,
          reorder = FALSE, highlight = input$highlight, max_lines = max_lines),
        colnames = c('Max', 'Score&nbsp;Commentaire', 'Critère',  'Contenu',
          'Graphique', 'Liens', 'Evaluateur', 'Étudiant/groupe'),
        rownames = FALSE,
        selection = "none",
        escape = FALSE,
        #callback = DT::JS(js),
        extensions = c("Buttons", "KeyTable"),
        options = list(
          paging = FALSE,
          searching = TRUE,
          #fixedColumns = TRUE,
          autoWidth = TRUE,
          autoFill = TRUE,
          ordering = FALSE,
          dom = 'Bfrtip',
          buttons = c('csv', 'excel'),
          language = list(search = 'Filtrer :'),
          columnDefs = list(
            list(width = '300px', targets = 1),  # score_comment
            #list(width = '300px', targets = 2), # criterion
            list(width = '1000px', targets = 3), # content
            list(visible = FALSE, targets = 7)   # student/group (hidden here)
          )
        ),
        editable = list(target = "cell", disable = list(columns = c(0, 2:10))),
        class = "display"
      ), 'score_comment', backgroundColor = "lightgrey"
    )
  })

  #output$last_edited <- renderText({
  #  #str(input$correction_table_criterion_cell_edit)
  #  if (!is.null(input$correction_table_criterion_cell_edit)) {
  #    if (input$correction_table_criterion_cell_edit$col == 2) {
  #      score_comment <- input$correction_table_criterion_cell_edit$value
  #      score <- trimws(sub("^([^ ]+).*$", "\\1", score_comment))
  #      comment <- trimws(sub("^[^ ]+(.*)$", "\\1", score_comment))
  #      num_score <- try(as.numeric(score), silent = TRUE)
  #      if (inherits(num_score, "try-error")) {
  #        "ERROR: score must be a numeric value or NA"
  #      } else {# TODO: check that score is between 0 and item_score_max
  #        paste(
  #          context$user_logins[
  #            order[input$correction_table_criterion_cell_edit$row]],
  #          "score:", score, "comment:", comment)
  #      }
  #    }
  #  }
  #})
  #TODO: the same for last_edited_grid vs correction_table_grid
  #TODO: add shinyFeedback item, something like
  #half <- reactive({
  #  even <- input$orrectionTable_cell_edit %% 2 == 0
  #  shinyFeedback::feedbackWarning("n", !even, "Please select an even number")
  #  input$orrectionTable_cell_edit / 2
  #})
  # Or use this instead:
  #showNotification("message", type = "message")
  #showNotification("message", type = "warning")
  #showNotification("message", type = "error")

  #myProxy = DT::dataTableProxy('correction_table_criterion')

  observeEvent(input$correction_table_criterion_cell_edit, {
    # validate(need(!is.null(input$correction_table_criterion_cell_edit), ''))
    #print(input$correction_table_criterion_cell_edit$row)
    #print(input$correction_table_criterion_cell_edit$col)
    #print(input$correction_Table_criterion_cell_edit$value)
    if (input$correction_table_criterion_cell_edit$col == 1) {
      # This is a score, possibly followed by a comment
      # Get the corr_file
      corr_file <- context$corr_files[
        order[input$correction_table_criterion_cell_edit$row]]
      # Read it
      corrs <- read(corr_file)
      # Make sure columns score, evaluator and comment are the right type
      corrs$score <- as.numeric(corrs$score)
      corrs$evaluator <- as.character(corrs$evaluator)
      corrs$comment <- as.character(corrs$comment)
      # Get position in the file
      pos <- (1:nrow(corrs))[corrs$criterion == input$item]
      max <- corrs$max[pos]
      if (length(pos) != 1) {
        msg <- paste0("ERROR: criterion ", input$item, " not found in ",
          corr_file)
        message(msg)
        showNotification(msg, type = "error")
        #DT::selectCells(myProxy, selected = NULL)

      } else {# pos found in corrs
        score_comment <- input$correction_table_criterion_cell_edit$value
        # Decompose into score and comment
        score <- trimws(sub("^([^ ]+).*$", "\\1", score_comment))
        if (is.null(score) || !length(score) || score == "")
          score <- "NA"
        comment <- trimws(sub("^[^ ]+(.*)$", "\\1", score_comment))
        if (is.null(comment) || !length(comment))
          comment <- ""

        is_ok <- TRUE
        # Special case for score being "NA"
        if (score == "NA") {
          num_score <- as.numeric(NA)
        } else {
          num_score <- try(suppressWarnings(as.numeric(score)), silent = TRUE)
          if (is.na(num_score) || inherits(num_score, "try-error"))
            is_ok <- FALSE
        }

        if (is_ok) {
          # Check score is not > max (note: since we allow negative scores, they
          # can be lower that zero and it is not an error)
          # Exception: if the item is for a BONUS, max is zero, and we allow for
          # positive values
          if (!is.na(num_score) && max > 0 && num_score > max) {
            msg <- paste0("ERROR: attempting to put a score ", num_score,
              " higher than the max score (", max, ")")
            message(msg)
            showNotification(msg, type = "error")
            #DT::selectCells(myProxy, selected = NULL)

          } else {# Everything is fine, record this entry
            corrs[pos, "score"] <- num_score
            corrs[pos, "comment"] <- comment
            corrs[pos, "evaluator"] <- input$evaluator
            # Write it
            data.io::write$csv(corrs, corr_file)
          }
        } else {# !is_ok
          msg <- paste0("ERROR: score '", score,
            "' not convertible into a numeric value")
          message(msg)
          showNotification(msg, type = "error")
          #DT::selectCells(myProxy, selected = NULL)
        }
      }
    }
  })

  # TODO: refactor to avoid duplicated code
  observeEvent(input$correction_table_grid_cell_edit, {
    # validate(need(!is.null(input$correction_table_grid_cell_edit), ''))
    #print(input$correction_table_grid_cell_edit$row)
    #print(input$correction_table_grid_cell_edit$col)
    #print(input$correction_Table_grid_cell_edit$value)
    if (input$correction_table_grid_cell_edit$col == 1) {
      # This is a score, possibly followed by a comment
      # Get the corr_file
      corr_pos <-
        (1:length(context$repos_names))[context$repos_names == input$grid]
      if (length(corr_pos) != 1) {
        msg <- paste0("ERROR: grid ", input$grid, " not found")
        message(msg)
        showNotification(msg, type = "error")
        #DT::selectCells(myProxy, selected = NULL)
      } else {
        corr_file <- context$corr_files[corr_pos]
        # Read it
        corrs <- read(corr_file)
        # Make sure columns score, evaluator and comment are the right type
        corrs$score <- as.numeric(corrs$score)
        corrs$evaluator <- as.character(corrs$evaluator)
        corrs$comment <- as.character(corrs$comment)
        # Get position in the file (criterion)... assume no changes in the order
        # of the criteria in the evaluation grid!
        pos <- input$correction_table_grid_cell_edit$row
        max <- corrs$max[pos]
        score_comment <- input$correction_table_grid_cell_edit$value
        # Decompose into score and comment
        score <- trimws(sub("^([^ ]+).*$", "\\1", score_comment))
        if (is.null(score) || !length(score) || score == "")
          score <- "NA"
        comment <- trimws(sub("^[^ ]+(.*)$", "\\1", score_comment))
        if (is.null(comment) || !length(comment))
          comment <- ""

        is_ok <- TRUE
        # Special case for score being "NA"
        if (score == "NA") {
          num_score <- as.numeric(NA)
        } else {
          num_score <- try(suppressWarnings(as.numeric(score)), silent = TRUE)
          if (is.na(num_score) || inherits(num_score, "try-error"))
            is_ok <- FALSE
        }

        if (is_ok) {
          # Check score is not > max (note: since we allow negative scores, they
          # can be lower that zero and it is not an error)
          # Exception: if the item is for a BONUS, max is zero, and we allow for
          # positive values
          if (!is.na(num_score) && max > 0 && num_score > max) {
            msg <- paste0("ERROR: attempting to put a score ", num_score,
              " higher than the max score (", max, ")")
            message(msg)
            showNotification(msg, type = "error")
            #DT::selectCells(myProxy, selected = NULL)

          } else {# Everything is fine, record this entry
            corrs[pos, "score"] <- num_score
            corrs[pos, "comment"] <- comment
            corrs[pos, "evaluator"] <- input$evaluator
            # Write it
            data.io::write$csv(corrs, corr_file)
          }
        } else {# !is_ok
          msg <- paste0("ERROR: score '", score,
            "' not convertible into a numeric value")
          message(msg)
          showNotification(msg, type = "error")
          #DT::selectCells(myProxy, selected = NULL)
        }
      }
    }
  })

  ## Reset last selected value
  #observeEvent(input$reset, {
  #  DT::selectCells(myProxy, selected = NULL)
  #})
}

shinyApp(shinyUI, shinyServer)
# or ... to deal with return value:
#my_app <- shinyApp(shinyUI, shinyServer)
#res <- runApp(my_app)
#if (res == 1)
#  message("Application concelled by the user")
