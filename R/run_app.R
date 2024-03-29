#' Run the Shiny Application
#'
#' @param ... arguments to pass to golem_opts.
#' See `?golem::get_golem_options` for more details.
#' @inheritParams shiny::shinyApp
#'
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options
run_app <- function(
  user = "username",
  pass = "password"
) {
  with_golem_options(
    app = shinyApp(
      ui = app_ui,
      server = app_server
    ),
    golem_opts = list(user = user, pass = pass)
  )
}
