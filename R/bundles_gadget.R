#' @title Run a fixed areas bundle gadget
#'
#' @description Automatically generates code for fixed areas bundle and rules
#' @export
#' @importFrom stringr str_split
#' @import shiny
bundles_gadget <- function() {
  print_true <- function()
  {
    return (
      "rule_true <- function (data_line, fixed_areas_bundle, flag, scroll)
{
  return (TRUE)
}"
    )
  }

  print_before_scroll <- function ()
  {
    return (
      "rule_before_scrolling <- function (data_line, fixed_areas_bundle, flag, scroll)
{
### Change VALUE to fit your needs ###
  if (scroll < VALUE)
  {
    return (TRUE)
  }
  else
  {
    return(FALSE)
  }
}"
    )
  }

  print_after_scroll <- function ()
  {
    return (
      "rule_after_scrolling <- function (data_line, fixed_areas_bundle, flag, scroll)
{
### Change VALUE to fit your needs ###
  if (scroll >= VALUE)
  {
    return (TRUE)
  }
  else
  {
    return(FALSE)
  }
}"
    )
  }

  print_custom <- function ()
  {
    return (
      "rule_custom <- function (data_line, fixed_areas_bundle, flag, scroll)
### Change the name of the function ###
{
### Write your code here ###
}"
    )
  }

  # Define UI ----
  ui <- fluidPage(
    titlePanel("Fixed areas bundle creator"),
    tags$div(id = 'bundles'),
    fluidRow(column(
      3,
      actionButton("add_bundle", label = "Add new Bundle")
    ),
    column(
      3,
      actionButton("remove_bundle", label = "Remove Bundle")
    )),
    hr(),
    wellPanel(verbatimTextOutput("code")),
    actionButton("done_gadget", label = "Done"),
    tags$script(
      "$(document).on('click', 'button', function(e) {
 e.stopPropagation()
 if(typeof BUTTON_CLICK_COUNT == 'undefined') {
    BUTTON_CLICK_COUNT = 1;
  } else {
    BUTTON_CLICK_COUNT ++;
  }
  Shiny.onInputChange('js.add_fixed',
    e.target.id + '_' + BUTTON_CLICK_COUNT);
});"
    )
  )

  # Define server logic ----
  server <- function(input, output, session) {
    values <- reactiveValues(n_fixed_areas = c(),
                             n_bundles = 0)

    observeEvent(input$js.add_fixed, {
      uid <- str_split(input$js.add_fixed, "_")
      add_or_remove <- uid[[1]][1]
      fixed_or_bundle <- uid[[1]][2]
      if (fixed_or_bundle == 'fixed')
      {
        n_bundle <- strtoi(uid[[1]][3])
        n_fixed <- strtoi(uid[[1]][4])
        if (add_or_remove == 'add')
        {
          values$n_fixed_areas[n_bundle] <- values$n_fixed_areas[n_bundle] + 1
          insertUI(
            selector = paste0('#bundle_', n_bundle, "_fixed_areas"),
            ## wrap element in a div with id for ease of removal
            ui = tags$div(
              id = paste0(
                "fixed_",
                n_bundle,
                "_",
                values$n_fixed_areas[n_bundle]
              ),
              fluidRow(column(12, h4(
                paste0("Fixed Area ", values$n_fixed_areas[n_bundle])
              ))),
              fluidRow(column(
                12, h5("Coordinates on the screen")
              )),
              fluidRow(
                column(
                  3,
                  numericInput(
                    paste0(
                      "num_screen_",
                      n_bundle,
                      "_",
                      values$n_fixed_areas[n_bundle],
                      "_1"
                    ),
                    label = h5("Top left X"),
                    value = 0
                  )
                ),
                column(
                  3,
                  numericInput(
                    paste0(
                      "num_screen_",
                      n_bundle,
                      "_",
                      values$n_fixed_areas[n_bundle],
                      "_2"
                    ),
                    label = h5("Top left Y"),
                    value = 0
                  )
                ),
                column(
                  3,
                  numericInput(
                    paste0(
                      "num_screen_",
                      n_bundle,
                      "_",
                      values$n_fixed_areas[n_bundle],
                      "_3"
                    ),
                    label = h5("Bottom right X"),
                    value = 0
                  )
                ),
                column(
                  3,
                  numericInput(
                    paste0(
                      "num_screen_",
                      n_bundle,
                      "_",
                      values$n_fixed_areas[n_bundle],
                      "_4"
                    ),
                    label = h5("Bottom right Y"),
                    value = 0
                  )
                )
              ),
              fluidRow(column(
                12, h5("Coordinates on the image")
              )),
              fluidRow(
                column(
                  3,
                  numericInput(
                    paste0(
                      "num_image_",
                      n_bundle,
                      "_",
                      values$n_fixed_areas[n_bundle],
                      "_1"
                    ),
                    label = h5("Top left X"),
                    value = 0
                  )
                ),
                column(
                  3,
                  numericInput(
                    paste0(
                      "num_image_",
                      n_bundle,
                      "_",
                      values$n_fixed_areas[n_bundle],
                      "_2"
                    ),
                    label = h5("Top left Y"),
                    value = 0
                  )
                ),
                column(
                  3,
                  numericInput(
                    paste0(
                      "num_image_",
                      n_bundle,
                      "_",
                      values$n_fixed_areas[n_bundle],
                      "_3"
                    ),
                    label = h5("Bottom right X"),
                    value = 0
                  )
                ),
                column(
                  3,
                  numericInput(
                    paste0(
                      "num_image_",
                      n_bundle,
                      "_",
                      values$n_fixed_areas[n_bundle],
                      "_4"
                    ),
                    label = h5("Bottom right Y"),
                    value = 0
                  )
                )
              )
            )
          )
        }
        else if (add_or_remove == 'remove')
        {
          if (values$n_fixed_areas[n_bundle] > 0)
          {
            removeUI(paste0(
              "#fixed_",
              n_bundle,
              "_",
              values$n_fixed_areas[n_bundle]
            ))
            values$n_fixed_areas[n_bundle] <-
              values$n_fixed_areas[n_bundle] - 1
          }
        }
      }
      else if (fixed_or_bundle == 'bundle')
      {
        if (add_or_remove == 'add')
        {
          values$n_bundles <- values$n_bundles + 1
          values$n_fixed_areas[values$n_bundles] <- 0
          insertUI(
            selector = '#bundles',
            ## wrap element in a div with id for ease of removal
            ui = tags$div(
              id = paste0('bundle_', values$n_bundles),
              fluidRow(column(12, h2(
                paste0("Bundle ", values$n_bundles)
              ))),
              tags$div(
                id = paste0(
                  'bundle_',
                  values$n_bundles,
                  '_fixed_areas'
                ),
                tags$div(
                  id = paste0(
                    "fixed_",
                    values$n_bundles,
                    "_",
                    values$n_fixed_areas[values$n_bundles]
                  )
                )
              ),
              fluidRow(
                column(
                  3,
                  actionButton(
                    paste0("add_fixed_", values$n_bundles, "_1"),
                    label = "Add fixed area in Bundle"
                  )
                ),
                column(
                  3,
                  actionButton(
                    paste0("remove_fixed_", values$n_bundles, "_1"),
                    label = "Remove fixed area in Bundle"
                  )
                )
              ),
              fluidRow(column(12,
                              h3(
                                paste0("Rule for Bundle ", values$n_bundles)
                              )),
                       column(
                         12,
                         selectInput(
                           paste0("select_", values$n_bundles),
                           label = h4("Type of rule"),
                           choices = list(
                             "Always True" = 1,
                             "True when Scrolled < value" = 2,
                             "True when Scrolled >= value" = 3,
                             "Custom" = 4
                           ),
                           selected = 1
                         )
                       ))
            )
          )
        }
        else if (add_or_remove == 'remove')
        {
          if (values$n_bundles > 0)
          {
            removeUI(paste0('#bundle_', values$n_bundles))
            values$n_fixed_areas[values$n_bundles] <- 0
            values$n_bundles <- values$n_bundles - 1
          }
        }

      }
      else
      {
        stopApp()
      }
    })

    output$code <- renderText({
      text <- ""
      if (values$n_bundles > 0)
      {
        argument_list_bundles <- ""
        argument_list_rules <- ""
        for (bundle in 1:values$n_bundles)
        {
          argument_bundle_creation <- ""
          if (values$n_fixed_areas[bundle] > 0)
          {
            text <- paste0(text, "### Bundle ", bundle, "\n")
            for (fixed_area in 1:values$n_fixed_areas[bundle])
            {
              if (argument_bundle_creation == "")
              {
                argument_bundle_creation <-
                  paste0(
                    "bundle_",
                    bundle,
                    "_fixed_area_",
                    fixed_area,
                    "_screen, ",
                    "bundle_",
                    bundle,
                    "_fixed_area_",
                    fixed_area,
                    "_image"
                  )
              }
              else
              {
                argument_bundle_creation <-
                  paste(
                    argument_bundle_creation,
                    paste0(
                      "bundle_",
                      bundle,
                      "_fixed_area_",
                      fixed_area,
                      "_screen, ",
                      "bundle_",
                      bundle,
                      "_fixed_area_",
                      fixed_area,
                      "_image"
                    ),
                    sep = ",\n"
                  )
              }
              text <-
                paste0(text, "# Fixed area ", fixed_area, "\n")
              text <-
                paste0(
                  paste0(
                    text,
                    "bundle_",
                    bundle,
                    "_fixed_area_",
                    fixed_area,
                    "_screen <- ",
                    "c(",
                    input[[paste0("num_screen_",
                                  bundle,
                                  "_",
                                  fixed_area,
                                  "_1")]],
                    ", ",
                    input[[paste0("num_screen_",
                                  bundle,
                                  "_",
                                  fixed_area,
                                  "_2")]],
                    ", ",
                    input[[paste0("num_screen_",
                                  bundle,
                                  "_",
                                  fixed_area,
                                  "_3")]],
                    ", ",
                    input[[paste0("num_screen_",
                                  bundle,
                                  "_",
                                  fixed_area,
                                  "_4")]],
                    ")\n"
                  )
                )
              text <-
                paste0(
                  paste0(
                    text,
                    "bundle_",
                    bundle,
                    "_fixed_area_",
                    fixed_area,
                    "_image <- ",
                    "c(",
                    input[[paste0("num_image_",
                                  bundle,
                                  "_",
                                  fixed_area,
                                  "_1")]],
                    ", ",
                    input[[paste0("num_image_",
                                  bundle,
                                  "_",
                                  fixed_area,
                                  "_2")]],
                    ", ",
                    input[[paste0("num_image_",
                                  bundle,
                                  "_",
                                  fixed_area,
                                  "_3")]],
                    ", ",
                    input[[paste0("num_image_",
                                  bundle,
                                  "_",
                                  fixed_area,
                                  "_4")]],
                    ")\n"
                  )
                )
            }
            text <- paste0(text, "\n")
            text <-
              paste0(text, "## Rule for bundle ", bundle, "\n")
            if (!is.null(input[[paste0("select_", bundle)]]))
            {
              function_to_print <-
                switch(strtoi(input[[paste0("select_", bundle)]]),
                       {
                         print_true()
                       },
                       {
                         print_before_scroll()
                       },
                       {
                         print_after_scroll()
                       },
                       {
                         print_custom()
                       })
              text <- paste0(text,
                             function_to_print,
                             "\n\n")
              if (argument_list_rules == "")
              {
                argument_list_rules <-
                  str_split(function_to_print, pattern = " ")[[1]][1]
              }
              else
              {
                argument_list_rules <-
                  paste(
                    argument_list_rules,
                    str_split(function_to_print, pattern = " ")[[1]][1],
                    sep = ", "
                  )
              }

            }
            text <-
              paste0(
                text,
                "areas_bundle_",
                bundle,
                " <- ",
                "fixed_areas_bundle(",
                argument_bundle_creation,
                ")\n\n"
              )
            if (argument_list_bundles == "")
            {
              argument_list_bundles <- paste0("areas_bundle_", bundle)
            }
            else
            {
              argument_list_bundles <-
                paste(
                  argument_list_bundles,
                  paste("areas_bundle_", bundle, sep = ""),
                  sep = ", "
                )
            }
          }
        }
        text <-
          paste0(text,
                 "fixed_areas <- list(",
                 argument_list_bundles,
                 ")\n")
        text <-
          paste0(text,
                 "rules <- list(",
                 argument_list_rules,
                 ")")
      }
      return(text)
    })
  }

  # Run the app ----
  runGadget(ui, server)
}
