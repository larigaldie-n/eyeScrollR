library(stringr)

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

# Define UI ----
ui <- fluidPage(
  titlePanel("Fixed area bundle creator"),

  tags$div(
    id = 'bundles',
    tags$div(
      id = 'bundle_1',
      fluidRow(column(12, h2("Bundle 1"))),
      tags$div(id = 'bundle_1_fixed_areas',
               tags$div(
                 id = "fixed_1_1",
                 fluidRow(column(12, h4("Fixed Area 1"))),
                 fluidRow(
                   column(3,
                          numericInput(
                            "num_1_1_1", label = h5("Top left X"), value = 0
                          )),
                   column(3,
                          numericInput(
                            "num_1_1_2", label = h5("Top left Y"), value = 0
                          )),
                   column(3,
                          numericInput(
                            "num_1_1_3",
                            label = h5("Bottom left X"),
                            value = 0
                          )),
                   column(3,
                          numericInput(
                            "num_1_1_4",
                            label = h5("Bottom left Y"),
                            value = 0
                          ))
                 )

               )),


      fluidRow(column(
        3,
        actionButton("add_fixed_1_1", label = "Add fixed area in Bundle")
      ),
      column(
        3,
        actionButton("remove_fixed_1_1", label = "Remove fixed area in Bundle")
      )),
      fluidRow(column(12,
                      h2("Rule for Bundle 1")),
               column(
                 12,
                 selectInput(
                   "select_1",
                   label = h3("Type of rule"),
                   choices = list(
                     "Always True" = 1,
                     "True when Scrolled >= value" = 2,
                     "True when Scrolled < value" = 3,
                     "Custom" = 4
                   ),
                   selected = 1
                 )
               ))

    )
  ),

  fluidRow(column(
    6,
    actionButton("add_bundle", label = "Add new Bundle")),
    column(
      6,
      actionButton("remove_bundle", label = "Remove Bundle"))
  ),
  hr(),
  wellPanel(verbatimTextOutput("code")),
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
  values <- reactiveValues(n_fixed_areas = c(1), n_bundles = 1)

  observeEvent(input$js.add_fixed, {
    uid = str_split(input$js.add_fixed, "_")
    add_or_remove = uid[[1]][1]
    fixed_or_bundle = uid[[1]][2]
    if (fixed_or_bundle == 'fixed')
    {
      n_bundle = strtoi(uid[[1]][3])
      n_fixed = strtoi(uid[[1]][4])
      if (add_or_remove == 'add')
      {
        values$n_fixed_areas[n_bundle] <- values$n_fixed_areas[n_bundle] + 1
        insertUI(
          selector = paste0('#bundle_', n_bundle, "_fixed_areas"),
          ## wrap element in a div with id for ease of removal
          ui = tags$div(
            id = paste0("fixed_", n_bundle, "_", values$n_fixed_areas[n_bundle]),
            fluidRow(column(12, h4(
              paste0("Fixed Area ", values$n_fixed_areas[n_bundle])
            ))),
            fluidRow(
              column(3,
                     numericInput(
                       paste0("num_", n_bundle, "_", values$n_fixed_areas[n_bundle], "_1"),
                       label = h5("Top left X"),
                       value = 0
                     )),
              column(3,
                     numericInput(
                       paste0("num_", n_bundle, "_", values$n_fixed_areas[n_bundle], "_2"),
                       label = h5("Top left Y"),
                       value = 0
                     )),
              column(3,
                     numericInput(
                       paste0("num_", n_bundle, "_", values$n_fixed_areas[n_bundle], "_3"),
                       label = h5("Bottom left X"),
                       value = 0
                     )),
              column(3,
                     numericInput(
                       paste0("num_", n_bundle, "_", values$n_fixed_areas[n_bundle], "_4"),
                       label = h5("Bottom left Y"),
                       value = 0
                     ))
            )
          )
        )

      }
      else if (add_or_remove == 'remove')
      {
        if (values$n_fixed_areas[n_bundle] > 1)
        {
          removeUI(paste0("#fixed_", n_bundle, "_", values$n_fixed_areas[n_bundle]))
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
        values$n_fixed_areas[values$n_bundles] <- 1
        insertUI(
          selector = '#bundles',
          ## wrap element in a div with id for ease of removal
          ui = tags$div(
            id = paste0('bundle_', values$n_bundles),
            fluidRow(column(12, h2(paste0("Bundle ", values$n_bundles)))),
            tags$div(id = paste0('bundle_', values$n_bundles,'_fixed_areas'),
                     tags$div(
                       id = paste0("fixed_", values$n_bundles,"_1"),
                       fluidRow(column(12, h4("Fixed Area 1"))),
                       fluidRow(
                         column(3,
                                numericInput(
                                  paste0("num_", values$n_bundles,"_1_1"),
                                  label = h5("Top left X"),
                                  value = 0
                                )),
                         column(3,
                                numericInput(
                                  paste0("num_", values$n_bundles,"_1_2"),
                                  label = h5("Top left Y"),
                                  value = 0
                                )),
                         column(3,
                                numericInput(
                                  paste0("num_", values$n_bundles,"_1_3"),
                                  label = h5("Bottom left X"),
                                  value = 0
                                )),
                         column(3,
                                numericInput(
                                  paste0("num_", values$n_bundles,"_1_4"),
                                  label = h5("Bottom left Y"),
                                  value = 0
                                ))
                       )

                     )),


            fluidRow(column(
              3,
              actionButton(paste0("add_fixed_", values$n_bundles,"_1"), label = "Add fixed area in Bundle")
            ),
            column(
              3,
              actionButton(paste0("remove_fixed_", values$n_bundles,"_1"), label = "Remove fixed area in Bundle")
            )),
            fluidRow(column(12,
                            h2(
                              paste0("Rule for Bundle ", values$n_bundles)
                            )),
                     column(
                       12,
                       selectInput(
                         paste0("select_", values$n_bundles),
                         label = h3("Type of rule"),
                         choices = list(
                           "Always True" = 1,
                           "True when Scrolled >= value" = 2,
                           "True when Scrolled < value" = 3,
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
        if (values$n_bundles > 1)
        {
          removeUI(paste0('#bundle_', values$n_bundles))
          values$n_fixed_areas[values$n_bundles] <- 0
          values$n_bundles <- values$n_bundles - 1
        }
      }

    }

  })

  output$code <- renderText({
    text <- ""
    for (bundle in 1:values$n_bundles)
    {
      text <- paste0(text, "### Bundle ", bundle, "\n")
      for (fixed_area in 1:values$n_fixed_areas[bundle])
      {
        text <- paste0(text, "# Fixed area ", fixed_area, "\n")
        text <- paste0(paste0(text, "bundle_", bundle, "_fixed_area_", fixed_area,"_top_x = ", input[[paste0("num_", bundle,"_", fixed_area,"_1")]], "\n"))
        text <- paste0(paste0(text, "bundle_", bundle, "_fixed_area_", fixed_area,"_top_y = ", input[[paste0("num_", bundle,"_", fixed_area,"_2")]], "\n"))
        text <- paste0(paste0(text, "bundle_", bundle, "_fixed_area_", fixed_area,"_bottom_x = ", input[[paste0("num_", bundle,"_", fixed_area,"_3")]], "\n"))
        text <- paste0(paste0(text, "bundle_", bundle, "_fixed_area_", fixed_area,"_bottom_y = ", input[[paste0("num_", bundle,"_", fixed_area,"_4")]], "\n"))
      }
      text <- paste0(text, "\n")

      text <- paste0(text, "## Rule for bundle ", bundle, "\n")
      if (!is.null(input[[paste0("select_", bundle)]]))
      {
        text <- paste0(text,
                       switch(strtoi(input[[paste0("select_", bundle)]]),
                              {print_true()},
                              {print_before_scroll()},
                              {"Function Scroll Up"},
                              {"Custom Function"}
                       ),
                       "\n\n")
      }
      }

    return(text)
  })

}

# Run the app ----
shinyApp(ui = ui, server = server)
