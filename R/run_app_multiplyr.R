
#' Run the shiny multiplyr app **work in progress**
#'
#' @return Launching of the shiny multiplyr app
#' @export
#' @import shiny
#' @import bslib
#' @import htmltools
#' @import waiter
#' @importFrom colourpicker colourInput
#' @examples

run_app_multiplyr <- function() {
  # bslib theme definition
  theme <- bslib::bs_theme(version = 5, bootswatch = "flatly") %>%
    bslib::bs_theme_update(
      base_font = bslib::font_google("Indie Flower"),
      heading_font = bslib::font_google("Indie Flower")
    )
  # colrox function definition
  colrow <- function(html) {
    div(class = "col-6 col-lg-6", html)
  }
  # control bar
  button_bar <- div(
    class = "row",
    colrow(
      shiny::numericInput(
        inputId = "table",
        width = "100%",
        label = "Multiplication table",
        min = 2,
        max = 100,
        value = 4
      )
    ),
    colrow(
      shiny::numericInput(
        inputId = "modulo",
        width = "100%",
        label = "Modulo",
        min = 2,
        value = 5000
      )
    ),
    colrow(
      shiny::numericInput(
        inputId = "vertice",
        width = "100%",
        label = "Number of vertices",
        min = 3,
        value = 6
      )
    ),
    colrow(
      shiny::numericInput(
        inputId = "alpha",
        width = "100%",
        label = "Alpha",
        value = 0.05
      )
    ),
    colrow(
      shiny::numericInput(
        inputId = "curvature",
        width = "100%",
        label = "Curvature",
        value = 0
      )
    ),
    colrow(
      shiny::numericInput(
        inputId = "angle",
        width = "100%",
        label = "Angle",
        value = 0
      )
    ),
    colrow(
      shiny::numericInput(
        inputId = "zoom",
        width = "100%",
        label = "Zoom",
        value = 1
      )
    ),
    colrow(
      colourpicker::colourInput(
        inputId = "color",
        label = div(class = "pb-2", "Color"),
        value = bslib::bs_get_variables(theme, "primary")
      )
    ),
    colrow(
      colourpicker::colourInput(
        inputId = "bgcolor",
        label = div(class = "pb-2", "Background color"),
        value = '#FFFFFF'
      )
    ),
    br(),
    div(
      class = "d-grid gap-2 d-block",
      button_app(
        id = "ok",
        color = "primary",
        outline = TRUE,
        oc = TRUE,
        id_oc = "id_oc",
        text = tags$b("Draw"),
        icon = icon("drafting-compass")
      )
    )
  )
  # Define UI
  ui <- shiny::bootstrapPage(
    theme = theme,
    waiter::useWaiter(),
    #title
    h3("Multiplication tables for my kids", class = "text-center fw-bold text-primary"),
    # canva
    add_oc(
      id = "id_oc",
      header = TRUE,
      title = h4("Drawing parameters", class = "p-0 m-0"),
      body = button_bar,
      position = "end",
      class_header = "bg-primary",
      class_body = "bg-white",
      class_btn = "btn-primary",
      class_oc = "border border-primary shadow",
      scroll = TRUE,
      backdrop = FALSE,
      close_btn = TRUE
    ),
    # tool bar
    # button group
    shiny::div(
      class = "d-flex justify-content-center w-100",
      shiny::div(
        class = "col-lg-5 col-md-6 col-12 justify-content-center",
        div(
          class = "w-100 p-0 mb-2 bg-transparent d-flex justify-content-center",
          # first button : trigger off canvas parameters
          add_tooltip(
            position = "bottom",
            trigger = "hover",
            text = "Drawing parameters",
            color = "primary",
            tag = div(
              class = "w-25 me-1 ",
              button_app(
                id = "btn1",
                color = "primary",
                add_class = "w-100",
                outline = TRUE,
                oc = TRUE,
                id_oc = "id_oc",
                icon = icon("ruler")
              )
            )
          ),
          # second button : download plot
          add_tooltip(
            position = "bottom",
            trigger = "hover",
            text = "Download the plot",
            color = "primary",
            tag = div(
              class = "w-25 me-1",
              dwld_button_app("btn2", 
                              icon = icon("copy"), 
                              label ="", 
                              class = "btn-outline-primary w-100 shadow")
            )
          ),
          # third button : about this site
          add_tooltip(
            position = "bottom",
            trigger = "hover",
            text = "About this App",
            color = "primary",
            tag = div(
              class = "w-25 me-1",
              button_app(
                id = "btn3",
                color = "primary",
                outline = TRUE,
                add_class = "w-100",
                icon = icon("question")
              )
            )
          ),
          # Fourth button : link to github
          add_tooltip(
            position = "bottom",
            trigger = "hover",
            text = "Link to github project",
            color = "primary",
            tag = div(
              class = "w-25",
              button_app(
                id = "btn4",
                href = "https://github.com/mhanf/multiplyR",
                color = "primary",
                add_class = "w-100",
                outline = TRUE,
                icon = icon("github")
              )
            )
          )
          
        )
      )
    ),
    # Show a plot of the generated distribution
    shiny::div(
      class = "d-flex justify-content-center",
      shiny::div(class = "col-lg-5 col-md-6 col-12 justify-content-center",
                 div(class = "row row-cols-1 g-1",
                     div(
                       class = "col",
                       div(class = "card p-0 shadow h-100 rounded-5 border-primary",
                           div(
                             class = "ratio ratio-1x1",
                             shiny::plotOutput("plot",width="100%",height = "100%")
                           )
                       )
                     )))
    )
  )
  
  # Define server logic
  server <- function(input, output) {
    # waiter initiation
    w <- waiter::Waiter$new(id = "plot",
                            html = tagList(
                              bs5_spinner(style = "spin",color = "primary"),
                              h4("Be patient...",class="text-primary")),
                            color = "white"
    )
    # reactive value initiation
    reactlist <- reactiveValues(graph = NULL)
    # observe event parameter validation
    observeEvent(input$ok,{
      w$show()
      reactlist$graph <- graph_line(
        nb_vertice = as.numeric(input$vertice),
        modulo = as.numeric(input$modulo),
        table = as.numeric(input$table),
        alpha = as.numeric(input$alpha),
        curvature = as.numeric(input$curvature),
        angle = as.numeric(input$angle),
        colour = input$color,
        bgcolor = input$bgcolor,
        zoom = as.numeric(input$zoom)
      )
    })   
    # plot update
    output$plot <- shiny::renderPlot({ 
      reactlist$graph 
    })
    # download button
    output$btn2 <- shiny::downloadHandler(
      filename = function(){paste("input$plot3",'.png',sep='')},
      content = function(file){ ggsave(file,plot = reactlist$graph) }
    )
  }
  # launch
  shiny::shinyApp(ui = ui, server = server)
}