
#' Run the shiny multiplyr app
#'
#' @return Launching of the shiny multiplyr app
#' @export
#' @import shiny
#' @import bslib
#' @importFrom colourpicker colourInput
#' @examples

run_app_multiplyr <- function(){
  
  # bslib theme definition
  theme <- bslib::bs_theme(version = 5, bootswatch="flatly")%>%
    bslib::bs_theme_update(
      base_font = bslib::font_google("Indie Flower"),
      heading_font = bslib::font_google("Indie Flower")
    )
  # colrox function definition
  colrow <- function(html){
    div(class = "col-6 col-lg-6", html)
  }
  # control bar
  button_bar <- div(
    class="row",
    colrow(
      shiny::numericInput(
        inputId = "table",
        width = "100%",
        label = "Multiplication table",
        min = 2,
        max = 100,
        value = 4)
    ),
    colrow(
      shiny::numericInput(inputId = "modulo",
                          width = "100%",
                          label = "Modulo",
                          min = 2,
                          value = 5000)
    ),
    colrow(
      shiny::numericInput(
        inputId = "vertice",
        width = "100%",
        label = "Number of vertices",
        min = 3,
        value = 6)
    ),
    colrow(shiny::numericInput(inputId = "alpha",width = "100%", label = "Alpha",value = 0.05)),
    colrow(shiny::numericInput(inputId = "curvature", width = "100%", label = "Curvature",value = 0)),
    colrow(shiny::numericInput(inputId = "angle", width = "100%", label = "Angle",value = 0)),
    colrow(shiny::numericInput(inputId = "zoom", width = "100%", label = "Zoom",value = 1)),
    colrow(colourpicker::colourInput(inputId = "color",
                                     label = div(class="pb-2","Color"),
                                     value = bslib::bs_get_variables(theme,"primary"))),
    colrow(colourpicker::colourInput(inputId = "bgcolor",
                                     label = div(class="pb-2","Background color"),
                                     value = '#FFFFFF')),
    br(),
    div(class = "d-grid gap-2 d-block",
        tags$button(class="btn btn-primary action-button",
                    type="button",
                    id = "ok",
                    'data-bs-toggle'="offcanvas",
                    'data-bs-target'="#bottom_canva",
                    'aria-controls'="paste0(bottom_canva,'label')",
                    "Draw"
        ),
    )
  )
  # Define UI 
  ui <- shiny::bootstrapPage(
    theme = theme,
    #title
    h3("Multiplication tables for my kids",class="text-center fw-bold text-primary"),
    # canva
    add_oc(
      id = "id_oc",
      header = TRUE,
      title = h4("Drawing parameters",class="p-0 m-0"),
      body = button_bar,
      position = "end",
      class_header = "bg-primary",
      class_body = "bg-white",
      class_btn = "btn-primary",
      class_oc ="border border-primary shadow",
      scroll = TRUE,
      backdrop = FALSE,
      close_btn = TRUE
      ),
    # tool bar
    # button group
    shiny::div(class="d-flex justify-content-center w-100",
               shiny::div(class="col-lg-5 col-md-6 col-12 justify-content-center",
                          div(class="btn-group w-100 p-1 px-0 bg-transparent", role="group", 'aria-label'="First group",
                              add_tooltip(
                                position = "bottom",
                                trigger = "hover",
                                text = "Drawing parameters",
                                color = "primary",
                                tag = div(class="w-25", tags$button(type="button",
                    class="btn m-1 ms-0 me-1 btn-outline-primary shadow w-100",
                    'data-bs-toggle'="offcanvas",
                    'data-bs-target'="#id_oc",
                    'aria-controls'="paste0(id_oc,'label')",
                    icon("ruler")))),
                    add_tooltip(
                      position = "bottom",
                      trigger = "hover",
                      text = "Download the plot",
                      color = "primary",
                      tag = tags$button(type="button", width = "25%", class="btn m-1 btn-outline-primary shadow",icon("copy"))),
                    add_tooltip(
                      position = "bottom",
                      trigger = "hover",
                      text = "About this App",
                      color = "primary",
                      tag = tags$button(type="button", width = "25%", class="btn m-1 btn-outline-primary shadow",icon("question"))),
                    add_tooltip(
                      position = "bottom",
                      trigger = "hover",
                      text = "Link to github project",
                      color = "primary",
                      tag = tags$button(type="button", width = "25%", class="btn m-1 me-0 btn-outline-primary outline-0 shadow",icon("github"))
    )
    
    ))),
    # Show a plot of the generated distribution
    shiny::div(class="d-flex justify-content-center",
               shiny::div(class="col-lg-5 col-md-6 col-12 justify-content-center",
                          div(class="row row-cols-1 g-1",
                              div(class="col",
                                  div(class="card p-0 shadow h-100 rounded-5 border-primary",
                                      div(class = "ratio ratio-1x1",
                                          shiny::plotOutput("distPlot",width = "100%",height = "100%")
                                      )
                                  )
                              )
                              )
               )
    )
  )
  
  # Define server logic 
  server <- function(input, output) {
    
    shiny::observeEvent(input$ok,{
      graph <- graph_line(nb_vertice = as.numeric(input$vertice),
                          modulo = as.numeric(input$modulo),
                          table = as.numeric(input$table),
                          alpha = as.numeric(input$alpha),
                          curvature = as.numeric(input$curvature),
                          angle = as.numeric(input$angle),
                          colour = input$color,
                          bgcolor = input$bgcolor,
                          zoom = as.numeric(input$zoom))
      
      output$distPlot <- shiny::renderPlot({ graph })
    })
  }
  
  
  shiny::shinyApp(ui = ui, server = server)
  
  
}