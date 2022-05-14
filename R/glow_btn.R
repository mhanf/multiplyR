
colrow <- function(html) {
  div(class = "col-6 col-lg-6", html)
}


glow_btn <- function(label,default_color) {
  
  btn_fin <- tagList(
    # outer glow checkbox
    colrow(
      checkboxInput(
        inputId = tolower(gsub(" ", "_", label)), 
        label = label
        )
      ),
    # outer glow color
    colrow(
      colourpicker::colourInput(
        inputId = tolower(paste0(gsub(" ", "_", label),"_color")),
        label = div(class = "pb-2", paste(label, "color")),
        value = default_color,#bslib::bs_get_variables(theme, "success")
      )
    ),
    # outer glow sigma
    colrow(
      shiny::numericInput(
        inputId = tolower(paste0(gsub(" ", "_", label),"_sigma")),
        width = "100%",
        label = paste(label, "sigma"),
        min = 0,
        value = 2
      )
    ),
    # outer glow expand
    colrow(
      shiny::numericInput(
        inputId = tolower(paste0(gsub(" ", "_", label),"_expand")),
        width = "100%",
        label = paste(label, "expand"),
        min = 0,
        value = 5
      )
    )
  )
  return(btn_fin)
}
