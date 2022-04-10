#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(dplyr)
library(leaflet)
library(mapview)

app_ui <- dashboardPage(


      dashboardHeader(


        title = span("Rotas Semânicas"),

        titleWidth = 300

      ),
      dashboardSidebar(sidebarMenuOutput("menu")


      ),
      dashboardBody(

        tags$head(tags$style(HTML('
                                /* logo */
                                .skin-blue .main-header .logo {
                                background-color: #120a8f;
                                }

                                /* logo when hovered */
                                .skin-blue .main-header .logo:hover {
                                background-color:  #120a8f;
                                }

                                /* navbar (rest of the header) */
                                .skin-blue .main-header .navbar {
                                background-color: #120a8f;
                                }

                                /* main sidebar */
                                .skin-blue .main-sidebar {
                                background-color:  #d3d3d3;
                                }

                                /* active selected tab in the sidebarmenu */
                                .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                                background-color:  #d3d3d3;
                                }

                                /* other links in the sidebarmenu */
                                .skin-blue .main-sidebar .sidebar .sidebar-menu a{
                                background-color:  #d3d3d3;
                                color: #000000;
                                }

                                /* other links in the sidebarmenu when hovered */
                                .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
                                background-color:  #FFFFFF;
                                }
                                /* toggle button when hovered  */
                                .skin-blue .main-header .navbar .sidebar-toggle:hover{
                                background-color: #FFFFFF;
                                }

                                /* body */
                                .content-wrapper, .right-side {
                                background-color:  #FFFFFF;
                                }

                                '))),

        tabsetPanel(
          id = "tabs",

          tabPanel(
            title = "Rotas",

            fluidRow(
              valueBoxOutput("boxAutomovel", width = 3),
              valueBoxOutput("boxCaminhao", width = 3),
              valueBoxOutput("boxCamioneta", width = 3),
              valueBoxOutput("boxUtilitario", width = 3)
            ),

            fluidRow(
              column(12,
                     mapviewOutput("mapa", height = "1000px")

              )
            )
          ),

          tabPanel(
            title = "Tabela",
            fluidRow(
              column(12,
                     DT::dataTableOutput("tabela")
              )
            )
          )
        )
      )
    )

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "RotasSemanticas"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
