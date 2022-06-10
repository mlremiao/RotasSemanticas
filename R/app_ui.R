library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(dplyr)
library(leaflet)
library(mapview)

################################################################################

tabela <- readRDS("./R/tabela.rds")

microrregioes_ini <- readRDS("./R/microrregioes_ini.rds")

microrregioes_fim <- readRDS("./R/microrregioes_fim.rds")

################################################################################


ui <- dashboardPage(

  title = HTML("Rotas Semânticas"),

  dashboardHeader(

    title = HTML('<center><img src="Logotipo_PRF2.png", height = "28"></center>'),

    titleWidth = 200

  ),
  dashboardSidebar(

    sidebarMenu(

      ################################################################################

      menuItem("Home", tabName = "tab_home", icon = icon("home")),

      ################################################################################


      menuItem("Tipo de Apreesão", icon = icon("filter"), startExpanded = F,

               fluidPage(

                 pickerInput(
                   inputId = "uf",
                   label = "Unidades da Federação",
                   choices = "",
                   options = list(
                     `actions-box` = TRUE),
                   choicesOpt = list(
                     style = rep(("color: black; background: lightgrey; font-weight: bold;"),28)
                   ),
                   multiple = FALSE,
                   selected = ""
                 ),

                 pickerInput(
                   inputId = "apreensao",
                   label = "Tipo de Apreensão",
                   choices = "",
                   options = list(
                     `actions-box` = TRUE),
                   choicesOpt = list(
                     style = rep(("color: black; background: lightgrey; font-weight: bold;"),10)
                   ),
                   multiple = FALSE,
                   selected = ""
                 ),

                 menuItem("", tabName = "tab_rotas", icon = icon("arrow-right")),


               )
      ),

      ################################################################################

      menuItem("Origem", icon = icon("filter"), startExpanded = F,

               fluidPage(

                 pickerInput(
                   inputId = "uf1",
                   label = "Unidades da Federação",
                   choices = "",
                   options = list(
                     `actions-box` = TRUE),
                   choicesOpt = list(
                     style = rep(("color: black; background: lightgrey; font-weight: bold;"),28)
                   ),
                   multiple = FALSE,
                   selected = ""
                 ),

                 pickerInput(
                   inputId = "microrregiao1",
                   label = "Microrregião de Origem",
                   choices = "",
                   options = list(
                     `actions-box` = TRUE),
                   choicesOpt = list(
                     style = rep(("color: black; background: lightgrey; font-weight: bold;"),398)
                   ),
                   multiple = FALSE,
                   selected = ""

                 ),

                 menuItem("", tabName = "tab_origem", icon = icon("arrow-right")),

               )
      ),


      ################################################################################

      menuItem("Destino", icon = icon("filter"), startExpanded = F,

               fluidPage(

                 pickerInput(
                   inputId = "uf2",
                   label = "Unidades da Federação",
                   choices = "",
                   options = list(
                     `actions-box` = TRUE),
                   choicesOpt = list(
                     style = rep(("color: black; background: lightgrey; font-weight: bold;"),28)
                   ),
                   multiple = FALSE,
                   selected = ""
                 ),

                 pickerInput(
                   inputId = "microrregiao2",
                   label = "Microrregião de Destino",
                   choices = "",
                   options = list(
                     `actions-box` = TRUE),
                   choicesOpt = list(
                     style = rep(("color: black; background: lightgrey; font-weight: bold;"),398)
                   ),
                   multiple = FALSE,
                   selected = ""

                 ),

                 menuItem("", tabName = "tab_destino", icon = icon("arrow-right")),

               )
      )



      ################################################################################
    )
  ),


  ################################################################################

  dashboardBody(
    tabItems(

      ################################################################################

      tabItem(

        "tab_home",

        ################################################################################

        fluidPage(

          HTML('<center><img src="Prf_brasao_novo.jpg" width="500"></center>'),

          imageOutput("imagem"),

        ),


        ################################################################################


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

      ')
        )
        )
        ################################################################################
      ),


      ################################################################################

      tabItem(

        "tab_rotas",

        ################################################################################

        tabsetPanel(
          id = "tabs1",

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
        ################################################################################
      ),

      ################################################################################

      tabItem(
        "tab_origem",

        ################################################################################

        tabsetPanel(
          id = "tabs2",

          tabPanel(
            title = "Rotas por Origem",
            fluidRow(
              column(12,
                     mapviewOutput("mapa1", height = "1000px")

              )
            )
          )
        )
        ################################################################################
      ),

      ################################################################################

      tabItem(
        "tab_destino",

        ################################################################################

        tabsetPanel(
          id = "tabs",

          tabPanel(
            title = "Rotas por Destino",

            fluidRow(
              column(12,
                     mapviewOutput("mapa2", height = "1000px")

              )
            )
          )
        )
      )

      ################################################################################

    )
  )
)


################################################################################
