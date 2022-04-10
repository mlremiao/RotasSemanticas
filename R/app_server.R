#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(dplyr)
library(leaflet)
library(mapview)

app_server <- function(input, output, session) {

  tabela <- readRDS("./R/tabela.rds")

  observe({
    ufs <- tabela$uf_rodovia %>%
      unique() %>%
      sort()
    updatePickerInput(
      session,
      inputId = "uf",
      choices = c("", ufs)
    )
  })

  observe({
    if (isTruthy(input$uf)) {
      tipo.apreensao <- tabela %>%
        filter(uf_rodovia %in% input$uf) %>%
        pull(apreensao_tipo) %>%
        unique() %>%
        sort()
    } else {
      tipo.apreensao <- ""
    }
    updatePickerInput(
      session,
      inputId = "apreensao",
      choices = c("", tipo.apreensao)
    )
  })



  output$menu <- renderMenu({
    sidebarMenu(
      menuItem("My Home", tabName = "home", icon = icon("home")),
      menuItem("Rotas por UF/Apreesão/Veículo", icon = icon("fas fa-filter")),

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
      )


      #tags$footer(tags$img(src = "FRIDAY.gif", height = 130), align = "center")

    )
  })

  ################################################################################################

  output$boxAutomovel <- renderValueBox({
    req(input$uf)
    req(input$apreensao)
    name_var_qtds <- paste("quantidades", input$apreensao, input$uf, sep = "_")
    name_file_quantidades <- paste(paste("quantidades", input$apreensao,input$uf, sep = "_"), "rds", sep = ".")
    quantidades <- readRDS(paste0("./R/Geometrias/Quantidades/", name_file_quantidades))
    valueBox(quantidades[1], "Automovel", color="navy")
  })

  output$boxCaminhao <- renderValueBox({
    req(input$uf)
    req(input$apreensao)
    name_var_qtds <- paste("quantidades", input$apreensao, input$uf, sep = "_")
    name_file_quantidades <- paste(paste("quantidades", input$apreensao,input$uf, sep = "_"), "rds", sep = ".")
    quantidades <- readRDS(paste0("./R/Geometrias/Quantidades/", name_file_quantidades))
    valueBox(quantidades[2]+quantidades[5], "Caminhão/Caminhão Trator", color="blue")
  })

  output$boxCamioneta <- renderValueBox({
    req(input$uf)
    req(input$apreensao)
    name_var_qtds <- paste("quantidades", input$apreensao, input$uf, sep = "_")
    name_file_quantidades <- paste(paste("quantidades", input$apreensao,input$uf, sep = "_"), "rds", sep = ".")
    quantidades <- readRDS(paste0("./R/Geometrias/Quantidades/", name_file_quantidades))
    valueBox(quantidades[3], "Camioneta", color="light-blue")
  })

  output$boxUtilitario <- renderValueBox({
    req(input$uf)
    req(input$apreensao)
    name_var_qtds <- paste("quantidades", input$apreensao, input$uf, sep = "_")
    name_file_quantidades <- paste(paste("quantidades", input$apreensao,input$uf, sep = "_"), "rds", sep = ".")
    quantidades <- readRDS(paste0("./R/Geometrias/Quantidades/", name_file_quantidades))
    valueBox(quantidades[4]+quantidades[6], "Caminhonete/Utilitário", color="aqua")
  })

  ################################################################################################

  output$mapa <- renderLeaflet({
    req(input$uf)
    req(input$apreensao)

    name_var_qtds <- paste("quantidades", input$apreensao, input$uf, sep = "_")
    name_file_quantidades <- paste(paste("quantidades", input$apreensao,input$uf, sep = "_"), "rds", sep = ".")
    quantidades <- readRDS(paste0("./R/Geometrias/Quantidades/", name_file_quantidades))


    name_file_bops_loc <- paste(paste("bops_loc", input$apreensao, input$uf, sep = "_"), "rds", sep = ".")
    if (file.exists(paste0("./R/Geometrias/BOPs/", name_file_bops_loc))) {
      bops_loc <- readRDS(paste0("./R/Geometrias/BOPs/", name_file_bops_loc))
    }

    name_file_pontos_ab <- paste(paste("pontos_ab", input$apreensao, input$uf, sep = "_"), "rds", sep = ".")
    if (file.exists(paste0("./R/Geometrias/PontosAB/", name_file_pontos_ab))) {
      pontos_ab <- readRDS(paste0("./R/Geometrias/PontosAB/", name_file_pontos_ab))
    }

    name_file_micro_ini <- paste(paste("Micro_ini", input$apreensao, input$uf, sep = "_"), "rds", sep = ".")
    if (file.exists(paste0("./R/Geometrias/Microregioes_inicio/", name_file_micro_ini))) {
      micro_ini <- readRDS(paste0("./R/Geometrias/Microregioes_inicio/", name_file_micro_ini))
    }

    name_file_micro_fim <- paste(paste("Micro_fim", input$apreensao, input$uf, sep = "_"), "rds", sep = ".")
    if (file.exists(paste0("./R/Geometrias/Microregioes_final/", name_file_micro_fim))) {
      micro_fim <- readRDS(paste0("./R/Geometrias/Microregioes_final/", name_file_micro_fim))
    }

    name_file_rotas <- paste(paste("Rotas", input$apreensao, input$uf, "AUTOMOVEL", sep = "_"), "rds", sep = ".")
    if (file.exists(paste0("./R/Geometrias/Rotas/", name_file_rotas))) {
      rotas_automovel <- readRDS(paste0("./R/Geometrias/Rotas/", name_file_rotas))
    }

    name_file_rotas <- paste(paste("Rotas", input$apreensao, input$uf, "CAMINHAO", sep = "_"), "rds", sep = ".")
    if (file.exists(paste0("./R/Geometrias/Rotas/", name_file_rotas))) {
      rotas_caminhao <- readRDS(paste0("./R/Geometrias/Rotas/", name_file_rotas))
    }

    name_file_rotas <- paste(paste("Rotas", input$apreensao, input$uf, "CAMINHAO TRATOR", sep = "_"), "rds", sep = ".")
    if (file.exists(paste0("./R/Geometrias/Rotas/", name_file_rotas))) {
      rotas_caminhao_trator <- readRDS(paste0("./R/Geometrias/Rotas/", name_file_rotas))
    }

    name_file_rotas <- paste(paste("Rotas", input$apreensao, input$uf, "CAMINHONETE", sep = "_"), "rds", sep = ".")
    if (file.exists(paste0("./R/Geometrias/Rotas/", name_file_rotas))) {
      rotas_caminhonete <- readRDS(paste0("./R/Geometrias/Rotas/", name_file_rotas))
    }

    name_file_rotas <- paste(paste("Rotas", input$apreensao, input$uf, "CAMIONETA", sep = "_"), "rds", sep = ".")
    if (file.exists(paste0("./R/Geometrias/Rotas/", name_file_rotas))) {
      rotas_camioneta <- readRDS(paste0("./R/Geometrias/Rotas/", name_file_rotas))
    }

    name_file_rotas <- paste(paste("Rotas", input$apreensao, input$uf, "UTILITARIO", sep = "_"), "rds", sep = ".")
    if (file.exists(paste0("./R/Geometrias/Rotas/", name_file_rotas))) {
      rotas_utilitario <- readRDS(paste0("./R/Geometrias/Rotas/", name_file_rotas))
    }


    #    if (input$uf == "" || input$apreensao == "") {

    #      geometrias <- mapview("./R/Geometrias/Brasil/brasil.rds", col.regions = "transparent", map.types = c("OpenStreetMap", "Esri.WorldImagery"))

    #    } else {

    if (sum(quantidades)!=0) {

      geometrias <-
        mapview(bops_loc, col.regions = "red", layer.name = paste("BOPs", input$apreensao, input$uf, sep = " "), cex = "apreensao_quantidade", map.types = c("OpenStreetMap", "Esri.WorldImagery")) +
        mapview(pontos_ab, col.regions = "gray", layer.name = paste("Ponto AB - BOPs", input$apreensao, input$uf, sep = " "), cex = 3, map.types = c("OpenStreetMap", "Esri.WorldImagery")) +
        mapview(micro_ini, layer.name = paste("Regiões de partida", input$apreensao, input$uf, sep = " "), col.regions = "green", map.types = c("OpenStreetMap", "Esri.WorldImagery")) +
        mapview(micro_fim, layer.name = paste("Regiões de chegada", input$apreensao, input$uf, sep = " "), col.regions = "yellow", map.types = c("OpenStreetMap", "Esri.WorldImagery")) +

        if (file.exists(paste0("./R/Geometrias/Rotas/", paste(paste("Rotas", input$apreensao, input$uf, "AUTOMOVEL", sep = "_"), "rds", sep = ".")))) {
          mapview(rotas_automovel, layer.name = paste("Rotas", input$apreensao, input$uf, " - AUTOMOVEL", sep = " "), color = "navy", map.types = c("OpenStreetMap", "Esri.WorldImagery"))
        } +

        if (file.exists(paste0("./R/Geometrias/Rotas/", paste(paste("Rotas", input$apreensao, input$uf, "CAMIONETA", sep = "_"), "rds", sep = ".")))) {
          mapview(rotas_camioneta, layer.name = paste("Rotas", input$apreensao, input$uf, " - CAMIONETA", sep = " "), color = "navy", map.types = c("OpenStreetMap", "Esri.WorldImagery"))
        } +

        if (file.exists(paste0("./R/Geometrias/Rotas/", paste(paste("Rotas", input$apreensao, input$uf, "CAMINHAO TRATOR", sep = "_"), "rds", sep = ".")))) {
          mapview(rotas_caminhao_trator, layer.name = paste("Rotas", input$apreensao, input$uf, " - CAMINHAO TRATOR", sep = " "), color = "navy", map.types = c("OpenStreetMap", "Esri.WorldImagery"))
        } +

        if (file.exists(paste0("./R/Geometrias/Rotas/", paste(paste("Rotas", input$apreensao, input$uf, "CAMINHONETE", sep = "_"), "rds", sep = ".")))) {
          mapview(rotas_caminhonete, layer.name = paste("Rotas", input$apreensao, input$uf, " - CAMINHONETE", sep = " "), color = "navy", map.types = c("OpenStreetMap", "Esri.WorldImagery"))
        } +

        if (file.exists(paste0("./R/Geometrias/Rotas/", paste(paste("Rotas", input$apreensao, input$uf, "CAMINHAO", sep = "_"), "rds", sep = ".")))) {
          mapview(rotas_caminhao, layer.name = paste("Rotas", input$apreensao, input$uf, " - CAMINHAO", sep = " "), color = "navy", map.types = c("OpenStreetMap", "Esri.WorldImagery"))
        } +

        if (file.exists(paste0("./R/Geometrias/Rotas/", paste(paste("Rotas", input$apreensao, input$uf, "UTILITARIO", sep = "_"), "rds", sep = ".")))) {
          mapview(rotas_utilitario, layer.name = paste("Rotas", input$apreensao, input$uf, " - UTILITARIO", sep = " "), color = "navy", map.types = c("OpenStreetMap", "Esri.WorldImagery"))
        }

    } else {

      name_uf <- paste(input$uf, "rds", sep = ".")
      geometrias <- mapview(readRDS(paste0("./R/Geometrias/Brasil/", name_uf)), layer.name = "Sem Ocorrências", col.regions = "transparent", map.types = c("OpenStreetMap", "Esri.WorldImagery"))

    }
    #    }


    geometrias@map
  })


  ################################################################################################


  output$tabela <- DT::renderDataTable({
    req(input$uf)
    req(input$apreensao)

    DT::datatable(subset(tabela, uf_rodovia == input$uf & apreensao_tipo == input$apreensao),
                  escape = FALSE,
                  rownames = FALSE,
                  filter = "top",
                  style = "bootstrap",
                  #autoHideNavigation = TRUE,
                  selection = "none",
                  extensions = "Responsive",
                  options = list(searchHighlight = TRUE),
                  colnames = c("UF do BOP", "Placa", "Tipo de Apreensão", "Veículo", "Local de Passagem", "Data/Hora", "Latitude", "Longitude", "Microrregião de Passagem", "UF de Passagem")
    )
  })

}
