library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(dplyr)
library(leaflet)
library(mapview)

################################################################################

server <- function(input, output, session) {

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

  ################################################################################

  observe({
    ufs1 <- microrregioes_ini$UF.Pass %>%
      unique() %>%
      sort()
    updatePickerInput(
      session,
      inputId = "uf1",
      choices = c("", ufs1)
    )
  })

  observe({
    if (isTruthy(input$uf1)) {
      micro.ini <- microrregioes_ini %>%
        filter(UF.Pass %in% input$uf1) %>%
        pull(Microregiao.Pass) %>%
        unique() %>%
        sort()
    } else {
      micro.ini <- ""
    }
    updatePickerInput(
      session,
      inputId = "microrregiao1",
      choices = c("", micro.ini)
    )
  })

  ################################################################################

  observe({
    ufs2 <- microrregioes_fim$UF.Pass %>%
      unique() %>%
      sort()
    updatePickerInput(
      session,
      inputId = "uf2",
      choices = c("", ufs2)
    )
  })

  observe({
    if (isTruthy(input$uf2)) {
      micro.fim <- microrregioes_fim %>%
        filter(UF.Pass %in% input$uf2) %>%
        pull(Microregiao.Pass) %>%
        unique() %>%
        sort()
    } else {
      micro.fim <- ""
    }
    updatePickerInput(
      session,
      inputId = "microrregiao2",
      choices = c("", micro.fim)
    )
  })

  ################################################################################

  output$menu <- renderMenu({
    req(input$uf)
    req(input$uf1)
    req(input$uf2)
    req(input$apreensao)
    req(input$microrregiao1)
    req(input$microrregiao2)

  })
  ################################################################################
  #  tab_rotas
  ################################################################################

  output$boxAutomovel <- renderValueBox({
    req(input$uf)
    req(input$apreensao)
    name_var_qtds <- paste("quantidades", input$apreensao, input$uf, sep = "_")
    name_file_quantidades <- paste(paste("quantidades", input$apreensao,input$uf, sep = "_"), "rds", sep = ".")
    quantidades <- readRDS(paste0("./Geometrias/Quantidades/", name_file_quantidades))
    valueBox(quantidades[1], "Automovel", color="navy")
  })

  output$boxCaminhao <- renderValueBox({
    req(input$uf)
    req(input$apreensao)
    name_var_qtds <- paste("quantidades", input$apreensao, input$uf, sep = "_")
    name_file_quantidades <- paste(paste("quantidades", input$apreensao,input$uf, sep = "_"), "rds", sep = ".")
    quantidades <- readRDS(paste0("./Geometrias/Quantidades/", name_file_quantidades))
    valueBox(quantidades[2]+quantidades[5], "Caminhão/Caminhão Trator", color="blue")
  })

  output$boxCamioneta <- renderValueBox({
    req(input$uf)
    req(input$apreensao)
    name_var_qtds <- paste("quantidades", input$apreensao, input$uf, sep = "_")
    name_file_quantidades <- paste(paste("quantidades", input$apreensao,input$uf, sep = "_"), "rds", sep = ".")
    quantidades <- readRDS(paste0("./Geometrias/Quantidades/", name_file_quantidades))
    valueBox(quantidades[3], "Camioneta", color="light-blue")
  })

  output$boxUtilitario <- renderValueBox({
    req(input$uf)
    req(input$apreensao)
    name_var_qtds <- paste("quantidades", input$apreensao, input$uf, sep = "_")
    name_file_quantidades <- paste(paste("quantidades", input$apreensao,input$uf, sep = "_"), "rds", sep = ".")
    quantidades <- readRDS(paste0("./Geometrias/Quantidades/", name_file_quantidades))
    valueBox(quantidades[4]+quantidades[6], "Caminhonete/Utilitário", color="aqua")
  })

  ##############################################################################

  output$mapa <- renderLeaflet({
    req(input$uf)
    req(input$apreensao)

    name_var_qtds <- paste("quantidades", input$apreensao, input$uf, sep = "_")
    name_file_quantidades <- paste(paste("quantidades", input$apreensao,input$uf, sep = "_"), "rds", sep = ".")
    quantidades <- readRDS(paste0("./Geometrias/Quantidades/", name_file_quantidades))


    name_file_bops_loc <- paste(paste("bops_loc", input$apreensao, input$uf, sep = "_"), "rds", sep = ".")
    if (file.exists(paste0("./Geometrias/BOPs/", name_file_bops_loc))) {
      bops_loc <- readRDS(paste0("./Geometrias/BOPs/", name_file_bops_loc))
    }

    name_file_pontos_ab <- paste(paste("pontos_ab", input$apreensao, input$uf, sep = "_"), "rds", sep = ".")
    if (file.exists(paste0("./Geometrias/PontosAB/", name_file_pontos_ab))) {
      pontos_ab <- readRDS(paste0("./Geometrias/PontosAB/", name_file_pontos_ab))
    }

    name_file_micro_ini <- paste(paste("Micro_ini", input$apreensao, input$uf, sep = "_"), "rds", sep = ".")
    if (file.exists(paste0("./Geometrias/Microregioes_inicio/", name_file_micro_ini))) {
      micro_ini0 <- readRDS(paste0("./Geometrias/Microregioes_inicio/", name_file_micro_ini))
    }

    name_file_micro_fim <- paste(paste("Micro_fim", input$apreensao, input$uf, sep = "_"), "rds", sep = ".")
    if (file.exists(paste0("./Geometrias/Microregioes_final/", name_file_micro_fim))) {
      micro_fim <- readRDS(paste0("./Geometrias/Microregioes_final/", name_file_micro_fim))
    }

    name_file_rotas <- paste(paste("Rotas", input$apreensao, input$uf, "AUTOMOVEL", sep = "_"), "rds", sep = ".")
    if (file.exists(paste0("./Geometrias/Rotas/", name_file_rotas))) {
      rotas_automovel <- readRDS(paste0("./Geometrias/Rotas/", name_file_rotas))
    }

    name_file_rotas <- paste(paste("Rotas", input$apreensao, input$uf, "CAMINHAO", sep = "_"), "rds", sep = ".")
    if (file.exists(paste0("./Geometrias/Rotas/", name_file_rotas))) {
      rotas_caminhao <- readRDS(paste0("./Geometrias/Rotas/", name_file_rotas))
    }

    name_file_rotas <- paste(paste("Rotas", input$apreensao, input$uf, "CAMINHAO TRATOR", sep = "_"), "rds", sep = ".")
    if (file.exists(paste0("./Geometrias/Rotas/", name_file_rotas))) {
      rotas_caminhao_trator <- readRDS(paste0("./Geometrias/Rotas/", name_file_rotas))
    }

    name_file_rotas <- paste(paste("Rotas", input$apreensao, input$uf, "CAMINHONETE", sep = "_"), "rds", sep = ".")
    if (file.exists(paste0("./Geometrias/Rotas/", name_file_rotas))) {
      rotas_caminhonete <- readRDS(paste0("./Geometrias/Rotas/", name_file_rotas))
    }

    name_file_rotas <- paste(paste("Rotas", input$apreensao, input$uf, "CAMIONETA", sep = "_"), "rds", sep = ".")
    if (file.exists(paste0("./Geometrias/Rotas/", name_file_rotas))) {
      rotas_camioneta <- readRDS(paste0("./Geometrias/Rotas/", name_file_rotas))
    }

    name_file_rotas <- paste(paste("Rotas", input$apreensao, input$uf, "UTILITARIO", sep = "_"), "rds", sep = ".")
    if (file.exists(paste0("./Geometrias/Rotas/", name_file_rotas))) {
      rotas_utilitario <- readRDS(paste0("./Geometrias/Rotas/", name_file_rotas))
    }


    if (sum(quantidades)!=0) {

      geometrias1 <-
        mapview(bops_loc, col.regions = "red", layer.name = paste("BOPs", input$apreensao, input$uf, sep = " "), cex = "apreensao_quantidade", map.types = c("OpenStreetMap", "Esri.WorldImagery")) +
        mapview(pontos_ab, col.regions = "gray", layer.name = paste("Ponto AB - BOPs", input$apreensao, input$uf, sep = " "), cex = 3, map.types = c("OpenStreetMap", "Esri.WorldImagery")) +
        mapview(micro_ini0, layer.name = paste("Regiões de partida", input$apreensao, input$uf, sep = " "), col.regions = "green", map.types = c("OpenStreetMap", "Esri.WorldImagery")) +
        mapview(micro_fim, layer.name = paste("Regiões de chegada", input$apreensao, input$uf, sep = " "), col.regions = "yellow", map.types = c("OpenStreetMap", "Esri.WorldImagery")) +

        if (file.exists(paste0("./Geometrias/Rotas/", paste(paste("Rotas", input$apreensao, input$uf, "AUTOMOVEL", sep = "_"), "rds", sep = ".")))) {
          mapview(rotas_automovel, layer.name = paste("Rotas", input$apreensao, input$uf, " - AUTOMOVEL", sep = " "), color = "navy", map.types = c("OpenStreetMap", "Esri.WorldImagery"))
        } +

        if (file.exists(paste0("./Geometrias/Rotas/", paste(paste("Rotas", input$apreensao, input$uf, "CAMIONETA", sep = "_"), "rds", sep = ".")))) {
          mapview(rotas_camioneta, layer.name = paste("Rotas", input$apreensao, input$uf, " - CAMIONETA", sep = " "), color = "navy", map.types = c("OpenStreetMap", "Esri.WorldImagery"))
        } +

        if (file.exists(paste0("./Geometrias/Rotas/", paste(paste("Rotas", input$apreensao, input$uf, "CAMINHAO TRATOR", sep = "_"), "rds", sep = ".")))) {
          mapview(rotas_caminhao_trator, layer.name = paste("Rotas", input$apreensao, input$uf, " - CAMINHAO TRATOR", sep = " "), color = "navy", map.types = c("OpenStreetMap", "Esri.WorldImagery"))
        } +

        if (file.exists(paste0("./Geometrias/Rotas/", paste(paste("Rotas", input$apreensao, input$uf, "CAMINHONETE", sep = "_"), "rds", sep = ".")))) {
          mapview(rotas_caminhonete, layer.name = paste("Rotas", input$apreensao, input$uf, " - CAMINHONETE", sep = " "), color = "navy", map.types = c("OpenStreetMap", "Esri.WorldImagery"))
        } +

        if (file.exists(paste0("./Geometrias/Rotas/", paste(paste("Rotas", input$apreensao, input$uf, "CAMINHAO", sep = "_"), "rds", sep = ".")))) {
          mapview(rotas_caminhao, layer.name = paste("Rotas", input$apreensao, input$uf, " - CAMINHAO", sep = " "), color = "navy", map.types = c("OpenStreetMap", "Esri.WorldImagery"))
        } +

        if (file.exists(paste0("./Geometrias/Rotas/", paste(paste("Rotas", input$apreensao, input$uf, "UTILITARIO", sep = "_"), "rds", sep = ".")))) {
          mapview(rotas_utilitario, layer.name = paste("Rotas", input$apreensao, input$uf, " - UTILITARIO", sep = " "), color = "navy", map.types = c("OpenStreetMap", "Esri.WorldImagery"))
        }

    }

    geometrias1@map
  })


  ################################################################################
  #  tab_origem
  ################################################################################

  output$mapa1 <- renderLeaflet({
    req(input$uf1)
    req(input$microrregiao1)

    name_file_micro_ini <- paste(paste("Micro_ini", input$microrregiao1, input$uf1, sep = "_"), "rds", sep = ".")
    if (file.exists(paste0("./Geometrias/Microrregiao_de_origem/", name_file_micro_ini))) {
      micro_ini <- readRDS(paste0("./Geometrias/Microrregiao_de_origem/", name_file_micro_ini))
    }

    name_file_rotas <- paste(paste("Rotas", "Micro_Ini", input$microrregiao1, input$uf1, "Maconha", sep = "_"), "rds", sep = ".")
    if (file.exists(paste0("./Geometrias/Rotas_por_origem/", name_file_rotas))) {
      rotas_maconha <- readRDS(paste0("./Geometrias/Rotas_por_origem/", name_file_rotas))
    }

    name_file_rotas <- paste(paste("Rotas", "Micro_Ini", input$microrregiao1, input$uf1, "Cocaina", sep = "_"), "rds", sep = ".")
    if (file.exists(paste0("./Geometrias/Rotas_por_origem/", name_file_rotas))) {
      rotas_cocaina <- readRDS(paste0("./Geometrias/Rotas_por_origem/", name_file_rotas))
    }

    name_file_rotas <- paste(paste("Rotas", "Micro_Ini", input$microrregiao1, input$uf1, "Cigarros", sep = "_"), "rds", sep = ".")
    if (file.exists(paste0("./Geometrias/Rotas_por_origem/", name_file_rotas))) {
      rotas_cigarros <- readRDS(paste0("./Geometrias/Rotas_por_origem/", name_file_rotas))
    }

    name_file_rotas <- paste(paste("Rotas", "Micro_Ini", input$microrregiao1, input$uf1, "Crack", sep = "_"), "rds", sep = ".")
    if (file.exists(paste0("./Geometrias/Rotas_por_origem/", name_file_rotas))) {
      rotas_crack <- readRDS(paste0("./Geometrias/Rotas_por_origem/", name_file_rotas))
    }

    name_file_rotas <- paste(paste("Rotas", "Micro_Ini", input$microrregiao1, input$uf1, "Real", sep = "_"), "rds", sep = ".")
    if (file.exists(paste0("./Geometrias/Rotas_por_origem/", name_file_rotas))) {
      rotas_real <- readRDS(paste0("./Geometrias/Rotas_por_origem/", name_file_rotas))
    }

    name_file_rotas <- paste(paste("Rotas", "Micro_Ini", input$microrregiao1, input$uf1, "Haxixe", sep = "_"), "rds", sep = ".")
    if (file.exists(paste0("./Geometrias/Rotas_por_origem/", name_file_rotas))) {
      rotas_haxixe <- readRDS(paste0("./Geometrias/Rotas_por_origem/", name_file_rotas))
    }

    name_file_rotas <- paste(paste("Rotas", "Micro_Ini", input$microrregiao1, input$uf1, "Dolar", sep = "_"), "rds", sep = ".")
    if (file.exists(paste0("./Geometrias/Rotas_por_origem/", name_file_rotas))) {
      rotas_dolar <- readRDS(paste0("./Geometrias/Rotas_por_origem/", name_file_rotas))
    }

    name_file_rotas <- paste(paste("Rotas", "Micro_Ini", input$microrregiao1, input$uf1, "Skunk", sep = "_"), "rds", sep = ".")
    if (file.exists(paste0("./Geometrias/Rotas_por_origem/", name_file_rotas))) {
      rotas_skunk <- readRDS(paste0("./Geometrias/Rotas_por_origem/", name_file_rotas))
    }

    name_file_rotas <- paste(paste("Rotas", "Micro_Ini", input$microrregiao1, input$uf1, "Ecstasy", sep = "_"), "rds", sep = ".")
    if (file.exists(paste0("./Geometrias/Rotas_por_origem/", name_file_rotas))) {
      rotas_ecstasy <- readRDS(paste0("./Geometrias/Rotas_por_origem/", name_file_rotas))
    }


    geometrias2 <-
      mapview(micro_ini, layer.name = paste("Região de partida", input$microrregiao1, input$uf1, sep = " "), col.regions = "green", map.types = c("OpenStreetMap", "Esri.WorldImagery")) +

      if (file.exists(paste0("./Geometrias/Rotas_por_origem/", paste(paste("Rotas", "Micro_Ini", input$microrregiao1, input$uf1, "Maconha", sep = "_"), "rds", sep = ".")))) {
        mapview(rotas_maconha, layer.name = paste("Rotas", "- Maconha", sep = " "), color = "navy", map.types = c("OpenStreetMap", "Esri.WorldImagery"))
      } +

      if (file.exists(paste0("./Geometrias/Rotas_por_origem/", paste(paste("Rotas", "Micro_Ini", input$microrregiao1, input$uf1, "Cocaina", sep = "_"), "rds", sep = ".")))) {
        mapview(rotas_cocaina, layer.name = paste("Rotas", "- Cocaina", sep = " "), color = "navy", map.types = c("OpenStreetMap", "Esri.WorldImagery"))
      } +

      if (file.exists(paste0("./Geometrias/Rotas_por_origem/", paste(paste("Rotas", "Micro_Ini", input$microrregiao1, input$uf1, "Cigarros", sep = "_"), "rds", sep = ".")))) {
        mapview(rotas_cigarros, layer.name = paste("Rotas", "- Cigarros", sep = " "), color = "navy", map.types = c("OpenStreetMap", "Esri.WorldImagery"))
      } +

      if (file.exists(paste0("./Geometrias/Rotas_por_origem/", paste(paste("Rotas", "Micro_Ini", input$microrregiao1, input$uf1, "Crack", sep = "_"), "rds", sep = ".")))) {
        mapview(rotas_crack, layer.name = paste("Rotas", "- Crack", sep = " "), color = "navy", map.types = c("OpenStreetMap", "Esri.WorldImagery"))
      } +

      if (file.exists(paste0("./Geometrias/Rotas_por_origem/", paste(paste("Rotas", "Micro_Ini", input$microrregiao1, input$uf1, "Real", sep = "_"), "rds", sep = ".")))) {
        mapview(rotas_real, layer.name = paste("Rotas", "- Real", sep = " "), color = "navy", map.types = c("OpenStreetMap", "Esri.WorldImagery"))
      } +

      if (file.exists(paste0("./Geometrias/Rotas_por_origem/", paste(paste("Rotas", "Micro_Ini", input$microrregiao1, input$uf1, "Haxixe", sep = "_"), "rds", sep = ".")))) {
        mapview(rotas_haxixe, layer.name = paste("Rotas", "- Haxixe", sep = " "), color = "navy", map.types = c("OpenStreetMap", "Esri.WorldImagery"))
      } +

      if (file.exists(paste0("./Geometrias/Rotas_por_origem/", paste(paste("Rotas", "Micro_Ini", input$microrregiao1, input$uf1, "Dolar", sep = "_"), "rds", sep = ".")))) {
        mapview(rotas_dolar, layer.name = paste("Rotas", "- Dolar", sep = " "), color = "navy", map.types = c("OpenStreetMap", "Esri.WorldImagery"))
      } +

      if (file.exists(paste0("./Geometrias/Rotas_por_origem/", paste(paste("Rotas", "Micro_Ini", input$microrregiao1, input$uf1, "Skunk", sep = "_"), "rds", sep = ".")))) {
        mapview(rotas_skunk, layer.name = paste("Rotas", "- Skunk", sep = " "), color = "navy", map.types = c("OpenStreetMap", "Esri.WorldImagery"))
      } +

      if (file.exists(paste0("./Geometrias/Rotas_por_origem/", paste(paste("Rotas", "Micro_Ini", input$microrregiao1, input$uf1, "Ecstasy", sep = "_"), "rds", sep = ".")))) {
        mapview(rotas_ecstasy, layer.name = paste("Rotas", "- Ecstasy", sep = " "), color = "navy", map.types = c("OpenStreetMap", "Esri.WorldImagery"))
      }

    geometrias2@map
  })


  ################################################################################
  #  tab_destino
  ################################################################################


  output$mapa2 <- renderLeaflet({
    req(input$uf2)
    req(input$microrregiao2)

    name_file_micro_fim <- paste(paste("Micro_Fim", input$microrregiao2, input$uf2, sep = "_"), "rds", sep = ".")
    if (file.exists(paste0("./Geometrias/Microrregiao_de_destino/", name_file_micro_fim))) {
      micro_fim <- readRDS(paste0("./Geometrias/Microrregiao_de_destino/", name_file_micro_fim))
    }

    name_file_rotas <- paste(paste("Rotas", "Micro_Fim", input$microrregiao2, input$uf2, "Maconha", sep = "_"), "rds", sep = ".")
    if (file.exists(paste0("./Geometrias/Rotas_por_destino/", name_file_rotas))) {
      rotas_maconha <- readRDS(paste0("./Geometrias/Rotas_por_destino/", name_file_rotas))
    }

    name_file_rotas <- paste(paste("Rotas", "Micro_Fim", input$microrregiao2, input$uf2, "Cocaina", sep = "_"), "rds", sep = ".")
    if (file.exists(paste0("./Geometrias/Rotas_por_destino/", name_file_rotas))) {
      rotas_cocaina <- readRDS(paste0("./Geometrias/Rotas_por_destino/", name_file_rotas))
    }

    name_file_rotas <- paste(paste("Rotas", "Micro_Fim", input$microrregiao2, input$uf2, "Cigarros", sep = "_"), "rds", sep = ".")
    if (file.exists(paste0("./Geometrias/Rotas_por_destino/", name_file_rotas))) {
      rotas_cigarros <- readRDS(paste0("./Geometrias/Rotas_por_destino/", name_file_rotas))
    }

    name_file_rotas <- paste(paste("Rotas", "Micro_Fim", input$microrregiao2, input$uf2, "Crack", sep = "_"), "rds", sep = ".")
    if (file.exists(paste0("./Geometrias/Rotas_por_destino/", name_file_rotas))) {
      rotas_crack <- readRDS(paste0("./Geometrias/Rotas_por_destino/", name_file_rotas))
    }

    name_file_rotas <- paste(paste("Rotas", "Micro_Fim", input$microrregiao2, input$uf2, "Real", sep = "_"), "rds", sep = ".")
    if (file.exists(paste0("./Geometrias/Rotas_por_destino/", name_file_rotas))) {
      rotas_real <- readRDS(paste0("./Geometrias/Rotas_por_destino/", name_file_rotas))
    }

    name_file_rotas <- paste(paste("Rotas", "Micro_Fim", input$microrregiao2, input$uf2, "Haxixe", sep = "_"), "rds", sep = ".")
    if (file.exists(paste0("./Geometrias/Rotas_por_destino/", name_file_rotas))) {
      rotas_haxixe <- readRDS(paste0("./Geometrias/Rotas_por_destino/", name_file_rotas))
    }

    name_file_rotas <- paste(paste("Rotas", "Micro_Fim", input$microrregiao2, input$uf2, "Dolar", sep = "_"), "rds", sep = ".")
    if (file.exists(paste0("./Geometrias/Rotas_por_destino/", name_file_rotas))) {
      rotas_dolar <- readRDS(paste0("./Geometrias/Rotas_por_destino/", name_file_rotas))
    }

    name_file_rotas <- paste(paste("Rotas", "Micro_Fim", input$microrregiao2, input$uf2, "Skunk", sep = "_"), "rds", sep = ".")
    if (file.exists(paste0("./Geometrias/Rotas_por_destino/", name_file_rotas))) {
      rotas_skunk <- readRDS(paste0("./Geometrias/Rotas_por_destino/", name_file_rotas))
    }

    name_file_rotas <- paste(paste("Rotas", "Micro_Fim", input$microrregiao2, input$uf2, "Ecstasy", sep = "_"), "rds", sep = ".")
    if (file.exists(paste0("./Geometrias/Rotas_por_destino/", name_file_rotas))) {
      rotas_ecstasy <- readRDS(paste0("./Geometrias/Rotas_por_destino/", name_file_rotas))
    }


    geometrias3 <-
      mapview(micro_fim, layer.name = paste("Região de partida", input$microrregiao2, input$uf2, sep = " "), col.regions = "green", map.types = c("OpenStreetMap", "Esri.WorldImagery")) #+

    if (file.exists(paste0("./Geometrias/Rotas_por_destino/", paste(paste("Rotas", "Micro_Fim", input$microrregiao2, input$uf2, "Maconha", sep = "_"), "rds", sep = ".")))) {
      mapview(rotas_maconha, layer.name = paste("Rotas", "- Maconha", sep = " "), color = "navy", map.types = c("OpenStreetMap", "Esri.WorldImagery"))
    } +

      if (file.exists(paste0("./Geometrias/Rotas_por_destino/", paste(paste("Rotas", "Micro_Fim", input$microrregiao2, input$uf2, "Cocaina", sep = "_"), "rds", sep = ".")))) {
        mapview(rotas_cocaina, layer.name = paste("Rotas", "- Cocaina", sep = " "), color = "navy", map.types = c("OpenStreetMap", "Esri.WorldImagery"))
      } +

      if (file.exists(paste0("./Geometrias/Rotas_por_destino/", paste(paste("Rotas", "Micro_Fim", input$microrregiao2, input$uf2, "Cigarros", sep = "_"), "rds", sep = ".")))) {
        mapview(rotas_cigarros, layer.name = paste("Rotas", "- Cigarros", sep = " "), color = "navy", map.types = c("OpenStreetMap", "Esri.WorldImagery"))
      } +

      if (file.exists(paste0("./Geometrias/Rotas_por_destinom/", paste(paste("Rotas", "Micro_Fim", input$microrregiao2, input$uf2, "Crack", sep = "_"), "rds", sep = ".")))) {
        mapview(rotas_crack, layer.name = paste("Rotas", "- Crack", sep = " "), color = "navy", map.types = c("OpenStreetMap", "Esri.WorldImagery"))
      } +

      if (file.exists(paste0("./Geometrias/Rotas_por_destino/", paste(paste("Rotas", "Micro_Fim", input$microrregiao2, input$uf2, "Real", sep = "_"), "rds", sep = ".")))) {
        mapview(rotas_real, layer.name = paste("Rotas", "- Real", sep = " "), color = "navy", map.types = c("OpenStreetMap", "Esri.WorldImagery"))
      } +

      if (file.exists(paste0("./Geometrias/Rotas_por_destino/", paste(paste("Rotas", "Micro_Fim", input$microrregiao2, input$uf2, "Haxixe", sep = "_"), "rds", sep = ".")))) {
        mapview(rotas_haxixe, layer.name = paste("Rotas", "- Haxixe", sep = " "), color = "navy", map.types = c("OpenStreetMap", "Esri.WorldImagery"))
      } +

      if (file.exists(paste0("./Geometrias/Rotas_por_destino/", paste(paste("Rotas", "Micro_Fim", input$microrregiao2, input$uf2, "Dolar", sep = "_"), "rds", sep = ".")))) {
        mapview(rotas_dolar, layer.name = paste("Rotas", "- Dolar", sep = " "), color = "navy", map.types = c("OpenStreetMap", "Esri.WorldImagery"))
      } +

      if (file.exists(paste0("./Geometrias/Rotas_por_destino/", paste(paste("Rotas", "Micro_Fim", input$microrregiao2, input$uf2, "Skunk", sep = "_"), "rds", sep = ".")))) {
        mapview(rotas_skunk, layer.name = paste("Rotas", "- Skunk", sep = " "), color = "navy", map.types = c("OpenStreetMap", "Esri.WorldImagery"))
      } +

      if (file.exists(paste0("./Geometrias/Rotas_por_destino/", paste(paste("Rotas", "Micro_Fim", input$microrregiao2, input$uf2, "Ecstasy", sep = "_"), "rds", sep = ".")))) {
        mapview(rotas_ecstasy, layer.name = paste("Rotas", "- Ecstasy", sep = " "), color = "navy", map.types = c("OpenStreetMap", "Esri.WorldImagery"))

      }

    geometrias3@map
  })

  ################################################################################


  output$tabela <- DT::renderDataTable({
    req(input$uf)
    req(input$apreensao)

    DT::datatable(subset(tabela, uf_rodovia == input$uf & apreensao_tipo == input$apreensao),
                  escape = FALSE,
                  rownames = FALSE,
                  filter = "top",
                  style = "bootstrap",
                  selection = "none",
                  extensions = "Responsive",
                  options = list(searchHighlight = TRUE),
                  colnames = c("UF do BOP", "Placa", "Tipo de Apreensão", "Veículo", "Local de Passagem", "Data/Hora", "Latitude", "Longitude", "Microrregião de Passagem", "UF de Passagem")
    )
  })

}
