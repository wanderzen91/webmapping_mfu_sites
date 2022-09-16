



server <- function(input, output, session){
  
  observe({
    updateSelectInput(session, "sites_cen",
                      choices = c("", as.character(sort(unique(filtered_sites_cen_depts()$nom_site))))
                      )})
  
  observe(session$setCurrentTheme(
    if (isTRUE(input$light_mode)) light else dark
  ))
  
  filtered_sites_cen_depts <- reactive({
    sites_mfu[sites_mfu$departement == input$depts,]
  })
  
 
  filtered_sites_cen <- reactive({
    sites_mfu[sites_mfu$nom_site == input$sites_cen,]
  })
  
  
  filtered_dept <- reactive({
    depts_na[depts_na$nom == input$depts,]
  })
  
  subset1 <-reactive({parts_habitats_1 %>%
      dplyr::filter(nom_site ==  input$sites_cen)
  })
  
  subset2 <-reactive({types_mfu_1 %>%
      dplyr::filter(nom_site ==  input$sites_cen)
  })
  
  
  
  subset3 <-reactive({parts_habitats_depts %>%
      dplyr::filter(nom_dept ==  input$dept1)
  })
  
  subset4 <-reactive({parts_mfu_depts %>%
      dplyr::filter(nom_dept == input$dept1)
  })
  
  
  
  subset5 <-reactive({parts_habitats_depts %>%
      dplyr::filter(nom_dept ==  input$dept2)
  })
  
  subset6 <-reactive({parts_mfu_depts %>%
      dplyr::filter(nom_dept == input$dept2)
  })
  
  
  nb_sites_dept_reactive1 <- reactive({nb_sites_dept %>%
      dplyr::filter(nom_dept ==  input$dept1)
  })
  
  nb_sites_dept_reactive2 <- reactive({nb_sites_dept %>%
      dplyr::filter(nom_dept ==  input$dept2)
  })
  
  
  surf_geree_dept_reactive1 <- reactive({surf_geree_dept %>%
      dplyr::filter(nom_dept ==  input$dept1)
  })
  
  surf_geree_dept_reactive2 <- reactive({surf_geree_dept %>%
      dplyr::filter(nom_dept ==  input$dept2)
  })
  
  
  bbox_sites_cen <- reactive({st_bbox(filtered_sites_cen()) %>% 
    as.vector() })
  
  bbox_dept <- reactive({st_bbox(filtered_dept()) %>% 
      as.vector() })
  
  output$map <- renderLeaflet({
    
    leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
      setView(lng = 0, lat= 44.00, zoom = 7) %>%
      addTiles(group = "Fond de carte standard (OSM)") %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Imagerie satelitte (ESRI)") %>%
      addPolygons(data = depts_na,
                  color = "darkgrey",
                  fillColor = "transparent",
                  weight = 0.9,
                  opacity = 1,
                  group = "Départements") %>%
      addPolygons(data= sites_mfu,
                  color = "grey",
                  fillColor = "transparent",
                  weight = 2,
                  opacity = 1)  %>%
      addLayersControl(
        overlayGroups = c("Départements"),
        baseGroups = c("Fond de carte standard (OSM)", "Imagerie satelitte (ESRI)"),
        options = layersControlOptions(collapsed = TRUE),
        position = "bottomleft")
  })
  
  
  
  observeEvent(input$sites_cen,{
     leafletProxy('map') %>%
      addPolylines(weight = 3, opacity = 1, color="yellow", data=filtered_sites_cen(), layerId = "sites_CEN_NA")  %>%
        flyToBounds(bbox_sites_cen()[1], bbox_sites_cen()[2], bbox_sites_cen()[3], bbox_sites_cen()[4])

    req(input$sites_cen)
    ignoreInit = TRUE
  })
  
  
  observeEvent(input$depts,{
    leafletProxy('map') %>%
      addPolylines(weight = 3, opacity = 1, color="white", data=filtered_dept(), layerId = "departements_NA")  %>%
      flyToBounds(bbox_dept()[1], bbox_dept()[2], bbox_dept()[3], bbox_dept()[4])
   
     req(input$depts)
    
  })
  
  
  # observeEvent(input$map_shape_click, {
  #   shinyjs::toggle(id= "dataviz")
  #     })
  
  

  
  output$type_habitats <- renderpier({
    advanced.pie <- subset1() %>%
      pier() %>%
      pie.size(inner=70, outer=100, width = 600, height = 225) %>%
      pie.header(text="Habitats", font='Impact', location='pie-center') %>%
      pie.subtitle(text='Sites CEN-NA') %>%
      # pie.footer(text="Diversité de la maîtrise foncière et d'usage",
      #            location = 'bottom-left') %>%
      pie.tooltips()
    
    
  })
  
  
  output$types_mfu <- renderpier({
    advanced.pie <- subset2() %>%
      pier() %>%
      pie.size(inner=70, outer=100, width = 600, height = 225) %>%
      pie.header(text="MFU", font='Impact', location='pie-center') %>%
      pie.subtitle(text='CEN-NA') %>%
      # pie.footer(text="Diversité de la maîtrise foncière et d'usage",
      #            location = 'bottom-left') %>%
      pie.tooltips()
    
    
  })
  
  
  output$plot1 <-  renderHighchart ({
      highchart() %>%
        hc_add_series(type="bar",name = "Surface totale ", data = round(subset4()$surf_ha, digits = 2) )%>%
        hc_xAxis(categories = subset4()$type_mfu)%>%
      hc_yAxis_multiples(
          list(lineWidth = 0,
               title = list(text = "Surface (ha)")),
          list(showLastLabel = FALSE, opposite = TRUE,
               title = list(text = ""))
        ) %>%
      hc_colors("orange")
    })
  
  
  
  
  output$plot2 <-  renderHighchart ({
    highchart() %>%
      hc_add_series(type="bar",name = "Surface totale ", data = round(subset3()$surf_ha, digits = 2) )%>%
      hc_xAxis(categories = subset3()$libhabitat)%>%
      hc_yAxis_multiples(
        list(lineWidth = 0,
             title = list(text = "Surface (ha)")),
        list(showLastLabel = FALSE, opposite = TRUE,
             title = list(text = ""))
      ) %>%
      hc_colors("red")
  })
  
  
  
  
  
  
  output$plot3 <-  renderHighchart ({
    highchart() %>%
      hc_add_series(type="bar",name = "Surface totale ", data = round(subset6()$surf_ha, digits = 2) )%>%
      hc_xAxis(categories = subset6()$type_mfu)%>%
      hc_yAxis_multiples(
        list(lineWidth = 0,
             title = list(text = "Surface (ha)")),
        list(showLastLabel = FALSE, opposite = TRUE,
             title = list(text = ""))
      ) %>%
      hc_colors("lightgreen")
  })
  
  
  
  
  
  output$plot4 <-  renderHighchart ({
    highchart() %>%
      hc_add_series(type="bar",name = "Surface totale ", data = round(subset5()$surf_ha, digits = 2) )%>%
      hc_xAxis(categories = subset5()$libhabitat) %>%
      hc_yAxis_multiples(
        list(lineWidth = 0,
             title = list(text = "Surface (ha)")),
        list(showLastLabel = FALSE, opposite = TRUE,
             title = list(text = ""))
      ) %>%
      hc_colors("darkgreen")
  })
  
  output$box1 <- shinydashboard::renderValueBox({
    shinydashboard::valueBox(
      nb_sites_dept_reactive1()$codesite,
      "Nombre de sites gérés",
      icon = icon("tree"),
      color = "light-blue"
    )
  })
  
  output$box2 <- shinydashboard::renderValueBox({
    shinydashboard::valueBox(
      round(surf_geree_dept_reactive1()$surf_ha, digits=1),
      "Surface totale gérée",
      icon = icon("chart-area"),
      color = "olive"
    )
  })
  
  
  output$box3 <- shinydashboard::renderValueBox({
    shinydashboard::valueBox(
      nb_sites_dept_reactive2()$codesite,
      "Nombre de sites gérés",
      icon = icon("tree"),
      color = "light-blue"
    )
  })
  
  output$box4 <- shinydashboard::renderValueBox({
    shinydashboard::valueBox(
      round(surf_geree_dept_reactive2()$surf_ha, digits=1),
      "Surface totale gérée",
      icon = icon("chart-area"),
      color = "olive"
    )
  })
  
  
  # output$vbox2 <- renderValueBox({
  #   valueBox(
  #     "Surface gérée",
  #     14213,
  #     icon = icon("credit-card")
  #   )
  # })
  
  
  
  
}
