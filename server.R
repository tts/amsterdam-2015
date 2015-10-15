function(input, output, session) {
  
  
  # When School is selected, filter and draw its data to a ggivs chart.
  # If 'All' as School is selected, return all original data.
  selectedSchoolData <- reactive({
    
    if ( input$school == 'All' ) 
      return(dataForCharts)

     dataForCharts %>%
        filter(School %in% input$school) 
  })
  
  
  
  # When School is selected, populate selection with its items for drawing with NVD3
  observe(
    updateSelectizeInput(session, 
                         inputId = 'items', 
                         choices = if ( input$school == 'All' ) dataForCharts$Title  else  selectedSchoolData()$Title 
    )
  )
  
  
  
  
  # When items are selected, filter School data with them
  itemsData <- reactive({
    
     if( is.null(input$items) ){
       return(NULL)
     }

    isolate(selectedSchoolData()[selectedSchoolData()$Title %in% input$items, ])
    
    })
  

  
  # NVD3
  output$chart <- renderChart({
    
    validate(
      need(!is.null(itemsData()), "Please select some items")
    )
    
    dataC <- itemsData() 
    
    datatomelt <- dataC %>%
      mutate(id = Title10) %>%
      select(-Type, -Title, -Title10, -Journal, -Dept, -School, -Unit, -keys, -Year, -AltmetricURL)
    
    dataM <- reshape2::melt(datatomelt, id.vars = "id")
    nplot <- nPlot(value ~ id, data = dataM, 
                   group = "variable", type = "multiBarChart")
    nplot$set(dom="chart")
    return(nplot)
    
  })
  
  
  
  
 
  # GGVIS
  ggvisdata <- reactive({
    
    show_title <- function(x=NULL) {
      if(is.null(x)) return(NULL)
      key <- x["keys"][[1]]
      selectedSchoolData()$Title[key]
    } 
    
    xvar_name <- input$xc 
    yvar_name <- input$yc
    
    xc <- prop("x", as.symbol(input$xc))
    yc <- prop("y", as.symbol(input$yc))
    
    df <- selectedSchoolData()
    
    df$keys <- seq_along(df[,1])
    
    df %>%
      ggvis(x = xc, 
            y = yc, 
            key := ~keys, 
            fill = ~School, 
            opacity := 0.80,
            size.hover := 200) %>%
      layer_points() %>%
      add_axis("x", title = xvar_name) %>%
      add_axis("y", title = yvar_name, title_offset = 50) %>% 
      set_options(width = "100%", height = "500px") %>%
      add_tooltip(show_title)
  })
  ggvisdata %>% bind_shiny("gv")
  

  
  # Some ValueBoxes showing few top metrics in this School
  output$nrofitemswithmetrics <- renderValueBox({
    valueBox(
      "Items with metrics", 
      nrow(selectedSchoolData()), 
      icon = icon("calculator"),
      color = "yellow"
    )
  })
  
  output$maxaltmetrics <- renderValueBox({
    valueBox(
      "Top Altmetric score", 
      max(selectedSchoolData()$Altmetric), 
      icon = icon("spinner"),
      color = "green",
      href = selectedSchoolData()[selectedSchoolData()$Altmetric == max(selectedSchoolData()$Altmetric), "AltmetricURL"][1]
    )
  })
  
  output$maxgplus <- renderValueBox({
    valueBox(
      "Top Google+ score", 
      max(selectedSchoolData()$GPlus), 
      icon = icon("google-plus"),
      color = "teal",
      href = selectedSchoolData()[selectedSchoolData()$GPlus == max(selectedSchoolData()$GPlus), "AltmetricURL"][1]
    )
  })

  output$maxfb <- renderValueBox({
    valueBox(
      "Top Facebook score", 
      max(selectedSchoolData()$Facebook), 
      icon = icon("facebook-f"),
      color = "light-blue",
      href = selectedSchoolData()[selectedSchoolData()$Facebook == max(selectedSchoolData()$Facebook), "AltmetricURL"][1]
    )
  })
  
  output$maxwikipedia <- renderValueBox({
    valueBox(
      "Top Wikipedia score", 
      max(selectedSchoolData()$Wikipedia), 
      icon = icon("wikipedia-w"),
      color = "fuchsia",
      href = selectedSchoolData()[selectedSchoolData()$Wikipedia == max(selectedSchoolData()$Wikipedia), "AltmetricURL"][1]
    )
  })
  
  
  
  
  # Sunburst
  output$sunburst <- renderSunburst({
    sunburst(datafreq, count=TRUE)
  })
  
  
  
  # Download sunburst to a standalone HTML page
  output$downloadSun <- downloadHandler(
    filename = "sunburst.html", contentType = "text/plain", 
    content = function(file) {
      out <- sunburst(datafreq)
      htmlwidgets::saveWidget(out, file)
    })
  
  
  # Network
  output$force <- renderForceNetwork({
    forceNetwork(Links = links, Nodes = nodes,
                 Source = "Source", Target = "Target",
                 Value = "Value", NodeID = "Name",
                 Group = "Group", opacity = 0.8, zoom = T)
  })
  
  
  

  # Datatable
   output$datatable <- DT::renderDataTable({
    
    dtrows <- selectedSchoolData()
    
    totable <- dtrows %>%
      select(-keys, -Title10)

    
    for (i in 1:nrow(totable)) {
      url <- substr(totable$AltmetricURL[i], 47, nchar(totable$AltmetricURL[i]))
      doUrl <- paste0("<a href='", totable[i, c("AltmetricURL")], "'>", url, "</a>")
      totable[i, c("AltmetricURL")] <- doUrl
    } 
    totable
    }, escape = FALSE, options = list(scrollX = T)
)
  
  
   
  
  # Impactstory
  iss <- reactive({
    issstats %>%
      filter(tolower(Title) == unlist(strsplit(input$istitle, "] "))[2])
  })
  
  
  
  output$dygraph <- renderDygraph({
    
    if( nrow(iss()) == 0 ) return() 
    
    iss_spread <- iss() %>% 
      select(Action, Count, Date) %>%
      tidyr::spread(Action, Count) %>%
      lapply(., repeat.before)
  
    # Separate Date+Action combinations
    issList <- lapply(seq(from=2, to=length(iss_spread)), function(i) c(iss_spread[1],iss_spread[i]))
    
    # Make xts objects from these
    xsList <- lapply(issList, function(x) xts(data.frame(x, stringsAsFactors = F), order.by = x$Date))
    
    # Bind columns
    all.xts <- do.call("cbind", xsList)
    
    dygraph(all.xts) %>%
      dyAxis("x", drawGrid = FALSE) %>%
      dyAxis("y", label = "Count") %>%
      dyOptions(colors = RColorBrewer::brewer.pal(3, "Set2")) %>%
      dyLegend(show = "onmouseover", width = 400, showZeroValues = FALSE, hideOnMouseOut = TRUE) %>%
      dyOptions(stepPlot = TRUE,
                strokeWidth = 2)
  
  })

  
  
  # Impactstory link
  output$item <- renderUI({
    
    if ( nrow(iss()) == 0 ) return(NULL)
    
    p("Current item is: ", iss()[1, 'Title'], "see at ")
    url <- paste0("http://impactstory.org/AaltoUniversity/product/", iss()[1, 'ID'])
    t <- iss()[1, 'Title']
    a(t, class="web", href=url)
    })
  
  
}
