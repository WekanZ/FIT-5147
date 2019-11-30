
tmLocate <-
  function(coor, tmSave) {
    tm <- tmSave$tm
    
    # retrieve selected rectangle
    rectInd <- which(tm$x0 < coor[1] &
                       (tm$x0 + tm$w) > coor[1] &
                       tm$y0 < coor[2] &
                       (tm$y0 + tm$h) > coor[2])
    
    return(tm[rectInd[1], ])
    
  }


shinyServer(
  function(input, output, session) {
    
    data_selected_region<-reactive({
      tree[tree$Type%in%input$region &
           tree$Year >= input$yearRange[1] & 
             tree$Year <= input$yearRange[2],]
    })
    
    output$treemap_industry <- renderPlot({ 
      
      par(mar=c(0,0,0,0), xaxs='i', yaxs='i') 
      plot(c(0,1), c(0,1),axes=F, col="white")
      vps <- baseViewports()
      
      temp=data_selected_region()
      
      .tm <<- treemap(temp, 
                      index="Industry", 
                      vSize="ROA", 
                      vColor="ROA",
                      type="value",
                      title = "",
                      palette="Blues",
                      border.col ="white",
                      position.legend="right",
                      fontsize.labels = 16,
                      title.legend="")
    })
    
    
    treemap_clicked_country <- reactiveValues(
      center = NULL,
      for_condition=NULL
    )
    
    # Handle clicks on treemap by country
    observeEvent(input$click_treemap_industry, {
      x <- input$click_treemap_industry$x
      y <- input$click_treemap_industry$y
      treemap_clicked_country$center <- c(x,y)
      
      if(is.null(treemap_clicked_country$for_condition)){
        treemap_clicked_country$for_condition=c(x,y)
      }
      else{treemap_clicked_country$for_condition=NULL}
    })
    
    getRecord_population_country <- reactive({
      x <- treemap_clicked_country$center[1]
      y <- treemap_clicked_country$center[2]
      
      x <- (x - .tm$vpCoorX[1]) / (.tm$vpCoorX[2] - .tm$vpCoorX[1])
      y <- (y - .tm$vpCoorY[1]) / (.tm$vpCoorY[2] - .tm$vpCoorY[1])
      
      
      l <- tmLocate(list(x=x, y=y), .tm)
      z=l[, 1:(ncol(l)-5)]
      
      
      if(is.na(z[,1]))
        return(NULL)
      
      col=as.character(z[,1])
      
      filter(tree,Industry==col)
    })
    
    condition1<-reactive({
      
      refresh=refresh()
      
      if(is.null(treemap_clicked_country$for_condition) & 
         refresh==0){
          result=1
        }else if((refresh%%2==0) & 
                 !is.null(treemap_clicked_country$for_condition)){
          result =0
        }else if((refresh%%2!=0) & 
                 !is.null(treemap_clicked_country$for_condition)){
          result =1
        }else if((refresh%%2!=0) & 
                 is.null(treemap_clicked_country$for_condition)){
          result =0
        }else if((refresh%%2==0) & 
                 is.null(treemap_clicked_country$for_condition)){
          result =1
        }
    })
    
    
    output$condition1 <- renderText({
      condition1()
    })
    
    outputOptions(output, 'condition1', 
                  suspendWhenHidden=FALSE)
    
    
    output$roa_year<-renderPlotly({
      temp = getRecord_population_country()
      title = "ROA in ten years"
      
      f <- list(
        family = "Courier New, monospace",
        size = 16,
        color = "#7f7f7f"
      )
      plot_ly(temp, 
              x = ~Year,
              y = ~ROA, 
              type = 'bar', 
              color = I("orange")) %>%
        layout(title = title,font = f,
               xaxis = list(title = ""),
               yaxis = list(title = "",
                            range=c(min(temp$ROA),max(temp$ROA))))
    })
    
    
    output$zoomout = renderUI({
      actionButton(
        "refresh", 
        em("Go to the previous page",
           style="text-align:center;
           color:red;font-size:200%"))
    })
    
    refresh=reactive({
      input$refresh
    })
    
    output$tree = renderCollapsibleTree(
      collapsibleTree(
        tree[tree$Year >= input$yearRange[1] & 
               tree$Year <= input$yearRange[2],],
        hierarchy = c("Type", "Industry", "Year"),
        attribute = input$column,
        nodeSize = input$column, 
        aggFun = mean,
        tooltip = TRUE,
        width = 800,
        height = 600,
        #fill = "Color",
        fillByLevel = TRUE,
        root = input$column
      )
    )
    
    output$plot1 = renderPlot(
      ggplot(tree[tree$Industry %in% input$mychooser$right &
                    tree$Year >= input$yearRange[1] &
                    tree$Year <= input$yearRange[2],],
             aes(Year,ROA,color = Industry)) +
        geom_point() + geom_smooth(se = FALSE)
    )
    
    output$plot2 = renderPlot(
      ggplot(tree[tree$Industry %in% input$mychooser$right &
                    tree$Year >= input$yearRange[1] &
                    tree$Year <= input$yearRange[2],],
             aes(Year,Rate,color = Industry)) +
        geom_point() + geom_smooth(se = FALSE)
    )
    
    output$text = renderText({
      input$column
    })
  }
)