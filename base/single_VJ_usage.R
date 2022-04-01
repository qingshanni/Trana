output$ui_VJ_usage <- renderUI(
  fluidRow(
    column(width = 3, 
           wellPanel(
             selectInput(inputId = "VJ_usage_samplename", 
                         label   = "Chose one Sample",
                         choices = hot_to_r(input$showSampleInfo)$SampleName),
             
             # tags$hr(),
             # radioButtons(inputId = "V_and_J_usage",
             #              label = "Chose Plot",
             #              choices = c("Circos","HeatMap"))
           ),
           box(width = NULL, height = NULL,
               title = "Description", background = "navy",
               "Circos graph was used to count the combination of V and J gene fragments in the samples. 
               Each color block in the figure represents a V or J gene fragment. 
               The wider the color block, the higher the frequency of VJ gene fragment; 
               the wider the line, the higher the frequency of V-J gene fragment.
               Heat maps are also provided.")
    ),
    column(width = 9,
           tabBox(width = "600px", height = "600px",
             tabPanel("Circos",
                      plotOutput(outputId = "VJ_usage_circos", height = "700px", width = "700px"),
                      tags$hr(),
                      downloadButton(outputId = 'VJ_usage_downloaddatacircos', 
                                     label = 'Download data (csv)'),
                      downloadButton(outputId = 'VJ_usage_downloadplotcircos', 
                                     label = 'Download figure(pdf)')
                      ),
             tabPanel("HeatMap",
                      checkboxGroupInput(inputId = "param.VJ_usage_heatmap",
                                         label = "HeatMap Params",
                                         choices = c("cluster_rows","cluster_cols")),
                      plotOutput(outputId = "VJ_usage_heatmap", height = "700px", width = "600px"),
                      tags$hr(),
                      downloadButton(outputId = 'VJ_usage_downloaddataheatmap',
                                     label = 'Download data (csv)'),
                      downloadButton(outputId = "VJ_usage_downloadplotheatmap",
                                     label = "Download figure(pdf)")
                      )
                 )
          )
      )
)


VJ_usage <- reactive({
  spn <- input$VJ_usage_samplename
  filename <- hot_to_r(input$showSampleInfo)[hot_to_r(input$showSampleInfo)$SampleName==spn,"FilesName"]
  immdata <- immlist()[[filename]]
  df <- immdata[, c("Clones","V.name","J.name")]
  df_counts <- aggregate(df$Clones, by=list(df$V.name, df$J.name), FUN = "sum")
  names(df_counts) <- c("V.name", "J.name", "counts")
  tb <- reshape2::dcast(df_counts, V.name~J.name)
  tb[is.na(tb)] <- 0
  rownames(tb) <- tb[,1]
  tb <- tb[,-1]
  return(tb)
})


VJ_circos <- reactive({
  plotVJcircos(t(VJ_usage()))
})


VJ_heatmap <- reactive({
  cr <- F
  cc <- F
  if("cluster_rows" %in% input$param.VJ_usage_heatmap) cr <- T
  if("cluster_cols" %in% input$param.VJ_usage_heatmap) cc <- T
  p <- pheatmap(log(VJ_usage()+1),cluster_rows = cr, cluster_cols = cc)
  return(p)
})



output$VJ_usage_circos <- renderPlot({
  VJ_circos()
})

output$VJ_usage_heatmap <- renderPlot({
  VJ_heatmap()
})


output$VJ_usage_downloadplotcircos <- downloadHandler(
  filename = function() { 
    return(paste0(input$VJ_usage_samplename,"_VJ_junction_circos.pdf"))
  },
  content  = function(file) {
    # pdf(file)
    # VJ_circos()
    # dev.off()
    ggsave(file,
           plot = VJ_circos(), width = 6, height = 6, dpi = 300)
  }
)

output$VJ_usage_downloaddatacircos <- downloadHandler(
  filename = function() { 
    return(paste0(input$VJ_usage_samplename,"_VJ_junction.csv"))
  },
  content  = function(file) {
    write.csv(VJ_usage(), file = file, quote = F)
  }
)


output$VJ_usage_downloadplotheatmap <- downloadHandler(
  filename = function() { 
    return(paste0(input$VJ_usage_samplename,"_VJ_junction_heatmap.pdf"))
  },
  content  = function(file) {
    ggsave(file, 
           plot = VJ_heatmap(), width = 6.5, height = 5, dpi = 300)
  }
)

output$VJ_usage_downloaddataheatmap <- downloadHandler(
  filename = function() { 
    return(paste0(input$VJ_usage_samplename,"_VJ_junction.csv"))
  },
  content  = function(file) {
    write.csv(VJ_usage(), file = file, quote = F)
  }
)
