output$ui_similarity <- renderUI(
  fluidRow( 
    column(width = 3, 
           wellPanel(
             radioButtons(inputId = "AA_or_NT_similarity",
                          label = "Chose Type",
                          choices = c("AA","NT","VJ")),
             radioButtons(inputId = "similarity_type",
                          label = "Chose Similarity",
                          choices = c("Morisita", "Jaccard", "Bray_Curtis")),
             checkboxGroupInput(inputId = "param.similarity_heatmap",
                                label = "HeatMap Params",
                                choices = c("cluster_rows","cluster_cols")),
             tags$hr(),
             checkboxGroupInput(inputId = "similarity_samplenames",
                                label = "Multi Sample Chose",
                                choices = hot_to_r(input$showSampleInfo)$SampleName,
                                selected = hot_to_r(input$showSampleInfo)$SampleName)
           ),
           box(width = NULL, height = NULL,
               title = "Description", background = "navy",
               "The similarity index was used to evaluate the similarity of clones among samples, 
               and the sample similarity clustering analysis was carried out. 
               Similarity indexes include morisita-Horn Similarity index, Jaccard Index and Bhattacharyya Coef ficient.")
           
    ),
    column(width = 9,
           box(
             plotOutput(outputId = "similarity_plot", width = "600px", height = "600px"),
             tags$hr(),
             downloadButton(outputId = 'similarity_downloadData',
                            label = 'Download data (csv)'),
             downloadButton(outputId = 'similarity_downloadPlot',
                            label = 'Download figure(pdf)'),
             width = NULL, height=NULL,  collapsible = F,title = " ", status = "primary", solidHeader = TRUE)
           
    )
  )
)


similarity_plot <- reactive({
  spn <- input$similarity_samplenames
  filenames <- hot_to_r(input$showSampleInfo)[hot_to_r(input$showSampleInfo)$SampleName %in% spn,"FilesName"]
  dat_clones <- immlist()[filenames]
  res <- myimmunarch2vectorlist(dat_clones, type=input$AA_or_NT_similarity)
  tmp <- list_vector_to_mx(res)
  methd <- switch (input$similarity_type,
                   Morisita = "morisita",
                   Jaccard = "jaccard",
                   Bray_Curtis  = "bray")
  mtx <- 1 - as.matrix(vegdist(tmp, method = methd))
  # write.csv(mtx, file = paste0(outpath,"/",type,"/",methd,".csv"), quote = F)
  cr <- F
  cc <- F
  if("cluster_rows" %in% input$param.similarity_heatmap) cr <- T
  if("cluster_cols" %in% input$param.similarity_heatmap) cc <- T
  p <- pheatmap(mtx,cluster_rows = cr, cluster_cols = cc)
  
  return(p)
})


output$similarity_plot <- renderPlot({
  similarity_plot()
})


output$similarity_downloadPlot <- downloadHandler(
  filename = function() { 
    return(paste0(input$AA_or_NT_similarity,"_similarity.pdf"))
  },
  content  = function(file) {
    ggsave(file, 
           plot = similarity_plot(), width = 5, height = 5, dpi = 300)
  }
)

output$similarity_downloadData <- downloadHandler(
  filename = function() { 
    return(paste0(input$AA_or_NT_similarity,"_similarity.csv"))
  },
  content  = function(file) {
    spn <- input$similarity_samplenames
    filenames <- hot_to_r(input$showSampleInfo)[hot_to_r(input$showSampleInfo)$SampleName %in% spn,"FilesName"]
    dat_clones <- immlist()[filenames]
    names(dat_clones) <- spn
    res <- myimmunarch2vectorlist(dat_clones, type=input$AA_or_NT_similarity)
    tmp <- list_vector_to_mx(res)
    methd <- switch (input$similarity_type,
                     Morisita = "morisita",
                     Jaccard = "jaccard",
                     Bray_Curtis  = "bray")
    mtx <- 1 - as.matrix(vegdist(tmp, method = methd))
    write.csv(mtx, file = file, quote = F, row.names = T)
  }
)





