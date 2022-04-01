output$ui_CDR3_rarecurve <- renderUI(
  fluidRow( 
    
    column(width = 3, 
           wellPanel(
             checkboxGroupInput(inputId = "rarecurve_samplenames",
                                label = "Multi Sample Chose",
                                choices = hot_to_r(input$showSampleInfo)$SampleName,
                                selected = hot_to_r(input$showSampleInfo)$SampleName)
           ),
           box(width = NULL, height = NULL,
               title = "Description", background = "navy",
               "The dilution curve is a random sampling of a certain amount of CDR3 from the sample to count the number of unique CDR3. 
               When the curve tends to be flat, it indicates that the amount of sequencing data is reasonable, 
               and more data will only generate a small amount of new CDR3; otherwise, it indicates that more sequencing may generate more new CDR3. 
               Therefore, the dilution curve can not only reflect whether the amount of sample sequencing data is reasonable, 
               but also reflect the species richness of the sample.")
           
    ),
    column(width = 9,
           box(plotOutput(outputId = "rarecurve_plot", height = "500px", width = "500px"),
               tags$hr(),
               # downloadButton(outputId = 'rarecurve_downloadData', 
               #                label = 'Download data (csv)'),
               downloadButton(outputId = 'rarecurve_downloadPlot', 
                              label = 'Download figure(pdf)'),
               width = NULL, height=NULL,  collapsible = F,title = " ", status = "success", solidHeader = TRUE),
           
    )
  )
)


rarecurve_plot <- reactive({
  spn <- input$rarecurve_samplenames
  filenames <- hot_to_r(input$showSampleInfo)[hot_to_r(input$showSampleInfo)$SampleName %in% spn,"FilesName"]
  in_counts <- nt_mx()
  tmpdf <- as.data.frame(in_counts)[filenames,]
  
  if (length(spn) == 1){
    tmpdf <- data.frame(sp=tmpdf)
    # rownames(tmp) <- rownames(t(in_counts))
    # tmpdf <- tmp
  }
  rownames(tmpdf) <- spn
  
  collst <- brewer.pal(nrow(tmpdf),"Dark2") 
  
  raewmax <- min(rowSums(tmpdf))
  # rarecurve <- rarecurve(tmpdf, step = raewmax/50,  sample = raewmax,
  #                      col = collst, cex = 1, ylab = "Diversity", lwd=2)
  
  # pdf(paste0(outpath,"/", "rarecurve.pdf"), width = 6.5, height = 6.5)
  # print(rarecurve(tmpdf, step = raewmax/50,  sample = raewmax,
  #                 col = collst, cex = 1, ylab = "Diversity", lwd=2))
  # dev.off()
  # png(paste0(outpath,"/", "rarecurve.png"), width = 6.5, height = 6.5, res = 300, units = "in")
  # print(rarecurve(tmpdf, step = raewmax/50,  sample = raewmax,
  #                 col = collst, cex = 1, ylab = "Diversity", lwd=2))
  # dev.off()
  withProgress(message = 'Calculation in progress',
               detail = 'This may take a while...', value = 0,
               expr = {
                 p <- rarecurve(tmpdf, step = raewmax/50,  sample = raewmax,
                                col = collst, cex = 1, ylab = "Diversity", lwd=2)
               })
  return(p)
})

output$rarecurve_plot <- renderPlot({
  rarecurve_plot()
})







