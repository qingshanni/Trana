###V_J_usage###----
output$ui_V_J_usage <- renderUI(
  fluidRow( 
    column(width = 3, 
           wellPanel(
             selectInput(inputId = "V_J_usage_samplename", 
                         label   = "Chose one Sample",
                         choices = hot_to_r(input$showSampleInfo)$SampleName),
             tags$hr(),
             radioButtons(inputId = "V_or_J_usage",
                          label = "Chose Gene",
                          choices = c("V","J"))
           ),
           box(width = NULL, height = NULL,
               title = "Description", background = "navy",
               "The frequency of TRBV or TRBJ gene segment was measured by histogram")
    ),
    column(width = 9,
           box(width = NULL, height = NULL,
               plotOutput(outputId = "V_J_usage_plot"),
               tags$hr(),
               downloadButton(outputId = 'V_J_usage_downloadData', 
                              label = 'Download data (csv)'),
               downloadButton(outputId = 'V_J_usage_downloadPlot', 
                              label = 'Download figure(pdf)'),
               collapsible = F,title = " ", status = "primary", solidHeader = TRUE)
    )
  )
)

V_J_usage <- reactive({
  spn <- input$V_J_usage_samplename
  filename <- hot_to_r(input$showSampleInfo)[hot_to_r(input$showSampleInfo)$SampleName==spn,"FilesName"]
  immdata <- immlist()[[filename]]
  df <- immdata[, c("Clones","V.name","J.name")]
})

V_usage_plot <- reactive({
  spn <- input$V_J_usage_samplename
  V_usage <- aggregate(V_J_usage()$Clones, list(V.name=V_J_usage()$V.name), "sum")
  names(V_usage) <- c("gene", "Clones")
  p <- ggplot(V_usage, aes(x = gene, y = Clones, fill = Clones)) + 
    geom_bar(stat = "identity", color = "black") + 
    scale_fill_gradient2(low = "steelblue", mid = "white", high = "red3", midpoint = (max(V_usage$Clones)-min(V_usage$Clones))/2) +
    theme_change2(yrot = 0) + xlab("Gene") + ylab(paste0("V"," frequency in ",spn)) +
    theme(axis.text.x = element_text(hjust = 1, vjust = 1, angle = 45), 
          legend.position = "right", legend.text = element_text(size = 6))
  return(p)
})

J_usage_plot <- reactive({
  spn <- input$V_J_usage_samplename
  J_usage <- aggregate(V_J_usage()$Clones, list(J.name=V_J_usage()$J.name), "sum")
  names(J_usage) <- c("gene", "Clones")
  p <- ggplot(J_usage, aes(x = gene, y = Clones, fill = Clones)) + 
    geom_bar(stat = "identity", color = "black") + 
    scale_fill_gradient2(low = "steelblue", mid = "white", high = "red3", midpoint = (max(J_usage$Clones)-min(J_usage$Clones))/2) +
    theme_change2(yrot = 0) + xlab("Gene") + ylab(paste0("J"," frequency in ",spn)) +
    theme(axis.text.x = element_text(hjust = 1, vjust = 1, angle = 45), 
          legend.position = "right", legend.text = element_text(size = 6))
  return(p)
})

output$V_J_usage_plot <- renderPlot({
  if(input$V_or_J_usage == "V") V_usage_plot()
  else J_usage_plot()
})

output$V_J_usage_downloadPlot <- downloadHandler(
  filename = function() { 
    if(input$V_or_J_usage == "V") return(paste0(input$V_J_usage_samplename,"_V_usage.pdf"))
    else return(paste0(input$V_J_usage_samplename, "_J_usage.pdf"))
  },
  content  = function(file) {
    if(input$V_or_J_usage == "V"){
      p <- V_usage_plot()
      usage <- aggregate(V_J_usage()$Clones, list(V.name=V_J_usage()$V.name), "sum")
    }
    else{
      p <- J_usage_plot()
      usage <- aggregate(V_J_usage()$Clones, list(J.name=V_J_usage()$J.name), "sum")
    }
    ggsave(file, 
           plot = p, width = 0.2*nrow(usage)+2, height = 3.5)
    
  }
)


####

output$V_J_usage_downloadData <- downloadHandler(
  filename = function(){
    if(input$V_or_J_usage == "V") return(paste0(input$V_J_usage_samplename, "_V_usage.csv"))
    else return(paste0(input$V_J_usage_samplename,"_J_usage.csv"))
  },
  content = function(file){
    if(input$V_or_J_usage == "V"){
      usage <- aggregate(V_J_usage()$Clones, list(V.name=V_J_usage()$V.name), "sum")
      names(usage) <- c("V.name", "Clones")
    }
    else{
      usage <- aggregate(V_J_usage()$Clones, list(J.name=V_J_usage()$J.name), "sum")
      names(usage) <- c("J.name", "Clones")
    }
        write.csv(usage, file = file, quote = F, row.names = F)
    
  }
)
# withProgress(message = 'Calculation in progress',
#              detail = 'This may take a while...', value = 0,
#              expr = {
#                dd           <- CreateSeuratObject(gdata_expr(),meta.data =gdata_phenotype())
#                mito.genes   <- grep(pattern = "^[mM][tT]-", x = rownames(x = dd@data), value = TRUE)
#                percent.mito <- Matrix::colSums(dd@raw.data[mito.genes, ])/Matrix::colSums(dd@raw.data)
#                dd           <- AddMetaData(object = dd, metadata = percent.mito, col.name = "percent.mito")
#              })
