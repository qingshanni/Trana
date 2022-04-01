output$ui_diversity <- renderUI(
  fluidRow( 
    column(width = 3, 
           wellPanel(
             radioButtons(inputId = "AA_or_NT_diversity",
                          label = "Chose Type",
                          choices = c("AA","NT")),
             radioButtons(inputId = "diversity_type",
                          label = "Chose Diversity",
                          choices = c("Shannon_entropy",
                                      "Simpson_index",
                                      "Inverse_Simpson_index",
                                      "Gini_Simpson_index",
                                      "Berger_Parker_index" ,
                                      "Renyi_entropy")),
             checkboxGroupInput(inputId = "diversity_samplenames",
                                label = "Multi Sample Chose",
                                choices = hot_to_r(input$showSampleInfo)$SampleName,
                                selected = hot_to_r(input$showSampleInfo)$SampleName)
           ),
           box(width = NULL, height = NULL,
               title = "Description", background = "navy",
               "The diversity index was used to evaluate the diversity of sample clones, 
               including Shannon Entropy, Simpson Index, Inverse Simpson Index, Gini Simpson Index, and Berger-Parker index.")
           
    ),
    column(width = 9,
           box(
             plotOutput(outputId = "diversity_plot", width = "600px", height = "600px"),
             tags$hr(),
             downloadButton(outputId = 'diversity_downloadData',
                            label = 'Download data (csv)'),
             downloadButton(outputId = 'diversity_downloadPlot',
                            label = 'Download figure(pdf)'),
             width = NULL, height=NULL,  collapsible = F,title = " ", status = "primary", solidHeader = TRUE)
           
    )
  )
)


diversity_plot <- reactive({
  spn <- input$diversity_samplenames
  filenames <- hot_to_r(input$showSampleInfo)[hot_to_r(input$showSampleInfo)$SampleName %in% spn,"FilesName"]
  if(input$AA_or_NT_diversity == "AA") in_counts <- aa_mx()
  else in_counts <- nt_mx()
  tmpdf <- as.data.frame(t(in_counts))[,filenames]
  
  if (length(spn) == 1){
    tmp <- data.frame(sp=tmpdf)
    rownames(tmp) <- rownames(t(in_counts))
    tmpdf <- tmp
  }
  colnames(tmpdf) <- spn
  tmp <- as.list(tmpdf)
  type <- switch (input$diversity_type,
                    Shannon_entropy = "shan" ,
                    Simpson_index = "smp",
                    Inverse_Simpson_index = "ismp",
                    Gini_Simpson_index = "gsi",
                    Berger_Parker_index = "bpi" ,
                    Renyi_entropy = "renyi")
  v <- sapply(tmp, function(dd){
    dd <- dd[dd>0]
    tcr_diversity(dd, method=type, q=2)
  })
  
  dd <- data.frame(Sample.name=spn, Diversity=v, stringsAsFactors=F)
  p <- ggplot(data = dd, aes(x = Sample.name, y = Diversity, fill = Diversity)) + 
    geom_bar(stat = "identity", colour = "black", width = 0.8) + 
    scale_fill_distiller(palette = "Blues") + scale_y_continuous(expand = c(0,0)) +
    theme_bw() + xlab("Sample") + ylab(input$diversity_type) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), 
          panel.border = element_blank(), 
          axis.line = element_line(),
          panel.grid = element_blank(), 
          legend.position = "none", 
          plot.margin = unit(c(0.3,0.3,0.3,0.3),"cm"))
  return(p)
})


output$diversity_plot <- renderPlot({
  diversity_plot()
})


output$diversity_downloadPlot <- downloadHandler(
  filename = function() { 
    return(paste0(input$AA_or_NT_diversity,"_diversity.pdf"))
  },
  content  = function(file) {
    wid = 0.8*length(input$diversity_samplenames)+2
    ggsave(file, 
           plot = diversity_plot(), width = wid, height = 5, dpi = 300)
  }
)

output$diversity_downloadData <- downloadHandler(
  filename = function() { 
    return(paste0(input$AA_or_NT_diversity,"_diversity.csv"))
  },
  content  = function(file) {
    spn <- input$diversity_samplenames
    filenames <- hot_to_r(input$showSampleInfo)[hot_to_r(input$showSampleInfo)$SampleName %in% spn,"FilesName"]
    if(input$AA_or_NT_diversity == "AA") in_counts <- aa_mx()
    else in_counts <- nt_mx()
    tmpdf <- as.data.frame(t(in_counts))[,filenames]
    
    if (length(spn) == 1){
      tmp <- data.frame(sp=tmpdf)
      rownames(tmp) <- rownames(t(in_counts))
      tmpdf <- tmp
    }
    colnames(tmpdf) <- spn
    tmp <- as.list(tmpdf)
    type <- switch (input$diversity_type,
                    Shannon_entropy = "shan" ,
                    Simpson_index = "smp",
                    Inverse_Simpson_index = "ismp",
                    Gini_Simpson_index = "gsi",
                    Berger_Parker_index = "bpi" ,
                    Renyi_entropy = "renyi")
    v <- sapply(tmp, function(dd){
      dd <- dd[dd>0]
      tcr_diversity(dd, method=type, q=2)
    })
    
    dd <- data.frame(Sample.name=spn, Diversity=v, stringsAsFactors=F)
    write.csv(dd, file = file, quote = F, row.names = F)
  }
)




















