output$ui_clone_cumul_freq <- renderUI(
  fluidRow( 
    column(width = 3, 
           wellPanel(
             radioButtons(inputId = "AA_or_NT_clone_cumul_freq",
                          label = "Chose Type",
                          choices = c("AA","NT")),
             checkboxGroupInput(inputId = "clone_cumul_freq_samplenames",
                                label = "Multi Sample Chose",
                                choices = hot_to_r(input$showSampleInfo)$SampleName,
                                selected = hot_to_r(input$showSampleInfo)$SampleName)
           ),
           box(width = NULL, height = NULL,
               title = "Description", background = "navy",
               "The cumulative frequency distribution of clone types of all samples was analyzed, 
               and the clone types of the samples were ranked from low to high according to the frequency, 
               and the cumulative frequency of clone types in different types (top 10%, Top20%, 30%, 40%, 50%, 60%, 70%, 80%, 90%, 100%) was calculated and plotted.")
           
    ),
    column(width = 9,
           box(
             plotOutput(outputId = "clone_cumul_freq_plot", width = "1000px", height = "500px"),
             tags$hr(),
             downloadButton(outputId = 'clone_cumul_freq_downloadData',
                            label = 'Download data (csv)'),
             downloadButton(outputId = 'clone_cumul_freq_downloadPlot',
                            label = 'Download figure(pdf)'),
             width = NULL, height=NULL,  collapsible = F,title = " ", status = "primary", solidHeader = TRUE)
           
    )
  )
)


clone_cumul_freq_plot <- reactive({
  spn <- input$clone_cumul_freq_samplenames
  filenames <- hot_to_r(input$showSampleInfo)[hot_to_r(input$showSampleInfo)$SampleName %in% spn,"FilesName"]
  if(input$AA_or_NT_clone_cumul_freq == "AA") in_counts <- aa_mx()
  else in_counts <- nt_mx()
  tmpdf <- as.data.frame(t(in_counts))[,filenames]

  if (length(spn) == 1){
    tmp <- data.frame(sp=tmpdf)
    rownames(tmp) <- rownames(t(in_counts))
    tmpdf <- tmp
  }
  colnames(tmpdf) <- spn

  tmp <- as.list(tmpdf)
  sort_counts <- lapply(tmp, function(x){
    dd <- x[x>0]
    dd <- sort(dd)
  })
  
  top <- sapply(sort_counts, function(x){
    ls_rate <- sapply(seq(0,50,1), function(fd){
      idx <- round(length(x) * 0.02 * fd)
      rate <- sum(x[0:idx]) / sum(x)
    })
  })
  df_top <- as.data.frame(top)
  tmp <- cbind(data.frame(top=0:50), df_top)
  # write.csv(tmp, file = paste0(outpath,"/",cloneType,"_","clonal_cumulative.csv"), quote = F)
  df_melt <- reshape2::melt(tmp, id="top")
  
  p <-  ggplot(data = df_melt,aes(x = top,y=value,color = variable))+
    geom_line(size=1.5)+
    theme_classic() +
    geom_abline(intercept=0,slope=0.02, size=1.5, linetype="dashed") +
    labs(x="Cumulative percent of unique CDR3 clones\n(ordered by increasing clonal size)", 
         y="Cumulative percent of total\nCDR3 reqertoire")+
    theme(legend.position = c(0.05, 1), legend.justification = c(0.1, 1), 
          legend.title = element_blank()) +
    scale_x_continuous(expand=c(0,0), breaks = seq(0,50,5), labels = paste0(seq(0,100,10),"%"))+
    scale_y_continuous(expand = c(0,0), breaks = seq(0,1,0.2), labels = paste0(seq(0,100,20), "%"))
  return(p)
})


output$clone_cumul_freq_plot <- renderPlot({
  clone_cumul_freq_plot()
})


output$clone_cumul_freq_downloadPlot <- downloadHandler(
  filename = function() { 
    return(paste0(input$AA_or_NT_clone_cumul_freq,"_clonal_cumulative.pdf"))
  },
  content  = function(file) {
    ggsave(file, 
           plot = clone_cumul_freq_plot(), width = 12, height = 6, dpi = 300)
  }
)

output$clone_cumul_freq_downloadData <- downloadHandler(
  filename = function() { 
    return(paste0(input$AA_or_NT_clone_cumul_freq,"_clonal_cumulative.csv"))
  },
  content  = function(file) {
    spn <- input$clone_cumul_freq_samplenames
    filenames <- hot_to_r(input$showSampleInfo)[hot_to_r(input$showSampleInfo)$SampleName %in% spn,"FilesName"]
    if(input$AA_or_NT_clone_cumul_freq == "AA") in_counts <- aa_mx()
    else in_counts <- nt_mx()
    tmpdf <- as.data.frame(t(in_counts))[,filenames]
    
    if (length(spn) == 1){
      tmp <- data.frame(sp=tmpdf)
      rownames(tmp) <- rownames(t(in_counts))
      tmpdf <- tmp
    }
    colnames(tmpdf) <- spn
    
    tmp <- as.list(tmpdf)
    sort_counts <- lapply(tmp, function(x){
      dd <- x[x>0]
      dd <- sort(dd)
    })
    
    top <- sapply(sort_counts, function(x){
      ls_rate <- sapply(seq(0,50,1), function(fd){
        idx <- round(length(x) * 0.02 * fd)
        rate <- sum(x[0:idx]) / sum(x)
      })
    })
    df_top <- as.data.frame(top)
    tmp <- cbind(data.frame(top=0:50), df_top)
    write.csv(tmp, file = file, quote = F, row.names = T)
  }
)






















