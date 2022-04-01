output$ui_high_freq_share_clone_var <- renderUI(
  fluidRow( 
    column(width = 3, 
           wellPanel(
             radioButtons(inputId = "AA_or_NT_high_freq_share_clone_var",
                          label = "Chose Type",
                          choices = c("AA","NT")),
             sliderInput(inputId = "top",
                         label   = "Top Clones",
                         min = 5,
                         max = 20,
                         value = 10,
                         step = 5),
             checkboxGroupInput(inputId = "high_freq_share_clone_var_samplenames",
                                label = "Multi Sample Chose",
                                choices = hot_to_r(input$showSampleInfo)$SampleName,
                                selected = hot_to_r(input$showSampleInfo)$SampleName)
           ),
           box(width = NULL, height = NULL,
               title = "Description", background = "navy",
               "High-frequency shared clones were tracked and analyzed between samples. 
               The total number of shared clones in the sample was sorted from high to low, 
               and the number change of the top 10 clones in each sample was shown in a broken line chart.")
           
    ),
    column(width = 9,
           box(
             plotOutput(outputId = "high_freq_share_clone_var_plot", 
                        width = paste0(length(input$high_freq_share_clone_var_samplenames)*100+400,"px")),
             tags$hr(),
             downloadButton(outputId = 'high_freq_share_clone_var_downloadData',
                            label = 'Download data (csv)'),
             downloadButton(outputId = 'high_freq_share_clone_var_downloadPlot',
                            label = 'Download figure(pdf)'),
             width = NULL, height=NULL,  collapsible = F,title = " ", status = "primary", solidHeader = TRUE)
           
    )
  )
)


high_freq_share_clone_var_plot <- reactive({
  spn <- input$high_freq_share_clone_var_samplenames
  filenames <- hot_to_r(input$showSampleInfo)[hot_to_r(input$showSampleInfo)$SampleName %in% spn,"FilesName"]
  if(input$AA_or_NT_high_freq_share_clone_var == "AA") in_counts <- aa_mx()
  else in_counts <- nt_mx()
  tmp <- as.data.frame(t(in_counts))[,filenames]
  
  colnames(tmp) <- spn
  tmp <- cbind(data.frame(seqnm=rownames(tmp)), tmp)
  tmp$totalClones <- rowSums(tmp[,2:ncol(tmp)])
  tmp <- tmp[order(tmp$totalClones, decreasing = T),]
  ntop <- input$top
  top <- tmp[1:ntop,1:ncol(tmp)-1]
  tmpdf <- reshape2::melt(top, id="seqnm")
  
  
  p <- (ggplot(tmpdf, aes(x = variable, y = log10(value), color = factor(seqnm), group=factor(seqnm))) 
        + geom_point(stat = "identity") 
        + geom_line(stat = "identity", size=1) 
        + theme_bw() 
        + theme(panel.grid = element_blank(), axis.text.x = element_text(angle = 45, hjust = 0.5, vjust = 0.5)) 
        + xlab("Sample") 
        + ylab("Number of clones (log10)")
        + labs(color="CDR3 Sequence"))
  return(p)
})


output$high_freq_share_clone_var_plot <- renderPlot({
  high_freq_share_clone_var_plot()
})


output$high_freq_share_clone_var_downloadPlot <- downloadHandler(
  filename = function() { 
    return(paste0(input$AA_or_NT_high_freq_share_clone_var,"_Top_freq_track.pdf"))
  },
  content  = function(file) {
    wid <- length(input$high_freq_share_clone_var_samplenames)*2 +3
    ht <- input$top*0.2 + 4
    ggsave(file, 
           plot = high_freq_share_clone_var_plot(), width = wid, height = ht, dpi = 300)
  }
)

output$high_freq_share_clone_var_downloadData <- downloadHandler(
  filename = function() {
    return(paste0(input$AA_or_NT_high_freq_share_clone_var,"_Top_freq_track.csv"))
  },
  content  = function(file) {
    spn <- input$high_freq_share_clone_var_samplenames
    filenames <- hot_to_r(input$showSampleInfo)[hot_to_r(input$showSampleInfo)$SampleName %in% spn,"FilesName"]
    if(input$AA_or_NT_high_freq_share_clone_var == "AA") in_counts <- aa_mx()
    else in_counts <- nt_mx()
    tmp <- as.data.frame(t(in_counts))[,filenames]
    
    colnames(tmp) <- spn
    tmp <- cbind(data.frame(seqnm=rownames(tmp)), tmp)
    tmp$totalClones <- rowSums(tmp[,2:ncol(tmp)])
    tmp <- tmp[order(tmp$totalClones, decreasing = T),]
    write.csv(tmp, file = file, quote = F, row.names = F)
  }
)














