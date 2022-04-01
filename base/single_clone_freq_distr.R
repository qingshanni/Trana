output$ui_clone_freq_distr <- renderUI(
  fluidRow( 
    
    column(width = 3, 
           wellPanel(
             selectInput(inputId = "clone_freq_distr_samplename", 
                         label   = "Chose one Sample",
                         choices = hot_to_r(input$showSampleInfo)$SampleName),
             tags$hr(),
             radioButtons(inputId = "AA_or_NT_clone_freq_distr",
                          label = "Chose Type",
                          choices = c("AA","NT"))
           ),
           box(width = NULL, height = NULL,
               title = "Description", background = "navy",
               "The clone frequency distribution map was drawn according to the frequency of clones in the sample. 
               The abscissa represents the ID number of the frequency of clones arranged from high to low, 
               and the ordinate represents the frequency of clones.")
           
    ),
    column(width = 9,
           box(
             conditionalPanel("input.AA_or_NT_clone_freq_distr == 'AA'",
                              plotOutput(outputId = "clone_freq_distr_AA_plot", height = "600px", width = "600px")),
             conditionalPanel("input.AA_or_NT_clone_freq_distr == 'NT'",
                              plotOutput(outputId = "clone_freq_distr_NT_plot", height = "600px", width = "600px")),
             tags$hr(),
             downloadButton(outputId = 'clone_freq_distr_downloadData',
                            label = 'Download data (csv)'),
             downloadButton(outputId = 'clone_freq_distr_downloadPlot',
                            label = 'Download figure(pdf)'),
             width = NULL, height=NULL,  collapsible = F,title = " ", status = "primary", solidHeader = TRUE)
           
           
    )
  )
)


clone_freq_distr_plot <- reactive({
  spn <- input$clone_freq_distr_samplename
  filename <- hot_to_r(input$showSampleInfo)[hot_to_r(input$showSampleInfo)$SampleName==spn,"FilesName"]
  if(input$AA_or_NT_clone_freq_distr == "AA") in_counts <- aa_mx()
  else in_counts <- nt_mx()
  sample <- t(in_counts)[,filename]
  # sample <- sample[sample>0]
  sort_sample <- sort(sample, decreasing = T)
  sort_df <- data.frame(id=1:length(sort_sample), counts <- sort_sample)
  p <- ggplot(sort_df, aes(x = log10(id), y = log10(counts))) + 
    geom_point(stat = "identity", color = "steelblue") +
    theme_bw() + theme(panel.grid = element_blank()) + 
    xlab("Rank of nucleotide clonotype (log10)") + 
    ylab("Number of clones (log10)")
  return(p)
})




output$clone_freq_distr_AA_plot <- renderPlot({
  clone_freq_distr_plot()
})

output$clone_freq_distr_NT_plot <- renderPlot({
  clone_freq_distr_plot()
})

output$clone_freq_distr_downloadPlot <- downloadHandler(
  filename = function() {
    return(paste0(input$clone_freq_distr_samplename,"_CDR3_",input$AA_or_NT_clone_freq_distr,"_freq_distribution.pdf"))
  },
  content  = function(file) {

    ggsave(file,
           plot = clone_freq_distr_plot(), width = 6.5, height = 5, dpi = 300)
  }
)

output$clone_freq_distr_downloadData <- downloadHandler(
  filename = function() {
    return(paste0(input$clone_freq_distr_samplename,"_CDR3_",input$AA_or_NT_clone_freq_distr,"_freq_distribution.csv"))
  },
  content  = function(file) {
    spn <- input$clone_freq_distr_samplename
    filename <- hot_to_r(input$showSampleInfo)[hot_to_r(input$showSampleInfo)$SampleName==spn,"FilesName"]
    if(input$AA_or_NT_clone_freq_distr == "AA") in_counts <- aa_mx()
    else in_counts <- nt_mx()
    sample <- t(in_counts)[,filename]
    # sample <- sample[sample>0]
    sort_sample <- sort(sample, decreasing = T)
    sort_df <- data.frame(id=1:length(sort_sample), counts <- sort_sample)
    write.csv(sort_df, file = file, quote = F, row.names = F)
  }
)