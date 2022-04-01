output$ui_clone_freq_analy <- renderUI(
  fluidRow( 
    column(width = 3, 
           wellPanel(
             selectInput(inputId = "clone_freq_analy_samplename", 
                         label   = "Chose one Sample",
                         choices = hot_to_r(input$showSampleInfo)$SampleName),
             tags$hr(),
             radioButtons(inputId = "AA_or_NT_clone_freq_analy",
                          label = "Chose Type",
                          choices = c("AA","NT"))
           ),
           box(width = NULL, height = NULL,
               title = "Description", background = "navy",
               "Sunburst Chart was drawn based on the frequency of nucleotide sequences of each sample. 
               The innermost layer shows the proportion of clones with frequency =1, =2 and ≥3; 
               The middle layer shows the proportion of nucleotide sequences per 20% in all clones with frequency ≥3; 
               The outermost layer shows the individual abundances of the top five clones with the highest frequency.")
           
    ),
    column(width = 9,
           box(
             # conditionalPanel("input.AA_or_NT_clone_freq_analy == 'AA'",
             #                  plotOutput(outputId = "clone_freq_analy_AA_plot", height = "600px", width = "600px")),
             # conditionalPanel("input.AA_or_NT_clone_freq_analy == 'NT'",
             #                  plotOutput(outputId = "clone_freq_analy_NT_plot", height = "600px", width = "600px")),
             plotOutput(outputId = "clone_freq_analy_plot", height = "600px", width = "600px"),
             tags$hr(),
             # downloadButton(outputId = 'clone_freq_analy_downloadData',
             #                label = 'Download data (csv)'),
             downloadButton(outputId = 'clone_freq_analy_downloadPlot',
                            label = 'Download figure(pdf)'),
             width = NULL, height=NULL,  collapsible = F,title = " ", status = "primary", solidHeader = TRUE)
           
           
    )
  )
)


clone_freq_analy_plot <- reactive({
  spn <- input$clone_freq_analy_samplename
  filename <- hot_to_r(input$showSampleInfo)[hot_to_r(input$showSampleInfo)$SampleName==spn,"FilesName"]
  if(input$AA_or_NT_clone_freq_analy == "AA") in_counts <- aa_mx()
  else in_counts <- nt_mx()
  v <- in_counts[filename,]
  
  v <- v[v>0]
  v <- sort(v,decreasing = T)
  df <- data.frame(Sequence = names(v), 
                   Clones   = v,
                   stringsAsFactors = F)
  
  p <- snailplot(df)
  return(p)
})


output$clone_freq_analy_plot <- renderPlot({
  clone_freq_analy_plot()
})

# output$clone_freq_analy_AA_plot <- renderPlot({
#   clone_freq_analy_plot()
# })
# 
# output$clone_freq_analy_NT_plot <- renderPlot({
#   clone_freq_analy_plot()
# })

output$clone_freq_analy_downloadPlot <- downloadHandler(
  filename = function() {
    return(paste0(input$clone_freq_analy_samplename,"_",input$AA_or_NT_clone_freq_analy,"__snail.pdf"))
  },
  content  = function(file) {
    
    ggsave(file,
           plot = clone_freq_analy_plot(), width = 6.5, height = 5.5, dpi = 300)
  }
)
























