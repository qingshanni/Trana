output$ui_overlapping <- renderUI(
  fluidRow( 
    column(width = 3, 
           wellPanel(
             selectInput(inputId = "overlapping_samplename_1", 
                         label   = "Chose Pair Sample",
                         choices = hot_to_r(input$showSampleInfo)$SampleName),
             selectInput(inputId = "overlapping_samplename_2", 
                         label = "Chose Pair Sample",
                         choices = hot_to_r(input$showSampleInfo)$SampleName,
                         selected = hot_to_r(input$showSampleInfo)$SampleName[2]),
             tags$hr(),
             radioButtons(inputId = "AA_or_NT_overlapping",
                          label = "Chose Type",
                          choices = c("AA","NT"))
           ),
           box(width = NULL, height = NULL,
               title = "Description", background = "navy",
               "The share clones of two samples were analyzed by scatter plot, 
               and the size of the dots indicated the degeneracy of the clonotype.")
           
    ),
    column(width = 9,
           box(
             plotOutput(outputId = "overlapping_plot", height = "600px", width = "600px"),
             tags$hr(),
             downloadButton(outputId = 'overlapping_downloadData',
                            label = 'Download data (csv)'),
             downloadButton(outputId = 'overlapping_downloadPlot',
                            label = 'Download figure(pdf)'),
             width = NULL, height=NULL,  collapsible = F,title = " ", status = "primary", solidHeader = TRUE)

    )
  )
)


overlapping_plot <- reactive({
  spn1 <- input$overlapping_samplename_1
  spn2 <- input$overlapping_samplename_2
  filename1 <- hot_to_r(input$showSampleInfo)[hot_to_r(input$showSampleInfo)$SampleName==spn1,"FilesName"]
  filename2 <- hot_to_r(input$showSampleInfo)[hot_to_r(input$showSampleInfo)$SampleName==spn2,"FilesName"]
  
  if(input$AA_or_NT_overlapping == "AA") in_counts <- aa_mx()
  else in_counts <- nt_mx()
  tmpdf <- as.data.frame(t(in_counts))[,c(filename1, filename2)]
  over_counts <- filter(tmpdf, tmpdf[,1]>0, tmpdf[,2]>0)
  over_counts$avr <- (over_counts[,1]+over_counts[,2])/2
  
  
  p <- ggplot()+theme_classic()+
    geom_point(data=over_counts,aes(x=log(over_counts[,1]),y=log(over_counts[,2]),size=log(over_counts[,3])), 
               fill="#FBB4AE", colour="black", alpha=0.8, pch=21) +
    geom_abline(intercept=0,slope=1, size=1, linetype="dashed") +
    theme(panel.grid = element_blank(), axis.text.y = element_text(angle = 90, hjust = 0.5), legend.title=element_blank()) +
    xlab(spn1) + ylab(spn2)
  return(p)
})


output$overlapping_plot <- renderPlot({
  overlapping_plot()
})


output$overlapping_downloadPlot <- downloadHandler(
  filename = function() { 
    return(paste0(input$overlapping_samplename_1,"&",input$overlapping_samplename_2,"_overlapping.pdf"))
  },
  content  = function(file) {
    
    ggsave(file, 
           plot = degeneracy_plot(), width = 5, height = 5, dpi = 300)
  }
)

output$overlapping_downloadData <- downloadHandler(
  filename = function() { 
    return(paste0(input$overlapping_samplename_1,"&",input$overlapping_samplename_2,"_overlapping.csv"))
  },
  content  = function(file) {
    spn1 <- input$overlapping_samplename_1
    spn2 <- input$overlapping_samplename_2
    filename1 <- hot_to_r(input$showSampleInfo)[hot_to_r(input$showSampleInfo)$SampleName==spn1,"FilesName"]
    filename2 <- hot_to_r(input$showSampleInfo)[hot_to_r(input$showSampleInfo)$SampleName==spn2,"FilesName"]
    
    if(input$AA_or_NT_overlapping == "AA") in_counts <- aa_mx()
    else in_counts <- nt_mx()
    tmpdf <- as.data.frame(t(in_counts))[,c(filename1, filename2)]
    over_counts <- filter(tmpdf, tmpdf[,1]>0, tmpdf[,2]>0)
    over_counts$avr <- (over_counts[,1]+over_counts[,2])/2
    write.csv(over_counts, file = file, quote = F, row.names = T)
  }
)
















