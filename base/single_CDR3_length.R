output$ui_CDR3_length <- renderUI(
  fluidRow( 
    
    column(width = 3, 
           wellPanel(
             selectInput(inputId = "CDR3_length_samplename", 
                         label   = "Chose one Sample",
                         choices = hot_to_r(input$showSampleInfo)$SampleName),
             tags$hr(),
             radioButtons(inputId = "AA_or_NT_length",
                          label = "Chose Type",
                          choices = c("AA","NT"))
           ),
           box(width = NULL, height = NULL,
               title = "Description", background = "navy",
               "The length distribution of CDR3 amino acid sequences in the samples was calculated by histogram, 
               and the Top10 clones with the highest frequency were marked in the graph.")
           
    ),
    column(width = 9,
           box(
             # conditionalPanel("input.AA_or_NT_length == 'AA'",
             #                  plotOutput(outputId = "CDR3_length_AA_plot", height = "600px", width = "600px")),
             # conditionalPanel("input.AA_or_NT_length == 'NT'",
             #                  plotOutput(outputId = "CDR3_length_NT_plot", height = "600px", width = "800px")),
             plotOutput(outputId = "CDR3_length_plot", height = "600px", width = "800px"),
             downloadButton(outputId = 'CDR3_length_downloadData',
                            label = 'Download data (csv)'),
             downloadButton(outputId = 'CDR3_length_downloadPlot',
                            label = 'Download figure(pdf)'),
             width = NULL, height=NULL,  collapsible = F,title = " ", status = "primary", solidHeader = TRUE)
             
           
    )
  )
)

CDR3_length_plot <- reactive({
  spn <- input$CDR3_length_samplename
  filename <- hot_to_r(input$showSampleInfo)[hot_to_r(input$showSampleInfo)$SampleName==spn,"FilesName"]
  if(input$AA_or_NT_length == "AA") in_counts <- aa_mx()
  else in_counts <- nt_mx()
  sample <- t(in_counts)[,filename]
  sample_df <- data.frame(seqnm=colnames(in_counts), counts=sample, stringsAsFactors = F)
  sample_df <- sample_df[sample_df$counts>0,]
  sample_df$len <- nchar(sample_df$seqnm)
  order_sample_df <- sample_df[order(sample_df$counts, decreasing = T),]
  order_sample_df$seqnm[11:nrow(order_sample_df)] <- "other"
  
  
  
  collst <- unique(c(brewer.pal(8,"Pastel1"),brewer.pal(8, "Pastel2")))
  p <- ggplot(order_sample_df, aes(x = len, y = counts, fill = seqnm)) + 
    geom_bar(stat = "identity", width = 0.5) + 
    scale_fill_manual(values = c(collst[1:10],"grey75")) +
    scale_y_continuous(expand = c(0, 0.1)) + theme_bw() + 
    labs(fill = "CloneType") +
    theme(panel.grid = element_blank()) + xlab("CDR3 length (bp)") + ylab("Number of clones")
  return(p)
})

output$CDR3_length_plot <- renderPlot({
  CDR3_length_plot()
})


output$CDR3_length_downloadPlot <- downloadHandler(
  filename = function() { 
    return(paste0(input$CDR3_length_samplename,"_CDR3_",input$AA_or_NT_length,"_length_distribution.pdf"))
  },
  content  = function(file) {
    if(input$AA_or_NT_length == "NT") width <- 12
    else width <- 6.5
    ggsave(file, 
           plot = CDR3_length_plot(), width = width, height = 5, dpi = 300)
  }
)

output$CDR3_length_downloadData <- downloadHandler(
  filename = function() { 
    return(paste0(input$CDR3_length_samplename,"_CDR3_",input$AA_or_NT_length,"_length_distribution.csv"))
  },
  content  = function(file) {
    spn <- input$CDR3_length_samplename
    filename <- hot_to_r(input$showSampleInfo)[hot_to_r(input$showSampleInfo)$SampleName==spn,"FilesName"]
    if(input$AA_or_NT_length == "AA") in_counts <- aa_mx()
    else in_counts <- nt_mx()
    tmp <- t(in_counts)[,filename]
    tmp <- data.frame(len=nchar(names(tmp)), clones=tmp)
    tmp_df <- aggregate(tmp$clones,by=list(tmp$len),FUN = "sum")
    tb_df <- tmp_df[,1:ncol(tmp_df)]
    colnames(tb_df) <- c("seq_Len","Clones")
    write.csv(tb_df, file = file, quote = F, row.names = F)
  }
)



