output$ui_degeneracy <- renderUI(
  fluidRow( 
    column(width = 3, 
           wellPanel(
             selectInput(inputId = "degeneracy_samplename", 
                         label   = "Chose one Sample",
                         choices = hot_to_r(input$showSampleInfo)$SampleName)
             # tags$hr(),
             # radioButtons(inputId = "AA_or_NT_clone_freq_analy",
             #              label = "Chose Type",
             #              choices = c("AA","NT"))
           ),
           box(width = NULL, height = NULL,
               title = "Description", background = "navy",
               "Due to the existence of codon degeneracy, multiple TCR Clonetype nucleic acid sequences can encode the same CDR3 amino acid sequence, 
               and the nucleotide encoding of CDR3 amino acid sequence in the sample can be displayed by Convergent analysis.")
           
    ),
    column(width = 9,
           box(
             plotOutput(outputId = "degeneracy_plot", height = "600px", width = "800px"),
             tags$hr(),
             downloadButton(outputId = 'degeneracy_downloadData',
                            label = 'Download data (csv)'),
             downloadButton(outputId = 'degeneracy_downloadPlot',
                            label = 'Download figure(pdf)'),
             width = NULL, height=NULL,  collapsible = F,title = " ", status = "primary", solidHeader = TRUE)
           
           
    )
  )
)


degeneracy_plot <- reactive({
  spn <- input$degeneracy_samplename
  filename <- hot_to_r(input$showSampleInfo)[hot_to_r(input$showSampleInfo)$SampleName==spn,"FilesName"]
  immdata <- immlist()[[filename]]
  df_clean <- immdata[,c("Clones","CDR3.nt","CDR3.aa")]
  df_counts <- aggregate(df_clean$Clones, by=list(df_clean$CDR3.aa), FUN = "sum")
  names(df_counts) <- c("CDR3.aa", "counts")
  df_num <- aggregate(df_clean$CDR3.nt, by=list(df_clean$CDR3.aa), FUN = "length")
  names(df_num) <- c("CDR3.aa", "CDR3.nt_num")
  
  idx <- rownames(df_counts)
  df_num <- df_num[idx,]
  dftmp <- cbind(df_counts, data.frame(CDR3.nt_num=df_num$CDR3.nt_num))
  
  smp_order <- dftmp[order(dftmp$counts, decreasing = T),]
  smp_order$id <- 1:nrow(smp_order)
  
  p <- ggplot(smp_order, aes(x = log10(id), y = log10(counts), size=CDR3.nt_num)) + 
    geom_point(stat = "identity", color = "steelblue") +
    theme_bw() + theme(panel.grid = element_blank()) + 
    labs(size="The number of CDR3")+
    xlab("Rank of nucleotide clonotype (log10)") + 
    ylab("Number of clones (log10)")
  
  return(p)
})

output$degeneracy_plot <- renderPlot({
  degeneracy_plot()
})


output$degeneracy_downloadPlot <- downloadHandler(
  filename = function() { 
    return(paste0(input$degeneracy_samplename,"_degeneracy.pdf"))
  },
  content  = function(file) {
    
    ggsave(file, 
           plot = degeneracy_plot(), width = 6.5, height = 5, dpi = 300)
  }
)

output$degeneracy_downloadData <- downloadHandler(
  filename = function() { 
    return(paste0(input$degeneracy_samplename,"_degeneracy.csv"))
  },
  content  = function(file) {
    spn <- input$degeneracy_samplename
    filename <- hot_to_r(input$showSampleInfo)[hot_to_r(input$showSampleInfo)$SampleName==spn,"FilesName"]
    immdata <- immlist()[[filename]]
    df_clean <- immdata[,c("Clones","CDR3.nt","CDR3.aa")]
    df_counts <- aggregate(df_clean$Clones, by=list(df_clean$CDR3.aa), FUN = "sum")
    names(df_counts) <- c("CDR3.aa", "counts")
    df_num <- aggregate(df_clean$CDR3.nt, by=list(df_clean$CDR3.aa), FUN = "length")
    names(df_num) <- c("CDR3.aa", "CDR3.nt_num")
    
    idx <- rownames(df_counts)
    df_num <- df_num[idx,]
    dftmp <- cbind(df_counts, data.frame(CDR3.nt_num=df_num$CDR3.nt_num))
    
    smp_order <- dftmp[order(dftmp$counts, decreasing = T),]
    smp_order$id <- 1:nrow(smp_order)
    write.csv(smp_order, file = file, quote = F, row.names = F)
  }
)


























