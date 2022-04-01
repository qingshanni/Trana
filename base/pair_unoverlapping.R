output$ui_unoverlapping <- renderUI(
  fluidRow( 
    column(width = 3, 
           wellPanel(
             selectInput(inputId = "unoverlapping_samplename_1", 
                         label   = "Chose Pair Sample",
                         choices = hot_to_r(input$showSampleInfo)$SampleName),
             selectInput(inputId = "unoverlapping_samplename_2", 
                         label = "Chose Pair Sample",
                         choices = hot_to_r(input$showSampleInfo)$SampleName,
                         selected = hot_to_r(input$showSampleInfo)$SampleName[2])
             
           ),
           box(width = NULL, height = NULL,
               title = "Description", background = "navy",
               "The frequency distribution of unique clones between the two samples was compared. 
               The horizontal axis represented the ID numbers of clones arranged from high frequency to low frequency, 
               and the vertical axis represented the frequency of clones of the samples.")
           
    ),
    column(width = 9,
           box(
             plotOutput(outputId = "unoverlapping_plot", height = "600px", width = "800px"),
             tags$hr(),
             downloadButton(outputId = 'unoverlapping_downloadData',
                            label = 'Download data (csv)'),
             downloadButton(outputId = 'unoverlapping_downloadPlot',
                            label = 'Download figure(pdf)'),
             width = NULL, height=NULL,  collapsible = F,title = " ", status = "primary", solidHeader = TRUE)
           
    )
  )
)


unoverlapping_plot <- reactive({
  spn1 <- input$unoverlapping_samplename_1
  spn2 <- input$unoverlapping_samplename_2
  filename1 <- hot_to_r(input$showSampleInfo)[hot_to_r(input$showSampleInfo)$SampleName==spn1,"FilesName"]
  filename2 <- hot_to_r(input$showSampleInfo)[hot_to_r(input$showSampleInfo)$SampleName==spn2,"FilesName"]
  dat_clones <- immlist()[c(filename1,filename2)]
  res <- lapply(dat_clones, function(df){
    df_clean <- df[,c("Clones","CDR3.nt","CDR3.aa")]
    df_counts <- aggregate(df_clean$Clones, by=list(df_clean$CDR3.aa), FUN = "sum")
    names(df_counts) <- c("CDR3.aa", "counts")
    df_num <- aggregate(df_clean$CDR3.nt, by=list(df_clean$CDR3.aa), FUN = "length")
    names(df_num) <- c("CDR3.aa", "CDR3.nt_num")
    
    idx <- rownames(df_counts)
    df_num <- df_num[idx,]
    dftmp <- cbind(df_counts, data.frame(CDR3.nt_num=df_num$CDR3.nt_num))
  })
  sample1 <- res[[filename1]]
  sample2 <- res[[filename2]]
  rownames(sample1) <- sample1$CDR3.aa
  rownames(sample2) <- sample2$CDR3.aa
  inter <- intersect(sample1$CDR3.aa, sample2$CDR3.aa) # 求CDR3.aa 交集
  smp1_diff <- setdiff(sample1$CDR3.aa, inter)
  smp2_diff <- setdiff(sample2$CDR3.aa, inter)
  
  smp1_clean <- sample1[smp1_diff,]
  smp2_clean <- sample2[smp2_diff,]
  
  smp1_order <- smp1_clean[order(smp1_clean$counts, decreasing = T),]
  smp2_order <- smp2_clean[order(smp2_clean$counts, decreasing = T),]
  
  smp1_order$id <- 1:nrow(smp1_order)
  smp2_order$id <- 1:nrow(smp2_order)
  
  smp1_order$sample <- filename1
  smp2_order$sample <- filename2
  
  df <- rbind(smp1_order, smp2_order)
  
  p <- ggplot(df, aes(x = log10(id), y = log10(counts), color = sample, size=CDR3.nt_num)) + 
    geom_point(stat = "identity") + scale_color_manual(values = c("steelblue","orange")) +
    theme_bw() + theme(panel.grid = element_blank()) +
    labs(size="The number of CDR3")+
    xlab("Rank of nucleotide clonotype (log10)") + ylab("Number of clones (log10)")
  
  return(p)
})


output$unoverlapping_plot <- renderPlot({
  unoverlapping_plot()
})


output$unoverlapping_downloadPlot <- downloadHandler(
  filename = function() { 
    return(paste0(input$overlapping_samplename_1,"&",input$overlapping_samplename_2,"_unoverlapping.pdf"))
  },
  content  = function(file) {
    
    ggsave(file, 
           plot = unoverlapping_plot(), width = 6.5, height = 5, dpi = 300)
  }
)

output$unoverlapping_downloadData <- downloadHandler(
  filename = function() { 
    return(paste0(input$overlapping_samplename_1,"&",input$unoverlapping_samplename_2,"_unoverlapping.csv"))
  },
  content  = function(file) {
    spn1 <- input$unoverlapping_samplename_1
    spn2 <- input$unoverlapping_samplename_2
    filename1 <- hot_to_r(input$showSampleInfo)[hot_to_r(input$showSampleInfo)$SampleName==spn1,"FilesName"]
    filename2 <- hot_to_r(input$showSampleInfo)[hot_to_r(input$showSampleInfo)$SampleName==spn2,"FilesName"]
    dat_clones <- immlist()[c(filename1,filename2)]
    res <- lapply(dat_clones, function(df){
      df_clean <- df[,c("Clones","CDR3.nt","CDR3.aa")]
      df_counts <- aggregate(df_clean$Clones, by=list(df_clean$CDR3.aa), FUN = "sum")
      names(df_counts) <- c("CDR3.aa", "counts")
      df_num <- aggregate(df_clean$CDR3.nt, by=list(df_clean$CDR3.aa), FUN = "length")
      names(df_num) <- c("CDR3.aa", "CDR3.nt_num")
      
      idx <- rownames(df_counts)
      df_num <- df_num[idx,]
      dftmp <- cbind(df_counts, data.frame(CDR3.nt_num=df_num$CDR3.nt_num))
    })
    sample1 <- res[[filename1]]
    sample2 <- res[[filename2]]
    rownames(sample1) <- sample1$CDR3.aa
    rownames(sample2) <- sample2$CDR3.aa
    inter <- intersect(sample1$CDR3.aa, sample2$CDR3.aa) # 求CDR3.aa 交集
    smp1_diff <- setdiff(sample1$CDR3.aa, inter)
    smp2_diff <- setdiff(sample2$CDR3.aa, inter)
    
    smp1_clean <- sample1[smp1_diff,]
    smp2_clean <- sample2[smp2_diff,]
    
    smp1_order <- smp1_clean[order(smp1_clean$counts, decreasing = T),]
    smp2_order <- smp2_clean[order(smp2_clean$counts, decreasing = T),]
    
    smp1_order$id <- 1:nrow(smp1_order)
    smp2_order$id <- 1:nrow(smp2_order)
    
    smp1_order$sample <- filename1
    smp2_order$sample <- filename2
    
    df <- rbind(smp1_order, smp2_order)
    write.csv(df, file = file, quote = F, row.names = T)
  }
)
















