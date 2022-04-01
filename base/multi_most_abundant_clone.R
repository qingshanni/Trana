output$ui_most_abundant_clone <- renderUI(
  fluidRow( 
    column(width = 3, 
           wellPanel(
             radioButtons(inputId = "AA_or_NT_most_abundant_clone",
                          label = "Chose Type",
                          choices = c("AA","NT")),
             checkboxGroupInput(inputId = "most_abundant_clone_samplenames",
                                label = "Multi Sample Chose",
                                choices = hot_to_r(input$showSampleInfo)$SampleName,
                                selected = hot_to_r(input$showSampleInfo)$SampleName)
           ),
           box(width = NULL, height = NULL,
               title = "Description", background = "navy",
               "The proportion of clones in different sequencing ranges was analyzed by stacking histogram (clone spatial steady-state analysis). 
               The clones were divided into 5 groups according to the frequency of cloning types, and the proportion of clones in each group was counted.")
           
    ),
    column(width = 9,
           box(
             plotOutput(outputId = "most_abundant_clone_plot"),
             tags$hr(),
             downloadButton(outputId = 'most_abundant_clone_downloadData',
                            label = 'Download data (csv)'),
             downloadButton(outputId = 'most_abundant_clone_downloadPlot',
                            label = 'Download figure(pdf)'),
             width = NULL, height=NULL,  collapsible = F,title = " ", status = "primary", solidHeader = TRUE)
           
    )
  )
)


most_abundant_clone_plot <- reactive({
  spn <- input$most_abundant_clone_samplenames
  filenames <- hot_to_r(input$showSampleInfo)[hot_to_r(input$showSampleInfo)$SampleName %in% spn,"FilesName"]
  if(input$AA_or_NT_most_abundant_clone == "AA") in_counts <- aa_mx()
  else in_counts <- nt_mx()
  tmpdf <- as.data.frame(t(in_counts))[,filenames]
  
  if (length(spn) == 1){
    tmp <- data.frame(sp=tmpdf)
    rownames(tmp) <- rownames(t(in_counts))
    tmpdf <- tmp
  }
  colnames(tmpdf) <- spn
  df1 <- cbind(data.frame(seqnm=rownames(tmpdf)), tmpdf)
  df_melt <- reshape2::melt(df1, id="seqnm")  
  colnames(df_melt) <- c("seqnm", "sample", "counts")
  vec <- c(1,10,100,1000,10000,1000000, Inf)   
  # labels <- zoo::rollapply(vec, width = 2, by = 1, function(x) paste0("[",x[1],", ",x[2],")"))
  labels <- c("[1，10)", "[10，100)", "[100，1000)", "[1000，10000)", ">=10000")
  df_melt$q <- findInterval(df_melt$counts, vec = vec, all.inside = T)
  maxvec <- max(df_melt$q)
  df_melt$`Clone frequency` <- labels[df_melt$q]
  
  tmp <- filter(df_melt, counts>0)
  tb <- table(tmp$sample, tmp$`Clone frequency`)
  df_tb <- as.data.frame(tb)
  df <- reshape2::dcast(df_tb, Var1~Var2, value.var = "Freq")
  rownames(df) <- df[,1]
  df <- df[,-1]
  
  rsum <- rowSums(df)
  rate <- df / rsum

  df_rate <- cbind(sample=rownames(rate),rate)
  df_rate <- reshape2::melt(df_rate, id="sample")
  
  p <- ggplot(df_rate, aes(x=sample, y=value, fill = variable)) + 
    geom_bar(stat = "identity", width = 0.8, color = "grey", size = 0.5) + 
    scale_fill_brewer(palette = "RdYlBu") + theme_bw() + 
    ylab("Occupied homeostatic space, proportion") + xlab("Sample") + 
    theme(text = element_text(size = 15), axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
  return(p)
})


output$most_abundant_clone_plot <- renderPlot({
  most_abundant_clone_plot()
})


output$most_abundant_clone_downloadPlot <- downloadHandler(
  filename = function() { 
    return(paste0(input$AA_or_NT_most_abundant_clone,"_abundant_clone.pdf"))
  },
  content  = function(file) {
    wid = 0.2*length(input$most_abundant_clone_samplenames)+5
    ggsave(file, 
           plot = most_abundant_clone_plot(), width = wid, height = 6, dpi = 300)
  }
)

output$most_abundant_clone_downloadData <- downloadHandler(
  filename = function() { 
    return(paste0(input$AA_or_NT_most_abundant_clone,"_abundant_clone.csv"))
  },
  content  = function(file) {
    spn <- input$most_abundant_clone_samplenames
    filenames <- hot_to_r(input$showSampleInfo)[hot_to_r(input$showSampleInfo)$SampleName %in% spn,"FilesName"]
    if(input$AA_or_NT_most_abundant_clone == "AA") in_counts <- aa_mx()
    else in_counts <- nt_mx()
    tmpdf <- as.data.frame(t(in_counts))[,filenames]
    
    if (length(spn) == 1){
      tmp <- data.frame(sp=tmpdf)
      rownames(tmp) <- rownames(t(in_counts))
      tmpdf <- tmp
    }
    colnames(tmpdf) <- spn
    df1 <- cbind(data.frame(seqnm=rownames(tmpdf)), tmpdf)
    df_melt <- reshape2::melt(df1, id="seqnm")  
    colnames(df_melt) <- c("seqnm", "sample", "counts")
    vec <- c(1,10,100,1000,10000,1000000, Inf)   
    # labels <- zoo::rollapply(vec, width = 2, by = 1, function(x) paste0("[",x[1],", ",x[2],")"))
    labels <- c("[1 - 10)", "[10 - 100)", "[100 - 1000)", "[1000 - 10000)", ">=10000")
    df_melt$q <- findInterval(df_melt$counts, vec = vec, all.inside = T)
    maxvec <- max(df_melt$q)
    df_melt$`Clone frequency` <- labels[df_melt$q]
    
    tmp <- filter(df_melt, counts>0)
    tb <- table(tmp$sample, tmp$`Clone frequency`)
    df_tb <- as.data.frame(tb)
    df <- reshape2::dcast(df_tb, Var1~Var2, value.var = "Freq")
    rownames(df) <- df[,1]
    df <- df[,-1]
    write.csv(df, file = file, quote = F, row.names = T)
  }
)










