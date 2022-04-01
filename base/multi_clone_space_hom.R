output$ui_clone_space_hom <- renderUI(
  fluidRow( 
    column(width = 3, 
           wellPanel(
             radioButtons(inputId = "AA_or_NT_clone_space_hom",
                          label = "Chose Type",
                          choices = c("AA","NT")),
             checkboxGroupInput(inputId = "clone_space_hom_samplenames",
                                label = "Multi Sample Chose",
                                choices = hot_to_r(input$showSampleInfo)$SampleName,
                                selected = hot_to_r(input$showSampleInfo)$SampleName)
           ),
           box(width = NULL, height = NULL,
               title = "Description", background = "navy",
               "The proportion of clone types in different frequency ranges in the samples was analyzed by stacking histogram. 
               According to the frequency of clone types, the clone types in the samples were divided into 5 groups (Hyperexpanded, Large, Medium, Small and Rare), 
               and the proportion of clone types in each group was counted.")
           
    ),
    column(width = 9,
           box(
             plotOutput(outputId = "clone_space_hom_plot"),
             tags$hr(),
             downloadButton(outputId = 'clone_space_hom_downloadData',
                            label = 'Download data (csv)'),
             downloadButton(outputId = 'clone_space_hom_downloadPlot',
                            label = 'Download figure(pdf)'),
             width = NULL, height=NULL,  collapsible = F,title = " ", status = "primary", solidHeader = TRUE)
           
    )
  )
)


clone_space_hom_plot <- reactive({
  spn <- input$clone_space_hom_samplenames
  filenames <- hot_to_r(input$showSampleInfo)[hot_to_r(input$showSampleInfo)$SampleName %in% spn,"FilesName"]
  if(input$AA_or_NT_clone_space_hom == "AA") in_counts <- aa_mx()
  else in_counts <- nt_mx()
  tmpdf <- as.data.frame(t(in_counts))[,filenames]
  
  if (length(spn) == 1){
    tmp <- data.frame(sp=tmpdf)
    rownames(tmp) <- rownames(t(in_counts))
    tmpdf <- tmp
  }
  colnames(tmpdf) <- spn
  sum_counts <- colSums(tmpdf)
  df_freq <- tmpdf / sum_counts
  df1 <- cbind(data.frame(seqnm=rownames(df_freq)), df_freq)
  df_melt <- reshape2::melt(df1, id="seqnm") 
  
  colnames(df_melt) <- c("seqnm", "sample", "freq")
  
  vec <- c(0,1e-5,1e-4,0.001,0.01,1)
  labels <- zoo::rollapply(vec, width = 2, by = 1, function(x) paste0("(",x[1],", ",x[2],"]"))
  labels <- paste0(c("Rare ","Small ","Medium ","Large ","Hyperexpanded "), labels)
  df_melt$q <- findInterval(df_melt$freq, vec = vec, left.open = T, all.inside = T)
  df_melt$`Clone frequency` <- labels[df_melt$q]
  
  tmp <- filter(df_melt, freq>0)
  tb <- table(tmp$sample, tmp$`Clone frequency`)
  df_tb <- as.data.frame(tb)
  df <- reshape2::dcast(df_tb, Var1~Var2, value.var = "Freq")
  rownames(df) <- df[,1]
  df <- df[,-1]
  rsum <- rowSums(df)
  rate <- df / rsum
  # write.csv(rate, file = paste0(outpath,"/",cloneType,"_","homeostasis.csv"), quote = F)
  rate <- cbind(sample=rownames(rate),rate)
  df_rate <- reshape2::melt(rate, id="sample")
  
  p <- ggplot(df_rate, aes(x=sample, y=value, fill = variable)) + 
    geom_bar(stat = "identity", width = 0.8, color = "grey", size = 0.5) + 
    scale_fill_brewer(palette = "RdYlBu") + theme_bw() + 
    ylab("Occupied homeostatic space, proportion") + xlab("Sample") + 
    theme(text = element_text(size = 15), axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
  return(p)
})


output$clone_space_hom_plot <- renderPlot({
  clone_space_hom_plot()
})


output$clone_space_hom_downloadPlot <- downloadHandler(
  filename = function() { 
    return(paste0(input$AA_or_NT_clone_space_hom,"_homeostasis.pdf"))
  },
  content  = function(file) {
    wid = 0.2*length(input$clone_space_hom_samplenames)+5
    ggsave(file, 
           plot = clone_space_hom_plot(), width = wid, height = 6, dpi = 300)
  }
)

output$clone_space_hom_downloadData <- downloadHandler(
  filename = function() { 
    return(paste0(input$AA_or_NT_clone_space_hom,"_homeostasis.csv"))
  },
  content  = function(file) {
    spn <- input$clone_space_hom_samplenames
    filenames <- hot_to_r(input$showSampleInfo)[hot_to_r(input$showSampleInfo)$SampleName %in% spn,"FilesName"]
    if(input$AA_or_NT_clone_space_hom == "AA") in_counts <- aa_mx()
    else in_counts <- nt_mx()
    tmpdf <- as.data.frame(t(in_counts))[,filenames]
    
    if (length(spn) == 1){
      tmp <- data.frame(sp=tmpdf)
      rownames(tmp) <- rownames(t(in_counts))
      colnames(tmp) <- filenames
      tmpdf <- tmp
    }
    colnames(tmpdf) <- spn
    sum_counts <- colSums(tmpdf)
    df_freq <- tmpdf / sum_counts
    df1 <- cbind(data.frame(seqnm=rownames(df_freq)), df_freq)
    df_melt <- reshape2::melt(df1, id="seqnm") 
    
    colnames(df_melt) <- c("seqnm", "sample", "freq")
    
    vec <- c(0,1e-5,1e-4,0.001,0.01,1)
    labels <- zoo::rollapply(vec, width = 2, by = 1, function(x) paste0("(",x[1]," - ",x[2],"]"))
    labels <- paste0(c("Rare ","Small ","Medium ","Large ","Hyperexpanded "), labels)
    df_melt$q <- findInterval(df_melt$freq, vec = vec, left.open = T, all.inside = T)
    df_melt$`Clone frequency` <- labels[df_melt$q]
    
    tmp <- filter(df_melt, freq>0)
    tb <- table(tmp$sample, tmp$`Clone frequency`)
    df_tb <- as.data.frame(tb)
    df <- reshape2::dcast(df_tb, Var1~Var2, value.var = "Freq")
    rownames(df) <- df[,1]
    df <- df[,-1]
    rsum <- rowSums(df)
    rate <- df / rsum
    write.csv(rate, file = file, quote = F, row.names = T)
  }
)















