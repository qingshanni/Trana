output$ui_high_freq_clone_track <- renderUI(
  fluidRow( 
    column(width = 3, 
           wellPanel(
             radioButtons(inputId = "AA_or_NT_high_freq_clone_track",
                          label = "Chose Type",
                          choices = c("AA","NT")),
             selectInput(inputId = "high_freq_clone_track_samplename",
                                label = "Single Sample Chose",
                                choices = hot_to_r(input$showSampleInfo)$SampleName,
                                selected = hot_to_r(input$showSampleInfo)$SampleName),
             sliderInput(inputId = "top_clone",
                         label   = "Top Clones",
                         min = 10,
                         max = 50,
                         value = 20,
                         step = 10),
             sliderInput(inputId = "transparency",
                         label = "Dot Transparency",
                         min = 0.1,
                         max = 1,
                         value = 0.7,
                         step = 0.1),
             sliderInput(inputId = "max_clones",
                         label = "MAX Clone Number",
                         min = 500,
                         max = 5000,
                         value = 800,
                         step = 500),
             textInput(inputId = "dot_color",
                       label = "Dot Colour",
                       value = "#336699",
                       placeholder = "color or RGB(#000000)")
           ),
           box(width = NULL, height = NULL,
               title = "Description", background = "navy",
               "The frequency distribution of high-frequency clones (Top N) in each sample was analyzed and the bubble map was drawn.")
           
    ),
    column(width = 8,
           box(
             plotOutput(outputId = "high_freq_clone_track_plot"),
             tags$hr(),
             downloadButton(outputId = 'high_freq_clone_track_downloadData',
                            label = 'Download data (csv)'),
             downloadButton(outputId = 'high_freq_clone_track_downloadPlot',
                            label = 'Download figure(pdf)'),
             width = NULL, height=NULL,  collapsible = F,title = " ", status = "primary", solidHeader = TRUE),
           box(title = "Description", width = 9, background = "light-blue",
               h4("Analysing the frequency distribution of high-frequency clone (Top N) in other samples. 
                  The purpose is to help users understand the frequency distribution of the first n high frequency clones in a given sample in other samples."))
           
    )
  )
)



high_freq_clone_track_plot <- reactive({
  topn <- input$top_clone
  color <- input$dot_color
  alpha <- input$transparency
  
  maxNum <- input$max_clones
  size <- 10
  
  spname <- input$high_freq_clone_track_samplename
  # filename <- hot_to_r(input$showSampleInfo)[hot_to_r(input$showSampleInfo)$SampleName %in% spn,"FilesName"]
  if(input$AA_or_NT_high_freq_clone_track == "AA") {
    in_counts <- aa_mx()
  }
  else in_counts <- nt_mx()
  
  # tmpdf <- as.data.frame(t(in_counts))[,filename]
  spns <- hot_to_r(input$showSampleInfo)$SampleName
  rownames(in_counts) <- spns
  idx <- order(in_counts[spname,],decreasing = T)
  mxt <- in_counts[,idx[1:topn]]
  # write.csv(t(mxt),file = file.path(out_path,paste0(type,'_Clone_share_',spname[i],'.csv')),quote = F)
  
  mxt[mxt > maxNum] <- maxNum
  
  df <- data.frame(sample=rep(spns,each=topn),
                   nt = rep(colnames(mxt),nrow(in_counts)),
                   no=rep(1:topn,nrow(in_counts)),
                   num=as.vector(t(mxt)),
                   stringsAsFactors = F)
  
  p <-ggplot(df,aes(x=sample,y=no))+
    geom_point(aes(size=num),alpha=alpha,color=color)+
    scale_size(range=c(1,size),limits=c(1,maxNum))+
    scale_color_brewer(palette = "Accent")+scale_y_reverse() +
    theme_bw()+
    theme(axis.text.x = element_text(angle=30, hjust=1))
  return(p)
})



output$high_freq_clone_track_plot <- renderPlot({
  high_freq_clone_track_plot()
})


output$high_freq_clone_track_downloadPlot <- downloadHandler(
  filename = function() { 
    return(paste0(input$high_freq_clone_track_samplename,"_",input$AA_or_NT_high_freq_clone_track,"_high_freq_clone_track.pdf"))
  },
  content  = function(file) {
    wid <- length(input$high_freq_clone_track_samplename)/10 + 4
    ht <- input$top_clone/4
    ### wid过大  会报错
    ggsave(file, 
           plot = high_freq_clone_track_plot(), width = wid, height = ht, dpi = 300)
  }
)

output$high_freq_clone_track_downloadData <- downloadHandler(
  filename = function() {
    return(paste0(input$high_freq_clone_track_samplename,"_",input$AA_or_NT_high_freq_clone_track,"_high_freq_clone_track.csv"))
  },
  content  = function(file) {
    spname <- input$high_freq_clone_track_samplename
    if(input$AA_or_NT_high_freq_clone_track == "AA") in_counts <- aa_mx()
    else in_counts <- nt_mx()
    spns <- hot_to_r(input$showSampleInfo)$SampleName
    rownames(in_counts) <- spns
    idx <- order(in_counts[spname,],decreasing = T)
    mxt <- in_counts[,idx[1:topn]]
    write.csv(mxt, file = file, quote = F, row.names = F)
  }
)






























