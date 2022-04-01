output$ui_share_clone_distr <- renderUI(
  fluidRow( 
    column(width = 3, 
           wellPanel(
             radioButtons(inputId = "AA_or_NT_share_clone_distr",
                          label = "Chose Type",
                          choices = c("AA","NT")),
             checkboxGroupInput(inputId = "share_clone_distr_samplenames",
                                label = "Multi Sample Chose",
                                choices = hot_to_r(input$showSampleInfo)$SampleName,
                                selected = hot_to_r(input$showSampleInfo)$SampleName)
           ),
           box(width = NULL, height = NULL,
               title = "Description", background = "navy",
               "The Number of cells per clonotype in each sample was counted, and the Number distribution of shared clones among samples was analyzed by Upset graph. 
               In the Upset graph, the Y-axis represents the sample, the X-axis represents the number of unique clones in the sample (pink), 
               the lower right represents the different sample combinations (if there is a sample in the combination, the sample is shown in orange), 
               and the height of the upper right bar represents the number of clones shared between samples.")
           
    ),
    column(width = 9,
           box(
             plotOutput(outputId = "share_clone_distr_plot"),
             tags$hr(),
             # downloadButton(outputId = 'share_clone_distr_downloadData',
             #                label = 'Download data (csv)'),
             downloadButton(outputId = 'share_clone_distr_downloadPlot',
                            label = 'Download figure(pdf)'),
             width = NULL, height=NULL,  collapsible = F,title = " ", status = "primary", solidHeader = TRUE)
           
    )
  )
)


share_clone_distr_plot <- reactive({
  spn <- input$share_clone_distr_samplenames
  filenames <- hot_to_r(input$showSampleInfo)[hot_to_r(input$showSampleInfo)$SampleName %in% spn,"FilesName"]
  if(input$AA_or_NT_share_clone_distr == "AA") in_counts <- aa_mx()
  else in_counts <- nt_mx()
  tmpdf <- as.data.frame(t(in_counts))[,filenames]
  
  # if (length(spn) < 2){
  #   print("样本必须大于二！！！")
  # }
  colnames(tmpdf) <- spn
  tmpdf[tmpdf>0] <- 1
  tmp <- cbind(data.frame(seqnm=rownames(tmpdf)), tmpdf)
  rownames(tmpdf) <- 1:nrow(tmpdf)
  nr <- ncol(tmpdf)
  # nc <- nrow(unique(tmpdf[,2:ncol(tmpdf)]))
  r <- min(0.7, (0.3+0.01*nr))
  
  p <- upset(tmp, nsets = nr, nintersects = 50, 
             mb.ratio = c(1-r, r),order.by = c("freq"), decreasing = c(TRUE), 
             matrix.color = "#FDB462",  sets.bar.color = "#FBB4AE", 
             point.size = 3, mainbar.y.label = "No. of intersected clonotypes (Log10)", 
             sets.x.label = "No. of clonotypes", scale.intersections = "log10", show.numbers = FALSE,
             )
  return(p)
})


output$share_clone_distr_plot <- renderPlot({
  share_clone_distr_plot()
})


output$share_clone_distr_downloadPlot <- downloadHandler(
  filename = function() {
    return(paste0(input$AA_or_NT_share_clone_distr,"_share_clone_distr.pdf"))
  },
  content  = function(file) {
    wid <- 10
    ht <- 6
    p <- share_clone_distr_plot()
    pdf(file=file, onefile=T, width = wid, height = ht) # or other device
    p
    dev.off()
    }
)



# output$share_clone_distr_downloadData <- downloadHandler(
#   filename = function() { 
#     return(paste0(input$AA_or_NT_share_clone_distr,"_share_clone_distr.csv"))
#   },
#   content  = function(file) {
#     
#     write.csv(mtx, file = file, quote = F, row.names = T)
#   }
# )



