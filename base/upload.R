output$ui_upload <- renderUI(
  fluidRow( 
    
    column(width = 3, 
           wellPanel(fileInput(inputId = 'uploadfiles', 
                               label = h5('Choose data'),
                               accept = c('text/csv'),
                               multiple = T))
    ),
    column(width = 9,
           box(rHandsontableOutput("showSampleInfo"),
                                  actionButton(inputId = "delete", label = "Delete", icon = icon("refresh")),
                                  width = NULL,  collapsible = F, title = "Data", status = "primary", solidHeader = TRUE)
    )
  )
)

### save global var
values <- reactiveValues()
values[["files"]] <- data.frame()
upload <- eventReactive(input$uploadfiles, {
  for(i in 1:nrow(input$uploadfiles)){
    ### 如何传入文件为非法文件则自动去掉
    
  }
  for(i in 1:nrow(input$uploadfiles)){
    if(input$uploadfiles[i,]$name %in% values[["files"]]$name) next()
    else values[["files"]] <- rbind(values[["files"]], input$uploadfiles[i,])
  }
})


output$showSampleInfo <- renderRHandsontable({
  req(input$uploadfiles)
  ### 统计克隆型
  upload()
  immlist <- immlist()
  res <- lapply(immlist, function(dd){
    total_clones <- sum(dd$Clones)
    nt_types <- nrow(dd)
    aa <- aggregate(dd$Clones,list(CDR3.aa=dd$CDR3.aa),sum)
    aa_types <- nrow(aa)
    clone_max <- max(dd$Clones)
    clone_min <- min(dd$Clones)
    clone_avg <- round(sum(dd$Clones)/nrow(dd),1)
    data.frame(total_clones=total_clones, nt_types=nt_types, aa_types=aa_types, clone_max=clone_max, clone_min=clone_min, clone_avg=clone_avg)
  })
  tmp <- do.call(rbind, res)

  
  df <- data.frame(Select     = rep(F, nrow(values[["files"]])),
                   FilesName  = values[["files"]]$name,
                   SampleName = values[["files"]]$name,
                   TotalCloneNum = tmp$total_clones,
                   NT_ClonoType = tmp$nt_types,
                   AA_ClonoType = tmp$aa_types,
                   CloneMax     = tmp$clone_max,
                   CloneMin     = tmp$clone_min,
                   CloneAvg     = tmp$clone_avg)
  
  if(input$delete){
    
    df <- deletesample()
    rownames(df) <- 1:nrow(df)
  }
  
  rhandsontable(df) %>% hot_col(c("FilesName", "TotalCloneNum", "NT_ClonoType", "AA_ClonoType", "CloneMax", "CloneMin", "CloneAvg"), readOnly = T)
})


deletesample <- eventReactive(input$delete,{
  df <- hot_to_r(input$showSampleInfo)
  values[["files"]] <- values[["files"]][!df$Select, ]
  df <- df[!df$Select,]
})


immlist <- reactive({
  files_to_immlist(values[["files"]]$datapath, values[["files"]]$name)
})


nt_mx <- reactive({
  immlist_to_mx(immlist(), type="nt")
})

aa_mx <- reactive({
  immlist_to_mx(immlist(), type="aa")
})
  
  
  
  