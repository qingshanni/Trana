files_to_immlist <- function(fns, SampleName){
  ### 读取immunarch文件
  dd_list <- lapply(fns,function(fn){
    dd <- read.table(fn, header = T)
  })
  
  ### 处理样本名
  # sp_name <- sapply(filesName, function(fn){
  #   tmp <- strsplit(fn,"\\.")
  #   name <- paste(tmp[[1]][1:length(tmp[[1]])-1], collapse = ".")
  # })
  ###############
  names(dd_list) <- SampleName
  dd_list
}


### 将immlist转化为mx
immlist_to_mx <- function(immlist, type="nt"){
  ### 将imm格式转换为可用于转换为mx的格式
  res <- lapply(immlist, function(dd){
    Clones <- dd$Clones
    if(type=="nt"){
      names(Clones) <- dd$CDR3.nt
    }
    else if(type=="aa"){
      names(Clones) <- dd$CDR3.aa
      ### 将重复的aa序列合并
      tmp_df <- aggregate(Clones, by=list(CDR3.aa=names(Clones)), sum)
      colnames(tmp_df) <- c("CDR3.aa", "Clones")
      Clones <- tmp_df$Clones
      names(Clones) <- tmp_df$CDR3.aa
    }
    Clones
  })
  
  ### 将list转换为mx
  s <- unique(unlist(lapply(res,names)))
  ns <- length(s)
  nl <- length(res)
  mx <- matrix(rep(0,ns*nl),nl,ns)
  colnames(mx) <- s
  rownames(mx) <- names(res)
  
  for(i in 1:nl){
    mx[i,names(res[[i]])] <- res[[i]] 
  }
  #####################
  return(mx)
}


theme_change2 <- function(ticklable = 8, titlelabel = 12, strip = 10, label = 10, xrot = 90, yrot = 90, legend.just = c(1, 1), panel_distance = 0.001) {
  theme_change <- theme_bw() + 
    theme(axis.text.x = element_text(face = "bold", size = ticklable,angle = xrot, hjust = 0.5, vjust = 0.5), 
          axis.text.y = element_text(face = "bold", size = ticklable,angle = yrot, hjust = 0.5, vjust = 0.5), 
          plot.title = element_text(face = "bold", size = titlelabel), 
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
          strip.background = element_blank(), 
          strip.text.y = element_text(face = "bold", size = strip), 
          strip.text.x = element_text(face = "bold", size = strip), 
          axis.title.x = element_text(face = "bold", size = label), 
          axis.title.y = element_text(face = "bold", size = label), 
          legend.background = element_rect(colour = "transparent",fill = "transparent"), 
          legend.justification = legend.just, 
          legend.position = legend.just, 
          legend.text = element_text(size = 4), 
          text = element_text(face = "bold", size = label), 
          legend.title = element_blank(), legend.key = element_blank(), 
          panel.spacing = unit(panel_distance, "lines"))
  return(theme_change)
}


plotVJcircos <- function(mtx) {
  O1 <- sort(rowSums(mtx, na.rm = T))
  O2 <- sort(colSums(mtx, na.rm = T))
  mtx <- mtx[names(O1), names(O2)]
  
  # sector colors
  rcols <- rep(brewer.pal(9, "Pastel1"), times = nrow(mtx)/9 + 1)[1:nrow(mtx)]
  ccols <- rep(brewer.pal(9, "Pastel1"), ncol(mtx)/9 + 1)[1:ncol(mtx)]
  names(rcols) <- rownames(mtx)
  names(ccols) <- colnames(mtx)
  # starting angle
  circos.par(start.degree = 30) 
  # plot sectors
  chordDiagram(mtx, annotationTrack = "grid", grid.col = c(rcols, ccols),small.gap = 2, big.gap = 10, grid.border = "grey",
               preAllocateTracks = list(track.height = 0.1))
  # add label
  circos.trackPlotRegion(track.index = 1, bg.border = NA,
                         panel.fun = function(x, y) {
                           sector.name = get.cell.meta.data("sector.index")
                           xlim = get.cell.meta.data("xlim")
                           ylim = get.cell.meta.data("ylim")
                           circos.text(mean(xlim), ylim[1], cex = 0.5, sector.name, facing = "clockwise", adj = c(0, 0.5))
                         })
  circos.clear()
}


snailplot <- function(df) {
  df$g <- ifelse(df$Clones>=3,"3+",df$Clones)
  df$freq <- df$Clones/sum(df$Clones)
  df1 <- ddply(df, .(g), summarise, freq = sum(freq))
  df1 <- df1[match(c("3+","2","1"),df1$g),]
  df1$vmax <- cumsum(df1$freq)
  df1$vmin <- df1$vmax-df1$freq
  
  df2 <- df[df$g=="3+",]
  df2$g2 <- findInterval(-df2$freq, quantile(-df2$freq, probs = seq(0,1,0.2), 3), rightmost.closed = T, all.inside = T)
  df2 <- ddply(df2, .(g2), summarise, freq = sum(freq))
  df2$g2 <- paste0("Q",df2$g2)
  df2$x <- 2
  df2$vmax <- cumsum(df2$freq)
  df2$vmin <- df2$vmax-df2$freq
  
  df3 <- df[order(-df$freq),]
  df3 <- df3[1:5,]
  df3$x <- 3
  df3$vmax <- cumsum(df3$freq)
  df3$vmin <- df3$vmax-df3$freq
  
  O <- c("1","2","3+","Q1","Q2","Q3","Q4","Q5",df3$`Sequence`)
  df1 <- within(df1, g <- factor(g, levels = O))
  df2 <- within(df2, g2 <- factor(g2, levels = O))
  df3 <- within(df3, `Sequence` <- factor(`Sequence`, levels = O))
  p <- ggplot() +
    # geom_rect(data = df1, colour="grey75", size = 0.5, aes(fill=g, ymax=vmax, ymin=vmin, xmax=2,xmin=1)) +
    geom_rect(data = df1, colour="grey75", size = 0.5, aes(fill=g,  ymax=vmax, ymin=vmin, xmax=2,xmin=1)) +
    geom_rect(data = df2, colour="grey75", size = 0.5, aes(fill=g2, ymax=vmax, ymin=vmin, xmax=4,xmin=2)) +
    geom_rect(data = df3, colour="grey75", size = 0.5, aes(fill=`Sequence`, ymax=vmax, ymin=vmin, xmax=8,xmin=4)) +
    # here we also rotate text to point to right direction
    geom_text(data = df1, aes(x=1.5, y = vmax - freq/2, angle=180-..y..*360, label = g), size=3)+
    geom_text(data = df2, aes(x=3.0, y = vmax - freq/2, angle=90-..y..*360, label = g2), size=3)+
    geom_text(data = df3, aes(x=4, y = vmax - freq/2, angle=90-..y..*360, label = `Sequence`, size = freq), hjust=0) +
    coord_polar(theta="y") +
    scale_x_continuous(expand=c(0,0),limits=c(0, 8.5)) +
    scale_size_continuous(range=c(1, 3)) +
    xlab("") + ylab("")+
    theme_bw() +
    theme(panel.grid=element_blank(),panel.border=element_blank(),axis.text=element_blank(),
          axis.ticks=element_blank(),legend.position="none",  plot.margin = margin(1, 1, -2, -2, "cm"))
  return(p)
}


tcr_diversity <- function(dd,method="shan",q=2){
  v <-   switch(method,
                shan  =  { md <- dd / sum(dd)
                -sum(md * log(md)) },
                smp  =  { md <- dd / sum(dd)
                sum(md^2) },
                ismp =  { md <- dd / sum(dd)
                1 / sum(md^2)   },
                gsi  =  { md <- dd / sum(dd)
                1 - sum(md^2)  },
                bpi  =  max(dd) / sum(dd) ,
                renyi=  {  md <- dd / sum(dd)
                log(sum(md^q))/(1-q) }
  )
}


myimmunarch2vectorlist <- function(datList,type='NT'){
  ddlist <- lapply(datList,function(dd){
    if(type == 'NT'){
      seq <- dd$CDR3.nt
    }else if(type == 'AA' ){
      seq <- dd$CDR3.aa
    }else if(type == 'VJ'){
      seq <-  paste(dd$V.name,dd$J.name,sep='_')
    }else{
      return( NULL)
    }
    
    clones <- aggregate(dd$Clones, by=list(seq),FUN=sum,na.rm=TRUE)
    v <- clones[,2]
    names(v) <- clones[,1]
    v <- sort(v,decreasing = T)
    
  })
}

list_vector_to_mx <- function(lv){
  s <- unique(unlist(lapply(lv,names)))
  ns <- length(s)
  nl <- length(lv)
  mx <- matrix(rep(0,ns*nl),nl,ns)
  colnames(mx) <- s
  rownames(mx) <- names(lv)
  
  for(i in 1:nl){
    mx[i,names(lv[[i]])] <- lv[[i]] 
  }
  mx
}

