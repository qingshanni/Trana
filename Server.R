shinyServer(function(input, output){ 
  for (file in list.files(c(file.path(r_path,"base")),
                          pattern="\\.(r|R)$", full.names = TRUE))
    source(file, encoding = r_encoding, local = TRUE)
  
  
})