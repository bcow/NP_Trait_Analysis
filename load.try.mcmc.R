load.try.mcmc <- function(model,traits){
  
  out_files <- dir("output/", pattern = paste0("try.",model,".*.outputs"), full.names = TRUE )
  
  load(paste0("output/try.",model,".outputs.c1.Rdata"))
  
  if(exists(paste0("out.",model,".try"))){
    out.mcmc <- eval(parse(text = paste0("out.",model,".try")))
    out.df <- as.data.frame(as.matrix(out.mcmc))
    out.df <- out.df[,grep("mu", colnames(out.df))]
  }
  
  if(exists(paste0("out.",model,".try.na"))){
    out.na.mcmc <- eval(parse(text = paste0("out.",model,".try.na")))
    out.na.df <- as.data.frame(as.matrix(out.na.mcmc))
    out.na.df <- out.na.df[,grep("mu", colnames(out.na.df))]
  }
  
  if(length(out_files) > 1){
    for(i in 2:length(out_files)){
      load(out_files[i])
      if(exists(paste0("out.",model,".try"))){
        out.new <- as.data.frame(as.matrix(
          eval(parse(text = paste0("out.",model,".try")))))
        out.df <- rbind(out.df, out.new[,grep("mu", colnames(out.new))])
      }
      if(exists(paste0("out.",model,".try.na"))){
        out.na.new <- as.data.frame(as.matrix(
          eval(parse(text = paste0("out.",model,".try.na")))))
        out.na.df <- rbind(out.na.df, out.na.new[,grep("mu", colnames(out.na.new))])
      }
      suppressWarnings(remove(out.new, out.na.new))
    }
  }
  vars <- c()
  if(exists("out.mcmc")){
    colnames(out.df) <- traits
    assign(x = paste0("out.",model), value = out.df , envir = .GlobalEnv)
    vars <- c(vars, paste0("out.",model))}
  if(exists("out.na.mcmc")){
    colnames(out.na.df) <- traits
    assign(x = paste0("out.",model,".na"), value = out.na.df , envir = .GlobalEnv)
    vars <- c(vars, paste0("out.",model,".na"))}
  return(vars)
}

