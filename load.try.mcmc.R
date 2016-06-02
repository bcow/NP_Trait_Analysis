# load in the mcmc outputs

load.try.mcmc <- function(model,traits,var){
  
  files <- dir("output/", pattern = paste0(model,".Rdata"), full.names = TRUE )
  files_na <- dir("output/", pattern = paste0(model,".na.Rdata"), full.names = TRUE )
  
######### Without NA's ######### 
  
  if(length(files) > 0){
    load(files[1])
    out.mcmc <- as.mcmc(out)
    out.df <- as.data.frame(as.matrix(out.mcmc))
    out.df <- out.df[,grep(var, colnames(out.df))]
    remove(out)
  }
  
  if(length(files) > 1){
    for(i in 2:length(files)){
      load(files[i])
      out.new <- as.data.frame(as.matrix(out))
      out.df <- rbind(out.df, out.new[,grep(var, colnames(out.new))])
      remove(out.new)
    }
  }
  
  
  ######### With NA's #########   
  
  if(length(files_na) > 0){
    load(files_na[1])
    out.na.mcmc <- as.mcmc(out)
    out.na.df <- as.data.frame(as.matrix(out.na.mcmc))
    out.na.df <- out.na.df[,grep(var, colnames(out.na.df))]
    remove(out)
  }

  if(length(files_na) > 1){
    for(i in 2:length(files_na)){
      load(files_na[i])
      out.na.new <- as.data.frame(as.matrix(out))
      out.na.df <- rbind(out.na.df, out.na.new[,grep(var, colnames(out.na.new))])
      remove(out.na.new)
    }
  }

  #############################  
  
  vars <- c()
  if(exists("out.mcmc")){
    assign(x = paste(var,model,sep="."), value = out.df , envir = .GlobalEnv)
    vars <- c(vars, paste(var,model,sep="."))}
  if(exists("out.na.mcmc")){
    assign(x = paste(var,model,"na",sep="."), value = out.na.df , envir = .GlobalEnv)
    vars <- c(vars, paste(var,model,"na",sep="."))}
  return(vars)
}

