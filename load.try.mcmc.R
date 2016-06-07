# load in the mcmc outputs
load.try.mcmc <- function(models,v){
 
  out.names <- c()
   
  for(m in 1:length(models)){
    model <- models[m]
    pooled <- dir(file.path("output",model), pattern = paste0(model,".Rdata"), full.names = TRUE)
    grouped <- setdiff(dir(file.path("output",model), full.names = TRUE), pooled)
    
    # No grouping
    if(length(pooled) == 1){
      load(pooled)
      out.pooled <- as.data.table(as.matrix(as.mcmc(out)))
      out.pooled <- out.pooled[,grep(v,names(out.pooled)), with=F]
      
      name.pooled <- paste0(model,".pooled")
      assign(name.pooled, out.pooled, envir = .GlobalEnv)
      remove(out)
      
      out.names <- c(out.names, name.pooled)
    }
    # Grouping
    
    if(length(grouped) > 0){
      for(i in 1:length(grouped)){
        load(grouped[i])
        out.grouped <- as.data.table(as.matrix(as.mcmc(out)))
        out.grouped <- out.grouped[,grep(v,names(out.grouped)), with=F]
        
        name.grouped <- paste0(model,".pft.",tail(unlist(strsplit(grouped[i],"[.]")),2)[1])
        assign(name.grouped, out.grouped, envir = .GlobalEnv)
        remove(out)
        
        out.names <- c(out.names, name.grouped)
      }
    }
  }
  return(out.names)
}
