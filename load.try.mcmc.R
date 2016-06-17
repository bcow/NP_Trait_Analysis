# load in the mcmc outputs
load.try.mcmc <- function(models,v){
 
  out.names <- c()
   
  for(m in 1:length(models)){
    model <- models[m]
    global <- dir(file.path("output",model), pattern = paste0(model,".Rdata"), full.names = TRUE)
    by.pft <- setdiff(dir(file.path("output",model), full.names = TRUE), global)
    
    # Global
    if(length(global) == 1){
      load(global)
      out.global <- as.data.frame(out$BUGSoutput$sims.list[v])
      name.global <- paste0(model,".global")
      assign(name.global, out.global, envir = .GlobalEnv)
      remove(out)
      
      out.names <- c(out.names, name.global)
    }
    # Grouping
    
    if(length(by.pft) > 0){
      for(i in 1:length(by.pft)){
        load(by.pft[i])
        out.by.pft <- as.data.frame(out$BUGSoutput$sims.list[v])
        name.by.pft <- sprintf("%s.pft.%s",model,tail(unlist(strsplit(by.pft[i],"[.]")),2)[1])
        assign(name.by.pft, out.by.pft, envir = .GlobalEnv)
        remove(out)
        
        out.names <- c(out.names, name.by.pft)
      }
    }
    
    if(length(grep("hier", model)) == 1 & v == "mu_trait" & length(global) == 1){
        load(global)
        out.by.pft <- as.data.frame(as.matrix(as.mcmc(out))) %>% select(., contains("mu_pft_trait"))
        for(i in 1:(ncol(out.by.pft)/5)){
          out.by.pft.new <- select(out.by.pft, contains(sprintf("[%i,",i)))
          name.by.pft.new <- sprintf("%s.pft.%02.f",model,i)
          assign(name.by.pft.new, out.by.pft.new, envir = .GlobalEnv)
          out.names <- c(out.names, name.by.pft.new)
        }
        remove(out)
    }
  }
  return(out.names)
}
