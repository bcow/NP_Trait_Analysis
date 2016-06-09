# Common definitions for scripts
library(data.table)

traits <- c("log.LL", "log.LMA", "log.Nmass", "log.Pmass", "log.Rdmass")

is.error <- function(x) class(x) == "try-error"

replace.with.global <- function(x, global){
    nas <- which(is.na(x) | is.nan(x))
    x[nas] <- global[nas]
    return(x)
}

convert.rownames <- function(dat){
    pattern <- "(.*)\\.na\\..*"
    dat$Model <- gsub(pattern, "\\1", rownames(dat))
    rownames(dat) <- NULL
    return(dat)
}

array2DApply <- function(arr, func, ...){
    arr.dim <- dim(arr)
    out <- array(NA, arr.dim)
    for(i in 1:arr.dim[1]) out[i,,] <- func(arr[i,,], ...)
    return(out)
}

array3Dapply <- function(arr, func, ...){
    out <- array(NA, dim(arr))
    for(i in 1:dim(out)[1]){
        for(j in 1:dim(out)[2]){
            out[i,j,,] <- func(arr[i,j,,], ...)
        }
    }
    return(out)
}

try.pfts <- readRDS("data/try.pft.table.rds")
pft.factor <- try.pfts$pft.factor
pft.names <- levels(pft.factor)
pft.names[pft.names == "CAM"] <- "arid_CAM"
npft <- length(pft.names)


# Generate biome colors for plots
generate.colors <- function(models){
  
  library(RColorBrewer)
  require(data.table)
  
  pft.table <- readRDS("data/try.pft.table.rds")
  pft.biome <- pft.table[,pft.num := sprintf("%02.f",as.numeric(pft.factor))][,list(biome = unique(biome)),pft.num][,biome.num := as.numeric(as.factor(biome))][1:35];setnames(pft.biome,"biome","Biome")
  
  pal <- brewer.pal(length(unique(pft.biome[,Biome])),"Set1")
  
  pft.biome <- rbind(pft.biome, data.table(pft.num = "global", Biome = "global", biome.num = 0))
  
  
  color.dt <- data.table(Model = unique(models))[,pft.num := unlist(lapply(Model, function(x) tail(unlist(strsplit(x,"[.]")),1)))]
  color.dt <- merge(color.dt,pft.biome,by = "pft.num", all.x = T)[,Color:= unlist(lapply(biome.num, function(x) ifelse(x == 0,"black", pal[x])))]
  
  return(color.dt[,.(Model,Biome,Color,pft.num)])
}


