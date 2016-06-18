# Common definitions for scripts
library(data.table)

traits <- c("log.LL", "log.LMA", "log.Nmass", "log.Pmass", "log.Rdmass")
trait.combine <- combn(traits, 2)

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
  pft.biome <- pft.table[,pft.num := sprintf("pft.%02.f",as.numeric(pft.factor))][,list(biome = unique(biome)),pft.num][,biome.num := as.numeric(as.factor(biome))][1:35];setnames(pft.biome,"biome","Biome")
  
  pal <- brewer.pal(length(unique(pft.biome[,Biome])),"Set1")
  
  pft.biome <- rbind(pft.biome, data.table(pft.num = "global", Biome = "global", biome.num = 0))
  
  color.dt <- data.table(Model = unique(models))[,pft.num :=  sapply(X = unique(models), FUN = function(x) tail(unlist(strsplit(x,"\\.na.")),1))]
  color.dt <- merge(color.dt,pft.biome,by = "pft.num", all.x = T)[,Color:= unlist(lapply(biome.num, function(x) ifelse(x == 0,"black", pal[x])))]
  
  return(color.dt[,.(Model,Biome,Color,pft.num)])
}

## Correlation and Covariance Matrices

summarizeSampleMatrix <- function(cov.all.samples, dims, dim.names){
  # Calculate summary statistics across samples
  cov.all.list <- list(Mean = apply(cov.all.samples, dims, mean),
                       SD = apply(cov.all.samples, dims, sd),
                       q025 = apply(cov.all.samples, dims, quantile, 0.025),
                       q500 = apply(cov.all.samples, dims, quantile, 0.500),
                       q975 = apply(cov.all.samples, dims, quantile, 0.975)
  ) %>% lapply("dimnames<-",  dim.names)
  
  trait.combine <- combn(traits, 2)
  getcov <- function(trait, cov.all) cov.all[, trait[1], trait[2]]
  columnize <- function(mat.wide){
    cov.mat <- apply(trait.combine, 2, getcov, mat.wide) %>% as.data.frame
    cov.mat.names <- apply(trait.combine, 2, paste, collapse="_") %>%
      gsub("log.", "", .)
    colnames(cov.mat) <- cov.mat.names
    return(cov.mat)
  }
  
  cov.dat <- lapply(cov.all.list, columnize) %>% 
    do.call(cbind, .) %>%
    add_rownames(var = "PFT") %>%
    gather(Stat.Trait, Value, -PFT) %>%
    separate(Stat.Trait, into=c("Stat", "Trait"), sep="\\.") %>%
    spread(Stat, Value) %>%
    separate(PFT, into=c("Biome", "Function"), sep="_", 
             extra="merge", remove=FALSE)
  
  return(cov.dat)
}
