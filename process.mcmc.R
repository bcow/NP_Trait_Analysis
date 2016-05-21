library(rjags)
library(coda)
library(mvtnorm)
library(data.table)

source("load.try.data.R")
source("load.try.mcmc.R")

traits <- colnames(try)
uni <- load.try.mcmc("uni",traits)
multi <- load.try.mcmc("multi",traits)
pft <- load.try.mcmc("pft",traits)

out.names <- c(uni,multi,pft)


mus <- as.data.frame(matrix(NA, length(out.names)+1, length(traits)))
mus[1,] <- colMeans(try)
for(i in 1:length(out.names)){
  for(j in 1:length(traits)){
    mus[i+1,j] <- mean(get(out.names[i])[traits[j]][,1])
  }
}
colnames(mus) <- traits
rownames(mus) <- c("Data",out.names)
print(mus)

outs <- list()
for(n in out.names){
  outs[[n]] = get(n)
}

iter <- dim(outs[[1]])[1]

a <- sort(try_full$pft[!is.na(try_full$pft)],index.return = T)
splits <- split(try.na[a$ix,], a$x)

trait_means <- colMeans(try)
pft_means <- as.data.frame(matrix(NA, length(splits), length(traits)))
colnames(pft_means) <- traits

dim(pft_means)
for(i in 1:length(splits)){
  pft_means[i,] <- colMeans(splits[[i]],na.rm=T)
  for(j in 1:length(traits)){
    pft_means[i,traits[j]] <- ifelse(is.nan(pft_means[i,traits[j]]),trait_means[j],pft_means[i,traits[j]])
  }
}

save(outs, mus, pft_means, traits, iter, file = 'output/try.outs.Rdata')

#########################
## Sanity check
# 
# y <- outs[[5]]$log.LL 
# 
# plot(y[1:15000], type = "l")
# lines(y[15001:30000],col=2)
# lines(outs[[5]]$log.LL[30001:45000],col=3)
# lines(outs[[5]]$log.LL[45001:60000],col=4)
# lines(outs[[5]]$log.LL[60001:75000],col=5)
