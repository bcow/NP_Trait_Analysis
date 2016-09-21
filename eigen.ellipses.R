library(mvtraits)
library(RColorBrewer)
require(grid)
require(gridExtra)

all_summary <- readRDS("processed_output/summary.rds")

model.type <- "multi"
sigma.name <- "Sigma"
mu.name <- "mu"


pft.biome <- all_summary[model_type == model.type][,c("PFT","Biome"),with=F] %>%
  distinct(.) 
pft.biome <-  right_join(pft.biome,data.table(Biome = unique(pft.biome$Biome),color = c("black",brewer.pal(length(unique(pft.biome$Biome))-1,"Set1"))), by="Biome")
pfts <- pft.biome$PFT

# For testing
# pfts <- pfts[1:3]


BiomeColors <- pft.biome$color 
names(BiomeColors) <- pft.biome$PFT

trait.names <- gsub("log.","",traits)
trait.pairs <- combn(trait.names,2)
i = 1

for(i in 1:length(trait.pairs)){
  
  ctrs <- as.data.frame(matrix(NA,length(pfts),2))
  colnames(ctrs) <- c("x","y")
  rownames(ctrs) <- pfts
  ctrs$shape <- rep(16,length(pfts))
  
  
  s <- 50 
  ells.list <- list()
  eigs.list <- list()
  
  for(j in 1:length(pfts)){
    dt <- all_summary[model_type == model.type][PFT == pfts[j]]
    # Calculate the center
    ctrs[j,1:2] <- dt[var_type == "mu"][trait == trait.pairs[1,i]|trait == trait.pairs[2,i]][,Mean]
    # Calculate the covariance matrix
    cov_matrix <- dt[var_type == sigma.name][PFT == pfts[j]] %>% 
      tab2mat %>% .[trait.pairs[,i], trait.pairs[,i]]
    # Calculate Eigenvectors and Eigenvalues
    evals  <- eigen(cov_matrix)$values
    evecs  <- eigen(cov_matrix)$vectors
    # "Note that when plotting confidence ellipses for data, the ellipse-axes 
    #  are usually scaled to have length = square-root of the corresp eigenvalues, 
    #  and this is what the ellipse function in CARS gives"
    evecs.scaled <- evecs %*% diag(sqrt(evals))
    evacs.use <- evecs.scaled
    eig.points   <- as.data.frame(cbind(rbind(ctrs[j,1] + evecs.scaled[1, ], 
                                              ctrs[j,1] - evecs.scaled[1, ]),
                                        rbind(ctrs[j,2] + evecs.scaled[2, ],
                                              ctrs[j,2] - evecs.scaled[2, ])))
    colnames(eig.points) <- c("x1","x2","y1","y2")
    eigs.list <- append(eigs.list, list(eig.points))
    
    if(!all(eig.points < 100)){
      ctrs$shape[j] <- 8
    }
    
    # Calculate normal ellipse centered at the means, rotated 
    alpha <- atan(evecs[,1][2] / evecs[,1][1])
    theta <- seq(0, 2 * pi, length=(s))
    ell.normal <- 1.96 * cbind(sqrt(evals[1])*cos(theta), sqrt(evals[2])*sin(theta)) # normal ellipse scaled to 1.96
    ell.rotate  <- evecs %*% t(ell.normal)                                    # rotated ellipse
    ell <- as.data.table(t(ell.rotate+unlist(ctrs[j,1:2]))) %>% 
      bind_cols(.,as.data.frame(rep(pfts[j],s))) %>%
      setNames(.,c("x","y","pft"))
    ells.list <- append(ells.list, list(ell))
  }
  
  # ells <- do.call(rbind,ells.list) 
  
  # Plotting 
  
  add_centers <- lapply(1:length(pfts), function(n){
    geom_point(data = ctrs[n,1:2], aes_q(x=as.name("x"),y=as.name("y"), colour=pfts[n]), shape=ctrs[n,3], size=2)
  } )
  
  # add_ellipses <- lapply(1:length(pfts), function(n){
  #   geom_polygon(data = ells.list[[n]], aes_q(x=as.name("x"),y=as.name("y"), colour = pfts[n]), fill = NA)
  # })
  
  add_global_ellipses <- geom_polygon(data = ells.list[[1]], 
                                      aes(x=x,y=y), fill = "black", alpha=.3)
  
  add_eig1 <- lapply(1:length(pfts), function(n){
    geom_line(data = eigs.list[[n]],
              aes_q(x=as.name("x1"),y=as.name("y1"), colour = pfts[n]), size=1)
  })
  
  add_eig2 <- lapply(1:length(pfts), function(n){
    geom_line(data = eigs.list[[n]],
              aes_q(x=as.name("x2"),y=as.name("y2"), colour = pfts[n]), size=.5)
    })
  
  g <- ggplot() + 
    # add_ellipses + 
    add_global_ellipses +
    add_centers + 
    add_eig1 + 
    # add_eig2 +
    labs(title = sprintf("%s vs %s", trait.pairs[1,i],trait.pairs[2,i]),x = trait.pairs[1,i],y = trait.pairs[2,i]) + 
    scale_colour_manual(name = pfts, values = BiomeColors) + 
    xlim(-1,5) + ylim(-5.5,2) +
    theme(legend.position="none")
  plot(g)
  
}