eigen.ellipses <- function(model.type,sigma.name,mu.name){
  # Two setups: 
  # eigen.ellipses("multi","Sigma","mu")
  # eigen.ellipses("hier","Sigma_pft","mu_pft")
  
  library(mvtraits)
  library(RColorBrewer)
  require(grid)
  require(gridExtra)
  ##################################
  # Global Theme for plots
  global_theme <- theme_bw() + 
    theme(text = element_text(size = 25),
          title = element_blank(),
          axis.text = element_text(size = rel(0.3)),
          legend.title = element_text(size = rel(1)),
          legend.text = element_text(size = rel(0.6)),
          legend.direction = "horizontal"
          # axis.text.y = element_blank(),
          # axis.ticks.y = element_blank()
    )
  ##################################
  # Hardcoding for setting different x and y limits depending on trait 
  lims <- list()
  lims[["LL"]] <- c(-.5,4.5)
  lims[["LMA"]] <- c(-5.2,1.5)
  lims[["Nmass"]] <- c(1.7,4.1)
  lims[["Pmass"]] <- c(-1.3,1.7)
  lims[["Rdmass"]] <- c(-7.5,1.7)
  ##################################
  
  all_summary <- readRDS("processed_output/summary.rds")
  pft.biome <- all_summary[model_type == model.type][,c("PFT","Biome"),with=F] %>%
    distinct(.) 
  pfts <- pft.biome$PFT
  # For testing
  # pfts <- pfts[1:3]
  
  traits_nolog <- gsub("log.","",traits)
  trait.pairs <- combn(traits_nolog,2)
  p <- list()
  
  ellipse.scale = 1.96 # 95% 
  
  # Setting up plot colors themes for Biomes 
  biome.color <- data.table(Biome = unique(all_summary[,Biome]),color = c(brewer.pal(length(unique(pft.biome$Biome))-1,"Set1"),"black"))
  pft.biome.color <-  right_join(pft.biome,biome.color, by="Biome")
  BiomeColorsPFT <- pft.biome.color$color; names(BiomeColorsPFT) <- pft.biome.color$PFT
  BiomeColors <- biome.color$color; names(BiomeColors) <- biome.color$Biome 
  
  for(i in 1:dim(trait.pairs)[2]){
    
    ctrs <- as.data.frame(matrix(NA,length(pfts),2))
    colnames(ctrs) <- c("x","y")
    rownames(ctrs) <- pfts
    ctrs$shape <- rep(16,length(pfts)) # shape 16 is a point 
    
    ells.list <- list()
    eigs.list <- list()
    
    for(j in 1:length(pfts)){
      dt <- all_summary[model_type == model.type][PFT == pfts[j]]
      # Calculate the center
      if(pfts[j]=="global" & model.type=="hier"){#gross exception
        ctrs[j,1:2] <- dt[var_type == "mu_global"][trait == trait.pairs[1,i]|trait == trait.pairs[2,i]][,Mean]
      }else{
        ctrs[j,1:2] <- dt[var_type == mu.name][trait == trait.pairs[1,i]|trait == trait.pairs[2,i]][,Mean]
      }
      # Calculate the covariance matrix
      if(pfts[j]=="global" & model.type=="hier"){#gross exception
        cov_matrix <- dt[var_type == "Sigma_global"][PFT == pfts[j]] %>% 
          tab2mat %>% .[trait.pairs[,i], trait.pairs[,i]]
      }else{
        cov_matrix <- dt[var_type == sigma.name][PFT == pfts[j]] %>% 
          tab2mat %>% .[trait.pairs[,i], trait.pairs[,i]]
        }
      # Calculate Eigenvectors and Eigenvalues
      evals  <- eigen(cov_matrix)$values
      evecs  <- eigen(cov_matrix)$vectors
      # "Note that when plotting confidence ellipses for data, the ellipse-axes 
      #  are usually scaled to have length = square-root of the corresp eigenvalues"
      evecs.scaled <- evecs %*% diag(sqrt(evals))
      evacs.use <- evecs.scaled
      eig.points   <- as.data.frame(cbind(rbind(ctrs[j,1] + evecs.scaled[1, ], 
                                                ctrs[j,1] - evecs.scaled[1, ]),
                                          rbind(ctrs[j,2] + evecs.scaled[2, ],
                                                ctrs[j,2] - evecs.scaled[2, ])))
      colnames(eig.points) <- c("x1","x2","y1","y2")
      eigs.list[[pfts[j]]] <- eig.points
      
      if(!all(eig.points < 100)){
        ctrs$shape[j] <- 8 # Change point to *
      }
      # Calculate normal ellipse centered at the means, rotated 
      alpha <- atan(evecs[,1][2] / evecs[,1][1])
      theta <- seq(0, 2 * pi, length=(50))
      ell.normal <- ellipse.scale * cbind(sqrt(evals[1])*cos(theta), sqrt(evals[2])*sin(theta)) # normal ellipse scaled to 1.96
      ell.rotate  <- evecs %*% t(ell.normal)                                    # rotated ellipse
      # Either use the ellipse that is calculated by hand
      ell <- as.data.table(t(ell.rotate+unlist(ctrs[j,1:2]))) %>% 
        bind_cols(.,as.data.frame(rep(pfts[j],50))) %>%
        setNames(.,c("x","y","pft"))
      # OR the built in function (I did both to make sure I understood)
      ell.alt <- as.data.frame(ellipse(
        center = c(ctrs[j,1],ctrs[j,2]), 
        shape = cov_matrix, draw = F, radius = ellipse.scale))%>% 
        bind_cols(.,as.data.frame(rep(pfts[j],dim(.)[1]))) %>%
        setNames(.,c("x","y","pft"))
      ells.list[[pfts[j]]] <- ell
    } # End loop over pfts
    
    # Create ggplot elements
    add_centers <- lapply(setdiff(pfts,"global"), function(pft){
      geom_point(data = ctrs[pft,1:2], aes_q(x=as.name("x"),y=as.name("y"), colour=pft), 
                 shape=ctrs[pft,3], size=1)
    } )
    add_ellipses <- lapply(setdiff(pfts,"global"), function(pft){
      geom_polygon(data = ells.list[[pft]], aes_q(x=as.name("x"),y=as.name("y"), colour = pft), fill = NA)
    })
    add_global_ellipse <- geom_polygon(data = ells.list[["global"]],aes(x=x,y=y)
                                       ,fill = "black", alpha=.4)
    add_global_eig <- list(
      geom_line(data = eigs.list[["global"]], aes(x=x1,y=y1,colour ="global"), 
                size=.5, alpha=.6),
      geom_point(data = ctrs["global",1:2], aes(x=x,y=y,colour ="global"), 
                 shape=ctrs["global",3], size=2)
    )
    add_eig1 <- lapply(setdiff(pfts,"global"), function(pft){
      geom_line(data = eigs.list[[pft]],
                aes_q(x=as.name("x1"),y=as.name("y1"), colour = pft), size=.5, alpha=.6)
    })
    add_eig2 <- lapply(setdiff(pfts,"global"), function(pft){
      geom_line(data = eigs.list[[pft]],
                aes_q(x=as.name("x2"),y=as.name("y2"), colour = pft), size=.5)
    })
    add_xlim <- xlim(lims[[trait.pairs[1,i]]][1],lims[[trait.pairs[1,i]]][2])
    add_ylim <- ylim(lims[[trait.pairs[2,i]]][1],lims[[trait.pairs[2,i]]][2])
    
    p[[trait.pairs[1,i]]][[trait.pairs[2,i]]] <- 
      ggplot() + 
      add_global_ellipse +
      # add_ellipses +
      add_centers + 
      add_eig1 + 
      # add_eig2 +
      add_global_eig +
      scale_colour_manual(name = pfts, values = BiomeColorsPFT) + 
      add_xlim + add_ylim +
      global_theme + theme(legend.position="none")
    
    png(filename = sprintf("figures/%s.cov.eigen.ellipse.%s.%s.png",
                           model.type,trait.pairs[1,i], trait.pairs[2,i]))
    plot( p[[trait.pairs[1,i]]][[trait.pairs[2,i]]] +
            labs(title = sprintf("%s vs %s", trait.pairs[1,i],trait.pairs[2,i]),
                 x = trait.pairs[1,i],y = trait.pairs[2,i]))
    dev.off()
    
  } # End loop over trait pairs
  ################################################################################
  # Assemble full pairs plot and save to .png
  
  # Make "biome plot" to use the legend
  
  biomeplot <- ggplot(data = biome.color, aes(Biome,..count..)) + 
    geom_bar(aes(fill=Biome)) + 
    scale_fill_manual(name = names(BiomeColors), values = BiomeColors)
  
  # Construct the pairs plot
  png("what")
  r <- textGrob("")
  txt <- lapply(traits_nolog, textGrob,
                gp = gpar(cex = 1.5))
  names(txt) <- traits_nolog
  plt <- arrangeGrob(
    txt$LL, r, r, r, r, 
    p$LL$LMA, txt$LMA, r, r, r,
    p$LL$Nmass, p$LMA$Nmass, txt$Nmass, r, r,
    p$LL$Pmass, p$LMA$Pmass, p$Nmass$Pmass, txt$Pmass, r,
    p$LL$Rdmass, p$LMA$Rdmass, p$Nmass$Rdmass, p$Pmass$Rdmass, txt$Rdmass,
    ncol = 5,
    heights = c(0.15, 1, 1, 1, 1),
    widths = c(1, 1, 1, 1, 0.5))
  legplot <- biomeplot + global_theme + theme(legend.position = "bottom", legend.direction = "horizontal")+guides(fill=guide_legend(nrow=1))
  g <- ggplotGrob(legplot)$grobs
  legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  lheight <- sum(legend$height)
  lwidth <- sum(legend$width)
  dev.off()
  
  png(filename = sprintf("figures/%s.cov.eigen.ellipse.png",model.type), 
      height = 7, width=9, units = "in", res = 300)
  grid.newpage() 
  grid.draw(arrangeGrob(plt, legend, nrow = 2,
                        heights = unit.c(unit(1, "npc") - lheight,
                                         lheight)))
  dev.off()
}