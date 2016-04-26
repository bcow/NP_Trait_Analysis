panel.cor.scale <- function(x, y, digits=2, prefix="", cex.cor){
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r = (cor(x, y,use="pairwise"))
  txt <- format(c(r, 0.123456789), digits=digits)[1]
  txt <- paste(prefix, txt, sep="")
  if(missing(cex.cor)) cex <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex * abs(r))
}

panel.var.scale <- function(x, y, digits=2, prefix="", cex.var){
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r = (var(x, y))
  txt <- format(c(r, 0.123456789), digits=digits)[1]
  txt <- paste(prefix, txt, sep="")
  if(missing(cex.var)) cex <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex * abs(r))
}


panel.cor <- function(x, y, digits=2, prefix="", cex.cor){
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r = (cor(x, y,use="pairwise"))
  txt <- format(c(r, 0.123456789), digits=digits)[1]
  txt <- paste(prefix, txt, sep="")
  if(missing(cex.cor)) cex <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex )
}

panel.var <- function(x, y, digits=2, prefix="", cex.var){
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r = (var(x, y))
  txt <- format(c(r, 0.123456789), digits=digits)[1]
  txt <- paste(prefix, txt, sep="")
  if(missing(cex.var)) cex <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex )
}


panel.hist <- function(x, ...){
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col="blue", ...)
}

panel.r2 <- function(x, y, digits=2, prefix="R2 = ", cex.cor){
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r = summary(lm(y~x))$r.squared
  txt <- format(c(r, 0.123456789), digits=digits)[1]
  txt <- paste(prefix, txt, sep="")
  if(missing(cex.cor)) cex <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex )
}


pairs.panels <- function (x,y,smooth=TRUE,scale=FALSE)
{if (smooth ){
  if (scale) {
    pairs(x,diag.panel=panel.hist,upper.panel=panel.cor.scale,lower.panel=panel.smooth)
  }
  else {pairs(x,diag.panel=panel.hist,upper.panel=panel.cor,lower.panel=panel.smooth)
  } #else {pairs(x,diag.panel=panel.hist,upper.panel=panel.cor,lower.panel=panel.smooth)
}
  else #smooth is not true
  { if (scale) {pairs(x,diag.panel=panel.hist,upper.panel=panel.cor.scale)
  } else {pairs(x,diag.panel=panel.hist,upper.panel=panel.cor) }
  } #end of else (smooth)
} #end of function