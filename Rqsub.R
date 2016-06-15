args <- commandArgs(TRUE)
name <- args[1]
c <- args[2]
print(args)
na <- args[3]
na <- ifelse(!is.na(na),na,"") 

wf.name <- paste(name,na,c,sep=".")

fileConn<-file("q.sh")
writeLines(c(
  "#!/bin/sh",
  paste("#$ -N", wf.name),
  "#$ -l h_rt=48:00:00",
  "#$ -V",
  '#$ -q "geo*"',
  paste("Rscript try.workflow.2.R",c," TRUE TRUE TRUE TRUE", na) 
), fileConn)

close(fileConn)

text <- paste("qsub q.sh")

system(text)