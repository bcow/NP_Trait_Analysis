# After system("geo/qsub_try.sh")

source("load.try.data.R") # load the data from TRY
source("load.try.mcmc.R") # load the outputs
source("process.mcmc.R")  # export everything to a dataframe saved as outs
source("contour.ellipse.R") # pairwise distribution plots

