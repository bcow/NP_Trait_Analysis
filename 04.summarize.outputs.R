library(mvtraits)

storage_dir <- "processed_output"
dir.create(storage_dir, showWarnings = FALSE)

# Get summaries for all output and save
output_dir <- "output_selected"
all_files <- list.files(output_dir, ".Rdata", full.names=TRUE)
datlist <- list()
for (f in all_files) {
    print(f)
    datlist[[f]] <- summarizeStan(f)
}
summarydat <- rbindlist(datlist)
saveRDS(summarydat, file = file.path(storage_dir, "summary.rds"))
