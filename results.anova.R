library(mvtraits)
library(tibble)
library(RColorBrewer)

cor.dat <- readRDS("processed_output/summary.hier.cor.rds")

trait.pairs <- unique(cor.dat[, trait])
cor.list <- list()
rnames <- c("Biome", "ps_type", "growth_form", 
            "leaf_type", "phenology", "Residuals")
for(trait in trait.pairs){
    cor.list[[trait]] <- lm(Mean ~ Biome + ps_type + growth_form + ps_type + leaf_type + phenology,
                cor.dat, subset = trait == trait) %>% 
                anova %>% dplyr::select(matches("Sum Sq"))
}

cor.anova <- do.call(cbind, cor.list)
colnames(cor.anova) <- trait.pairs
cor.plot.dat <- cor.anova %>% rownames_to_column(var = "Type") %>%
    gather(Trait, Value, -Type) %>%
    setDT
cor.plot.dat[, scaledValue := Value / sum(Value), by=Trait]

## Compute total variance by type
tot.var.table <- cor.plot.dat[, list(tot.var = sum(Value)), by=Type]
tot.var.table[, pct.var := 100 * tot.var / sum(tot.var)]
sink("figures/tot.var.table.txt")
print(tot.var.table[order(tot.var, decreasing=TRUE)], 
      digits = 3)
sink()

Type.colors <- c(brewer.pal(5, "Spectral"), "grey")
names(Type.colors) <- rnames
  
cor.anova.plot <- ggplot(cor.plot.dat) + 
  aes(x = Trait, fill = Type) + 
  geom_bar(stat="identity") + 
  scale_fill_manual(values = Type.colors) 

mypng("figures/pft.cor.anova.scaled.png")
plot(cor.anova.plot + aes(y = scaledValue))
dev.off()

mypng("figures/pft.cor.anova.png")
plot(cor.anova.plot + aes(y = Value))
dev.off()
