# ITEM RESPONSE THEORY
# PRACTICE WITH R following: https://philippmasur.de/2022/05/13/how-to-run-irt-analyses-in-r/

# FEDERICO FERRERO

# Data wrangling
library(tidyverse)

# Very comprehensive package for IRT analyses
library(mirt)

# Extension for 'mirt' 
#install.packages("devtools")
#library(devtools)

devtools::install_github("masurp/ggmirt")
library(ggmirt)


set.seed(42)
d <- sim_irt(500, 10, discrimination = .25, seed = 42)
head(d)



unimodel <- 'F1 = 1-10'

fit3PL <- mirt(data = d, 
               model = unimodel,  # alternatively, we could also just specify model = 1 in this case
               itemtype = "3PL", 
               verbose = FALSE)
fit3PL

# Factor solution
summary(fit3PL)



params3PL <- coef(fit3PL, IRTpars = TRUE, simplify = TRUE)
round(params3PL$items, 2) # g = c = guessing parameter


M2(fit3PL)


itemfit(fit3PL)


itemfit(fit3PL, fit_stats = "infit") # typical for Rasch modeling


itemfitPlot(fit3PL)


head(personfit(fit3PL))


personfit(fit3PL) %>%
  summarize(infit.outside = prop.table(table(z.infit > 1.96 | z.infit < -1.96)),
            outfit.outside = prop.table(table(z.outfit > 1.96 | z.outfit < -1.96))) # lower row = non-fitting people


personfitPlot(fit3PL)

library(cowplot)

itempersonMap(fit3PL)


tracePlot(fit3PL)


tracePlot(fit3PL, facet = F, legend = T) + scale_color_brewer(palette = "Set3")

# Plotting only individual items
tracePlot(fit3PL,items = c(1:3), facet = F, legend = T) + scale_color_brewer(palette = "Set2")
