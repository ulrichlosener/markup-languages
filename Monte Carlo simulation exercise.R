# Markup Languages Monte Carlo simulation exercise

library(plyr)
library("magrittr")
library(ggplot2)

set.seed(123)
samples <- rlply(100, rnorm(5000, mean = 0, sd = 1))

descr <- function(x){ 
  M <- mean(x)
  DF <- length(x) - 1
  SE <- 1 / sqrt(length(x))
  INT <- qt(.975, DF) * SE
  return(c(M, M - 0, SE, M - INT, M + INT))
}

labels <- c("Mean" = 0, "Bias" = 0, "Std.Err" = 0, "Lower" = 0, "Upper" = 0)

results <- samples %>%
  vapply(., descr, labels) %>%
  t()

results <- results %>%
  as.data.frame() %>%
  mutate(Covered = Lower < 0 & 0 < Upper)

colMeans(results)
table <- results[!results$Covered, ]

kable(table, "html") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                full_width = F,
                position = "float_right")


limits <- aes(ymax = results$Upper, ymin = results$Lower)

ggplot(results, aes(y=Mean, x=1:100, colour = Covered)) + 
  geom_hline(aes(yintercept = 0), color = "dark grey", size = 2) + 
  geom_pointrange(limits) + 
  xlab("Simulations 1-100") +
  ylab("Means and 95% Confidence Intervals")
