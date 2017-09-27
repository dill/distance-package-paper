# does K-S testing with estimated parameters cause an issue?

# from dsm
source("generate.ds.uncertainty.R")


my_ks <- function(model, nboot=100){
  ks.boot <- rep(0, nboot)
  for (i in 1:nboot) {
    # simulate data from the model
    # fit a model
    refit <- generate.ds.uncertainty(model)

    if(!inherits(refit, "try-error")){
      gof_res <- qqplot.ddf(refit, plot=FALSE)
      ks.boot[i] <- gof_res$ks$Dn
    }
  }

  gof_res <- ddf.gof(model)
  mean(gof_res$dsgof$ks$Dn <= ks.boot, na.rm=TRUE)
}


library(Distance)
library(mrds)

nsim <- 1000

ps <- rep(NA, nsim)
ps_corrected <- rep(NA, nsim)

data(book.tee.data)
region <- book.tee.data$book.tee.region
egdata <- book.tee.data$book.tee.dataframe
samples <- book.tee.data$book.tee.samples
obs <- book.tee.data$book.tee.obs
mod <- ddf(dsmodel = ~mcds(key = "hn", formula = ~sex),
           data = egdata[egdata$observer==1, ], method = "ds",
           meta.data = list(width = 4))

for(i in 1:10){
  gof_res <- ddf.gof(mod)
  ps[i] <- gof_res$dsgof$ks$p

  ps_corrected[i] <- my_ks(mod)

}


