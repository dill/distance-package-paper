# does K-S testing with estimated parameters cause an issue?

# from dsm
source("generate.ds.uncertainty.R")


my_ks <- function(model){
  ks.boot <- rep(0,1000)
  for (i in 1:1000) {
    # simulate data from the model
    # fit a model
    refit <- generate.ds.uncertainty(model$ddf)

    if(class(refit)!="try-error"){
      gof_res <- qqplot.ddf(refit, plot=FALSE)
      ks.boot[i] <- gof_res$ks$Dn
    }
  }

  gof_res <- gof_ds(model, plot=FALSE)
  mean(gof_res$dsgof$ks$Dn <= ks.boot, na.rm=TRUE)
}


library(Distance)

nsim <- 1000

ps <- rep(NA, nsim)
ps_corrected <- rep(NA, nsim)

for(i in 1:nsim){
  dat <- abs(rnorm(100))
  #hist(dat)
  mod <- ds(dat, adjustment=NULL, key="hn")

  gof_res <- gof_ds(mod, plot=FALSE)
  ps[i] <- gof_res$dsgof$ks$p

  ps_corrected[i] <- my_ks(mod)

}



