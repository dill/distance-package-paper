### R code for "Distance Sampling in R"
### David L Miller, Eric A Rexstad, Len Thomas,
### Laura Marshall, Jeffrey L Laake

### code chunk number 1: setup prompt for paper
options(prompt = 'R> ')


### code chunk number 2: detectdists
### this builds figure 1
par(mfrow=c(2,3))

# detection functions
hn.detect <- function(x, sigma) exp(-((x^2)/(2*(sigma^2))))
hn.detect.scaled <- function(x, sigma) hn.detect(x,sigma)/integrate(hn.detect, 0, 500, sigma = sigma)$value

# limits
x <- seq(0,500, length = 100)
# evaluate and rescale the detection function
y <- hn.detect(x, sigma = 200)
y.scaled <- hn.detect.scaled(x, sigma = 200)

# set plotting options
set.cex = 1
set.cex.lab = 1.2
set.cex.main = 2

# now plot some detection functions and pdfs
# Line transect example
plot(x, y, xlab = "Perpendicular distance", ylab = "Probability of detection", type = "l", ylim = c(0,1.1), cex = set.cex, cex.lab = set.cex.lab, axes=FALSE)
axis(2, at=c(0,1/2,1)); axis(1); box()

w <- 1/500
line.availability <- rep(w,100)
plot(x, line.availability, xlab = "Perpendicular distance", ylab = "pdf of object distances", type = "l", ylim = c(0,w*1.1), xlim = c(0,500), cex = set.cex, cex.lab = set.cex.lab, main = "Line Transect", cex.main = set.cex.main, axes=FALSE)
axis(2, at=c(0,w/2,w)); axis(1); box()

plot(x,y.scaled, xlab = "Perpendicular distance", ylab= "pdf of observed distances", type = "l", ylim = c(0,0.0044), cex = set.cex, cex.lab = set.cex.lab, axes=FALSE)
axis(2, at=round(c(0,median(y.scaled),max(y.scaled)),3)); axis(1); box()

# Point Transect example
plot(x, y, xlab = "Radial distance", ylab = "Probability of detection", type = "l", ylim = c(0,1.1), cex = set.cex, cex.lab = set.cex.lab, axes=FALSE)
axis(2, at=c(0,1/2,1)); axis(1); box()

point.area <- function(x) 2*pi*x

point.availability <- 2*pi*x/integrate(point.area, 0, 500)$value

plot(x, point.availability, xlab = "Radial distance", ylab = "pdf of object distances", type = "l", ylim = c(0,2*w*1.1), xlim = c(0,500), cex = set.cex, cex.lab = set.cex.lab, main = "Point Transect", cex.main = set.cex.main, axes=FALSE)
axis(2, at=c(0,w,2*w)); axis(1); box()

point.observed.shape <- function(x, sigma) 2*pi*x*hn.detect(x, sigma)

pdf.point.observed <- function(x, sigma) point.observed.shape(x, sigma = sigma)/integrate(point.observed.shape, 0, 500, sigma = sigma)$value


pdf.observed <- pdf.point.observed(x, sigma = 200)
plot(x, pdf.observed, xlab = "Radial distance", ylab= "pdf of observed distances", type = "l", ylim = c(0,0.00345), cex = set.cex, cex.lab = set.cex.lab, axes=FALSE)
axis(2, at=round(c(0,max(pdf.observed)/2,max(pdf.observed)),4)); axis(1); box()


### code chunk number 3: points-and-lines
# this builds figure 2
opar <- par(mfrow=c(2,2))
# line and point transect examples
# top row points/samplers in space
# bottom row histograms

# generate some animals to sample
set.seed(15) # same results every time
suppressPackageStartupMessages(library("mgcv")) # for inSide

# generate population locations
N <- 500
x <- runif(N)
y <- runif(N)

## line transects
opar2 <- par(mar=c(1, 1, 1, 1) + 0.1)
plot(x, y, pch=19, asp=1, cex=0.6, main="",col="grey", axes=FALSE, xlab="", ylab="")
polygon(x=c(0,0,1,1,0), y=c(0,1,1,0,0))
# generate some lines
# in this case we don't randomise the offset of the grid
lt <- list(x=c(-0.0125,-0.0125,0.0125,0.0125,-0.0125), y=c(0,1,1,0,0))
# set sigma
sigma <- 0.035
# storage for detected distances
detected_distances <- c()
for(i in 1:6){
  # calculate next strip location
  lt$x <- lt$x+0.15
  # plot the line transect
  lines(x=rep(mean(range(lt$x)),2), y=c(0,1), col="blue",lty=2)
  # calculate the distances to objects from the line
  distances <- abs(lt$x - x)
  # randomly decide which were detected
  detected <- exp(-distances^2/(2*sigma^2)) > runif(length(distances))
#  detected <- (1-exp(-(distances/sigma)^(-1.2))) > runif(length(distances))
  # plot those objects detected
  points(x[detected], y[detected], pch=19, cex=0.6, col="red")
  # collect the distances to detected objects
  detected_distances <- c(detected_distances, distances[detected])
}

#par(opar2)
par(mar=c(4, 4, 1, 2) + 0.1)
hist(detected_distances, main="", xlab="Observed perpendicular distances")

## point transects
par(mar=c(1, 1, 1, 1) + 0.1)
plot(x, y, pch=19, asp=1, cex=0.6, main="", col="grey", axes=FALSE, xlab="", ylab="")
polygon(x=c(0,0,1,1,0), y=c(0,1,1,0,0))

# set sigma
sigma <- 0.05
# storage for detected distances
detected_distances <- c()
# lay out a grid of points
pt <- as.list(expand.grid(x=seq(0.15, 0.85, len=3), y=seq(0.15, 0.85, len=3)))
for(i in 1:length(pt$x)){
  # generate point location
  # plot the point transect
  points(pt$x[i], pt$y[i], pch=17, col="blue",cex=1.5)
  # calculate the distances to objects from the line
  distances <- sqrt((pt$x[i] - x)^2+(pt$y[i]-y)^2)
  # randomly decide which were detected
  detected <- exp(-distances^2/(2*sigma^2)) > runif(length(distances))
  # plot those objects detected
  points(x[detected], y[detected], pch=19, cex=0.6, col="red")
  # collect the distances to detected objects
  detected_distances <- c(detected_distances, distances[detected])
}

par(mar=c(4, 4, 1, 2) + 0.1)
hist(detected_distances, main="", xlab="Observed radial distances")

par(opar)


### code chunk number 4: minke-data-head
# load the library and the minke data
library("Distance")
data("minke")
head(minke)

### code chunk number 5: amakihi-data-head
# load amakihi data set
data("amakihi")
head(amakihi)


### code chunk number 6: hn-hr-par-comp
# figure 3
# example detection functions, loop over different parameter values
par(mfrow=c(2,4), mar=c(3.5, 3, 2, 1) + 0.1)

## half-normal
g.hn <- function(x,sigma) exp(-x^2/(2*sigma^2))
for(this.sig in c(0.05, 0.25, 1, 10)){
  this.g <- function(x) g.hn(x, sigma=this.sig)
  curve(this.g, from=0, to=1, xlab="", ylab="",
        main=bquote(sigma == .(this.sig)),
        xlim=c(0,1), ylim=c(0,1), asp=1)
  title(xlab="Distance", line=2)
  title(ylab="Detection probability", line=2)
}

## hazard-rate
g.hr <- function(x, sigma, b) 1 - exp(-(x/sigma)^-b)
for(this.sig in c(0.1, 0.5)){
  for(this.b in c(5, 1)){
    this.g <- function(x) g.hr(x, sigma=this.sig, b=this.b)
    curve(this.g, from=0, to=1, xlab="", ylab="",
          main=bquote(sigma == .(this.sig) ~ .(", b") == .(this.b)),
          xlim=c(0,1), ylim=c(0,1), asp=1)
    title(xlab="Distance", line=2)
    title(ylab="Detection probability", line=2)
  }
}


### code chunk number 7: adjust-mix-comp
# figure 4, as above, but compare mixtures and key+adjustment models
par(mfrow=c(1,4), mar=c(3.5, 3, 3, 1) + 0.1)

## half-normal with cosine
g <- function(x) exp(-x^2/(2*0.01^2))*(1+0.5*cos((2*pi*x)/0.025))
f <- function(x) g(x)/g(0)
curve(f, from=0, to=0.025, xlab="", ylab="", main="Half-normal with \n1 cosine adjustment", xlim=c(0,0.025), ylim=c(0,1))
title(xlab="Distance", line=2)
title(ylab="Detection probability", line=2)
# 2 cosines
g <- function(x) exp(-x^2/(2*0.011^2))*(1-0.06*cos((2*pi*x)/0.025)+0.25*cos((3*pi*x)/0.025))
f <- function(x) g(x)/g(0)
curve(f, from=0, to=0.025, xlab="", ylab="", main="Half-normal with \n2 cosine adjustments",xlim=c(0,0.025), ylim=c(0,1))
title(xlab="Distance", line=2)
title(ylab="Detection probability", line=2)

## hazard-rate with cosine
g <- function(x) (1 - exp(-(x/0.011)^-3))*(1+0.05*cos((2*pi*x)/0.025))
f <- function(x) g(x)/g(0)
curve(f, from=0, to=0.025, xlab="", ylab="", main="Hazard-rate with \n1 cosine adjustment", xlim=c(0,0.025), ylim=c(0,1))
title(xlab="Distance", line=2)
title(ylab="Detection probability", line=2)
# 2 cosines
g <- function(x) (1 - exp(-(x/0.0199)^-1.4))*
                 (1-0.08*cos((2*pi*x)/0.025)+0.08*cos((3*pi*x)/0.025))
f <- function(x) g(x)/g(0)
curve(f, from=0, to=0.025, xlab="", ylab="", main="Hazard-rate with \n2 cosine adjustment", xlim=c(0,0.025), ylim=c(0,1))
title(xlab="Distance", line=2)
title(ylab="Detection probability", line=2)


### code chunk number 8: minke-hn
# fit a simple detection function to the minke data
# (half-normal key, selecting adjustments by AIC)
minke_hn <- ds(minke, truncation = 1.5)


### code chunk number 9: minke-hr-cos
# fit another detection function to the minke data
# (hazard-rate key, selecting adjustments by AIC)
minke_hrcos <- ds(minke, truncation = 1.5, key = "hr")


### code chunk number 10: minke-unif-cos
# again fitting to the minke data, now uniform with cosine(1,2) adjustments
minke_unifcos <- ds(minke, truncation = 1.5, key = "unif",
                    adjustment = "cos", order = c(1, 2))


### code chunk number 11: amakihi-hr-obs
# now with the amakihi point transect data, hazard-rate key with observer
# as a covariate
amakihi_hr_obs <- ds(amakihi, truncation = 82.5, transect = "point",
                     key = "hr", formula = ~obs)


### code chunk number 12: amakihi-hr-obs-mas
# again for the amakihi data, hazard-rate key with observer and minutes after
# sunrise as covariates
amakihi_hr_obs_mas <- ds(amakihi, truncation = 82.5, transect = "point",
                         key = "hr", formula = ~obs + mas)


### code chunk number 13: minke-amakihi-hn-plot
# figure 5: plot the fitted detection functions
par(mfrow=c(1, 3))
plot(minke_hn, showpoints=FALSE)
title("Minke whales", cex.main=1.5)
plot(amakihi_hr_obs, pdf=TRUE, cex=0.7)
title("Amakihi\n(observer)", cex.main=1.5)
plot(amakihi_hr_obs_mas, pdf=TRUE, cex=0.7)
title("Amakihi\n(observer+minutes)", cex.main=1.5)


### code chunk number 14: amakihi-summary
# print the summary for the amakihi model with observer as a covariate
summary(amakihi_hr_obs)


### code chunk number 15: amakihi-gof
# goodness of fit info for some amakihi models
# note the plot is supressed in this case
amakihi_hn <- ds(amakihi, truncation = 82.5, transect = "point",
                 key = "hn", adjustment = NULL)
gof_ds(amakihi_hn)
gof_ds(amakihi_hr_obs_mas)


### code chunk number 16: amakihiqq
# figure 6 same goodness of fit code as before now slightly adjusted to plot
par(mfrow=c(1,2))
gof_ds(amakihi_hn, asp=1, cex=0.3)  # shrinking dot size to obliterate 45-deg line less
gof_ds(amakihi_hr_obs_mas, asp=1, cex=0.3)


### code chunk number 17: summary-table
# table 2, list of models and summary statistics, generates a LaTeX table
# that can be included verbatim into the paper
library("knitr")
kable(summarize_ds_models(amakihi_hn, amakihi_hr_obs, amakihi_hr_obs_mas),
      digits = 3, format = "latex", row.names = FALSE, escape=FALSE,
      booktabs=TRUE,
      caption="Summary for the detection function models fitted to the amakihi data. ``C-vM'' stands for Cram\\'{e}r-von Mises, $P_a$ is average detectability (see ``Estimating abundance and variance''), se is standard error. Models are sorted according to AIC.\\label{tab:amakihi}")


### code chunk number 18: minke-N-summary
# summary of the minke whale model, showing the abundance estimates
summary(minke_hn)


### code chunk number 19: minke-abundance-table
# table 3 stratified abundance estimate for minkes
minke_table <- summary(minke_hn)$dht$individuals$N
minke_table$lcl <- minke_table$ucl <- minke_table$df <- NULL
colnames(minke_table) <- c("Stratum", "$\\hat{N}$", "$\\text{se}(\\hat{N}$)", "$\\text{CV}(\\hat{N}$)")
kable(minke_table, digits=3, format="latex", booktabs=TRUE, row.names=FALSE, escape=FALSE, caption="Summary of abundance estimation for the half-normal model for the minke whale data.\\label{minke-abund}")


