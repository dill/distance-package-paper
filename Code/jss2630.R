## ----label=setup, echo=FALSE, include=FALSE------------------------------
library("knitr")
options(prompt = 'R> ')
knitr::opts_knit$set(out.format="sweave", prompt=TRUE)
knitr::opts_chunk$set(cache=TRUE, fig.path="Figures/")
render_sweave()

## ----label=detectdists, echo=FALSE, message=FALSE, fig.height=5.5, fig.width=8----
par(mfrow=c(2,3))
#line and point transect examples

hn.detect <- function(x, sigma) exp(-((x^2)/(2*(sigma^2))))
hn.detect.scaled <- function(x, sigma) hn.detect(x,sigma)/integrate(hn.detect, 0, 500, sigma = sigma)$value

x <- seq(0,500, length = 100)
y <- hn.detect(x, sigma = 200)
y.scaled <- hn.detect.scaled(x, sigma = 200)

set.cex = 1
set.cex.lab = 1.2
set.cex.main = 2
#Line transect example
plot(x, y, xlab = "Perpendicular distance", ylab = "Probability of detection", type = "l", ylim = c(0,1.1), cex = set.cex, cex.lab = set.cex.lab, axes=FALSE)
axis(2, at=c(0,1/2,1)); axis(1); box()

w <- 1/500
line.availability <- rep(w,100)
plot(x, line.availability, xlab = "Perpendicular distance", ylab = "pdf of object distances", type = "l", ylim = c(0,w*1.1), xlim = c(0,500), cex = set.cex, cex.lab = set.cex.lab, main = "Line Transect", cex.main = set.cex.main, axes=FALSE)
axis(2, at=c(0,w/2,w)); axis(1); box()

plot(x,y.scaled, xlab = "Perpendicular distance", ylab= "pdf of observed distances", type = "l", ylim = c(0,0.0044), cex = set.cex, cex.lab = set.cex.lab, axes=FALSE)
axis(2, at=round(c(0,median(y.scaled),max(y.scaled)),3)); axis(1); box()

#Point Transect example
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

## ----label=points-and-lines, echo=FALSE, message=FALSE, warning=FALSE, fig.width=7, fig.height=6----
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
##LT: Changed from pch19 to pch4 so that colour-blind people can see where the samples are
#  points(pt$x[i], pt$y[i], pch=19, col="blue",cex=0.7)
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

## ----label=minke-data-head, message=FALSE--------------------------------
library("Distance")
data("minke")
head(minke)

## ----label=amakihi-data-head---------------------------------------------
data("amakihi")
head(amakihi)

## ----label=hn-hr-par-comp, echo=FALSE, fig.height=5, fig.width=10--------
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

## ----label=adjust-mix-comp, echo=FALSE, fig.height=3, fig.width=10-------
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
curve(f, from=0, to=0.025, xlab="", ylab="", main="Hazard-rate with \n2 cosine adjustments", xlim=c(0,0.025), ylim=c(0,1))
title(xlab="Distance", line=2)
title(ylab="Detection probability", line=2)

## ----label=minke-hn------------------------------------------------------
minke_hn <- ds(minke, truncation = 1.5)

## ----label=minke-hr-cos, message=TRUE------------------------------------
minke_hrcos <- ds(minke, truncation = 1.5, key = "hr")

## ----label=minke-unif-cos------------------------------------------------
minke_unifcos <- ds(minke, truncation = 1.5, key = "unif",
                    adjustment = "cos", order = c(1, 2))

## ----label=amakihi-hr-obs------------------------------------------------
amakihi_hr_obs <- ds(amakihi, truncation = 82.5, transect = "point",
                     key = "hr", formula = ~obs)

## ----label=amakihi-hr-obs-mas, warning=FALSE-----------------------------
amakihi_hr_obs_mas <- ds(amakihi, truncation = 82.5, transect = "point",
                         key = "hr", formula = ~obs + mas)

## ----label=minke-amakihi-hn-plot, echo=FALSE, warning=FALSE, fig.height=4, fig.width=9----
par(mfrow=c(1, 3))
plot(minke_hn, showpoints=FALSE, xlab="Distance (km)")
title("Minke whales", cex.main=1.5)
plot(amakihi_hr_obs, pdf=TRUE, cex=0.7, xlab="Distance (m)")
title("Amakihi\n(observer)", cex.main=1.5)
plot(amakihi_hr_obs_mas, pdf=TRUE, cex=0.7, xlab="Distance (m)")
title("Amakihi\n(observer+minutes)", cex.main=1.5)

## ----label=amakihi-summary-----------------------------------------------
summary(amakihi_hr_obs)

## ----label=amakihi-gof, fig.keep="none"----------------------------------
amakihi_hn <- ds(amakihi, truncation = 82.5, transect = "point",
                 key = "hn", adjustment = NULL)
gof_ds(amakihi_hn)
gof_ds(amakihi_hr_obs)

## ----label=amakihiqq, echo=FALSE, message=FALSE, fig.height=4, fig.width=7, results="hide"----
par(mfrow=c(1,2))
gof_ds(amakihi_hn, cex=0.3)  # shrinking dot size to obliterate 45-deg line less
gof_ds(amakihi_hr_obs, cex=0.3)

## ----label=summary-table, results='asis', echo=FALSE---------------------
kable(summarize_ds_models(amakihi_hn, amakihi_hr_obs, amakihi_hr_obs_mas),
      digits = 3, format = "latex", row.names = FALSE, escape=FALSE,
      booktabs=TRUE,
      caption="Summary for the detection function models fitted to the amakihi data. ``C-vM'' stands for Cram\\'{e}r-von Mises, $P_a$ is average detectability (see ``Estimating abundance and variance''), se is standard error. Models are sorted according to AIC.\\label{tab:amakihi}")

## ----label=minke-N-summary-----------------------------------------------
summary(minke_hn)

## ----label=minke-abundance-table, results='asis', echo=FALSE-------------
minke_table <- summary(minke_hn)$dht$individuals$N
minke_table$lcl <- minke_table$ucl <- minke_table$df <- NULL
colnames(minke_table) <- c("Stratum", "$\\hat{N}$", "$\\text{se}(\\hat{N}$)", "$\\text{CV}(\\hat{N}$)")
kable(minke_table, digits=3, format="latex", booktabs=TRUE, row.names=FALSE, escape=FALSE, caption="Summary of abundance estimation for the half-normal model for the minke whale data.\\label{minke-abund}")

