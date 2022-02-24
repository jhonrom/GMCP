
################################################################################################################
# Testing Hydrological Post-processing: MCP, ABC, MCMC for calibration and validation period                   #
# Goal: Estimate predictive uncertainty                                                                        #
# Jonathan Romero-Cuellar;  15/04/2019                                                                         #                                                                                                 #
# Reference: Using MOPEX data with univariate framework and GR4J model                                         #
#                                                                                                              #
################################################################################################################

#  Set up -----------------------------------------------------------------------------------
#fix directory
file.choose()
setwd("G:\\Mi unidad\\1Doctorado\\TesinaMasterCoDireccion")
setwd("F:/TesinaMasterCoDireccion")
setwd("E:/Doctorado/TesinaMasterCoDireccion")

rm(list=ls()) 
set.seed(25021987)
# Load last workspace
load("MOPEXpostprocessing.RData")
# 0. Install and load libraries
if(!require(zoo)){install.packages('zoo'); library(zoo)} else {library(zoo)}
if(!require(ks)){install.packages('ks'); library(ks)} else {library(ks)}
if(!require(colorspace)){install.packages('colorspace'); library(colorspace)} else {library(colorspace)}
if(!require(NSM3)){install.packages('NSM3'); library(NSM3)} else {library(NSM3)}
if(!require(Bolstad2)){install.packages('Bolstad2'); library(Bolstad2)} else {library(Bolstad2)}
if(!require(MASS)){install.packages('MASS'); library(MASS)} else {library(MASS)}
if(!require(MHadaptive)){install.packages('MHadaptive'); library(MHadaptive)} else {library(MHadaptive)}
if(!require(readr)){install.packages('readr'); library(readr)} else {library(readr)}
if(!require(abc)){install.packages('abc'); library(abc)} else {library(abc)}
if(!require(moments)){install.packages('moments'); library(moments)} else {library(moments)}
if(!require(kdecopula)){install.packages('kdecopula'); library(kdecopula)} else {library(kdecopula)}
if(!require(moments)){install.packages('moments'); library(moments)} else {library(moments)}
if(!require(beepr)){install.packages('beepr'); library(beepr)} else {library(beepr)}
if(!require(scales)){install.packages('scales'); library(scales)} else {library(scales)}
library(lattice)
# 1. Load cleaning data -----------------------------------------------------------------------------------
# first import data and save all data for GR4J model
save(dataMopex, file = "dataMopex.RData")                          # save R object
write.table(dataMopex, "./dataMopex.txt", sep="\t", row.names = F) # save .txt object
setwd("D:/Doctorado/TesinaMasterCoDireccion")

# load de data
load("dataMopex.RData")     # import    # 
# Split calibration and validation period
caldata <- dataMopex[dataMopex$Date < "1980-12-01",]
valdata <- dataMopex[dataMopex$Date >= "1980-12-01",]


# convert to time series object for MOPEX catchment GR4J model streamflow (m3/s)
  # calibration period
B1qocal  <- zoo(caldata$B1qo, order.by = caldata$Date)
B1qscal  <- zoo(caldata$B1qs, order.by = caldata$Date)
B2qocal  <- zoo(caldata$B2qo, order.by = caldata$Date)
B2qscal  <- zoo(caldata$B2qs, order.by = caldata$Date)
B3qocal  <- zoo(caldata$B3qo, order.by = caldata$Date)
B3qscal  <- zoo(caldata$B3qs, order.by = caldata$Date)
B4qocal  <- zoo(caldata$B4qo, order.by = caldata$Date)
B4qscal  <- zoo(caldata$B4qs, order.by = caldata$Date)
B5qocal  <- zoo(caldata$B5qo, order.by = caldata$Date)
B5qscal  <- zoo(caldata$B5qs, order.by = caldata$Date)
B6qocal  <- zoo(caldata$B6qo, order.by = caldata$Date)
B6qscal  <- zoo(caldata$B6qs, order.by = caldata$Date)
B7qocal  <- zoo(caldata$B7qo, order.by = caldata$Date)
B7qscal  <- zoo(caldata$B7qs, order.by = caldata$Date)
B8qocal  <- zoo(caldata$B8qo, order.by = caldata$Date)
B8qscal  <- zoo(caldata$B8qs, order.by = caldata$Date)
B9qocal  <- zoo(caldata$B9qo, order.by = caldata$Date)
B9qscal  <- zoo(caldata$B9qs, order.by = caldata$Date)
B10qocal <- zoo(caldata$B10qo, order.by = caldata$Date)
B10qscal <- zoo(caldata$B10qs, order.by = caldata$Date)
B11qocal <- zoo(caldata$B11qo, order.by = caldata$Date)
B11qscal <- zoo(caldata$B11qs, order.by = caldata$Date)
B12qocal <- zoo(caldata$B12qo, order.by = caldata$Date)
B12qscal <- zoo(caldata$B12qs, order.by = caldata$Date)

  # validation period
B1qoval <- zoo(valdata$B1qo, order.by = valdata$Date)
B1qsval <- zoo(valdata$B1qs, order.by = valdata$Date)
B2qoval  <- zoo(valdata$B2qo, order.by = valdata$Date)
B2qsval  <- zoo(valdata$B2qs, order.by = valdata$Date)
B3qoval  <- zoo(valdata$B3qo, order.by = valdata$Date)
B3qsval  <- zoo(valdata$B3qs, order.by = valdata$Date)
B4qoval  <- zoo(valdata$B4qo, order.by = valdata$Date)
B4qsval  <- zoo(valdata$B4qs, order.by = valdata$Date)
B5qoval  <- zoo(valdata$B5qo, order.by = valdata$Date)
B5qsval  <- zoo(valdata$B5qs, order.by = valdata$Date)
B6qoval  <- zoo(valdata$B6qo, order.by = valdata$Date)
B6qsval  <- zoo(valdata$B6qs, order.by = valdata$Date)
B7qoval  <- zoo(valdata$B7qo, order.by = valdata$Date)
B7qsval  <- zoo(valdata$B7qs, order.by = valdata$Date)
B8qoval  <- zoo(valdata$B8qo, order.by = valdata$Date)
B8qsval  <- zoo(valdata$B8qs, order.by = valdata$Date)
B9qoval  <- zoo(valdata$B9qo, order.by = valdata$Date)
B9qsval  <- zoo(valdata$B9qs, order.by = valdata$Date)
B10qoval <- zoo(valdata$B10qo, order.by = valdata$Date)
B10qsval <- zoo(valdata$B10qs, order.by = valdata$Date)
B11qoval <- zoo(valdata$B11qo, order.by = valdata$Date)
B11qsval <- zoo(valdata$B11qs, order.by = valdata$Date)
B12qoval <- zoo(valdata$B12qo, order.by = valdata$Date)
B12qsval <- zoo(valdata$B12qs, order.by = valdata$Date)

# 2. Common Functions -----------------------------------------------------------------------------------

# Assessmet Performance
Nash <- function(obs, sim, na.rm = T){
  # Compute the Nash-Sutcliffe efficiency rating for model estiamtes.
  # inputs : obs = observations, sim =simulations;   
  # Outputs: Nash-Sutcliffe efficiency
  # calls  : NSE <- Nash(obs = obs, sim = sim)
  NSE <- 1 - sum((obs - sim)^2) / sum((obs - mean(obs))^2)
  return(NSE)
}

klinggupta <- function(obs, sim, na.rm = T){
  # Calculate Kling-Gupta Efficiency.
  # inputs : obs = observations, sim =simulations;   
  # Outputs: Kling-Gupta Efficiency
  # calls  : KGE <- klinggupta(obs = obs, sim = sim)
  r <- cor(obs, sim)                # coeficiente correlation
  rat.var <- sd(sim) / sd(obs)      # variance rate
  rat.bias <- mean(sim) / mean(obs) # bias rate
  KGE <- 1 - sqrt(((r - 1)^2) + ((rat.var - 1)^2) + ((rat.bias - 1)^2))
  return(list(KGE = KGE, Correlation = r, CV.ratio = rat.var, Bias.ratio = rat.bias))
}

# Normal Quantil Transformation NQT
# install.packages("ks")
#library(ks)   # nonparametric fitting
NQT <- function(N = 1000, factor.sample = 0.5, qobs, qsim, na.rm = T ){
  # Transformation to Normal space N(0,1) through Normal Quantil Transformation. (Nonparametric approach)
  # inputs: N = numbers samples; factor.sample = double empirical observations; qobs = observations, sim =simulations;
  # outputs: eta = observations N(0,1); etaS = simulations N(0,1);
  # calls: AipeTransNQT <- NQT(N = 1000, factor.sample = 0.5, qobs = AipeDataPost$Obs_m3s, qsim = AipeDataPost$Sim_m3s)
  
  # 1. interpolation observations and fitting nonparametric distribution
  # observations
  qmax <- 2 * max(qobs)
  qmin <- min(qobs)
  maxsize <- length(qobs)
  t <- 1 : maxsize
  tsample <- seq(from = 1, to = maxsize, by = factor.sample)
  qsam <- approx(t, qobs, xout = tsample)               # resample observations (interpolation)
  Pq <- pkde(q = qsam$y, fhat = kde(x = qobs, binned = TRUE ))
  # same for simulation
  qmaxs <- 2 * max(qsim)
  qmins <- min(qsim)
  maxsizes <- length(qsim)
  t_s <- 1 : maxsizes
  tsamples <- seq(from = 1, to = maxsizes, by = factor.sample)
  qssam <- approx(t_s, qsim, xout = tsamples)               # resample observations (interpolation)
  Pqs <- pkde(q = qssam$y, fhat = kde(x = qsim, binned = TRUE ))
  # 2. NQT transformation
  eta  <- qnorm(p = Pq, mean = 0, sd = 1)
  etaS <- qnorm(p = Pqs, mean = 0, sd = 1)
  
  return(list(eta = eta, etaS = etaS ))
  
}

# COMPUTE PREDICTIVE DISTRIBUTION
PostPredDist <- function(forecastN, postparam, factor.sample, forecast){
  # Compute the Posterior Predictive Distribution.
  # inputs : forecastN = simulations un Normal space, postparam = posterior parameters, factor.sample= double empirical observations, forecast = simulations or observations in real space (depent it is calibration[obs], validation[sim])
  # Outputs: posterior predictive distribution
  # calls  : PostPredCalMCMC <- PostPredDist(forecastN = datos[,2], postparam = mcmc.lrm$trace, factor.sample = 0.5, forecast = AipeDataPostVal$Sim_m3s)
  # function for very low streamflows i.e Guadalupe cathment
  qkde2 <- function (p, fhat) 
  {
    if (any(p > 1) | any(p < 0)) 
      stop("p must be <= 1 and >= 0")
    cumul.prob <- pkde(q = fhat$eval.points, fhat = fhat)
    ind <- findInterval(x = p, vec = sort(cumul.prob))
    quant <- rep(0, length(ind))
    for (j in 1:length(ind)) {
      i <- ind[j]
      if (i == 0) 
        quant[j] <- fhat$eval.points[1]
      else if (i >= length(fhat$eval.points)) 
        quant[j] <- fhat$eval.points[length(fhat$eval.points)]
      else {
        quant1 <- fhat$eval.points[i]
        quant2 <- fhat$eval.points[i + 1]
        prob1 <- cumul.prob[i]
        prob2 <- cumul.prob[i + 1]
        alpha <- (p[j] - prob2)/(prob1 - prob2)
        quant[j] <- quant1 * alpha + quant2 * (1 - alpha)
      }
    }
    return(quant)
  }
  
  
  ypred <- matrix(0, length(forecastN), nrow(postparam))  #matrix(observations,posterior), crate a matrix for predictions
  for(i in  1:length(forecastN)){
    for(j in 1:nrow(postparam)){
      # sample rnorm(n, mean = linear model, sd = error parameter estimate by MCMC )
      ypred[i, j] <- rnorm(1, postparam[j,1] + postparam[j,2] * forecastN[i], postparam[j,3])
    }
  }
  # posterior predictive distribution normal space
  posteriorN <- matrix(data = NA, nrow = nrow(ypred), ncol = ncol(ypred))
  for (k in 1:nrow(ypred)) {
    posteriorN[k,] <- pnorm(q = ypred[k,], mean = 0, sd = 1)
  }
  # posterior predictive distribution real space with resample   ¡¡¡¡ojo¡¡¡¡ change kde for validation 
  posteriorR <- matrix(data = NA, nrow = nrow(ypred), ncol = ncol(ypred))
  # browser()
  for (k in 1:nrow(ypred)) {
    posteriorR[k,] <- qkde(p = posteriorN[k,], fhat = kde(x = forecast, binned = TRUE ))
    # posteriorR[k,] <- qkde2(p = posteriorN[k,], fhat = kde(x = forecast, binned = TRUE )) # just for low flow near to zero
  }
  # posterior predictive distribution real space without resample  ¡¡¡¡ojo¡¡¡¡ change lenght to validation for validation 
  # invert matrix (posterior,time)
  tsample2 <- seq(from = 1, to = length(forecastN), by = 1/factor.sample)
  posterior <- matrix(data = NA, nrow = ncol(ypred), ncol = length(tsample2)) #¡¡¡¡ojo¡¡¡¡ 
  posteriorR2 <-t(posteriorR)     #transpose matrix
  for (k in 1:nrow(posterior)) {
    posterior[k,] <- posteriorR2 [k,tsample2] 
  }
  return(posterior)
}

# COMPUTE PREDICTIVE DISTRIBUTION FOR VALIDATION PERIOD
PostPredDistVal <- function(simvalN, parerrormod,factor.sample, simval, NP){
  # Compute the Posterior Predictive Distribution.
  # inputs : simvalN = simulations in Normal space, NP = number of points for predictive posterior = 1000,factor.sample= double empirical observations, simval = simulations or observations in real space (depent it is calibration[obs], validation[sim])
  #           parerrormod = vector of parmeter postprocessor (b0,b1,e)
  # Outputs: posterior predictive distribution in validation
  # calls  : PostPredVal <- PostPredDistVal(simvalN = TransNQTval$etaS, factor.sample = 0.5, simval = as.numeric(simval), NP = 1000)
  
  # function for very low streamflows i.e Guadalupe cathment
  qkde2 <- function (p, fhat) 
  {
    if (any(p > 1) | any(p < 0)) 
      stop("p must be <= 1 and >= 0")
    cumul.prob <- pkde(q = fhat$eval.points, fhat = fhat)
    ind <- findInterval(x = p, vec = sort(cumul.prob))
    quant <- rep(0, length(ind))
    for (j in 1:length(ind)) {
      i <- ind[j]
      if (i == 0) 
        quant[j] <- fhat$eval.points[1]
      else if (i >= length(fhat$eval.points)) 
        quant[j] <- fhat$eval.points[length(fhat$eval.points)]
      else {
        quant1 <- fhat$eval.points[i]
        quant2 <- fhat$eval.points[i + 1]
        prob1 <- cumul.prob[i]
        prob2 <- cumul.prob[i + 1]
        alpha <- (p[j] - prob2)/(prob1 - prob2)
        quant[j] <- quant1 * alpha + quant2 * (1 - alpha)
      }
    }
    return(quant)
  }
  
  
  ypredval <- matrix(data = NA, nrow = NP, ncol = length(simvalN))  #matrix(observations,posterior), crate a matrix for predictions
  for (i in 1:length(simvalN)){
    ypredval[, i] <- rnorm(NP, parerrormod[1] + parerrormod[2] * simvalN, parerrormod[3])
  }
  
  # posterior predictive distribution normal space
  posteriorNval <- matrix(data = NA, nrow = nrow(ypredval), ncol = ncol(ypredval))
  for (k in 1:nrow(ypredval)) {
    posteriorNval[k,] <- pnorm(q = ypredval[k,], mean = 0, sd = 1)
  }
  # posterior predictive distribution real space with resample   ¡¡¡¡ojo¡¡¡¡ change kde for validation 
  posteriorRval <- matrix(data = NA, nrow = nrow(ypredval), ncol = ncol(ypredval))
  for (k in 1:nrow(ypredval)) {
    posteriorRval[k,] <- qkde(p = posteriorNval[k,], fhat = kde(x = simval, binned = TRUE ))
    #posteriorRval[k,] <- qkde2(p = posteriorNval[k,], fhat = kde(x = simval, binned = TRUE )) # just for low flow near to zero
  }
  # posterior predictive distribution real space without resample  ¡¡¡¡ojo¡¡¡¡ change lenght to validation for validation 
  # invert matrix (posterior,time)
  tsample2 <- seq(from = 1, to = length(simvalN), by = 1/factor.sample)
  posteriorval <- matrix(data = NA, nrow = ncol(ypredval), ncol = length(tsample2)) #¡¡¡¡ojo¡¡¡¡ 
  posteriorR2val <-t(posteriorRval)     #transpose matrix
  for (k in 1:nrow(posteriorval)) {
    posteriorval[k,] <- posteriorR2val [k,tsample2] 
  }
  return(posteriorval)
}


# Edit posterior 
Edit.Posterior <- function(observations, posterior){
  # Goal   : Compute statistics from posterior   
  # inputs : discharge = observations, posterior = matrix (posterior,time);  
  # Outputs: QQplot, kolmogorov test
  # calls  :Est.Posterior <- Edit.Posterior(observations = Datos[,2], posterior = Cita_mon_MCPm_posterio)
  # functions for compute mode as summary statistics
  moda <- function(x, na.rm = T){
    # function compute mode of vector
    uniqv <- unique(x)
    uniqv[which.max(tabulate(match(x, uniqv)))]
    
  }
  pos.posterior <- matrix(data = NA, nrow = length(observations), ncol = 10)
  colnames(pos.posterior) <- c("sd","mean","median","quantile5","quantile95","p","ecdf","quan2_5","quan97_5","moda")
  posterior <-t(posterior)  # transpose data (time, posterior)
  for (i in 1:length(observations)) {
    pos.posterior[i,1] <- sd(posterior[i,], na.rm = T)
    pos.posterior[i,2] <- mean(posterior[i,], na.rm = T)
    pos.posterior[i,3] <- median(posterior[i,], na.rm = T)
    pos.posterior[i,4] <- quantile(posterior[i,], probs = c(0.05),na.rm = T)
    pos.posterior[i,5] <- quantile(posterior[i,], probs = c(0.95),na.rm = T)
    pos.posterior[i,6] <- pnorm(observations[i], pos.posterior[i,2],  pos.posterior[i,1])
    posteriorECDF <- ecdf(pos.posterior[i,])
    pos.posterior[i,7] <- posteriorECDF(observations[i])
    pos.posterior[i,8] <- quantile(posterior[i,], probs = c(0.025),na.rm = T)
    pos.posterior[i,9] <- quantile(posterior[i,], probs = c(0.975),na.rm = T)
    pos.posterior[i,10] <- moda(posterior[i,], na.rm = T)
  }
  return(pos.posterior)
  
}
# Test Predictive Uncertainty 
# 1. Cotton test 1
# install.packages("NSM3", dependencies = T), install.packages("colorspace"), install.packages("Bolstad2")
# library(colorspace), library(NSM3), library(Bolstad2)
QQtest <- function(discharge, posterior) {
  # inputs : discharge = observations, posterior = matrix (posterior,time);  
  # Outputs: QQplot, kolmogorov test
  # calls  :QQplotout <- QQtest(discharge = Datos$q, posterior = y)
  # QQplotout <- QQtest(discharge = New.datos$q, posterior = COPfrenchBroad$posterior)
  # QQplotoutPrueba <- QQtest(discharge = DataSitarum$Observation.discharge, posterior = COPsitarum$posterior)
  # QQplotoutmcplnor <- QQtest(discharge = DataSitarum$Observation.discharge, posterior = mcpoutputLNORMAL$posterior)
  require(Bolstad2); require(NSM3); 
  # 1.1 reduce dimension of monte carlo matix
  # yo <- apply(y, 2, mean)               # discharge from PD bivariate
  # Empiro <- apply(Empir, 2, mean)       # CDF from PD bivariate
  # MA <- cbind(yo, Empiro)               # create matrix to estimate quantile
  # MAsor <- apply(MA, 2, sort)           # Order matrix 
  y.tran <- t(posterior)                   # transpose matrix y (time, posterior)
  zi <-matrix(data = NA, nrow = length(discharge), ncol = 1 )
  for (i in 1:length(discharge)) {
    m.ecdf <-ecdf(y.tran[i,])      # calculated cdf for every posterior
    zi[i,1] <- m.ecdf(discharge[i])  # evaluate every observation in every cdf
  }
  # without loop
  # m.ecdf <-ecdf(y.tran)
  # zi <- m.ecdf(Datos$q)
  x <- qunif(ppoints(length(discharge)))           # theoretical quantile, observed data length
  # qqplot(x,zi,xlab="Theoretical Quantile U[0,1]",ylab="Quantile of forecast")
  # abline(0,1,lwd=2)
  # line(x, x + 0.05)
  # line(x, x - 0.05)
  qqarea <- sintegral(x, abs(x - zi), n.pts = 525)$int   #256
  #text(paste("Area ",formatC(qqarea,digits=4, format="f"),sep=""), x=0.9 ,y=0.3)
  # dev.copy(png, file="ReliabilityDiagramMCPcitarum.png", res=200, height=12, width=20, units="in")
  # dev.off()
  alphaInd <- 1 - (2 * (qqarea /length(discharge)))  # reliability index
  # Estimate precision or sharpeness or resolution (average precision)
  Esperanza <- apply(posterior, 2, mean, na.rm = T)  # expected posterior
  Desv      <- apply(posterior, 2, sd, na.rm = T)  # standar deviations posterior
  precision <- (sum(Esperanza /Desv) / length(discharge))
  return(list(area = qqarea, alpha = alphaInd, x = x, zi = zi, resolution = precision))
  # Kendall's test of independence
  #cor.test(zi, x, method="kendall")
  
}

# 2. Cotton test 2: How many points outside the uncertainty band
TestUncertBand <- function(observations, quantil5, quantil95){
  # inputs : observations = discharge; quantil5 = vector quantile 5%; quantil95 = vector quantile 95% 
  # Outputs: Cotton test II, how many points outside the uncertainty band
  # calls  :CottonTest2 <- TestUncertBand(observations = Datos$q, quantil5 = mcpoutput$q5, quantil95 = mcpoutput$q95)
  
  QoutsideBand <- length(which(observations <= quantil5 | observations >= quantil95)) # identified points outside uncertainty band
  PorOutBand   <- (QoutsideBand/length(observations)) * 100                                # estimate %
  Coverage95  <- abs(100 - PorOutBand)
  #cat("Coverage 95% Uncertainty Band: ", Coverage95)                          # print coverage
  return(Coverage95)
}

# 95% Prediction Probability Uncertainty Band (95PPU)
UncertaintyBand95 <- function(observations, quantil5, quantil95){
  # Goal      : Compute 95% Prediction Probability Uncertainty Band (95PPU)
  # Reference : Li et al. (2017). Comparison of parameter uncertainty analysis techniques for a TOPMODEL application
  # inputs    : observations = streamflow discharge; quantil5 = vector quantile 5%; quantil95 = vector quantile 95%  
  # Outputs   : B = Average band with of 95PPU; CR = containing ratio; D = Average deviation amplitude, d_factor = d-factor is the average with fo the prediction interval
  # calls     : PPU95 <- UncertaintyBand95(observations = as.numeric(QCalAipe), quantil5 = Est.PosteriorMCMC[,4], quantil95 = Est.PosteriorMCMC[,5])
  n  <- length(observations)
  B  <- (1 / n) * sum(quantil95 - quantil5)
  # number of observations outside of this bounds
  nc <- length(which(observations <= quantil5 | observations >= quantil95))
  # number of observations enveloped by this bounds
  CR <- ((n - nc) / n)*100
  D  <- (1 / n) * sum(0.5 * abs((quantil95 + quantil5) - observations))
  d_factor <- ((1 / n) * sum(quantil95 - quantil5)) / sd(observations)
  return(list(B = B, CR = CR, D = D, d_factor = d_factor))
}
# water balance plot
hydro.plot <- function(Pre, EP, Qobs, na.rm = T){
  plot(time(Qobs),as.numeric(Qobs), ylim = c(-max(Qobs), 2*max(Qobs)), yaxt ="n", xlab = "", ylab = "",type = "l", col="red") #, ylab = "streamflow (m3/s)"
  axis(2, at = round(signif(seq(0, max(Qobs), length=5), 2)))
  #axis(1, at = seq.Date(min(time(Qobs)), max(time(Qobs)), 1))
  #axis(1, xaxp =c(1992,2006,100))
  abline(h = 0, lty = 3)
  par(new = T)
  #plot(time(Pre), as.numeric(Pre), col = "blue", type = "l", ylim = rev(c(0, 3*max(Pre))), xlab = "", ylab = "", axes = F)
  barplot(Pre, col = "blue", ylim = rev(c(0, 3*max(Pre))), xlab = "", ylab = "", axes = F)
  axis(4, at = round(signif(seq(0, max(Pre), length=5), 2)))
  par(new = T)
  deltaET <- max(EP) - min(EP)
  plot(time(EP), as.numeric(EP), col = "black", type = "l", ylim = c(min(EP), max(EP) + 2.5*deltaET), xlab = "", ylab = "", axes = F)
  axis(4, at = round(signif(seq(min(EP), max(EP),length=5), 2), 1))
}

# functions for compute mode as summary statistics
moda <- function(x){
  # function compute mode of vector
  uniqv <- unique(x)
  uniqv[which.max(tabulate(match(x, uniqv)))]
  
}


# 3. ABC postprocessor -----------------------------------------------------------------------------------

ABCpostprocessor <- function(calobs, calsim, valobs, valsim, n = 100000, linfa, lsupa, linfb, lsupb, linfe, lsupe){
  # Aim: Compute a predictive uncertainty in real space using the mean as summary statistics
  # Inputs: calobs, calsim, valobs, valsim are observations and simulations for calibration and validation period respectively,  are time series object; n = 1000000  # number of simulations
  #         linfa, lsupa, linfb, lsupb, linfe, lsupe are limite (min,max) of a prior parameters in the linear regression context
  # Outputs: posterior predictive uncertainty
  # Calls:  B1ABC  <- ABCpostprocessor(calobs = B1qocal, calsim = B1qscal, valobs = B1qoval, valsim = B1qsval, n = 100000, linfa = -0.1, lsupa = 0.1, linfb = 0.75, lsupb = 1, linfe = 0, lsupe = 0.6)) # n = 900000 long time

 
  
  ## make NQT transformation
  TransNQT <- NQT(N = 1000, factor.sample = 0.5, qobs = as.numeric(calobs), qsim = as.numeric(calsim))
  TransNQTval <- NQT(N = 1000, factor.sample = 0.5, qobs = as.numeric(valobs), qsim = as.numeric(valsim))
  
  # check NQT transformation calibration
  # plot(TransNQT$etaS, TransNQT$eta, pch = 19, cex = 0.5)
  # qqplot(TransNQT$etaS, TransNQT$eta)     # distribution is from N(0,1)
  # abline(0,1)
  # hist(TransNQT$eta)
  # hist(TransNQT$etaS)
  datos <- cbind(TransNQT$eta, TransNQT$etaS)
  # ab <-lm(TransNQT$etaS~TransNQT$eta) # regression model in order to define range of a priors
  #summary(ab)
  # datos <- cbind(as.numeric(observations), as.numeric(simulations))
  # ABC inference
  # inputs: 
  # 1. vector observed summary statistics
  # 2. matrix of the simulated summary statistics (simulates, summary statistic)
  # 3. matrix of the simulated parameters values (simulates, parameter)
  # prepar my inputs:
  # 1) define summary statistic from observations
  my.stat.obs <- c(mean(datos[,1]))
  names(my.stat.obs) <- c("mean")
  # 2) define prior parameters
  # Uniform prior parameters
  mypar <- data.frame(a =runif(n,linfa,lsupa), b = runif(n,linfb,lsupb), sde =runif(n,linfe,lsupe))
  # gaussian parmeters
  #mypar <- data.frame(a = rnorm(n,mean = -0.0222, sd = 0.005), b = rnorm(n,mean = 1.3287, sd = 0.01), sde =rnorm(n,mean = 0.9, sd = 0.5)) # matirx of parameter, sde = rgamma(100000, shape = 1, rate = 1/100)
  # eliminate negative values
  # mypar$sde[mypar$sde <0] <- 0
  # summary(mypar)
  
  # 3) simulated according with sample parameters 
  print("Simulating data to ABC")
  ysim <- matrix(NA,dim(datos)[1],dim(mypar)[1])
  for (i in 1:dim(mypar)[1]) {
    ysim[,i] <-  mypar$a[i] + mypar$b[i] * datos[,2] + rnorm(1,0,mypar$sde[i])  # + rnorm(1,0,mypar$sde[i]) simulation in normal space <- datos[,2] 
  }
  # eliminate negative values
  # ysim[ysim <0] <- 0
  # any(is.na(ysim))
  # 4) estimate summary statistics from simulate data
  #print("Compute summary statistics from simulating data (ABC)")
  mysim <-data.frame(mean.sim = apply(ysim, 2, mean))
  summary(mysim)
  # 5) ABC rejection sample
  my.rej <- abc(target=my.stat.obs, param=mypar, sumstat=mysim, tol=0.01, method = "rejection") #tol=0.0001, tol=0.01, tol=.1
  summary(my.rej)
  sumabcpar <-summary(my.rej)
  parerrormod <- c(median(my.rej$unadj.values[,1]), median(my.rej$unadj.values[,2]), median(my.rej$unadj.values[,3]))
  #browser()
  # 6) Estimate the predective posterior distribution by ABF
  # forecastN <- datos[,2]
  # postparam <- my.rej$unadj.values
  # ypred <- matrix(NA, length(forecastN), nrow(postparam))  #matrix(observations,posterior), crate a matrix for predictions
  # for(i in  1:length(forecastN)){
  #   for(j in 1:nrow(postparam)){
  #     # sample rnorm(n, mean = linear model, sd = error parameter estimate by MCMC )
  #     ypred[i, j] <- rnorm(1, postparam[j,1] + postparam[j,2] * forecastN[i], postparam[j,3])
  #   }
  # }
  # # eliminate negative values
  # ypred[ypred <0] <- 0
  # any(is.na(ypred))
  # PosteriorPred <-t(ypred)     #transpose matrix
  # save(PosteriorPred, file = "PostPredCalABC.RData")
  ## Estimate the predective posterior distribution by MCMC
  print("Compute Posterior Predictive Distribution from ABC")
  PostPredABC <- PostPredDist(forecastN = datos[,2], postparam = my.rej$unadj.values, factor.sample = 0.5, forecast = as.numeric(calobs))
  save(PostPredABC, file = "PostPredABC.RData")
  #browser()
  # compute statistiscs of posterior predictive
  Est.PosteriorABC <- Edit.Posterior(observations = as.numeric(calobs), posterior = PostPredABC)
  save(Est.PosteriorABC, file = "Est.PosteriorABC.RData")
  
  # Predective posterior Validation period
  PostPredVal <- PostPredDistVal(simvalN = TransNQTval$etaS, parerrormod = parerrormod, factor.sample = 0.5, simval = as.numeric(valobs), NP = 1000)
  EstPostPredVal <- Edit.Posterior(observations = as.numeric(valobs), posterior = PostPredVal)
  save(PostPredVal, file = "PostPredVal.RData")
  save(EstPostPredVal, file = "EstPostPredVal.RData")
  #  Performance metrics 
  # calibration period
  QQplot <- QQtest(discharge = as.numeric(calobs), posterior = PostPredABC)
  NSE <- Nash(obs = as.numeric(calobs), sim = Est.PosteriorABC[,"median"]) 
  KGE <- klinggupta(obs = as.numeric(calobs), sim = Est.PosteriorABC[,"median"])
  CottonTest2 <- TestUncertBand(observations = as.numeric(calobs), quantil5 = Est.PosteriorABC[,"quan2_5"], quantil95 = Est.PosteriorABC[,"quan97_5"])
  PPU95 <- UncertaintyBand95(observations = as.numeric(calobs), quantil5 = Est.PosteriorABC[,"quan2_5"], quantil95 = Est.PosteriorABC[,"quan97_5"])
  # Uniformity Test calibration
  UnifTestCalABC <-ks.test(QQplot$zi,'punif')
  # Ho : data are from U(0,1)
  # Ha : data are not from U(0,1)
  # p-value : 0.8106 > 0.05: accept Ho, data from U(0,1)
  
  # validation period
  QQplotv <- QQtest(discharge = as.numeric(valobs), posterior = PostPredVal)
  NSEv <- Nash(obs = as.numeric(valobs), sim = EstPostPredVal[,"median"]) 
  KGEv <- klinggupta(obs = as.numeric(valobs), sim = EstPostPredVal[,"median"])
  CottonTest2v <- TestUncertBand(observations = as.numeric(valobs), quantil5 = EstPostPredVal[,"quan2_5"], quantil95 = EstPostPredVal[,"quan97_5"])
  PPU95v <- UncertaintyBand95(observations = as.numeric(valobs), quantil5 = EstPostPredVal[,"quan2_5"], quantil95 = EstPostPredVal[,"quan97_5"])
  # Uniformity Test calibration
  UnifTestValABCv <-ks.test(QQplotv$zi,'punif')
  
  
  # Plots 
  # Parametric Posterior
  jpeg("./Fig1ParamePosterior.jpg", res = 600, width = 20, height = 12, units = "in")
  par(mfcol = c(1,4),mar = c(5.5,5.9,0.5,0.5), mgp = c(4,1,0))
  plot(density(my.rej$unadj.values[,1]), xlab = expression(beta[0]), cex.lab = 2.5, cex.axis = 2.5, main = "", ylab = "Density",  lwd = 2) #ylim = c(0,20), xlim = c(-27,-5),
  plot(density(my.rej$unadj.values[,2]), xlab = expression(beta[1]), cex.axis = 2.5, cex.lab = 2.5, main = "", ylab = "", lwd = 2) # ylim = c(0,20), xlim = c(0.5,1.2),
  plot(density(my.rej$unadj.values[,3]), xlab = expression(epsilon), cex.axis = 2.5, cex.lab = 2.5, main = "", ylab = "",  lwd = 2) # ylim = c(0,max(hist(my.rej$unadj.values[,3], freq = F, plot = F)$density)), xlim = c(0,0.9),
  plot(density(PostPredABC[,5]), xlab = expression(paste("Streamflow"~ (m^3~s^-1))), main = "", ylab = "", cex.axis = 2.5, cex.lab = 2.5,  lwd = 2) # "discharge [m3/s] = 2.46"xlim = c(0,50),
  dev.off()
  # Predictive Uncertainty
  # jpeg("./Fig3MonthlyPredUncer.jpg", res = 600, width = 20, height = 12, units = "in")
  # layout(matrix(c(1,1,2,3,3,4), 1, 3, byrow = TRUE))
  # par(mar = c(5.5,6,0.5,0.5))
  # plot(time(simulations), as.numeric(simulations), type ="n", xlab="", ylab=expression(paste("Streamflow"~ (m^3~s^-1))),  cex.axis = 2.5, cex.lab = 2.5)#xaxt="n",
  # #axis(1, at=1:12, labels = month.abb, las = 2,cex.axis = 2.5)
  # polygon(c(time(simulations), rev(time(simulations))), c(Est.PosteriorABC[,"quan2_5"], rev(Est.PosteriorABC[,"quan97_5"])), col='grey', border=F)
  # points(time(observations), as.numeric(observations), col="red", pch =21, bg="red", cex = 1.5)
  # # lines(time(observations), as.numeric(observations), col="red", pch =21, bg="red", cex = 1.5)
  # lines(time(observations), Est.PosteriorABC[,"median"],lty =1, lwd = 3, col="black")
  # #lines(1:12, as.numeric(modelCC), col="blue", lwd = 2)
  # qqplot(QQplot$x, QQplot$zi, cex.axis = 2.5, xlim = c(0,1),ylim = c(0,1),cex.lab = 2.5, xlab="Theoretical Quantile U[0,1]",ylab="Quantile of forecast", col = "blue",type = "l", pch=20, lty=1, lwd = 3)
  # abline(0,1,lwd=2)
  # dev.off()
  
  return(list(posterior = PostPredABC, NSE = NSE, KGE = KGE, UnifTest = UnifTestCalABC$p.value,  PoinsInsideBand = CottonTest2, Est.Posterior = Est.PosteriorABC, PPU95 = PPU95, QQplot = QQplot, abcpar = my.rej, posteriorv = PostPredVal, NSEv = NSEv, KGEv = KGEv, UnifTestv = UnifTestValABCv$p.value,  PoinsInsideBandv = CottonTest2v, Est.Posteriorv = EstPostPredVal, PPU95v = PPU95v, QQplotv = QQplotv))
}


# 4. MCMC postprocessor -----------------------------------------------------------------------------------

MCMCpostprocessor <- function(calobs, calsim, valobs, valsim){
  # Aim: Compute a predictive uncertainty in real space using the MOS and MCMC infer parameters
  # Inputs: calobs, calsim, valobs, valsim are observations and simulations for calibration and validation period respectively,  are time series object;
  # Outputs: posterior predictive uncertainty and metrics
  # Calls:  B1MCMC <- MCMCpostprocessor(calobs = B1qocal, calsim = B1qscal, valobs = B1qoval, valsim = B1qsval)
  # Assessmet Performance
  
  # Function for MCMC post-processor
  # Use de same structure datos["Obs", "Sim"]
  # 2.1 Define a Bayesian linear regression model; 
  li_reg <- function(pars, data){
    a    <- pars[1]   # intercept
    b    <- pars[2]   # slope
    sd_e <- pars[3]   # error (residuals)
    if(sd_e <= 0){return(NaN)}
    pred <- a + b * data[,2]   # linear regression model
    # like likelihood function: dnorm(x, mean = 0, sd = 1, log = FALSE
    log_likelihood <- sum(dnorm(x = data[,1], mean = pred, sd = sd_e, log = TRUE))
    prior <- prior_reg(pars)   # define prior of parameters as function
    return(log_likelihood + prior)
  }
  # 2.2 Define the prior distribution
  prior_reg <- function(pars){
    a       <- pars[1]   # intercept
    b       <- pars[2]   # slope
    epsilon <- pars[3]   # error
    # non-informative (flat) priors on all
    prior_a <- dnorm(x = a, mean = 0, sd = 100, log = T)   # normal but too much sd
    prior_b <- dnorm(x = b, mean = 0, sd = 100, log = T)   # prior b
    prior_epsilon <- dgamma(x = epsilon, shape = 1, rate = 1/100, log = T)
    return(prior_a + prior_b + prior_epsilon)
  }
  
  ## make NQT transformation
  TransNQT <- NQT(N = 1000, factor.sample = 0.5, qobs = as.numeric(calobs), qsim = as.numeric(calsim))
  TransNQTval <- NQT(N = 1000, factor.sample = 0.5, qobs = as.numeric(valobs), qsim = as.numeric(valsim))
  
  
  # check NQT transformation calibration
  # plot(TransNQT$etaS, TransNQT$eta, pch = 19, cex = 0.5)
  # qqplot(TransNQT$etaS, TransNQT$eta)     # distribution is from N(0,1)
  # abline(0,1)
  # hist(TransNQT$eta)
  # hist(TransNQT$etaS)
  # data from calibration to post-processing
  #datos <- cbind(as.numeric(observations), as.numeric(simulations))
  datos <- cbind(TransNQT$eta, TransNQT$etaS)
  #datos <- cbind(TransNQT$eta, TransNQT$etaS)
  #pars <- c(theta.1, theta.2, sd_e)  # organize the parameters in a vector
  mcmc.lrm <- Metro_Hastings(li_func = li_reg, pars = c(0,1,1), par_names = c('a','b','epsilon'), data = datos) #pars = c(0,1,1)
  # For best results, run again with the previously
  # adapted variance-covariance matrix.
  mcmc.lrm <- Metro_Hastings(li_func = li_reg, pars = c(0,1,1), prop_sigma = mcmc.lrm$prop_sigma, par_names = c('a','b','epsilon'), data = datos)
  # plot the posterior and trace
  # plotMH(mcmc.lrm)
  # dev.off()
  # mcmc.lrm <- mcmc_thin(mcmc_object = mcmc.lrm, thin = 5)
  # plotMH(mcmc.lrm)
  ran_aprimcmc <- summary(mcmc.lrm$trace)
  (ran_aprimcmc)
  parerrormod <- c(median(mcmc.lrm$trace[,1]), median(mcmc.lrm$trace[,2]), median(mcmc.lrm$trace[,3]))
  
  # save(mcmc.lrm, file = "PostPredMCMC.RData")
  # browser()
  ## Estimate the predective posterior distribution by MCMC
  print("Compute Posterior Predictive Distribution from MCMC for calibration")
  PostPredMCMC <- PostPredDist(forecastN = datos[,2], postparam = mcmc.lrm$trace, factor.sample = 0.5, forecast = as.numeric(calobs))
  save(PostPredMCMC, file = "PostPredMCMC.RData")
  
  # # 6) Estimate the predective posterior distribution by ABF
  # forecastN <- datos[,2]
  # postparam <- mcmc.lrm$trace
  # ypred <- matrix(NA, length(forecastN), nrow(postparam))  #matrix(observations,posterior), crate a matrix for predictions
  # for(i in  1:length(forecastN)){
  #   for(j in 1:nrow(postparam)){
  #     # sample rnorm(n, mean = linear model, sd = error parameter estimate by MCMC )
  #     ypred[i, j] <- rnorm(1, postparam[j,1] + postparam[j,2] * forecastN[i], postparam[j,3])
  #   }
  # }
  # # identify negative numbers
  # length(ypred[ypred <0]) >0 # if we have negative numbers we get TRUE
  # # eliminate negative values
  # ypred[ypred <0] <- 0
  # any(is.na(ypred))
  # PosteriorPred <-t(ypred)     #transpose matrix
  # save(PosteriorPred, file = "PostPredCalMCMC.RData")
  #browser()
  # compute statistiscs of posterior predictive
  Est.PosteriorMCMC <- Edit.Posterior(observations = as.numeric(calobs), posterior = PostPredMCMC)
  save(Est.PosteriorMCMC, file = "Est.PosteriorMCMC.RData")
  
  # Predective posterior Validation period
  print("Compute Posterior Predictive Distribution from MCMC for validation")
  PostPredVal <- PostPredDistVal(simvalN = TransNQTval$etaS, parerrormod = parerrormod, factor.sample = 0.5, simval = as.numeric(valobs), NP = 1000)
  EstPostPredVal <- Edit.Posterior(observations = as.numeric(valobs), posterior = PostPredVal)
  save(PostPredVal, file = "PostPredVal.RData")
  save(EstPostPredVal, file = "EstPostPredVal.RData")
  # beep(8)
  # browser()
  #  Performance metrics 
  QQplot <- QQtest(discharge = as.numeric(calobs), posterior = PostPredMCMC)
  NSE <- Nash(obs = as.numeric(calobs), sim = Est.PosteriorMCMC[,"median"]) 
  KGE <- klinggupta(obs = as.numeric(calobs), sim = Est.PosteriorMCMC[,"median"])
  CottonTest2 <- TestUncertBand(observations = as.numeric(calobs), quantil5 = Est.PosteriorMCMC[,"quan2_5"], quantil95 = Est.PosteriorMCMC[,"quan97_5"])
  PPU95 <- UncertaintyBand95(observations = as.numeric(calobs), quantil5 = Est.PosteriorMCMC[,"quan2_5"], quantil95 = Est.PosteriorMCMC[,"quan97_5"])
  # Uniformity Test calibration
  UnifTestCal <-ks.test(QQplot$zi,'punif')
  # Ho : data are from U(0,1)
  # Ha : data are not from U(0,1)
  # p-value : 0.8106 > 0.05: accept Ho, data from U(0,1)
  
  # validation period
  QQplotv <- QQtest(discharge = as.numeric(valobs), posterior = PostPredVal)
  NSEv <- Nash(obs = as.numeric(valobs), sim = EstPostPredVal[,"median"]) 
  KGEv <- klinggupta(obs = as.numeric(valobs), sim = EstPostPredVal[,"median"])
  CottonTest2v <- TestUncertBand(observations = as.numeric(valobs), quantil5 = EstPostPredVal[,"quan2_5"], quantil95 = EstPostPredVal[,"quan97_5"])
  PPU95v <- UncertaintyBand95(observations = as.numeric(valobs), quantil5 = EstPostPredVal[,"quan2_5"], quantil95 = EstPostPredVal[,"quan97_5"])
  # Uniformity Test calibration
  UnifTestValABCv <-ks.test(QQplotv$zi,'punif')
  
  # Plots 
  # Parametric Posterior
  #browser()
  jpeg("./Fig1ParamePosterior.jpg", res = 600, width = 20, height = 12, units = "in")
  par(mfcol = c(1,4),mar = c(5.5,5.9,0.5,0.5), mgp = c(4,1,0))
  plot(density(mcmc.lrm$trace[,1]), xlab = expression(beta[0]), cex.lab = 2.5, cex.axis = 2.5, main = "", ylab = "Density",  lwd = 2) #ylim = c(0,20), xlim = c(-27,-5),
  plot(density(mcmc.lrm$trace[,2]), xlab = expression(beta[1]), cex.axis = 2.5, cex.lab = 2.5, main = "", ylab = "", lwd = 2) # ylim = c(0,20), xlim = c(0.5,1.2),
  plot(density(mcmc.lrm$trace[,3]), xlab = expression(epsilon), cex.axis = 2.5, cex.lab = 2.5, main = "", ylab = "",  lwd = 2) # ylim = c(0,max(hist(my.rej$unadj.values[,3], freq = F, plot = F)$density)), xlim = c(0,0.9),
  plot(density(PostPredMCMC[,5]), xlab = expression(paste("Streamflow"~ (m^3~s^-1))), main = "", ylab = "", cex.axis = 2.5, cex.lab = 2.5,  lwd = 2) # "discharge [m3/s] = 2.46"xlim = c(0,50),
  dev.off()
  # Predictive Uncertainty
  # browser()
  # jpeg("./Fig3MonthlyPredUncer.jpg", res = 600, width = 20, height = 12, units = "in")
  # layout(matrix(c(1,1,2,3,3,4), 1, 3, byrow = TRUE))
  # par(mar = c(5.5,6,0.5,0.5))
  # plot(time(simulations), as.numeric(simulations), type ="n", xlab="", ylab=expression(paste("Streamflow"~ (m^3~s^-1))),  cex.axis = 2.5, cex.lab = 2.5)#xaxt="n",
  # #axis(1, at=1:12, labels = month.abb, las = 2,cex.axis = 2.5)
  # polygon(c(time(simulations), rev(time(simulations))), c(Est.PosteriorMCMC[,"quan2_5"], rev(Est.PosteriorMCMC[,"quan97_5"])), col='grey', border=F)
  # points(time(observations), as.numeric(observations), col="red", pch =21, bg="red", cex = 1.5)
  # lines(time(observations), Est.PosteriorMCMC[,"median"],lty =1, lwd = 3, col="black")
  # #lines(1:12, as.numeric(modelCC), col="blue", lwd = 2)
  # qqplot(QQplot$x, QQplot$zi, cex.axis = 2.5, xlim = c(0,1),ylim = c(0,1),cex.lab = 2.5, xlab="Theoretical Quantile U[0,1]",ylab="Quantile of forecast", col = "blue",type = "l", pch=20, lty=1, lwd = 3)
  # abline(0,1,lwd=2)
  # dev.off()
  
  return(list(posterior = PostPredMCMC, NSE = NSE, KGE = KGE, UnifTest = UnifTestCal$p.value, PoinsInsideBand = CottonTest2, Est.Posterior = Est.PosteriorMCMC, PPU95 = PPU95, QQplot = QQplot, mcmcpar =ran_aprimcmc, posteriorv = PostPredVal, NSEv = NSEv, KGEv = KGEv, UnifTestv = UnifTestValABCv$p.value,  PoinsInsideBandv = CottonTest2v, Est.Posteriorv = EstPostPredVal, PPU95v = PPU95v, QQplotv = QQplotv))
}

# 5. MCP postprocessor -----------------------------------------------------------------------------------


MCP2D <- function(calobs, calsim, valobs, valsim, N){
  # Aim: Compute a predictive uncertainty in real space using the MCP approach
  # Inputs: calobs, calsim, valobs, valsim are observations and simulations for calibration and validation period respectively,  are time series object;
  # Outputs: posterior predictive uncertainty and metrics
  # Calls:  B1MCP <- MCP2D(calobs = as.numeric(B1qocal), calsim = as.numeric(B1qscal), valobs = as.numeric(B1qoval), valsim = as.numeric(B1qsval), N = 1000) 
  
  # 1. Make NQT transformation for calibration and validation period
  TransNQT <- NQT(N = 1000, factor.sample = 0.5, qobs = as.numeric(calobs), qsim = as.numeric(calsim))
  TransNQTval <- NQT(N = 1000, factor.sample = 0.5, qobs = as.numeric(valobs), qsim = as.numeric(valsim))
  
  # 2. Estimating moments from conditional pdf (bivariate gaussian distribution)
  Mat.eta <- cbind(TransNQT$eta, TransNQT$etaS)
  MatCov <- cov(x = Mat.eta)   # Variance - covariance matix
  AuxMat <- MatCov[1,2] * (MatCov[2,2]^ -1)
  GausConMean <- mean(TransNQT$eta) + (TransNQT$etaS - mean(TransNQT$etaS)) * AuxMat
  GausConVar <- var(TransNQT$eta) - MatCov[1,2] * AuxMat
  
  # 2.1 parmeters for validation
  GausConMeanv <- mean(TransNQT$eta) + (TransNQTval$etaS - mean(TransNQTval$etaS)) * AuxMat
  
  # 3. Compute condictional predictive distribution (PD)
  
  PosteriorPred <- function(meancond, varcond, simval, N = 1000, factor.sample = 0.5){
    # Aim: Compute a predictive uncertainty in real space using MCP parameters
    # Inputs: meancond = mean conditional vector, varcond = variance conditional parameter, simval = observations in calibration or simulations in validation,
    # Outputs: posterior predictive uncertainty 
    # Calls: calibration::  PostPredMCP <- PosteriorPred (meancond = GausConMean, varcond = GausConVar, simval = calobs, N = 1000, factor.sample = 0.5)
    # Calls: validation::  PostPredMCPv <- PosteriorPred (meancond = GausConMeanv, varcond = GausConVar, simval = valsim, N = 1000, factor.sample = 0.5)
    
    # function for very low streamflows i.e Guadalupe cathment
    qkde2 <- function (p, fhat) 
    {
      if (any(p > 1) | any(p < 0)) 
        stop("p must be <= 1 and >= 0")
      cumul.prob <- pkde(q = fhat$eval.points, fhat = fhat)
      ind <- findInterval(x = p, vec = sort(cumul.prob))
      quant <- rep(0, length(ind))
      for (j in 1:length(ind)) {
        i <- ind[j]
        if (i == 0) 
          quant[j] <- fhat$eval.points[1]
        else if (i >= length(fhat$eval.points)) 
          quant[j] <- fhat$eval.points[length(fhat$eval.points)]
        else {
          quant1 <- fhat$eval.points[i]
          quant2 <- fhat$eval.points[i + 1]
          prob1 <- cumul.prob[i]
          prob2 <- cumul.prob[i + 1]
          alpha <- (p[j] - prob2)/(prob1 - prob2)
          quant[j] <- quant1 * alpha + quant2 * (1 - alpha)
        }
      }
      return(quant)
    }
    
    
  # Compute condictional predictive distribution (PD) in Normal space
  # matrix to compute PD matrix(posterior, time(lenght of data))
  ypred <- matrix(data = NA, nrow = N, ncol = length(meancond))
  for (i in 1:length(meancond)) {
    ypred[,i] <- rnorm(n = N, mean = meancond[i], sd = (varcond ^ 0.5))
  }
  
  # 4. Inverse NQT for predictive distribution
  # posterior predictive distribution normal space
  posteriorN <- matrix(data = NA, nrow = nrow(ypred), ncol = ncol(ypred))
  for (k in 1:nrow(ypred)) {
    posteriorN[k,] <- pnorm(q = ypred[k,], mean = 0, sd = 1)
  }
  # posterior predictive distribution real space with resample   
  posteriorR <- matrix(data = NA, nrow = nrow(ypred), ncol = ncol(ypred))
  for (k in 1:nrow(ypred)) {
    posteriorR[k,] <- qkde(p = posteriorN[k,], fhat = kde(x = simval, binned = TRUE ))
    #posteriorR[k,] <- qkde2(p = posteriorN[k,], fhat = kde(x = simval, binned = TRUE )) # just for low flow near to zero
  }
  # posterior predictive distribution real space without resample  
  # invert matrix (posterior,time)
  #factor.sample <- 0.5 # factor resample for NQT
  tsample2 <- seq(from = 1, to = length(meancond), by = 1/factor.sample)
  posterior <- matrix(data = NA, nrow = N, ncol = length(tsample2)) #¡¡¡¡ojo¡¡¡¡ 
  #posteriorR2 <-t(posteriorR)
  #browser()
  for (k in 1:nrow(posterior)) {
    posterior[k,] <- posteriorR [k,tsample2] 
  }
  return(posterior)
  }
  
  # compute predictive distribution calibration
  print("Compute Posterior Predictive Distribution MCP for calibration")
  PostPredMCP <- PosteriorPred (meancond = GausConMean, varcond = GausConVar, simval = as.numeric(calobs), N = 1000, factor.sample = 0.5)
  save(PostPredMCP, file = "PostPredMCP.RData")
  # compute statistiscs of posterior predictive
  Est.PosteriorMCP <- Edit.Posterior(observations = as.numeric(calobs), posterior = PostPredMCP)
  save(Est.PosteriorMCP, file = "Est.PosteriorMCP.RData")
  
  # Predective posterior Validation period
  print("Compute Posterior Predictive Distribution MCP for validation")
  #PostPredMCPval <- PosteriorPred (meancond = GausConMeanv, varcond = GausConVar, simval = as.numeric(valsim), N = 1000, factor.sample = 0.5)
  PostPredMCPval <- PosteriorPred (meancond = GausConMeanv, varcond = GausConVar, simval = as.numeric(valobs), N = 1000, factor.sample = 0.5)
  EstPosteriorMCPval <- Edit.Posterior(observations = as.numeric(valobs), posterior = PostPredMCPval)
  save(PostPredMCPval, file = "PostPredMCPval.RData")
  save(EstPosteriorMCPval, file = "EstPosteriorMCPval.RData")
  #browser()
  #  Performance metrics
  QQplot <- QQtest(discharge = as.numeric(calobs), posterior = PostPredMCP)
  NSE <- Nash(obs = as.numeric(calobs), sim = Est.PosteriorMCP[,"median"]) 
  KGE <- klinggupta(obs = as.numeric(calobs), sim = Est.PosteriorMCP[,"median"])
  CottonTest2 <- TestUncertBand(observations = as.numeric(calobs), quantil5 = Est.PosteriorMCP[,"quan2_5"], quantil95 = Est.PosteriorMCP[,"quan97_5"])
  PPU95 <- UncertaintyBand95(observations = as.numeric(calobs), quantil5 = Est.PosteriorMCP[,"quan2_5"], quantil95 = Est.PosteriorMCP[,"quan97_5"])
  # Uniformity Test calibration
  UnifTestCal <-ks.test(QQplot$zi,'punif')
  # Ho : data are from U(0,1)
  # Ha : data are not from U(0,1)
  # p-value : 0.8106 > 0.05: accept Ho, data from U(0,1)
  
  # validation period
  QQplotv <- QQtest(discharge = as.numeric(valobs), posterior = PostPredMCPval)
  NSEv <- Nash(obs = as.numeric(valobs), sim = EstPosteriorMCPval[,"median"]) 
  KGEv <- klinggupta(obs = as.numeric(valobs), sim = EstPosteriorMCPval[,"median"])
  CottonTest2v <- TestUncertBand(observations = as.numeric(valobs), quantil5 = EstPosteriorMCPval[,"quan2_5"], quantil95 = EstPosteriorMCPval[,"quan97_5"])
  PPU95v <- UncertaintyBand95(observations = as.numeric(valobs), quantil5 = EstPosteriorMCPval[,"quan2_5"], quantil95 = EstPosteriorMCPval[,"quan97_5"])
  # Uniformity Test calibration
  UnifTestVal <-ks.test(QQplotv$zi,'punif')
  
  return(list(posterior = PostPredMCP, NSE = NSE, KGE = KGE, UnifTest = UnifTestCal$p.value, PoinsInsideBand = CottonTest2, Est.Posterior = Est.PosteriorMCP, PPU95 = PPU95, QQplot = QQplot, posteriorv = PostPredMCPval, NSEv = NSEv, KGEv = KGEv, UnifTestv = UnifTestVal$p.value,  PoinsInsideBandv = CottonTest2v, Est.Posteriorv = EstPosteriorMCPval, PPU95v = PPU95v, QQplotv = QQplotv))
}

# 6. MCP Truncated Postprocessor  -----------------------------------------------------------------------------------

 
Truncamiento <- function(calobs, calsim, inipoints) {
  # Aim: Find the trucated point
  # Inputs: calobs, calsim,  are observations and simulations for calibration period respectively,  are time series object;
  #          inipoints : range to search truncated point from scatter plot c(-1,1)
  # Outputs: Separate samples up and low with indices
  # call: B1trun <- Truncamiento(calobs = as.numeric(B1qocal), calsim = as.numeric(B1qscal), inipoints = c(-1,1.5))
  
  TransNQT <- NQT(N = 1000, factor.sample = 0.5, qobs = as.numeric(calobs), qsim = as.numeric(calsim))
  a <- NA        # initial value store trunkpoint
  varup <- 1000000 # optimization by brute force so hight value to into the cycle 
  delta <- 0.01    # step to move on optimization
  j <- 1
  trunkpoint <- inipoints[1] 
  # order the data
  sortetaS <- sort(TransNQT$etaS, index.return=TRUE)  
  # extract only the index : sort(TransNQT$etaS, index.return=TRUE)$ix
  sorteta <- TransNQT$eta[sortetaS$ix] # order according sortetaS
  
  
  while (trunkpoint < inipoints[2]) {
    trunkidx <- which(sortetaS$x >= trunkpoint)
    upsample <- sorteta[trunkidx]
    upsampleS <- sortetaS$x[trunkidx]
    
    dist <- length(sorteta) - length(trunkidx)
    lowsample <- sorteta[1:dist]
    lowsampleS <- sortetaS$x[1:dist]
    
    if (is.na(trunkpoint)){
      print("Change range initial trunked point")
    }else{
      # below sample
      Mat.etalow <- cbind(lowsample, lowsampleS)
      MatCovlow <- cov(x = Mat.etalow)   # Variance - covariance matix
      AuxMatlow <- MatCovlow[1,2] * (MatCovlow[2,2]^ -1)
      GausConMeanlow <- mean(lowsample) + (lowsampleS - mean(lowsampleS)) * AuxMatlow
      GausConVarlow <- var(lowsample) - MatCovlow[1,2] * AuxMatlow
      # Up sample
      Mat.etaup <- cbind(upsample, upsampleS)
      MatCovup <- cov(x = Mat.etaup)   # Variance - covariance matix
      AuxMatup <- MatCovup[1,2] * (MatCovup[2,2]^ -1)
      GausConMeanup <- mean(upsample) + (upsampleS - mean(upsampleS)) * AuxMatup
      GausConVarup <- var(upsample) - MatCovup[1,2] * AuxMatup
      # 
      # varstoreup[j] <- GausConVarup
      # varstorelow(j) <- GausConVarlow
      # trunkpointstore <- trunkpoint
    }
    
    if (is.na(GausConVarup)){
      print("the up sample is to small so change range")
      browser()
    }
    
    if (GausConVarup < varup){
      varup <- GausConVarup
      varlow <- GausConVarlow
      a <- trunkpoint
    }
    j <- j+1
    trunkpoint <- trunkpoint + delta
    
  }
  
  idxup <- which(TransNQT$etaS > a)
  idxlow <- which(TransNQT$etaS <= a)
  
  upsample <- TransNQT$eta[idxup]
  lowsample <- TransNQT$eta[idxlow]
  
  upsampleS <- TransNQT$etaS[idxup]
  lowsampleS <- TransNQT$etaS[idxlow]
  
  return(list(trunkedpoint = a, upsample = upsample, lowsample = lowsample, upsampleS = upsampleS, lowsampleS = lowsampleS, idxup = idxup, idxlow = idxlow))
  
}

MCP2Dtrun <- function(calobs, calsim, valobs, valsim, N, inipoints, factor.sample){
  # Aim: Compute a predictive uncertainty in real space using the MCP truncated approach
  # Inputs: calobs, calsim, valobs, valsim are observations and simulations for calibration and validation period respectively,  are time series object;
  #         N : # samples from predictive uncertainty; inipoints : range to search truncated point from scatter plot c(-1,1)
  # Outputs: posterior predictive uncertainty and metrics
  # Calls:  B1MCPt <- MCP2Dtrun(calobs = as.numeric(B1qocal), calsim = as.numeric(B1qscal), valobs = as.numeric(B1qoval), valsim = as.numeric(B1qsval), N = 1000, inipoints = c(-1,1.5), factor.sample = 0.5) 
  
  # 1. Make NQT transformation for calibration and validation period
  TransNQT <- NQT(N = 1000, factor.sample = 0.5, qobs = as.numeric(calobs), qsim = as.numeric(calsim))
  TransNQTval <- NQT(N = 1000, factor.sample = 0.5, qobs = as.numeric(valobs), qsim = as.numeric(valsim))
  
  # Scatterplot to check the range of inipoints
  jpeg("./Fig0Scatterplot.jpg", res = 600, width = 20, height = 12, units = "in")
  plot(TransNQT$eta, TransNQT$etaS)
  dev.off()
  # 2. Separate samples up and low
  separate <- Truncamiento(calobs = calobs, calsim = calsim, inipoints = inipoints)
  
  # 3. Estimating moments from conditional pdf (bivariate gaussian distribution) for up and low sample
  # 3.1 below sample
  Mat.etalow <- cbind(separate$lowsample, separate$lowsampleS)
  MatCovlow <- cov(x = Mat.etalow)   # Variance - covariance matix
  AuxMatlow <- MatCovlow[1,2] * (MatCovlow[2,2]^ -1)
  GausConMeanlow <- mean(separate$lowsample) + (separate$lowsampleS - mean(separate$lowsampleS)) * AuxMatlow
  GausConVarlow <- var(separate$lowsample) - MatCovlow[1,2] * AuxMatlow
  # 3.2 Up sample
  Mat.etaup <- cbind(separate$upsample, separate$upsampleS)
  MatCovup <- cov(x = Mat.etaup)   # Variance - covariance matix
  AuxMatup <- MatCovup[1,2] * (MatCovup[2,2]^ -1)
  GausConMeanup <- mean(separate$upsample) + (separate$upsampleS - mean(separate$upsampleS)) * AuxMatup
  GausConVarup <- var(separate$upsample) - MatCovup[1,2] * AuxMatup
  # conditional mean for validation 
  GausConMeanv <- mean(TransNQT$eta) + (TransNQTval$etaS - mean(TransNQTval$etaS)) * AuxMatlow
  
  # 4. Compute condictional predictive distribution (PD)
  # for validation
  PosteriorPred <- function(meancond, varcond, simval, N = 1000, factor.sample = 0.5){
    # Aim: Compute a predictive uncertainty in real space using MCP parameters
    # Inputs: meancond = mean conditional vector, varcond = variance conditional parameter, simval = observations in calibration or simulations in validation,
    # Outputs: posterior predictive uncertainty (posterior, time)
    # Calls: calibration::  PostPredMCP <- PosteriorPred (meancond = GausConMean, varcond = GausConVar, simval = calobs, N = 1000, factor.sample = 0.5)
    # Calls: validation::  PostPredMCPv <- PosteriorPred (meancond = GausConMeanv, varcond = GausConVar, simval = valsim, N = 1000, factor.sample = 0.5)
    
    # function for very low streamflows i.e Guadalupe cathment
    qkde2 <- function (p, fhat) 
    {
      if (any(p > 1) | any(p < 0)) 
        stop("p must be <= 1 and >= 0")
      cumul.prob <- pkde(q = fhat$eval.points, fhat = fhat)
      ind <- findInterval(x = p, vec = sort(cumul.prob))
      quant <- rep(0, length(ind))
      for (j in 1:length(ind)) {
        i <- ind[j]
        if (i == 0) 
          quant[j] <- fhat$eval.points[1]
        else if (i >= length(fhat$eval.points)) 
          quant[j] <- fhat$eval.points[length(fhat$eval.points)]
        else {
          quant1 <- fhat$eval.points[i]
          quant2 <- fhat$eval.points[i + 1]
          prob1 <- cumul.prob[i]
          prob2 <- cumul.prob[i + 1]
          alpha <- (p[j] - prob2)/(prob1 - prob2)
          quant[j] <- quant1 * alpha + quant2 * (1 - alpha)
        }
      }
      return(quant)
    }
    
    
    # Compute condictional predictive distribution (PD) in Normal space
    # matrix to compute PD matrix(posterior, time(lenght of data))
    ypred <- matrix(data = NA, nrow = N, ncol = length(meancond))
    for (i in 1:length(meancond)) {
      ypred[,i] <- rnorm(n = N, mean = meancond[i], sd = (varcond ^ 0.5))
    }
    
    # 4. Inverse NQT for predictive distribution
    # posterior predictive distribution normal space
    posteriorN <- matrix(data = NA, nrow = nrow(ypred), ncol = ncol(ypred))
    for (k in 1:nrow(ypred)) {
      posteriorN[k,] <- pnorm(q = ypred[k,], mean = 0, sd = 1)
    }
    # posterior predictive distribution real space with resample   
    posteriorR <- matrix(data = NA, nrow = nrow(ypred), ncol = ncol(ypred))
    for (k in 1:nrow(ypred)) {
      posteriorR[k,] <- qkde(p = posteriorN[k,], fhat = kde(x = simval, binned = TRUE ))
      #posteriorR[k,] <- qkde2(p = posteriorN[k,], fhat = kde(x = simval, binned = TRUE )) # just for low flow near to zero
    }
    # posterior predictive distribution real space without resample  
    # invert matrix (posterior,time)
    #factor.sample <- 0.5 # factor resample for NQT
    tsample2 <- seq(from = 1, to = length(meancond), by = 1/factor.sample)
    posterior <- matrix(data = NA, nrow = N, ncol = length(tsample2)) #¡¡¡¡ojo¡¡¡¡ 
    #posteriorR2 <-t(posteriorR)
    #browser()
    for (k in 1:nrow(posterior)) {
      posterior[k,] <- posteriorR [k,tsample2] 
    }
    return(posterior)
  }
  
  PosteriorPredt <- function(meancondlow, varcondlow, meancondup, varcondup, simval, N = 1000, factor.sample = 0.5, idxlow, idxup){
    # Aim: Compute a predictive uncertainty in real space using MCPt parameters, do MCP two times for low and up samples next joint by order 
    # Inputs: meancondlow = mean conditional vector for low sample, varcondlow = variance conditional parameter for low sample, simval = observations in calibration or simulations in validation,
    #        meancondup, varcondup are the same but for up sample; idxlow, idxup are order index for low and up samples
    # Outputs: posterior predictive uncertainty (posterior, time)
    # Calls: calibration::  PostPredMCPt <- PosteriorPredt (meancondlow = GausConMeanlow, varcondlow = GausConVarlow, meancondup = GausConMeanup, varcondup = GausConVarup, simval = calobs, N = 1000, factor.sample = 0.5, idxlow = separate$idxlow, idxup = separate$idxup)
    
    # function for very low streamflows i.e Guadalupe cathment
    qkde2 <- function (p, fhat) 
    {
      if (any(p > 1) | any(p < 0)) 
        stop("p must be <= 1 and >= 0")
      cumul.prob <- pkde(q = fhat$eval.points, fhat = fhat)
      ind <- findInterval(x = p, vec = sort(cumul.prob))
      quant <- rep(0, length(ind))
      for (j in 1:length(ind)) {
        i <- ind[j]
        if (i == 0) 
          quant[j] <- fhat$eval.points[1]
        else if (i >= length(fhat$eval.points)) 
          quant[j] <- fhat$eval.points[length(fhat$eval.points)]
        else {
          quant1 <- fhat$eval.points[i]
          quant2 <- fhat$eval.points[i + 1]
          prob1 <- cumul.prob[i]
          prob2 <- cumul.prob[i + 1]
          alpha <- (p[j] - prob2)/(prob1 - prob2)
          quant[j] <- quant1 * alpha + quant2 * (1 - alpha)
        }
      }
      return(quant)
    }
    
    # Compute condictional predictive distribution (PD) in Normal space
    # matrix to compute PD matrix(posterior, time(lenght of data))
    # low sample
    ypredlow <- matrix(data = NA, nrow = N, ncol = length(meancondlow))
    for (i in 1:length(meancondlow)) {
      ypredlow[,i] <- rnorm(n = N, mean = meancondlow[i], sd = (varcondlow ^ 0.5))
    }
    # up sample
    ypredup <- matrix(data = NA, nrow = N, ncol = length(meancondup))
    for (i in 1:length(meancondup)) {
      ypredup[,i] <- rnorm(n = N, mean = meancondup[i], sd = (varcondup ^ 0.5))
    }
    # 4. Inverse NQT for predictive distribution
    # posterior predictive distribution normal space
    # low sample
    posteriorNlow <- matrix(data = NA, nrow = nrow(ypredlow), ncol = ncol(ypredlow))
    for (k in 1:nrow(ypredlow)) {
      posteriorNlow[k,] <- pnorm(q = ypredlow[k,], mean = 0, sd = 1)
    }
    # up sample
    posteriorNup <- matrix(data = NA, nrow = nrow(ypredup), ncol = ncol(ypredup))
    for (k in 1:nrow(ypredup)) {
      posteriorNup[k,] <- pnorm(q = ypredup[k,], mean = 0, sd = 1)
    }
    # posterior predictive distribution real space with resample   
    # low sample
    posteriorRlow <- matrix(data = NA, nrow = nrow(ypredlow), ncol = ncol(ypredlow))
    for (k in 1:nrow(ypredlow)) {
      posteriorRlow[k,] <- qkde(p = posteriorNlow[k,], fhat = kde(x = simval, binned = TRUE ))
      #posteriorRlow[k,] <- qkde2(p = posteriorNlow[k,], fhat = kde(x = simval, binned = TRUE )) # just for low flow near to zero
    }
    # up sample
    posteriorRup <- matrix(data = NA, nrow = nrow(ypredup), ncol = ncol(ypredup))
    for (k in 1:nrow(ypredup)) {
      posteriorRup[k,] <- qkde(p = posteriorNup[k,], fhat = kde(x = simval, binned = TRUE ))
      #posteriorRup[k,] <- qkde2(p = posteriorNup[k,], fhat = kde(x = simval, binned = TRUE )) # just for up flow near to zero
    }
    
    # posterior predictive distribution real space without resample  
    # invert matrix (posterior,time)
    #factor.sample <- 0.5 # factor resample for NQT
    #browser()
    # # transform order to real space
    posteriorAux <- matrix(data = NA, nrow = N, ncol = dim(posteriorRup)[2]+dim(posteriorRlow)[2])  
     # low sample
    for (i in 1:length(idxlow)) {
      posteriorAux[,idxlow[i]] <- posteriorRlow[,i]
    }
    # up sample
    for (i in 1:length(idxup)) {
      posteriorAux[,idxup[i]] <- posteriorRup[,i]
    }
    
    tsample2 <- seq(from = 1, to = dim(posteriorAux)[2], by = 1/factor.sample)
    posterior <- matrix(data = NA, nrow = N, ncol = length(tsample2)) #¡¡¡¡ojo¡¡¡¡ 
    #posteriorR2 <-t(posteriorR)
    #browser()
    for (k in 1:nrow(posterior)) {
      posterior[k,] <- posteriorAux [k,tsample2] 
    }
    return(posterior)
  }
  
  
  # compute predictive distribution calibration
  print("Compute Posterior Predictive Distribution MCPt for calibration")
  PostPredMCP <- PosteriorPredt(meancondlow = GausConMeanlow, varcondlow = GausConVarlow, meancondup = GausConMeanup, varcondup = GausConVarup, simval = calobs, N = 1000, factor.sample = 0.5, idxlow = separate$idxlow, idxup = separate$idxup)

  save(PostPredMCP, file = "PostPredMCP.RData")
  # compute statistiscs of posterior predictive
  Est.PosteriorMCP <- Edit.Posterior(observations = as.numeric(calobs), posterior = PostPredMCP)
  save(Est.PosteriorMCP, file = "Est.PosteriorMCP.RData")
  
  # Predective posterior Validation period
  print("Compute Posterior Predictive Distribution MCPt for validation")
  PostPredMCPval <- PosteriorPred (meancond = GausConMeanv, varcond = GausConVarlow, simval = as.numeric(valobs), N = 1000, factor.sample = 0.5)
  EstPosteriorMCPval <- Edit.Posterior(observations = as.numeric(valobs), posterior = PostPredMCPval)
  save(PostPredMCPval, file = "PostPredMCPval.RData")
  save(EstPosteriorMCPval, file = "EstPosteriorMCPval.RData")
  #browser()
  #  Performance metrics
  QQplot <- QQtest(discharge = as.numeric(calobs), posterior = PostPredMCP)
  NSE <- Nash(obs = as.numeric(calobs), sim = Est.PosteriorMCP[,"median"]) 
  KGE <- klinggupta(obs = as.numeric(calobs), sim = Est.PosteriorMCP[,"median"])
  CottonTest2 <- TestUncertBand(observations = as.numeric(calobs), quantil5 = Est.PosteriorMCP[,"quan2_5"], quantil95 = Est.PosteriorMCP[,"quan97_5"])
  PPU95 <- UncertaintyBand95(observations = as.numeric(calobs), quantil5 = Est.PosteriorMCP[,"quan2_5"], quantil95 = Est.PosteriorMCP[,"quan97_5"])
  # Uniformity Test calibration
  UnifTestCal <-ks.test(QQplot$zi,'punif')
  # Ho : data are from U(0,1)
  # Ha : data are not from U(0,1)
  # p-value : 0.8106 > 0.05: accept Ho, data from U(0,1)
  
  # validation period
  QQplotv <- QQtest(discharge = as.numeric(valobs), posterior = PostPredMCPval)
  NSEv <- Nash(obs = as.numeric(valobs), sim = EstPosteriorMCPval[,"median"]) 
  KGEv <- klinggupta(obs = as.numeric(valobs), sim = EstPosteriorMCPval[,"median"])
  CottonTest2v <- TestUncertBand(observations = as.numeric(valobs), quantil5 = EstPosteriorMCPval[,"quan2_5"], quantil95 = EstPosteriorMCPval[,"quan97_5"])
  PPU95v <- UncertaintyBand95(observations = as.numeric(valobs), quantil5 = EstPosteriorMCPval[,"quan2_5"], quantil95 = EstPosteriorMCPval[,"quan97_5"])
  # Uniformity Test calibration
  UnifTestVal <-ks.test(QQplotv$zi,'punif')
  
  return(list(posterior = PostPredMCP, NSE = NSE, KGE = KGE, UnifTest = UnifTestCal$p.value, PoinsInsideBand = CottonTest2, Est.Posterior = Est.PosteriorMCP, PPU95 = PPU95, QQplot = QQplot, posteriorv = PostPredMCPval, NSEv = NSEv, KGEv = KGEv, UnifTestv = UnifTestVal$p.value,  PoinsInsideBandv = CottonTest2v, Est.Posteriorv = EstPosteriorMCPval, PPU95v = PPU95v, QQplotv = QQplotv))
  
}


# 7. MCP Gaussian Mixture Cluster Postprocessor (GMM) -----------------------------------------------------------------------------------
# This post-processor run in matlab so we load the predictive posterior  and compute statistics
setwd("F:/TesinaMasterCoDireccion")
# load posterior from GMM post-processor for calibration and validation period
load("POSTERIOR-CAL- VAL.RData")
# Save results of GMM post-processor in a new folder
setwd("F:/TesinaMasterCoDireccion/ResultGMM")

GMMPostEstMetrics <- function(calobs, calsim, valobs, valsim, posteriorcal, posteriorval){
  # Aim: Compute output from the GMM post-processor
  # Inputs: calobs, calsim, valobs, valsim are observations and simulations for calibration and validation period respectively,  are time series object;
  #         posteriorcal: matrix posterior (posteior, time) for calibration, posteriorval for validation period
  # Outputs: statistics of posterior predictive  and metrics
  # Calls:  B1GMM <- GMMPostEstMetrics(calobs = as.numeric(B1qocal), calsim = as.numeric(B1qscal), valobs = as.numeric(B1qoval), valsim = as.numeric(B1qsval), posteriorcal = posteriorgc1cal, posteriorval = posteriorgc1v) 
  
  # compute statistiscs of posterior predictive calibration
  EstPosteriorcal <- Edit.Posterior(observations = as.numeric(calobs), posterior = posteriorcal)
  #save(Est.PosteriorGMM, file = "Est.PosteriorGMM.RData")
  # compute statistiscs of posterior predictive validation
  EstPosteriorval <- Edit.Posterior(observations = as.numeric(valobs), posterior = posteriorval)
  #save(EstPosteriorMCPval, file = "EstPosteriorMCPval.RData")
  
  #  Performance metrics
  QQplot <- QQtest(discharge = as.numeric(calobs), posterior = posteriorcal)
  NSE <- Nash(obs = as.numeric(calobs), sim = EstPosteriorcal[,"median"]) 
  KGE <- klinggupta(obs = as.numeric(calobs), sim = EstPosteriorcal[,"median"])
  CottonTest2 <- TestUncertBand(observations = as.numeric(calobs), quantil5 = EstPosteriorcal[,"quan2_5"], quantil95 = EstPosteriorcal[,"quan97_5"])
  PPU95 <- UncertaintyBand95(observations = as.numeric(calobs), quantil5 = EstPosteriorcal[,"quan2_5"], quantil95 = EstPosteriorcal[,"quan97_5"])
  # Uniformity Test calibration
  UnifTestCal <-ks.test(QQplot$zi,'punif')
  # Ho : data are from U(0,1)
  # Ha : data are not from U(0,1)
  # p-value : 0.8106 > 0.05: accept Ho, data from U(0,1)
  
  # validation period
  QQplotv <- QQtest(discharge = as.numeric(valobs), posterior = posteriorval)
  NSEv <- Nash(obs = as.numeric(valobs), sim = EstPosteriorval[,"median"]) 
  KGEv <- klinggupta(obs = as.numeric(valobs), sim = EstPosteriorval[,"median"])
  CottonTest2v <- TestUncertBand(observations = as.numeric(valobs), quantil5 = EstPosteriorval[,"quan2_5"], quantil95 = EstPosteriorval[,"quan97_5"])
  PPU95v <- UncertaintyBand95(observations = as.numeric(valobs), quantil5 = EstPosteriorval[,"quan2_5"], quantil95 = EstPosteriorval[,"quan97_5"])
  # Uniformity Test calibration
  UnifTestVal <-ks.test(QQplotv$zi,'punif')
  
  return(list(posterior = as.matrix(posteriorcal), NSE = NSE, KGE = KGE, UnifTest = UnifTestCal$p.value, PoinsInsideBand = CottonTest2, EstPosterior = EstPosteriorcal, PPU95 = PPU95, QQplot = QQplot, posteriorv = as.matrix(posteriorval), NSEv = NSEv, KGEv = KGEv, UnifTestv = UnifTestVal$p.value,  PoinsInsideBandv = CottonTest2v, EstPosteriorv = EstPosteriorval, PPU95v = PPU95v, QQplotv = QQplotv))
}





# Example GMM post-processor with R and ,clust package
install.packages("mclust")
library(mclust)

calobs <- B1qocal
calsim <- B1qscal
# 1. Make NQT transformation for calibration and validation period
TransNQT <- NQT(N = 1000, factor.sample = 0.5, qobs = as.numeric(calobs), qsim = as.numeric(calsim))
TransNQTval <- NQT(N = 1000, factor.sample = 0.5, qobs = as.numeric(valobs), qsim = as.numeric(valsim))
datos <- cbind(TransNQT$eta, TransNQT$etaS)

mod1 <- Mclust(datos, G = 3)
summary(mod1, parameters = T)
plot(mod1, what = "BIC")
plot(mod1, what = "classification")
plot(mod1, what = "density")

# 8. Call Postprocessors  -----------------------------------------------------------------------------------

# call ABC postprocessor
dir.create("ResultABC", recursive = T)   # new folder for results
setwd("F:/TesinaMasterCoDireccion/ResultABC")
system.time(B1ABC   <- ABCpostprocessor(calobs = B1qocal, calsim = B1qscal, valobs = B1qoval, valsim = B1qsval, n = 100000, linfa = -0.1, lsupa = 0.1, linfb = 0.75, lsupb = 1, linfe = 0, lsupe = 0.6)) # n = 900000 long time
system.time(B2ABC   <- ABCpostprocessor(calobs = B2qocal, calsim = B2qscal, valobs = B2qoval, valsim = B2qsval, n = 100000, linfa = -0.1, lsupa = 0.1, linfb = 0.75, lsupb = 1, linfe = 0, lsupe = 0.6)) # n = 900000 long time
system.time(B3ABC   <- ABCpostprocessor(calobs = B3qocal, calsim = B3qscal, valobs = B3qoval, valsim = B3qsval, n = 100000, linfa = -0.1, lsupa = 0.1, linfb = 0.75, lsupb = 1, linfe = 0, lsupe = 0.6)) # n = 900000 long time
system.time(B6ABC   <- ABCpostprocessor(calobs = B6qocal, calsim = B6qscal, valobs = B6qoval, valsim = B6qsval, n = 100000, linfa = -0.1, lsupa = 0.1, linfb = 0.75, lsupb = 1, linfe = 0, lsupe = 0.6)) # n = 900000 long time
system.time(B7ABC   <- ABCpostprocessor(calobs = B7qocal, calsim = B7qscal, valobs = B7qoval, valsim = B7qsval, n = 100000, linfa = -0.1, lsupa = 0.1, linfb = 0.75, lsupb = 1, linfe = 0, lsupe = 0.6)) # n = 900000 long time
system.time(B8ABC   <- ABCpostprocessor(calobs = B8qocal, calsim = B8qscal, valobs = B8qoval, valsim = B8qsval, n = 100000, linfa = -0.1, lsupa = 0.1, linfb = 0.75, lsupb = 1, linfe = 0, lsupe = 0.6)) # n = 900000 long time
system.time(B9ABC   <- ABCpostprocessor(calobs = B9qocal, calsim = B9qscal, valobs = B9qoval, valsim = B9qsval, n = 100000, linfa = -0.1, lsupa = 0.1, linfb = 0.75, lsupb = 1, linfe = 0, lsupe = 0.6)) # n = 900000 long time
system.time(B10ABC  <- ABCpostprocessor(calobs = B10qocal, calsim = B10qscal, valobs = B10qoval, valsim = B10qsval, n = 100000, linfa = -0.1, lsupa = 0.1, linfb = 0.75, lsupb = 1, linfe = 0, lsupe = 0.6)) # n = 900000 long time
system.time(B11ABC  <- ABCpostprocessor(calobs = B11qocal, calsim = B11qscal, valobs = B11qoval, valsim = B11qsval, n = 100000, linfa = -0.1, lsupa = 0.1, linfb = 0.75, lsupb = 1, linfe = 0, lsupe = 0.6)) # n = 900000 long time
system.time(B12ABC  <- ABCpostprocessor(calobs = B12qocal, calsim = B12qscal, valobs = B12qoval, valsim = B12qsval, n = 100000, linfa = -0.1, lsupa = 0.1, linfb = 0.75, lsupb = 1, linfe = 0, lsupe = 0.6)) # n = 900000 long time
# Require qkde2 to compute predictive uncertainty for validation
system.time(B4ABC   <- ABCpostprocessor(calobs = B4qocal, calsim = B4qscal, valobs = B4qoval, valsim = B4qsval, n = 100000, linfa = -0.1, lsupa = 0.1, linfb = 0.75, lsupb = 1, linfe = 0, lsupe = 0.6)) # n = 900000 long time
system.time(B5ABC   <- ABCpostprocessor(calobs = B5qocal, calsim = B5qscal, valobs = B5qoval, valsim = B5qsval, n = 100000, linfa = -0.1, lsupa = 0.1, linfb = 0.75, lsupb = 1, linfe = 0, lsupe = 0.6)) # n = 900000 long time
beep(8)

# call MCMC postprocessor
setwd("F:/TesinaMasterCoDireccion")
dir.create("ResultMCMC", recursive = T)   # new folder for results
setwd("F:/TesinaMasterCoDireccion/ResultMCMC")
system.time(B1MCMC   <- MCMCpostprocessor(calobs = B1qocal, calsim = B1qscal, valobs = B1qoval, valsim = B1qsval)) # n = 900000 long time
system.time(B2MCMC   <- MCMCpostprocessor(calobs = B2qocal, calsim = B2qscal, valobs = B2qoval, valsim = B2qsval)) # n = 900000 long time
system.time(B3MCMC   <- MCMCpostprocessor(calobs = B3qocal, calsim = B3qscal, valobs = B3qoval, valsim = B3qsval)) # n = 900000 long time
system.time(B4MCMC   <- MCMCpostprocessor(calobs = B4qocal, calsim = B4qscal, valobs = B4qoval, valsim = B4qsval)) # n = 900000 long time
system.time(B5MCMC   <- MCMCpostprocessor(calobs = B5qocal, calsim = B5qscal, valobs = B5qoval, valsim = B5qsval)) # n = 900000 long time
system.time(B6MCMC   <- MCMCpostprocessor(calobs = B6qocal, calsim = B6qscal, valobs = B6qoval, valsim = B6qsval)) # n = 900000 long time
system.time(B7MCMC   <- MCMCpostprocessor(calobs = B7qocal, calsim = B7qscal, valobs = B7qoval, valsim = B7qsval)) # n = 900000 long time
system.time(B8MCMC   <- MCMCpostprocessor(calobs = B8qocal, calsim = B8qscal, valobs = B8qoval, valsim = B8qsval)) # n = 900000 long time
system.time(B9MCMC   <- MCMCpostprocessor(calobs = B9qocal, calsim = B9qscal, valobs = B9qoval, valsim = B9qsval)) # n = 900000 long time
system.time(B10MCMC  <- MCMCpostprocessor(calobs = B10qocal, calsim = B10qscal, valobs = B10qoval, valsim = B10qsval)) # n = 900000 long time
# Require qkde2 to compute predictive uncertainty for validation
system.time(B11MCMC  <- MCMCpostprocessor(calobs = B11qocal, calsim = B11qscal, valobs = B11qoval, valsim = B11qsval)) # n = 900000 long time
system.time(B12MCMC  <- MCMCpostprocessor(calobs = B12qocal, calsim = B12qscal, valobs = B12qoval, valsim = B12qsval)) # n = 900000 long time
beep(8)

# call MCP postprocessor
setwd("F:/TesinaMasterCoDireccion")
dir.create("ResultMCP", recursive = T)   # new folder for results
setwd("F:/TesinaMasterCoDireccion/ResultMCP")
system.time(B1MCP  <- MCP2D(calobs = as.numeric(B1qocal), calsim = as.numeric(B1qscal), valobs = as.numeric(B1qoval), valsim = as.numeric(B1qsval), N = 1000) )
system.time(B2MCP  <- MCP2D(calobs = as.numeric(B2qocal), calsim = as.numeric(B2qscal), valobs = as.numeric(B2qoval), valsim = as.numeric(B2qsval), N = 1000) )
system.time(B3MCP  <- MCP2D(calobs = as.numeric(B3qocal), calsim = as.numeric(B3qscal), valobs = as.numeric(B3qoval), valsim = as.numeric(B3qsval), N = 1000) )
system.time(B4MCP  <- MCP2D(calobs = as.numeric(B4qocal), calsim = as.numeric(B4qscal), valobs = as.numeric(B4qoval), valsim = as.numeric(B4qsval), N = 1000) )
system.time(B5MCP  <- MCP2D(calobs = as.numeric(B5qocal), calsim = as.numeric(B5qscal), valobs = as.numeric(B5qoval), valsim = as.numeric(B5qsval), N = 1000) )
system.time(B6MCP  <- MCP2D(calobs = as.numeric(B6qocal), calsim = as.numeric(B6qscal), valobs = as.numeric(B6qoval), valsim = as.numeric(B6qsval), N = 1000) )
system.time(B7MCP  <- MCP2D(calobs = as.numeric(B7qocal), calsim = as.numeric(B7qscal), valobs = as.numeric(B7qoval), valsim = as.numeric(B7qsval), N = 1000) )
system.time(B8MCP  <- MCP2D(calobs = as.numeric(B8qocal), calsim = as.numeric(B8qscal), valobs = as.numeric(B8qoval), valsim = as.numeric(B8qsval), N = 1000) )
system.time(B9MCP  <- MCP2D(calobs = as.numeric(B9qocal), calsim = as.numeric(B9qscal), valobs = as.numeric(B9qoval), valsim = as.numeric(B9qsval), N = 1000) )
system.time(B10MCP <- MCP2D(calobs = as.numeric(B10qocal), calsim = as.numeric(B10qscal), valobs = as.numeric(B10qoval), valsim = as.numeric(B10qsval), N = 1000) )
# Require qkde2 to compute predictive uncertainty for validation
system.time(B11MCP <- MCP2D(calobs = as.numeric(B11qocal), calsim = as.numeric(B11qscal), valobs = as.numeric(B11qoval), valsim = as.numeric(B11qsval), N = 1000) )
system.time(B12MCP <- MCP2D(calobs = as.numeric(B12qocal), calsim = as.numeric(B12qscal), valobs = as.numeric(B12qoval), valsim = as.numeric(B12qsval), N = 1000) )
beep(8)

# call MCPt postprocessor
setwd("F:/TesinaMasterCoDireccion")
dir.create("ResultMCP", recursive = T)   # new folder for results
setwd("F:/TesinaMasterCoDireccion/ResultMCPt")
system.time(B1MCPt  <- MCP2Dtrun(calobs = as.numeric(B1qocal), calsim = as.numeric(B1qscal), valobs = as.numeric(B1qoval), valsim = as.numeric(B1qsval), N = 1000, inipoints = c(-1,1.5), factor.sample = 0.5) )
system.time(B2MCPt  <- MCP2Dtrun(calobs = as.numeric(B2qocal), calsim = as.numeric(B2qscal), valobs = as.numeric(B2qoval), valsim = as.numeric(B2qsval), N = 1000, inipoints = c(-1,1.5), factor.sample = 0.5) )
system.time(B3MCPt  <- MCP2Dtrun(calobs = as.numeric(B3qocal), calsim = as.numeric(B3qscal), valobs = as.numeric(B3qoval), valsim = as.numeric(B3qsval), N = 1000, inipoints = c(-1,1.5), factor.sample = 0.5) )
system.time(B4MCPt  <- MCP2Dtrun(calobs = as.numeric(B4qocal), calsim = as.numeric(B4qscal), valobs = as.numeric(B4qoval), valsim = as.numeric(B4qsval), N = 1000, inipoints = c(-1,1.5), factor.sample = 0.5) )
system.time(B5MCPt  <- MCP2Dtrun(calobs = as.numeric(B5qocal), calsim = as.numeric(B5qscal), valobs = as.numeric(B5qoval), valsim = as.numeric(B5qsval), N = 1000, inipoints = c(-1,1.5), factor.sample = 0.5) )
system.time(B6MCPt  <- MCP2Dtrun(calobs = as.numeric(B6qocal), calsim = as.numeric(B6qscal), valobs = as.numeric(B6qoval), valsim = as.numeric(B6qsval), N = 1000, inipoints = c(-1,1.5), factor.sample = 0.5) )
system.time(B7MCPt  <- MCP2Dtrun(calobs = as.numeric(B7qocal), calsim = as.numeric(B7qscal), valobs = as.numeric(B7qoval), valsim = as.numeric(B7qsval), N = 1000, inipoints = c(-1,1.5), factor.sample = 0.5) )
system.time(B8MCPt  <- MCP2Dtrun(calobs = as.numeric(B8qocal), calsim = as.numeric(B8qscal), valobs = as.numeric(B8qoval), valsim = as.numeric(B8qsval), N = 1000, inipoints = c(-1,1.5), factor.sample = 0.5) )
system.time(B9MCPt  <- MCP2Dtrun(calobs = as.numeric(B9qocal), calsim = as.numeric(B9qscal), valobs = as.numeric(B9qoval), valsim = as.numeric(B9qsval), N = 1000, inipoints = c(-1,1.5), factor.sample = 0.5) )
system.time(B10MCPt <- MCP2Dtrun(calobs = as.numeric(B10qocal), calsim = as.numeric(B10qscal), valobs = as.numeric(B10qoval), valsim = as.numeric(B10qsval), N = 1000, inipoints = c(-1,1.5), factor.sample = 0.5) )
# Require qkde2 to compute predictive uncertainty for validation
#beep(8)
system.time(B11MCPt <- MCP2Dtrun(calobs = as.numeric(B11qocal), calsim = as.numeric(B11qscal), valobs = as.numeric(B11qoval), valsim = as.numeric(B11qsval), N = 1000, inipoints = c(-1,1.5), factor.sample = 0.5) )
system.time(B12MCPt <- MCP2Dtrun(calobs = as.numeric(B12qocal), calsim = as.numeric(B12qscal), valobs = as.numeric(B12qoval), valsim = as.numeric(B12qsval), N = 1000, inipoints = c(-1,1.5), factor.sample = 0.5) )
beep(8)

# call GMM postprocessor
setwd("F:/TesinaMasterCoDireccion/ResultGMM")
system.time(B1GMM  <- GMMPostEstMetrics(calobs = as.numeric(B1qocal), calsim = as.numeric(B1qscal), valobs = as.numeric(B1qoval), valsim = as.numeric(B1qsval), posteriorcal = posteriorgc1cal, posteriorval = posteriorgc1v) )
system.time(B2GMM  <- GMMPostEstMetrics(calobs = as.numeric(B2qocal), calsim = as.numeric(B2qscal), valobs = as.numeric(B2qoval), valsim = as.numeric(B2qsval), posteriorcal = posteriorgc2cal, posteriorval = posteriorgc2v) )
system.time(B3GMM  <- GMMPostEstMetrics(calobs = as.numeric(B3qocal), calsim = as.numeric(B3qscal), valobs = as.numeric(B3qoval), valsim = as.numeric(B3qsval), posteriorcal = posteriorgc3cal, posteriorval = posteriorgc3v) )
system.time(B4GMM  <- GMMPostEstMetrics(calobs = as.numeric(B4qocal), calsim = as.numeric(B4qscal), valobs = as.numeric(B4qoval), valsim = as.numeric(B4qsval), posteriorcal = posteriorgc4cal, posteriorval = posteriorgc4v) )
system.time(B5GMM  <- GMMPostEstMetrics(calobs = as.numeric(B5qocal), calsim = as.numeric(B5qscal), valobs = as.numeric(B5qoval), valsim = as.numeric(B5qsval), posteriorcal = posteriorgc5cal, posteriorval = posteriorgc5v) )
system.time(B6GMM  <- GMMPostEstMetrics(calobs = as.numeric(B6qocal), calsim = as.numeric(B6qscal), valobs = as.numeric(B6qoval), valsim = as.numeric(B6qsval), posteriorcal = posteriorgc6cal, posteriorval = posteriorgc6v) )
system.time(B7GMM  <- GMMPostEstMetrics(calobs = as.numeric(B7qocal), calsim = as.numeric(B7qscal), valobs = as.numeric(B7qoval), valsim = as.numeric(B7qsval), posteriorcal = posteriorgc7cal, posteriorval = posteriorgc7v) )
system.time(B8GMM  <- GMMPostEstMetrics(calobs = as.numeric(B8qocal), calsim = as.numeric(B8qscal), valobs = as.numeric(B8qoval), valsim = as.numeric(B8qsval), posteriorcal = posteriorgc8cal, posteriorval = posteriorgc8v) )
system.time(B9GMM  <- GMMPostEstMetrics(calobs = as.numeric(B9qocal), calsim = as.numeric(B9qscal), valobs = as.numeric(B9qoval), valsim = as.numeric(B9qsval), posteriorcal = posteriorgc9cal, posteriorval = posteriorgc9v) )
system.time(B10GMM <- GMMPostEstMetrics(calobs = as.numeric(B10qocal), calsim = as.numeric(B10qscal), valobs = as.numeric(B10qoval), valsim = as.numeric(B10qsval), posteriorcal = posteriorgc10cal, posteriorval = posteriorgc10v) )
system.time(B11GMM <- GMMPostEstMetrics(calobs = as.numeric(B11qocal), calsim = as.numeric(B11qscal), valobs = as.numeric(B11qoval), valsim = as.numeric(B11qsval), posteriorcal = posteriorgc11cal, posteriorval = posteriorgc11v) )
system.time(B12GMM <- GMMPostEstMetrics(calobs = as.numeric(B12qocal), calsim = as.numeric(B12qscal), valobs = as.numeric(B12qoval), valsim = as.numeric(B12qsval), posteriorcal = posteriorgc12cal, posteriorval = posteriorgc12v) )
beep(8)



# 9. Plotting  -----------------------------------------------------------------------------------

# fig 1 Map of catchments

# fig 2 Budyko curve #----------------------------------------------------
setwd("F:/TesinaMasterCoDireccion")
load("DataBudyko.RData")
setwd("F:/TesinaMasterCoDireccion/PaperMOPEX")
yb<-Budyko[[1]]
xb<-Budyko[[2]]
jpeg("./Fig2Budyko.jpg", res = 600, width = 20, height = 12.36068, units = "in")
par(mar = c(4.8,4.8,4,0.5),lab=c(5,5, 5),lty=1,lwd=2,mex=1.45,mgp=c(3,1.2,0))
plot(xb,yb,ylim = c(0,1.3),xlim=c(0,2.5),cex.axis=3,cex.lab = 3,cex.main=4,main = "",xlab="PET/P",ylab="ET/P",col=c(1:12),pch = c(1:12), cex = 2.5)
lines(x=c("0","1","2.5"),y=c("0","1","1"),col ="red", lwd = 3)
lines(x=c("1","2.5"),y=c("1","1"),col ="blue", lwd = 3)
lines(x=c("1","1"),y=c("0","1"),col ="black", lwd = 3,lty=2)
#text(x=0.5,y=0.6,adj = 0,labels = "Energy limit",srt=35,cex = 3)
#text(x=1.5,y=1.08,adj = 0,labels = "Water limit",cex = 3)
text(x=1.7,y=0.4,adj = 0,labels = "PET/P>1",cex = 3)
text(x=1.7,y=0.3,adj = 0,labels = "Water-limit",cex = 3)
text(x=0.6,y=0.28,adj = 0,labels = "PET/P<1",cex = 3)
text(x=0.6,y=0.18,adj = 0,labels = "Energy-limit",cex = 3)
legend(x=0.3,y=1.2, c("B1","B2","B3","B4","B5","B6","B7","B8","B9","B10","B11","B12"),col=c(1:12),lty=c(1,1),seg.len=2,xjust=1,cex=2.5,pch=c(1:12),bty = "n")
dev.off()


# fig. 2.1 % performance NSE and KGE
setwd("F:/TesinaMasterCoDireccion/TABLA") 
load("porcentaje.RData")
setwd("F:/TesinaMasterCoDireccion/PaperMOPEX")
#Resultados
#calibracion
NSEq1cal <- Nash(obs = B1qocal, sim = B1qscal) 
KGEq1cal <- klinggupta(obs = B1qocal, sim = B1qscal)

NSEq2cal <- Nash(obs = B2qocal, sim = B2qscal) 
KGEq2cal <- klinggupta(obs = B2qocal, sim = B2qscal)

NSEq3cal <- Nash(obs = B3qocal, sim = B3qscal) 
KGEq3cal <- klinggupta(obs = B3qocal, sim = B3qscal)

NSEq4cal <- Nash(obs = B4qocal, sim = B4qscal) 
KGEq4cal <- klinggupta(obs = B4qocal, sim = B4qscal)

NSEq5cal <- Nash(obs = B5qocal, sim = B5qscal) 
KGEq5cal <- klinggupta(obs = B5qocal, sim = B5qscal)

NSEq6cal <- Nash(obs = B6qocal, sim = B6qscal) 
KGEq6cal <- klinggupta(obs = B6qocal, sim = B6qscal)

NSEq7cal <- Nash(obs = B7qocal, sim = B7qscal) 
KGEq7cal <- klinggupta(obs = B7qocal, sim = B7qscal)

NSEq8cal <- Nash(obs = B8qocal, sim = B8qscal) 
KGEq8cal <- klinggupta(obs = B8qocal, sim = B8qscal)

NSEq9cal <- Nash(obs = B9qocal, sim = B9qscal) 
KGEq9cal <- klinggupta(obs = B9qocal, sim = B9qscal)

NSEq10cal <- Nash(obs = B10qocal, sim = B10qscal) 
KGEq10cal <- klinggupta(obs = B10qocal, sim = B10qscal)

NSEq11cal <- Nash(obs = B11qocal, sim = B11qscal) 
KGEq11cal <- klinggupta(obs = B11qocal, sim = B11qscal)

NSEq12cal <- Nash(obs = B12qocal, sim = B12qscal) 
KGEq12cal <- klinggupta(obs = B12qocal, sim = B12qscal)

NSEcalq1.12<-cbind(NSEq1cal,NSEq2cal,NSEq3cal,NSEq4cal,NSEq5cal,NSEq6cal,NSEq7cal,NSEq8cal,NSEq9cal,NSEq10cal,NSEq11cal,NSEq12cal)
KGEcalq1.12<-cbind(KGEq1cal$KGE,KGEq2cal$KGE,KGEq3cal$KGE,KGEq4cal$KGE,KGEq5cal$KGE,KGEq6cal$KGE,KGEq7cal$KGE,KGEq8cal$KGE,KGEq9cal$KGE,KGEq10cal$KGE,KGEq11cal$KGE,KGEq12cal$KGE)
Correlationcalq1.12<-cbind(KGEq1cal$Correlation,KGEq2cal$Correlation,KGEq3cal$Correlation,KGEq4cal$Correlation,KGEq5cal$Correlation,KGEq6cal$Correlation,KGEq7cal$Correlation,KGEq8cal$Correlation,KGEq9cal$Correlation,KGEq10cal$Correlation,KGEq11cal$Correlation,KGEq12cal$Correlation)
CV.ratiocalq1.12<-cbind(KGEq1cal$CV.ratio,KGEq2cal$CV.ratio,KGEq3cal$CV.ratio,KGEq4cal$CV.ratio,KGEq5cal$CV.ratio,KGEq6cal$CV.ratio,KGEq7cal$CV.ratio,KGEq8cal$CV.ratio,KGEq9cal$CV.ratio,KGEq10cal$CV.ratio,KGEq11cal$CV.ratio,KGEq12cal$CV.ratio)
Bias.ratiocalq1.12<-cbind(KGEq1cal$Bias.ratio,KGEq2cal$Bias.ratio,KGEq3cal$Bias.ratio,KGEq4cal$Bias.ratio,KGEq5cal$Bias.ratio,KGEq6cal$Bias.ratio,KGEq7cal$Bias.ratio,KGEq8cal$Bias.ratio,KGEq9cal$Bias.ratio,KGEq10cal$Bias.ratio,KGEq11cal$Bias.ratio,KGEq12cal$Bias.ratio)
matrix12cuencas<-rbind(NSEcalq1.12[1,],KGEcalq1.12[1,],Correlationcalq1.12[1,],CV.ratiocalq1.12[1,],Bias.ratiocalq1.12[1,])
rownames(matrix12cuencas)<-c ("NSE","KGE","Correlation","CV.ratio","Bias.ratio")
colnames(matrix12cuencas)<-c ("1 ","2","3","4","5","6 ","7","8","9","10","11","12")

#Validation

NSEq1val <- Nash(obs = B1qoval, sim = B1qsval) 
KGEq1val <- klinggupta(obs = B1qoval, sim = B1qsval)

NSEq2val <- Nash(obs = B2qoval, sim = B2qsval) 
KGEq2val <- klinggupta(obs = B2qoval, sim = B2qsval)

NSEq3val <- Nash(obs = B3qoval, sim = B3qsval) 
KGEq3val <- klinggupta(obs = B3qoval, sim = B3qsval)

NSEq4val <- Nash(obs = B4qoval, sim = B4qsval) 
KGEq4val <- klinggupta(obs = B4qoval, sim = B4qsval)

NSEq5val <- Nash(obs = B5qoval, sim = B5qsval) 
KGEq5val <- klinggupta(obs = B5qoval, sim = B5qsval)

NSEq6val <- Nash(obs = B6qoval, sim = B6qsval) 
KGEq6val <- klinggupta(obs = B6qoval, sim = B6qsval)

NSEq7val <- Nash(obs = B7qoval, sim = B7qsval) 
KGEq7val <- klinggupta(obs = B7qoval, sim = B7qsval)

NSEq8val <- Nash(obs = B8qoval, sim = B8qsval) 
KGEq8val <- klinggupta(obs = B8qoval, sim = B8qsval)

NSEq9val <- Nash(obs = B9qoval, sim = B9qsval) 
KGEq9val <- klinggupta(obs = B9qoval, sim = B9qsval)

NSEq10val <- Nash(obs = B10qoval, sim = B10qsval) 
KGEq10val <- klinggupta(obs = B10qoval, sim = B10qsval)

NSEq11val <- Nash(obs = B11qoval, sim = B11qsval) 
KGEq11val <- klinggupta(obs = B11qoval, sim = B11qsval)

NSEq12val <- Nash(obs = B12qoval, sim = B12qsval) 
KGEq12val <- klinggupta(obs = B12qoval, sim = B12qsval)

NSEvalq1.12<-cbind(NSEq1val,NSEq2val,NSEq3val,NSEq4val,NSEq5val,NSEq6val,NSEq7val,NSEq8val,NSEq9val,NSEq10val,NSEq11val,NSEq12val)
KGEvalq1.12<-cbind(KGEq1val$KGE,KGEq2val$KGE,KGEq3val$KGE,KGEq4val$KGE,KGEq5val$KGE,KGEq6val$KGE,KGEq7val$KGE,KGEq8val$KGE,KGEq9val$KGE,KGEq10val$KGE,KGEq11val$KGE,KGEq12val$KGE)
Correlationvalq1.12<-cbind(KGEq1val$Correlation,KGEq2val$Correlation,KGEq3val$Correlation,KGEq4val$Correlation,KGEq5val$Correlation,KGEq6val$Correlation,KGEq7val$Correlation,KGEq8val$Correlation,KGEq9val$Correlation,KGEq10val$Correlation,KGEq11val$Correlation,KGEq12val$Correlation)
CV.ratiovalq1.12<-cbind(KGEq1val$CV.ratio,KGEq2val$CV.ratio,KGEq3val$CV.ratio,KGEq4val$CV.ratio,KGEq5val$CV.ratio,KGEq6val$CV.ratio,KGEq7val$CV.ratio,KGEq8val$CV.ratio,KGEq9val$CV.ratio,KGEq10val$CV.ratio,KGEq11val$CV.ratio,KGEq12val$CV.ratio)
Bias.ratiovalq1.12<-cbind(KGEq1val$Bias.ratio,KGEq2val$Bias.ratio,KGEq3val$Bias.ratio,KGEq4val$Bias.ratio,KGEq5val$Bias.ratio,KGEq6val$Bias.ratio,KGEq7val$Bias.ratio,KGEq8val$Bias.ratio,KGEq9val$Bias.ratio,KGEq10val$Bias.ratio,KGEq11val$Bias.ratio,KGEq12val$Bias.ratio)
matrix12cuencasval<-rbind(NSEvalq1.12[1,],KGEvalq1.12[1,],Correlationvalq1.12[1,],CV.ratiovalq1.12[1,],Bias.ratiovalq1.12[1,])
rownames(matrix12cuencasval)<-c ("NSE","KGE","Correlation","CV.ratio","Bias.ratio")
colnames(matrix12cuencasval)<-c ("1 ","2","3","4","5","6 ","7","8","9","10","11","12")

## para sacar el % VALIDACION

#NASHv
ABCNv=c(B1ABC$NSEv,B2ABC$NSEv,B3ABC$NSEv,B4ABC$NSEv,B5ABC$NSEv,B6ABC$NSEv,B7ABC$NSEv,B8ABC$NSEv,B9ABC$NSEv,B10ABC$NSEv,B11ABC$NSEv,B12ABC$NSEv)
MCMCNv=c(B1MCMC$NSEv,B2MCMC$NSEv,B3MCMC$NSEv,B4MCMC$NSEv,B5MCMC$NSEv,B6MCMC$NSEv,B7MCMC$NSEv,B8MCMC$NSEv,B9MCMC$NSEv,B10MCMC$NSEv,B11MCMC$NSEv,B12MCMC$NSEv)
MCPNv=c(B1MCP$NSEv,B2MCP$NSEv,B3MCP$NSEv,B4MCP$NSEv,B5MCP$NSEv,B6MCP$NSEv,B7MCP$NSEv,B8MCP$NSEv,B9MCP$NSEv,B10MCP$NSEv,B11MCP$NSEv,B12MCP$NSEv)
MCPtNv=c(B1MCPt$NSEv,B2MCPt$NSEv,B3MCPt$NSEv,B4MCPt$NSEv,B5MCPt$NSEv,B6MCPt$NSEv,B7MCPt$NSEv,B8MCPt$NSEv,B9MCPt$NSEv,B10MCPt$NSEv,B11MCPt$NSEv,B12MCPt$NSEv)
GMMNv=c(B1GMM$NSEv,B2GMM$NSEv,B3GMM$NSEv,B4GMM$NSEv,B5GMM$NSEv,B6GMM$NSEv,B7GMM$NSEv,B8GMM$NSEv,B9GMM$NSEv,B10GMM$NSEv,B11GMM$NSEv,B12GMM$NSEv)
#KGE
ABCKGEv=c(B1ABC$KGEv$KGE,B2ABC$KGEv$KGE,B3ABC$KGEv$KGE,B4ABC$KGEv$KGE,B5ABC$KGEv$KGE,B6ABC$KGEv$KGE,B7ABC$KGEv$KGE,B8ABC$KGEv$KGE,B9ABC$KGEv$KGE,B10ABC$KGEv$KGE,B11ABC$KGEv$KGE,B12ABC$KGEv$KGE)
MCMCKGEv=c(B1MCMC$KGEv$KGE,B2MCMC$KGEv$KGE,B3MCMC$KGEv$KGE,B4MCMC$KGEv$KGE,B5MCMC$KGEv$KGE,B6MCMC$KGEv$KGE,B7MCMC$KGEv$KGE,B8MCMC$KGEv$KGE,B9MCMC$KGEv$KGE,B10MCMC$KGEv$KGE,B11MCMC$KGEv$KGE,B12MCMC$KGEv$KGE)
MCPKGEv=c(B1MCP$KGEv$KGE,B2MCP$KGEv$KGE,B3MCP$KGEv$KGE,B4MCP$KGEv$KGE,B5MCP$KGEv$KGE,B6MCP$KGEv$KGE,B7MCP$KGEv$KGE,B8MCP$KGEv$KGE,B9MCP$KGEv$KGE,B10MCP$KGEv$KGE,B11MCP$KGEv$KGE,B12MCP$KGEv$KGE)
MCPtKGEv=c(B1MCPt$KGEv$KGE,B2MCPt$KGEv$KGE,B3MCPt$KGEv$KGE,B4MCPt$KGEv$KGE,B5MCPt$KGEv$KGE,B6MCPt$KGEv$KGE,B7MCPt$KGEv$KGE,B8MCPt$KGEv$KGE,B9MCPt$KGEv$KGE,B10MCPt$KGEv$KGE,B11MCPt$KGEv$KGE,B12MCPt$KGEv$KGE)
GMMKGEv=c(B1GMM$KGEv$KGE,B2GMM$KGEv$KGE,B3GMM$KGEv$KGE,B4GMM$KGEv$KGE,B5GMM$KGEv$KGE,B6GMM$KGEv$KGE,B7GMM$KGEv$KGE,B8GMM$KGEv$KGE,B9GMM$KGEv$KGE,B10GMM$KGEv$KGE,B11GMM$KGEv$KGE,B12GMM$KGEv$KGE)
#Correlation
ABCCorrelation =c(B1ABC$KGEv$Correlation,B2ABC$KGEv$Correlation,B3ABC$KGEv$Correlation,B4ABC$KGEv$Correlation,B5ABC$KGEv$Correlation,B6ABC$KGEv$Correlation,B7ABC$KGEv$Correlation,B8ABC$KGEv$Correlation,B9ABC$KGEv$Correlation,B10ABC$KGEv$Correlation,B11ABC$KGEv$Correlation,B12ABC$KGEv$Correlation)
MCMCCorrelation =c(B1MCMC$KGEv$Correlation,B2MCMC$KGEv$Correlation,B3MCMC$KGEv$Correlation,B4MCMC$KGEv$Correlation,B5MCMC$KGEv$Correlation,B6MCMC$KGEv$Correlation,B7MCMC$KGEv$Correlation,B8MCMC$KGEv$Correlation,B9MCMC$KGEv$Correlation,B10MCMC$KGEv$Correlation,B11MCMC$KGEv$Correlation,B12MCMC$KGEv$Correlation)
MCPCorrelation =c(B1MCP$KGEv$Correlation,B2MCP$KGEv$Correlation,B3MCP$KGEv$Correlation,B4MCP$KGEv$Correlation,B5MCP$KGEv$Correlation,B6MCP$KGEv$Correlation,B7MCP$KGEv$Correlation,B8MCP$KGEv$Correlation,B9MCP$KGEv$Correlation,B10MCP$KGEv$Correlation,B11MCP$KGEv$Correlation,B12MCP$KGEv$Correlation)
MCPtCorrelation =c(B1MCPt$KGEv$Correlation,B2MCPt$KGEv$Correlation,B3MCPt$KGEv$Correlation,B4MCPt$KGEv$Correlation,B5MCPt$KGEv$Correlation,B6MCPt$KGEv$Correlation,B7MCPt$KGEv$Correlation,B8MCPt$KGEv$Correlation,B9MCPt$KGEv$Correlation,B10MCPt$KGEv$Correlation,B11MCPt$KGEv$Correlation,B12MCPt$KGEv$Correlation)
GMMCorrelation =c(B1GMM$KGEv$Correlation,B2GMM$KGEv$Correlation,B3GMM$KGEv$Correlation,B4GMM$KGEv$Correlation,B5GMM$KGEv$Correlation,B6GMM$KGEv$Correlation,B7GMM$KGEv$Correlation,B8GMM$KGEv$Correlation,B9GMM$KGEv$Correlation,B10GMM$KGEv$Correlation,B11GMM$KGEv$Correlation,B12GMM$KGEv$Correlation)
#Bias.ratio
ABCBias.ratio =c(B1ABC$KGEv$Bias.ratio,B2ABC$KGEv$Bias.ratio,B3ABC$KGEv$Bias.ratio,B4ABC$KGEv$Bias.ratio,B5ABC$KGEv$Bias.ratio,B6ABC$KGEv$Bias.ratio,B7ABC$KGEv$Bias.ratio,B8ABC$KGEv$Bias.ratio,B9ABC$KGEv$Bias.ratio,B10ABC$KGEv$Bias.ratio,B11ABC$KGEv$Bias.ratio,B12ABC$KGEv$Bias.ratio)
MCMCBias.ratio =c(B1MCMC$KGEv$Bias.ratio,B2MCMC$KGEv$Bias.ratio,B3MCMC$KGEv$Bias.ratio,B4MCMC$KGEv$Bias.ratio,B5MCMC$KGEv$Bias.ratio,B6MCMC$KGEv$Bias.ratio,B7MCMC$KGEv$Bias.ratio,B8MCMC$KGEv$Bias.ratio,B9MCMC$KGEv$Bias.ratio,B10MCMC$KGEv$Bias.ratio,B11MCMC$KGEv$Bias.ratio,B12MCMC$KGEv$Bias.ratio)
MCPBias.ratio =c(B1MCP$KGEv$Bias.ratio,B2MCP$KGEv$Bias.ratio,B3MCP$KGEv$Bias.ratio,B4MCP$KGEv$Bias.ratio,B5MCP$KGEv$Bias.ratio,B6MCP$KGEv$Bias.ratio,B7MCP$KGEv$Bias.ratio,B8MCP$KGEv$Bias.ratio,B9MCP$KGEv$Bias.ratio,B10MCP$KGEv$Bias.ratio,B11MCP$KGEv$Bias.ratio,B12MCP$KGEv$Bias.ratio)
MCPtBias.ratio =c(B1MCPt$KGEv$Bias.ratio,B2MCPt$KGEv$Bias.ratio,B3MCPt$KGEv$Bias.ratio,B4MCPt$KGEv$Bias.ratio,B5MCPt$KGEv$Bias.ratio,B6MCPt$KGEv$Bias.ratio,B7MCPt$KGEv$Bias.ratio,B8MCPt$KGEv$Bias.ratio,B9MCPt$KGEv$Bias.ratio,B10MCPt$KGEv$Bias.ratio,B11MCPt$KGEv$Bias.ratio,B12MCPt$KGEv$Bias.ratio)
GMMBias.ratio =c(B1GMM$KGEv$Bias.ratio,B2GMM$KGEv$Bias.ratio,B3GMM$KGEv$Bias.ratio,B4GMM$KGEv$Bias.ratio,B5GMM$KGEv$Bias.ratio,B6GMM$KGEv$Bias.ratio,B7GMM$KGEv$Bias.ratio,B8GMM$KGEv$Bias.ratio,B9GMM$KGEv$Bias.ratio,B10GMM$KGEv$Bias.ratio,B11GMM$KGEv$Bias.ratio,B12GMM$KGEv$Bias.ratio)

#matrices VALIDACIÓN el % 
#NASH
M1Nv = rbind(ABCNv,MCMCNv,MCPNv,MCPtNv,GMMNv)
rownames(M1Nv)<-c ("ABC ","MCMC","MCP","MCPt","GMM")
colnames(M1Nv)<-c ("1 ","2","3","4","5","6 ","7","8","9","10","11","12")
#d_factorv
M2KGEv = rbind(ABCKGEv,MCMCKGEv,MCPKGEv,MCPtKGEv,GMMKGEv)
rownames(M2KGEv)<-c ("ABC ","MCMC","MCP","MCPt","GMM")
colnames(M2KGEv)<-c ("1 ","2","3","4","5","6 ","7","8","9","10","11","12")
#PRECISION
M3PGMMCorrelation = rbind(ABCCorrelation,MCMCCorrelation,MCPCorrelation ,MCPtCorrelation,GMMCorrelation)
rownames(M3PGMMCorrelation)<-c ("ABC ","MCMC","MCP","MCPt","GMM")
colnames(M3PGMMCorrelation)<-c ("1 ","2","3","4","5","6 ","7","8","9","10","11","12")
#PM4Bias.ratio
M4Bias.ratio= rbind(ABCBias.ratio ,MCMCBias.ratio,MCPBias.ratio,MCPtBias.ratio,GMMBias.ratio)
rownames(M4Bias.ratio)<-c ("ABC ","MCMC","MCP","MCPt","GMM")
colnames(M4Bias.ratio)<-c ("1 ","2","3","4","5","6 ","7","8","9","10","11","12")

## para sacar el % CALIBRACION POST
#NASHv
ABCNCAL=c(B1ABC$NSE,B2ABC$NSE,B3ABC$NSE,B4ABC$NSE,B5ABC$NSE,B6ABC$NSE,B7ABC$NSE,B8ABC$NSE,B9ABC$NSE,B10ABC$NSE,B11ABC$NSE,B12ABC$NSE)
MCMCNCAL=c(B1MCMC$NSE,B2MCMC$NSE,B3MCMC$NSE,B4MCMC$NSE,B5MCMC$NSE,B6MCMC$NSE,B7MCMC$NSE,B8MCMC$NSE,B9MCMC$NSE,B10MCMC$NSE,B11MCMC$NSE,B12MCMC$NSE)
MCPNCAL=c(B1MCP$NSE,B2MCP$NSE,B3MCP$NSE,B4MCP$NSE,B5MCP$NSE,B6MCP$NSE,B7MCP$NSE,B8MCP$NSE,B9MCP$NSE,B10MCP$NSE,B11MCP$NSE,B12MCP$NSE)
MCPtNCAL=c(B1MCPt$NSE,B2MCPt$NSE,B3MCPt$NSE,B4MCPt$NSE,B5MCPt$NSE,B6MCPt$NSE,B7MCPt$NSE,B8MCPt$NSE,B9MCPt$NSE,B10MCPt$NSE,B11MCPt$NSE,B12MCPt$NSE)
GMMNCAL=c(B1GMM$NSE,B2GMM$NSE,B3GMM$NSE,B4GMM$NSE,B5GMM$NSE,B6GMM$NSE,B7GMM$NSE,B8GMM$NSE,B9GMM$NSE,B10GMM$NSE,B11GMM$NSE,B12GMM$NSE)
#KGE
ABCKGECAL=c(B1ABC$KGE$KGE,B2ABC$KGE$KGE,B3ABC$KGE$KGE,B4ABC$KGE$KGE,B5ABC$KGE$KGE,B6ABC$KGE$KGE,B7ABC$KGE$KGE,B8ABC$KGE$KGE,B9ABC$KGE$KGE,B10ABC$KGE$KGE,B11ABC$KGE$KGE,B12ABC$KGE$KGE)
MCMCKGECAL=c(B1MCMC$KGE$KGE,B2MCMC$KGE$KGE,B3MCMC$KGE$KGE,B4MCMC$KGE$KGE,B5MCMC$KGE$KGE,B6MCMC$KGE$KGE,B7MCMC$KGE$KGE,B8MCMC$KGE$KGE,B9MCMC$KGE$KGE,B10MCMC$KGE$KGE,B11MCMC$KGE$KGE,B12MCMC$KGE$KGE)
MCPKGECAL=c(B1MCP$KGE$KGE,B2MCP$KGE$KGE,B3MCP$KGE$KGE,B4MCP$KGE$KGE,B5MCP$KGE$KGE,B6MCP$KGE$KGE,B7MCP$KGE$KGE,B8MCP$KGE$KGE,B9MCP$KGE$KGE,B10MCP$KGE$KGE,B11MCP$KGE$KGE,B12MCP$KGE$KGE)
MCPtKGECAL=c(B1MCPt$KGE$KGE,B2MCPt$KGE$KGE,B3MCPt$KGE$KGE,B4MCPt$KGE$KGE,B5MCPt$KGE$KGE,B6MCPt$KGE$KGE,B7MCPt$KGE$KGE,B8MCPt$KGE$KGE,B9MCPt$KGE$KGE,B10MCPt$KGE$KGE,B11MCPt$KGE$KGE,B12MCPt$KGE$KGE)
GMMKGECAL=c(B1GMM$KGE$KGE,B2GMM$KGE$KGE,B3GMM$KGE$KGE,B4GMM$KGE$KGE,B5GMM$KGE$KGE,B6GMM$KGE$KGE,B7GMM$KGE$KGE,B8GMM$KGE$KGE,B9GMM$KGE$KGE,B10GMM$KGE$KGE,B11GMM$KGE$KGE,B12GMM$KGE$KGE)
#Correlation
ABCCorrelationCAL=c(B1ABC$KGE$Correlation,B2ABC$KGE$Correlation,B3ABC$KGE$Correlation,B4ABC$KGE$Correlation,B5ABC$KGE$Correlation,B6ABC$KGE$Correlation,B7ABC$KGE$Correlation,B8ABC$KGE$Correlation,B9ABC$KGE$Correlation,B10ABC$KGE$Correlation,B11ABC$KGE$Correlation,B12ABC$KGE$Correlation)
MCMCCorrelationCAL =c(B1MCMC$KGE$Correlation,B2MCMC$KGE$Correlation,B3MCMC$KGE$Correlation,B4MCMC$KGE$Correlation,B5MCMC$KGE$Correlation,B6MCMC$KGE$Correlation,B7MCMC$KGE$Correlation,B8MCMC$KGE$Correlation,B9MCMC$KGE$Correlation,B10MCMC$KGE$Correlation,B11MCMC$KGE$Correlation,B12MCMC$KGE$Correlation)
MCPCorrelationCAL=c(B1MCP$KGE$Correlation,B2MCP$KGE$Correlation,B3MCP$KGE$Correlation,B4MCP$KGE$Correlation,B5MCP$KGE$Correlation,B6MCP$KGE$Correlation,B7MCP$KGE$Correlation,B8MCP$KGE$Correlation,B9MCP$KGE$Correlation,B10MCP$KGE$Correlation,B11MCP$KGE$Correlation,B12MCP$KGE$Correlation)
MCPtCorrelationCAL=c(B1MCPt$KGE$Correlation,B2MCPt$KGE$Correlation,B3MCPt$KGE$Correlation,B4MCPt$KGE$Correlation,B5MCPt$KGE$Correlation,B6MCPt$KGE$Correlation,B7MCPt$KGE$Correlation,B8MCPt$KGE$Correlation,B9MCPt$KGE$Correlation,B10MCPt$KGE$Correlation,B11MCPt$KGE$Correlation,B12MCPt$KGE$Correlation)
GMMCorrelationCAL=c(B1GMM$KGE$Correlation,B2GMM$KGE$Correlation,B3GMM$KGE$Correlation,B4GMM$KGE$Correlation,B5GMM$KGE$Correlation,B6GMM$KGE$Correlation,B7GMM$KGE$Correlation,B8GMM$KGE$Correlation,B9GMM$KGE$Correlation,B10GMM$KGE$Correlation,B11GMM$KGE$Correlation,B12GMM$KGE$Correlation)
#Bias.ratio
ABCBias.ratioCAL=c(B1ABC$KGE$Bias.ratio,B2ABC$KGE$Bias.ratio,B3ABC$KGE$Bias.ratio,B4ABC$KGE$Bias.ratio,B5ABC$KGE$Bias.ratio,B6ABC$KGE$Bias.ratio,B7ABC$KGE$Bias.ratio,B8ABC$KGE$Bias.ratio,B9ABC$KGE$Bias.ratio,B10ABC$KGE$Bias.ratio,B11ABC$KGE$Bias.ratio,B12ABC$KGE$Bias.ratio)
MCMCBias.ratioCAL =c(B1MCMC$KGE$Bias.ratio,B2MCMC$KGE$Bias.ratio,B3MCMC$KGE$Bias.ratio,B4MCMC$KGE$Bias.ratio,B5MCMC$KGE$Bias.ratio,B6MCMC$KGE$Bias.ratio,B7MCMC$KGE$Bias.ratio,B8MCMC$KGE$Bias.ratio,B9MCMC$KGE$Bias.ratio,B10MCMC$KGE$Bias.ratio,B11MCMC$KGE$Bias.ratio,B12MCMC$KGE$Bias.ratio)
MCPBias.ratioCAL =c(B1MCP$KGE$Bias.ratio,B2MCP$KGE$Bias.ratio,B3MCP$KGE$Bias.ratio,B4MCP$KGE$Bias.ratio,B5MCP$KGE$Bias.ratio,B6MCP$KGE$Bias.ratio,B7MCP$KGE$Bias.ratio,B8MCP$KGE$Bias.ratio,B9MCP$KGE$Bias.ratio,B10MCP$KGE$Bias.ratio,B11MCP$KGE$Bias.ratio,B12MCP$KGE$Bias.ratio)
MCPtBias.ratioCAL =c(B1MCPt$KGE$Bias.ratio,B2MCPt$KGE$Bias.ratio,B3MCPt$KGE$Bias.ratio,B4MCPt$KGE$Bias.ratio,B5MCPt$KGE$Bias.ratio,B6MCPt$KGE$Bias.ratio,B7MCPt$KGE$Bias.ratio,B8MCPt$KGE$Bias.ratio,B9MCPt$KGE$Bias.ratio,B10MCPt$KGE$Bias.ratio,B11MCPt$KGE$Bias.ratio,B12MCPt$KGE$Bias.ratio)
GMMBias.ratioCAL=c(B1GMM$KGE$Bias.ratio,B2GMM$KGE$Bias.ratio,B3GMM$KGE$Bias.ratio,B4GMM$KGE$Bias.ratio,B5GMM$KGE$Bias.ratio,B6GMM$KGE$Bias.ratio,B7GMM$KGE$Bias.ratio,B8GMM$KGE$Bias.ratio,B9GMM$KGE$Bias.ratio,B10GMM$KGE$Bias.ratio,B11GMM$KGE$Bias.ratio,B12GMM$KGE$Bias.ratio)

#matrices CALIBRACION
M1NCAL = rbind(ABCNCAL,MCMCNCAL,MCPNCAL,MCPtNCAL,GMMNCAL)
rownames(M1NCAL)<-c ("ABC ","MCMC","MCP","MCPt","GMM")
colnames(M1NCAL)<-c ("1 ","2","3","4","5","6 ","7","8","9","10","11","12")
#d_factorCAL
M2KGECAL = rbind(ABCKGECAL,MCMCKGECAL,MCPKGECAL,MCPtKGECAL,GMMKGECAL)
rownames(M2KGECAL)<-c ("ABC ","MCMC","MCP","MCPt","GMM")
colnames(M2KGECAL)<-c ("1 ","2","3","4","5","6 ","7","8","9","10","11","12")
#PRECISION
M3PGMMCorrelationCAL = rbind(ABCCorrelationCAL,MCMCCorrelationCAL,MCPCorrelationCAL ,MCPtCorrelationCAL,GMMCorrelationCAL)
rownames(M3PGMMCorrelationCAL)<-c ("ABC ","MCMC","MCP","MCPt","GMM")
colnames(M3PGMMCorrelationCAL)<-c ("1 ","2","3","4","5","6 ","7","8","9","10","11","12")
#PM4Bias.ratio
M4Bias.ratioCAL= rbind(ABCBias.ratioCAL ,MCMCBias.ratioCAL,MCPBias.ratioCAL,MCPtBias.ratioCAL,GMMBias.ratioCAL)
rownames(M4Bias.ratioCAL)<-c ("ABC ","MCMC","MCP","MCPt","GMM")
colnames(M4Bias.ratioCAL)<-c ("1 ","2","3","4","5","6 ","7","8","9","10","11","12")
#-----------------------------------------------------------------
#% DE MEJORA O EMPEORA CALIRACION y validacion

NASH12CUENCASCRUDOCAL = rbind(NSEcalq1.12,NSEcalq1.12,NSEcalq1.12,NSEcalq1.12,NSEcalq1.12)
NASH12CUENCASCRUDOv = rbind(NSEvalq1.12,NSEvalq1.12,NSEvalq1.12,NSEvalq1.12,NSEvalq1.12)
KGE12CUENCASCRUDOCAL = rbind(KGEcalq1.12,KGEcalq1.12,KGEcalq1.12,KGEcalq1.12,KGEcalq1.12)
KGE12CUENCASCRUDOv = rbind(KGEvalq1.12,KGEvalq1.12,KGEvalq1.12,KGEvalq1.12,KGEvalq1.12)
Correlation12CUENCASCRUDOCAL = rbind(Correlationcalq1.12,Correlationcalq1.12,Correlationcalq1.12,Correlationcalq1.12,Correlationcalq1.12)
Correlation12CUENCASCRUDOv = rbind(Correlationvalq1.12,Correlationvalq1.12,Correlationvalq1.12,Correlationvalq1.12,Correlationvalq1.12)
Bias.ratio12CUENCASCRUDOCAL = rbind(Bias.ratiocalq1.12,Bias.ratiocalq1.12,Bias.ratiocalq1.12,Bias.ratiocalq1.12,Bias.ratiocalq1.12)
Bias.ratio12CUENCASCRUDOv = rbind(Bias.ratiovalq1.12,Bias.ratiovalq1.12,Bias.ratiovalq1.12,Bias.ratiovalq1.12,Bias.ratiovalq1.12)

##CÁLCULO DEL PORCENTAJE
NASH_1<-(M1Nv-NASH12CUENCASCRUDOv)/NASH12CUENCASCRUDOv 
KGE_1<-(M2KGEv-KGE12CUENCASCRUDOv)/KGE12CUENCASCRUDOv
Pearson_Correlation_1<-(M3PGMMCorrelation-Correlation12CUENCASCRUDOv)/Correlation12CUENCASCRUDOv
Bias_ratio_1<-(M4Bias.ratio-Bias.ratio12CUENCASCRUDOv)/Bias.ratio12CUENCASCRUDOv

NASHCAL1<-(M1NCAL-NASH12CUENCASCRUDOCAL)/NASH12CUENCASCRUDOCAL
KGE_CAL1<-(M2KGECAL-KGE12CUENCASCRUDOCAL)/KGE12CUENCASCRUDOCAL
Pearson_Correlation_CAL1<-(M3PGMMCorrelationCAL-Correlation12CUENCASCRUDOCAL)/Correlation12CUENCASCRUDOCAL
Bias_ratio_CAL1<-(M4Bias.ratioCAL-Bias.ratio12CUENCASCRUDOCAL)/Bias.ratio12CUENCASCRUDOCAL

NASH_<-NASH_1*100
KGE_<-KGE_1*100
Pearson_Correlation_<-Pearson_Correlation_1*100
Bias_ratio_<-Bias_ratio_1*100

NASHCAL<-NASHCAL1*100
KGE_CAL<-KGE_CAL1*100
Pearson_Correlation_CAL<-Pearson_Correlation_CAL1*100
Bias_ratio_CAL<-Bias_ratio_CAL1*100

#rownames(NASH_)<-c ("MCMC","MCP","MCPt","GMM")
rownames(NASH_)<-c ("ABC ","MCMC","MCP","MCPt","GMM")
colnames(NASH_)<-c ("1 ","2","3","4","5","6 ","7","8","9","10","11","12")
rownames(NASHCAL)<-c ("ABC ","MCMC","MCP","MCPt","GMM")
colnames(NASHCAL)<-c ("1 ","2","3","4","5","6 ","7","8","9","10","11","12")
#KGE_CAL
rownames(KGE_)<-c ("ABC ","MCMC","MCP","MCPt","GMM")
colnames(KGE_)<-c ("1 ","2","3","4","5","6 ","7","8","9","10","11","12")
rownames(KGE_CAL)<-c ("ABC ","MCMC","MCP","MCPt","GMM")
colnames(KGE_CAL)<-c ("1 ","2","3","4","5","6 ","7","8","9","10","11","12")
#Pearson_Correlation_CAL
rownames(Pearson_Correlation_)<-c ("ABC ","MCMC","MCP","MCPt","GMM")
colnames(Pearson_Correlation_)<-c ("1 ","2","3","4","5","6 ","7","8","9","10","11","12")
rownames(Pearson_Correlation_CAL)<-c ("ABC ","MCMC","MCP","MCPt","GMM")
colnames(Pearson_Correlation_CAL)<-c ("1 ","2","3","4","5","6 ","7","8","9","10","11","12")
#PoinsInsideBand
rownames(Bias_ratio_)<-c ("ABC ","MCMC","MCP","MCPt","GMM")
colnames(Bias_ratio_)<-c ("1 ","2","3","4","5","6 ","7","8","9","10","11","12")
rownames(Bias_ratio_CAL)<-c ("ABC ","MCMC","MCP","MCPt","GMM")
colnames(Bias_ratio_CAL)<-c ("1 ","2","3","4","5","6 ","7","8","9","10","11","12")

#grafica en matrix validación
col.5 <- colorRampPalette(c('red','yellow',"light green","magenta",'blue'))(150)
col.6 <- colorRampPalette(c('red','yellow',"light green","magenta",'blue'))(150)
col.7 <- colorRampPalette(c('yellow',"light green","magenta",'blue'))(150)
#print(levelplot(t(M2dfactorv [c(nrow(M2dfactorv):1) ,]),xlab="Basin",ylab="post-processor",main="D-factor",cex.axis=24,cex.lab = 24,cex.main=24,col.regions=colorRampPalette(brewer.pal(9, 'GnBu')),at=seq(0, 2, length=30), aspect='iso',scales=list(alternating=FALSE, tck=1:0)),split=c(1, 2, 2, 2), newpage=FALSE)
setwd("F:/TesinaMasterCoDireccion/PaperMOPEX")

jpeg("./Fig4AccuracyVal.jpg", res = 600, width = 10, height = 6, units = "in")
#plot.new()
par(mfrow=c(2,2), oma=c(2,2,2,2),mar=c(6,4,6,2))
print(levelplot(t(NASH_ [c(nrow(NASH_):1) ,]),cex.axis = 2, cex.lab = 2,xlab="Catchment",ylab="Post-processor",main="Nash-Sutcliffe efficiency (NSE)",col.regions=col.5,at=seq(min(NASH_), max(NASH_), length=30), aspect='iso',scales=list(alternating=FALSE, tck=1:0)),split=c(1, 1, 2, 2))
print(levelplot(t(KGE_ [c(nrow(KGE_):1) ,]),xlab="Catchment",ylab="Post-processor",main="Kling-Gupta efficiency (KGE)",col.regions=col.5,at=seq(min(KGE_ ), max(KGE_ ), length=30), aspect='iso',scales=list(alternating=FALSE, tck=1:0)),split=c(1, 2, 2, 2), newpage=FALSE)
print(levelplot(t(Pearson_Correlation_ [c(nrow(Pearson_Correlation_):1) ,]),xlab="Catchment",ylab="Post-processor",main="Pearson Correlation",col.regions=col.5,at=seq(min(Pearson_Correlation_), max(Pearson_Correlation_), length=100), aspect='iso',scales=list(alternating=FALSE, tck=1:0)),split=c(2, 1, 2, 2), newpage=FALSE)
print(levelplot(t(Bias_ratio_ [c(nrow(Bias_ratio_):1) ,]),xlab="Catchment",ylab="Post-processor",main="Bias Ratio",col.regions=col.5,at=seq(min(Bias_ratio_), max(Bias_ratio_), length=100), aspect='iso',scales=list(alternating=FALSE, tck=1:0)),split=c(2, 2, 2, 2), newpage=FALSE)
#title("Centered Overall Title", outer=TRUE)
#mtext(side=1, "Centered Subtitle", outer=TRUE)
dev.off()


# Figure 4 % improve NSE #----------------------------------------------------
setwd("F:/TesinaMasterCoDireccion/PaperMOPEX")
jpeg("./Fig4AccuracyNSEval.jpg", res = 600, width = 10, height = 6, units = "in")
NSEvalImp <- NASH_[-1,] # remove ABC metrics
par(mar=c(3,4,2,2))
ckey <- list(labels=list(cex=1.5)) # size legend
levelplot(t(NSEvalImp [c(nrow(NSEvalImp):1) ,]),cex.axis = 1.5, cex = 1.5, cex.lab = 1.5,xlab=list("Catchment", cex = 1.5), ylab=list("Post-processor", cex = 1.5), main=list("Percent changes in NSE", cex = 1.5),col.regions=col.5,at=seq(-15.1, 15, length=30), aspect='iso',scales = list(cex=1.5),  colorkey=ckey) #scales = list(cex=2), scales=list(alternating=FALSE, tck=1:0)
#levelplot(t(M1Nv [c(nrow(M1Nv):1) ,]),cex.axis = 2, cex.lab = 2,xlab="Catchment",ylab="Post-processor",main="Nash-Sutcliffe efficiency (NSE)",col.regions=col.l,at=seq(min(M1Nv), max(M1Nv), length=30), aspect='iso',scales=list(alternating=FALSE, tck=1:0))   # min(NSEvalImp), max(NSEvalImp)
dev.off()

jpeg("./Fig4AccuracyCal.jpg", res = 600, width = 10, height = 6, units = "in")
#plot.new()
par(mfrow=c(2,2), oma=c(2,2,2,2),mar=c(6,4,6,2))
print(levelplot(t(NASHCAL [c(nrow(NASHCAL):1) ,]),cex.axis = 2, cex.lab = 2,xlab="Catchment",ylab="Post-processor",main="Nash-Sutcliffe efficiency (NSE)",col.regions=col.7,at=seq(min(NASHCAL), max(NASHCAL), length=30), aspect='iso',scales=list(alternating=FALSE, tck=1:0)),split=c(1, 1, 2, 2))
print(levelplot(t(KGE_CAL [c(nrow(KGE_CAL):1) ,]),xlab="Catchment",ylab="Post-processor",main="Kling-Gupta efficiency (KGE)",col.regions=col.5,at=seq(min(KGE_CAL ), max(KGE_CAL ), length=30), aspect='iso',scales=list(alternating=FALSE, tck=1:0)),split=c(1, 2, 2, 2), newpage=FALSE)
print(levelplot(t(Pearson_Correlation_CAL [c(nrow(Pearson_Correlation_CAL):1) ,]),xlab="Catchment",ylab="Post-processor",main="Pearson Correlation",col.regions=col.7,at=seq(min(Pearson_Correlation_CAL), max(Pearson_Correlation_CAL), length=100), aspect='iso',scales=list(alternating=FALSE, tck=1:0)),split=c(2, 1, 2, 2), newpage=FALSE)
print(levelplot(t(Bias_ratio_CAL [c(nrow(Bias_ratio_CAL):1) ,]),xlab="Catchment",ylab="Post-processor",main="Bias Ratio",col.regions=col.5,at=seq(min(Bias_ratio_CAL), max(Bias_ratio_CAL), length=100), aspect='iso',scales=list(alternating=FALSE, tck=1:0)),split=c(2, 2, 2, 2), newpage=FALSE)
#title("Centered Overall Title", outer=TRUE)
#mtext(side=1, "Centered Subtitle", outer=TRUE)
dev.off()

# Fig4 heatmap %NSE with numbers
library(reshape2)
library(ggplot2)
library(plotly)
NSEvalImp2 <- round (NSEvalImp, 2) # define only 2 decimals
rownames(NSEvalImp2)<-c ("MCMC","MCP","MCPt","GMM")
colnames(NSEvalImp2)<-c ("1 ","2","3","4","5","6 ","7","8","9","10","11","12")
# colnames(NSEvalImp2)<-c ("B1 ","B2","B3","B4","B5","B6 ","B7","B8","B9","B10","B11","B12")
NSEvalImp2 <- melt(NSEvalImp2)     # convert data frame
b <- c(-1,0,15,50)
colors <- c('azure2','lightsteelblue2','dodgerblue4')
jpeg("./Fig4AccuracyNSEval2.jpg", res = 600, width = 10, height = 5, units = "in")
ggheatmap <- ggplot(NSEvalImp2, aes(Var2, Var1, fill = value))+
  geom_tile(color = "black")+
scale_fill_gradient2(low = "red", high = "blue", limit = c(-15.1,15), space = "Lab", 
                       name="")+
  theme_classic()+
  theme(axis.text.x = element_text(colour = "gray9", size = 15), 
        axis.text.y = element_text(colour = "gray9", size = 15))+
  # ggtitle("Percent changes in NSE")+
  theme(plot.title = element_text(lineheight=4, face="bold"))+
  labs(x = "Catchment", y = "Post-processsor")+
coord_fixed()
print(ggheatmap)
ggheatmap + 
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) 
dev.off()

# #% DE MEJORA O EMPEORA CALIRACION y validacion
# NASH_<-(as.matrix(NASH_))
# NASHCAL<-as.matrix(NASHCAL)
# KGE_<-as.matrix(KGE_)
# KGE_CAL<-as.matrix(KGE_CAL)
# Pearson_Correlation_<-as.matrix(Pearson_Correlation_)
# Pearson_Correlation_CAL<-as.matrix(Pearson_Correlation_CAL)
# Bias_ratio_<-as.matrix(Bias_ratio_)
# Bias_ratio_CAL<-as.matrix(Bias_ratio_CAL)
# 
# ##nombre columnas y filas - calibracion y validacion
# rownames(NASH_)<-c ("ABC ","MCMC","MCP","MCPt","GMM")
# colnames(NASH_)<-c ("1 ","2","3","4","5","6 ","7","8","9","10","11","12")
# rownames(NASHCAL)<-c ("ABC ","MCMC","MCP","MCPt","GMM")
# colnames(NASHCAL)<-c ("1 ","2","3","4","5","6 ","7","8","9","10","11","12")
# #KGE_CAL
# rownames(KGE_)<-c ("ABC ","MCMC","MCP","MCPt","GMM")
# colnames(KGE_)<-c ("1 ","2","3","4","5","6 ","7","8","9","10","11","12")
# rownames(KGE_CAL)<-c ("ABC ","MCMC","MCP","MCPt","GMM")
# colnames(KGE_CAL)<-c ("1 ","2","3","4","5","6 ","7","8","9","10","11","12")
# #Pearson_Correlation_CAL
# rownames(Pearson_Correlation_)<-c ("ABC ","MCMC","MCP","MCPt","GMM")
# colnames(Pearson_Correlation_)<-c ("1 ","2","3","4","5","6 ","7","8","9","10","11","12")
# rownames(Pearson_Correlation_CAL)<-c ("ABC ","MCMC","MCP","MCPt","GMM")
# colnames(Pearson_Correlation_CAL)<-c ("1 ","2","3","4","5","6 ","7","8","9","10","11","12")
# #PoinsInsideBand
# rownames(Bias_ratio_)<-c ("ABC ","MCMC","MCP","MCPt","GMM")
# colnames(Bias_ratio_)<-c ("1 ","2","3","4","5","6 ","7","8","9","10","11","12")
# rownames(Bias_ratio_CAL)<-c ("ABC ","MCMC","MCP","MCPt","GMM")
# colnames(Bias_ratio_CAL)<-c ("1 ","2","3","4","5","6 ","7","8","9","10","11","12")
# 
# #grafica en matrix validación
# col.5 <- colorRampPalette(c('red','yellow',"light green","magenta",'blue'))(150)
# col.6 <- colorRampPalette(c('red','yellow',"light green","magenta",'blue'))(150)
# col.7 <- colorRampPalette(c('yellow',"light green","magenta",'blue'))(150)
# #print(levelplot(t(M2dfactorv [c(nrow(M2dfactorv):1) ,]),xlab="Basin",ylab="post-processor",main="D-factor",cex.axis=24,cex.lab = 24,cex.main=24,col.regions=colorRampPalette(brewer.pal(9, 'GnBu')),at=seq(0, 2, length=30), aspect='iso',scales=list(alternating=FALSE, tck=1:0)),split=c(1, 2, 2, 2), newpage=FALSE)
# #setwd("F:/TesinaMasterCoDireccion/PaperMOPEX")
# 
# jpeg("./Fig4AccuracyVal.jpg", res = 600, width = 10, height = 6, units = "in")
# #plot.new()
# par(mfrow=c(2,2), oma=c(2,2,2,2),mar=c(6,4,6,2))
# print(levelplot(t(NASH_ [c(nrow(NASH_):1) ,]),cex.axis = 2, cex.lab = 2,xlab="Catchment",ylab="Post-processor",main="Nash-Sutcliffe efficiency (NSE)",col.regions=col.5,at=seq(min(NASH_), max(NASH_), length=30), aspect='iso',scales=list(alternating=FALSE, tck=1:0)),split=c(1, 1, 2, 2))
# print(levelplot(t(KGE_ [c(nrow(KGE_):1) ,]),xlab="Catchment",ylab="Post-processor",main="Kling-Gupta efficiency (KGE)",col.regions=col.5,at=seq(min(KGE_ ), max(KGE_ ), length=30), aspect='iso',scales=list(alternating=FALSE, tck=1:0)),split=c(1, 2, 2, 2), newpage=FALSE)
# print(levelplot(t(Pearson_Correlation_ [c(nrow(Pearson_Correlation_):1) ,]),xlab="Catchment",ylab="Post-processor",main="Pearson Correlation",col.regions=col.5,at=seq(min(Pearson_Correlation_), max(Pearson_Correlation_), length=100), aspect='iso',scales=list(alternating=FALSE, tck=1:0)),split=c(2, 1, 2, 2), newpage=FALSE)
# print(levelplot(t(Bias_ratio_ [c(nrow(Bias_ratio_):1) ,]),xlab="Catchment",ylab="Post-processor",main="Bias Ratio",col.regions=col.5,at=seq(min(Bias_ratio_), max(Bias_ratio_), length=100), aspect='iso',scales=list(alternating=FALSE, tck=1:0)),split=c(2, 2, 2, 2), newpage=FALSE)
# #title("Centered Overall Title", outer=TRUE)
# #mtext(side=1, "Centered Subtitle", outer=TRUE)
# dev.off()
# 
# jpeg("./Fig4AccuracyCal.jpg", res = 600, width = 10, height = 6, units = "in")
# #plot.new()
# par(mfrow=c(2,2), oma=c(2,2,2,2),mar=c(6,4,6,2))
# print(levelplot(t(NASHCAL [c(nrow(NASHCAL):1) ,]),cex.axis = 2, cex.lab = 2,xlab="Catchment",ylab="Post-processor",main="Nash-Sutcliffe efficiency (NSE)",col.regions=col.7,at=seq(min(NASHCAL), max(NASHCAL), length=30), aspect='iso',scales=list(alternating=FALSE, tck=1:0)),split=c(1, 1, 2, 2))
# print(levelplot(t(KGE_CAL [c(nrow(KGE_CAL):1) ,]),xlab="Catchment",ylab="Post-processor",main="Kling-Gupta efficiency (KGE)",col.regions=col.5,at=seq(min(KGE_CAL ), max(KGE_CAL ), length=30), aspect='iso',scales=list(alternating=FALSE, tck=1:0)),split=c(1, 2, 2, 2), newpage=FALSE)
# print(levelplot(t(Pearson_Correlation_CAL [c(nrow(Pearson_Correlation_CAL):1) ,]),xlab="Catchment",ylab="Post-processor",main="Pearson Correlation",col.regions=col.7,at=seq(min(Pearson_Correlation_CAL), max(Pearson_Correlation_CAL), length=100), aspect='iso',scales=list(alternating=FALSE, tck=1:0)),split=c(2, 1, 2, 2), newpage=FALSE)
# print(levelplot(t(Bias_ratio_CAL [c(nrow(Bias_ratio_CAL):1) ,]),xlab="Catchment",ylab="Post-processor",main="Bias Ratio",col.regions=col.5,at=seq(min(Bias_ratio_CAL), max(Bias_ratio_CAL), length=100), aspect='iso',scales=list(alternating=FALSE, tck=1:0)),split=c(2, 2, 2, 2), newpage=FALSE)
# #title("Centered Overall Title", outer=TRUE)
# #mtext(side=1, "Centered Subtitle", outer=TRUE)
# dev.off()



#----------------------------------------------------


# fig 3 Heat map (3*3) for calibration and validation period
###GRAFICAS TESIS Y ARTICULO
#NASH DE POST PROCESADOR 1:12
if(!require(RColorBrewer)){install.packages('RColorBrewer'); library(RColorBrewer)} else {library(RColorBrewer)}
if(!require(graphics)){install.packages('graphics'); library(graphics)} else {library(graphics)}
if(!require(stats)){install.packages('stats'); library(stats)} else {library(stats)}
if(!require(plotly)){install.packages('plotly'); library(plotly)} else {library(plotly)}
if(!require(ggplot2)){install.packages('ggplot2'); library(ggplot2)} else {library(ggplot2)}
library(fitdistrplus)
library(moments)
library(actuar)
library(stats)
library(fitdistrplus)
library(hydroGOF)
library(grDevices)
library(MASS)
library(survival)
library(ks)
library(NSM3)      
library(Bolstad2)  
library(combinat) 
library(graphicsQC)
library(lattice)
library(tidyverse)
library(inegiR)
library(lubridate)
library(dplyr)
#calibracion
#NASH
ABCN=c(B1ABC$NSE,B2ABC$NSE,B3ABC$NSE,B4ABC$NSE,B5ABC$NSE,B6ABC$NSE,B7ABC$NSE,B8ABC$NSE,B9ABC$NSE,B10ABC$NSE,B11ABC$NSE,B12ABC$NSE)
MCMCN=c(B1MCMC$NSE,B2MCMC$NSE,B3MCMC$NSE,B4MCMC$NSE,B5MCMC$NSE,B6MCMC$NSE,B7MCMC$NSE,B8MCMC$NSE,B9MCMC$NSE,B10MCMC$NSE,B11MCMC$NSE,B12MCMC$NSE)
MCPN=c(B1MCP$NSE,B2MCP$NSE,B3MCP$NSE,B4MCP$NSE,B5MCP$NSE,B6MCP$NSE,B7MCP$NSE,B8MCP$NSE,B9MCP$NSE,B10MCP$NSE,B11MCP$NSE,B12MCP$NSE)
MCPtN=c(B1MCPt$NSE,B2MCPt$NSE,B3MCPt$NSE,B4MCPt$NSE,B5MCPt$NSE,B6MCPt$NSE,B7MCPt$NSE,B8MCPt$NSE,B9MCPt$NSE,B10MCPt$NSE,B11MCPt$NSE,B12MCPt$NSE)
GMMN=c(B1GMM$NSE,B2GMM$NSE,B3GMM$NSE,B4GMM$NSE,B5GMM$NSE,B6GMM$NSE,B7GMM$NSE,B8GMM$NSE,B9GMM$NSE,B10GMM$NSE,B11GMM$NSE,B12GMM$NSE)
#d_factor
ABCdfactor=c(B1ABC$PPU95$d_factor,B2ABC$PPU95$d_factor,B3ABC$PPU95$d_factor,B4ABC$PPU95$d_factor,B5ABC$PPU95$d_factor,B6ABC$PPU95$d_factor,B7ABC$PPU95$d_factor,B8ABC$PPU95$d_factor,B9ABC$PPU95$d_factor,B10ABC$PPU95$d_factor,B11ABC$PPU95$d_factor,B12ABC$PPU95$d_factor)
MCMCdfactor=c(B1MCMC$PPU95$d_factor,B2MCMC$PPU95$d_factor,B3MCMC$PPU95$d_factor,B4MCMC$PPU95$d_factor,B5MCMC$PPU95$d_factor,B6MCMC$PPU95$d_factor,B7MCMC$PPU95$d_factor,B8MCMC$PPU95$d_factor,B9MCMC$PPU95$d_factor,B10MCMC$PPU95$d_factor,B11MCMC$PPU95$d_factor,B12MCMC$PPU95$d_factor)
MCPdfactor=c(B1MCP$PPU95$d_factor,B2MCP$PPU95$d_factor,B3MCP$PPU95$d_factor,B4MCP$PPU95$d_factor,B5MCP$PPU95$d_factor,B6MCP$PPU95$d_factor,B7MCP$PPU95$d_factor,B8MCP$PPU95$d_factor,B9MCP$PPU95$d_factor,B10MCP$PPU95$d_factor,B11MCP$PPU95$d_factor,B12MCP$PPU95$d_factor)
MCPtdfactor=c(B1MCPt$PPU95$d_factor,B2MCPt$PPU95$d_factor,B3MCPt$PPU95$d_factor,B4MCPt$PPU95$d_factor,B5MCPt$PPU95$d_factor,B6MCPt$PPU95$d_factor,B7MCPt$PPU95$d_factor,B8MCPt$PPU95$d_factor,B9MCPt$PPU95$d_factor,B10MCPt$PPU95$d_factor,B11MCPt$PPU95$d_factor,B12MCPt$PPU95$d_factor)
GMMdfactor=c(B1GMM$PPU95$d_factor,B2GMM$PPU95$d_factor,B3GMM$PPU95$d_factor,B4GMM$PPU95$d_factor,B5GMM$PPU95$d_factor,B6GMM$PPU95$d_factor,B7GMM$PPU95$d_factor,B8GMM$PPU95$d_factor,B9GMM$PPU95$d_factor,B10GMM$PPU95$d_factor,B11GMM$PPU95$d_factor,B12GMM$PPU95$d_factor)
#PRECISION
ABCresolution=c(B1ABC$QQplot$resolution,B2ABC$QQplot$resolution,B3ABC$QQplot$resolution,B4ABC$QQplot$resolution,B5ABC$QQplot$resolution,B6ABC$QQplot$resolution,B7ABC$QQplot$resolution,B8ABC$QQplot$resolution,B9ABC$QQplot$resolution,B10ABC$QQplot$resolution,B11ABC$QQplot$resolution,B12ABC$QQplot$resolution)
MCMCresolution=c(B1MCMC$QQplot$resolution,B2MCMC$QQplot$resolution,B3MCMC$QQplot$resolution,B4MCMC$QQplot$resolution,B5MCMC$QQplot$resolution,B6MCMC$QQplot$resolution,B7MCMC$QQplot$resolution,B8MCMC$QQplot$resolution,B9MCMC$QQplot$resolution,B10MCMC$QQplot$resolution,B11MCMC$QQplot$resolution,B12MCMC$QQplot$resolution)
MCPresolution=c(B1MCP$QQplot$resolution,B2MCP$QQplot$resolution,B3MCP$QQplot$resolution,B4MCP$QQplot$resolution,B5MCP$QQplot$resolution,B6MCP$QQplot$resolution,B7MCP$QQplot$resolution,B8MCP$QQplot$resolution,B9MCP$QQplot$resolution,B10MCP$QQplot$resolution,B11MCP$QQplot$resolution,B12MCP$QQplot$resolution)
MCPtresolution=c(B1MCPt$QQplot$resolution,B2MCPt$QQplot$resolution,B3MCPt$QQplot$resolution,B4MCPt$QQplot$resolution,B5MCPt$QQplot$resolution,B6MCPt$QQplot$resolution,B7MCPt$QQplot$resolution,B8MCPt$QQplot$resolution,B9MCPt$QQplot$resolution,B10MCPt$QQplot$resolution,B11MCPt$QQplot$resolution,B12MCPt$QQplot$resolution)
GMMresolution=c(B1GMM$QQplot$resolution,B2GMM$QQplot$resolution,B3GMM$QQplot$resolution,B4GMM$QQplot$resolution,B5GMM$QQplot$resolution,B6GMM$QQplot$resolution,B7GMM$QQplot$resolution,B8GMM$QQplot$resolution,B9GMM$QQplot$resolution,B10GMM$QQplot$resolution,B11GMM$QQplot$resolution,B12GMM$QQplot$resolution)
#PoinsInsideBand
ABCPoinsInsideBand=c(B1ABC$PoinsInsideBand,B2ABC$PoinsInsideBand,B3ABC$PoinsInsideBand,B4ABC$PoinsInsideBand,B5ABC$PoinsInsideBand,B6ABC$PoinsInsideBand,B7ABC$PoinsInsideBand,B8ABC$PoinsInsideBand,B9ABC$PoinsInsideBand,B10ABC$PoinsInsideBand,B11ABC$PoinsInsideBand,B12ABC$PoinsInsideBand)
MCMCPoinsInsideBand=c(B1MCMC$PoinsInsideBand,B2MCMC$PoinsInsideBand,B3MCMC$PoinsInsideBand,B4MCMC$PoinsInsideBand,B5MCMC$PoinsInsideBand,B6MCMC$PoinsInsideBand,B7MCMC$PoinsInsideBand,B8MCMC$PoinsInsideBand,B9MCMC$PoinsInsideBand,B10MCMC$PoinsInsideBand,B11MCMC$PoinsInsideBand,B12MCMC$PoinsInsideBand)
MCPPoinsInsideBand=c(B1MCP$PoinsInsideBand,B2MCP$PoinsInsideBand,B3MCP$PoinsInsideBand,B4MCP$PoinsInsideBand,B5MCP$PoinsInsideBand,B6MCP$PoinsInsideBand,B7MCP$PoinsInsideBand,B8MCP$PoinsInsideBand,B9MCP$PoinsInsideBand,B10MCP$PoinsInsideBand,B11MCP$PoinsInsideBand,B12MCP$PoinsInsideBand)
MCPtPoinsInsideBand=c(B1MCPt$PoinsInsideBand,B2MCPt$PoinsInsideBand,B3MCPt$PoinsInsideBand,B4MCPt$PoinsInsideBand,B5MCPt$PoinsInsideBand,B6MCPt$PoinsInsideBand,B7MCPt$PoinsInsideBand,B8MCPt$PoinsInsideBand,B9MCPt$PoinsInsideBand,B10MCPt$PoinsInsideBand,B11MCPt$PoinsInsideBand,B12MCPt$PoinsInsideBand)
GMMPoinsInsideBand=c(B1GMM$PoinsInsideBand,B2GMM$PoinsInsideBand,B3GMM$PoinsInsideBand,B4GMM$PoinsInsideBand,B5GMM$PoinsInsideBand,B6GMM$PoinsInsideBand,B7GMM$PoinsInsideBand,B8GMM$PoinsInsideBand,B9GMM$PoinsInsideBand,B10GMM$PoinsInsideBand,B11GMM$PoinsInsideBand,B12GMM$PoinsInsideBand)

#validación
#NASHv
ABCNv=c(B1ABC$NSEv,B2ABC$NSEv,B3ABC$NSEv,B4ABC$NSEv,B5ABC$NSEv,B6ABC$NSEv,B7ABC$NSEv,B8ABC$NSEv,B9ABC$NSEv,B10ABC$NSEv,B11ABC$NSEv,B12ABC$NSEv)
MCMCNv=c(B1MCMC$NSEv,B2MCMC$NSEv,B3MCMC$NSEv,B4MCMC$NSEv,B5MCMC$NSEv,B6MCMC$NSEv,B7MCMC$NSEv,B8MCMC$NSEv,B9MCMC$NSEv,B10MCMC$NSEv,B11MCMC$NSEv,B12MCMC$NSEv)
MCPNv=c(B1MCP$NSEv,B2MCP$NSEv,B3MCP$NSEv,B4MCP$NSEv,B5MCP$NSEv,B6MCP$NSEv,B7MCP$NSEv,B8MCP$NSEv,B9MCP$NSEv,B10MCP$NSEv,B11MCP$NSEv,B12MCP$NSEv)
MCPtNv=c(B1MCPt$NSEv,B2MCPt$NSEv,B3MCPt$NSEv,B4MCPt$NSEv,B5MCPt$NSEv,B6MCPt$NSEv,B7MCPt$NSEv,B8MCPt$NSEv,B9MCPt$NSEv,B10MCPt$NSEv,B11MCPt$NSEv,B12MCPt$NSEv)
GMMNv=c(B1GMM$NSEv,B2GMM$NSEv,B3GMM$NSEv,B4GMM$NSEv,B5GMM$NSEv,B6GMM$NSEv,B7GMM$NSEv,B8GMM$NSEv,B9GMM$NSEv,B10GMM$NSEv,B11GMM$NSEv,B12GMM$NSEv)
#NASHv order by wet to dry
# ABCNv=c(B2ABC$NSEv,B3ABC$NSEv,B5ABC$NSEv,B8ABC$NSEv,B1ABC$NSEv,B7ABC$NSEv,B9ABC$NSEv,B6ABC$NSEv,B4ABC$NSEv,B10ABC$NSEv,B11ABC$NSEv,B12ABC$NSEv)
# MCMCNv=c(B2MCMC$NSEv,B3MCMC$NSEv,B5MCMC$NSEv,B8MCMC$NSEv,B1MCMC$NSEv,B7MCMC$NSEv,B9MCMC$NSEv,B4MCMC$NSEv,B9MCMC$NSEv,B10MCMC$NSEv,B11MCMC$NSEv,B12MCMC$NSEv)
# MCPNv=c(B2MCP$NSEv,B3MCP$NSEv,B5MCP$NSEv,B8MCP$NSEv,B1MCP$NSEv,B7MCP$NSEv,B9MCP$NSEv,B6MCP$NSEv,B4MCP$NSEv,B10MCP$NSEv,B11MCP$NSEv,B12MCP$NSEv)
# MCPtNv=c(B2MCPt$NSEv,B3MCPt$NSEv,B5MCPt$NSEv,B8MCPt$NSEv,B1MCPt$NSEv,B7MCPt$NSEv,B9MCPt$NSEv,B4MCPt$NSEv,B9MCPt$NSEv,B10MCPt$NSEv,B11MCPt$NSEv,B12MCPt$NSEv)
# GMMNv=c(B2GMM$NSEv,B3GMM$NSEv,B5GMM$NSEv,B8GMM$NSEv,B1GMM$NSEv,B7GMM$NSEv,B9GMM$NSEv,B6GMM$NSEv,B4GMM$NSEv,B10GMM$NSEv,B11GMM$NSEv,B12GMM$NSEv)

#d_factorv
ABCdfactorv=c(B1ABC$PPU95v$d_factor,B2ABC$PPU95v$d_factor,B3ABC$PPU95v$d_factor,B4ABC$PPU95v$d_factor,B5ABC$PPU95v$d_factor,B6ABC$PPU95v$d_factor,B7ABC$PPU95v$d_factor,B8ABC$PPU95v$d_factor,B9ABC$PPU95v$d_factor,B10ABC$PPU95v$d_factor,B11ABC$PPU95v$d_factor,B12ABC$PPU95v$d_factor)
MCMCdfactorv=c(B1MCMC$PPU95v$d_factor,B2MCMC$PPU95v$d_factor,B3MCMC$PPU95v$d_factor,B4MCMC$PPU95v$d_factor,B5MCMC$PPU95v$d_factor,B6MCMC$PPU95v$d_factor,B7MCMC$PPU95v$d_factor,B8MCMC$PPU95v$d_factor,B9MCMC$PPU95v$d_factor,B10MCMC$PPU95v$d_factor,B11MCMC$PPU95v$d_factor,B12MCMC$PPU95v$d_factor)
MCPdfactorv=c(B1MCP$PPU95v$d_factor,B2MCP$PPU95v$d_factor,B3MCP$PPU95v$d_factor,B4MCP$PPU95v$d_factor,B5MCP$PPU95v$d_factor,B6MCP$PPU95v$d_factor,B7MCP$PPU95v$d_factor,B8MCP$PPU95v$d_factor,B9MCP$PPU95v$d_factor,B10MCP$PPU95v$d_factor,B11MCP$PPU95v$d_factor,B12MCP$PPU95v$d_factor)
MCPtdfactorv=c(B1MCPt$PPU95v$d_factor,B2MCPt$PPU95v$d_factor,B3MCPt$PPU95v$d_factor,B4MCPt$PPU95v$d_factor,B5MCPt$PPU95v$d_factor,B6MCPt$PPU95v$d_factor,B7MCPt$PPU95v$d_factor,B8MCPt$PPU95v$d_factor,B9MCPt$PPU95v$d_factor,B10MCPt$PPU95v$d_factor,B11MCPt$PPU95v$d_factor,B12MCPt$PPU95v$d_factor)
GMMdfactorv=c(B1GMM$PPU95v$d_factor,B2GMM$PPU95v$d_factor,B3GMM$PPU95v$d_factor,B4GMM$PPU95v$d_factor,B5GMM$PPU95v$d_factor,B6GMM$PPU95v$d_factor,B7GMM$PPU95v$d_factor,B8GMM$PPU95v$d_factor,B9GMM$PPU95v$d_factor,B10GMM$PPU95v$d_factor,B11GMM$PPU95v$d_factor,B12GMM$PPU95v$d_factor)
#PRECISIONv
ABCresolutionv=c(B1ABC$QQplotv$resolution,B2ABC$QQplotv$resolution,B3ABC$QQplotv$resolution,B4ABC$QQplotv$resolution,B5ABC$QQplotv$resolution,B6ABC$QQplotv$resolution,B7ABC$QQplotv$resolution,B8ABC$QQplotv$resolution,B9ABC$QQplotv$resolution,B10ABC$QQplotv$resolution,B11ABC$QQplotv$resolution,B12ABC$QQplotv$resolution)
MCMCresolutionv=c(B1MCMC$QQplotv$resolution,B2MCMC$QQplotv$resolution,B3MCMC$QQplotv$resolution,B4MCMC$QQplotv$resolution,B5MCMC$QQplotv$resolution,B6MCMC$QQplotv$resolution,B7MCMC$QQplotv$resolution,B8MCMC$QQplotv$resolution,B9MCMC$QQplotv$resolution,B10MCMC$QQplotv$resolution,B11MCMC$QQplotv$resolution,B12MCMC$QQplotv$resolution)
MCPresolutionv=c(B1MCP$QQplotv$resolution,B2MCP$QQplotv$resolution,B3MCP$QQplotv$resolution,B4MCP$QQplotv$resolution,B5MCP$QQplotv$resolution,B6MCP$QQplotv$resolution,B7MCP$QQplotv$resolution,B8MCP$QQplotv$resolution,B9MCP$QQplotv$resolution,B10MCP$QQplotv$resolution,B11MCP$QQplotv$resolution,B12MCP$QQplotv$resolution)
MCPtresolutionv=c(B1MCPt$QQplotv$resolution,B2MCPt$QQplotv$resolution,B3MCPt$QQplotv$resolution,B4MCPt$QQplotv$resolution,B5MCPt$QQplotv$resolution,B6MCPt$QQplotv$resolution,B7MCPt$QQplotv$resolution,B8MCPt$QQplotv$resolution,B9MCPt$QQplotv$resolution,B10MCPt$QQplotv$resolution,B11MCPt$QQplotv$resolution,B12MCPt$QQplotv$resolution)
GMMresolutionv=c(B1GMM$QQplotv$resolution,B2GMM$QQplotv$resolution,B3GMM$QQplotv$resolution,B4GMM$QQplotv$resolution,B5GMM$QQplotv$resolution,B6GMM$QQplotv$resolution,B7GMM$QQplotv$resolution,B8GMM$QQplotv$resolution,B9GMM$QQplotv$resolution,B10GMM$QQplotv$resolution,B11GMM$QQplotv$resolution,B12GMM$QQplotv$resolution)
#PoinsInsideBandv
ABCPoinsInsideBandv=c(B1ABC$PoinsInsideBandv,B2ABC$PoinsInsideBandv,B3ABC$PoinsInsideBandv,B4ABC$PoinsInsideBandv,B5ABC$PoinsInsideBandv,B6ABC$PoinsInsideBandv,B7ABC$PoinsInsideBandv,B8ABC$PoinsInsideBandv,B9ABC$PoinsInsideBandv,B10ABC$PoinsInsideBandv,B11ABC$PoinsInsideBandv,B12ABC$PoinsInsideBandv)
MCMCPoinsInsideBandv=c(B1MCMC$PoinsInsideBandv,B2MCMC$PoinsInsideBandv,B3MCMC$PoinsInsideBandv,B4MCMC$PoinsInsideBandv,B5MCMC$PoinsInsideBandv,B6MCMC$PoinsInsideBandv,B7MCMC$PoinsInsideBandv,B8MCMC$PoinsInsideBandv,B9MCMC$PoinsInsideBandv,B10MCMC$PoinsInsideBandv,B11MCMC$PoinsInsideBandv,B12MCMC$PoinsInsideBandv)
MCPPoinsInsideBandv=c(B1MCP$PoinsInsideBandv,B2MCP$PoinsInsideBandv,B3MCP$PoinsInsideBandv,B4MCP$PoinsInsideBandv,B5MCP$PoinsInsideBandv,B6MCP$PoinsInsideBandv,B7MCP$PoinsInsideBandv,B8MCP$PoinsInsideBandv,B9MCP$PoinsInsideBandv,B10MCP$PoinsInsideBandv,B11MCP$PoinsInsideBandv,B12MCP$PoinsInsideBandv)
MCPtPoinsInsideBandv=c(B1MCPt$PoinsInsideBandv,B2MCPt$PoinsInsideBandv,B3MCPt$PoinsInsideBandv,B4MCPt$PoinsInsideBandv,B5MCPt$PoinsInsideBandv,B6MCPt$PoinsInsideBandv,B7MCPt$PoinsInsideBandv,B8MCPt$PoinsInsideBandv,B9MCPt$PoinsInsideBandv,B10MCPt$PoinsInsideBandv,B11MCPt$PoinsInsideBandv,B12MCPt$PoinsInsideBandv)
GMMPoinsInsideBandv=c(B1GMM$PoinsInsideBandv,B2GMM$PoinsInsideBandv,B3GMM$PoinsInsideBandv,B4GMM$PoinsInsideBandv,B5GMM$PoinsInsideBandv,B6GMM$PoinsInsideBandv,B7GMM$PoinsInsideBandv,B8GMM$PoinsInsideBandv,B9GMM$PoinsInsideBandv,B10GMM$PoinsInsideBandv,B11GMM$PoinsInsideBandv,B12GMM$PoinsInsideBandv)
# Reliability
ABCRelv=c(B1ABC$QQplotv$alpha,B2ABC$QQplotv$alpha,B3ABC$QQplotv$alpha,B4ABC$QQplotv$alpha,B5ABC$QQplotv$alpha,B6ABC$QQplotv$alpha,B7ABC$QQplotv$alpha,B8ABC$QQplotv$alpha,B9ABC$QQplotv$alpha,B10ABC$QQplotv$alpha,B11ABC$QQplotv$alpha,B12ABC$QQplotv$alpha)
MCMCRelv=c(B1MCMC$QQplotv$alpha,B2MCMC$QQplotv$alpha,B3MCMC$QQplotv$alpha,B4MCMC$QQplotv$alpha,B5MCMC$QQplotv$alpha,B6MCMC$QQplotv$alpha,B7MCMC$QQplotv$alpha,B8MCMC$QQplotv$alpha,B9MCMC$QQplotv$alpha,B10MCMC$QQplotv$alpha,B11MCMC$QQplotv$alpha,B12MCMC$QQplotv$alpha)
MCPRelv=c(B1MCP$QQplotv$alpha,B2MCP$QQplotv$alpha,B3MCP$QQplotv$alpha,B4MCP$QQplotv$alpha,B5MCP$QQplotv$alpha,B6MCP$QQplotv$alpha,B7MCP$QQplotv$alpha,B8MCP$QQplotv$alpha,B9MCP$QQplotv$alpha,B10MCP$QQplotv$alpha,B11MCP$QQplotv$alpha,B12MCP$QQplotv$alpha)
MCPtRelv=c(B1MCPt$QQplotv$alpha,B2MCPt$QQplotv$alpha,B3MCPt$QQplotv$alpha,B4MCPt$QQplotv$alpha,B5MCPt$QQplotv$alpha,B6MCPt$QQplotv$alpha,B7MCPt$QQplotv$alpha,B8MCPt$QQplotv$alpha,B9MCPt$QQplotv$alpha,B10MCPt$QQplotv$alpha,B11MCPt$QQplotv$alpha,B12MCPt$QQplotv$alpha)
GMMRelv=c(B1GMM$QQplotv$alpha,B2GMM$QQplotv$alpha,B3GMM$QQplotv$alpha,B4GMM$QQplotv$alpha,B5GMM$QQplotv$alpha,B6GMM$QQplotv$alpha,B7GMM$QQplotv$alpha,B8GMM$QQplotv$alpha,B9GMM$QQplotv$alpha,B10GMM$QQplotv$alpha,B11GMM$QQplotv$alpha,B12GMM$QQplotv$alpha)
# matrix reliability validation
Relv <-rbind(MCPNv,MCPtNv,GMMNv)
colnames(Relv)<-c ("1 ","2","3","4","5","6 ","7","8","9","10","11","12")
rownames(Relv)<-c ("MCP","MCPt","GMM")

#matrices
#VALIDACIÓN
#NASH
M1N = rbind(ABCN,MCMCN,MCPN,MCPtN,GMMN)
M1Nv = rbind(MCMCNv,MCPNv,MCPtNv,GMMNv)
# M1Nv = rbind(ABCNv,MCMCNv,MCPNv,MCPtNv,GMMNv)
rownames(M1N)<-c ("ABC ","MCMC","MCP","MCPt","GMM")
colnames(M1N)<-c ("1 ","2","3","4","5","6 ","7","8","9","10","11","12")
rownames(M1Nv)<-c ("MCMC","MCP","MCPt","GMM")
# rownames(M1Nv)<-c ("ABC ","MCMC","MCP","MCPt","GMM")
colnames(M1Nv)<-c ("1 ","2","3","4","5","6 ","7","8","9","10","11","12")

# order from wet to dry
#colnames(M1Nv)<-c ("2 ","3","5","8","1","7","9","6","4","10","11","12")

#d_factorv
M2dfactor = rbind(ABCdfactor,MCMCdfactor,MCPdfactor,MCPtN,GMMdfactor)
M2dfactorv = rbind(MCMCdfactorv,MCPdfactorv,MCPtdfactorv,GMMdfactorv)
# M2dfactorv = rbind(ABCdfactorv,MCMCdfactorv,MCPdfactorv,MCPtdfactorv,GMMdfactorv)
rownames(M2dfactor)<-c ("ABC ","MCMC","MCP","MCPt","GMM")
colnames(M2dfactor)<-c ("1 ","2","3","4","5","6 ","7","8","9","10","11","12")
rownames(M2dfactorv)<-c ("LRP","MCP","MCPt","GMCP")
# rownames(M2dfactorv)<-c ("ABC ","MCMC","MCP","MCPt","GMM")
colnames(M2dfactorv)<-c ("1 ","2","3","4","5","6 ","7","8","9","10","11","12")
#PRECISION
M3PRECISION = rbind(ABCresolution,MCMCresolution,MCPresolution,MCPtresolution,GMMresolution)
M3PRECISIONV = rbind(MCMCresolutionv,MCPresolutionv,MCPtresolutionv,GMMresolutionv)
#M3PRECISIONV = rbind(ABCresolutionv,MCMCresolutionv,MCPresolutionv,MCPtresolutionv,GMMresolutionv)
rownames(M3PRECISION)<-c ("ABC ","MCMC","MCP","MCPt","GMM")
colnames(M3PRECISION)<-c ("1 ","2","3","4","5","6 ","7","8","9","10","11","12")
rownames(M3PRECISIONV)<-c ("LRP","MCP","MCPt","GMCP")
# rownames(M3PRECISIONV)<-c ("ABC ","MCMC","MCP","MCPt","GMM")
colnames(M3PRECISIONV)<-c ("1 ","2","3","4","5","6 ","7","8","9","10","11","12")
#PoinsInsideBand
M4PoinsInsideBand = rbind(ABCPoinsInsideBand,MCMCPoinsInsideBand,MCPPoinsInsideBand,MCPtPoinsInsideBand,GMMPoinsInsideBand)
M4PoinsInsideBandV= rbind(MCMCPoinsInsideBandv,MCPPoinsInsideBandv,MCPtPoinsInsideBandv,GMMPoinsInsideBandv)
#M4PoinsInsideBandV= rbind(ABCPoinsInsideBandv,MCMCPoinsInsideBandv,MCPPoinsInsideBandv,MCPtPoinsInsideBandv,GMMPoinsInsideBandv)
rownames(M4PoinsInsideBand)<-c ("ABC ","MCMC","MCP","MCPt","GMM")
colnames(M4PoinsInsideBand)<-c ("1 ","2","3","4","5","6 ","7","8","9","10","11","12")
rownames(M4PoinsInsideBandV)<-c ("LRP","MCP","MCPt","GMCP")
# rownames(M4PoinsInsideBandV)<-c ("ABC ","MCMC","MCP","MCPt","GMM")
colnames(M4PoinsInsideBandV)<-c ("1 ","2","3","4","5","6 ","7","8","9","10","11","12")

#grafica en matrix validación
col.l <- colorRampPalette(c('red', 'blue'))(30)
col.2 <- colorRampPalette(c('red', 'blue'))(100)
col.3 <- colorRampPalette(c('blue', 'red'))(5)
#print(levelplot(t(M2dfactorv [c(nrow(M2dfactorv):1) ,]),xlab="Basin",ylab="post-processor",main="D-factor",cex.axis=24,cex.lab = 24,cex.main=24,col.regions=colorRampPalette(brewer.pal(9, 'GnBu')),at=seq(0, 2, length=30), aspect='iso',scales=list(alternating=FALSE, tck=1:0)),split=c(1, 2, 2, 2), newpage=FALSE)
#Fig 5 HeatMap Val ----------------------------------------------------
setwd("F:/TesinaMasterCoDireccion/PaperMOPEX")
jpeg("./Fig5HeatMapval.jpg", res = 600, width = 10, height = 6, units = "in")
#plot.new()
par(mfrow=c(2,2), oma=c(2,2,2,2),mar=c(6,4,6,2))
print(levelplot(t(M1Nv [c(nrow(M1Nv):1) ,]),cex.axis = 2, cex.lab = 2,xlab="Catchment",ylab="Post-processor",main="Nash-Sutcliffe efficiency (NSE)",col.regions=col.l,at=seq(min(M1Nv), max(M1Nv), length=30), aspect='iso',scales=list(alternating=FALSE, tck=1:0)),split=c(1, 1, 2, 2))
print(levelplot(t(M2dfactorv [c(nrow(M2dfactorv):1) ,]),xlab="Catchment",ylab="Post-processor",main="d-factor",col.regions=col.l,at=seq(min(M2dfactorv ), max(M2dfactorv ), length=30), aspect='iso',scales=list(alternating=FALSE, tck=1:0)),split=c(1, 2, 2, 2), newpage=FALSE)
print(levelplot(t(M3PRECISIONV [c(nrow(M3PRECISIONV):1) ,]),xlab="Catchment",ylab="Post-processor",main="Precision",col.regions=col.2,at=seq(min(M3PRECISIONV), max(M3PRECISIONV), length=100), aspect='iso',scales=list(alternating=FALSE, tck=1:0)),split=c(2, 1, 2, 2), newpage=FALSE)
print(levelplot(t(M4PoinsInsideBandV [c(nrow(M4PoinsInsideBandV):1) ,]),xlab="Catchment",ylab="Post-processor",main="CR (95%)",col.regions=col.2,at=seq(min(M4PoinsInsideBandV), max(M4PoinsInsideBandV), length=100), aspect='iso',scales=list(alternating=FALSE, tck=1:0)),split=c(2, 2, 2, 2), newpage=FALSE)
#title("Centered Overall Title", outer=TRUE)
#mtext(side=1, "Centered Subtitle", outer=TRUE)
dev.off()

#Fig 5.2 HeatMap Val Ordenado ----------------------------------------------------
# water journal figure
# Ordenado por indice de aridez humedo a seco
NSEord <- M1Nv[3:5,c(2,3,5,8,1,7,9,6,4,10,11,12)]
colnames(NSEord)<-c ("2 ","3","5","8","1","7","9","6","4","10","11","12")
rownames(NSEord)<-c ("MCP","MCPt","GMCP")
NSEord2 <- abs(1 - NSEord)
#sharpness
p <- M3PRECISIONV[3:5,c(2,3,5,8,1,7,9,6,4,10,11,12)]
rownames(p)<-c ("MCP","MCPt","GMCP")
CR95 <- M4PoinsInsideBandV[3:5,c(2,3,5,8,1,7,9,6,4,10,11,12)]
CR95_2 <- abs(95 - CR95)
rownames(CR95_2)<-c ("MCP","MCPt","GMCP")
# heatmap
setwd("G:\\Mi unidad\\1Doctorado\\TesinaMasterCoDireccion\\PaperMOPEX\\Water\\Figures")
jpeg("./Fig8HeatMapvalORDinv.jpg", res = 600, width = 10, height = 6, units = "in")
#plot.new()
#grafica en matrix validación
col.l <- colorRampPalette(c('red', 'blue'))(30)
col.2 <- colorRampPalette(c('red', 'blue'))
col.1 <- colorRampPalette(c('blue', 'red'))
#par(mar=c(1,1,1,1))
#par(mfrow=c(3,1))
par(mfrow=c(3,1), oma=c(2,2,2,2),mar=c(6,4,6,2))
#layout(matrix(c(1,2,3), 1, 3, byrow = TRUE))
#par(mar = c(4.8,5.8,0.5,0.5))
#layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE))
print(levelplot(t(NSEord2 [c(nrow(NSEord2):1) ,]),cex.axis = 2, cex.lab = 2,xlab="",ylab="Post-processor",main=expression(abs(1-NSE)),col.regions=col.1,at=seq(min(NSEord2), max(NSEord2), length=100),aspect='iso',scales=list(alternating=FALSE, tck=1:0)),split=c(1, 1, 1, 3))#,split=c(1, 1, 2, 2), aspect='iso',scales=list(alternating=FALSE,tck=1:0)
#print(levelplot(t(dford2 [c(nrow(dford2):1) ,]),xlab="Catchment",ylab="Post-processor",main= expression(abs(1-df)),col.regions=col.1,at=seq(min(dford2 ), max(dford2 ), length=30), aspect='iso',scales=list(alternating=FALSE, tck=1:0)),split=c(1, 2, 2, 2), newpage=FALSE)
print(levelplot(t(p [c(nrow(p):1) ,]),cex.axis = 2, cex.lab = 2,xlab="",ylab="Post-processor",main="Sharpness",col.regions=col.2,at=seq(min(p), max(p), length=100), aspect='iso',scales=list(alternating=FALSE, tck=1:0)), split=c(1, 2, 1, 3),newpage=FALSE) #split=c(2, 1, 2, 2),
print(levelplot(t(CR95_2 [c(nrow(CR95_2):1) ,]),cex.axis = 2, cex.lab = 2,xlab="Catchment",ylab="Post-processor",main=expression(abs(95-CR95)),col.regions=col.1,at=seq(min(CR95_2), max(CR95_2), length=100), aspect='iso',scales=list(alternating=FALSE, tck=1:0)),split=c(1, 3, 1, 3), newpage=FALSE)
#title("Centered Overall Title", outer=TRUE)
#mtext(side=1, "Centered Subtitle", outer=TRUE)
dev.off()

# Ordenado por indice de aridez humedo a seco
# NSEord <- M1Nv[1:4,c(2,3,5,8,1,7,9,6,4,10,11,12)]
# colnames(NSEord)<-c ("2 ","3","5","8","1","7","9","6","4","10","11","12")
# rownames(NSEord)<-c ("LRP","MCP","MCPt","GMCP")
# NSEord2 <- abs(1 - NSEord)
# dford <- M2dfactorv[2:5,c(2,3,5,8,1,7,9,6,4,10,11,12)]
# #colnames(dford)<-c ("2 ","3","5","8","1","7","9","6","4","10","11","12")
# #rownames(dford)<-c ("MCMC","MCP","MCPt","GMM")
# # convertir d-factor en abs(1-dfactor)
# dford2 <- abs(1 - dford)
# rownames(dford2)<-c ("LRP","MCP","MCPt","GMCP")
# p <- M3PRECISIONV[2:5,c(2,3,5,8,1,7,9,6,4,10,11,12)]
# rownames(p)<-c ("LRP","MCP","MCPt","GMCP")
# CR95 <- M4PoinsInsideBandV[2:5,c(2,3,5,8,1,7,9,6,4,10,11,12)]
# CR95_2 <- abs(95 - CR95)
# rownames(CR95_2)<-c ("LRP","MCP","MCPt","GMCP")
# 
# setwd("E:/Doctorado/TesinaMasterCoDireccion/PaperMOPEX/Figuras")
# jpeg("./Fig8HeatMapvalORDinv.jpg", res = 600, width = 10, height = 6, units = "in")
# 
# #grafica en matrix validación
# col.l <- colorRampPalette(c('red', 'blue'))(30)
# col.2 <- colorRampPalette(c('red', 'blue'))
# col.1 <- colorRampPalette(c('blue', 'red'))
# par(mfrow=c(2,2), oma=c(2,2,2,2),mar=c(6,4,6,2))
# print(levelplot(t(NSEord2 [c(nrow(NSEord2):1) ,]),cex.axis = 2, cex.lab = 2,xlab="Catchment",ylab="Post-processor",main=expression(abs(1-NSE)),col.regions=col.1,at=seq(min(NSEord2), max(NSEord2), length=30), aspect='iso',scales=list(alternating=FALSE, tck=1:0)),split=c(1, 1, 2, 2))
# print(levelplot(t(dford2 [c(nrow(dford2):1) ,]),xlab="Catchment",ylab="Post-processor",main= expression(abs(1-df)),col.regions=col.1,at=seq(min(dford2 ), max(dford2 ), length=30), aspect='iso',scales=list(alternating=FALSE, tck=1:0)),split=c(1, 2, 2, 2), newpage=FALSE)
# print(levelplot(t(p [c(nrow(p):1) ,]),xlab="Catchment",ylab="Post-processor",main="Sharpness",col.regions=col.2,at=seq(min(p), max(p), length=100), aspect='iso',scales=list(alternating=FALSE, tck=1:0)),split=c(2, 1, 2, 2), newpage=FALSE)
# print(levelplot(t(CR95_2 [c(nrow(CR95_2):1) ,]),xlab="Catchment",ylab="Post-processor",main=expression(abs(95-CR95)),col.regions=col.1,at=seq(min(CR95_2), max(CR95_2), length=100), aspect='iso',scales=list(alternating=FALSE, tck=1:0)),split=c(2, 2, 2, 2), newpage=FALSE)
# #title("Centered Overall Title", outer=TRUE)
# #mtext(side=1, "Centered Subtitle", outer=TRUE)
# dev.off()



# fig 6 barplot #----------------------------------------------------
# Grouped barplot
jpeg("./Fig6Barplot.jpg", res = 600, width = 10, height = 6, units = "in")
# jpeg("./Fig31Barplot2.jpg", res = 600, width = 20, height = 12.36068, units = "in")
layout(matrix(c(1,2,3), 3, 1, byrow = TRUE))
par(mar = c(4.8,6,0.5,0.5))
#par(mfrow=c(2,2), oma=c(2,2,2,2),mar=c(6,4,6,2))
barplot(NSEord, ylim = c(0,1), col = c("green3","red","blue") , border="white", font.axis=2, beside=T, ylab = "NSE", xlab="Catchment", font.lab=2)
# barplot(M1Nv, ylim = c(0,1), col = c("magenta","cyan","green3","red","blue") , border="white", font.axis=2, beside=T, ylab = "NSE", xlab="Catchment", font.lab=2)
box()
barplot(p, ylim = c(0,10),col = c("green3","red","blue") , border="white", font.axis=2, beside=T, ylab = "Sharpness" ,legend=rownames(p), xlab="Catchment", font.lab=2)
# barplot(M3PRECISIONV, ylim = c(0,14),col = c("magenta","cyan","green3","red","blue") , border="white", font.axis=2, beside=T, ylab = "Precision" ,legend=rownames(M3PRECISIONV), xlab="Catchment", font.lab=2)
box()
#barplot(M2dfactorv, col = c("cyan","green3","red","blue") , border="white", font.axis=2, beside=T, ylab = "d-factor", xlab="Catchment", font.lab=2)
# barplot(M2dfactorv, col = c("magenta","cyan","green3","red","blue") , border="white", font.axis=2, beside=T, ylab = "d-factor", xlab="Catchment", font.lab=2)
#box()
barplot(CR95, ylim = c(0,100),col = c("green3","red","blue") , border="white", font.axis=2, beside=T, ylab = "CR(95%)", xlab="Catchment", font.lab=2)
# barplot(M4PoinsInsideBandV, ylim = c(0,100),col = c("magenta","cyan","green3","red","blue") , border="white", font.axis=2, beside=T, ylab = "CR(95%)", xlab="Catchment", font.lab=2)
box()
dev.off()

# fig 4  Seasonal mean streamflows for validation period

# calculate statistics for the streamflows seasonal mean
B1qovalm <- aggregate(B1qoval, format(time(B1qoval), "%m"), mean)
B1qsvalm  <- aggregate(B1qsval, format(time(B1qsval), "%m"), mean)
B2qovalm <- aggregate(B2qoval, format(time(B2qoval), "%m"), mean)
B2qsvalm  <- aggregate(B2qsval, format(time(B2qsval), "%m"), mean)
B3qovalm <- aggregate(B3qoval, format(time(B3qoval), "%m"), mean)
B3qsvalm  <- aggregate(B3qsval, format(time(B3qsval), "%m"), mean)
B4qovalm <- aggregate(B4qoval, format(time(B4qoval), "%m"), mean)
B4qsvalm  <- aggregate(B4qsval, format(time(B4qsval), "%m"), mean)
B5qovalm <- aggregate(B5qoval, format(time(B5qoval), "%m"), mean)
B5qsvalm  <- aggregate(B5qsval, format(time(B5qsval), "%m"), mean)
B6qovalm <- aggregate(B6qoval, format(time(B6qoval), "%m"), mean)
B6qsvalm  <- aggregate(B6qsval, format(time(B6qsval), "%m"), mean)
B7qovalm <- aggregate(B7qoval, format(time(B7qoval), "%m"), mean)
B7qsvalm  <- aggregate(B7qsval, format(time(B7qsval), "%m"), mean)
B8qovalm <- aggregate(B8qoval, format(time(B8qoval), "%m"), mean)
B8qsvalm  <- aggregate(B8qsval, format(time(B8qsval), "%m"), mean)
B9qovalm <- aggregate(B9qoval, format(time(B9qoval), "%m"), mean)
B9qsvalm  <- aggregate(B9qsval, format(time(B9qsval), "%m"), mean)
B10qovalm <- aggregate(B10qoval, format(time(B10qoval), "%m"), mean)
B10qsvalm  <- aggregate(B10qsval, format(time(B10qsval), "%m"), mean)
B11qovalm <- aggregate(B11qoval, format(time(B11qoval), "%m"), mean)
B11qsvalm  <- aggregate(B11qsval, format(time(B11qsval), "%m"), mean)
B12qovalm <- aggregate(B12qoval, format(time(B12qoval), "%m"), mean)
B12qsvalm  <- aggregate(B12qsval, format(time(B12qsval), "%m"), mean)

# convert posterior to statistic mean seasonal
# convert to time series object for posterior
# validation period
B1GMMval <- zoo(B1GMM$EstPosteriorv[,"median"], order.by = valdata$Date)
B1MCPtval <- zoo(B1MCPt$Est.Posteriorv[,"median"], order.by = valdata$Date)
B1MCPval <- zoo(B1MCP$Est.Posteriorv[,"median"], order.by = valdata$Date)
B1ABCval <- zoo(B1ABC$Est.Posteriorv[,"median"], order.by = valdata$Date)
B1MCMCval <- zoo(B1MCMC$Est.Posteriorv[,"median"], order.by = valdata$Date)

B2GMMval <- zoo(B2GMM$EstPosteriorv[,"median"], order.by = valdata$Date)
B2MCPtval <- zoo(B2MCPt$Est.Posteriorv[,"median"], order.by = valdata$Date)
B2MCPval <- zoo(B2MCP$Est.Posteriorv[,"median"], order.by = valdata$Date)
B2ABCval <- zoo(B2ABC$Est.Posteriorv[,"median"], order.by = valdata$Date)
B2MCMCval <- zoo(B2MCMC$Est.Posteriorv[,"median"], order.by = valdata$Date)

B3GMMval <- zoo(B3GMM$EstPosteriorv[,"median"], order.by = valdata$Date)
B3MCPtval <- zoo(B3MCPt$Est.Posteriorv[,"median"], order.by = valdata$Date)
B3MCPval <- zoo(B3MCP$Est.Posteriorv[,"median"], order.by = valdata$Date)
B3ABCval <- zoo(B3ABC$Est.Posteriorv[,"median"], order.by = valdata$Date)
B3MCMCval <- zoo(B3MCMC$Est.Posteriorv[,"median"], order.by = valdata$Date)

B4GMMval <- zoo(B4GMM$EstPosteriorv[,"median"], order.by = valdata$Date)
B4MCPtval <- zoo(B4MCPt$Est.Posteriorv[,"median"], order.by = valdata$Date)
B4MCPval <- zoo(B4MCP$Est.Posteriorv[,"median"], order.by = valdata$Date)
B4ABCval <- zoo(B4ABC$Est.Posteriorv[,"median"], order.by = valdata$Date)
B4MCMCval <- zoo(B4MCMC$Est.Posteriorv[,"median"], order.by = valdata$Date)

B5GMMval <- zoo(B5GMM$EstPosteriorv[,"median"], order.by = valdata$Date)
B5MCPtval <- zoo(B5MCPt$Est.Posteriorv[,"median"], order.by = valdata$Date)
B5MCPval <- zoo(B5MCP$Est.Posteriorv[,"median"], order.by = valdata$Date)
B5ABCval <- zoo(B5ABC$Est.Posteriorv[,"median"], order.by = valdata$Date)
B5MCMCval <- zoo(B5MCMC$Est.Posteriorv[,"median"], order.by = valdata$Date)

B6GMMval <- zoo(B6GMM$EstPosteriorv[,"median"], order.by = valdata$Date)
B6MCPtval <- zoo(B6MCPt$Est.Posteriorv[,"median"], order.by = valdata$Date)
B6MCPval <- zoo(B6MCP$Est.Posteriorv[,"median"], order.by = valdata$Date)
B6ABCval <- zoo(B6ABC$Est.Posteriorv[,"median"], order.by = valdata$Date)
B6MCMCval <- zoo(B6MCMC$Est.Posteriorv[,"median"], order.by = valdata$Date)

B7GMMval <- zoo(B7GMM$EstPosteriorv[,"median"], order.by = valdata$Date)
B7MCPtval <- zoo(B7MCPt$Est.Posteriorv[,"median"], order.by = valdata$Date)
B7MCPval <- zoo(B7MCP$Est.Posteriorv[,"median"], order.by = valdata$Date)
B7ABCval <- zoo(B7ABC$Est.Posteriorv[,"median"], order.by = valdata$Date)
B7MCMCval <- zoo(B7MCMC$Est.Posteriorv[,"median"], order.by = valdata$Date)

B8GMMval <- zoo(B8GMM$EstPosteriorv[,"median"], order.by = valdata$Date)
B8MCPtval <- zoo(B8MCPt$Est.Posteriorv[,"median"], order.by = valdata$Date)
B8MCPval <- zoo(B8MCP$Est.Posteriorv[,"median"], order.by = valdata$Date)
B8ABCval <- zoo(B8ABC$Est.Posteriorv[,"median"], order.by = valdata$Date)
B8MCMCval <- zoo(B8MCMC$Est.Posteriorv[,"median"], order.by = valdata$Date)

B9GMMval <- zoo(B9GMM$EstPosteriorv[,"median"], order.by = valdata$Date)
B9MCPtval <- zoo(B9MCPt$Est.Posteriorv[,"median"], order.by = valdata$Date)
B9MCPval <- zoo(B9MCP$Est.Posteriorv[,"median"], order.by = valdata$Date)
B9ABCval <- zoo(B9ABC$Est.Posteriorv[,"median"], order.by = valdata$Date)
B9MCMCval <- zoo(B9MCMC$Est.Posteriorv[,"median"], order.by = valdata$Date)

B10GMMval <- zoo(B10GMM$EstPosteriorv[,"median"], order.by = valdata$Date)
B10MCPtval <- zoo(B10MCPt$Est.Posteriorv[,"median"], order.by = valdata$Date)
B10MCPval <- zoo(B10MCP$Est.Posteriorv[,"median"], order.by = valdata$Date)
B10ABCval <- zoo(B10ABC$Est.Posteriorv[,"median"], order.by = valdata$Date)
B10MCMCval <- zoo(B10MCMC$Est.Posteriorv[,"median"], order.by = valdata$Date)

B11GMMval <- zoo(B11GMM$EstPosteriorv[,"median"], order.by = valdata$Date)
B11MCPtval <- zoo(B11MCPt$Est.Posteriorv[,"median"], order.by = valdata$Date)
B11MCPval <- zoo(B11MCP$Est.Posteriorv[,"median"], order.by = valdata$Date)
B11ABCval <- zoo(B11ABC$Est.Posteriorv[,"median"], order.by = valdata$Date)
B11MCMCval <- zoo(B11MCMC$Est.Posteriorv[,"median"], order.by = valdata$Date)

B12GMMval <- zoo(B12GMM$EstPosteriorv[,"median"], order.by = valdata$Date)
B12MCPtval <- zoo(B12MCPt$Est.Posteriorv[,"median"], order.by = valdata$Date)
B12MCPval <- zoo(B12MCP$Est.Posteriorv[,"median"], order.by = valdata$Date)
B12ABCval <- zoo(B12ABC$Est.Posteriorv[,"median"], order.by = valdata$Date)
B12MCMCval <- zoo(B12MCMC$Est.Posteriorv[,"median"], order.by = valdata$Date)
# calculate statistics for the regimes seasonal mean
B1GMMvalm <- aggregate(B1GMMval, format(time(B1GMMval), "%m"), mean)
B1MCPtvalm <- aggregate(B1MCPtval, format(time(B1MCPtval), "%m"), mean)
B1MCPvalm <- aggregate(B1MCPval, format(time(B1MCPval), "%m"), mean)
B1ABCvalm <- aggregate(B1ABCval, format(time(B1ABCval), "%m"), mean)
B1MCMCvalm <- aggregate(B1MCMCval, format(time(B1MCMCval), "%m"), mean)

B2GMMvalm <- aggregate(B2GMMval, format(time(B2GMMval), "%m"), mean)
B2MCPtvalm <- aggregate(B2MCPtval, format(time(B2MCPtval), "%m"), mean)
B2MCPvalm <- aggregate(B2MCPval, format(time(B2MCPval), "%m"), mean)
B2ABCvalm <- aggregate(B2ABCval, format(time(B2ABCval), "%m"), mean)
B2MCMCvalm <- aggregate(B2MCMCval, format(time(B2MCMCval), "%m"), mean)

B3GMMvalm <- aggregate(B3GMMval, format(time(B3GMMval), "%m"), mean)
B3MCPtvalm <- aggregate(B3MCPtval, format(time(B3MCPtval), "%m"), mean)
B3MCPvalm <- aggregate(B3MCPval, format(time(B3MCPval), "%m"), mean)
B3ABCvalm <- aggregate(B3ABCval, format(time(B3ABCval), "%m"), mean)
B3MCMCvalm <- aggregate(B3MCMCval, format(time(B3MCMCval), "%m"), mean)

B4GMMvalm <- aggregate(B4GMMval, format(time(B4GMMval), "%m"), mean)
B4MCPtvalm <- aggregate(B4MCPtval, format(time(B4MCPtval), "%m"), mean)
B4MCPvalm <- aggregate(B4MCPval, format(time(B4MCPval), "%m"), mean)
B4ABCvalm <- aggregate(B4ABCval, format(time(B4ABCval), "%m"), mean)
B4MCMCvalm <- aggregate(B4MCMCval, format(time(B4MCMCval), "%m"), mean)

B5GMMvalm <- aggregate(B5GMMval, format(time(B5GMMval), "%m"), mean)
B5MCPtvalm <- aggregate(B5MCPtval, format(time(B5MCPtval), "%m"), mean)
B5MCPvalm <- aggregate(B5MCPval, format(time(B5MCPval), "%m"), mean)
B5ABCvalm <- aggregate(B5ABCval, format(time(B5ABCval), "%m"), mean)
B5MCMCvalm <- aggregate(B5MCMCval, format(time(B5MCMCval), "%m"), mean)

B6GMMvalm <- aggregate(B6GMMval, format(time(B6GMMval), "%m"), mean)
B6MCPtvalm <- aggregate(B6MCPtval, format(time(B6MCPtval), "%m"), mean)
B6MCPvalm <- aggregate(B6MCPval, format(time(B6MCPval), "%m"), mean)
B6ABCvalm <- aggregate(B6ABCval, format(time(B6ABCval), "%m"), mean)
B6MCMCvalm <- aggregate(B6MCMCval, format(time(B6MCMCval), "%m"), mean)

B7GMMvalm <- aggregate(B7GMMval, format(time(B7GMMval), "%m"), mean)
B7MCPtvalm <- aggregate(B7MCPtval, format(time(B7MCPtval), "%m"), mean)
B7MCPvalm <- aggregate(B7MCPval, format(time(B7MCPval), "%m"), mean)
B7ABCvalm <- aggregate(B7ABCval, format(time(B7ABCval), "%m"), mean)
B7MCMCvalm <- aggregate(B7MCMCval, format(time(B7MCMCval), "%m"), mean)

B8GMMvalm <- aggregate(B8GMMval, format(time(B8GMMval), "%m"), mean)
B8MCPtvalm <- aggregate(B8MCPtval, format(time(B8MCPtval), "%m"), mean)
B8MCPvalm <- aggregate(B8MCPval, format(time(B8MCPval), "%m"), mean)
B8ABCvalm <- aggregate(B8ABCval, format(time(B8ABCval), "%m"), mean)
B8MCMCvalm <- aggregate(B8MCMCval, format(time(B8MCMCval), "%m"), mean)

B9GMMvalm <- aggregate(B9GMMval, format(time(B9GMMval), "%m"), mean)
B9MCPtvalm <- aggregate(B9MCPtval, format(time(B9MCPtval), "%m"), mean)
B9MCPvalm <- aggregate(B9MCPval, format(time(B9MCPval), "%m"), mean)
B9ABCvalm <- aggregate(B9ABCval, format(time(B9ABCval), "%m"), mean)
B9MCMCvalm <- aggregate(B9MCMCval, format(time(B9MCMCval), "%m"), mean)

B10GMMvalm <- aggregate(B10GMMval, format(time(B10GMMval), "%m"), mean)
B10MCPtvalm <- aggregate(B10MCPtval, format(time(B10MCPtval), "%m"), mean)
B10MCPvalm <- aggregate(B10MCPval, format(time(B10MCPval), "%m"), mean)
B10ABCvalm <- aggregate(B10ABCval, format(time(B10ABCval), "%m"), mean)
B10MCMCvalm <- aggregate(B10MCMCval, format(time(B10MCMCval), "%m"), mean)

B11GMMvalm <- aggregate(B11GMMval, format(time(B11GMMval), "%m"), mean)
B11MCPtvalm <- aggregate(B11MCPtval, format(time(B11MCPtval), "%m"), mean)
B11MCPvalm <- aggregate(B11MCPval, format(time(B11MCPval), "%m"), mean)
B11ABCvalm <- aggregate(B11ABCval, format(time(B11ABCval), "%m"), mean)
B11MCMCvalm <- aggregate(B11MCMCval, format(time(B11MCMCval), "%m"), mean)

B12GMMvalm <- aggregate(B12GMMval, format(time(B12GMMval), "%m"), mean)
B12MCPtvalm <- aggregate(B12MCPtval, format(time(B12MCPtval), "%m"), mean)
B12MCPvalm <- aggregate(B12MCPval, format(time(B12MCPval), "%m"), mean)
B12ABCvalm <- aggregate(B12ABCval, format(time(B12ABCval), "%m"), mean)
B12MCMCvalm <- aggregate(B12MCMCval, format(time(B12MCMCval), "%m"), mean)


# Fig 3 Seasonal streamflow----------------------------------------------------
setwd("F:/TesinaMasterCoDireccion/PaperMOPEX")
jpeg("./Fig4SeasonalMeanValidation.jpg", res = 600, width = 20, height = 12, units = "in")
#jpeg("./Fig3SeasonalMeanValidation.jpg", res = 600, width = 10, height = 6, units = "in")
layout(matrix(c(1,2,3,4,5,6,7,8,9,10,11,12), 3, 4, byrow = TRUE))
#par(mar = c(4.8,9.5,0.5,0.5))
#par(mar = c(4.8,9.5,0.5,0.5))
par(mar = c(2,7.5,0.5,0.5))
plot(c(1,12), c(0,5), mgp=c(2,1,0),type ="n", xlab="", ylab=expression(paste("Streamflow"~ (m^3~s^-1))), xaxt="n", cex.axis = 2.5, cex.lab = 2.5, las = 1)
#axis(1, at=1:12, labels = month.abb, las = 1,cex.axis = 2)
points(1:12, as.numeric(B1qovalm), col="red", pch =20, bg="red", cex = 2.5)
lines(1:12, as.numeric(B1qsvalm), lwd = 3, col="black")
lines(1:12, as.numeric(B1GMMvalm), lwd = 3, col="blue")
lines(1:12, as.numeric(B1MCPtvalm), lwd = 3, col="brown")
lines(1:12, as.numeric(B1MCPvalm), lwd = 3, col="green3")
#lines(1:12, as.numeric(B1ABCvalm), lwd = 3, col="magenta")
#lines(1:12, as.numeric(B1MCMCvalm), lwd = 3, col="cyan")
text(6,5, labels = "B1", cex = 2.5)
legend(x =5.2 , y =5, cex = 2.3, legend = c("Observations","Uncorrected","GMCP","MCPt","MCP"), lwd = c(NA,3,3,3,3,3),col = c("red","black","blue","brown","green3","magenta"), pch = c(20,NA,NA,NA,NA,NA),bty = "n")
#legend(x =7 , y =4.8, cex = 2, legend = c("Obs","Unc","GMM","MCPt","MCP","ABC","MCMC"), lwd = c(NA,3,3,3,3,3,3),col = c("red","black","blue","brown","green3","magenta","cyan"), pch = c(20,NA,NA,NA,NA,NA,NA),bty = "n")

par(mar = c(2,4.5,0.5,0.5))
plot(c(1,12), c(0,4), mgp=c(2,1,0),type ="n", xlab="", ylab="", xaxt="n", cex.axis = 2.5, cex.lab = 1, las = 1)
#axis(1, at=1:12, labels = month.abb, las = 1,cex.axis = 2)
points(1:12, as.numeric(B2qovalm), col="red", pch =20, bg="red", cex = 2.5)
lines(1:12, as.numeric(B2qsvalm), lwd = 3, col="black")
lines(1:12, as.numeric(B2GMMvalm), lwd = 3, col="blue")
lines(1:12, as.numeric(B2MCPtvalm), lwd = 3, col="brown")
lines(1:12, as.numeric(B2MCPvalm), lwd = 3, col="green3")
#lines(1:12, as.numeric(B2ABCvalm), lwd = 3, col="magenta")
#lines(1:12, as.numeric(B2MCMCvalm), lwd = 3, col="cyan")
text(6,4, labels = "B2", cex = 2.5)

par(mar = c(2,4.5,0.5,0.5))
plot(c(1,12), c(0,5), mgp=c(2,1,0),type ="n", xlab="", ylab="", xaxt="n", cex.axis = 2.5, cex.lab = 1, las = 1)
#axis(1, at=1:12, labels = month.abb, las = 1,cex.axis = 2)
points(1:12, as.numeric(B3qovalm), col="red", pch =20, bg="red", cex = 2.5)
lines(1:12, as.numeric(B3qsvalm), lwd = 3, col="black")
lines(1:12, as.numeric(B3GMMvalm), lwd = 3, col="blue")
lines(1:12, as.numeric(B3MCPtvalm), lwd = 3, col="brown")
lines(1:12, as.numeric(B3MCPvalm), lwd = 3, col="green3")
#lines(1:12, as.numeric(B3ABCvalm), lwd = 3, col="magenta")
#lines(1:12, as.numeric(B3MCMCvalm), lwd = 3, col="cyan")
text(6,5, labels = "B3", cex = 2.5)

par(mar = c(2,4.5,0.5,0.5))
plot(c(1,12), c(0,2), mgp=c(3,1,0),type ="n", xlab="", ylab="", xaxt="n", cex.axis = 2.5, cex.lab = 1, las = 1)
#axis(1, at=1:12, labels = month.abb, las = 1,cex.axis = 2)
points(1:12, as.numeric(B4qovalm), col="red", pch =20, bg="red", cex = 2.5)
lines(1:12, as.numeric(B4qsvalm), lwd = 3, col="black")
lines(1:12, as.numeric(B4GMMvalm), lwd = 3, col="blue")
lines(1:12, as.numeric(B4MCPtvalm), lwd = 3, col="brown")
lines(1:12, as.numeric(B4MCPvalm), lwd = 3, col="green3")
#lines(1:12, as.numeric(B4ABCvalm), lwd = 3, col="magenta")
#lines(1:12, as.numeric(B4MCMCvalm), lwd = 3, col="cyan")
text(6,2, labels = "B4", cex = 2.5)

#par(mar = c(2,9.5,0.5,0.5))
par(mar = c(2,7.5,0.5,0.5))
plot(c(1,12), c(0,3), mgp=c(3,0.5,0),type ="n", xlab="", ylab=expression(paste("Streamflow"~ (m^3~s^-1))), xaxt="n", cex.axis = 2.5, cex.lab = 2.5, las = 1)
#axis(1, at=1:12, labels = month.abb, las = 1,cex.axis = 2)
points(1:12, as.numeric(B5qovalm), col="red", pch =20, bg="red", cex = 2.5)
lines(1:12, as.numeric(B5qsvalm), lwd = 3, col="black")
lines(1:12, as.numeric(B5GMMvalm), lwd = 3, col="blue")
lines(1:12, as.numeric(B5MCPtvalm), lwd = 3, col="brown")
lines(1:12, as.numeric(B5MCPvalm), lwd = 3, col="green3")
#lines(1:12, as.numeric(B5ABCvalm), lwd = 3, col="magenta")
#lines(1:12, as.numeric(B5MCMCvalm), lwd = 3, col="cyan")
text(6,3, labels = "B5", cex = 2.5)

par(mar = c(2,4.5,0.5,0.5))
plot(c(1,12), c(0,3), mgp=c(3,1,0),type ="n", xlab="", ylab="", xaxt="n", cex.axis = 2.5, cex.lab = 1, las = 1)
#axis(1, at=1:12, labels = month.abb, las = 1,cex.axis = 2)
points(1:12, as.numeric(B6qovalm), col="red", pch =20, bg="red", cex = 2.5)
lines(1:12, as.numeric(B6qsvalm), lwd = 3, col="black")
lines(1:12, as.numeric(B6GMMvalm), lwd = 3, col="blue")
lines(1:12, as.numeric(B6MCPtvalm), lwd = 3, col="brown")
lines(1:12, as.numeric(B6MCPvalm), lwd = 3, col="green3")
#lines(1:12, as.numeric(B6ABCvalm), lwd = 3, col="magenta")
#lines(1:12, as.numeric(B6MCMCvalm), lwd = 3, col="cyan")
text(6,3, labels = "B6", cex = 2.5)

par(mar = c(2,4.5,0.5,0.5))
plot(c(1,12), c(0,3), mgp=c(3,1,0),type ="n", xlab="", ylab= "", xaxt="n", cex.axis = 2.5, cex.lab = 1, las = 1)
#axis(1, at=1:12, labels = month.abb, las = 1,cex.axis = 2)
points(1:12, as.numeric(B7qovalm), col="red", pch =20, bg="red", cex = 2.5)
lines(1:12, as.numeric(B7qsvalm), lwd = 3, col="black")
lines(1:12, as.numeric(B7GMMvalm), lwd = 3, col="blue")
lines(1:12, as.numeric(B7MCPtvalm), lwd = 3, col="brown")
lines(1:12, as.numeric(B7MCPvalm), lwd = 3, col="green3")
#lines(1:12, as.numeric(B7ABCvalm), lwd = 3, col="magenta")
#lines(1:12, as.numeric(B7MCMCvalm), lwd = 3, col="cyan")
text(6,3, labels = "B7", cex = 2.5)

par(mar = c(2,4.5,0.5,0.5))
plot(c(1,12), c(0,3), mgp=c(3,1,0),type ="n", xlab="", ylab= "", xaxt="n", cex.axis = 2.5, cex.lab = 1, las = 1)
#axis(1, at=1:12, labels = month.abb, las = 1,cex.axis = 2)
points(1:12, as.numeric(B8qovalm), col="red", pch =20, bg="red", cex = 2.5)
lines(1:12, as.numeric(B8qsvalm), lwd = 3, col="black")
lines(1:12, as.numeric(B8GMMvalm), lwd = 3, col="blue")
lines(1:12, as.numeric(B8MCPtvalm), lwd = 3, col="brown")
lines(1:12, as.numeric(B8MCPvalm), lwd = 3, col="green3")
#lines(1:12, as.numeric(B8ABCvalm), lwd = 3, col="magenta")
#lines(1:12, as.numeric(B8MCMCvalm), lwd = 3, col="cyan")
text(6,3, labels = "B8", cex = 2.5)

par(mar = c(4.8,7.5,0.5,0.5))
plot(c(1,12), c(0,3), mgp=c(3,0.5,0),type ="n", xlab="", ylab=expression(paste("Streamflow"~ (m^3~s^-1))), xaxt="n", cex.axis = 2.5, cex.lab = 2.5, las = 1)
axis(1, at=1:12, labels = month.abb, las = 1,cex.axis = 2.5)
points(1:12, as.numeric(B9qovalm), col="red", pch =20, bg="red", cex = 2.5)
lines(1:12, as.numeric(B9qsvalm), lwd = 3, col="black")
lines(1:12, as.numeric(B9GMMvalm), lwd = 3, col="blue")
lines(1:12, as.numeric(B9MCPtvalm), lwd = 3, col="brown")
lines(1:12, as.numeric(B9MCPvalm), lwd = 3, col="green3")
#lines(1:12, as.numeric(B9ABCvalm), lwd = 3, col="magenta")
#lines(1:12, as.numeric(B9MCMCvalm), lwd = 3, col="cyan")
text(6,3, labels = "B9", cex = 2.5)

par(mar = c(4.8,4.5,0.5,0.5))
plot(c(1,12), c(0,2), mgp=c(3,1,0),type ="n", xlab="", ylab= "", xaxt="n", cex.axis = 2.5, cex.lab = 2.5, las = 1)
axis(1, at=1:12, labels = month.abb, las = 1,cex.axis = 2.5)
points(1:12, as.numeric(B10qovalm), col="red", pch =20, bg="red", cex = 2.5)
lines(1:12, as.numeric(B10qsvalm), lwd = 3, col="black")
lines(1:12, as.numeric(B10GMMvalm), lwd = 3, col="blue")
lines(1:12, as.numeric(B10MCPtvalm), lwd = 3, col="brown")
lines(1:12, as.numeric(B10MCPvalm), lwd = 3, col="green3")
#lines(1:12, as.numeric(B10ABCvalm), lwd = 3, col="magenta")
#lines(1:12, as.numeric(B10MCMCvalm), lwd = 3, col="cyan")
text(6,2, labels = "B10", cex = 2.5)

par(mar = c(4.8,4.5,0.5,0.5))
plot(c(1,12), c(0,2), mgp=c(3,1,0),type ="n", xlab="", ylab= "", xaxt="n", cex.axis = 2.5, cex.lab = 2.5, las = 1)
axis(1, at=1:12, labels = month.abb, las = 1,cex.axis = 2.5)
points(1:12, as.numeric(B11qovalm), col="red", pch =20, bg="red", cex = 2.5)
lines(1:12, as.numeric(B11qsvalm), lwd = 3, col="black")
lines(1:12, as.numeric(B11GMMvalm), lwd = 3, col="blue")
lines(1:12, as.numeric(B11MCPtvalm), lwd = 3, col="brown")
lines(1:12, as.numeric(B11MCPvalm), lwd = 3, col="green3")
#lines(1:12, as.numeric(B11ABCvalm), lwd = 3, col="magenta")
#lines(1:12, as.numeric(B11MCMCvalm), lwd = 3, col="cyan")
text(6,2, labels = "B11", cex = 2.5)

par(mar = c(4.8,4.5,0.5,0.5))
plot(c(1,12), c(0,1), mgp=c(3,1,0),type ="n", xlab="", ylab= "", xaxt="n", cex.axis = 2.5, cex.lab = 2.5, las = 1)
axis(1, at=1:12, labels = month.abb, las = 1,cex.axis = 2.5)
points(1:12, as.numeric(B12qovalm), col="red", pch =20, bg="red", cex = 2.5)
lines(1:12, as.numeric(B12qsvalm), lwd = 3, col="black")
lines(1:12, as.numeric(B12GMMvalm), lwd = 3, col="blue")
lines(1:12, as.numeric(B12MCPtvalm), lwd = 3, col="brown")
lines(1:12, as.numeric(B12MCPvalm), lwd = 3, col="green3")
#lines(1:12, as.numeric(B12ABCvalm), lwd = 3, col="magenta")
#lines(1:12, as.numeric(B12MCMCvalm), lwd = 3, col="cyan")
text(6,1, labels = "B12", cex = 2.5)
dev.off()


# fig 7 predictive qqplot for validation period #----------------------------------------------------

setwd("F:/TesinaMasterCoDireccion/PaperMOPEX")
jpeg("./Fig6qqplotAllValidation.jpg", res = 600, width = 20, height = 12, units = "in")
layout(matrix(c(1,2,3,4,5,6,7,8,9,10,11,12), 3, 4, byrow = TRUE))
par(mar = c(4.8,5.8,0.5,0.5))

qqplot(B1GMM$QQplotv$x, B1GMM$QQplotv$zi, cex.axis = 2, cex.lab = 2.5,xlim = c(0,1),ylim = c(0,1),xlab = "",ylab="Quantile of forecast", col = "blue",type = "l", pch=20, lty=1, lwd = 3)
grid(nx=20)
abline(0,1,lwd=2)
text(0.5,0.9, labels = "B1", cex = 2)
lines(sort(B1MCPt$QQplotv$x), sort(B1MCPt$QQplotv$zi), col = "red", lwd = 3,pch = 20)
lines(sort(B1MCP$QQplotv$x), sort(B1MCP$QQplotv$zi), col = "green3", lwd = 3,pch = 20)
# lines(sort(B1ABC$QQplotv$x), sort(B1ABC$QQplotv$zi), col = "magenta", lwd = 3,pch = 20)
#lines(sort(B1MCMC$QQplotv$x), sort(B1MCMC$QQplotv$zi), col = "magenta", lwd = 3,pch = 20)
legend(x=0.6, y=0.5, cex = 2, legend = c("GMCP","MCPt","MCP"), lwd = c(3,3,3,3),col = c("blue","red","green3"), bty = "n")
# legend(x=0.6, y=0.5, cex = 2, legend = c("GMM","MCPt","MCP","ABC","MCMC"), lwd = c(3,3,3,3,3),col = c("blue","red","green3","magenta","magenta"), bty = "n")

qqplot(B2GMM$QQplotv$x, B2GMM$QQplotv$zi, cex.axis = 2, cex.lab = 2.5,xlim = c(0,1),ylim = c(0,1),xlab = "",ylab="Quantile of forecast", col = "blue",type = "l", pch=20, lty=1, lwd = 3)
grid(nx=20)
abline(0,1,lwd=2)
text(0.5,0.9, labels = "B2", cex = 2)
lines(sort(B2MCPt$QQplotv$x), sort(B2MCPt$QQplotv$zi), col = "red", lwd = 3,pch = 20)
lines(sort(B2MCP$QQplotv$x), sort(B2MCP$QQplotv$zi), col = "green3", lwd = 3,pch = 20)
# lines(sort(B2ABC$QQplotv$x), sort(B2ABC$QQplotv$zi), col = "magenta", lwd = 3,pch = 20)
#lines(sort(B2MCMC$QQplotv$x), sort(B2MCMC$QQplotv$zi), col = "magenta", lwd = 3,pch = 20)

qqplot(B3GMM$QQplotv$x, B3GMM$QQplotv$zi, cex.axis = 2, cex.lab = 2.5,xlim = c(0,1),ylim = c(0,1),xlab = "",ylab="Quantile of forecast", col = "blue",type = "l", pch=20, lty=1, lwd = 3)
grid(nx=20)
abline(0,1,lwd=2)
text(0.5,0.9, labels = "B3", cex = 2)
lines(sort(B3MCPt$QQplotv$x), sort(B3MCPt$QQplotv$zi), col = "red", lwd = 3,pch = 20)
lines(sort(B3MCP$QQplotv$x), sort(B3MCP$QQplotv$zi), col = "green3", lwd = 3,pch = 20)
# lines(sort(B3ABC$QQplotv$x), sort(B3ABC$QQplotv$zi), col = "magenta", lwd = 3,pch = 20)
#lines(sort(B3MCMC$QQplotv$x), sort(B3MCMC$QQplotv$zi), col = "magenta", lwd = 3,pch = 20)

qqplot(B4GMM$QQplotv$x, B4GMM$QQplotv$zi, cex.axis = 2, cex.lab = 2.5,xlim = c(0,1),ylim = c(0,1),xlab = "",ylab="Quantile of forecast", col = "blue",type = "l", pch=20, lty=1, lwd = 3)
grid(nx=20)
abline(0,1,lwd=2)
text(0.5,0.9, labels = "B4", cex = 2)
lines(sort(B4MCPt$QQplotv$x), sort(B4MCPt$QQplotv$zi), col = "red", lwd = 3,pch = 20)
lines(sort(B4MCP$QQplotv$x), sort(B4MCP$QQplotv$zi), col = "green3", lwd = 3,pch = 20)
# lines(sort(B4ABC$QQplotv$x), sort(B4ABC$QQplotv$zi), col = "magenta", lwd = 3,pch = 20)
#lines(sort(B4MCMC$QQplotv$x), sort(B4MCMC$QQplotv$zi), col = "magenta", lwd = 3,pch = 20)

qqplot(B5GMM$QQplotv$x, B5GMM$QQplotv$zi, cex.axis = 2, cex.lab = 2.5,xlim = c(0,1),ylim = c(0,1),xlab = "",ylab="Quantile of forecast", col = "blue",type = "l", pch=20, lty=1, lwd = 3)
grid(nx=20)
abline(0,1,lwd=2)
text(0.5,0.9, labels = "B5", cex = 2)
lines(sort(B5MCPt$QQplotv$x), sort(B5MCPt$QQplotv$zi), col = "red", lwd = 3,pch = 20)
lines(sort(B5MCP$QQplotv$x), sort(B5MCP$QQplotv$zi), col = "green3", lwd = 3,pch = 20)
# lines(sort(B5ABC$QQplotv$x), sort(B5ABC$QQplotv$zi), col = "magenta", lwd = 3,pch = 20)
#lines(sort(B5MCMC$QQplotv$x), sort(B5MCMC$QQplotv$zi), col = "magenta", lwd = 3,pch = 20)

qqplot(B6GMM$QQplotv$x, B6GMM$QQplotv$zi, cex.axis = 2, cex.lab = 2.5,xlim = c(0,1),ylim = c(0,1),xlab = "",ylab="Quantile of forecast", col = "blue",type = "l", pch=20, lty=1, lwd = 3)
grid(nx=20)
abline(0,1,lwd=2)
text(0.5,0.9, labels = "B6", cex = 2)
lines(sort(B6MCPt$QQplotv$x), sort(B6MCPt$QQplotv$zi), col = "red", lwd = 3,pch = 20)
lines(sort(B6MCP$QQplotv$x), sort(B6MCP$QQplotv$zi), col = "green3", lwd = 3,pch = 20)
# lines(sort(B6ABC$QQplotv$x), sort(B6ABC$QQplotv$zi), col = "magenta", lwd = 3,pch = 20)
#lines(sort(B6MCMC$QQplotv$x), sort(B6MCMC$QQplotv$zi), col = "magenta", lwd = 3,pch = 20)

qqplot(B7GMM$QQplotv$x, B7GMM$QQplotv$zi, cex.axis = 2, cex.lab = 2.5,xlim = c(0,1),ylim = c(0,1),xlab = "",ylab="Quantile of forecast", col = "blue",type = "l", pch=20, lty=1, lwd = 3)
grid(nx=20)
abline(0,1,lwd=2)
text(0.5,0.9, labels = "B7", cex = 2)
lines(sort(B7MCPt$QQplotv$x), sort(B7MCPt$QQplotv$zi), col = "red", lwd = 3,pch = 20)
lines(sort(B7MCP$QQplotv$x), sort(B7MCP$QQplotv$zi), col = "green3", lwd = 3,pch = 20)
# lines(sort(B7ABC$QQplotv$x), sort(B7ABC$QQplotv$zi), col = "magenta", lwd = 3,pch = 20)
#lines(sort(B7MCMC$QQplotv$x), sort(B7MCMC$QQplotv$zi), col = "magenta", lwd = 3,pch = 20)

qqplot(B8GMM$QQplotv$x, B8GMM$QQplotv$zi, cex.axis = 2, cex.lab = 2.5,xlim = c(0,1),ylim = c(0,1),xlab = "",ylab="Quantile of forecast", col = "blue",type = "l", pch=20, lty=1, lwd = 3)
grid(nx=20)
abline(0,1,lwd=2)
text(0.5,0.9, labels = "B8", cex = 2)
lines(sort(B8MCPt$QQplotv$x), sort(B8MCPt$QQplotv$zi), col = "red", lwd = 3,pch = 20)
lines(sort(B8MCP$QQplotv$x), sort(B8MCP$QQplotv$zi), col = "green3", lwd = 3,pch = 20)
# lines(sort(B8ABC$QQplotv$x), sort(B8ABC$QQplotv$zi), col = "magenta", lwd = 3,pch = 20)
#lines(sort(B8MCMC$QQplotv$x), sort(B8MCMC$QQplotv$zi), col = "magenta", lwd = 3,pch = 20)

qqplot(B9GMM$QQplotv$x, B9GMM$QQplotv$zi, cex.axis = 2, cex.lab = 2.5,xlim = c(0,1),ylim = c(0,1),xlab = "Theoretical Quantile U[0,1]",ylab="Quantile of forecast", col = "blue",type = "l", pch=20, lty=1, lwd = 3)
grid(nx=20)
abline(0,1,lwd=2)
text(0.5,0.9, labels = "B9", cex = 2)
lines(sort(B9MCPt$QQplotv$x), sort(B9MCPt$QQplotv$zi), col = "red", lwd = 3,pch = 20)
lines(sort(B9MCP$QQplotv$x), sort(B9MCP$QQplotv$zi), col = "green3", lwd = 3,pch = 20)
# lines(sort(B9ABC$QQplotv$x), sort(B9ABC$QQplotv$zi), col = "magenta", lwd = 3,pch = 20)
#lines(sort(B9MCMC$QQplotv$x), sort(B9MCMC$QQplotv$zi), col = "magenta", lwd = 3,pch = 20)

qqplot(B10GMM$QQplotv$x, B10GMM$QQplotv$zi, cex.axis = 2, cex.lab = 2.5,xlim = c(0,1),ylim = c(0,1),xlab = "Theoretical Quantile U[0,1]",ylab="Quantile of forecast", col = "blue",type = "l", pch=20, lty=1, lwd = 3)
grid(nx=20)
abline(0,1,lwd=2)
text(0.5,0.9, labels = "B10", cex = 2)
lines(sort(B10MCPt$QQplotv$x), sort(B10MCPt$QQplotv$zi), col = "red", lwd = 3,pch = 20)
lines(sort(B10MCP$QQplotv$x), sort(B10MCP$QQplotv$zi), col = "green3", lwd = 3,pch = 20)
# lines(sort(B10ABC$QQplotv$x), sort(B10ABC$QQplotv$zi), col = "magenta", lwd = 3,pch = 20)
#lines(sort(B10MCMC$QQplotv$x), sort(B10MCMC$QQplotv$zi), col = "magenta", lwd = 3,pch = 20)

qqplot(B11GMM$QQplotv$x, B11GMM$QQplotv$zi, cex.axis = 2, cex.lab = 2.5,xlim = c(0,1),ylim = c(0,1),xlab = "Theoretical Quantile U[0,1]",ylab="Quantile of forecast", col = "blue",type = "l", pch=20, lty=1, lwd = 3)
grid(nx=20)
abline(0,1,lwd=2)
text(0.5,0.9, labels = "B11", cex = 2)
lines(sort(B11MCPt$QQplotv$x), sort(B11MCPt$QQplotv$zi), col = "red", lwd = 3,pch = 20)
lines(sort(B11MCP$QQplotv$x), sort(B11MCP$QQplotv$zi), col = "green3", lwd = 3,pch = 20)
# lines(sort(B11ABC$QQplotv$x), sort(B11ABC$QQplotv$zi), col = "magenta", lwd = 3,pch = 20)
#lines(sort(B11MCMC$QQplotv$x), sort(B11MCMC$QQplotv$zi), col = "magenta", lwd = 3,pch = 20)

qqplot(B12GMM$QQplotv$x, B12GMM$QQplotv$zi, cex.axis = 2, cex.lab = 2.5,xlim = c(0,1),ylim = c(0,1),xlab = "Theoretical Quantile U[0,1]",ylab="Quantile of forecast", col = "blue",type = "l", pch=20, lty=1, lwd = 3)
grid(nx=20)
abline(0,1,lwd=2)
text(0.5,0.9, labels = "B12", cex = 2)
lines(sort(B12MCPt$QQplotv$x), sort(B12MCPt$QQplotv$zi), col = "red", lwd = 3,pch = 20)
lines(sort(B12MCP$QQplotv$x), sort(B12MCP$QQplotv$zi), col = "green3", lwd = 3,pch = 20)
# lines(sort(B12ABC$QQplotv$x), sort(B12ABC$QQplotv$zi), col = "magenta", lwd = 3,pch = 20)
#lines(sort(B12MCMC$QQplotv$x), sort(B12MCMC$QQplotv$zi), col = "magenta", lwd = 3,pch = 20)
dev.off()

# fig 9 Uncertainty band all post-processors one catchment (B2) #----------------------------------------------------
setwd("F:/TesinaMasterCoDireccion/PaperMOPEX")
jpeg("./Fig9UncertaintyB2Validation.jpg", res = 600, width = 20, height = 16, units = "in")
layout(matrix(c(1,2,3,4), 4, 1, byrow = TRUE))
par(mar = c(4.8,6.5,0,0.5))
plot(time(B2qoval), as.numeric(B2qoval), ylim = c(0,8), type = "n", cex.lab = 3, cex.axis = 3,main = "", xlab = "", ylab = "",  las = 1 )#ylab=expression(paste("Streamflow"~ (m^3~s^-1))),
polygon(c(time(B2qoval), rev(time(B2qoval))), c(B2GMM$EstPosteriorv[,"quan2_5"], rev(B2GMM$EstPosteriorv[,"quan97_5"])), col="magenta", border=F) # #00000044 : grey;  #0000FF44: blue
points(time(B2qoval), as.numeric(B2qoval), col="black", pch =21, bg="red", cex = 1.5)
lines(time(B2qoval), B2GMM$EstPosteriorv[,"median"], lwd = 3, col="black")
text(time(B2qoval[105]),8, labels = "GMM", cex = 3)
# lines(time(B2qoval), as.numeric(B2qsval), lwd = 3, col="black")
# legend("topleft", cex = 3, c("Observed", "Simulated", "95% uncertainty"), fill=c(NA, NA, "magenta"),border=c("white", "white", "black"), lwd = c(NA,3,NA),col = c("red","black",NA),bg=c("red","",""),pch = c(20,NA,NA),bty = "n", horiz = T, x.intersp = 0.5, xjust=1)
plot(time(B2qoval), as.numeric(B2qoval), ylim = c(0,8), type = "n", cex.lab = 3, cex.axis = 3,main = "", xlab = "", ylab = "",  las = 1 )#ylab=expression(paste("Streamflow"~ (m^3~s^-1))),
polygon(c(time(B2qoval), rev(time(B2qoval))), c(B2MCPt$Est.Posteriorv[,"quan2_5"], rev(B2MCPt$Est.Posteriorv[,"quan97_5"])), col="magenta", border=F) # #00000044 : grey;  #0000FF44: blue
points(time(B2qoval), as.numeric(B2qoval), col="black", pch =21, bg="red", cex = 1.5)
lines(time(B2qoval), B2MCPt$Est.Posteriorv[,"median"], lwd = 3, col="black")
text(time(B2qoval[105]),8, labels = "MCPt", cex = 3)
#lines(time(B1qoval), as.numeric(B2qsval), lwd = 3, col="black")
plot(time(B2qoval), as.numeric(B2qoval), ylim = c(0,8), type = "n", cex.lab = 3, cex.axis = 3,main = "", xlab = "", ylab="", las = 1 )#ylab=expression(paste("Streamflow"~ (m^3~s^-1)))
polygon(c(time(B2qoval), rev(time(B2qoval))), c(B2MCP$Est.Posteriorv[,"quan2_5"], rev(B2MCP$Est.Posteriorv[,"quan97_5"])), col="magenta", border=F) # #00000044 : grey;  #0000FF44: blue
points(time(B2qoval), as.numeric(B2qoval), col="black", pch =21, bg="red", cex = 1.5)
lines(time(B2qoval), B2MCP$Est.Posteriorv[,"median"], lwd = 3, col="black")
text(time(B2qoval[105]),8, labels = "MCP", cex = 3)
#lines(time(B1qoval), as.numeric(B2qsval), lwd = 3, col="black")
# plot(time(B2qoval), as.numeric(B2qoval), ylim = c(0,6), type = "n", cex.lab = 3, cex.axis = 3,main = "", xlab = "", ylab = "",  las = 1 )#ylab=expression(paste("Streamflow"~ (m^3~s^-1))),
# polygon(c(time(B2qoval), rev(time(B2qoval))), c(B2ABC$Est.Posteriorv[,"quan2_5"], rev(B2ABC$Est.Posteriorv[,"quan97_5"])), col="magenta", border=F) # #00000044 : grey;  #0000FF44: blue
# points(time(B2qoval), as.numeric(B2qoval), col="black", pch =21, bg="red", cex = 1.5)
# lines(time(B2qoval), B2ABC$Est.Posteriorv[,"median"], lwd = 3, col="black")
# text(time(B2qoval[105]),4, labels = "ABC", cex = 3)
#lines(time(B1qoval), as.numeric(B2qsval), lwd = 3, col="black")
plot(time(B2qoval), as.numeric(B2qoval), ylim = c(0,8), type = "n", cex.lab = 3, cex.axis = 3,main = "", xlab = "", ylab = "", las = 1 ) # ylab=expression(paste("Streamflow"~ (m^3~s^-1))),
polygon(c(time(B2qoval), rev(time(B2qoval))), c(B2MCMC$Est.Posteriorv[,"quan2_5"], rev(B2MCMC$Est.Posteriorv[,"quan97_5"])), col="magenta", border=F) # #00000044 : grey;  #0000FF44: blue
points(time(B2qoval), as.numeric(B2qoval), col="black", pch =21, bg="red", cex = 1.5)
lines(time(B2qoval), B2MCMC$Est.Posteriorv[,"median"], lwd = 3, col="black")
text(time(B2qoval[105]),8, labels = "MCMC", cex = 3)
#lines(time(B1qoval), as.numeric(B2qsval), lwd = 3, col="black")
dev.off()



# fig 8 Uncertainty band all post-processors one catchment (B12) #----------------------------------------------------
setwd("F:/TesinaMasterCoDireccion/PaperMOPEX")
jpeg("./Fig8UncertaintyB12Validation.jpg", res = 600, width = 20, height = 16, units = "in")
layout(matrix(c(1,2,3,4), 4, 1, byrow = TRUE))
par(mar = c(4.8,6.5,0,0.5))
plot(time(B12qoval), as.numeric(B12qoval), ylim = c(0,6), type = "n", cex.lab = 3, cex.axis = 3,main = "", xlab = "", ylab = "",  las = 1 )#ylab=expression(paste("Streamflow"~ (m^3~s^-1))),
polygon(c(time(B12qoval), rev(time(B12qoval))), c(B12GMM$EstPosteriorv[,"quan2_5"], rev(B12GMM$EstPosteriorv[,"quan97_5"])), col="cyan", border=F) # #00000044 : grey;  #0000FF44: blue
points(time(B12qoval), as.numeric(B12qoval), col="black", pch =21, bg="red", cex = 1.5)
lines(time(B12qoval), B12GMM$EstPosteriorv[,"median"], lwd = 3, col="black")
lines(time(B12qoval), B12qsval, lwd = 3, col="blue")
text(time(B12qoval[105]),4, labels = "GMM", cex = 3)

# Fig. bias correction cristhian thesis presentation
par(mar = c(4.8,6.5,0,0.5))
plot(time(B12qoval[40:100]), as.numeric(B12qoval[40:100]), ylim = c(0,6), ylab =expression(paste("Caudal"~ (m^3~s^-1))),type = "n", cex.lab = 3, cex.axis = 3,main = "", xlab = "",  las = 1 )#ylab=expression(paste("Streamflow"~ (m^3~s^-1))),
polygon(c(time(B12qoval[40:100]), rev(time(B12qoval[40:100]))), c(B12GMM$EstPosteriorv[40:100,"quan2_5"], rev(B12GMM$EstPosteriorv[40:100,"quan97_5"])), col="cyan", border=F) # #00000044 : grey;  #0000FF44: blue
points(time(B12qoval[40:100]), as.numeric(B12qoval[40:100]), col="black", pch =21, bg="red", cex = 1.5)
lines(time(B12qoval[40:100]), B12GMM$EstPosteriorv[40:100,"median"], lwd = 3, col="red")
lines(time(B12qoval[40:100]), B12qsval[40:100], lwd = 3, col="black")
legend("topleft", cex = 3, c("Observado", "Simulado", "95% incertidumbre","mediana post-procesador"), fill=c(NA, NA, "cyan", NA),border=c("white", "white", "black"), lwd = c(NA,3,NA),col = c("red","blue",NA, "black"),bg=c("red","",""),pch = c(20,NA,NA),bty = "n", horiz = T, x.intersp = 0.5, xjust=1)

# lines(time(B12qoval), as.numeric(B12qsval), lwd = 3, col="black")
# legend("topleft", cex = 3, c("Observed", "Simulated", "95% uncertainty"), fill=c(NA, NA, "cyan"),border=c("white", "white", "black"), lwd = c(NA,3,NA),col = c("red","black",NA),bg=c("red","",""),pch = c(20,NA,NA),bty = "n", horiz = T, x.intersp = 0.5, xjust=1)
plot(time(B12qoval), as.numeric(B12qoval), ylim = c(0,6), type = "n", cex.lab = 3, cex.axis = 3,main = "", xlab = "", ylab = "",  las = 1 )#ylab=expression(paste("Streamflow"~ (m^3~s^-1))),
polygon(c(time(B12qoval), rev(time(B12qoval))), c(B12MCPt$Est.Posteriorv[,"quan2_5"], rev(B12MCPt$Est.Posteriorv[,"quan97_5"])), col="cyan", border=F) # #00000044 : grey;  #0000FF44: blue
points(time(B12qoval), as.numeric(B12qoval), col="black", pch =21, bg="red", cex = 1.5)
lines(time(B12qoval), B12MCPt$Est.Posteriorv[,"median"], lwd = 3, col="black")
text(time(B12qoval[105]),4, labels = "MCPt", cex = 3)
#lines(time(B1qoval), as.numeric(B12qsval), lwd = 3, col="black")
plot(time(B12qoval), as.numeric(B12qoval), ylim = c(0,6), type = "n", cex.lab = 3, cex.axis = 3,main = "", xlab = "", ylab="", las = 1 )#ylab=expression(paste("Streamflow"~ (m^3~s^-1)))
polygon(c(time(B12qoval), rev(time(B12qoval))), c(B12MCP$Est.Posteriorv[,"quan2_5"], rev(B12MCP$Est.Posteriorv[,"quan97_5"])), col="cyan", border=F) # #00000044 : grey;  #0000FF44: blue
points(time(B12qoval), as.numeric(B12qoval), col="black", pch =21, bg="red", cex = 1.5)
lines(time(B12qoval), B12MCP$Est.Posteriorv[,"median"], lwd = 3, col="black")
text(time(B12qoval[105]),4, labels = "MCP", cex = 3)
#lines(time(B1qoval), as.numeric(B12qsval), lwd = 3, col="black")
# plot(time(B12qoval), as.numeric(B12qoval), ylim = c(0,6), type = "n", cex.lab = 3, cex.axis = 3,main = "", xlab = "", ylab = "",  las = 1 )#ylab=expression(paste("Streamflow"~ (m^3~s^-1))),
# polygon(c(time(B12qoval), rev(time(B12qoval))), c(B12ABC$Est.Posteriorv[,"quan2_5"], rev(B12ABC$Est.Posteriorv[,"quan97_5"])), col="cyan", border=F) # #00000044 : grey;  #0000FF44: blue
# points(time(B12qoval), as.numeric(B12qoval), col="black", pch =21, bg="red", cex = 1.5)
# lines(time(B12qoval), B12ABC$Est.Posteriorv[,"median"], lwd = 3, col="black")
# text(time(B12qoval[105]),4, labels = "ABC", cex = 3)
#lines(time(B1qoval), as.numeric(B12qsval), lwd = 3, col="black")
plot(time(B12qoval), as.numeric(B12qoval), ylim = c(0,6), type = "n", cex.lab = 3, cex.axis = 3,main = "", xlab = "", ylab = "", las = 1 ) # ylab=expression(paste("Streamflow"~ (m^3~s^-1))),
polygon(c(time(B12qoval), rev(time(B12qoval))), c(B12MCMC$Est.Posteriorv[,"quan2_5"], rev(B12MCMC$Est.Posteriorv[,"quan97_5"])), col="cyan", border=F) # #00000044 : grey;  #0000FF44: blue
points(time(B12qoval), as.numeric(B12qoval), col="black", pch =21, bg="red", cex = 1.5)
lines(time(B12qoval), B12MCMC$Est.Posteriorv[,"median"], lwd = 3, col="black")
text(time(B12qoval[105]),4, labels = "MCMC", cex = 3)
#lines(time(B1qoval), as.numeric(B12qsval), lwd = 3, col="black")
dev.off()

# fig 8 Uncertainty band catchment (B11) #----------------------------------------------------

setwd("F:/TesinaMasterCoDireccion/PaperMOPEX")
jpeg("./Fig6UncertaintyB11Validation.jpg", res = 600, width = 20, height = 16, units = "in")
layout(matrix(c(1,2,3), 3, 1, byrow = TRUE))
par(mar = c(4.8,6.5,0,0.5))
plot(time(B11qoval), as.numeric(B11qoval), ylim = c(0,10.2), type = "n", cex.lab = 3, cex.axis = 3,main = "", xlab = "", ylab=expression(paste("Streamflow"~ (m^3~s^-1))),  las = 1 )#ylab=expression(paste("Streamflow"~ (m^3~s^-1))),
polygon(c(time(B11qoval), rev(time(B11qoval))), c(B11GMM$EstPosteriorv[,"quan2_5"], rev(B11GMM$EstPosteriorv[,"quan97_5"])), col="cyan", border=F) # #00000044 : grey;  #0000FF44: blue
points(time(B11qoval), as.numeric(B11qoval), col="black", pch =21, bg="red", cex = 1.5)
lines(time(B11qoval), B11GMM$EstPosteriorv[,"median"], lwd = 3, col="black")
text(time(B11qoval[105]),4, labels = "GMCP", cex = 3)
text(time(B11qoval[50]),9, labels = "Reliability = 0.94", cex = 3)
text(time(B11qoval[50]),8, labels = "Sharpness = 4.44", cex = 3)
text(time(B11qoval[50]),7, labels = "  NSE = 0.94", cex = 3)
text(time(B11qoval[50]),6, labels = "  95%CR = 93.55", cex = 3)
legend(time(B11qoval[140]),10, cex = 3,c("95% Confidence Interval","Forecast Median","Observation"), fill=c("cyan",NA, "red"),col = c(NA,"black","black"), lty = c(0,1,0), lwd = c(0,3,0), pch = c(NA,NA,21),border=c("black","white", "white"),bg=c("","","red"),bty = "n")

# lines(time(B11qoval), as.numeric(B11qsval), lwd = 3, col="black")
# legend("topleft", cex = 3, c("Observed", "Simulated", "95% uncertainty"), fill=c(NA, NA, "cyan"),border=c("white", "white", "black"), lwd = c(NA,3,NA),col = c("red","black",NA),bg=c("red","",""),pch = c(20,NA,NA),bty = "n", horiz = T, x.intersp = 0.5, xjust=1)
plot(time(B11qoval), as.numeric(B11qoval), ylim = c(0,10.2), type = "n", cex.lab = 3, cex.axis = 3,main = "", xlab = "", ylab=expression(paste("Streamflow"~ (m^3~s^-1))),  las = 1 )#ylab=expression(paste("Streamflow"~ (m^3~s^-1))),
polygon(c(time(B11qoval), rev(time(B11qoval))), c(B11MCPt$Est.Posteriorv[,"quan2_5"], rev(B11MCPt$Est.Posteriorv[,"quan97_5"])), col="cyan", border=F) # #00000044 : grey;  #0000FF44: blue
points(time(B11qoval), as.numeric(B11qoval), col="black", pch =21, bg="red", cex = 1.5)
lines(time(B11qoval), B11MCPt$Est.Posteriorv[,"median"], lwd = 3, col="black")
text(time(B11qoval[105]),4, labels = "MCPt", cex = 3)
text(time(B11qoval[50]),9, labels = "Reliability = 0.81", cex = 3)
text(time(B11qoval[50]),8, labels = "Sharpness = 1.74", cex = 3)
text(time(B11qoval[50]),7, labels = "  NSE = 0.81", cex = 3)
text(time(B11qoval[50]),6, labels = "  95%CR = 98.16", cex = 3)

#lines(time(B1qoval), as.numeric(B11qsval), lwd = 3, col="black")
plot(time(B11qoval), as.numeric(B11qoval), ylim = c(0,10.2), type = "n", cex.lab = 3, cex.axis = 3,main = "", xlab = "", ylab=expression(paste("Streamflow"~ (m^3~s^-1))), las = 1 )#ylab=expression(paste("Streamflow"~ (m^3~s^-1)))
polygon(c(time(B11qoval), rev(time(B11qoval))), c(B11MCP$Est.Posteriorv[,"quan2_5"], rev(B11MCP$Est.Posteriorv[,"quan97_5"])), col="cyan", border=F) # #00000044 : grey;  #0000FF44: blue
lines(time(B11qoval), B11MCP$Est.Posteriorv[,"median"], lwd = 3, col="black")
points(time(B11qoval), as.numeric(B11qoval), col="black", pch =21, bg="red", cex = 1.5)
text(time(B11qoval[105]),4, labels = "MCP", cex = 3)
text(time(B11qoval[50]),9, labels = "Reliability = 0.82", cex = 3)
text(time(B11qoval[50]),8, labels = "Sharpness = 1.84", cex = 3)
text(time(B11qoval[50]),7, labels = "  NSE = 0.82", cex = 3)
text(time(B11qoval[50]),6, labels = "  95%CR = 98.16", cex = 3)
dev.off()

# fig 4 boxplot for post-processors for validation #----------------------------------------------------
setwd("F:/TesinaMasterCoDireccion/PaperMOPEX")
jpeg("./Fig4box_plotValidation.jpg", res = 600, width = 20, height = 12, units = "in")
# jpeg("./box-plot.jpg", res = 600, width = 22, height = 12, units = "in")
# plot.new()
# ajuste base de datos
# M1Nv <- M1Nv[2:5,] # quito la columna del abc
# rownames(M1Nv)<-c ("LRP","MCP","MCPt","GMCP") # cambio los nombre

par(mfrow=c(1,3), oma=c(2,2,2,2),cex.lab=2.5,cex.axis=2.5,cex.main=3,mar=c(6,4,6,2))
print(boxplot(t(NSEord [c(nrow(NSEord):1) ,]), main = "Nash-Sutcliffe efficiency (NSE)",notch = FALSE, col =c("blue","red","green3"),xlab="Post-processor"),split=c(1, 1, 2, 2))
print(boxplot(t(p [c(nrow(p):1) ,]), main = "Sharpness",notch = FALSE, col =c("blue","red","green3"),xlab="Post-processor"),split=c(2, 1, 2, 2), newpage=FALSE,xlab="Post-processor")
#print(boxplot(t(M2dfactorv [c(nrow(M2dfactorv):1) ,]), main = "d-factor",notch = FALSE, col =c("blue","red","green3","cyan","magenta"),xlab="Post-processor"),split=c(1, 2, 2, 2), newpage=FALSE)
print(boxplot(t(CR95 [c(nrow(CR95):1) ,]), main = " 95%CR",notch = FALSE, col =c("blue","red","green3"),xlab="Post-processor"),split=c(2, 2, 2, 2), newpage=FALSE,xlab="Post-processor")
dev.off()

# computing % improven in sharpness

Pimp_NSE <- ((mean(NSEord[3,]) - mean(NSEord[2,])) / mean(NSEord[2,])) * 100
Pimp_shar <- ((mean(p[3,]) - mean(p[2,])) / mean(p[2,])) * 100
Pimp_CR <- ((mean(CR95[3,]) - mean(CR95[2,])) / mean(CR95[2,])) * 100

####  examples differents plots: Art color combination
# colors: grey  cyan #00000044 #0000FF44 aquamarine chartreuse1 darkorange deeppink deepskyblue blueviolet
# Predictive Uncertainty B1 ABC validation
setwd("F:/TesinaMasterCoDireccion/ResultABC")
jpeg("./FigPUB1ABCval.jpg", res = 600, width = 20, height = 12, units = "in")
layout(matrix(c(1,1,2,3,3,4), 1, 3, byrow = TRUE))
par(mar = c(5.5,7,0.5,0.5))
plot(time(B1qoval), as.numeric(B1qsval), type ="n", xlab="", ylab=expression(paste("Streamflow"~ (m^3~s^-1))),  cex.axis = 3, cex.lab = 3)#xaxt="n",
#axis(1, at=1:12, labels = month.abb, las = 2,cex.axis = 2.5)
polygon(c(time(B1qoval), rev(time(B1qoval))), c(B1ABC$Est.Posteriorv[,"quan2_5"], rev(B1ABC$Est.Posteriorv[,"quan97_5"])), col='grey', border=F)
points(time(B1qoval), as.numeric(B1qoval), col="red", pch =21, bg="red", cex = 2)
lines(time(B1qoval), B1ABC$Est.Posteriorv[,"median"],lty =1, lwd = 3, col="black")
#lines(1:12, as.numeric(modelCC), col="blue", lwd = 2)
qqplot(B1ABC$QQplotv$x, B1ABC$QQplotv$zi, cex.axis = 2.5, xlim = c(0,1),ylim = c(0,1),cex.lab = 2.5, xlab="Theoretical Quantile U[0,1]",ylab="Quantile of forecast", col = "blue",type = "l", pch=20, lty=1, lwd = 3)
abline(0,1,lwd=2)
dev.off()

# Predictive Uncertainty B1 MCMC validation
setwd("F:/TesinaMasterCoDireccion/ResultMCMC")
jpeg("./FigPUB1MCMCval.jpg", res = 600, width = 20, height = 12, units = "in")
layout(matrix(c(1,1,2,3,3,4), 1, 3, byrow = TRUE))
par(mar = c(5.5,7,0.5,0.5))
plot(time(B1qoval), as.numeric(B1qsval), type ="n", xlab="", ylab=expression(paste("Streamflow"~ (m^3~s^-1))),  cex.axis = 3, cex.lab = 3)#xaxt="n",
#axis(1, at=1:12, labels = month.abb, las = 2,cex.axis = 2.5)
polygon(c(time(B1qoval), rev(time(B1qoval))), c(B1MCMC$Est.Posteriorv[,"quan2_5"], rev(B1MCMC$Est.Posteriorv[,"quan97_5"])), col='grey', border=F)
points(time(B1qoval), as.numeric(B1qoval), col="red", pch =21, bg="red", cex = 2)
lines(time(B1qoval), B1MCMC$Est.Posteriorv[,"median"],lty =1, lwd = 3, col="black")
#lines(1:12, as.numeric(modelCC), col="blue", lwd = 2)
qqplot(B1MCMC$QQplotv$x, B1MCMC$QQplotv$zi, cex.axis = 2.5, xlim = c(0,1),ylim = c(0,1),cex.lab = 2.5, xlab="Theoretical Quantile U[0,1]",ylab="Quantile of forecast", col = "blue",type = "l", pch=20, lty=1, lwd = 3)
abline(0,1,lwd=2)
dev.off()

# colors: grey  cyan #00000044 #0000FF44 aquamarine chartreuse1 darkorange deeppink deepskyblue blueviolet


# Predictive Uncertainty B1 MCP validation
setwd("F:/TesinaMasterCoDireccion/ResultMCP")
jpeg("./FigPUB1MCPval.jpg", res = 600, width = 20, height = 12, units = "in")
layout(matrix(c(1,1,2,3,3,4), 1, 3, byrow = TRUE))
par(mar = c(5.5,7,0.5,0.5))
plot(time(B1qoval), as.numeric(B1qsval), type ="n", xlab="", ylab=expression(paste("Streamflow"~ (m^3~s^-1))),  cex.axis = 3, cex.lab = 3)#xaxt="n",
#axis(1, at=1:12, labels = month.abb, las = 2,cex.axis = 2.5)
polygon(c(time(B1qoval), rev(time(B1qoval))), c(B1MCP$Est.Posteriorv[,"quan2_5"], rev(B1MCP$Est.Posteriorv[,"quan97_5"])), col='chartreuse1', border=F)
points(time(B1qoval), as.numeric(B1qoval), col="red", pch =21, bg="red", cex = 2)
lines(time(B1qoval), B1MCP$Est.Posteriorv[,"median"],lty =1, lwd = 3, col="black")
#lines(1:12, as.numeric(modelCC), col="blue", lwd = 2)
qqplot(B1MCP$QQplotv$x, B1MCP$QQplotv$zi, cex.axis = 2.5, xlim = c(0,1),ylim = c(0,1),cex.lab = 2.5, xlab="Theoretical Quantile U[0,1]",ylab="Quantile of forecast", col = "blue",type = "l", pch=20, lty=1, lwd = 3)
abline(0,1,lwd=2)
dev.off()


# Predictive Uncertainty B1 MCPt validation
setwd("F:/TesinaMasterCoDireccion/ResultMCPt")
jpeg("./FigPUB1MCPtval.jpg", res = 600, width = 20, height = 12, units = "in")
layout(matrix(c(1,1,2,3,3,4), 1, 3, byrow = TRUE))
par(mar = c(5.5,7,0.5,0.5))
plot(time(B1qoval), as.numeric(B1qsval), type ="n", xlab="", ylab=expression(paste("Streamflow"~ (m^3~s^-1))),  cex.axis = 3, cex.lab = 3)#xaxt="n",
#axis(1, at=1:12, labels = month.abb, las = 2,cex.axis = 2.5)
polygon(c(time(B1qoval), rev(time(B1qoval))), c(B1MCPt$Est.Posteriorv[,"quan2_5"], rev(B1MCPt$Est.Posteriorv[,"quan97_5"])), col='grey', border=F)
points(time(B1qoval), as.numeric(B1qoval), col="black", pch =21, bg="darkolivegreen1", cex = 2)
lines(time(B1qoval), B1MCPt$Est.Posteriorv[,"median"],lty =1, lwd = 3, col="black")
#lines(1:12, as.numeric(modelCC), col="blue", lwd = 2)
qqplot(B1MCPt$QQplotv$x, B1MCPt$QQplotv$zi, cex.axis = 2.5, xlim = c(0,1),ylim = c(0,1),cex.lab = 2.5, xlab="Theoretical Quantile U[0,1]",ylab="Quantile of forecast", col = "blue",type = "l", pch=20, lty=1, lwd = 3)
abline(0,1,lwd=2)
dev.off()

# calibration MCPt
par(mar = c(5.5,7,0.5,0.5))
plot(time(B1qocal), as.numeric(B1qscal), type ="n", xlab="", ylab=expression(paste("Streamflow"~ (m^3~s^-1))),  cex.axis = 3, cex.lab = 3)#xaxt="n",
#axis(1, at=1:12, labels = month.abb, las = 2,cex.axis = 2.5)
polygon(c(time(B1qocal), rev(time(B1qocal))), c(B1MCPt$Est.Posterior[,"quan2_5"], rev(B1MCPt$Est.Posterior[,"quan97_5"])), col='grey', border=F)
points(time(B1qocal), as.numeric(B1qocal), col="black", pch =21, bg="darkolivegreen1", cex = 2)
lines(time(B1qocal), B1MCPt$Est.Posterior[,"median"],lty =1, lwd = 3, col="black")
#lines(1:12, as.numeric(modelCC), col="blue", lwd = 2)
qqplot(B1MCPt$QQplot$x, B1MCPt$QQplot$zi, cex.axis = 2.5, xlim = c(0,1),ylim = c(0,1),cex.lab = 2.5, xlab="Theoretical Quantile U[0,1]",ylab="Quantile of forecast", col = "blue",type = "l", pch=20, lty=1, lwd = 3)
abline(0,1,lwd=2)



# Art
setwd("F:/TesinaMasterCoDireccion")
jpeg("./ArtUncertaintyBand.jpg", res = 600, width = 20, height = 12, units = "in")
par(mar=c(1,1,1,1))
layout(matrix(c(1,2,3,4,5,6,7,8,9), 3, 3, byrow = TRUE))

plot(time(B1qoval), as.numeric(B1qsval), type ="n", xlab= "", ylab= "")#xaxt="n",
polygon(c(time(B1qoval), rev(time(B1qoval))), c(B1MCP$Est.Posteriorv[,"quan2_5"], rev(B1MCP$Est.Posteriorv[,"quan97_5"])), col='grey', border=F)
points(time(B1qoval), as.numeric(B1qoval), col="black", pch =21, bg="red", cex = 2)
lines(time(B1qoval), B1MCP$Est.Posteriorv[,"median"],lty =1, lwd = 3, col="black")
text(time(B1qoval[1]),8,labels ="A", cex = 2)
#title("clasico")

plot(time(B1qoval), as.numeric(B1qsval), type ="n", xlab= "", ylab= "")#xaxt="n",
polygon(c(time(B1qoval), rev(time(B1qoval))), c(B1MCP$Est.Posteriorv[,"quan2_5"], rev(B1MCP$Est.Posteriorv[,"quan97_5"])), col='deepskyblue', border=F)
points(time(B1qoval), as.numeric(B1qoval), col="black", pch =21, bg="darkolivegreen1", cex = 2)
lines(time(B1qoval), B1MCP$Est.Posteriorv[,"median"],lty =1, lwd = 3, col="black")
text(time(B1qoval[1]),8,labels ="B", cex = 2)
#title("pavo real")

plot(time(B1qoval), as.numeric(B1qsval), type ="n", xlab= "", ylab= "")#xaxt="n",
polygon(c(time(B1qoval), rev(time(B1qoval))), c(B1MCP$Est.Posteriorv[,"quan2_5"], rev(B1MCP$Est.Posteriorv[,"quan97_5"])), col='cornflowerblue', border=F)
points(time(B1qoval), as.numeric(B1qoval), col="black", pch =21, bg="darkgoldenrod1", cex = 2)
lines(time(B1qoval), B1MCP$Est.Posteriorv[,"median"],lty =1, lwd = 3, col="black")
text(time(B1qoval[1]),8,labels ="C", cex = 2)
#title("exoctic")

plot(time(B1qoval), as.numeric(B1qsval), type ="n", xlab= "", ylab= "")#xaxt="n",
polygon(c(time(B1qoval), rev(time(B1qoval))), c(B1MCP$Est.Posteriorv[,"quan2_5"], rev(B1MCP$Est.Posteriorv[,"quan97_5"])), col='coral', border=F)
points(time(B1qoval), as.numeric(B1qoval), col="red", pch =21, bg="black", cex = 2)
lines(time(B1qoval), B1MCP$Est.Posteriorv[,"median"],lty =1, lwd = 3, col="black")
text(time(B1qoval[1]),8,labels ="D", cex = 2)
#title("avestrus")

plot(time(B1qoval), as.numeric(B1qsval), type ="n", xlab= "", ylab= "")#xaxt="n",
polygon(c(time(B1qoval), rev(time(B1qoval))), c(B1MCP$Est.Posteriorv[,"quan2_5"], rev(B1MCP$Est.Posteriorv[,"quan97_5"])), col='darkorange', border=F)
points(time(B1qoval), as.numeric(B1qoval), col="red", pch =21, bg="black", cex = 2)
lines(time(B1qoval), B1MCP$Est.Posteriorv[,"median"],lty =1, lwd = 3, col="black")
text(time(B1qoval[1]),8,labels ="E", cex = 2)
#title("Negro")

plot(time(B1qoval), as.numeric(B1qsval), type ="n", xlab= "", ylab= "")#xaxt="n",
polygon(c(time(B1qoval), rev(time(B1qoval))), c(B1MCP$Est.Posteriorv[,"quan2_5"], rev(B1MCP$Est.Posteriorv[,"quan97_5"])), col='deepskyblue4', border=F)
points(time(B1qoval), as.numeric(B1qoval), col="red", pch =21, bg="darkgoldenrod1", cex = 2)
lines(time(B1qoval), B1MCP$Est.Posteriorv[,"median"],lty =1, lwd = 3, col="black")
text(time(B1qoval[1]),8,labels ="F", cex = 2)
#title("indian")

plot(time(B1qoval), as.numeric(B1qsval), type ="n", xlab= "", ylab= "")#xaxt="n",
polygon(c(time(B1qoval), rev(time(B1qoval))), c(B1MCP$Est.Posteriorv[,"quan2_5"], rev(B1MCP$Est.Posteriorv[,"quan97_5"])), col='darkorchid', border=F)
points(time(B1qoval), as.numeric(B1qoval), col="red", pch =21, bg="yellow", cex = 2)
lines(time(B1qoval), B1MCP$Est.Posteriorv[,"median"],lty =1, lwd = 3, col="black")
text(time(B1qoval[1]),8,labels ="G", cex = 2)
#title("Purple & yellow")

plot(time(B1qoval), as.numeric(B1qsval), type ="n", xlab= "", ylab= "")#xaxt="n",
polygon(c(time(B1qoval), rev(time(B1qoval))), c(B1MCP$Est.Posteriorv[,"quan2_5"], rev(B1MCP$Est.Posteriorv[,"quan97_5"])), col='blue4', border=F)
points(time(B1qoval), as.numeric(B1qoval), col="red", pch =21, bg="deeppink", cex = 2)
lines(time(B1qoval), B1MCP$Est.Posteriorv[,"median"],lty =1, lwd = 3, col="coral")
text(time(B1qoval[1]),8,labels ="H", cex = 2)
#title("Blue & pink")

plot(time(B1qoval), as.numeric(B1qsval), type ="n", xlab= "", ylab= "")#xaxt="n",
polygon(c(time(B1qoval), rev(time(B1qoval))), c(B1MCP$Est.Posteriorv[,"quan2_5"], rev(B1MCP$Est.Posteriorv[,"quan97_5"])), col='cyan', border=F)
points(time(B1qoval), as.numeric(B1qoval), col="black", pch =21, bg="red", cex = 2)
lines(time(B1qoval), B1MCP$Est.Posteriorv[,"median"],lty =1, lwd = 3, col="black")
text(time(B1qoval[1]),8,labels ="I", cex = 2)
#title("news")
dev.off()

# save workspace
save.image("G:/Mi unidad/1Doctorado/TesinaMasterCoDireccion/PaperMOPEX/Water/Figures/MOPEXpostprocessing.RData")
load("MOPEXpostprocessing.RData")
