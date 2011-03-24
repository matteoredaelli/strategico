#!/usr/bin/env Rscript

## This program is free software: you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 3 of the License, or
## any later version.
##
## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with this program.  If not, see <http://www.gnu.org/licenses/>.

##authors L. Finos, M. Rinaldo

## parametri aggiunti
## diff.sea=1,diff.trend=1,max.p=2,max.q=1,max.P=0,max.Q=1, logtransform.es=FALSE , increment=1 ,idDiff=FALSE, idLog =FALSE

## ho inserito anche due funzioni una per la scelta delle differenze in modo automatico per l'arima e l'altra per la scelta del log
## di default ho impostato che i vari parametri vengano utilizzati quelli de default
## se idDiff viene messo a TRUE i valori di defaut non vengono utilizzati ma sostituiti con quelli trovati dalla funzione
## lo stesso per idLog.

## le ho inserito una lista per il report della classe es

## non ho inserito la possibilita di inserire manualmente un modello es perche non intuitivo
## questi sono i parametri che si stima in modo automatico, sincetamente non riuscirei proprio 
## osservando una serie a fissarli ad un valore.
## es(alpha = 0, beta = 0, phi = 1, gamma = 0, l.start, d.start, freq = 1, s.start, sigma2 = 1,
## drift = c("none", "additive", "c/additive", "d/additive", "multiplicative", "c/multiplicative"),
## seasonality = c("none","additive", "c/additive", "multiplicative", "c/multiplicative"),
## innovation = c("additive", "multiplicative"))


############## ltp()

ltp <- function(product, try.models = c("lm", "arima","es"), criterion = "BestIC", criterionExcludeMaxGreaterThan = Inf, n.ahead = 4, logtransform = TRUE,logtransform.es=FALSE, 
                period.freq = 2,increment=1, xreg.lm = NA,diff.sea=1,diff.trend=1,max.p=2,max.q=1,max.P=1,max.Q=0, 
                xreg.arima = NULL,idDiff=FALSE,idLog=FALSE, stationary.arima = FALSE, period.start = c(1997, 1),
                period.end=c(2010,1), NA2value = 3, range = c(3, Inf), n.min = 15, stepwise = TRUE, formula.right.lm = NULL, negToZero = TRUE, predictInteger = TRUE) {
  
#####################################
  ## ATTENZIONE normalizedata: ho sistemato la mia versione a funziona
  ## l'importante secondo me e' che venga aggiornata anche il nuovo period.start
#####################################
  
  ## result.normalize <- ltp.normalizeData(product, range, NA2value,period.end)
  result.normalize <- ltp.normalizeData(product=product,range=range,NA2value=NA2value,period.start=period.start,increment=increment,period.end=period.end,period.freq=period.freq)

  period.start = result.normalize$start
  product = result.normalize$product
  n = dim(product)[1]
  
  
  if(idLog) logtransform = IDlog(product,period.start)
  
  if (is.null(try.models)) 
    try.models = c("mean","trend","lm", "arima", "es")
  
  library(forecast)
  library(ast)
  
  AIC <- rep(NA,5)
  names(AIC) <- c("Mean","Trend","ExponentialSmooth","LinearModel","Arima")
  IC.width <- R2 <- VarCoeff <- AIC
  NULL -> Mean -> Trend -> ExponentialSmooth -> LinearModel -> Arima
  
  if (("mean" %in% try.models)&(n >= period.freq )) {
    Mean = mod.lm(product = product, n.ahead = n.ahead, 
      period.start = period.start, period.freq = period.freq, 
      xreg.lm = NA, logtransform = FALSE, 
      stepwise = FALSE, formula.right.lm = 'S', negToZero=negToZero,predictInteger=predictInteger)
    AIC["Mean"] = Mean$AIC
    IC.width["Mean"] = Mean$IC.width
    R2["Mean"] = Mean$R2
	VarCoeff["Mean"] = Mean$VarCoeff 
  }
  if (("trend" %in% try.models)&(n >= 5 )) {
    Trend = mod.lm(product = product, n.ahead = n.ahead, 
      period.start = period.start, period.freq = period.freq, 
      xreg.lm = NA, logtransform = FALSE, 
      stepwise = FALSE, formula.right.lm = 'S+trend', negToZero=negToZero,predictInteger=predictInteger)
    AIC["Trend"] = Trend$AIC
    IC.width["Trend"] = Trend$IC.width
    R2["Trend"] = Trend$R2
	VarCoeff["Trend"] = Trend$VarCoeff
  }
  if (("lm" %in% try.models)&(n >= n.min )) {
    LinearModel = mod.lm(product = product, n.ahead = n.ahead, 
      period.start = period.start, period.freq = period.freq, 
      xreg.lm = xreg.lm, logtransform = logtransform, 
      stepwise = stepwise, formula.right.lm = formula.right.lm, negToZero=negToZero,predictInteger=predictInteger)
    AIC["LinearModel"] = LinearModel$AIC
    IC.width["LinearModel"] = LinearModel$IC.width
    R2["LinearModel"] = LinearModel$R2
	VarCoeff["LinearModel"] = LinearModel$VarCoeff
  }
  if (("es" %in% try.models)&(n >= n.min )) {
    ExponentialSmooth = mod.es(product = product, n.ahead = n.ahead, 
      period.freq = period.freq, period.start = period.start, 
      logtransform.es = logtransform.es, stepwise = stepwise, negToZero=negToZero,predictInteger=predictInteger)
    AIC["ExponentialSmooth"] = ExponentialSmooth$AIC
    IC.width["ExponentialSmooth"] = ExponentialSmooth$IC.width
    R2["ExponentialSmooth"] = ExponentialSmooth$R2
	VarCoeff["ExponentialSmooth"] = ExponentialSmooth$VarCoeff 
  }
  if (("arima" %in% try.models)&(n >= n.min )) {
    Arima = mod.arima(product=product,logtransform=logtransform,
      diff.sea=diff.sea,diff.trend=diff.trend,idDiff=idDiff,max.p=max.p,max.q=max.q,
      max.P=max.P,max.Q=max.Q,stationary.arima=stationary.arima,n.ahead=n.ahead,
      period.freq=period.freq,xreg.arima=xreg.arima,period.start=period.start,stepwise=stepwise, negToZero=negToZero,predictInteger=predictInteger)
    AIC["Arima"] = Arima$AIC
    IC.width["Arima"] = Arima$IC.width
    R2["Arima"] = Arima$R2
	VarCoeff["Arima"] = Arima$VarCoeff
  }
 
  ID.model <- switch(criterion, BestIC = which.min(IC.width*(ifelse(VarCoeff<criterionExcludeMaxGreaterThan,1,NA))), 
                                BestAIC = which.min(AIC*(ifelse(VarCoeff<criterionExcludeMaxGreaterThan,1,NA))) )		
  results = list(values = product, Mean = Mean, Trend = Trend, LinearModel = LinearModel, 
    ExponentialSmooth = ExponentialSmooth, Arima = Arima, BestModel = names(ID.model), criterion=criterion, criterionExcludeMaxGreaterThan=criterionExcludeMaxGreaterThan)
  results
}

## tronca le prime osservazioni se ==0 o NA
## gli 0 in mezzo e alla fine vengono considerati dati validi
## ltp.normalizeData <- function(product, range, NA2value,period.end) {

                                        # stop = which(rownames(product) == paste(period.end, collapse = "-"))[1]
                                        # if (!is.na(stop)) {
                                        # if (stop < dim(product)[1]) 
                                        # out <- ((stop + 1):dim(product)[1])
                                        # else if (is.na(product[period.end,]) | (unlist(product[period.end,]) == 0) )
                                        # out= (1:dim(product)[1])
                                        # } else {
                                        # out= (1:dim(product)[1])
                                        # }

                                        # product = product[-out, ,drop=FALSE]

                                        # from = which(cumsum(is.na(product) | (unlist(product) == 0)) < (1:length(unlist(product))))[1]
                                        # if (!is.na(from)) {
                                        # if (from > 1) {
                                        # product <- product[-(1:(from - 1)), , drop = FALSE]
                                        # }
                                        # }

                                        # if(dim(product)[1]==0) return(product)

                                        # product[is.na(product), ] = NA2value
                                        # product[product < range[1], ] = range[1]
                                        # product[product > range[2], ] = range[2]

                                        # product
## }



ltp.normalizeData <- function(product, range, NA2value=NULL,period.start,period.freq,increment,period.end) {
  period.start.fix = period.start
  period.start = apply(matrix(as.numeric(unlist(strsplit(rownames(product),"-"))),nrow=2),1,min)
  
  times=sapply (0:(sum((period.end-period.start)*c(period.freq,1))),	function(i) paste(.incSampleTime(now=period.start, period.freq = period.freq, increment = i),collapse="-"))
  if (is.na(NA2value)){
	temp = sapply(1: period.freq,function(i) mean(product[seq(from=i,by=period.freq,to=max(i,dim(product)[1])),],na.rm=TRUE))
	if(period.start[2]>1) temp = c(temp[period.start[2]:period.freq], temp[1:(period.start[2]-1)])
  } 
  temp = rep(NA2value, period.freq)
  
  productnew=data.frame( rep(temp, len = length(times) ))
  rownames(productnew)=times
  colnames(productnew)=colnames(product)
  productnew[rownames(product),]=product
  
  id.start=which(paste(period.start.fix,collapse="-")==rownames(product))
  if( length(id.start)>0 ) {
	productnew=productnew[id.start:dim(productnew)[1],,drop=FALSE]
	period.start=period.start.fix
  } else if( sum(period.start.fix*c(period.freq,1)) > sum(period.start*c(period.freq,1)) ){
	return(list(product=productnew[-(1:dim(productnew)[1]),,drop=FALSE],start=NA))
  }  #else go on
  
  temp=mean(product[grep(period.end[1],rownames(product)),],na.rm=TRUE) #mean of values in last year
  temp=ifelse(is.na(temp),0,temp)
  if (temp>0) {
    productnew=productnew[1:which(rownames(productnew)==paste(period.end,collapse="-")),,drop=FALSE]
  } else {
    return(list(product=productnew[-(1:dim(productnew)[1]), ,drop=FALSE],start=NA))
  }
  

  n = dim(productnew)[1]

  if(n>0){
    flag = TRUE
    i = 1
    start = period.start
    while ((i<=n)&flag) {
      if (productnew[1,] == 0) {
        productnew = productnew[-1,,drop=FALSE]
        start = .incSampleTime(start, period.freq = period.freq, increment=increment) 
      }
      else flag = FALSE
    }
    if(dim(productnew)[1]==0) return(productnew)
    if (!is.na(NA2value))    productnew[is.na(productnew), ] = NA2value
    else{
      for(i in 1:period.freq) 
        productnew[seq(from=i,by=period.freq,to=dim(productnew)[1])[is.na(productnew[seq(from=i,by=period.freq,to=dim(productnew)[1]),])],]=mean(productnew[seq(from=i,by=period.freq,to=dim(productnew)[1]),],na.rm=TRUE)
    }
    productnew[productnew < range[1], ] = range[1]
    productnew[productnew > range[2], ] = range[2]

    return(list(product=productnew,start=start))
  } else {return(list(product=productnew,start=NA))}
}



## da chiarire la frequenza dei data suprattutto per la classe lm
## intervalli di confidenza per le previsioni a 0.95 per tutte le classi di modelli
## per quanto riguarda lm ho lasciato libera la scelta del log
## possibilitÃƒÂ  di inserire fino a due regressori perÃƒÂ² devono essere della medesima lunghezza della serie analizzata
## criterio utilizzato AIC


## tutti i risultati dei modelli contengono un campo della lista contenente la serie strorica del product
## oltre ai data product con la forma originaes.

############## best.lm()
mod.lm <- function(product, n.ahead, period.start, period.freq, xreg.lm = NA, logtransform, stepwise, formula.right.lm = NULL,negToZero=negToZero,predictInteger=predictInteger) {
  
  if(is.null(formula.right.lm)) formula.right.lm = match.arg(formula.right.lm, " S * trend + S * trend2")
  
  y = as.vector(product)
  n = max(length(y), dim(y)[1])
  y = ts(y, start = period.start, frequency = period.freq)
  attr(y, "product") = names(product)
  
  k = n + n.ahead
  
  
                                        # var trend
  trend = 1:k
                                        # dummy stag
  S = as.factor(((period.start[2] - 1 + (0:(k - 1)))%%period.freq) + 1)
  
  d = data.frame(tempo = rep(NA, k), qta = rep(NA, k), trend = trend, 
    trend2 = trend^2, S = S, xreg.lm = xreg.lm)
  
  form = as.formula(paste(ifelse(logtransform, "log(stima$qta)", "stima$qta"), 
    " ~ ", formula.right.lm, sep = ""))
  
  d$tempo[1:n] = rownames(product)
  d$qta[1:n] = unlist(product)
  stima = d[(1:n), ]
  new.data = d[-(1:n), ]
  
  if (stepwise) steps=100 
  else steps=0
  modlm = step(lm(form,data=stima),steps=steps, direction = "both", trace = 0)
 
  pred = predict(modlm, newdata = new.data, interval = "prediction", level = 0.95)
  
  if (logtransform) {
    pred.modlm = exp(pred[, "fit",drop=FALSE])
    IC.pred.modlm = list(upr = exp(pred[, "upr"]), lwr = exp(pred[, "lwr"]))
  }
  else {
    pred.modlm = (pred[, "fit",drop=FALSE])
    IC.pred.modlm = list(upr = pred[, "upr"], lwr = pred[, "lwr"])
  }
  if(negToZero) {
	pred.modlm[pred.modlm<0]=0
	IC.pred.modlm$upr[IC.pred.modlm$upr<0]=0
	IC.pred.modlm$lwr[IC.pred.modlm$lwr<0]=0
	}
  if(predictInteger) {
	pred.modlm=round(pred.modlm,0)
	}
  
  pred.modlm=ts(pred.modlm, start=.incSampleTime(now=end(y), period.freq = period.freq) , frequency=period.freq)
  
  lm.AIC = AIC(modlm, k = 2)
  lm.R2 = summary(modlm)$r.squared
  ic.delta = mean(IC.pred.modlm$upr - IC.pred.modlm$lwr)
  maxJump = max(abs(product[(dim(product)[1]-period.freq+1):dim(product)[1],1]/pred.modlm[1:period.freq]-1),na.rm=TRUE)
  VarCoeff= sd(c(as.vector(pred.modlm),unlist(product)),na.rm=TRUE)/mean(c(as.vector(pred.modlm),unlist(product)),na.rm=TRUE)
  # m=matrix(NA,period.freq, ceiling((dim(pred.modlm)[1])/period.freq)+1)
  # m[1:(length(pred.modlm)+period.freq)]=c(y[dim(y)[1]:(dim(y)[1]-period.freq+1),],pred.modlm)
  # m=apply(m,2,mean,na.rm=TRUE)
  # sdJumps = sd(m[-1]/m[-length(m)])
  res = ts(residuals(modlm), start = period.start, frequency = period.freq)
                                        #media_errori = mean(errori_lm)
  lista.lm = list(ts.product = y, model = modlm, prediction = pred.modlm, 
    IC = IC.pred.modlm, AIC = lm.AIC, R2 = lm.R2, IC.width = ic.delta, maxJump=maxJump,  VarCoeff=VarCoeff, Residuals = res)
  lista.lm
}
############## best.arima()
mod.arima <- function(product,logtransform,diff.sea,diff.trend,idDiff,max.p,max.q,max.P,
                      max.Q,n.ahead,period.freq,xreg.arima,period.start,stepwise,stationary.arima,negToZero=negToZero,predictInteger=predictInteger) {
  y = as.vector(product)
  n = max(length(y), dim(y)[1])
                                        # vettore errori
                                        #errori_arima = rep(NA,fuori)
  y = ts(y, start = period.start, frequency = period.freq)
  if (idDiff) { d = IDdiff(y,period.freq=period.freq)
                diff.trend = as.integer(d[1])
                diff.sea = as.integer(d[2])
              }
  if (logtransform) {
    if (stepwise) 
      if (is.null(xreg.arima)) {
        mod = try(arimaId(log(y),c(max.p,diff.trend,max.q),list(order=c(max.P,diff.sea,max.Q)),idDiff=FALSE,method="ML",verbose=FALSE),TRUE)
        if (is(mod,"try-error")) { mod.arima=auto.arima(log(y),d=diff.trend,D=diff.sea,max.p=max.p,max.q=max.q,stationary=stationary.arima,max.Q=max.Q,max.P=max.P,ic="aic",xreg=xreg.arima,stepwise=stepwise)}
        else {
          index=print.allArima(mod, nshow=1)
          mod.arima = try(arima(log(y),c(as.integer(index["p"]),as.integer(index["d"]),as.integer(index["q"])),c(as.integer(index["P"]),as.integer(index["D"]),as.integer(index["Q"])),method="ML"),TRUE)
          if (is(mod.arima,"try-error")) { mod.arima=auto.arima(log(y),d=diff.trend,D=diff.sea,max.p=max.p,max.q=max.q,stationary=stationary.arima,max.Q=max.Q,max.P=max.P,ic="aic",xreg=xreg.arima,stepwise=stepwise)}
        }
      }
      else mod.arima=auto.arima(log(y),d=diff.trend,D=diff.sea,max.p=max.p,max.q=max.q,stationary=stationary.arima,max.Q=max.Q,max.P=max.P,ic="aic",xreg=xreg.arima,stepwise=stepwise)  

    else {mod.arima = arima(log(y), order = c(max.p,diff.trend,max.q), seasonal = list(order=c(max.P,diff.sea,max.Q)), xreg = xreg.arima )}
    arima.AIC = mod.arima$aic
    pred = predict(mod.arima, n.ahead)
    pred.arima = pred$pred
    ic_arima = pred.arima + qnorm(0.975) * cbind(-pred$se, pred$se)


    colnames(ic_arima) = c("lwr", "upr")
    IC.pred.arima = list(lwr = exp(ic_arima[, "lwr"]), upr = exp(ic_arima[, "upr"]))


    pred.arima = exp(pred.arima)
    tss = var(log(y)) * (n - 1)
    res = residuals(mod.arima)
  }
  else {
    if (stepwise)
      if (is.null(xreg.arima)) {
        mod = try(arimaId(y,c(max.p,diff.trend,max.q),list(order=c(max.P,diff.sea,max.Q)),idDiff=FALSE,method="ML",verbose=FALSE),TRUE)
        if (is(mod,"try-error")) { mod.arima=auto.arima(y,d=diff.trend,D=diff.sea,max.p=max.p,max.q=max.q,stationary=stationary.arima,max.Q=max.Q,max.P=max.P,ic="aic",xreg=xreg.arima,stepwise=stepwise)} 
        else {
          index=print.allArima(mod, nshow=1)
          mod.arima = try(arima(y,c(as.integer(index["p"]),as.integer(index["d"]),as.integer(index["q"])),c(as.integer(index["P"]),as.integer(index["D"]),as.integer(index["Q"])),method="ML"),TRUE)
          if (is(mod.arima,"try-error")) { mod.arima=auto.arima(y,d=diff.trend,D=diff.sea,max.p=max.p,max.q=max.q,stationary=stationary.arima,max.Q=max.Q,max.P=max.P,ic="aic",xreg=xreg.arima,stepwise=stepwise)} 
        }
      }
      else mod.arima=auto.arima(y,d=diff.trend,D=diff.sea,max.p=max.p,max.q=max.q,stationary=stationary.arima,max.Q=max.Q,max.P=max.P,ic="aic",xreg=xreg.arima,stepwise=stepwise)  
    else {mod.arima = arima(y, order = c(max.p,diff.trend,max.q), seasonal = list(order=c(max.P,diff.sea,max.Q)),xreg = xreg.arima)}
    arima.AIC = mod.arima$aic
    pred = predict(mod.arima, n.ahead)
    pred.arima = pred$pred
    ic_arima = pred.arima + qnorm(0.975) * cbind(-pred$se,pred$se)


    colnames(ic_arima) = c("lwr", "upr")
    IC.pred.arima = list(lwr = ic_arima[, "lwr"], upr = ic_arima[, "upr"])

    tss = var(y) * (n - 1)
    res = residuals(mod.arima)
  }
   if(negToZero) {
	pred.arima[pred.arima<0]=0
	IC.pred.arima$upr[IC.pred.arima$upr<0]=0
	IC.pred.arima$lwr[IC.pred.arima$lwr<0]=0
	} 
   if(predictInteger) {
	pred.arima=round(pred.arima,0)
	}
	#errori_arima = abs(as.vector(log(validazione_arima) - log(pred.arima[1:fuori])))
                                        #media_errori = mean(errori_arima)
                                        #res=mod.arima[[1]]$residuals
  rss = sum(res^2)
  r2 = 1 - (rss/tss)
                                        #k = length(as.vector(mod.arima$coef))
                                        #N = n - 3
  arima.R2 = r2
  attr(y, "product") = names(product)
  ic.delta = mean(IC.pred.arima$upr - IC.pred.arima$lwr)
  maxJump = max(abs(product[(dim(product)[1]-period.freq+1):dim(product)[1],1]/pred.arima[1:period.freq]-1),na.rm=TRUE)
  VarCoeff=sd(c(as.vector(pred.arima),t(as.vector(product))),na.rm=TRUE)/mean(c(as.vector(pred.arima),t(as.vector(product))),na.rm=TRUE)
  # m=matrix(NA,period.freq, ceiling((length(pred.arima))/period.freq)+1)
  # m[1:(length(pred.arima)+period.freq)]=c(y[dim(y)[1]:(dim(y)[1]-period.freq+1),],pred.arima)
  # m=apply(m,2,mean,na.rm=TRUE)
  # sdJumps = sd(m[-1]/m[-length(m)])
  lista.arima = list(ts.product = y, model = mod.arima, prediction = pred.arima, 
    IC = IC.pred.arima, AIC = arima.AIC, R2 = arima.R2, IC.width = ic.delta, maxJump=maxJump, VarCoeff=VarCoeff,  Residuals = res)
  lista.arima
}

############## best.es()
mod.es <- function(product, n.ahead, period.start, period.freq, n, logtransform.es, stepwise,negToZero=negToZero,predictInteger=predictInteger) {
	
	#occhio qui:
	product[product==0]=1
	
  y = as.vector(product)
  n = max(length(y), dim(y)[1])
  y = ts(y, start = period.start, frequency = period.freq)
                                        # data mancanti mercato
                                        #stima_le = window(y,end=end_stima)
                                        #validazione_le = window(y,start=start_val)
  if (logtransform.es) {
    mod = esId(log(y), keep = 1)
    mod = mod[which(mod$rankAIC == 1), ]
    modle = esFit(log(y), mod$drift, mod$sea, mod$inn)
    pred = try(predict(modle, n.ahead, method = "resample"),TRUE)
    
    if( is(pred,"try-error") ) { 
      return(list(ts.product = y, model = modle, prediction = NA, IC = NA, AIC = NA, R2 = NA, IC.width = NA, VarCoeff =NA, Residuals = NA))
    } else {
      n.par = mod$np
      es.AIC = modle$loglik + 2 * n.par
      pred.modle = exp(unlist( pred[,c("mean"),drop=FALSE][,1]))
      pred.modle[abs(pred.modle) == Inf] = NA
      pred[abs(pred[,"97.5%"]) == Inf ,"97.5%"] =NA			
      pred[abs(pred[,"2.5%"]) == Inf ,"2.5%"] =NA
      IC.pred.modle = list(lwr = exp(pred[, "2.5%"]), upr = exp(pred[,"97.5%"]))
      tss = var(y) * (n - 1)
                                        #errori_le = abs(as.vector(log(validazione_le) - log(pred.modle)))
                                        #media_errori = mean(errori_le)
    }
  }
  else {
    mod = esId(y, keep = 1)
    mod = mod[which(mod$rankAIC == 1), ]
    modle = esFit(y, mod$drift, mod$sea, mod$inn)  
    pred = try(predict(modle, n.ahead, method = "resample"),TRUE)
    if( is(pred,"try-error") ) { 
      return(list(ts.product = y, model = modle, prediction = NA, IC = NA, AIC = NA, R2 = NA, IC.width = NA, VarCoeff =NA,Residuals = NA))
    } else {
      n.par = mod$np
      es.AIC = modle$loglik + 2 * n.par
      pred.modle =unlist( pred[,c("mean"),drop=FALSE][,1])
#############OCCHIO QUI
      pred.modle[abs(pred.modle) == Inf] = NA
      pred[abs(pred[,"97.5%"]) == Inf ,"97.5%"] =NA			
      pred[abs(pred[,"2.5%"]) == Inf ,"2.5%"] =NA
      IC.pred.modle = list(lwr = pred[, "2.5%"], upr = pred[,"97.5%"])
      tss = var(y) * (n - 1)
                                        #errori_le = abs(as.vector(log(validazione_le) - log(pred.modle)))
                                        #media_errori = mean(errori_le)
    }
  }
  if(negToZero) {
	pred.modle[pred.modle<0]=0
	IC.pred.modle$upr[IC.pred.modle$upr<0]=0
	IC.pred.modle$lwr[IC.pred.modle$lwr<0]=0
	}
  if(predictInteger) {
	pred.modle=round(pred.modle,0)
	}
	
  res = residuals(modle)
  rss = sum(res^2)
  r2 = 1 - (rss/tss)
                                        #k = n.par
  es.R2 = r2
  attr(y, "product") = names(product)
  ic.delta = mean(IC.pred.modle$upr - IC.pred.modle$lwr)
  maxJump = max(abs(product[(dim(product)[1]-period.freq+1):dim(product)[1],1]/pred.modle[1:period.freq]-1),na.rm=TRUE)
  VarCoeff=sd(c(as.vector(pred.modle),t(as.vector(product))),na.rm=TRUE)/mean(c(as.vector(pred.modle),t(as.vector(product))),na.rm=TRUE)
  # # m=matrix(NA,period.freq, ceiling((length(pred.modle))/period.freq)+1)
  # # m[1:(length(pred.modle)+period.freq)]=c(y[dim(y)[1]:(dim(y)[1]-period.freq+1),],pred.modle)
  # # m=apply(m,2,mean,na.rm=TRUE)
  # # sdJumps = sd(m[-1]/m[-length(m)])
  lista.es = list(ts.product = y, model = modle, prediction = pred.modle, 
    IC = IC.pred.modle, AIC = es.AIC, R2 = es.R2, IC.width = ic.delta, maxJump=maxJump,  VarCoeff=VarCoeff, Residuals = res)
  lista.es
}

## funzione che calcola in modo automatico il numero delle differenze per l'arima
IDdiff = function(y,period.freq){
  diff.trend = 0
  diff.sea = 0
  v = rep(NA,6)
  v[1] = var(y)
  v[2] = var(diff(y,period.freq))
  v[3] = var(diff(y))
  v[4] = var(diff(diff(y,period.freq)))
  v[5] = var(diff(diff(y,period.freq),period.freq))
  v[6] = var(diff(diff(y)))
  d_sea = c(0,1,0,1,2,0)
  d_trend = c(0,0,1,1,0,2)
  d = cbind(v,d_sea,d_trend)
  if (sum(is.na(v))==0){  
    index = which.min(v)
    diff.trend = d[index,3]
    diff.sea = d[index,2]}
  
  c(diff.trend,diff.sea)
}

## funzione che valuta l'uso del log
## ATTENZIONE possibili valori negativi per le previsioni ed IC
IDlog = function(product,period.start){
  y = as.vector(product)
  y = ts(y, start = period.start, frequency = period.freq)
  mod = esId(y, keep = 1)
  mod = mod[which(mod$rankAIC == 1), ]
  if (mod$sea=="m" | mod$sea=="c/m") logtransform = TRUE
  else logtransform = FALSE
  logtransform
} 

#####################################

.plot.best = function(best, plot.trend =TRUE, color.ic, 
  color.forecast, fies.name, title,filename="modelBest.png", width = width, height = height) {
  
  y = best$ts.product
  period.freq = frequency(y)
  end_serie = end(y)
  period.start = start(y)
  start_pred = .incSampleTime(period.freq = period.freq, now = end_serie)
  
  pred = best$prediction
  ic.lwr = best$IC$lwr
  ic.upr = best$IC$upr
  
  if (!is.ts(pred)) pred = ts(pred,start=start_pred,frequency=period.freq)
  if (!is.ts(ic.lwr)) ic.lwr = ts(ic.lwr,start=start_pred,frequency=period.freq)
  if (!is.ts(ic.upr)) ic.upr = ts(ic.upr,start=start_pred,frequency=period.freq)
  
  p.best = append(as.vector(window(y,end=end_serie)),pred[1])
  p.best = ts(p.best,start=period.start,frequency=period.freq)
  
  y.best = append(as.vector(window(y,end=end_serie)),pred)
  y.best = ts(y.best,start=period.start,frequency=period.freq)
  
  inf = min(ic.lwr,y,na.rm = TRUE)
  sup = max(ic.upr,y,na.rm = TRUE)
  
  bitmap(units="px",filename, width = width, height = height)
  plot(window(p.best, end = start_pred), ylim = c((inf - (inf/4)), 
                                           (sup + (sup/2))), xlim = c(period.start[1], end(pred)[1]), 
       main = title,ylab="y")
  lines(y.best, col = color.forecast[1], pch = "*", lwd = 2)
  lines(ic.lwr, col = color.ic, lwd = 1)
  lines(ic.upr, col = color.ic, lwd = 1)
  points(ic.upr, col = color.ic, cex = 1, pch = "*")
  points(ic.lwr, col = color.ic, cex = 1, pch = "*")
  lines(y, pch = "*", lwd = 2)
  legend(x = period.start[1], y = (sup + (sup/2)), legend = c("Prediction", 
                                                     "Confidence band 95%"), col = c(color.forecast[1], color.ic), 
         lty = 1, lwd = 2, horiz = FALSE, x.intersp = 1)
  
  if (plot.trend) {
    trend.best = try(smooth.spline(y.best),TRUE)
    if(!is(trend.best,"try-error"))  lines(trend.best, col = color.forecast[1], lwd = 1)
  }
  dev.off()
}

.plot.all = function(model, color.forecast, plot.trend = TRUE, fies.name, title,filename="modelAll.png",width = width, height = height) {
  y = model[[model$BestModel]]$ts.product
  period.freq = frequency(y)
  end_serie = end(y)
  period.start = start(y)
  start_pred = .incSampleTime(period.freq = period.freq, now = end_serie)
  
  pred=list(pred.lm = model[["LinearModel"]]$prediction,
    pred.arima = model[["Arima"]]$prediction,
	pred.es = model[["ExponentialSmooth"]]$prediction,
    pred.trend = model[["Trend"]]$prediction,
    pred.mean = model[["Mean"]]$prediction)
  names(pred)=c("LinearModel","Arima","ExponentialSmooth","Trend","Mean")
  
  
  pred = lapply(pred, function(pr){ if (!is.null(pr))  if (!is.ts(pr)) pr = ts(pr, start = start_pred, frequency = period.freq); pr})
  
  
                                        #concateno la prima prediction
  
  p = lapply( pred, function(pr) {pr=append(as.vector(window(y, end = end_serie)), pr)
                                  pr= ts(pr, start = period.start, frequency = period.freq)})
  
  yy=list()
  for(i in which(sapply(p,function(yyy)!is.null(yyy) ))){
    if(!is.null(p[i])){ 
      yy[[i]]=append(as.vector(window(y, end = end_serie)), p[[i]])
      yy[[i]] = ts(yy[[i]], start = period.start, frequency = period.freq)
    }
  }

  inf = min(unlist(pred), y,na.rm = TRUE)
  sup = max(unlist(pred), y,na.rm = TRUE)
  
  
                                        #bmp(file=fies.name)
  bitmap(units="px",filename, width = width, height = height)
  plot(y, ylim = c((inf - (inf/4)), (sup + (sup/2))), xlim = c(period.start[1], end(p[[model$BestModel]])[1]), 
       lwd = 2, main = title,ylab="y")
 
 for(i in names(pred)[which(sapply(pred,function(pp)!is.null(pp) ))]){
    lines(window(p[[i]], start = end_serie) , col = color.forecast[i], pch = "*", cex = 2, lwd = 2)
  }
  
  legend(x = period.start[1], y = (sup + (sup/2)), legend = c("Linear Model","Arima" , "Exp. Smooth" , "Trend" ,"Mean" )[which(sapply(pred,function(pp)!is.null(pp) ))], 
         col = color.forecast[names(color.forecast)[which(sapply(pred,function(pp)!is.null(pp) ))]], lty = 1, lwd = 2, horiz = FALSE, x.intersp = 1)
  if (plot.trend) {
    for(i in which(sapply(pred,function(yyy)!is.null(yyy) ))){
      trend = try(smooth.spline(yy[[i]]),TRUE)
      if(!is(trend,"try-error")) lines(trend, pch = "*", col = color.forecast[i], lwd = 1)
    }
  } 
dev.off()
  }

####################### decidere per il nome grafico e filename

## possibili nomi per i file
                                        #paste('All model-',names(product),'.bmp',sep='')
                                        #paste('Best model for--',names(product),'.bmp',sep='')
                                        #paste('Best model for--',names(product),'.bmp',sep='')

## best ÃƒÂ¨ la il risultato fornito da ltp una lista che contiene il model migliore
plot.ltp = function(model, plot.try.models = c("best", 
                             "all"), color.forecast = NULL, color.ic = "red", 
  plot.trend = FALSE, title = "Time Series", filename,width = width, height = height) {
  
  if(is.null(color.forecast)) {
	color.forecast=c("green", "red", "blue","gray","black")
	names(color.forecast)=c("LinearModel","Arima","ExponentialSmooth","Trend","Mean")
	}
  
  for (i in plot.try.models) {
    if (i == "all") 
      .plot.all(model = model, color.forecast = color.forecast, 
                plot.trend = plot.trend, title = title,filename=filename, width = width, height = height)
    if (i == "best") 
      .plot.best(best = model[[model$BestModel]], color.ic = color.ic, 
                 plot.trend = plot.trend, color.forecast = color.forecast[model$BestModel], 
                 title = title, filename = filename,width = width, height = height)
  }
}

###################################################
## # crea report

ltp.HTMLreport <- function(obj, keys, value, value.description, param, html.form.eval="", directory=NULL, width=1000, height=600) {
  library(R2HTML)
  library(hwriter)
  
  if(is.null(directory)) directory =  paste(.GetItemPath(keys,project.path), "/",paste("report-", value.description, sep = "") , sep = "")
  dir.create(directory, showWarnings = FALSE, recursive = TRUE)


  
  html.filename = file.path(directory, "summary.html")

  
  title = paste("Strategico: Long Term Prediction for ", .GetItemName(keys), " - ", value.description, sep = " ")
                                        #ReporTable = data.frame(model = as.character(rep("--", 5)),AIC = as.character(rep("--", 5)),R2 = as.character(rep("--", 5)),IC.whidth = as.character(rep("--", 5)),maxJump = as.character(rep("--", 5)), selected=as.character(rep("", 5)))
  ReporTable = cbind(matrix("--",5,6),"")
  colnames(ReporTable) = c("model", "R2","AIC","IC.width","maxJump","VarCoeff","selected")
  rownames(ReporTable) = c("LinearModel", "Arima", "ExponentialSmooth","Trend","Mean")
                                        #ReporTable[, 1] <- as.character(ReporTable$model)
                                        #as.character(obj$LinearModel$model$call[2])
                                        #as.character(attributes(obj$LinearModel$model$call[[2]])$variables[2])
                                        #	gsub("~","=",gsub("stima$qta","Y",as.character(obj$LinearModel$model$call[2]),fixed=TRUE))
  
  if(!is.null(obj$ExponentialSmooth)) {
    terms=sapply(c("drift","seasonality"),
      function(compon){ if(obj$ExponentialSmooth$model[compon]=="none") return() 
                        compon})
    terms=terms[!sapply(terms,is.null)] 
    
    es.string=paste( "level",sep="+", paste(terms,collapse=ifelse(length(grep("multiplicative",obj$ExponentialSmooth$model["seasonality"])>0),"*","+")))
  }
  
  
  ReporTable[, 1] = c(ifelse(is.null(obj$LinearModel),"--",	gsub("~","=",gsub("stima$qta","y",as.character(obj$LinearModel$model$call[2]),fixed=TRUE))),#paste("Y=",paste(attributes(obj$LinearModel$model$call[[2]])$term.labels,collapse="+"),sep="")), 
              ifelse(is.null(obj$Arima),"--",ifelse(length(obj$Arima$model$coef)==0,"-constant-",paste(obj$Arima$model$series,"=",paste(names(obj$Arima$model$coef), collapse = "+"),sep=""))), 
              ifelse(is.null(obj$ExponentialSmooth),"--", es.string ),
              ifelse(is.null(obj$Trend),"--",paste("y=",paste(attributes(obj$Trend$model$call[[2]])$term.labels,collapse="+"),sep="")),
              ifelse(is.null(obj$Mean),"--",paste("y=",paste(attributes(obj$Mean$model$call[[2]])$term.labels,collapse="+"),sep="")) )
  temp=rbind(unlist(obj$LinearModel[c( "R2","AIC", "IC.width","maxJump","VarCoeff")]), unlist(obj$Arima[c( "R2", "AIC","IC.width","maxJump","VarCoeff")]), 
  unlist(obj$ExponentialSmooth[c("R2", "AIC", "IC.width","maxJump","VarCoeff")]),unlist(obj$Trend[c("R2", "AIC", "IC.width","maxJump","VarCoeff")]),unlist(obj$Mean[c("R2", "AIC", "IC.width","maxJump","VarCoeff")]))
  colnames(temp)= c("R2", "AIC", "IC.width","maxJump","VarCoeff")
  

  temp[,"R2"]=round(temp[,"R2"],4)	
  temp[,"AIC"]=round(temp[,"AIC"],2)
  temp[,"IC.width"]=round(temp[,"IC.width"],0)
  temp[,"maxJump"]=round(temp[,"maxJump"],3)
  temp[,"VarCoeff"]=round(temp[,"VarCoeff"],3)

  ReporTable[which(!(ReporTable[,1]=="--")),c("R2", "AIC", "IC.width","maxJump","VarCoeff")] = as.matrix(temp)
  ReporTable=as.data.frame(ReporTable)
  levels(ReporTable$selected)=c("","BEST")
  ReporTable[obj$BestModel,"selected"]="BEST"
  
### plot SELECTED model
                                        # Write graph to a file
  plot.ltp(obj, plot.try.models = c("best"), color.forecast = NULL, color.ic = "orange", plot.trend = FALSE, title = obj$BestModel ,filename=file.path(directory, "best_model.png"),width = width, height = height)
                                        #plot.ts(data, main = paste(project.path, ':', names(obj)))
  
  
### plot ALL models
                                        # Write graph to a file
  plot.ltp(obj, plot.try.models = c("all"), color.forecast = NULL, color.ic = "orange", plot.trend = FALSE, title = "All Predictors" ,filename=file.path(directory, "all_models.png"),width = width, height = height)
    
  text = paste("<html>\n<head>\n<title>", title, "</title>\n</html>\n<body>\n<h1>", 
    title, "</h1><a href=\"http://code.google.com/p/strategico/wiki/LTP\"/>Quick Help</a>",

    "<h2>Best Model</h2>Criterion:",obj$criterion,", and MaxPredRatioNOTGreaterThan < ",obj$criterionExcludeMaxGreaterThan,",<br><img src=\"best_model.png\" />\n<h2>All Models </h2>\n<img src=\"all_models.png\" />\n",

  
 hwrite(ReporTable), sep = "")
  cat(text, append = FALSE, file = html.filename)

  

  #html.form.eval = GetStrHTMLformEvalItem(project.path, .GetItemPath(keys), value, param)
  cat(html.form.eval, append = TRUE, file = html.filename)  

  notNA <- sapply(c("LinearModel", "Arima", "ExponentialSmooth","Trend","Mean"), 
                  function(i) if(!is.null(obj[[i]])) ( !is.null(obj[[i]]$Residuals))&(!any(is.na(obj[[i]]$Residuals))) else FALSE )
  
    for (modType in names(notNA[notNA])) {




    residPlot = paste("resid_", modType,".png", sep = "")
    bitmap(units="px",file.path(directory, residPlot), width = width * 0.6, height = height * 0.6 )
    plot(obj[[modType]]$Residuals, type = "p", col="blue", main = paste("Residuals of ", modType, sep = ""),ylab="Residuals")
    abline(0, 0, col="red")
    dev.off()
    text = paste("\n<h3> Model: ", modType, "</h3>\n<img src=\"", residPlot, "\" />", sep = "")
    cat(text, append = TRUE, file = html.filename)
    HTML(file = html.filename, report(obj[[modType]]$model,obj[[modType]]))
  }

  text = "<h2>Recorded and Predicted Data</h2>"
  cat(text, append = TRUE, file = html.filename)
  
  y = obj$values
  names(y)="Historical values"
  #HTML(file = html.filename, y,digits=12)
  cat(hwrite(y), append = TRUE, file = html.filename) 
  
  if(!is.null(obj$BestModel)){ 	 
    pred = as.data.frame(obj[[obj$BestModel]]$prediction,ncol=1)
    period.freq = frequency(obj[[obj$BestModel]]$ts.product)
    end_serie = end(obj[[obj$BestModel]]$ts.product)
    pred.names = sapply(1:dim(pred)[1],function(x) paste(.incSampleTime(period.freq = period.freq, now = end_serie,increment =x),collapse="-"))
    rownames(pred)=pred.names
    colnames(pred)="Predicted values"

    ##HTML(file = html.filename, pred,digits=12)
    cat(hwrite(pred), append = TRUE, file = html.filename) 
  }

  cat("</body> </html>", append = TRUE, file = html.filename)  

}

"report" <- function(model,list, ...) {
  UseMethod("report")
}

"report.lm" <- function(model,list, ...) { 
  summary(model)
}


"report.Arima" <- function(model,list, ...) {
  AIC = list$AIC
  R2 = list$R2
  Coefficients = cbind(Estimate = model$coef, Std.Error=matrix(sqrt(diag(model$var.coef))))
  colnames(Coefficients) = c("Estimate", "Std. Error")
  if (dim(Coefficients)[1]==0) Coefficients=rbind(Coefficients,NA)
  rr=t(as.matrix(summary(model$residuals)))
  list(Call = model$call, residuals = list("Residuals",summary(model$residuals)), 
       Coefficients = list("Coefficients",Coefficients), AIC = paste("AIC:",round(AIC, 4), sep = " "), 
       R2 = paste("R2:",R2, sep = " "), 
       Residuals = paste("Residuals standard error:", round(sqrt(model$sigma2), 4), sep = " "))
}


"report.expSmoothingFit" <- function(model,list, ...) {
  ## ritornare una list eventualmente con table come elementi simile a quella per Arima
  equations = .eq.es(model) 
  list(Drift= paste("type Trend:",model$drift, sep = " "),
       Seasonality=paste("type Seasonality:",model$seasonality, sep = " "),
       ## Innovation=paste("Innovation:",model$innovation, sep = " "),
       alpha=paste("Parameter Smoothing Level:",round(model$par[1],4), sep = " "),
       beta=paste("Parameter Smoothing Seasonality:",round(model$par[4],4), sep = " "),
       gamma=paste("Parameter Smoothing Trend:",round(model$par[3],4), sep = " "),
       R2=paste("R2:",round(list$R2,4), sep = " "),AIC=paste("AIC:",round(list$AIC,3), sep = " "),
       paste("Residuals standard error:",round(sqrt(model$sigma2), 2), sep = " "),
       list(paste("Recursive equations:",sep = " "),
			paste(equations[1]),paste(equations[2]),paste(equations[3]))
		)
  
  
  
  ## se la stagionalita' moltiplicativa ed è presente trend:
  ## y(t+1) = level(t) + trend(t)*stagionalità(t)
  
  ## dove 
  ## level(t) = alpha * y(t) + (1-alpha)*(level(t-1) + trend(t-1))
  ## trend(t) = beta * (level(t) - level(t-1)) + (1-beta) * trend(t-1)
  ## stag.(t) = gamma * ()
  
  ## se la stagionalità è adittiva ed è presente trend:
  
  ## y(t+1) = level + trend + stagionalità
  
  ## alpha(parametro del livello) è il parametro che modella la memoria del mio modello
  ## infatti alpha determina il peso dato alle osservazioni
  ## un alpha prossimo a 1 mi dara peso solo alle ultime osservazioni
  ## un alpha molto basso dara peso significante anche a osservazioni passate lontane dall'ultima osservata
  ## per avere un idea:
  ## k = 1:10
  ## par(mfrow=c(2,1))
  ## plot((1-0.3330807)^k,type="l")
  ## plot((1-0.9)^k,type="l")
  
}




.eq.es = function(model) {
 model$par[c(1,3,4)] =round(model$par[c(1,3,4)],4)
  trend = model$drift
  sea = model$seasonality
  if ((trend=="additive") | (trend=="c/additive") | (trend=="d/additive"))  {trend="additive"}
    else { if ((trend=="multiplicative") | (trend=="c/multiplicative")) trend="multiplicative"} 

  if ((sea=="additive") | (sea=="c/additive"))  {sea="additive"}
    else { if ((sea=="multiplicative") | (sea=="c/multiplicative")) sea="multiplicative"} 
  
  if ((trend=="none")&(sea=="none")) {
     eq1 = paste("level(t)=","(1-",model$par[1],") * level(t-1) + ",model$par[1]," * y(t)",sep="")
     eq2 = "Trend none"
     eq3 = "Seasonality none" }
    else if ((trend=="additive")&(sea=="none")) {
              eq1 = paste("level(t)=","(1-",model$par[1],") * (level(t-1) + drift(t-1)) + ",model$par[1]," * y(t)",sep="")
              eq2 = paste("drift(t)=","(1-",model$par[3],") * drift(t-1) + ",model$par[3]," * (level(t) - level(t-1))",sep="")
              eq3 = "Seasonality none"  }
            else if ((trend=="multiplicative")&(sea=="none")) {
                      eq1 = paste("level(t)=","(1-",model$par[1],") * level(t-1) * drift(t-1) + ",model$par[1]," * y(t)",sep="")
                      eq2 = paste("drift(t)=","(1-",model$par[3],") * drift(t-1) + ",model$par[3]," * (level(t) / level(t-1))",sep="")
                      eq3 = "Seasonality none"  }
                    else if ((trend=="none")&(sea=="additive")) {
                              eq1 = paste("level(t)=","(1-",model$par[1],") * level(t-1) + ",model$par[1]," * (y(t) - sea(t-f))",sep="")
                              eq2 = "Trend none"
                              eq3 = paste("sea(t)=","(1-",model$par[4],") * sea(t-f) + ",model$par[4]," * (y(t) - level(t-1))",sep="")  }
                            else if ((trend=="none")&(sea=="multiplicative")) {
                                      eq1 = paste("level(t)=","(1-",model$par[1],") * level(t-1) + ",model$par[1]," * (y(t) / sea(t-f))",sep="")
                                      eq2 = "Trend none"
                                      eq3 = paste("sea(t)=","(1-",model$par[4],") * sea(t-f) + ",model$par[4]," * (y(t) / level(t-1))",sep="")  }                                   
            
            
  if ((trend=="additive")&(sea=="additive")) {
     eq1 = paste("level(t)=","(1-",model$par[1],") * ((level(t-1) + drift(t-1)) + ",model$par[1]," * (y(t) - sea(t-f))",sep="")
     eq2 = paste("drift(t)=","(1-",model$par[3],") * drift(t-1) + ",model$par[3]," * (level(t) - level(t-1))",sep="")
     eq3 = paste("sea(t)=","(1-",model$par[4],") * sea(t-f) + ",model$par[4]," * (y(t) - level(t-1) - drift(t-1))",sep="") }
  
  if ((trend=="additive")&(sea=="multiplicative")) {
     eq1 = paste("level(t)=","(1-",model$par[1],") * ((level(t-1) + drift(t-1)) + ",model$par[1]," * (y(t)/sea(t-f))",sep="")
     eq2 = paste("drift(t)=","(1-",model$par[3],") * drift(t-1) + ",model$par[3]," * (level(t) - level(t-1))",sep="")
     eq3 = paste("sea(t)=","(1-",model$par[4],") * sea(t-f) + ",model$par[4]," * (y(t) / (level(t-1) - drift(t-1)) )",sep="") }  
     
  if ((sea=="additive")&(trend=="multiplicative")) {
     eq1 = paste("level(t)=","(1-",model$par[1],") * level(t-1) * drift(t-1) + ",model$par[1]," * (y(t) - sea(t-f))",sep="")
     eq2 = paste("drift(t)=","(1-",model$par[3],") * drift(t-1) + ",model$par[3]," * (level(t) / level(t-1))",sep="")
     eq3 = paste("sea(t)=","(1-",model$par[4],") * sea(t-f) + ",model$par[4]," * (y(t) - level(t-1) - drift(t-1))",sep="") }

  if ((trend=="multiplicative")&(sea=="multiplicative")) {
     eq1 = paste("level(t)=","(1-",model$par[1],") * level(t-1) * drift(t-1) + ",model$par[1]," * (y(t) / sea(t-f))",sep="")
     eq2 = paste("drift(t)=","(1-",model$par[3],") * drift(t-1) + ",model$par[3]," * (level(t) / level(t-1))",sep="")
     eq3 = paste("sea(t)=","(1-",model$par[4],") * sea(t-f) + ",model$par[4]," * (y(t) / (level(t-1) - drift(t-1)) )",sep="") }
 c(eq1,eq2,eq3)
 }


BuildOneRowSummary <- function(keys, model, manual.model, param, return.code) {
	stats=as.list(rep(NA,16))
	names(stats)=c("BestModel","R2","AIC","ICwidth","maxJump","VarCoeff","Points","NotZeroPoints","LastNotEqualValues",
	"MeanPedicted","MeanValues","MeanPedictedRatioMeanValues","SdPedictedRatioSdValues",
	"BestAICNoOutRangeExclude","BestICNoOutRangeExclude","Timestamp")

	#mean values (ie observed data)
	stats["MeanValues"]=mean(model$values,na.rm=TRUE)
	#nunb of points (observations)
	stats["Points"]=nrow(model$values)
	#non zero values
	stats["NotZeroPoints"]=ifelse(dim(model$values)[1]==0,0, sum(model$values!=0))

	if(!is.null(model$BestModel)){
		stats[c("R2","AIC","maxJump","VarCoeff")]=round(unlist(model[[model$BestModel]][c("R2","AIC","maxJump","VarCoeff")]),4)
		stats["ICwidth"] = round(model[[model$BestModel]][["IC.width"]],0)

		#find (che cum sum of) not equal (ie constant) consecutive values
		temp=cumsum((model$values[-1,]-model$values[-nrow(model$values),])==0)
		#length of last not-constant consecutives serie of values
		stats["LastNotEqualValues"]=sum(temp==max(temp))-1
		
		#mean predicted
		stats["MeanPedicted"]=mean(model[[model$BestModel]]$prediction,na.rm=T)
		#mean predicted over mean values (ie observed data)
		stats["MeanPedictedRatioMeanValues"]=stats[["MeanPedicted"]]/stats[["MeanValues"]]
		#and rounding
		stats[c("MeanPedicted","MeanValues","MeanPedictedRatioMeanValues")]=lapply(stats[c("MeanPedicted","MeanValues","MeanPedictedRatioMeanValues")],round,3)
		#sd predicted over sd values (ie observed data)
		stats["SdPedictedRatioSdValues"]=round(sd(model[[model$BestModel]]$prediction,na.rm=T)/sd(model$values),3)
		
		#Best Model if not exclusion criterion were performed
		st=names(which.min(unlist(lapply(model[c("Mean","Trend","LinearModel","ExponentialSmooth","Arima")],function(x) x$AIC))))
		stats["BestAICNoOutRangeExclude"]=ifelse(is.null(st),"None",st)
		st=names(which.min(unlist(lapply(model[c("Mean","Trend","LinearModel","ExponentialSmooth","Arima")],function(x) x$IC.width))))
		stats["BestICNoOutRangeExclude"]=ifelse(is.null(st),"None",st)
		#note: stat is changed from numeric to string
		stats["BestModel"] = model$BestModel
	}

	stats["Timestamp"] = as.character(Sys.time())
	stats["ManualModel"] = manual.model
	stats["Parameters"] = BuildParamString(param)
	stats["ReturnCode"] = return.code
	stats["Run"] = 0
	
	#clean out the (possible) Inf values
	stats= lapply(stats,function(x) ifelse(is.numeric(x) & (!is.finite(x)), NA,x))
	
	summ=data.frame( t(keys), as.data.frame(stats))
}