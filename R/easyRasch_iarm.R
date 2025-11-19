# functions in this file are copied from the `iarm` package and modified to fix
# issues.
# Original code: https://github.com/muellermarianne/iarm/

#' Temporary fix for upstream bug in `iarm::person_estimates()`
#'
#' To get `RIscoreSE()` working properly for cases with theta range up til
#' c(-10,10).
#'
#' code from package `iarm`
#' <https://github.com/cran/iarm/blob/master/R/Person-Fit.R>
#'
#' @param object Output from PCM() or RM()
#' @param properties All properties or not
#' @param allperson All respondents or not
#' @export

RI_iarm_person_estimates <- function(object, properties = F, allperson = F,
                                     sthetarange = c(-10, 10)){
  if (!any("Rm"%in%class(object),class(object)%in%c("raschmodel","pcmodel"))) stop("object must be of class Rm, raschmodel or pcmodel!")
  if(class(object)[1]=="pcmodel") object$model <- "pcmodel"
  if(class(object)[1]=="raschmodel") object$model <- "raschmodel"
  if (object$model%in%c("raschmodel","pcmodel")) {X <- object$data
  } else {X <- object$X
  }
  if (object$model%in%c("RM","raschmodel")) {
    k <- dim(X)[2]
    if (object$model == "RM") coeff <- (-1)*coef(object)
    else coeff <- itempar(object)
    m <- k
    respm <- rbind(rep(0, k), lower.tri(matrix(1, k, k)) + diag(k))
  } else {
    if (object$model == "PCM"){
      coeff <- thresholds(object)[[3]][[1]][, -1]- mean(thresholds(object)[[3]][[1]][, -1], na.rm=T)
    } else {
      coeff <- coef(threshpar(object),type="matrix")
    }
    k <- dim(X)[2]
    mi <- apply(X, 2, max, na.rm = TRUE)
    m <- sum(mi)
    respm <- matrix(0, ncol = k, nrow = m + 1)
    respm[, 1] <- c(0:mi[1], rep(mi[1], nrow(respm) - mi[1] - 1))
    for (i in 2:k) respm[, i] <- c(rep(0, cumsum(mi)[i - 1] + 1), 1:mi[i], rep(mi[i], nrow(respm) - cumsum(mi)[i]  -1))
  }
  if (object$model=="pcmodel"){
    mode <- "PCM"
  } else {
    if (object$model=="raschmodel"){
      mode  <- "RM"
    } else {
      mode <- object$model
    }
  }
  mm <- cbind(0:m, RI_iarm_persons_mle(respm, coeff, model=mode, type = "MLE" )[, 1],
              RI_iarm_persons_mle(respm, coeff, model=mode, type="WLE")[,1])
  rownames(mm) <- rep(" ", m + 1)
  colnames(mm) <- c("Raw Score", "MLE", "WLE")
  if (allperson){
    properties <- F
    rv <- rowSums(X, na.rm = TRUE)
    mm <- mm[rv+1,]
    mm
  } else {
    if (properties == F) {
      mm
    } else {
      if (object$model%in%c("RM","raschmodel")){
        koeff <- as.list(coeff)
      } else {
        koeff <- lapply(as.list(as.data.frame(t(coeff))), function(x) cumsum(na.omit(x)))
      }
      gr <- elementary_symmetric_functions(koeff)[[1]]
      s.theta <- function(r){
        function(x){
          ((exp(x*(0:m))*gr)/as.vector(exp(x*(0:m))%*%gr))%*%(0:m) - r
        }
      }
      if (object$model%in%c("pcmodel","raschmodel")) mm[1, 2] <- NA else  mm[1, 2] <- person.parameter(object)$pred.list[[1]]$y[1]
      try(mm[1, 2] <- uniroot(s.theta(0.25), sthetarange)$root)
      mm[m + 1, 2] <- uniroot(s.theta(m - 0.25), sthetarange)$root # this is where the change was made
      rvec = 0:m
      pers_prop <- function(x, persons){
        pr <- (exp(x[2]*rvec)*gr)/as.vector(exp(x[2]*rvec)%*%gr)
        bias <- pr%*%persons - x[2]
        sem <- sqrt((persons - as.vector(pr%*%persons))^2%*%pr)
        rsem <- sqrt((persons - x[2])^2%*%pr)
        scoresem <- sqrt((rvec- x[1])^2%*%pr)
        c(SEM = sem, Bias = bias, RMSE = rsem, Score.SEM = scoresem)
      }
      result <- list(cbind(mm[, 1:2],t(apply(mm[, c(1, 2)], 1, pers_prop, persons = mm[, 2]))),
                     cbind(mm[, c(1,3)], t(apply(mm[, c(1, 3)], 1, pers_prop, persons = mm[, 3]))))
      result
    }
  }
}

#' Temporary fix for upstream bug in `iarm::person_estimates()`
#'
#' To get `RIscoreSE()` working properly for cases with theta range up til
#' c(-10,10).
#'
#' code from package `iarm`
#' <https://github.com/cran/iarm/blob/master/R/Person-Fit.R>
#'
#' @param respm temp
#' @param thresh temp
#' @param model temp
#' @param theta temp
#' @param type temp
#' @param extreme temp
#' @param maxit temp
#' @param maxdelta temp
#' @param tol temp
#' @param maxval temp
#' @export

RI_iarm_persons_mle <- function (respm, thresh, model=c("RM","PCM"), theta = rep(0, dim(respm)[1]),
                                 type = c("MLE","WLE"), extreme=TRUE, maxit = 20, maxdelta = 3, tol = 1e-04, maxval = 9) {
  # thresh Matrix mit  Schwellenwerten  oder betas
  n <- dim(respm)[1]
  k <- dim(respm)[2]
  rv <- rowSums(respm, na.rm = TRUE)
  mode <- match.arg(model)
  typ <- match.arg(type)
  cll.rasch <- function(theta){
    ksi   <- exp(theta)
    mm <- outer(ksi,1/exp(thresh))
    mm[is.na(respm)] <- 0
    dll <- rv - rowSums(mm/(1+mm))
    d2ll <- - rowSums(mm/(1+mm)^2)
    d3ll <- 0
    if (typ=="WLE") {
      d3ll <- - rowSums((mm*(1-mm))/(1+mm)^3)
      if (extreme==FALSE){
        d3ll[rv==0] <- 0
        d3ll[rv==maxr] <- 0
      }
    }
    list(dll=dll,d2ll=d2ll,d3ll=d3ll)
  }
  cll.pcm <- function(theta){
    dlogki <-function(i) {
      mmn <- exp(outer(theta,1:mi[i]) + matrix(psi.l[[i]],ncol=mi[i],nrow=n,byrow=T))
      kd <- 1 + rowSums(mmn)
      kd1 <- rowSums(matrix(1:mi[i],ncol=mi[i],nrow=n,byrow=T)*mmn)
      kd2 <- rowSums(matrix((1:mi[i])^2,ncol=mi[i],nrow=n,byrow=T)*mmn)
      kd3 <- rowSums(matrix((1:mi[i])^3,ncol=mi[i],nrow=n,byrow=T)*mmn)
      cbind(dlli=kd1/kd, d2lli=kd2/kd -(kd1/kd)^2, d3lli=-kd3/kd + 3*kd2*kd1/(kd^2) -2*(kd1/kd)^3)
    }
    mm <- sapply(1:k,dlogki)
    mm[is.na(rbind(respm,respm,respm))] <- 0
    dll <- rv -rowSums(mm[1:n,])
    d2ll <- -rowSums(mm[(n+1):(2*n),])
    d3ll <- 0
    if (typ=="WLE") {
      d3ll <- rowSums(mm[(2*n+1):(3*n),])
      if (extreme==FALSE){
        d3ll[rv==0] <- 0
        d3ll[rv==maxr] <- 0
      }
    }
    list(dll=dll,d2ll=d2ll,d3ll=d3ll)
  }
  iter <- 1
  conv <- 1
  if (mode=="RM") {
    maxr <- apply(respm,1,function(x) sum(!is.na(x)))
    clog <- cll.rasch
  }
  if (mode=="PCM") {
    thresh.l <- apply(thresh,1,function(x) as.vector(na.omit(x)), simplify=F)
    psi.l <- lapply(thresh.l, function(x){(-1)*cumsum(x)})
    mi <- sapply(psi.l, length)
    m <- sum(mi)
    maxr <- apply(respm,1,function(x) sum(mi[!is.na(x)]))
    clog <- cll.pcm
  }

  while ((conv > tol) & (iter <= maxit)) {
    theta0 <- theta
    fn <- clog(theta)
    delta <- -fn[[1]]/fn[[2]]
    if (typ=="WLE") {
      delta <- -fn[[1]]/fn[[2]] - fn[[3]]/(2 * fn[[2]]^2)
    }
    maxdelta <- maxdelta/1.05
    delta <- ifelse(abs(delta) > maxdelta, sign(delta) * maxdelta, delta)
    theta <- theta + delta
    theta <- ifelse(abs(theta) > maxval, sign(theta) * maxval, theta)
    conv <- max(abs(theta - theta0))
    iter <- iter + 1
  }
  se <- sqrt(abs(-1/fn[[2]]))
  se <- ifelse(abs(theta) == maxval, NA, se)
  theta <- ifelse(theta == maxval, Inf, ifelse(theta == -maxval, -Inf, theta))
  res <- structure(data.frame(est = theta, se = se), model = mode, type = typ)
  return(res)
}


#' Item Characteristic Curves
#'
#'Plots Item Characteristic Curves for dichotomous and polytomous items. The plot can display observed scores as
#'total scores (method="score") or as average scores within adjacent class intervals (method="cut").
#'Class intervals can be useful when the sample size is not large enough to contain an adequate
#'number of respondents with the same total score for each possible total score. The function includes
#'the option to plot observed scores according to values of an exogenous variable to evaluate differential item functioning
#'(dif="yes").
#'
#' @param data An object of class "data.frame" containing the items (include all items present in the model).
#' The variables need to be numeric.
#' @param itemnumber A numeric vector indicating the columns of the data (the items) which ICCs are going to be plotted.
#' Maximum of four items per plot.
#' @param pallete An object of class "character". Choose a pre-made color pallete from package RColorBrewer.
#' Only available for dif="no".
#' @param xticks A numeric scalar. Specify x-axis tick values.
#' @param yticks A numeric scalar. Specify y-axis tick values.
#' @param thetain A numeric scalar. Specify minimum theta values for person parameters.
#' @param thetaend A numeric scalar. Specify maximum theta values for person parameters.
#' @param method The method for displaying observed scores. Choose "score" to plot total scores.
#' Choose "cut" to plot class intervals.
#' @param grid Chooses whether the background grid should be displayed. Options are "yes" or "no".
#' @param cinumber A numeric scalar. The number of adjacent class intervals in which participants will be divided.
#' Notice that the number of class intervals cannot be higher than the number of total scores.
#' @param itemdescrip A character vector indicating the description of the plotted items. Maximum of four descriptions
#' (one description per item plotted).
#' @param axis.rumm Configures whether the plot should display the entire trait range or solely the trait range close
#' to the observed scores (similar to private software RUMM2030). Options are "yes" or "no".
#' @param dif Configures whether the observed scores will be plotted according to values of an exogenous variable
#' to evaluate differential item function. Options are "yes" or "no".
#' @param difvar Chooses the variable which will be used to evaluate differential item functioning. Only
#' necessary when dif="yes".
#' @param diflabels A character vector indicating the labels to values of the variable chosen to evaluate differential item functioning.
#' Only necessary when dif="yes".
#' @param difstats Displays the partial gamma coefficient to indicate the magnitude of differential item
#' functioning. Options are "yes" or "no". Only necessary when dif="yes".
#' @param title A character vector. The title of the plot.
#' @param icclabel Displays the labels of Expected Item Score and Observed Item Score. Options are "yes"
#' or "no".
#' @param xaxistitle A character vector. The x-axis title.
#' @param yaxistitle A character vector. The y-axis title.
#' @importFrom psychotools pcmodel raschmodel personpar
#' @importFrom Hmisc cut2
#' @import ggplot2
#' @importFrom gridExtra grid.arrange
#' @importFrom stats aggregate predict
#' @importFrom utils capture.output
#' @export
#' @author Pedro Henrique Ribeiro Santiago \email{pedro.ribeirosantiago@@adelaide.edu.au}, Marianne Mueller

RI_iarm_ICCplot <- function(data, itemnumber, pallete='Paired', xticks=1.0, yticks=0.5,
                    thetain=-6.000, thetaend=6.000, method="score", grid="yes", cinumber=6, itemdescrip="",
                    axis.rumm="yes", dif="no", difvar=NA, diflabels=c("Group1", "Group 2", "Group 3", "Group 4", "Group5"),
                    difstats="yes", title="Item Characteristic Curve", icclabel="yes",
                    xaxistitle="Theta", yaxistitle="Item Score") {

  pltC <- function() {

    if (grid=="no") {
      background <- element_blank()
      gridy <- element_blank()
      gridx <- element_blank()
      panelgrid <- element_blank()
    } else {
      background <- element_rect(fill = "white", colour="black")
      gridy <- element_blank()
      gridx <- element_blank()
      panelgrid <- element_line(colour="grey87", linewidth=0.25)
    }

    if (icclabel=="yes") {
      icclabels <- "bottom"
    } else {icclabels <- "none"}
    xpos <- ypos <- annotateText <- hjustvar <- vjustvar <- NULL
    annotations <- data.frame(
      xpos = c(-Inf,-Inf,Inf,Inf),
      ypos =  c(-Inf, Inf,-Inf,Inf),
      annotateText = itmn,
      hjustvar = c(-0.5) ,
      vjustvar = c(3.0))

    if (any(lapply(data,class)=="character")==TRUE) {
      stop(' "Input variables must be numeric" ')
    } else
      if (any(lapply(data,class)=="factor")==TRUE) {
        stop(' "Input variables must be numeric" ')
      }
    else {}

    seqres <- NA
    for (i in 1:ncol(data)) {
      seqres[i] <- all(abs(diff(sort(unique(data[,i])))) == 1)
      seqres
    }
    if (any(seqres=="FALSE")==TRUE) {
      stop(' "You need to provide the number of responses for all items categories.
              If there were zero responses to one category, please include this information in the data" ')
    } else {}

    maxr <- max(data, na.rm=TRUE)
    minr <- min(data, na.rm=TRUE)
    if(minr>0) {data2 <- (data - minr)} #Checks whether the minimum value is 0
    if(minr==0) {data2 <- data}
    else {}
    adat <- as.data.frame(data2)
    adat <- adat[complete.cases(adat),]

    cati <- NA
    for (i in 1:ncol(adat)) {
      cati[i] <- max(adat[,i])
      mxsc <- sum(cati)
    } #Calculates the number of categories

    ttsc <- rowSums(adat, na.rm=TRUE)
    if (ncol(adat)>2) {
      rtsc <- rowSums(adat[,-itmc], na.rm=TRUE)
    } else {
      rtsc <- adat[,-itmc]
    }
    adat <- cbind(adat, ttsc, rtsc)
    adat <- adat[order(ttsc),]
    itms <- ncol(adat)-2
    pop <- seq(from=thetain, to=thetaend, by=0.01)
    if ((maxr-minr)>1) {
      pcmo <- pcmodel(adat[,1:itms])
      prcu <- predict(pcmo, newdata=pop, type="cumprobability")
    } else {
      ramo <- raschmodel(adat[,1:itms])
      prcu <- predict(ramo, newdata=pop, type="cumprobability")
    }
    type <- 1
    cure <- cbind(prcu, pop, type)
    cure <- as.data.frame(cure)
    cat <- table(adat[,itmc])
    ncat <- length(cat)
    pron=c()
    for (i in 1:(ncol(cure)-2)) {
      pron[i]=(sum(cure[,i])==nrow(cure))
    }
    rcat <- which(pron == TRUE)
    nrem <- length(rcat)
    ini <- rcat[itmc] + 1
    fin <- rcat[itmc] + (ncat-1)
    mxcl <- ncat*itmc
    mncl <- (ncat*(itmc-1))+2
    if (ncat>2) {
      cend <- cbind(rowSums(cure[,ini:fin]), cure$pop, cure$type, rowSums(cure[,1:(length(cure)-2)])-nrem)
    } else {
      cend <- cbind(cure[,mxcl], cure$pop, cure$type, rowSums(cure[,1:(length(cure)-2)])-nrem)
    }
    cend <- as.data.frame(cend)
    names(cend) <- c("score", "theta", "type", "ttsc")
    if ((maxr-minr)>1) {
      ppar <- person_estimates(pcmo)
      psav <- ppar
      ppar <- ppar[2:(nrow(ppar)-1),3]
    } else {
      ppar <- personpar(ramo)
    }
    mxpp <- length(ppar)
    ppar <- ppar[1:mxpp]

    if (method=="cut"&cinumber >= max(adat$ttsc)) {
      stop('Number of class intervals need to be smaller than the total scores')
    } else {}

    if (method=="cut"){
      pcrv <- adat
      pmsc <- which(pcrv$ttsc==0)
      if (length(pmsc)==0) {
        pcrv <- pcrv
      } else {
        pcrv <- pcrv[-which(pcrv$ttsc==0),]
      }
      pxsc <- which(pcrv$ttsc==mxsc)
      if (length(pxsc)==0) {
        pcrv <- pcrv
      } else {
        pcrv <- pcrv[-which(pcrv$ttsc==mxsc),]
      }
      pcrv$class <- cut2(pcrv$ttsc, g=cinumber, oneval=TRUE, levels.mean = TRUE)
    } else {}

    if (method=="cut")
    {if (cinumber > length(unique(pcrv$class)))
    { stop('There are not enough subjects in each total score to produce this number of class intervals')}
      else{}
    }
    else {}

    if(method=="cut" & dif!="yes") {
      obs <- aggregate(pcrv[,itmc], by=list(pcrv$class), FUN=mean)
      obs[,1] <- as.numeric(levels(obs[,1]))
      type <- 2
      x <- rep(NA, cinumber)
      for (i in 1:cinumber) {
        x[i] = cend$theta[which(abs(cend$ttsc-obs[i,1])==min(abs(cend$ttsc-obs[i,1])))]
      }
      x <- x[1:cinumber]
      obs <- cbind(obs[,2], x, type)
      obs <- as.data.frame(obs)
      names(obs) <- c("score", "theta", "type")
      cend <- cend[,1:3]
      cend <- as.data.frame(cend)
      names(cend) <- c("score", "theta", "type")
      cend <- rbind(cend, obs)
      cend[,1:3] <- as.numeric(unlist(cend[,1:3]))

      if(axis.rumm=="yes") {
        classbreak=c(
          max(min(subset(cend$theta, cend$type==1)),
              min((subset(cend$theta, cend$type!=1))-
                    1.0)),
          min(max((subset(cend$theta, cend$type==1))),
              max((subset(cend$theta, cend$type!=1)))+
                1.0))
      } else {
        classbreak=c(NA,NA)
      }

      theta <- NULL
      score <- NULL
      myICCplot <- ggplot(cend, aes(x=theta, y=score, col=as.factor(type))) + #The graph itself
        ggtitle(title) +
        theme_light() +
        theme(plot.title = element_text(hjust = 0.5), legend.position=icclabels,
              panel.grid = panelgrid,
              panel.grid.major.y = gridy,
              panel.grid.major.x = gridx,
              panel.border = element_rect(colour="black", size=0.25, fill=NA),
              panel.background = background) +
        scale_x_continuous(breaks = round(seq(min(cend$theta), max(cend$theta), by = xticks),1),
                           limits=classbreak) +
        scale_y_continuous(breaks = round(seq(min(0), max(cend$score)+0.5, by = yticks),1)) +
        scale_color_brewer(palette = pallete, name="", labels=c("Expected Item Score", "Average Observed Item Score")) +
        labs(y = yaxistitle, x = xaxistitle) +
        geom_point(na.rm=TRUE) +
        geom_text(data=annotations,aes(x=xpos,y=ypos,hjust=hjustvar,vjust=vjustvar,label=annotateText), color="black")
    }

    else if (method!="cut"&dif!="yes") {

      obs <- aggregate(adat[,itmc], by=list(adat$ttsc), FUN=mean)
      zero <- which(obs[,1]==0)
      if (length(zero)==0) { obs <- obs}
      else {obs <- obs[-which(obs[,1]==0),] }
      ext <- which(obs==mxsc)
      if (length(ext)==0) { obs <- obs}
      else {obs <- obs[-which(obs==mxsc),] }
      rest <- aggregate(adat[,itmc], by=list(adat$rtsc), FUN=mean)
      type <- 2
      tlsc <- as.vector(unlist(obs[1]))
      names(obs) <- c("score", "obsmean")

      if ((maxr-minr)>1) {
        pseq <- seq(from=psav[2,1], to=psav[(nrow(psav)-1),1], by=1)
      }
      else {
        pseq <- as.numeric(unlist(regmatches(capture.output(ppar)
                                             [c(seq(1, length(capture.output(ppar)), 2))], gregexpr("[[:digit:]]+",
                                                                                                    capture.output(ppar)[c(seq(1, length(capture.output(ppar)), 2))]))))
      }
      ppnw <- cbind(ppar, pseq)
      ppnw <- as.data.frame(ppnw)
      names(ppnw) <- c("theta", "score")
      ptmp <- ppnw[ppnw$score %in% tlsc,]
      ptmp <- as.data.frame(ptmp)
      names(ptmp) <- c("theta", "score")
      obs <- merge(obs, ptmp, by="score")
      obs <- cbind(obs[,2:3], type)
      obs <- as.data.frame(obs)
      names(obs) <- c("score", "theta", "type")
      cend <- cend[,1:3]
      cend <- rbind(cend, obs)

      if(axis.rumm=="yes") {
        classbreak=c(
          max(min(subset(cend$theta, cend$type==1)),
              min((subset(cend$theta, cend$type!=1))-
                    1.0)),
          min(max((subset(cend$theta, cend$type==1))),
              max((subset(cend$theta, cend$type!=1)))+
                1.0))
      }
      else {classbreak=c(NA,NA)}

      myICCplot <- ggplot(cend, aes(x=theta, y=score, col=as.factor(type))) +
        ggtitle(title) + #Choose title
        theme_light() +
        theme(plot.title = element_text(hjust = 0.5), legend.position=icclabels,
              panel.grid = panelgrid,
              panel.grid.major.y = gridy,
              panel.grid.major.x = gridx,
              panel.border = element_rect(colour="black", size=0.25, fill=NA),
              panel.background = background) +
        scale_x_continuous(breaks = round(seq(min(cend$theta), max(cend$theta), by = xticks),1),
                           limits=classbreak) +
        scale_y_continuous(breaks = round(seq(min(0), max(cend$score)+0.5, by = yticks),1)) +
        scale_color_brewer(palette = pallete, name="", labels=c("Expected Item Score", "Average Observed Item Score")) +
        labs(y = yaxistitle, x = xaxistitle) +
        geom_point(na.rm=TRUE) +
        geom_text(data=annotations,aes(x=xpos,y=ypos,hjust=hjustvar,vjust=vjustvar,label=annotateText), color="black")
    }

    else if (method=="cut"&dif=="yes") {

      difd <- cbind(data2, difvar)
      difd <- difd[complete.cases(difd),]
      difd[,1:ncol(difd)] <- as.numeric(unlist(difd[,1:ncol(difd)]))
      difs <- min(difd$difvar)
      if (difs>1) {difd$difvar <- ((difd$difvar)-(difs-1))}
      else if (difs==1) {}
      else if (difs<1) {difd$difvar <- ((difd$difvar)+1)}
      else {}
      ldif <- list()
      ttsc <- list()
      obs <- list()
      zero <- list()
      ext <- list()
      class <- list()
      x <- list()
      for (i in 1:length(unique(difd$difvar))){
        x[[i]]<- vector()
      }

      for (i in 1:length(unique(difd$difvar))) {
        ldif[[i]] <- subset(difd, difd$difvar==i)
        ldif[[i]] <- ldif[[i]][complete.cases(ldif[[i]]),]
        ttsc[[i]] <- rowSums(ldif[[i]][,1:(ncol(ldif[[i]])-1)], na.rm=TRUE)
        ldif[[i]] <- cbind(ldif[[i]][,1:(ncol(ldif[[i]])-1)], ttsc[[i]])
        ldif[[i]] <- ldif[[i]][order(ldif[[i]][,ncol(ldif[[i]])]),]

        zero[[i]] <- which(ldif[[i]]$`ttsc[[i]]`==0)
        if (length(zero[[i]])==0) { ldif[[i]] <- ldif[[i]]}
        else {ldif[[i]] <- ldif[[i]][-which(ldif[[i]]$`ttsc[[i]]`==0),] }
        ext[[i]] <- which(ldif[[i]]==mxsc)
        if (length(ext[[i]])==0) { ldif[[i]] <- ldif[[i]]}
        else {ldif[[i]] <- ldif[[i]][-which(ldif[[i]]$`ttsc[[i]]`==mxsc),] }
        ldif[[i]]$class <- cut2(ldif[[i]][,ncol(ldif[[i]])], g=cinumber, oneval=TRUE, levels.mean = TRUE)
        obs[[i]] <- aggregate(ldif[[i]][,itmc], by=list(ldif[[i]][,ncol(ldif[[i]])]), FUN=mean)
        type[i] <- i+1
        obs[[i]][,1] <- as.numeric(levels(obs[[i]][,1]))
      }

      for (i in 1:length(unique(difd$difvar))) {
        for (j in 1:nrow(obs[[i]])) {
          x[[i]][j] = cend$theta[which(abs(cend$ttsc-obs[[i]][j,1])==min(abs(cend$ttsc-obs[[i]][j,1])))]
        }}

      for (i in 1:length(unique(difd$difvar))) {
        obs[[i]] <- cbind(obs[[i]][,2], x[[i]], type[i])
        obs[[i]] <- as.data.frame(obs[[i]])
        names(obs[[i]]) <- c("score", "theta", "type")}

      cend <- cend[,1:3]
      cend <- as.data.frame(cend)
      names(cend) <- c("score", "theta", "type")
      big_data = do.call(rbind, obs)
      cend <- rbind(cend, big_data)

      allg <- partgam_DIF(data2, difvar)
      pgmm <- allg[itmc,3]
      pgmm <- format(round(pgmm, digits=2), nsmall=2)

      if(difstats=="yes") {

        annotateDIF <- data.frame(
          xpos = c(-Inf,-Inf,Inf,Inf),
          ypos =  c(-Inf, Inf,-Inf,Inf),
          annotateText = paste("gamma == ", pgmm),
          hjustvar = c(-0.35) ,
          vjustvar = c(4.0)) }

      else {annotateDIF <- data.frame(
        xpos = c(-Inf,-Inf,Inf,Inf),
        ypos =  c(-Inf, Inf,-Inf,Inf),
        annotateText = c(""),
        hjustvar = c(-0.35) ,
        vjustvar = c(4.0)) }

      if(axis.rumm=="yes") {
        classbreak=c(
          max(min(subset(cend$theta, cend$type==1)),
              min((subset(cend$theta, cend$type!=1))-
                    1.0)),
          min(max((subset(cend$theta, cend$type==1))),
              max((subset(cend$theta, cend$type!=1)))+
                1.0))
      }
      else {classbreak=c(NA,NA)}

      myICCplot <- ggplot(cend, aes(x=theta, y=score, col=as.factor(type))) +
        ggtitle(title) + #Choose title
        theme_light() +
        theme(plot.title = element_text(hjust = 0.5), legend.position=icclabels,
              panel.grid = panelgrid,
              panel.grid.major.y = gridy,
              panel.grid.major.x = gridx,
              panel.border = element_rect(colour="black", size=0.25, fill=NA),
              panel.background = background) +
        scale_x_continuous(breaks = round(seq(min(cend$theta), max(cend$theta), by = xticks),1),
                           limits=classbreak) +
        scale_y_continuous(breaks = round(seq(min(0), max(cend$score)+0.5, by = yticks),1)) +
        scale_color_manual(values=c("#F0F0F0","#1F78B4","#B2DF8A","#33A02C","#FB9A99",
                                    "#E31A1C","#FDBF6F","#FF7F00","#CAB2D6","#6A3D9A","#FFFF99",
                                    "#B15928"), name="", labels=c("Expected Item Score", diflabels)) +
        labs(y = yaxistitle, x = xaxistitle) +
        geom_point(na.rm=TRUE) +
        geom_line(na.rm=TRUE) +
        geom_text(data=annotations,aes(x=xpos,y=ypos,hjust=hjustvar,vjust=vjustvar,label=annotateText), color="black") +
        geom_text(data=annotateDIF,aes(x=xpos,y=ypos,hjust=hjustvar,vjust=vjustvar,label=annotateText), color="black", parse=TRUE)
    }

    else if (method!="cut"&dif=="yes") {

      difd <- cbind(data2, difvar)
      difd <- difd[complete.cases(difd),]
      difd[,1:ncol(difd)] <- as.numeric(unlist(difd[,1:ncol(difd)]))
      difs <- min(difd$difvar)
      if (difs>1) {difd$difvar <- ((difd$difvar)-(difs-1))}
      else if (difs==1) {}
      else if (difs<1) {difd$difvar <- ((difd$difvar)+1)}
      else {}

      ldif <- list()
      ttsc <- list()
      obs <- list()
      zero <- list()
      ext <- list()
      tlsc <- list()
      ptmp <- list()

      for (i in 1:length(unique(difd$difvar))) {
        ldif[[i]] <- subset(difd, difd$difvar==i)
        ldif[[i]] <- ldif[[i]][complete.cases(ldif[[i]]),]
        ttsc[[i]] <- rowSums(ldif[[i]][,1:(ncol(ldif[[i]])-1)], na.rm=TRUE)
        ldif[[i]] <- cbind(ldif[[i]][,1:(ncol(ldif[[i]])-1)], ttsc[[i]])
        ldif[[i]] <- ldif[[i]][order(ldif[[i]][,ncol(ldif[[i]])]),]
        obs[[i]] <- aggregate(ldif[[i]][,itmc], by=list(ldif[[i]][,ncol(ldif[[i]])]), FUN=mean)
        zero[[i]] <- which(obs[[i]][,1]==0)
        if (length(zero[[i]])==0) { obs[[i]] <- obs[[i]]}
        else {obs[[i]] <- obs[[i]][-which(obs[[i]][,1]==0),] }
        ext[[i]] <- which(obs[[i]]==mxsc)
        if (length(ext[[i]])==0) { obs[[i]] <- obs[[i]]}
        else {obs[[i]] <- obs[[i]][-which(obs[[i]]==mxsc),] }
        type[i] <- i+1
        names(obs[[i]]) <- c("score", "obsmean")
        tlsc[[i]] <- as.vector(unlist(obs[[i]][1]))
        ptmp[[i]] <- cbind(ppar[1:length(tlsc[[i]])], tlsc[[i]])
        ptmp[[i]] <- as.data.frame(ptmp[[i]])
        names(ptmp[[i]]) <- c("theta", "score")
        obs[[i]] <- merge(obs[[i]], ptmp[[i]], by="score")
        obs[[i]] <- cbind(obs[[i]][,2:3], type[i])
        obs[[i]] <- as.data.frame(obs[[i]])
        names(obs[[i]]) <- c("score", "theta", "type")
      }

      cend <- cend[,1:3]
      cend <- as.data.frame(cend)
      names(cend) <- c("score", "theta", "type")
      big_data = do.call(rbind, obs)
      cend <- rbind(cend, big_data)

      allg <- partgam_DIF(data2, difvar)
      pgmm <- allg[itmc,3]
      pgmm <- format(round(pgmm, digits=2), nsmall=2)

      if(difstats=="yes") {

        annotateDIF <- data.frame(
          xpos = c(-Inf,-Inf,Inf,Inf),
          ypos =  c(-Inf, Inf,-Inf,Inf),
          annotateText = paste("gamma == ", pgmm),
          hjustvar = c(-0.35) ,
          vjustvar = c(4.0)) }

      else {annotateDIF <- data.frame(
        xpos = c(-Inf,-Inf,Inf,Inf),
        ypos =  c(-Inf, Inf,-Inf,Inf),
        annotateText = c(""),
        hjustvar = c(-0.35) ,
        vjustvar = c(4.0)) }

      if(axis.rumm=="yes") {
        classbreak=c(
          max(min(subset(cend$theta, cend$type==1)),
              min((subset(cend$theta, cend$type!=1))-
                    1.0)),
          min(max((subset(cend$theta, cend$type==1))),
              max((subset(cend$theta, cend$type!=1)))+
                1.0))
      }
      else {classbreak=c(NA,NA)}

      myICCplot <- ggplot(cend, aes(x=theta, y=score, col=as.factor(type))) +
        ggtitle(title) + #Choose title
        theme_light() +
        theme(plot.title = element_text(hjust = 0.5), legend.position=icclabels,
              panel.grid = panelgrid,
              panel.grid.major.y = gridy,
              panel.grid.major.x = gridx,
              panel.border = element_rect(colour="black", size=0.25, fill=NA),
              panel.background = background) +
        scale_x_continuous(breaks = round(seq(min(cend$theta), max(cend$theta), by = xticks),1),
                           limits= classbreak) +
        scale_y_continuous(breaks = round(seq(min(0), max(cend$score)+0.5, by = yticks),1)) +
        scale_color_manual(values=c("#F0F0F0","#1F78B4","#B2DF8A","#33A02C","#FB9A99",
                                    "#E31A1C","#FDBF6F","#FF7F00","#CAB2D6","#6A3D9A","#FFFF99",
                                    "#B15928"), name="", labels=c("Expected Item Score", diflabels)) +
        labs(y = yaxistitle, x = xaxistitle) +
        geom_point(na.rm=TRUE) +
        geom_line(na.rm=TRUE) +
        geom_text(data=annotations,aes(x=xpos,y=ypos,hjust=hjustvar,vjust=vjustvar,label=annotateText), color="black") +
        geom_text(data=annotateDIF,aes(x=xpos,y=ypos,hjust=hjustvar,vjust=vjustvar,label=annotateText), color="black", parse=TRUE)
    }

  }

  if (length(itemnumber)>4)

  { stop(' "The function plots only a maximum of 4 items simultaneously" ')}

  else if (length(itemnumber)==1) {
    itmc=itemnumber
    itmn=itemdescrip[1]
    plot <- pltC()
    plot
  }

  else {
    plst <- list()
    for (i in 1:length(itemnumber)) {
      itmc=itemnumber[i]
      itmn=itemdescrip[i]
      plst[[i]] <- pltC()
    }

    do.call(grid.arrange, args=(c(plst, nrow=2, ncol=2)))
    paste("Please press Zoom on the Plots window to see the plot")
  }
}


