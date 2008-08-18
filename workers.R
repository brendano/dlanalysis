dlanalysis$worker_an = function(a, labels=NULL, labels_long=a$gold, 
  trim_workers=FALSE, trim_units=TRUE, ...)
{
  worker_ids = a$WorkerId 
  if (trim_workers)  worker_ids = trim_levels(worker_ids)
  if (trim_units)  a$orig_id = trim_levels(a$orig_id)

  if ( !is.null(labels) ) {
    a$label = flipleft(a,labels, 'orig_id')
  } else if (!is.null(labels_long)) {
    a$label = labels_long
  } else {
    stop("need labels somehow")
  }
  
  fn = dlanalysis$mygeneric('worker_an')
  fn(a, worker_ids, ...)
} 


dlanalysis$worker_an.numeric <- function(a, worker_ids) {
  stopifnot(nrow(a) == length(worker_ids))
  worker_info = dfagg(a, worker_ids, function(x) {
    r = list(
      num = nrow(x),
      # cor = safecor(x$response, x$gold),
      cm_sd = sd(x$response - x$gold),
      cm_bias = mean(x$response - x$gold))
    linmodel = lm(response~gold, data=x)
    r$lm_coef = coef(linmodel)[['gold']]
    r$lm_bias = coef(linmodel)[['(Intercept)']] 
    r$lm_sd = sd(predict(linmodel) - x$response)
    # r$lm_sd = sd(jitter(predict(linmodel) - x$response))
    r
  }, trim=FALSE)

  missing = is.na(worker_info$num)
  
  worker_info$num[missing]  = 0
  worker_info$cm_bias[missing] = 0
  worker_info$cm_sd[missing]   = Inf
  # worker_info$lm_bias[missing] = 0
  # worker_info$lm_coef[missing] = 1
  # worker_info$lm_sd[missing] = Inf
  # 
  # worker_info$lm_coef[is.na(worker_info$lm_coef)] = 1
  worker_info
}

dlanalysis$safecor <- function(x,y) {
  safe = !is.na(x) & !is.na(y)
  x=x[safe]; y=y[safe]
  if ( !is.na(sd(x)) && sd(x)==0 )  NA
  else if ( !is.na(sd(y)) && sd(y)==0 )  NA
  else try(cor(x,y, use='pairwise.complete.obs'))
}

dlanalysis$fit_anno_model <- dlanalysis$mygeneric("fit_anno_model")

dlanalysis$fit_anno_model.numeric <- function(a, compute_priors=FALSE, ...) {
  prior_mean=NULL; prior_sd=NULL;
  if (compute_priors) {
    u_gold = dfagg(a$gold, a$orig_id, first)
    prior_mean = mean(u_gold)
    prior_sd = sd(u_gold)
  }

  ret = list(
    w = worker_an(a, ...), #name='workeran'),
    prior_mean = prior_mean,
    prior_sd = prior_sd
  )
  ret
}

dlanalysis$label_posterior.numeric <- function(model, a, u=NULL, no_priors=TRUE, ...) {
  p = posterior_given_workers(model$w, a, ...)
  
  if (! no_priors) {
    w1 = p$worker_weight;  w2 = 1/model$prior_sd^2 
    p$mean = 1/(w1+w2) * (w1*p$mean  +  w2*model$prior_mean)    
  }
  
  p
}

dlanalysis$posterior_given_workers.numeric <- function(w, a, use='cm') {
  aw = merge(a,w, by.x='WorkerId', by.y=0)
  posterior = dfagg(aw, aw$orig_id, function(x) {
    if (use=='lm') {
      list(
        # mean = mean((x$response - x$lm_bias)/(x$lm_coef+.1)),
        # mean = weighted.mean((x$response - x$lm_bias)/x$lm_coef,  log( 1e10*(x$lm_coef/x$lm_sd)^2)),
        mean = weighted.mean((x$response - x$lm_bias)/x$lm_coef,  (x$lm_coef/x$lm_sd)^2),
        worker_weight = sum((x$lm_coef/x$lm_sd)^2) )
    }
    else if (use=='lm2') {
      r = list(
        mean = weighted.mean((x$response - x$lm_bias)/x$lm_coef,  (x$lm_coef/x$lm_sd)^2),
        worker_weight = sum((x$lm_coef/x$lm_sd)^2) )
      r$mean = min(r$mean,100)
      r$mean = max(r$mean,0)
      r
    }
    else if (use=='cm') {
      list(
        mean = weighted.mean(x$response - x$cm_bias, 1/x$cm_sd^2),
        sd = 42,
        worker_weight = sum(1/x$cm_sd^2))
    } else if (use=='0b') {
      list(
        mean = weighted.mean(x$response, 1/x$cm_sd^2),
        worker_weight = sum(1/x$cm_sd^2))
    }

    else stop("specify model to use")
  })
  posterior
}






dlanalysis$merge_worker_tail <- function(a, cutoff=20) {
  w = worker_an(a)
  tail_workers = row.names(w)[ w$num <= cutoff ]
  a$WorkerId = factor(a$WorkerId, levels=c('mr_tail', levels(a$X.amt_w)))
  a[a$WorkerId %in% tail_workers, 'WorkerId'] = 'mr_tail'
  a$WorkerId = trim_levels(a$WorkerId)
  a
}



dlanalysis$worker_unit_plot <- function(a, nbreaks=10, reorder=FALSE, ...) {
  m = df2matrix(a,c('WorkerId','orig_id'),'response')
  w_first_j = dfagg(a,a$X.amt_w,function(x) mean(as.integer(x$orig_id)))
  m = m[order(w_first_j),]
  
  if (a@data_type=='numeric') {
    breaks = seq(min(c(a$gold,a$response)), max(c(a$gold,a$response)), length.out=nbreaks+1)
    colors = rev(heat.colors(nbreaks))
    legend_labels = breaks[1:nbreaks]
  } else if (a@data_type=='categ') {
    m = matrix(as.integer(factor(m, levels=a@candidates)),  nrow(m), ncol(m))
    breaks = 0:length(a@candidates)
    colors = head(c('red','blue','green','purple'),  length(a@candidates))
    ncol = length(colors)
    legend_labels = a@candidates
  }

  if (reorder) {
    distfun = function(x) { x[is.na(x)]=0; d=dist(x); d}
    heatmap(m, scale='none',col=colors,breaks=breaks, distfun=distfun, ...)
  } else {
    heatmap(m, Rowv=NA,Colv=NA,scale='none', col=colors, breaks=breaks, ...)
  }
  legend("topleft", legend=legend_labels, fill=colors, inset=0)
}


dlanalysis$worker_numeric_model_plot1 <- function(a) {
  xyplot(response ~ gold | WorkerId, data=a, auto.key=T, 
    panel=function(x,y, subscripts,...) { 
      panel.xyplot(x,y,subscripts=subscripts,...)
      m = lm(y~x)
      panel.abline(m,col='blue')
      panel.abline(coef=c(0,1), col='gray', lty=2)
      # panel.arrows(x,y, x, est$calib[subscripts], length=.1)
      # # if (!all(train)) {
      #   print(coef(m))
      #   calib_y = (y[!train] - coef(m)[1]) / coef(m)[2]
      #   # print(list(...))
      #   cat("New position: ", calib_y,"\n")
      #   # panel.xyplot(x[!train], calib_y, col='green', pch=24)
      #   panel.arrows(x[!train],y[!train], x[!train],calib_y,  length=.1)
      # #   m = lm(y~x)
      # #   panel.abline(m,col='red',...)
      # # } 
    })
  
}


dlanalysis$worker_numeric_model_plot2 <- function(a,  est) {
  xyplot(response ~ gold | WorkerId*task, data=a, auto.key=T, 
    panel=function(x,y, subscripts,...) { 
      panel.xyplot(x,y,subscripts=subscripts,...)
      m = lm(y~x)
      panel.abline(m,col='blue')
      panel.abline(coef=c(0,1), col='gray', lty=2)
      panel.arrows(x,y, x, est$calib[subscripts], length=.1, col='gray')
      # panel.text(10,95, sprintf("weight = %.0f", 10000*coef(m)[2]^2/sd(predict(m)-y)), offset=0)
      panel.text(0,100, sprintf("sd=%.1f a=%.1f b=%.1f\nweight = %.0f", 
        sd(predict(m)-y),  coef(m)[2], coef(m)[1],  10000*coef(m)[2]^2/sd(predict(m)-y)),
          offset=0, adj=c(0,1))
      
      # # if (!all(train)) {
      #   print(coef(m))
      #   calib_y = (y[!train] - coef(m)[1]) / coef(m)[2]
      #   # print(list(...))
      #   cat("New position: ", calib_y,"\n")
      #   # panel.xyplot(x[!train], calib_y, col='green', pch=24)
      #   panel.arrows(x[!train],y[!train], x[!train],calib_y,  length=.1)
      # #   m = lm(y~x)
      # #   panel.abline(m,col='red',...)
      # # } 
    })
  
}


dlanalysis$worker_numeric_model_plot3 <- function(a,  atest=rep(FALSE,nrow(a))) {
  x=data.frame(a,atest=atest)
  xyplot(response~gold|WorkerId, data=x, groups=atest,auto.key=T, 
    panel=function(x,y, subscripts, groups,...) { 
      panel.xyplot(x,y,subscripts=subscripts,groups=groups,...)
      train=!groups[subscripts]
      m = lm(y[train]~x[train])
      panel.abline(m,col='blue')
      panel.abline(coef=c(0,1), col='gray', lty=2)
      if (!all(train)) {
        print(coef(m))
        calib_y = (y[!train] - coef(m)[1]) / coef(m)[2]
        # print(list(...))
        cat("New position: ", calib_y,"\n")
        # panel.xyplot(x[!train], calib_y, col='green', pch=24)
        panel.arrows(x[!train],y[!train], x[!train],calib_y,  length=.1)
      #   m = lm(y~x)
      #   panel.abline(m,col='red',...)
      } 
    })
  
}