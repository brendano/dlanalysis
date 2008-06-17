# generic analysis functions

source("~/dlanalysis/util.R")

# if ( ! exists('JOB_INFO'))
#   JOB_INFO = list()

dlanalysis = new.env()
source("~/dlanalysis/workers.R")
source("~/dlanalysis/xval.R")

dlanalysis$load_categ_anno <- function(filename) {
  a = read.delim(filename,sep="\t",colClasses=list(response='factor',gold='factor'))
  attr(a,'data_type') = 'categ'
  if ( ! setequal(levels(a$responsee), levels(a$factor)))
    stop("Uhoh, levels of response and gold are not the same.  need to hack up this code here")
  # a$response <<- as.factor(a$response, levels=BLA)
  # a$gold     <<- as.factor(a$gold, levels=BLA)
  attr(a,'candidates') = levels(a$response)
  attr(a,'target') = tail(candidates, 1)
  msg("Candidates: ",a@candidates)
  msg("Target: ",a@target)
  a
}

dlanalysis$anno_subset <- function(a, limit=Inf, stochastic=FALSE) {
  subset_fn = if ( !stochastic ) function(x) head(x, limit)
              else stop("oops")
  ret = data.frame()
  for (oid in unique(a$orig_id)) {
    ret = rbind(ret, a[ subset_fn(which(a$orig_id==oid)) , ])
  }
  ret
  # dfagg(a, a$orig_id, subset_fn)
}

dlanalysis$agg_to_unit = mygeneric('agg_to_unit')

dlanalysis$agg_to_unit.categ <- function(a,
    by = 'orig_id',
    limit=Inf,
    candidates = union(levels(response), levels(gold))
  ) {
  u = dfagg(a, a[,by], function(x) {
    x = head(x,limit)
    ret = list()
    # winner = names(which.max(table( x[,attr] )))
    # ret[[paste(attr,'decision',sep='_')]] = winner
    p = table(x$response) / nrow(x)
    ret[['entropy']] = sum(-p * log(p), na.rm=TRUE)
    ret = c(ret, table(x$response))
    ret[['plurality']] = most_common(x$response)
    t = table(x$gold, exclude=NULL)
    t = t[t>0]
    if (length(t) > 1)  stop("uhoh bad gold anno set")
    ret[['gold']] = names(t)
    ret
  })
  attr(u,'candidates') = a@candidates
  attr(u,'target') = a@target
  attr(u,'data_type') = 'categ'
  u  
}

dlanalysis$agg_to_unit_old1 <- function(a, by='unit_id',
  up_for_vote = names(a)[bgrep("^X\\.",names(a)) & !bgrep("^X\\.amt",names(a))]
  ) {

  has_gold = 'gold' %in% names(a)
  u = dfagg(a, a[,by], function(x) {
    ret = list()
    for(attr in up_for_vote) {
      # winner = names(which.max(table( x[,attr] )))
      # ret[[paste(attr,'decision',sep='_')]] = winner
      p = table(x[,attr]) / nrow(x)
      ret[[paste(attr,'entropy',sep='_')]] = sum(-p * log(p), na.rm=T)
      ret = c(ret, table(x[,attr]))
    }
    if (has_gold) {
      t = table(x$gold, exclude=NULL)
      if (length(t) > 1)  stop("uhoh bad gold anno set")
      ret[['gold']] = names(t)
    }
    ret
  })
  # cat("Need candidates attribute and target...\n")
  u
}

dlanalysis$classify_probs <- function(u, target=u@target, candidates=u@candidates) {
  u[,target] / apply(u[,candidates], 1, sum)
}

dlanalysis$binary_vote <- function(u, thresh=0.5, 
  # conserve_thresh=0, 
  target=attr(u,'target'), candidates=attr(u,'candidates'), bool=FALSE) {
    
  class_probs = classify_probs(u)
  bool_vote =  class_probs  >=  thresh
  # if (any(class_probs < conserve_thresh, na.rm=T))
  #   bool_vote[class_probs < conserve_thresh] = NA
  if (bool)  return (bool_vote)
  fill_bool(bool_vote, target, paste(setdiff(candidates, target), collapse='-'))
}

# TODO -- all threshold vs accuracy analysis should be redone on top of the ROCR package
# this was going in that direction, but ROCR is so much better

dlanalysis$accuracy <- function(u, gold=u$gold, target=attr(u,'target'), candidates=attr(u,'candidates'), ...) {
  decisions = binary_vote(u, ...)
  correct = decisions == gold
  x = list(
    acc = mean(correct, na.rm=T),
    # num = sum( !is.na(decisions) ),
    acc_count    = sum(correct, na.rm=T),
    true_pos  = sum(decisions==target & correct, na.rm=T),
    false_pos = sum(decisions==target & !correct, na.rm=T),
    true_neg  = sum(decisions!=target & correct, na.rm=T),
    false_neg = sum(decisions!=target & !correct, na.rm=T)
  )
  # summary stats of the confusion matrix!  fun fun!
  # see infobox on en.wikipedia.org/wiki/Receiver_operating_characteristic
  x$prec   = x$true_pos / (x$true_pos + x$false_pos)
  x$recall = x$true_pos / (x$true_pos + x$false_neg)
  # x$spec   = x$true_neg / (x$false_pos + x$true_neg)
  # x$fallout= 1 - x$spec
  x
}

dlanalysis$pr_curve <- function(u, thresh=seq(0,1,.05), plot=T, plot.counts=F) {
  x = dfapply(thresh, function(t) accuracy(u, thresh=t))
  if (plot) {
    if (plot.counts) {
      target = attr(u,'target')
      prec_upper = sum(u$gold==target, na.rm=T)
      recall_upper = sum(u$gold!=target, na.rm=T)
      plot(x$prec_count, x$recall_count, type='o', xlim=c(0,prec_upper), ylim=c(0,recall_upper))
    } else {
      plot(x$prec, x$recall, type='o', xlim=c(0,1), ylim=c(0,1))
      # text(x$prec, x$recall, sprintf("%.0f",100*x$acc))
    }
  }
  x$thresh = thresh
  x
}

dlanalysis$rocr <- function(ug, measure='acc', x.measure='cutoff', conf=NULL, gold=ug$gold, target=ug@target) {
  stopifnot( all(!is.na(ug$gold)) )
  library(ROCR)
  
  if (is.null(conf))
    conf = classify_probs(ug)
  
  if (length(ug@candidates) == 2) {
    cands = ug@candidates
  } else {
    gold = rep(NA, nrow(ug))
    gold[ug$gold==target] = target
    no_label = if (target != "no") "no" else "off"
    gold[ug$gold!=target] = no_label
    cands = c(no_label, target)
  }    
  
  pred = prediction(conf, gold, label.ordering=c(setdiff(cands,target), target))
  perf = performance(pred, measure, x.measure)
  list(pred=pred, perf=perf)
}

dlanalysis$thresh_analysis <- function(u, num_thresh=sum(!is.na(u$gold)), conf=NULL, target=u@target) {
  ug = u[ !is.na(u$gold), ]
  r = rocr(ug,'acc', conf=conf)
  cutoffs = r$pred@cutoffs[[1]]
  cutoff_inds = unique(floor(seq(1, length(cutoffs), length.out=num_thresh)))
  # cutoff_inds = sapply(thresh, function(t) {
  #   if (all(cutoffs > t))  1
  #   else which(cutoffs[cutoffs > t])[1]
  # })

  
  ret = data.frame(thresh=cutoffs[cutoff_inds], 
    acc=r$perf@y.values[[1]][cutoff_inds],
    tp=r$pred@tp[[1]][cutoff_inds],
    fp=r$pred@fp[[1]][cutoff_inds],
    tn=r$pred@tn[[1]][cutoff_inds],
    fn=r$pred@fn[[1]][cutoff_inds]
  )
  
  ret$prec = ret$tp / (ret$tp+ret$fp)
  ret$rec = ret$tp / (ret$tp+ret$fn)
  ret$spec = ret$tn / (ret$fp+ret$tn)
  
  attr(ret,'rocr') = r
  ret[order(ret$thresh),]
}

dlanalysis$remove_convex_interior_of_crappiness <- function(ta) {
  # strictly crappier points in the confusion space, go away
  good_rows = rep(NA, nrow(ta))
  good_rows[1] = TRUE
  ta = ta[order(ta$thresh),]
  # weak pareto dominance: require at least one elementwise strict dominance
  dominates <- function(x,y)  any(x > y) && all(x >= y)
  for (i in 2:nrow(ta)) {
    if (ta[i,'thresh'] == Inf) { good_rows[i]=TRUE; next}
    if (dominates(ta[i,c('tp','tn')], ta[i-1,c('tp','tn')])) {
      good_rows[i-1] = FALSE
      good_rows[i] = TRUE
    } else if (dominates(ta[i-1,c('tp','tn')], ta[i,c('tp','tn')])) {
      good_rows[i] = FALSE
    } else {
      good_rows[i] = TRUE
    }
  }
  if (any(is.na(good_rows))) stop("wtf")
  ta[good_rows,]
}

dlanalysis$plot_confusion_bars <- function(ta,
    mode='errmid',
    comps=c('tn','fp','fn','tp'),
    type_colors = NULL,
    # legend_texts = list(tp='true pos (hit)', fp='false pos (false alarm)', tn='true neg', fn='false neg (miss)'),
    legend_texts = list(tp='true pos', fp='false pos', tn='true neg', fn='false neg'),
    # legend = legend_texts[comps],
    main=paste("Classifier performance on gold standard at different thresholds"),
    names.arg.downsample=1,
    col=gray( (1:4+1)/5 ),
    anno_percs='yo',
    horiz=TRUE,
    threshlab='Threshold to say "yes": higher values are more conservative',
    countlab='Raw counts',
    legend.x='topright', legend.inset=.06,
    top_texts=FALSE,
    ...
) {
  if (!is.null(mode)) {
    if (mode=='acc')  { comps = c('tp','tn','fn','fp'); subtitle="Accuracy in %" }
    else if (mode=='roc'){ comps = c('tp','fn','fp','tn'); subtitle="Recall vs False Pos Rate in %" }
    else if (mode=='pr') { comps = c('tp','fp','fn','tn'); subtitle="Prec vs Recall in %" }
    else if (mode=='errmid') { 
      comps = c('tp','fn','fp','tn')
      subtitle="Recall, Precision, Specificity in %\nMiddle bars are errors"
      top_texts = TRUE
    } else if (mode=='posmid') {
      comps = c('fn','tp','fp','tn')
      subtitle="Recall, Precision, Specificity in %"
    }
    main = paste(main,subtitle,sep="\n")
  }
    
  type_colors = list(tp=hsv(.68,.8,1),fn=hsv(.68,.5,1),tn=hsv(.03,.75,1),fp=hsv(.03,.5,1))
  
  # type_colors = list(tp='gray20','darkblue',fp='gray50',fn='gray70',tn='gray30')
  # colors = gray(1:4 / 4)
  fmt = sprintf("%.2f",ta$thresh)
  names.arg = rep(NA, nrow(ta))
  spots = c(seq(1, nrow(ta), names.arg.downsample), nrow(ta))
  names.arg[spots] = fmt[spots]

  col = if (is.null(type_colors)) col else as.character(type_colors[comps])

  bars = t(as.matrix(ta[, comps]))
  # if (top_texts) {
  #   bars = cbind(bars,c(0,0,0,0))
  #   names.arg = c(names.arg,'')
  # }
  bp=barplot(bars , names.arg=names.arg,
    horiz=horiz,
    space=0, las=if(horiz) 1 else 2, 
    # legend=legend_texts[comps],
    col = col,
    xlab = if(horiz) countlab else threshlab,
    ylab = if(horiz) threshlab else countlab,
    main=main,
    # sub="By Dolores Labs - http://blog.doloreslabs.com/?p=61",
    ...
  )
  if (horiz)
    legend(legend.x, legend=(legend_texts[comps]),fill=(col),inset=legend.inset) #, horiz=T)  
  else
    legend(legend.x, legend=rev(legend_texts[comps]),fill=rev(col),inset=legend.inset)
  
  nice_labels <- function(letter, frac) {
    x = 100*frac
    s = sprintf("%.0f",round(x))
    # s[ x>99.5 ] = ':-)'
    # s [ is.na(x) ] = 
    s = paste(letter, s, sep=' ')
    # s[ x>99.5 | x<.5 | is.na(x) ] = ''
    s
  }
  
  dotext <- function(count_points, labels) {
    thresh_points = bp
    if (horiz)
      text(count_points + 1, thresh_points, labels=labels, adj=c(0,.5))
    else 
      text(thresh_points, count_points, labels=labels, adj=c(.5,0))
  }
  ht=6.5
  x=ta
  if (mode=='errmid' || mode=='posmid') {
    if(mode=='errmid')
      dotext(x$tp, labels=nice_labels('R',x$tp/(x$tp+x$fn)))
    if(mode=='posmid')
      dotext(x$fn, labels=nice_labels('R',x$tp/(x$tp+x$fn)))
    # dotext(x$tp+x$fn+x$fp, labels=nice_labels('P',x$tp/(x$fp+x$tp)))
    # dotext(x$tp+x$fn+x$fp, labels=nice_labels('         S',x$tn/(x$fp+x$tn)))
    dotext(x$tp+x$fn+x$fp, labels=paste(
      nice_labels('P',x$tp/(x$fp+x$tp)),
      nice_labels('S',x$tn/(x$fp+x$tn)), sep='  '  ))
    
  }
  else if (mode=='roc') {
    dotext(x$tp, labels=nice_labels('R',x$tp/(x$tp+x$fn)))
    dotext(x$tp+x$fn+x$fp, labels=nice_labels('F',x$fp/(x$fp+x$tn)))  # FPR
  } else if (mode=='pr') {
    dotext(x$tp+x$fp+x$fn, labels=nice_labels('R',x$tp/(x$tp+x$fn)))
  } else if (mode=='acc') {
    dotext(x$tp+x$tn + .2*ht, labels=nice_labels('A',(x$tp+x$tn) / (x$tp+x$tn+x$fp+x$fn)))
  }
  
  if (top_texts) {
    actual_pos_middle = (ta$tp+ta$fn)[1] / 2
    actual_neg_middle = (ta$tp+ta$fn)[1] + (ta$tn+ta$fp)[1]/2
    y = nrow(ta) + .4
    text(actual_pos_middle, y, "Instances with gold label YES")
    text(actual_neg_middle, y, "Instances with gold label NO")
    
    # pred_pos_middle = (ta$tp+ta$fn)[1]
    # legend(pred_pos_middle, 5, legend="Dark blue and pink: Positive predictions",xjust=.5)
  }

  invisible(bp)
}

dlanalysis$plot_thresh_cost <- function(rocr_pred, cost_ratios=c(1,2,5,10,20)) {
  # col = gray( (1:length(cost_ratios) + 4) / (length(cost_ratios)+6) )
  col=rainbow(length(cost_ratios))
   
  plot(performance(pred,'cost'),cost.fp=cost_ratios[1],cost.fn=1, col=col[1], xaxis.at=seq(0,1,.05))
  for (i in 2:length(cost_ratios)) {
    plot(performance(pred,'cost',cost.fp=cost_ratios[i],cost.fn=1),  add=TRUE, col=col[i], xaxis.at=seq(0,1,.05))
  }
  legend(.8,.6, paste(cost_ratios, ":1 FP vs FN cost ratio",sep=''), fill=col)
  title(main="Misclassification expected costs")
}

dlanalysis$assert_golds <- function(ug)  stopifnot( all(!is.na(ug$gold)) )

dlanalysis$ta_at_thresh <- function(ta, thresh=0.5) {
  # like ta[ta$thresh==thresh,]  ... except tolerant for weirdness  
  # ta[ which.min(abs(ta$thresh - thresh)), ]
  
  distances = ta$thresh - thresh
  distances[distances<0] = -Inf
  ta[ which.min(abs(distances)), ]
}

dlanalysis$plot_vertical_thresh <- function(ug, p_tol=.01, r_tol=.01,
  use_acc=TRUE, use_prec=TRUE, use_rec=TRUE,
  conf = classify_probs(u),
  # conf = if (!is.null(ug$conf)) ug$conf else (ug[,target] / apply(ug[,ug@candidates], 1, sum)),
  # conf = ug$conf,
   ...) {
  # select interesting points
  ta = thresh_analysis(ug, conf=conf)
  ips = list()
  if (use_acc) {
    best_acc = ta$thresh[ which.max(ta$acc) ]
    ips$acc = list("Best accuracy:",best_acc)
  }
  if (use_prec) {
    z = ta$prec >= (1-p_tol)   # region
    good_prec = ta$thresh[z][ which.max(ta$acc[z]) ]
    # ips$prec = list(sprintf("Best >%.0f%% precision: ",100*(1-p_tol)), good_prec)
    ips$prec = list("High precision:", good_prec)
  }
  if (use_rec) {
    z = ta$rec >= (1-r_tol)   # region
    good_rec  = ta$thresh[z][ which.max(ta$acc[z]) ]
    # ips$rec = list(sprintf("Best >%.0f%% recall: ",100*(1-r_tol)), good_rec)
    ips$rec = list("High recall:", good_rec)
  }
  plot_vertical_thresh2(ug, ips, conf=conf, ...)
}

dlanalysis$plot_vertical_thresh2 <- function(ug, 
  interesting_points=list(list("Lame middle threshold",.5)), 
  target=ug@target,
  # conf=ug$conf,
  conf=classify_probs(ug),

  main="Test set separation by binary classifier",
  sub="Above a threshold, classified as Y, below as N
Errors above are false pos; errors below are false neg
Accuracy, Precision, Recall in %
Dots have horizontal jitter (x-axis has no meaning)",
  ylab="Confidence level (% of Turker vote for YES)",
  legend.x="bottomright", legend.inset=1,
  ylim=c(0,1), ylim_auto=FALSE
) {
  assert_golds(ug)
  plot.new()
  if (ylim_auto) {
    conf2 = conf[conf<Inf & conf>-Inf]
    spacer = sd(conf2) / 5
    ylim = c(min(conf2)-spacer, max(conf2)+spacer)
  }
  plot.window(ylim=ylim,xlim=c(0,1))
  axis(2,at=seq(0,1,.1),ylab='asfdafsd')
  title(main=main,sub=sub,ylab=ylab)

  j = jitter(rep(.1,nrow(ug)),50)
  j = j-min(j) + .037

  colors = list(y='blue', n='red')
  x = ug$gold==target
  points(j[x], conf[x], col=colors$y)
  x = ug$gold!=target
  points(j[x], conf[x], col=colors$n)
  
  legend(legend.x, legend=c("YES label","NO label"), fill=c(colors$y,colors$n), title="Gold standard's labels:", inset=legend.inset)
  
  prec <- function(thresh) {
    mean(ug[conf>=thresh,'gold']==target, na.rm=T)
  }
  rec <- function(thresh) {
    tp = sum(ug[conf>=thresh,'gold']==target, na.rm=T)
    fn = sum(ug[conf< thresh,'gold']==target, na.rm=T)
    tp / (tp+fn)
  }
  acc <- function(thresh) {
    mean((conf>=thresh) == (ug$gold==target), na.rm=T)
  }
  msg <- function(t) sprintf("thresh=%.1f\nA %.0f, P %.0f, R %.0f",
          100*t, 100*acc(t), 100*prec(t), 100*rec(t))
  # myarrow <- function(t) arrows(.2,t, .12,t, .1)
  myarrow <- function(t) { lines(c(-.02,.3), c(t,t)) }
  stuff <- function(t,m) {
    t_txt = t
    t = min(t, max(conf))
    t = max(t, min(conf))          
    text(.32,t, paste(m,msg(t_txt)), adj=0)

    myarrow(t)
  }
  for (pair in interesting_points) {
    if (length(pair[[2]])==0) next
    stuff(pair[[2]], pair[[1]])
  }
}



# i'm sure s3 has a way to do this but i cant figure it out.  (briefly tried
# subclassing by making class() a vector, but couldnt figure out how to accept
# more than 1 arg.  yes i'm sure i'm dumb.)  but, easier to make my own!
dlanalysis$mygeneric <- function(orig_fname) {
  return(function(arg1, ...) {
    if ( is.null(attr(arg1,'data_type')) )
      stop("Error, need a 'data_type' attribute on first arg")
    fname = paste(orig_fname,arg1@data_type, sep='.')
    fn = dlanalysis[[fname]]
    if ( is.null(fn) ) {
      stop("Error, couldn't find a function named ", fname)
    }
    fn(arg1, ...)
  })
}

while("dlanalysis" %in% search())
  detach("dlanalysis")
attach(dlanalysis)
