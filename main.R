# generic analysis functions

source("~/dlanalysis/util.R")

# if ( ! exists('JOB_INFO'))
#   JOB_INFO = list()

dlanalysis = new.env()

dlanalysis$agg_to_unit <- function(a, by='unit_id',
  up_for_vote = names(a)[bgrep("^X\\.",names(a)) & !bgrep("^X\\.amt",names(a))]
  ) {

  u = dfagg(a, a[,by], function(x) {
    ret = list()
    for(attr in up_for_vote) {
      # winner = names(which.max(table( x[,attr] )))
      # ret[[paste(attr,'decision',sep='_')]] = winner
      p = table(x[,attr]) / nrow(x)
      ret[[paste(attr,'entropy',sep='_')]] = sum(-p * log(p), na.rm=T)
      ret = c(ret, table(x[,attr]))
    }
    ret
  })
  cat("Need candidates attribute...\n")
  cat("Need target...\n")
  u
}

dlanalysis$classify_probs <- function(u, target=attr(u,'target'), candidates=attr(u,'candidates')) {
  u[,target] / apply(u[,candidates], 1, sum)
}

dlanalysis$binary_vote <- function(u, dec_thresh=0.5, conserv_thresh=0, target=attr(u,'target'), candidates=attr(u,'candidates'), bool=FALSE) {
  class_probs = classify_probs(u)
  bool_vote =  class_probs  >=  dec_thresh
  if (any(class_probs < conserv_thresh, na.rm=T))
    bool_vote[class_probs < conserv_thresh] = NA
  if (bool)  return (bool_vote)
  fill_bool(bool_vote, target, paste(setdiff(candidates, target), collapse='-'))
}

dlanalysis$accuracy <- function(u, gold=u$gold, target=attr(u,'target'), candidates=attr(u,'candidates'), ...) {
  correct = binary_vote(u, ...) == gold
  list(
    acc    = mean(correct, na.rm=T),
    prec   = mean(correct[u$gold==target], na.rm=T),
    recall = mean(correct[u$gold!=target], na.rm=T)
  )
}

dlanalysis$pr_curve <- function(u, thresh=seq(0,1,.05), plot=T) {
  x = dfapply(thresh, function(t) accuracy(u, dec_thresh=t))
  if (plot) {
    plot(x$prec, x$recall, type='n')
    text(x$prec, x$recall, sprintf("%.0f",100*x$acc))
  }
  invisible(x)
}

# dlanalysis$conserve_curve <- function(u, thresh=seq(0,1,.05), plot=T) {
#   class_probs = classify_probs(u)
#   x = dfapply(thresh, function(t) {
#     corrects = class_probs 
#     list(
#       num = sum(class_probs >= t),
#       perc = mean(class_probs >= t),5 
#   })
# }


# w = data.frame(row.names=names(sort(-table(a$X.amt_worker_ids))))
# w$num = table(a$X.amt_worker_ids)[row.names(w)]
  # list(w=w, u=u)


while("dlanalysis" %in% search())
  detach("dlanalysis")
attach(dlanalysis)
