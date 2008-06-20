##
## OLD CODE for threshold/binary classification / confusion matrix  analysis
##

# all threshold vs accuracy analysis is now somewhat done on top of the ROCR package
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
