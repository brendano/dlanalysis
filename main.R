# generic analysis functions

source("~/dlanalysis/util.R")

dlanalysis = new.env()

agg_to_unit <- function(a, by='unit_id',
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
  u
}

binary_vote <- function(u, thresh=0.5, target='same', total_from=c('same','diff'), bool=FALSE) {
  bool_vote = u[,target] / apply(u[,total_from], 1, sum)  >  thresh
  if (bool)  return (bool_vote)
  fill_bool(bool_vote, target, setdiff(total_from, target)[1])
}


# w = data.frame(row.names=names(sort(-table(a$X.amt_worker_ids))))
# w$num = table(a$X.amt_worker_ids)[row.names(w)]
  # list(w=w, u=u)


while("dlanalysis" %in% search())
  detach("dlanalysis")
attach(dlanalysis)
