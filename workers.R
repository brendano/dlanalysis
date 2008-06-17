dlanalysis$worker_an = mygeneric('worker_an')

dlanalysis$worker_an.categ <- function(a, pseudocount=1, candidates=levels(a$gold)) {
  worker_confusions = by(a,a$X.amt_w,function(x){ table(x[,c('response','gold')]) })
  # print(worker_confusion_counts)   # prints extremely prettily
  
  worker_info = data.frame(
    # X.amt_worker_id = names(worker_confusions), 
    row.names = names(worker_confusions), 
    num = as.vector(table(a$X.amt_w)),
    acc = dfagg(a, a$X.amt_w, function(x) mean(x$response==x$gold))
  )
  for (pred in candidates) {
    for (real in candidates) {
      name = confusion_likelihood_name(pred,real)
      worker_info[,name] = sapply(worker_confusions, function(t) {  
        (t[pred,real] + pseudocount) / (sum(t[,real]) + pseudocount*length(candidates)) 
      })
      name = confusion_likelihood_name(pred,real,not_rhs=TRUE)
      other_reals = setdiff(candidates,real)
      worker_info[,name] = sapply(worker_confusions, function(t) {
        (sum(t[pred,other_reals]) + pseudocount*(length(candidates)-1)) / (sum(t[,other_reals]) + pseudocount*length(candidates)) 
      })
    }
  }
  # worker_info$X.amt_worker_ids = row.names(worker_info)
  # row.names(worker_info) = NULL
  worker_info
}

dlanalysis$confusion_likelihood_name <- function (pred,real, not_rhs=FALSE) {
  if (not_rhs)
    real = paste('~',real,  sep='')
  paste('p',pred,'|',real,  sep='')
}

dlanalysis$map_estimate_labels <- function(a,w, candidates=a@candidates ) {
  label_posteriors = posterior_given_workers(a,w,candidates=candidates)
  # i'll show *you* a posteriori
  apply(label_posteriors, 1, function(row) { candidates[which.max(row)] })
}

dlanalysis$posterior_given_workers = mygeneric('posterior_given_workers')

dlanalysis$posterior_given_workers.categ <- function(a, w, 
    candidates=a@candidates,
    label_priors=sapply(candidates, function(x) p2o( 1/length(candidates) ))
) {
  # only use a$X.amt_worker_id and a$response.
  # print(candidates)
  # print(label_priors)
  join = mymerge(a, w, by='X.amt_worker_ids', row.y=TRUE)
  post_odds = dfagg(join, join$orig_id, dotprogress(function(x) {
    odds_per_label = lapply(candidates, function(label) {
      # print(label)
      signal_lratios = sapply(1:nrow(x), function(i)
        x[i, confusion_likelihood_name(x$response[i], label)] / x[i, confusion_likelihood_name(x$response[i], label, not_rhs=TRUE)] )
      # print(signal_lratios)
      # print(prod(signal_lratios))
      prod(signal_lratios)  *  label_priors[label]
    })
    names(odds_per_label) = candidates
    # opl<<-odds_per_label
    # print(odds_per_label)
    odds_per_label
  }, interval=10))
  cat("\n")
  post_odds
}


# dlanalysis$worker_em <- 
# }