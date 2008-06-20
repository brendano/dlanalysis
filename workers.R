dlanalysis$worker_an = dlanalysis$mygeneric('worker_an')

dlanalysis$worker_an.categ <- function(a, labels=NULL, pseudocount=1,
  # pseudocount_diag=5, pseudocount_offdiag=1,
  pseudocounts=NULL,
  labels_long=a$gold, count_thresh=0, candidates=a@candidates,
  target_only=FALSE, diag_only=FALSE, counts=FALSE, lrs=FALSE, llrs=TRUE, probs=FALSE) {
  worker_ids = as.c(a$X.amt_w)
  
  # msg("LABELS");  print(labels)
  if ( !is.null(labels) ) {
    if ( is.null(names(labels)))
      names(labels) = levels(a$orig_id)
    a$label = flipleft(a,labels, 'orig_id') 
  } else if (!is.null(labels_long)) {
    a$label = labels_long
  } else stop("need labels somehow")
  
  if (is.null(pseudocounts)) {
    pseudocounts = matrix(pseudocount, nrow=length(candidates), ncol=length(candidates))
  }
  if (is.null(dimnames(pseudocounts))) {
    dimnames(pseudocounts) = list(candidates, candidates)
  }

  # print(a)
  worker_confusions = by(a, worker_ids, function(x){ table(x[,c('response','label')]) })
  # print(worker_confusions)   # prints extremely prettily
  
  worker_info = data.frame(
    # X.amt_worker_id = names(worker_confusions), 
    row.names = names(worker_confusions), 
    num = as.vector(table(worker_ids)),
    acc = dfagg(a, worker_ids, function(x) mean(x$response==x$label))
  )
  reals_for_pred = function(pred) { 
    if (target_only) a@target
    else if (diag_only) pred
    else candidates
  }
  for (pred in candidates) {
    for (real in reals_for_pred(pred)) {
      
      # log likelihood ratios
      if (llrs) {
        name = llr_name(pred,real)
        # print(worker_confusions)
        worker_info[,name] = sapply(worker_confusions, function(t) {
          if (sum(t) < count_thresh)
            t[,] = 0
          notreal = setdiff(candidates,real)
          log2(
            (t[pred,real] + pseudocounts[pred,real]) / 
            sum(t[pred,notreal] +sum(pseudocounts[pred,notreal]))
          )
        })        
      }
      
      if (counts) {
        name = count_name(pred,real)
        worker_info[,name] = sapply(worker_confusions, function(t) {
          t[pred,real]
        })
      }
      if (lrs && stop("wrong")) {
        # likelihood ratios
        name = lr_name(pred,real)
        worker_info[,name] = sapply(worker_confusions, function(t) {
          notreal = setdiff(candidates,real)
          (t[pred,real] + pseudocount) / 
            sum(t[pred,notreal] +pseudocount)
        })        
      }
      if (probs && stop("wrong")) {
        name = likelihood_name(pred,real)
        worker_info[,name] = sapply(worker_confusions, function(t) {  
          (t[pred,real] + pseudocount) / (sum(t[,real]) + pseudocount*length(candidates)) 
        })
        # likelihood ratio denominators
        # name = likelihood_name(pred,real,not_rhs=TRUE)
        # other_reals = setdiff(candidates,real)
        # worker_info[,name] = sapply(worker_confusions, function(t) {
        #   (sum(t[pred,other_reals]) + pseudocount*(length(candidates)-1)) / (sum(t[,other_reals]) + pseudocount*length(candidates)) 
        # })        
      } 
    }
  }
  
  # worker_info$X.amt_worker_ids = row.names(worker_info)
  # row.names(worker_info) = NULL
  worker_info
}

dlanalysis$uniform_worker_an <- function(a) {
  # dummy worker models for first EM step.  Using these has same effect as a
  # equally-weighted plurality voting rule.
  w = data.frame(row.names=levels(a$X.amt_worker_ids))
  for (pred in a@candidates)
    for (real in a@candidates)
      w[,llr_name(pred,real)] = if (pred==real) 1 else -1
  w = w[row.names(w) %in% as.c(unique(a$X.amt_worker_ids)), ]
  w
}

dlanalysis$likelihood_name <- function (pred,real, not_rhs=FALSE) {
  if (not_rhs)
    real = paste('~',real,  sep='')
  paste('p[',pred,'|',real,']',  sep='')
}

dlanalysis$lr_name <- function(pred, real) {
  paste('lr[', pred,'|',real, ']',  sep='')
}

dlanalysis$llr_name <- function(pred, real) {
  paste('llr[', pred,'|',real, ']',  sep='')
}

dlanalysis$count_name <- function(pred, real) {
  paste('c[',pred,'|',real,']', sep='')
}

dlanalysis$map_estimate_labels <- function(a, w, joint_posterior=FALSE, ...) {
  label_posteriors = posterior_given_workers(a,w,...)
  ret = list2df(apply(label_posteriors, 1, function(row) {
    list(label=a@candidates[which.max(row)], logit=row[which.max(row)])
  }))
  ret$label = factor(ret$label, levels=a@candidates)
  ret
  # apply(label_posteriors, 1, function(row) { a@candidates[which.max(row)] })
}

dlanalysis$posterior_given_workers = dlanalysis$mygeneric('posterior_given_workers')

dlanalysis$posterior_given_workers.categ <- function(a, w, 
    candidates=a@candidates,
    label_priors=sapply(candidates, function(x) 1/length(candidates) )
) {
  logit_priors = log2(p2o(label_priors))    # all 0
  # only use a$X.amt_worker_ids and a$response.
  join = mymerge(a, w, by='X.amt_worker_ids', row.y=TRUE)
  post_odds = dfagg(join, join$orig_id, dotprogress(function(x) {
    logits_per_label = lapply(candidates, function(label) {
      signal_llrs = sapply(1:nrow(x), function(i)
        x[i,  llr_name(x$response[i], label)]  )
      # print(signal_llrs)
      sum(signal_llrs) + logit_priors[label]
    })
    names(logits_per_label) = candidates
    logits_per_label
  }, interval=10))
  msg("\n")
  post_odds
}



dlanalysis$worker_em <- function(a, iterations=10, w=uniform_worker_an(a), labels=NULL, init_state=NULL, ...) {
  if ( !is.null(init_state)) {
    w = init_state$w
    labels = init_state$labels
  }
  if (is.null(labels))
    labels = map_estimate_labels(a,w)$label
  ret = list(..., history=list(list(w=w,labels=labels)))
  ret$init = ret$history[[1]]
  
  msg("EM initial state:")
  print(head(w))
  x=labels; names(x)=NULL; print(factor(x))

  for (iter in 2:iterations) {
    msg("--- EM iteration",iter,"---")
    
    w = worker_an(a, labels=labels, lrs=F, ...)
    # msg("New w:"); print(head(w[,c('acc','llr[1|1]','llr[0|0]')]))
    msg("New w:"); {print(head(w)); z=w[order(-w$num),]; print(head(z)); print(tail(z)) }
    
    map_est = map_estimate_labels(a,w)
    map_est<<-map_est
    labels = map_est$label
    msg("New labels:")
    x=labels; names(x)=NULL; print(factor(x))
    msg("Posterior logit:", sum(map_est$logit))
    lbs<<-labels
    ret$history[[iter]] = list(labels=labels, w=w, joint_logit=sum(map_est$logit))
    
    if (iter>1) {
      funparams = ngrep("^llr",names(w))
      print(dim(ret$history[[iter]]$w[,funparams]))
      print(dim(ret$history[[iter-1]]$w[,funparams]))
      
      w_movements = ret$history[[iter]]$w[,funparams] - ret$history[[iter-1]]$w[,funparams]
      w_movements = as.matrix(abs(w_movements))
      msg("LLR param movements mean=", mean(w_movements), "median=", median(w_movements))
      l_movements = ret$history[[iter]]$labels != ret$history[[iter-1]]$labels
      msg("Number of labels changed:", sum(l_movements), "of", length(labels))
      msg("New label distribution:", table(labels))
      
      if(sum(l_movements)==0 && mean(w_movements)==0) {
        msg("CONVERGED, exiting")
        break
      }
    }
    ret <<- ret
  }
  ret$last = ret$history[[ length(ret$history) ]]
  ret
}



dlanalysis$eval_em <- function(results,u=u) {
  msg("\n*** EM evaluation ***")
  r=results
  
  if( !is.null(results$count_thresh)) {
    t = results$count_thresh
    pass = results$init$w$num >= t
    msg("Count thresh",t," had ",sum(pass)," workers passing, comprising ",sum(results$init$w$num[ pass]),"of",sum(results$init$w$num)," judgments")
  }
  moves = which(r$init$label != r$last$label)
  msg(length(moves),"of",length(r$init$label),"labels moved.")
  # print(table(r$init$label!=r$last$label))
  
  msg("Moved labels (numeric indexes):")
  print(moves)
  
  msg("How many moves were good")
  move_goodness = rep(NA, length(moves))
  # init_m = r$init$label[moves]
  # last_m = r$last$label[moves]
  # gold_m = u$g
  move_goodness[ (r$init$label!=u$gold & r$last$label==u$gold)[moves] ] = 'correction'
  move_goodness[ (r$init$label==u$gold & r$last$label!=u$gold)[moves] ] = 'mistake'
  move_goodness[ (r$init$label!=u$gold & r$last$label!=u$gold)[moves] ] = 'no_change'
  print(table(move_goodness))
  print(table.freq(move_goodness))
  
  msg("Old acc=",mean(r$init$label==u$gold), " New acc=",mean(r$last$label==u$gold))
  
}

















# This somewhat infinitely hangs.  wtf
# dlanalysis$posterior_given_workers2.categ <- function(a, w, 
#     candidates=a@candidates,
#     label_priors=sapply(candidates, function(x) 1/length(candidates) )
# ) {
#   # only use a$X.amt_worker_id and a$response.
#   join = mymerge(a, w, by='X.amt_worker_ids', row.y=TRUE)
#   signal_llrs = data.frame(row.names=row.names(join))
#   for (l in candidates) signal_llrs[,l] = rep(NA,nrow(join))
#   print(signal_llrs)
#   # names(signal_llrs) = candidates
#   
#   for (label in candidates) {
#     x = join$response==label
#     signal_llrs[x,label] = join[x,llr_name(join$response[x], label)]
#   }
#   print(signal_llrs)
#   return(NULL)
#   
#   post_odds = dfagg(signal_llrs, join$orig_id, dotprogress(function(signal_llrs_for_unit) {
#     logits_per_label = lapply(candidates, function(label) {
#       sum(signal_llrs_for_unit[,label]) + log(p2o(label_priors[label]))
#     })
#     names(logits_per_label) = candidates
#     logits_per_label
#   },interval=10))
#   # post_odds = dfagg(join, join$orig_id, dotprogress(function(x) {
#   #   logits_per_label = lapply(candidates, function(label) {
#   #     signal_llrs = sapply(1:nrow(x), function(i)
#   #       x[i,llr_name(x$response[i], label)]  )
#   #     sum(signal_llrs) + log(p2o(label_priors[label]))
#   #   })
#   #   names(logits_per_label) = candidates
#   #   logits_per_label
#   # }, interval=10))
#   msg("\n")
#   post_odds
# }


# odds_per_label = lapply(candidates, function(label) {
#   # print(label)
#   signal_lratios = sapply(1:nrow(x), function(i)
#     x[i, likelihood_name(x$response[i], label)] / x[i, likelihood_name(x$response[i], label, not_rhs=TRUE)] )
#   # print(signal_lratios)
#   # print(prod(signal_lratios))
#   prod(signal_lratios)  *  label_priors[label]
# })
# names(odds_per_label) = candidates
# # opl<<-odds_per_label
# # print(odds_per_label)
# odds_per_label
