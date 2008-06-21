dlanalysis$worker_an = dlanalysis$mygeneric('worker_an')

dlanalysis$worker_an.categ <- function(a, labels=NULL, 
  pseudocount=NULL, pseudocounts=NULL,
  pseudocount_diag=8, pseudocount_offdiag=2,
  trim_workers=TRUE, trim_units=TRUE,
  labels_long=a$gold, count_thresh=0, head_thresh=0, worker_restrict_list=NULL,
  candidates=a@candidates,
  positive_hack=FALSE, restrict_hack_to_workers=NULL,
  target_only=FALSE, diag_only=FALSE, counts=FALSE, lrs=FALSE, llrs=TRUE, probs=FALSE) {
  
  worker_ids = a$X.amt_worker_ids 
  if (trim_workers)  worker_ids = trim_levels(worker_ids)
  if (trim_units)  a$orig_id = trim_levels(a$orig_id)
  
  # msg("LABELS");  print(labels)
  if ( !is.null(labels) ) {
    if (length(labels) != length(levels(a$orig_id))) stop("uhoh")
    a$label = flipleft(a,labels, 'orig_id')
  } else if (!is.null(labels_long)) {
    a$label = labels_long
  } else stop("need labels somehow")
  
  if (is.null(pseudocounts)) {
    if (is.null(pseudocount)) {
      pseudocounts = matrix(pseudocount_offdiag/(length(candidates)-1), nrow=length(candidates), ncol=length(candidates))
      diag(pseudocounts) = pseudocount_diag
    } else {
      pseudocounts = matrix(pseudocount, nrow=length(candidates), ncol=length(candidates))
    }
  }
  if (is.null(dimnames(pseudocounts))) {
    dimnames(pseudocounts) = list(candidates, candidates)
  }

  worker_confusions = by(a, worker_ids, function(x){ table(x[,c('response','label')]) })
  
  zero_table = matrix(0, length(candidates),length(candidates), dimnames=list(response=candidates,label=candidates))
  
  if (head_thresh > 0) {
    j_mass = cumsum(sort(table(a$X.amt_w),decreasing=T))
    top_mass = j_mass[j_mass < head_thresh * nrow(a)]
    worker_restrict_list = names(top_mass)
  }
  
  for (worker in levels(worker_ids)) {
    if (is.null(worker_confusions[[worker]])) {
      worker_confusions[[worker]] = zero_table
    }
  }
  
  # wc<<-worker_confusions
  
  # print(worker_confusions)
  if (!is.null(worker_restrict_list)) {
    # mask = names(worker_confusions) != worker_restrict_list
    ignore_these = setdiff(names(worker_confusions), worker_restrict_list)
    worker_confusions[ignore_these] = zero_table  # UNTESTED
    # worker_confusions[ignore_these] = lapply(worker_confusions[ignore_these] , function(t) t*0)
  }
  
  worker_info = data.frame(
    # X.amt_worker_id = names(worker_confusions), 
    row.names = names(worker_confusions), 
    num = as.vector(table(worker_ids)),
    acc = dfagg(a, worker_ids, function(x) mean(x$response==x$label), trim=FALSE)
  )
  # print(worker_info)
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
          t = t + pseudocounts
          log2(t[pred,real] / t[pred,notreal])
          #   (t[pred,real] + pseudocounts[pred,real]) / 
          #   sum(t[pred,notreal] +sum(pseudocounts[pred,notreal]))
          # )
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
  
  if (positive_hack) {
    worker_mask = rep(TRUE, nrow(worker_info))
    if (!is.null(restrict_hack_to_workers))
      worker_mask[ row.names(worker_info) %in% restrict_hack_to_workers ] = FALSE
    
    for (llr in llr_name(candidates,candidates)) {
      worker_info[worker_mask  & worker_info[,llr] < .1, llr] = .1
      worker_info[worker_mask  & worker_info[,llr] > 5, llr] = 5
    }
    others = setdiff(names(worker_info), llr_name(candidates,candidates))
    for (llr in others) {
      worker_info[worker_mask  & worker_info[,llr] > -.1, llr] = -.1 / (length(candidates)-1)
      worker_info[worker_mask  & worker_info[,llr] < -5, llr] = -5 / (length(candidates)-1)
    }
  }
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

dlanalysis$map_estimate_labels <- function(w, a, joint_posterior=FALSE, ...) {
  label_posteriors = posterior_given_workers(w,a,...)
  ret = list2df(apply(label_posteriors, 1, function(row) {
    list(label=a@candidates[which.max(row)], logit=row[which.max(row)])
  }))
  ret$label = factor(ret$label, levels=a@candidates)
  ret
  # apply(label_posteriors, 1, function(row) { a@candidates[which.max(row)] })
}

dlanalysis$posterior_given_workers = dlanalysis$mygeneric('posterior_given_workers',2)

dlanalysis$posterior_given_workers.categ <- function(w, a, 
    candidates=a@candidates, trim_units=TRUE,
    label_priors=sapply(candidates, function(x) 1/length(candidates) )
) {
  logit_priors = log2(p2o(label_priors))    # all 0
  # only use a$X.amt_worker_ids and a$response.
  join = mymerge(a, w, by='X.amt_worker_ids', row.y=TRUE)
  if (trim_units)  join$orig_id = trim_levels(join$orig_id)
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



dlanalysis$worker_em <- function(a, iterations=10, w=uniform_worker_an(a), labels=NULL, ...) {
  if (is.null(labels))
    labels = map_estimate_labels(w,a)$label
  ret = list(..., history=list(list(w=w,labels=labels)))
  ret$init = ret$history[[1]]
  
  msg("EM initial state:")
  print(head(w))
  msg("New label distribution:"); print(table(labels))
  # x=labels; names(x)=NULL; print(factor(x))

  for (iter in 2:iterations) {
    msg("--- EM iteration",iter,"---")
    
    w = worker_an(a, labels=labels, lrs=F, ...)
    msg("New w:"); print(head(w[,c('num','acc',llr_name(a@candidates,a@candidates))]))
    # msg("New w:"); {print(head(w)); z=w[order(-w$num),]; print(head(z)); print(tail(z)) }
    
    map_est = map_estimate_labels(w,a)
    # map_est<<-map_est
    labels = map_est$label
    # msg("New labels:")
    # x=labels; names(x)=NULL; print(factor(x))
    msg("New label distribution:"); print(table(labels))

    msg("Posterior logit:", sum(map_est$logit))
    # lbs<<-labels
    ret$history[[iter]] = list(labels=labels, w=w, joint_logit=sum(map_est$logit))
    
    if (iter>1) {
      funparams = ngrep("^llr",names(w))      
      w_movements = ret$history[[iter]]$w[,funparams] - ret$history[[iter-1]]$w[,funparams]
      w_movements = as.matrix(abs(w_movements))
      msg("LLR param movements mean=", mean(w_movements), "median=", median(w_movements))
      l_movements = ret$history[[iter]]$labels != ret$history[[iter-1]]$labels
      msg("Number of labels changed:", sum(l_movements), "of", length(labels))
      
      if(sum(l_movements)==0 && mean(w_movements)==0) {
        msg("CONVERGED, exiting")
        break
      }
    }
  }
  ret$last = ret$history[[ length(ret$history) ]]
  ret
}



dlanalysis$eval_em <- function(results,u=u) {
  msg("\n*** EM evaluation ***")
  r=results
  
  if( !is.null(results$count_thresh)) {
    t = results$count_thresh
    w_num = results$history[[2]]$w$num
    msg("Count thresh",t," had ",sum(w_num>=t)," workers passing, comprising ",sum(w_num[w_num>=t]),"of",sum(w_num)," judgments")
  }
  moves = which(r$init$label != r$last$label)
  msg(length(moves),"of",length(r$init$label),"labels moved")

  # msg("Moved labels (numeric indexes):"); print(moves)
  
  ret = list()
    
  move_goodness = rep(NA, length(moves))
  # init_m = r$init$label[moves]
  # last_m = r$last$label[moves]
  # gold_m = u$g
  move_goodness[ (r$init$label!=u$gold & r$last$label==u$gold)[moves] ] = 'correction'
  move_goodness[ (r$init$label==u$gold & r$last$label!=u$gold)[moves] ] = 'mistake'
  move_goodness[ (r$init$label!=u$gold & r$last$label!=u$gold)[moves] ] = 'no_change'
  t = table(move_goodness)
  msg(sprintf("How many moves were good (p=%.3f)", if(sum(t)>0) binom.test(t['correction'],sum(t))$p.value else NA))
  print(t)
  print(table.freq(move_goodness))

  ret$old_acc = mean(r$init$label==u$gold)
  ret$acc = mean(r$last$label==u$gold)
  msg("Old acc=",ret$old_acc, " New acc=", ret$acc)
  
  invisible(ret)
}


dlanalysis$merge_worker_tail <- function(a, cutoff=20) {
  w = worker_an(a)
  tail_workers = row.names(w)[ w$num <= cutoff ]
  a$X.amt_worker_ids = factor(a$X.amt_worker_ids, levels=c('mr_tail', levels(a$X.amt_w)))
  a[a$X.amt_worker_ids %in% tail_workers, 'X.amt_worker_ids'] = 'mr_tail'
  a$X.amt_worker_ids = trim_levels(a$X.amt_worker_ids)
  a
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
