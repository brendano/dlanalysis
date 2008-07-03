

dlanalysis$worker_an.categ <- function(a, worker_ids,
  pseudocount=1,
  pseudocounts=NULL,
  pseudocount_diag=8, pseudocount_offdiag=2,  
  
  count_thresh=0, head_thresh=0, worker_restrict_list=NULL,
  globalize_tail_cutoff = -1,
  
  candidates=a@candidates,
  positive_hack=FALSE, restrict_hack_to_workers=NULL,
  target_only=FALSE, diag_only=FALSE, counts=FALSE, lrs=FALSE, llrs=TRUE, probs=FALSE)
{
  
  
  cs = candidates
  if (is.null(pseudocounts)) {
    if (is.null(pseudocount)) {
      pseudocounts = matrix(pseudocount_offdiag/(length(cs)-1), nrow=length(cs), ncol=length(cs))
      diag(pseudocounts) = pseudocount_diag
    } else {
      pseudocounts = matrix(pseudocount, nrow=length(cs), ncol=length(cs))
    }
  }
  if (is.null(dimnames(pseudocounts))) {
    dimnames(pseudocounts) = list(cs, cs)
  }

  worker_confusions = by(a, worker_ids, function(x){ table(x[,c('response','label')]) })
  
  zero_table = matrix(0, length(cs),length(cs), dimnames=list(response=cs,label=cs))
  
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
  
  if (!is.null(worker_restrict_list)) {
    ignore_these = setdiff(names(worker_confusions), worker_restrict_list)
    worker_confusions[ignore_these] = zero_table  # UNTESTED
  }
  
  if (count_thresh>0) stop("fixme")
  
  worker_info = data.frame(
    row.names = names(worker_confusions), 
    num = as.vector(table(worker_ids)),
    acc = dfagg(a, worker_ids, function(x) mean(x$response==x$label), trim=FALSE)
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
          notreal = setdiff(candidates,real)
          t = t + pseudocounts
          log2(t[pred,real] / sum(t[pred,notreal]))
        })
      }
      
      if (counts) {
        name = count_name(pred,real)
        worker_info[,name] = sapply(worker_confusions, function(t) {
          t = t + pseudocounts
          t[pred,real]
        })
      }
      if (lrs && stop("fixme")) {
        # likelihood ratios
        name = lr_name(pred,real)
        worker_info[,name] = sapply(worker_confusions, function(t) {
          notreal = setdiff(candidates,real)
          (t[pred,real] + pseudocount) / 
            sum(t[pred,notreal] +pseudocount)
        })        
      }
      if (probs && stop("fixme")) {
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
  
  if (globalize_tail_cutoff >= 0) {
    w_tail = row.names(worker_info)[worker_info$num <= globalize_tail_cutoff]
    global_pool = worker_info   # upper threshold maybe?
    global_confusion = table(a$response, a$gold)
    global_confusion = global_confusion + pseudocounts
    # cat("global confusion:"); print(global_confusion) 
    for (pred in candidates)  for (real in candidates) {
      otherreal = setdiff(candidates, real)
      worker_info[w_tail, llr_name(pred,real)] = 
          log2(global_confusion[pred,real]) - log2(sum(global_confusion[pred,otherreal]))
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
  
  attr(worker_info, 'candidates') = candidates
  worker_info
}

dlanalysis$fit_anno_model.categ <- function(a, ...) {
  ret = list(w = worker_an(a, ...))
  ret
}



dlanalysis$uniform_worker_an <- function(a) {
  # dummy worker models for first EM step.  Using these has same effect as an
  # equally-weighted plurality voting rule.
  w = data.frame(row.names=levels(a$X.amt_worker_ids))
  for (pred in a@candidates)
    for (real in a@candidates)
      w[,llr_name(pred,real)] = if (pred==real) 1 else -1
  w = w[row.names(w) %in% as.c(unique(a$X.amt_worker_ids)), ]
  attr(w,'candidates') = a@candidates
  w
}

dlanalysis$acc_based_llr <- function(w) {
  diag_c = count_name(w@candidates,w@candidates)
  all_c = sapply(xprod(w@candidates,w@candidates), function(z) count_name(z$x,z$y))
  offdiag_c = setdiff(all_c,diag_c)
  
  diag_llr = log2(apply(w[,diag_c],1,sum)) - log2(apply(w[,offdiag_c],1,sum))  # the big fun vote weight.
  offdiag_llr = -diag_llr

  diag_l = llr_name(w@candidates,w@candidates)
  all_l = sapply(xprod(w@candidates,w@candidates), function(z) llr_name(z$x,z$y))
  offdiag_l = setdiff(all_l,diag_l)

  w[,diag_l] = diag_llr
  w[,offdiag_l] = offdiag_llr
  # if (pred==real)
  #   w[,llr_name(pred,real)] = log2( w$acc / (1-w$acc) )
  # else
  #   w[,llr_name(pred,real)] = -log2( w$acc / (1-w$acc) )
  # print(w[,'llr[1|1]'] == w[,'llr[0|0]'])
  w
}

dlanalysis$likelihood_name <- function (pred,real, not_rhs=FALSE) {
  if (not_rhs)
    real = paste('~',real,  sep='')
  paste('p[',pred,'|',real,']',  sep='')
}

dlanalysis$lr_name <- function(pred, real)  paste('lr[', pred,'|',real, ']',  sep='')

dlanalysis$llr_name <- function(pred, real)  paste('llr[', pred,'|',real, ']',  sep='')

dlanalysis$count_name <- function(pred, real)  paste('c[',pred,'|',real,']', sep='')

dlanalysis$label_posterior <- dlanalysis$mygeneric("label_posterior",2)

dlanalysis$label_posterior.categ <- function(m, a, ...) {
  # stop("fixme use model not worker table")
  label_posteriors = posterior_given_workers(m$w,a,...)
  # if (a@data_type=='numeric')  return(label_posteriors)
  
  ret = list2df(apply(label_posteriors, 1, function(row) {
    list(label=a@candidates[which.max(row)], logit=row[which.max(row)])
  }))
  ret$label = factor(ret$label, levels=a@candidates)
  ret
  # apply(label_posteriors, 1, function(row) { a@candidates[which.max(row)] })
}

dlanalysis$posterior_given_workers = dlanalysis$mygeneric('posterior_given_workers',2)
dlanalysis$posterior_given_workers1 = dlanalysis$mygeneric('posterior_given_workers1',2)

dlanalysis$posterior_given_workers.categ <- function(w, a,
    candidates=a@candidates, trim_units=TRUE,
    label_priors=sapply(candidates, function(x) 1/length(candidates) ),
    use=NULL) 
{
  logit_priors = log2(p2o(label_priors))    # all 0
  if (trim_units)  a$orig_id = trim_levels(a$orig_id)
  
  wm = worker_llr_matrix(w)
  anno_llr_matrix = calculate_anno_llrs(wm, a$X.amt_worker_ids, a$response, a@candidates)

  unit_posteriors = dfagg(anno_llr_matrix, a$orig_id, function(x) apply(x,2,sum))
  
  repeated_priors = rep(1, nrow(unit_posteriors))  %*%  t(logit_priors)
  unit_posteriors = unit_posteriors + repeated_priors
  
  unit_posteriors
}


dlanalysis$calculate_anno_llrs <- function(wm, worker_ids, responses, candidates) {
  N = length(responses);   stopifnot(N == length(worker_ids))
  cs = candidates
  anno_llr_matrix = matrix(-42.0, nrow=N, ncol=length(cs))
  
  anno_llr_matrix = .C("calculate_anno_llrs_c", as.double(wm), as.integer(worker_ids), as.integer(responses),
    as.integer(N), as.integer(length(cs)), as.integer(dim(wm)[1]),
    output=as.double(anno_llr_matrix)
  )$output
  anno_llr_matrix = matrix(anno_llr_matrix, nrow=N, ncol=length(cs), dimnames=list(NULL,cs))
  anno_llr_matrix
}

dlanalysis$worker_llr_matrix <- function(w, cs=w@candidates) {
  m = array(NA, c(nrow(w), length(cs), length(cs)), list(row.names(w), cs,cs))
  for (pred in cs)  for (real in cs)  
    m[,pred,real] = w[,llr_name(pred,real)]
  m
}

dlanalysis$posterior_given_workers1.categ <- function(w, a, 
    candidates=a@candidates, trim_units=TRUE,
    label_priors=sapply(candidates, function(x) 1/length(candidates) ))
{
  logit_priors = log2(p2o(label_priors))    # all 0
  print(logit_priors)
  # only use a$X.amt_worker_ids and a$response.
  join = timeit(  mymerge(a, w, by='X.amt_worker_ids', row.y=TRUE)  )
  if (trim_units)  join$orig_id = trim_levels(join$orig_id)
  post_odds = 
        timeit({
  dfagg(join, join$orig_id, dotprogress(function(x) {
    logits_per_label = lapply(candidates, function(label) {
      signal_llrs = sapply(1:nrow(x), function(i)
        x[i,  llr_name(x$response[i], label)]  )
      sum(signal_llrs) + logit_priors[label]
    })
    names(logits_per_label) = candidates
    logits_per_label
  }, interval=10))
        })
  msg("\n")
  post_odds
}

dlanalysis$worker_em <- function(a, iterations=10, w=uniform_worker_an(a), labels=NULL, ...) {
  if (is.null(labels))
    labels = label_posterior(w,a)$label
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
    
    map_est = label_posterior(w,a)
    labels = map_est$label
    # msg("New labels:")
    # x=labels; names(x)=NULL; print(factor(x))
    msg("New label distribution:"); print(table(labels))

    msg("Posterior logit:", sum(map_est$logit))
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
