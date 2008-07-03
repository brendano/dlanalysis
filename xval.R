dlanalysis$xval_by_hit <- function(a,u, ...) {  
}


# # weird bugs in this
# dlanalysis$xval_by_unit <- function(a, u, 
#     model_fn=function(...) worker_an(..., trim_workers=FALSE), 
#     pred_fn=function(w,a) {
#       label_posterior(w,a)
#     },
#     labels=u$gold, ngroup=10, ...) {
#   stopifnot( setequal(present_levels(a$orig_id), row.names(u)) )
#   library(bootstrap)
# 
#   # crossval(1:10, y, function(x,y) lm(y~x), function(m,x) predict(m,list(x=x)))$cv.fit
#   r = bootstrap::crossval(row.names(u), labels,
#     theta.fit = function(units, labels) {
#       subset = a[a$orig_id %in% units,]
#       subset$orig_id = trim_levels(subset$orig_id)
#       model_fn(subset, labels=labels, ...)
#     },
#     theta.predict = function(model, units) {
#       preds = pred_fn(model, a[a$orig_id %in% units,])
#       # they come back sorted by unit name.  have to reorder to the random order that crossval() gave
#       # print(preds[units,]$label)
#       as.c( preds[units,]$label )
#     },
#     ngroup = ngroup
#   )
#   r$acc = mean(r$cv.fit==labels)
#   class(r) = 'xval_results'
#   r
# }
# 
# 
# dlanalysis$print.xval_results <- function(r) {
#   cat(sprintf("Cross-validation accuracy: %.3f\n", r$acc))
#   cat(names(r),"\n")
# }


dlanalysis$train_test_split <- function(a,u, train_perc=.75, rand=F, rotate=1) {
  train = rep(TRUE,nrow(u))
  test_inds = round(train_perc*nrow(u)):nrow(u)
  test_inds = (test_inds - (rotate-1)*length(test_inds)) %% nrow(u)
  train[test_inds] = FALSE
  if (rand)  train = shuffle(train)
  
  train_test_split2(a,u, train)
}

dlanalysis$train_test_split2 <- function(a,u, train) {
  test = !train
  atrain = a$orig_id %in% row.names(u)[train]
  atest = !atrain
  list(train=train, test=test,  atrain=atrain, atest=atest)  
}


dlanalysis$xval_splits <- function(a,u, ngroup=5, rand=F, loo=FALSE) {
  if (loo)  ngroup = nrow(u)
  if (loo && rand) stop ("bad combination")
  
  folds = rep(1:ngroup, nrow(u)/ngroup + ngroup)[1:nrow(u)]
  if (rand)  folds = shuffle(folds)
  splits = sapply(1:ngroup, function(k) { train_test_split2(a,u, folds!=k) })
  attr(splits, 'folds') = folds
  splits
}

dlanalysis$xval_preds <- function(a,u, splits=NULL, model_fn=fit_anno_model, pred_fn=label_posterior, ngroup=5, rand=F, loo=F, ...) {
  if (is.null(splits)) {
    splits = xval_splits(a,u, ngroup=ngroup, rand=rand, loo=loo)
  }

  models = list()
  units = list()
  all_est = data.frame(row.names=row.names(u))
  cat(ncol(splits),"-fold: ", sep='')
  for (k in 1:ncol(splits)) {
    cat(k,"")
    s = splits[,k]
    # print(s)
    m = model_fn(a[ s$atrain, ], ...)
    est = pred_fn(m,a)
    if (k==1)  {
      all_est[,names(est)] = NA
      # if (a@data_type=='categ')
      all_est$label = factor(all_est, levels=a@candidates)
    }
    all_est[s$test,] = est[s$test,]
    
    models[[k]] = m
    units[[k]] = row.names(u)[s$train]
    
    # cat(sprintf("fold %2d: PLURALITY: %.3f  CALIB: %.3f\n", k, mean(u$plurality[s$test] == u$gold[s$test]), mean(est$label[s$test] == u$gold[s$test])))
    
  }  
  cat("\n")
  all_est
}


dlanalysis$print.xval_results <- function(r) {
  print(names(r))
}





dlanalysis$new_xval <- function(a,u, size=2, replications=40, ...) {
  N = 10
  replications = min(choose(N,size),replications)
  combo_matrix = combn(N, size)
  combos = sample(1:choose(N,size), replications)
  msg("Replications=",replications," on combinations=",choose(N,size))
  dfapply(levels(a$orig_id), function(uid) {
    inds = shuffle(which(a$orig_id==uid))
    # supermodel = fit_anno_model(a[a$orig_id!=uid,])
    x = dfapply(1:replications, function(i) {
      
      workers = a$X.amt_w[ inds[combo_matrix[,i]] ]
      atrain = trim_levels(a[a$X.amt_w %in% workers & a$orig_id != uid,])
      # print(atrain)
      m = fit_anno_model(atrain)
      # m = fit_anno_model(atrain, prior_mean=supermodel$prior_mean, prior_sd=supermodel$prior_sd)
      # print(m)
      p = label_posterior(m, trim_levels(a[a$orig_id==uid,]),
            ...)
      # print(p)
      list(calib=p$mean, raw=-42)
      # list(calib=p$mean, raw=a$response[inds[combo_matrix[,i]]])
      # list(calib=runif(1), raw=mean(a$response[inds[combo_matrix[,i]]]))
    })
    cat(sprintf("%s: gold %3s  full %.1_4f  calib %.1_4f  raw %.1_4f  ::  %s  ::  [CI %s]\n", 
        uid, u[uid,'gold'], u[uid,'mean_response'], mean(x$calib), mean(x$raw), 
        if(nrow(x)<=10) paste(sprintf("%.1_4f",sort(x$calib)),collapse=' ') else "", 
        paste( sprintf("%.1_4f",try(t.test(x$calib)$conf.int)), collapse=' ')
    ))

    list(calib=mean(x$calib), raw=mean(x$raw))
  })
}


dlanalysis$xval3000 <- function(a, test_groups=levels(a$orig_id), ...) {
  dfapply(test_groups, function(uid) {
    test_b = which(a$orig_id %in% uid)
    workers = unique(a$X.amt_w[ test_b ])
    atrain = trim_levels(a[a$X.amt_w %in% workers & !(a$orig_id %in% uid),])
    # atrain = trim_levels(a[-test_b,])
    cat("HERE 1\n")
    m = fit_anno_model(atrain)                             #, name='fitmodel')
    cat("HERE 2 \n")
    p = label_posterior(m, trim_levels(a[test_b,]), ...) #, name='post')
    cat("HERE 3 \n")
    if (a@data_type=='numeric')
      list(calib=p$mean, raw=mean(a$response[test_b]))
    else
      # TODO random ties for even numbered levels!!
      list(calib=p$label, raw=agg_to_unit(a)$plurality)
  })
}


# dlanalysis$


