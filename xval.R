# # maybe use http://rweb.stat.umn.edu/R/library/bootstrap/html/crossval.html instead
# dlanalysis$xval <- function(d, make_model, do_prediction, ...) {
#   folds = rep(1:k, nrow(d)/k + k)[1:nrow(d)]
#   pred = rep(NA, nrow(d))
#   for (fold in 1:k) {
#     data = d[folds!=fold,]
#     model = model_fn(..., data=data)
#     # pred[folds==fold] = predict(model, newdata=data[folds==fold,])
#     pred[folds==fold] = do_prediction(data, model)
#   }
#   
#   if ('gold' %in% names(d)) {
#     acc = mean(pred==d$gold)
#     cat(sprintf("Cross-validation accuracy: %.3f\n", acc))    
#   }
#   pred
# }

dlanalysis$xval_by_hit <- function(a,u, ...) {
  
}

dlanalysis$xval_by_unit <- function(a, u, 
    model_fn=function(...) worker_an(..., trim_workers=FALSE), 
    pred_fn=function(w,a) {
      map_estimate_labels(w,a)
    },
    labels=u$gold, ngroup=10, ...) {
  stopifnot( setequal(present_levels(a$orig_id), row.names(u)) )
  library(bootstrap)

  # crossval(1:10, y, function(x,y) lm(y~x), function(m,x) predict(m,list(x=x)))$cv.fit
  r = bootstrap::crossval(row.names(u), labels,
    theta.fit = function(units, labels) {
      subset = a[a$orig_id %in% units,]
      subset$orig_id = trim_levels(subset$orig_id)
      model_fn(subset, labels=labels, ...)
    },
    theta.predict = function(model, units) {
      preds = pred_fn(model, a[a$orig_id %in% units,])
      # they come back sorted by unit name.  have to reorder to the random order that crossval() gave
      # print(preds[units,]$label)
      as.c( preds[units,]$label )
    },
    ngroup = ngroup
  )
  r$acc = mean(r$cv.fit==labels)
  class(r) = 'xval_results'
  r
}


dlanalysis$print.xval_results <- function(r) {
  cat(sprintf("Cross-validation accuracy: %.3f\n", r$acc))
  cat(names(r),"\n")
}


dlanalysis$train_test_split <- function(a,u, train_perc=.75, rand=F, rotate=1) {
  train = rep(TRUE,nrow(u))
  test_inds = round(0.8*nrow(u)):nrow(u)
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


dlanalysis$xval_splits <- function(a,u, ngroup=5, rand=F) {
  folds = rep(1:ngroup, nrow(u)/ngroup + ngroup)[1:nrow(u)]
  
  if (rand)  folds = shuffle(folds)
  splits = sapply(1:ngroup, function(k) { train_test_split2(a,u, folds!=k) })
  splits
}

dlanalysis$xval_preds <- function(a,u, splits=NULL, model_fn=worker_an, pred_fn=map_estimate_labels, ngroup=10, rand=F, ...) {
  if (is.null(splits)) {
    splits = xval_splits(a,u, ngroup=ngroup, rand=rand)
  }
  all_est = data.frame(row.names=row.names(u))
  for (k in 1:ncol(splits)) {
    s = splits[,k]
    w = model_fn(a[ s$atrain, ], ...)
    est = pred_fn(w,a)
    if (k==1)  {
      all_est[,names(est)] = NA
      all_est$label = factor(all_est, levels=u@candidates)
    }
    all_est[s$test,] = est[s$test,]
    
    # cat(sprintf("fold %2d: PLURALITY: %.3f  CALIB: %.3f\n", k, mean(u$plurality[s$test] == u$gold[s$test]), mean(est$label[s$test] == u$gold[s$test])))
    
  }
  all_est
}
