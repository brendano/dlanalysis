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
      # msg("TRAINING UNITS"); print(units)
      subset = a[a$orig_id %in% units,]
      subset$orig_id = trim_levels(subset$orig_id)
      model_fn(subset, labels=labels, ...)
    },
    theta.predict = function(model, units) {
      # msg("TEST UNITS"); print(units)
      # msg("MODEL");      print(head(model[order(-model$num),],20));
      preds = pred_fn(model, a[a$orig_id %in% units,])
      # msg("PREDICTIONS");      print(preds)
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
}