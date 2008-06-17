dlanalysis$xval <- function(d, k, model_fn, ...) {
  folds = rep(1:k, nrow(d)/5 + 5)[1:nrow(d)]
  pred = rep(NA, nrow(d))
  for (fold in 1:k) {
    data = d[folds!=fold,]
    model = model_fn(..., data=data)
    pred[folds==fold] = predict(model, newdata=data[folds==fold,])
  }
  
  # acc = mean((pred>.5)==d$target, na.rm=T)
  # cat(sprintf("Cross-validation accuracy: %.3f\n", acc))
  
  pred
}