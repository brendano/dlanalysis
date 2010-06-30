# util.R:
# Utilities to make R a happier place
# Brendan O'Connor, brenocon@gmail.com


options(showWarnCalls=T, showErrorCalls=T)

if ( (numcol <-Sys.getenv("COLUMNS")) != "") {
  numcol = as.integer(numcol)
  options(width= numcol - 1)
} else if (system("stty -a &>/dev/null") == 0) {
  # mac specific?  probably bad in the R GUI too.
  numcol = as.integer(sub(".* ([0-9]+) column.*", "\\1", system("stty -a", intern=T)[1]))
  if (numcol > 0)
    options(width=  numcol - 1 )
}


util = new.env()

######
#
# dataframe-outputting apply and aggregation functions.
#  dfagg, dfapply, df2matrix, matrix2df
# i'm often confused whether proper R style should emphasize matrices or dataframes.
# so these support for a dataframe-centric lifestyle.
#
# Many of these are subsumed by the plyr library.




util$dfagg <- function(d, byvals, fn, trim=TRUE) {
  # like by() but usually returns dataframes:
  #    if fn() returns a list, a data frame is returned.
  #      -> byvals are the row names.
  #      -> each list is coerced into a row.
  #    if fn() returns a nonlist, a vector is returned.
  #      -> byvals are the names.
  # We attempt to be tolerant for slight inconsistencies in fn()'s return values.
  #
  # Goal is to be like SQL GROUP BY: dataframes in, aggregated dataframes out.
  #
  # If you have a multidimensional matrix (R calls "array"), apply() lets you 
  # select the margin for rollup in a similar way.
  #
  # ALTERNATIVE: ddply() from hadley wickham's plyr: http://had.co.nz/plyr/

  if (class(byvals) == 'function')
    byvals = byvals(d)
  if (trim && is.factor(byvals) && !setequal( present_levels(byvals), levels(byvals)) ) {
    # change to "stop" to find if necessary
    warning("Uhoh, byvals is a factor but only using only a subset of its levels.  Trimming them.  Hopefully this is what you want.")
    byvals = trim_levels(byvals)
  }

  b = by(d, byvals, fn)
  list2df(b)
}

util$list2df <- function(ls) {
  # Wants a list of lists, each of which has the same set named indexes.
  # Outputs a dataframe where said indexes are the column names.
  # Is tolerant for slight inconsistencies of present indexes.
  # Transfers list index names to dataframe rownames.
  #
  # ALTERNATIVE: ldply() -- i think -- from http://had.co.nz/plyr/
  
  b=ls
  cols = NULL
  for (i in 1:min(100,length(b))) {
    cols = c(cols, try(names(b[[i]])))
  }
  cols = unique(cols)

  dynamic_returns = (
    if (class(b[[1]]) == "data.frame") TRUE
    else if (class(b[[1]]) == "list") FALSE
    else FALSE
    # else stop(paste("don't know how to aggregate returns of type",class(b[[1]])))
  )

  ret = NULL
  if ( dynamic_returns ) {
    ret = as.list(rep(0, length(cols)))
    names(ret) = cols
    ret = data.frame(ret)[0,]

    for (i in 1:length(b)) {
      ret = rbind(ret, b[[i]])
    }
  } else {
    ret = data.frame(row.names=names(b))
    for (col in cols) {
      # print(col)
      ret[,col] = sapply(names(b), function(k) {
        if (is.null(b[[k]])) {
          NA
        } else if (!is.null(names(b[[k]]))) { 
          b[[k]][[col]] 
        } else if (length(b[[k]])==1 && is.na(b[[k]])) {
          NA 
        } else stop("dont know what to do with value ",b[[k]])
      })
    }
    if (length(cols) == 0) {
      return(sapply(names(b), function(k) b[[k]]))
    }
  }
  ret
}

util$matrix2df <- function(x) {
  # sapply() with fn() yielding lists returns a matrix with named rows/cols ... 
  # and whenever you name-index into this thing it return a list ... yuck
  # make that shit more normal.
  if (class(x) != 'matrix') stop("why is class ",class(x))
  colnames = dimnames(x)[[2]]
  if (nrow(x) > 1)
    data.frame(
      sapply(colnames, function(n) unlist(x[,n])),
      row.names=row.names(x))
  else
    # because sapply returns a named vector in this case...
    data.frame(
      t(sapply(colnames, function(n) unlist(x[,n]))),
      row.names=row.names(x))
}

util$dfapply <- function(collection, fn) {
  # like sapply/lapply except it expects fn() to yield lists.
  # each list gets coerced into a single row of a returned dataframe.
  # ALTERNATIVE: adply() -- i think -- from http://had.co.nz/plyr/

  r = sapply(collection, fn)
  r = base::t(r)
  # sapply gives real f'd up stuff for singleton list return values.  compare replicate(10,list(a=unif(1))) vs replicate(10,list(a=runif(1),b=runif(1)).  and the transposes are weirder
  if (length(unique(dimnames(r)[[2]])) == 1) {
    r = base::t(r)
    dimnames(r) = list(NULL, unique(dimnames(r)[[1]]))
  }
  r = matrix2df(r)
  row.names(r) = collection
  r
}


util$df2matrix <- function(d, bycols, targetcol, 
      targetfn = if (is.numeric(d[,targetcol])) mean else most_common)
{
  # for df's that essentially store sparse matrices.  make a real matrix via 
  # by()-like conditioning on multiple columns ... a contingency table.
  # Design goal: inspired by table(), which does the same thing, except cells are always counts.
  #
  # This is *NOT* the inverse of matrix2df !  would be good to change naming.
  #
  # e.g. you want to know the effects of "ps" and "t" on "acc", marginalizing out "size": 
  # > head(d)
  #   size           ps  t acc
  # 1    2 0.0009765625 -1 668
  # 2    2 0.0009765625  0 668
  # 3    2 0.0009765625 20 670
  # 4    2 0.0009765625 50 664
  # 
  # you do:
  # > df2matrix(head(d), c('ps','t'), 'acc', mean)
  #               -1   0  20  50
  # 0.0009765625 668 668 670 664
  # 0.5          668 668  NA  NA
  #
  # then heatmap(.Last.value, Rowv=NA,Colv=NA,scale='none') or whatever else your heart desires
  #
  # ALTERNATIVE: daply() from hadley wickham's plyr: http://had.co.nz/plyr/

  for (j in 1:length(bycols))
    d[,bycols[j]] = factor(d[,bycols[j]])

  the_dimnames = lapply(1:length(bycols),  function(j)  levels((d[,bycols[j]])) )

  # the by() cascade:
  # we want, for bycols=c('ps','t') and targetcol='acc', finalfn=mean:
  #     by(d,d$ps, function(x) by(x,x$t, function(x) mean(x$acc)))
  # so recursively build that linked list of closures, from right to left.
  by_cascade = list()
  by_cascade[[length(bycols)+1]] = function(x) targetfn(x[,targetcol])
  
  for (j in length(bycols):1) {
    by_cascade[[j]] = with(list(j=j),
      function(x) {
        by(x, x[,bycols[j]], by_cascade[[j+1]])
      }
    )
  }

  b = by_cascade[[1]](d)
  m = array(NA, dim=sapply(the_dimnames,length), dimnames=the_dimnames)

  # simplest and slowest: dont use any margins for assignments.
  # yes, this would be extremely speedy in c++
  all_spots = multi_xprod(lapply(1:length(bycols), function(j) 1:length(the_dimnames[[j]])))
  for (i in 1:length(all_spots)) {
    inds = all_spots[[i]]
    m[t(inds)] = b[[inds]]
  }
  m
}

util$kill_df_lists <- function(d) {
  # if you have internal lists inside your dataframe.  if you always use
  # matrix2df this should never happen.  but sometimes it does.  yikes!  
  for(n in names(d))
    if (is.list(d[,n]))
      d[,n] = list2v(d[,n])
  d
}

util$flipleft <- function(x, named_vec, by) {
  # Kinda dangerous but sometimes convenient: 
  # Left join data frame `x` against named_vec, matching named_vec's names
  # against a column in x.
  # Returns the new column as a bare vector, same height as x.
  if (is.null(names(named_vec))) {
    stopifnot(length(named_vec) == nlevels(x[,by]))
    names(named_vec) = levels(x[,by])    
  }
  y = data.frame(row.names=names(named_vec), ze_y_value=named_vec)
  x$ze_orig_order = 1:nrow(x)  
  merged = merge(x,y, by.x=by, by.y=0, all.x=T, all.y=F)

  merged$ze_y_value[order( merged$ze_orig_order )]
}

## now in plyr 0.19 as summiarise() http://github.com/hadley/plyr/blob/master/NEWS
# util$reframe = function(.data, ...) { 
#   e = eval(substitute(list(...)), .data, parent.frame()) 
#   data.frame(e) 
# } 

#######

util$read.tsv <- function(...)  read.delim(..., quote='', comment='', stringsAsFactors=FALSE)  # honest-to-goodness vanilla tsv with header

util$msg <- function(...)  cat(..., "\n", file=stderr())

util$strlen <- function(s)  length(strsplit(s,"")[[1]])

util$strmatch <- function(pat,s)  length(grep(pat,s)) > 0

util$strstrip <- function(s)  gsub("^\\s*|\\s*$", "", s)

util$as.c <- as.character

util$is_empty <- function(collection)  length(collection) == 0

util$first <- function(x)  head(x, 1)

util$last <- function(x)  tail(x, 1)

util$unwhich <- function(indices, len=length(indices)) {
  ret = rep(F,len)
  ret[indices] = T
  ret
}

util$nna <- function(...) !is.na(...)   # i type this a lot, i think its worth 3 characters + shift key

util$kna <- function(x) x[nna(x)]  # kill NA's (from vector)

# hm: unitnorm and rescale are both subsumed in reshape:rescaler

util$unitnorm <- function(x, na.rm=FALSE, ...)  (x - mean(x,na.rm=na.rm,...)) / sd(x,na.rm=na.rm)

util$renorm <- function(x, mean=0, sd=1, ...)  (unitnorm(x,...) * sd) + mean

util$rescale <- function(x, bounds=range(x))  (x-bounds[1]) / (bounds[2]-bounds[1])

util$rbern <- function(n, p=0.5)  rbinom(n, size=1, prob=p)

# util$my_rmultinom <- function(n, w=c(1,1,8)) {
#   # because rmultinom() doesn't make sense to me
#   # w are per-class weights (unnorm probs)
#   p = w / sum(w)
#   cutoffs = cumsum(p)
#   unif = runif(n)
#   mat = sapply(unif, function(x) x < cutoffs)
#   apply(mat, 2, function(x) min(which(x)) - 1)
# }

util$boot_binom <- function(n, p)   rbinom(1,n,p)/n

util$shuffle <- function(...) UseMethod("shuffle")

util$shuffle.default <- function(x)  x[order(runif(length(x)))]

util$shuffle.data.frame <- function(x)  x[order(runif(nrow(x))),]

util$sample_df <- function(d, size=10, ...)  {
  samp = sample(1:nrow(d), size=size, ...)
  d[samp,]
}

util$present_levels <- function(x) intersect(levels(x), x)

util$trim_levels <- function(...) UseMethod("trim_levels")

util$trim_levels.factor <- function(x)  factor(x, levels=present_levels(x))

util$trim_levels.data.frame <- function(x) {
  for (n in names(x))
    if (is.factor(x[,n]))
      x[,n] = trim_levels(x[,n])
  x
}

util$kill_names <- function(x) { names(x) = NULL; x }


#  Variants on table()

util$table.freq <- function(...)  table(...) / sum(table(...))

util$table.square <- function(x,y, ..., values=unique(c(as.c(x),as.c(y)))) {
  # intended for factor data
  x=as.c(x); y=as.c(y)
  for (i in 1:length(values))  for (j in 1:length(values)) {
    x = c(x, values[i]);  y = c(y, values[j])
  }
  legit_inds = as.c(x) %in% values  &  as.c(y) %in% values
  t = table(x[legit_inds], y[legit_inds], ...)
  # print(t)
  t = t - 1
  t
}

util$table.cond <- function(...) {
  # for two args x,y: x on rows, y on cols, cells are P(y|x)
  t = table(...)
  for (x1 in 1:nrow(t))
    t[x1,] = t[x1,] / sum(t[x1,])
  t
}

util$table.range <- function(x, min=NULL, max=NULL) {
  # like table(), but only for integers, and forces a contiguous range of bins
  # so counts of 0 can appear.  Useful if you want to compare tables between
  # different datasets.
  if (is.null(min))  min = min(x)
  if (is.null(max))  max = max(x)
  x2 = rep(0, max-min+1)
  t = table(x)
  t_inds = as.integer(names(t))
  t_mask = t_inds >= min  &  t_inds <= max
  t_inds = t_inds[t_mask]
  mask = t_inds - min + 1
  x2[mask] = x2[mask] + t[t_mask]
  names(x2) = min:max
  x2
}

#  Tie breakers

util$fair_gt <- function(x,y) {
  # breaks ties arbitrarily.  # of TRUE's should be halfway between > and >=.
  ret = rep(NA, length(x))
  ret[x > y] = TRUE
  ret[x < y] = FALSE
  # which filler order?  should randomly chooise either c(T,F) vs c(F,T) as the
  # seed (or a random permutation of 50/50 distribution on the whole length),
  # but not clear how to stably but arbitrarily choose one...  hash the bitmap
  # of the concatenation of x and y perhaps.  don't know how to do in highlevel R.
  filler_length = length(which(x==y))
  filler = rep(c(TRUE,FALSE), ceiling(filler_length/2) )[1:filler_length]
  ret[which(x == y)] = filler
  ret
}

util$fair_lt <- function(x,y)  ! fair_gt(x,y)

util$rand_gt <- function(x,y) {
  # breaks ties randomly.
  ret = rep(NA, length(x))
  ret[x > y] = TRUE
  ret[x < y] = FALSE
  filler_length = length(which(x==y))
  filler = as.logical(rbern(filler_length))
  ret[which(x == y)] = filler
  ret
}

util$rand_lt <- function(x,y)  ! rand_gt(x,y)

util$most_common <- function(x)  names(which.max(table(x, exclude=NULL)))

util$p2o <- function(p)  p / (1-p)    # probability -> odds ratio
util$o2p <- function(o)  o / (1+o)    # odds ratio  -> probability
util$lo2p <-function(lo) o2p(2^lo)
util$p2lo <-function(p) log2(p2o(p))  # i like base-2 logits best.
util$lno2p <-function(lo) o2p(exp(lo))
util$p2lno <-function(p) log(p2o(p))

util$merge.list <- function(x,y,only.new.y=FALSE,append=FALSE,...) {
  # http://tolstoy.newcastle.edu.au/R/devel/04/11/1469.html
  out=x

  ystructure = names(c(y,recursive=TRUE))
  xstructure = names(c(x,recursive=TRUE))
  yunique = ystructure[! ystructure %in% xstructure]

  ystructure = sapply(ystructure,FUN=function(element)   strsplit(element,"\\."))
  xstructure = sapply(xstructure,FUN=function(element)   strsplit(element,"\\."))
  yunique = sapply(yunique,FUN=function(element) strsplit(element,"\\."))

   if (only.new.y) 
    lapply(yunique, FUN=function(index) out[[index]]<<-y[[index]])
   else {
     if (!append) {
       lapply(ystructure, FUN=function(index) out[[index]]<<-y[[index]])
     }
     else lapply(ystructure, FUN=function(index) out[[index]]<<-c(out[[index]],y[[index]]))
   }
   return(out)
}

util$lax_rbind <- function(...) {
  inputs = list(...)
  each_names = sapply(inputs, names)
  all_names = unique(c(each_names, recursive=TRUE))
  if (is.data.frame(inputs[[1]]) && all(dim(inputs[[1]])==c(0,0)))
    inputs[[1]] = NULL
  for (k in 1:length(inputs)) {
    if (is.null(inputs[[k]])) next
    more = setdiff(all_names, names(inputs[[k]]))
    names(inputs[[k]]) = c(names(inputs[[k]]), more)
    # inputs[[k]][,more] = NA
  }
  do.call(rbind, inputs)
}

util$fill_bool <- function(bool, true='yes', false='no') {
  ret = rep(NA,length(bool))
  names(ret) = names(bool)
  ret[bool] = true
  ret[!bool] = false
  ret
}

util$trmap <- function(vec, translation_table) {
  # pre-obsoleted by chartr() ?  like unix "tr"
  ret = rep(NA, length(vec))
  for (x in names(translation_table))
    ret[as.c(vec)==x] = translation_table[[x]]
  ret
}

util$bgrep <- function(pat,x, ...) {
  # "boolean" grep: return a logical vector ready for &, | etc ops.
  # so bgrep works in the world of vector ops like ==, %in%, etc.
  unwhich(grep(pat,x,...), length(x))
}

util$ngrep <- function(pat,x, ...)
  # "normal" grep: return values, not indices
  x[grep(pat,x,...)]

util$tapply2 <- function(x, ...) {
  # like tapply but preserves factors
  if (is.factor(x)) {
    r = factor(tapply(as.character(x), ...), levels=levels(x))
  } else {
    r = tapply(x, ...)
  }
  r
}

util$inject <- function(collection, start, fn) {
  # like lisp reduce.  (named after ruby)
  acc = start
  for (x in collection)
    acc = fn(acc, x)
  acc
}

util$select <- function(collection, fn) {
  # like lisp filter.  (named after ruby)
  # nice for lists.  not useful for vectors, use boolean vector indexing instead.
  r = c()
  for (x in collection)
    if (fn(x))
      r = c(r, x)
  r
}

util$xprod <- function(xs,ys) {
  ret = list()
  i=0
  for (x in xs)  for (y in ys) {
    i = i+1
    ret[[i]] = list(x=x,y=y)
  }
  ret
}

util$multi_xprod <- function(args) {
  # args = list(...)
  pair_xprod <- function(xs,ys) {
    ret = list()
    i=0
    for (x in xs)  for (y in ys) {
      i = i+1
      ret[[i]] = c(x,y)
    }
    ret
  }
  ret = list(NA)
  for (i in 1:length(args)) {
    ret = pair_xprod(ret, args[[i]])
  }
  lapply(ret, function(x)  x[2:length(x)])
}

util$printf <- function(...) cat(sprintf(...))

util$listprint <- function(x) {
  s = paste(sapply(names(x), function(n)  sprintf("%s=%s", n,x[[n]])), collapse=' ')
  printf("%s\n", s)
}

# improved list of objects
# http://stackoverflow.com/questions/1358003/tricks-to-manage-the-available-memory-in-an-r-session
util$.ls.objects <- function (pos = 1, pattern, order.by,
                        decreasing=FALSE, head=FALSE, n=5) {
    napply <- function(names, fn) sapply(names, function(x)
                                         fn(get(x, pos = pos)))
    names <- ls(pos = pos, pattern = pattern)
    obj.class <- napply(names, function(x) as.character(class(x))[1])
    obj.mode <- napply(names, mode)
    obj.type <- ifelse(is.na(obj.class), obj.mode, obj.class)
    obj.size <- napply(names, object.size)
    obj.dim <- t(napply(names, function(x)
                        as.numeric(dim(x))[1:2]))
    vec <- is.na(obj.dim)[, 1] & (obj.type != "function")
    obj.dim[vec, 1] <- napply(names, length)[vec]
    out <- data.frame(obj.type, obj.size, obj.dim)
    names(out) <- c("Type", "Size", "Rows", "Columns")
    if (!missing(order.by))
        out <- out[order(out[[order.by]], decreasing=decreasing), ]
    if (head)
        out <- head(out, n)
    out
}
# shorthand
util$lsos <- function(..., n=10) {
    .ls.objects(..., order.by="Size", decreasing=TRUE, head=TRUE, n=n)
}


# routines to help manage longrunning jobs and optimize performance.
# so much more potential here...

util$timeit <- function(expr, name=NULL) {
  # print how long the expression takes, and return its value too.  
  # So you can interpose   timeit({ blabla })   around any chunk of code "blabla".
  start = Sys.time()
  ret = eval(expr)
  finish = Sys.time()
  if (!is.null(name)) cat(name,": ")
  print(finish-start)
  invisible(ret)
}

util$dotprogress <- function(callback, interval=10) {
  # intended to wrap the anonymous callback for sapply() or somesuch.
  count = 0
  return(function(...) {
    if ((count <<- count+1) %% interval == 0)
      cat(".")
    callback(...)
  })
}


########

util$hintonplot <- function(mat, max_value=max(abs(mat)), mid_value=0, ...) {
  # example for counts:
  # > t=table(cyl=mtcars$cyl, mpg=cut(mtcars$mpg,3))
  # > t
  #    mpg
  # cyl (10.4,18.2] (18.2,26.1] (26.1,33.9]
  #   4           0           6           5
  #   6           2           5           0
  #   8          12           2           0
  # > hintonplot(t)   # same thing but graphically

  plot.new()
  plot.window(xlim=c(0.5,ncol(mat)+0.5), ylim=c(0.5,nrow(mat)+0.5))

  x_mid = 1:ncol(mat)
  y_mid = 1:nrow(mat)

  area = abs(mat) / max_value
  side = sqrt(area)


  for (x in 1:ncol(mat)) {
    for (y in nrow(mat):1) {
      # ym = (nrow(mat):1)[y]
      ym = y
      d = side[ym,x] / 2
      rect(x-d, y-d, x+d, y+d, col=if (mat[ym,x]>0) 'darkblue' else 'darkred')
    }
  }

  axis(1, 1:ncol(mat), labels=colnames(mat))
  # axis(2, nrow(mat):1, labels=row.names(mat))
  axis(2, 1:nrow(mat), labels=row.names(mat))
  title(xlab=names(dimnames(mat))[2], ylab=names(dimnames(mat))[1], ...)
}

util$linelight <- function(x,y, lty='dashed', col='lightgray', ...) {
  # highlight a point with lines running to the axes.
  left = par('usr')[1]
  bot = par('usr')[3]
  segments(left,y, x,y, lty=lty, col=col, ...)
  segments(x,bot,  x,y, lty=lty, col=col, ...)
}

util$binary_eval <- function(pred,labels, cutoff='naive', repar=TRUE, ...) {
  library(ROCR)
  # plot(performance(prediction(pred,y),'acc'))
  rocr_pred = prediction(pred,labels)
  acc = performance(rocr_pred,'acc')
  f1 = performance(rocr_pred,'f')
  auc = performance(rocr_pred,'auc')@y.values[[1]]
  roc = performance(rocr_pred,'rec','spec')
  # sensspec = performance(rocr_pred,'rec','spec')
  pr_curve = performance(rocr_pred,'prec','rec')
  rp_curve = performance(rocr_pred,'rec','prec')

  printf("AUC = %.3f\n", auc)

  if (cutoff=='naive') {
    if (all(pred>=0) & all(pred<=1)) {
      printf("Predictions seem to be probabilities, so ")
      cutoff = 0.5
    } else if (any(pred<0) & any(pred>0)) {
      printf("Predictions seem to be real-valued scores, so ")
      cutoff = 0
    } else { 
      warning("cant tell what naive cutoff should be")
      cutoff = NULL
    }
    printf("using naive cutoff %s:\n", cutoff)
  } else if (class(cutoff)=='character') {
    printf("Using %s-best cutoff ", cutoff)
    perf = performance(rocr_pred, cutoff, ...)
    cutoff_ind = which.max(perf@y.values[[1]])
    cutoff = if (cutoff=='prbe') perf@x.values[[1]][1] else rocr_pred@cutoffs[[1]][cutoff_ind]
    printf("%f:\n", cutoff)
  } else {
    printf("For cutoff %s:\n", cutoff)
  }
  cutoff_ind = last(which(rocr_pred@cutoffs[[1]] >= cutoff))


  if (repar) par(mfrow=c(2,2))

  pp = function(perf)  {
    if (is.finite(cutoff_ind)) {
      x=perf@x.values[[1]][cutoff_ind]
      y=perf@y.values[[1]][cutoff_ind]
      points(x,y, col='blue')
      linelight(x,y, col='lightblue')
    }
  }
  plot(acc); pp(acc)
  plot(f1); pp(f1)
  plot(roc); pp(roc)
  abline(a=1,b=-1,lty='dashed',col='gray')
  legend('bottomleft',legend=sprintf("AUC = %.3f",auc))
  plot(rp_curve); pp(rp_curve)
  pp = function(ind,...) points(rp_curve@x.values[[1]][ind], rp_curve@y.values[[1]][ind], ...)
  best_f1 = which.max(f1@y.values[[1]])
  pp(best_f1, pch=2,col='green')
  f05 = performance(rocr_pred,'f',beta=0.5)
  best_f05 = which.max(f05@y.values[[1]])
  pp(best_f05,pch=2,col='green')
  f2 = performance(rocr_pred,'f',beta=2)
  best_f2 = which.max(f2@y.values[[1]])
  pp(best_f2,pch=2,col='green')

  prbe = performance(rocr_pred,'prbe')@y.values[[1]]
  linelight(prbe,prbe,col='lightgray')

  # printf("Acc = %.3f\n", mean((pred >= cutoff) == (labels > 0)))
  printf("Acc = %.3f\n", acc@y.values[[1]][cutoff_ind])

  printf("  F = %.3f\n", f1@y.values[[1]][cutoff_ind])
  printf(" Prec = %.3f\n", pr_curve@y.values[[1]][cutoff_ind])
  printf("  Rec = %.3f\n", pr_curve@x.values[[1]][cutoff_ind])
  printf(" Spec = %.3f\n", roc@x.values[[1]][cutoff_ind])
  if (rocr_pred@n.pos[[1]] != rocr_pred@n.neg[[1]])
    printf("Balanced Acc = %.3f\n", mean(c(roc@x.values[[1]][cutoff_ind], roc@y.values[[1]][cutoff_ind])))


  invisible(rocr_pred)
}




########

# for interactivity...

util$excel <- function(d) {
  f = paste("/tmp/tmp.", round(runif(1)*1000),".csv",  sep='')
  # con = file(f, "w", encoding="MACROMAN")
  con = file(f, "w")
  write.csv(d, con, row.names=FALSE)
  close(con)
  # system(paste("open -a 'Microsoft Excel' ",f, sep=''))
  system(paste("open -a '/Applications/Microsoft Office 2008/Microsoft Excel.app' ",f, sep=''))
}

util$mate <- function(...) {
  system(paste("mate", ...))
}

util$vim <- function(...) {
  system(paste("vim",...))
}

util$ppy <- function(x, column.major=FALSE, ...) {
  # pretty-print as yaml.  intended for rows with big textual cells.
  # a la mysql's \G operator
  # same usecase as ppy() in dotfiles.org/~brendano/.irbrc

  library(yaml)
  cat(as.yaml(x, column.major=column.major), ...)
  cat("\n", ...)
}

util$newwin <- function(x) {
  f = paste("/tmp/tmp.", round(runif(1)*100),".txt",  sep='')
  capture.output(print(x),file=f)
  # system("FILE_TO_VIEW=/tmp/tmp.txt /Applications/Utilities/Terminal.app/Contents/MacOS/Terminal /users/brendano/sw/bin/lame_viewer.sh")
  # system("DISPLAY=:0 /usr/X11R6/bin/xterm -geometry 80x60 -e less /tmp/tmp.txt &")
  system(paste("mate ",f," &", sep=''))
}


##########

while("util" %in% search())
  detach("util")
attach(util)

##########






##  deprecated  ##

util$mymerge <- function(x,y, row.x=F,row.y=F, keep.y=NULL, by=NULL, ...) {
  # Wrapper around merge().  turns out this is not needed because i didnt 
  # read merge()'s manual page carefully enough: it has a facility for
  # joining on rownames.  merge() is great.
    
  if (row.x)  x[,by] = row.names(x)
  if (row.y)  y[,by] = row.names(y)

  ret = merge(x,y,by=by, suffixes=c('','.y'), ...)
  if (row.x && nrow(ret)==nrow(x))  row.names(ret) = row.names(x)
  if (row.y && nrow(ret)==nrow(y))  row.names(ret) = row.names(y)
  
  if (!is.null(keep.y))
    ret = ret[ ,c(names(x),keep.y) ]
  ret
}

util$read.xmlss <- function(f) {
  # ALTERNATIVE: read.tsv(pipe("xlsx2tsv ..."))  with github.com/brendano/tsvutils
  # xlsx is DIFFERENT from xmlss.  on mac, need excel 2008 to get it

  ## BAD BUG: the xml skips cells sometimes.  tricky to parse, argh.
  # Mac Excel 2004 calls this "XML Spreadsheet".  It's nice because it's UTF-8.
  #  [ mac .xls seems to be macroman, but xls2csv (perl converter) f's it up,.
  #    and then iconv can't recover.  boo! ]


  csv_pipe = pipe(paste('ruby <<EOF
    require "rubygems"
    require "hpricot"
    require "fastercsv"
    h = Hpricot(File.read("',f,'"))
    mat = (h.at("worksheet")/"row").map{|row| (row/"cell").map{|data| data.inner_text}}
    mat.each{|row| puts row.to_csv}
', sep=''))
  df = read.csv(csv_pipe)
  # close(csv_pipe)
  df
}

util$list2v <- function(x)  sapply(x, I)    # turns list's values into a vector.  index names are dropped.  pre-obsoleted by unlist() ?

