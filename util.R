# util.R:
# Utilities to make R an even happier place
# Brendan O'Connor, brenocon@gmail.com


options(showWarnCalls=T, showErrorCalls=T)
# mac specific?
if (system("stty -a &>/dev/null") == 0)
  options(width= as.integer(sub(".* ([0-9]+) column.*", "\\1", system("stty -a", intern=T)[1])) - 1 )



util = new.env()

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

util$table.freq <- function(x, ...)  table(x, ...) / sum(table(x, ...))

util$unitnorm <- function(x, na.rm=FALSE, ...)  (x - mean(x,na.rm=na.rm,...)) / sd(x,na.rm=na.rm)

util$renorm <- function(x, mean=0, sd=1, ...)  (unitnorm(x,...) * sd) + mean

util$rbern <- function(n, p=0.5)  rbinom(n, size=1, prob=p)

util$shuffle <- function(...) UseMethod("shuffle")

util$shuffle.default <- function(x)  x[order(runif(length(x)))]

util$shuffle.data.frame <- function(x)  x[order(runif(nrow(x))),]

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
  for (k in 1:length(inputs)) {
    if (is.null(inputs[[k]])) next
    more = setdiff(all_names, names(inputs[[k]]))
    inputs[[k]][,more] = NA
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
  # like unix "tr"
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
  # slightly nicer than tapply for some reason, i dont remember
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
  # nice for lists.  not useful for vectors, just use boolean vector indexing.
  # (named after ruby)
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

# routines to help manage longrunning jobs.
# so much more potential here...

util$timeit <- function(expr, name=NULL) {
  # print how long the expression takes, and return its value too.  
  # So you can interpose   timeit({ blabla })   around and chunk of code "blabla".
  start = Sys.time()
  ret = eval(expr)
  finish = Sys.time()
  if (!is.null(name)) cat(name,": ")
  cat(sprintf("%.3f seconds\n", finish-start))
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

# dataframe-outputting apply and aggregation functions.
# i'm often confused whether proper R style should emphasize matrices or dataframes.
# so here's some support for a dataframe-centric lifestyle.

# like sapply/lapply except it expects fn() to yield lists.
# each list gets coerced into a single row of a returned dataframe.

util$dfapply <- function(collection, fn) {
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

# sapply() with fn() yielding lists returns a matrix with named rows/cols ... 
# and whenever you name-index into this thing it return a list ... yuck
# make that shit more normal.

util$matrix2df <- function(x) {
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

util$kill_df_lists <- function(d) {
  # if you have internal lists inside your dataframe.  if you always use
  # matrix2df this should never happen.  but sometimes it does.  yikes!  
  for(n in names(d))
    if (is.list(d[,n]))
      d[,n] = list2v(d[,n])
  d
}

util$list2v <- function(x)  sapply(x, I)    # turns list's values into a vector.  index names are dropped.


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
  
  b=ls
  cols = NULL
  for (i in 1:min(100,length(b))) {
    cols = c(cols, try(names(b[[i]])))
  }
  cols = unique(cols)

  ret = data.frame(row.names=names(b))

  for (col in cols) {
    ret[,col] = sapply(names(b), function(k) {
      if (is.null(b[[k]]))  NA
      else if (!is.null(names(b[[k]])))  b[[k]][[col]]
      else if (length(b[[k]])==1 && is.na(b[[k]]))  NA
      else stop("dont know what to do with value ",b[[k]])
    })
  }
  if (length(cols) == 0) {
    return(sapply(names(b), function(k) b[[k]]))
  }
  ret
}

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

util$flipleft <- function(x,named_vec, by) {
  # kinda dangerous.  but so convenient
  if (is.null(names(named_vec))) {
    stopifnot(length(named_vec) == nlevels(x[,by]))
    names(named_vec) = levels(x[,by])    
  }
  y = data.frame(row.names=names(named_vec), ze_y_value=named_vec)
  x$ze_orig_order = 1:nrow(x)  
  merged = merge(x,y, by.x=by, by.y=0, all.x=T, all.y=F)

  merged$ze_y_value[order( merged$ze_orig_order )]
}

util$read.xmlss <- function(f) {
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

########

# for interactivity...

util$excel <- function(d) {
  f = paste("/tmp/tmp.", round(runif(1)*100),".csv",  sep='')
  con = file(f, "w", encoding="MACROMAN")
  write.csv(d, con)
  system(paste("open -a 'Microsoft Excel' ",f, sep=''))
  close(con)
}

util$mate <- function(...) {
  system(paste("mate", ...))
}

util$ppy <- function(x, column.major=FALSE, ...) {
  # pretty-print as yaml.  intended for rows with big textual cells.
  # a la mysql's \G operator a la http://rubyisawesome.com/2007/7/10/mysql-secrets-g-instead-of
  # same usecase as ppy() in my http://dotfiles.org/~brendano/.irbrc

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
