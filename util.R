options(showWarnCalls=T, showErrorCalls=T)
# mac specific?
if (system("stty -a &>/dev/null") == 0)
  options(width= as.integer(sub(".* ([0-9]+) column.*", "\\1", system("stty -a", intern=T)[1])) - 1 )



util = new.env()

util$unitnorm <- function(x, ...)  (x - mean(x,...)) / sd(x,...)

util$rbern <- function(n, p=0.5)  rbinom(n, size=1, prob=p)

util$msg <- function(...)  cat(..., "\n", file=stderr())

util$strlen <- function(s)  length(strsplit(s,"")[[1]])

util$strmatch <- function(pat,s)  length(grep(pat,s)) > 0

util$strstrip <- function(s)  gsub("^\\s*|\\s*$", "", s)

util$is_empty <- function(collection)  length(collection) == 0

util$last <- function(x)  tail(x, 1)

util$as.c <- as.character

util$table.freq <- function(x, ...)  table(x, ...) / sum(table(x, ...))

util$unwhich <- function(indices, len=length(indices)) {
  ret = rep(F,len)
  ret[indices] = T
  ret
}

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
util$p2lo <-function(p) log2(p2o(p))

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

# util$merge_vec <- function(df, y, by, name) {
#   right = data.frame(bla=y)
#   right[[name]] = right$bla
#   rm(right$bla)
#   right[[by]] = as.numeric(names(y))
#   merge(df, right, sort=FALSE)
# }

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

# "normal" grep: return values, not indices
util$ngrep <- function(pat,x, ...)  x[grep(pat,x,...)]

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
  acc = start
  for (x in collection)
    acc = fn(acc, x)
  acc
}

util$select <- function(collection, fn) {
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

util$timeit <- function(x) {
  start = Sys.time()
  ret = eval(x)
  finish = Sys.time()
  print(finish - start)
  invisible(ret)
}

util$dotprogress <- function(callback, interval=100) {
  count = 0
  return(function(...) {
    if ((count <<- count+1) %% interval == 0)
      cat(".")
    callback(...)
  })
}

# dataframe-outputting apply and aggregation functions

# like sapply/lapply except it expects fn() to yield lists.
# each list gets coerced into a single row of a dataframe.

util$dfapply <- function(collection, fn, t=TRUE) {
  r = sapply(collection, fn)
  if (t)  r = base::t(r)
  r = matrix2df(r)
  row.names(r) = collection
  r
}

# sapply() with fn() yielding lists retrns a matrix with named rows/cols ... 
# and whenever you name-index into this thing it return a list ... yuck
# make that shit more normal.

util$matrix2df <- function(x) {
  if (class(x) != 'matrix') stop("why is class ",class(x))
  colnames = names(x[1,])
  data.frame(
    sapply(colnames, function(n) unlist(x[,n])),
    row.names=row.names(x))
}


# like by() but the data types are less crazy:
#  if fn() returns a list, a data frame is returned.  
#    -> byvals are the row names.
#    -> each list is coerced into the rows.
#  if fn() returns a nonlist, a list is returned.
#    -> byvals are the names.
# We attempt to be tolerant for slight inconsistencies in fn()'s return values.

util$dfagg <- function(d, byvals, fn) {
  if (class(byvals) == 'function')
    byvals = byvals(d)
  if (is.factor(byvals) && !setequal( as.c(unique(byvals)), levels(byvals)) ) {
    msg("Warning, byvals is a factor but only using only a subset of its levels.  Coercing to character to avoid weirdnesses.  Hopefully this is what you want.")
    byvals = as.character(byvals)
  }

  b = by(d, byvals, fn)
  list2df(b)

  # cols = NULL
  # for (i in 1:min(100,length(b))) {
  #   cols = c(cols, names(b[[i]]))
  # }
  # cols = unique(cols)
  # 
  # ret = data.frame(row.names=names(b))
  # 
  # for (col in cols) {
  #   ret[,col] = sapply(names(b), function(k) b[[k]][[col]])
  # }
  # if (length(cols) == 0) {
  #   return(sapply(names(b), function(k) b[[k]]))
  # }
  # ret
}

util$list2df <- function(ls) {
  b=ls
  cols = NULL
  for (i in 1:min(100,length(b))) {
    cols = c(cols, names(b[[i]]))
  }
  cols = unique(cols)

  ret = data.frame(row.names=names(b))

  for (col in cols) {
    ret[,col] = sapply(names(b), function(k) b[[k]][[col]])
  }
  if (length(cols) == 0) {
    return(sapply(names(b), function(k) b[[k]]))
  }
  ret
}

util$mymerge <- function(x,y, row.x=F,row.y=F, by=NULL, ...) {
  if (row.x)  x[,by] = row.names(x)
  if (row.y)  y[,by] = row.names(y)

  ret = merge(x,y,by=by, ...)
  if (row.x && nrow(ret)==nrow(x))  row.names(ret) = row.names(x)
  if (row.y && nrow(ret)==nrow(y))  row.names(ret) = row.names(y)
  ret
}

util$flipleft <- function(x,named_vec, by) {
  if (is.null(names(named_vec))) stop("rhs must be named")
  y = data.frame(row.names=names(named_vec), ze_y_value=named_vec)
  merged = mymerge(x,y, row.y=T, by=by)
  merged$ze_y_value
}

util$read.xmlss <- function(f) {
  ## BUG: the xml skips cells sometimes.  tricky to parse, argh
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
  con = file("/tmp/tmp.csv", "w", encoding="MACROMAN")
  write.csv(d, con)
  system("open -a 'Microsoft Excel' /tmp/tmp.csv")
  close(con)
}

util$mate <- function(...) {
  system(paste("mate", ...))
}

# pretty-print as yaml.  intended for rows with big textual cells.
# a la mysql's \G operator

util$ppy <- function(x, column.major=FALSE, ...) {
  library(yaml)
  cat(as.yaml(x, column.major=column.major), ...)
  cat("\n", ...)
}

util$newwin <- function(x) {
  capture.output(print(x),file="/tmp/tmp.txt")
  # system("FILE_TO_VIEW=/tmp/tmp.txt /Applications/Utilities/Terminal.app/Contents/MacOS/Terminal /users/brendano/sw/bin/lame_viewer.sh")
  # system("DISPLAY=:0 /usr/X11R6/bin/xterm -geometry 80x60 -e less /tmp/tmp.txt &")
  system("mate /tmp/tmp.txt &")
}


while("util" %in% search())
  detach("util")
attach(util)
