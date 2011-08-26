# util.R:
# Utilities to make R a happier place
# Brendan O'Connor, brenocon.com/code


########################################
## Tell R the terminal width.  Needs to be re-run every time you resize the
## terminal, so source() this file 

if ( (numcol <-Sys.getenv("COLUMNS")) != "") {
  numcol = as.integer(numcol)
  options(width= numcol - 1)

} else if (Sys.info()['sysname'] == 'Darwin') {
  # I think this is a mac-only stty output format. 
  # TODO need to prevent this from executing when under GUI
  output = tryCatch(system("stty -a", intern=T), error=I)
  if (length(output) > 0) {
    numcol = as.integer(sub(".* ([0-9]+) column.*", "\\1", output[1]))
    if (is.finite(numcol) && numcol > 0) {
      options(width=  numcol - 1 )
    }
  }
  rm(output)
}
rm(numcol)

########################################
## Put everything into an environment, to not pollute global namespace

util = new.env()


########################################
## Better I/O routines

util$read.tsv <- function(..., header=F, sep='\t', quote='', comment='', na.strings='', stringsAsFactors=FALSE) {
  # read.table() wrapper with default settings for no-nonsense, pure TSV
  # Typical use case is output from another program.
  # (R's defaults are more geared for human-readable datafiles, which is less
  # feasible for large-scale data anyway.)
  # These options are substantially faster than read.table() defaults.
  #   (see e.g. LINK)
  # stringsAsFactors is the devil.

  args = list(...)
  args$header = header
  if (!is.null(args$col.names)) {
    # read.delim() is not smart about this.  Yikes.
    args$header = FALSE
  }
  args$sep = sep
  args$quote = quote
  args$comment = comment
  args$stringsAsFactors = stringsAsFactors
  args$na.strings = na.strings
  do.call(read.delim, args)
}

util$write.tsv <- function(..., header=NA, col.names=F, row.names=F, sep='\t', na='', quote=F) {
  # 'header' to 'col.names' naming consistency with read.table()
  if (is.finite(header)) col.names = header
  write.table(..., col.names=col.names, row.names=row.names, sep=sep, na=na, quote=quote)
}


########################################
##  Misc small routines

util$as.c <- as.character

util$unwhich <- function(indices, len=length(indices)) {
  # reverse of which(): from indices to boolean mask.
  ret = rep(F,len)
  ret[indices] = T
  ret
}

util$nna <- function(...) !is.na(...)   # i type this a lot, i think its worth 3 characters + shift key

util$kna <- function(x) x[nna(x)]  # kill NA's (from vector) .. BUT is this same as na.omit() ?

# hm: is this subsumed by reshape::rescaler?

util$unitnorm <- function(x, na.rm=FALSE, ...)  (x - mean(x,na.rm=na.rm,...)) / sd(x,na.rm=na.rm)

util$renorm <- function(x, mean=0, sd=1, ...)  (unitnorm(x,...) * sd) + mean

util$rbern <- function(n, p=0.5)  rbinom(n, size=1, prob=p)

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

util$prio_check = function(...) {
  # priority-order, 3-value-logic backoff
  # the first argument that is not NA or NULL, return it.
  vars = list(...)
  for (i in 1:length(vars)) {
    if (!is.null(vars[[i]]) && !is.na(vars[[i]]))
      return(vars[[i]])
  }
  FALSE
}

util$grid_points <- function(min,max) {
  # 1,2,5,10,20,50 ... kinda-exponential scaling, nice for grid search
  x = min
  ret = NULL
  while(x <= max) {
    ret = c(ret, x, x*2, x*5)
    x = x * 10
  }
  ret[ret <= max]
}

# grep() returns indices of matches.  Variants:

util$bgrep <- function(pat,x, ...) {
  # "boolean" grep: return a logical vector ready for vector ops
  # like & |  and others
  unwhich(grep(pat,x,...), length(x))
}

util$ngrep <- function(pat,x, ...)
  # "normal" grep: return values, not indices
  x[grep(pat,x,...)]


########################################
##  Other data manipulation routines

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

util$xprod <- function(xs,ys) {
  # Set cross-product
  ret = list()
  i=0
  for (x in xs)  for (y in ys) {
    i = i+1
    ret[[i]] = list(x=x,y=y)
  }
  ret
}

util$multi_xprod <- function(args) {
  # Set cross-product
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


########################################
## Printing, viewing
## see also:  str()

util$printf <- function(...) cat(sprintf(...))

util$listprint <- function(x) {
  s = paste(sapply(names(x), function(n)  sprintf("%s=%s", n,x[[n]])), collapse=' ')
  printf("%s\n", s)
}

util$msg <- function(...)  cat(..., "\n", file=stderr())

util$h = utils::head

util$ppy <- function(x, column.major=FALSE, ...) {
  # pretty-print as yaml.  intended for rows with big textual cells.
  # a la mysql's \G operator
  library(yaml)
  cat(as.yaml(x, column.major=column.major), ...)
  cat("\n", ...)
}

util$table_html = function(...) {
  # Intended for inside dosink()
  columns = list(...)
  ncol = length(columns)
  nrow = length(columns[[1]])
  # assume columns are in parallel
  printf("\n<table cellpadding=3 border=1 cellspacing=0 bordercolor=gray>")
  for (i in 1:nrow) {
    printf("\n<tr>")
    for (j in 1:ncol)
      printf("\n  <td>%s", columns[[j]][i])
  }
  printf("\n</table>\n")
}


########################################
##  Workspace management

# improved list of objects
# http://stackoverflow.com/questions/1358003/tricks-to-manage-the-available-memory-in-an-r-session
util$list_objects = function (pos = 1, pattern) {
    napply = function(names, fn) sapply(names, function(x)
                                         fn(get(x, pos = pos)))
    names = ls(pos = pos, pattern = pattern)
    N = length(names)
    obj_class = napply(names, function(x) as.character(class(x))[1])
    obj_mode = napply(names, mode)
    obj_type = ifelse(is.na(obj_class), obj_mode, obj_class)
    obj_prettysize = napply(names, function(x) {
                           capture.output(print(object.size(x), units = "auto")) })
    obj_size = napply(names, object.size)
    obj_prettysize[obj_size < 1e6] = ""

    obj_length = napply(names, function(x) length(x))
    obj_dim = t(napply(names, function(x)
                        as.numeric(dim(x))[1:2]))

    is_flat = is.na(obj_dim)[, 1]
    is_vector = napply(names, function(x) is.vector(x) & class(x) != 'list')


    info_width = max(20, options('width')$width - 60)

    small_str = function(x) {
      out = capture.output(
        str(x, max.level=0, give.attr=F, give.head=F, width=info_width, strict.width='cut')
      )
      out = str_c(out,collapse=' ')
      out = cutoff(str_replace(out,"\n"," "))
      if (str_detect(out, "^List of"))
        out = str_c("[Names] $ ", str_c(names(x),collapse=' '))
      cutoff(out)
    }

    cutoff = function(s) {
      if (str_length(s) >= info_width) {
        str_c(str_sub(s,1,info_width-2),'..')
      } else {
        s
      }
    }
      
    pad = function(s) sprintf(" %s", s)

    out <- data.frame(
      Type = obj_type,
      Size = obj_prettysize,
      Dim = ifelse(is_vector | is_flat, obj_length, 
        sprintf("(%s, %s)", obj_dim[,1], obj_dim[,2])),
      Value = napply(names, function(x) 
        if (class(x) %in% c('data.frame','list') && !is.null(names(x)))
          cutoff(str_c("[Names] $ ",str_c(names(x), collapse=' ')))
        else small_str(x)
        ),
      stringsAsFactors=F)
    row.names(out) = names
    out$Dim = sprintf(" %s", out$Dim)
    out$Value = sprintf(str_c(" %-", info_width, "s"), out$Value)

    out = rbind(subset(out, Type!='function'), subset(out, Type=='function'))
    out
}

util$lsos = function() {
  d = list_objects()
  d$name = row.names(d)
  d = subset(d, name != 'util')
  row.names(d)=d$name
  d$name=NULL
  d
}


########################################
## For performance optimization and long-running jobs

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
  # ALTERNATIVE: plyr *ply(.progress='text')
  count = 0
  return(function(...) {
    if ((count <<- count+1) %% interval == 0)
      cat(".")
    callback(...)
  })
}


########################################
##  External programs for interactivity

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

util$ll <- function(...) {
  system(paste("ls","-l",...))
}

util$newwin <- function(x) {
  # Takes object printout into new file... dosink(OPEN=T) kinda subsumes this
  f = paste("/tmp/tmp.", round(runif(1)*100),".txt",  sep='')
  capture.output(print(x),file=f)
  # system("FILE_TO_VIEW=/tmp/tmp.txt /Applications/Utilities/Terminal.app/Contents/MacOS/Terminal /users/brendano/sw/bin/lame_viewer.sh")
  # system("DISPLAY=:0 /usr/X11R6/bin/xterm -geometry 80x60 -e less /tmp/tmp.txt &")
  system(paste("mate ",f," &", sep=''))
}


########################################
## Graphics output wrappers
## For easy one-liners, like:
## dopdf("tmp.pdf",width=5,height=5,cmd=plot(x,y))

util$dopdf <- function(filename,..., cmd) {
  pdf(filename, ...)
  eval(cmd)
  dev.off()
  if (exists('OPEN') && OPEN)
    system(sprintf("open %s", filename))
}

util$dopng <- function(filename,..., cmd) {
  png(filename, ...)
  eval(cmd)
  dev.off()
  if ((exists('OPEN') && OPEN))
    system(sprintf("open %s", filename))
}

util$dosink <- function(filename,cmd, open=NULL) {
  # like capture.output() but follows open/OPEN conventions here
  sink(filename)
  eval(cmd)
  sink(NULL)
  if (prio_check(open, exists('OPEN') && OPEN))
    system(sprintf("open %s", filename))
}

util$dosvg <- function(filename, ..., cmd, open=NULL) {
  library("RSvgDevice")
  devSVG(filename, ...)
  eval(cmd)
  dev.off()
  if (prio_check(open, exists('OPEN') && OPEN))
    system(sprintf("open %s", filename))
}


########################################
## Plotting routines

util$linelight <- function(x,y, lty='dashed', col='lightgray', ...) {
  # highlight a point with lines running to the axes.
  left = par('usr')[1]
  bot = par('usr')[3]
  segments(left,y, x,y, lty=lty, col=col, ...)
  segments(x,bot,  x,y, lty=lty, col=col, ...)
}

util$hintonplot <- function(mat, max_value=max(abs(mat)), mid_value=0, ...) {
  # Plots a matrix as colored, size-varying boxes
  # I dunno who started calling this a "Hinton plot", but anyways

  # Example:
  # hintonplot(matrix(rnorm(100),10))

  # Example, for counts:
  # table(cyl=mtcars$cyl, mpg=cut(mtcars$mpg,3))
  #    mpg
  # cyl (10.4,18.2] (18.2,26.1] (26.1,33.9]
  #   4           0           6           5
  #   6           2           5           0
  #   8          12           2           0
  # hintonplot(table(cyl=mtcars$cyl, mpg=cut(mtcars$mpg,3)))

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

util$binary_eval <- function(pred,labels, cutoff='naive', repar=TRUE, ...) {
  # Various binary classification evaluation plots and metrics
  library(ROCR)
  # plot(performance(prediction(pred,y),'acc'))
  rocr_pred = prediction(pred,labels)
  acc = performance(rocr_pred,'acc')
  f1 = performance(rocr_pred,'f')
  auc = performance(rocr_pred,'auc')@y.values[[1]]
  roc = performance(rocr_pred,'rec','spec')
  bac = if (rocr_pred@n.pos[[1]] != rocr_pred@n.neg[[1]])
      sapply(1:length(roc@x.values[[1]]), function(i)
        mean(c(roc@x.values[[1]][i], roc@y.values[[1]][i])))
    else
      rep(-1,length(pred))
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
    if (cutoff=='bac') {
      perf = NULL
      perf_y = bac
    } else {
      perf = performance(rocr_pred, cutoff, ...)
      perf_y = perf@y.values[[1]]
    }
    cutoff_ind = which.max(perf_y)
    cutoff = if (cutoff=='prbe') perf@x.values[[1]][1] else rocr_pred@cutoffs[[1]][cutoff_ind]
    printf("%f\n", cutoff)
  } else {
    printf("For cutoff %s:\n", cutoff)
  }
  cutoff_ind = last(which(rocr_pred@cutoffs[[1]] >= cutoff))

  if (repar) par(mfrow=c(2,2))

  pp = function(perf)  {
    if (length(cutoff_ind)>0 && is.finite(cutoff_ind)) {
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
  printf("Acc %.3f, ", acc@y.values[[1]][cutoff_ind])

  printf("  F %.3f, Prec %.3f, Rec %.3f, Spec %.3f",
    f1@y.values[[1]][cutoff_ind],
    pr_curve@y.values[[1]][cutoff_ind],
    pr_curve@x.values[[1]][cutoff_ind],
    roc@x.values[[1]][cutoff_ind])
  # printf(" Prec = %.3f\n", pr_curve@y.values[[1]][cutoff_ind])
  # printf("  Rec = %.3f\n", pr_curve@x.values[[1]][cutoff_ind])
  # printf(" Spec = %.3f\n", roc@x.values[[1]][cutoff_ind])

  if (bac[1] != -1)
    printf(", BalAcc %.3f", mean(bac))
  printf("\n")


  invisible(rocr_pred)
}


########################################
# These are kinda obscure, should delete?

util$fair_gt <- function(x,y) {
  # Breaks ties arbitrarily.  # of TRUE's should be halfway between > and >=.
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
  # Breaks ties randomly.
  ret = rep(NA, length(x))
  ret[x > y] = TRUE
  ret[x < y] = FALSE
  filler_length = length(which(x==y))
  filler = as.logical(rbern(filler_length))
  ret[which(x == y)] = filler
  ret
}

util$rand_lt <- function(x,y)  ! rand_gt(x,y)



########################################
# Deprecated

## now in plyr 0.19 as summarise() http://github.com/hadley/plyr/blob/master/NEWS
# util$reframe = function(.data, ...) {
#   e = eval(substitute(list(...)), .data, parent.frame())
#   data.frame(e)
# }

# util$table.range <- function(x, min=NULL, max=NULL) {
#   ## DEPRECATED: use factor() on both sides, instead, to specify the allowable range
#   # Like table(), but only for integers, and forces a contiguous range of bins
#   # so counts of 0 can appear.  Useful if you want to compare tables between
#   # different datasets.
#   if (is.null(min))  min = min(x)
#   if (is.null(max))  max = max(x)
#   x2 = rep(0, max-min+1)
#   t = table(x)
#   t_inds = as.integer(names(t))
#   t_mask = t_inds >= min  &  t_inds <= max
#   t_inds = t_inds[t_mask]
#   mask = t_inds - min + 1
#   x2[mask] = x2[mask] + t[t_mask]
#   names(x2) = min:max
#   x2
# }

########################################



########################################
## Has to be last in file

while("util" %in% search())
  detach("util")
attach(util)

