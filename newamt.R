# routines for the new AMT interface's CSV files.

dlanalysis$load_anno <- function(filename, remove_crap=T, ...) {
  datetime_cols = c('AcceptTime','AutoApprovalTime','CreationTime','Expiration','SubmitTime')
  
  colClasses=list()
  for (c in datetime_cols)  colClasses[[c]] = 'character'
  
  a = read.csv(filename, colClasses=colClasses)
  # return(a)  
  a = conversions(a, ...)
  
  if (remove_crap) {
    for (c in c('AssignmentDurationInSeconds','AutoApprovalDelayInSeconds','Description','Title','Keywords','NumberOfSimilarHITs','MaxAssignments')) {
      a[,c] = NULL
    }
  }  
  a
}

dlanalysis$conversions <- function(x, nice_worker_names=T, convert_dates=T) {
  datetime_cols = c('AcceptTime','AutoApprovalTime','CreationTime','Expiration','SubmitTime')
  if (convert_dates) {
    # datetimes are in strftime("%c") format.  of course strptime %c doesnt work.  i can't believe the following works but it seems to.
    lame_convert <- function(x)  strptime(x, "%A %b %d %T")
    for (c in datetime_cols) {
      x[,c] = as.POSIXct( lame_convert(x[,c]) )
    }    
  }
  if (nice_worker_names) {
    w = x$WorkerId
    # x$WorkerId = as.integer(w)
    x$WorkerId = factor(as.integer(w), labels=sprintf("w%d",1:nlevels(w)))
  }
  x
}

dlanalysis$load_assn <- function(filename, ...) {
  datetime_cols = c('AcceptTime','AutoApprovalTime','CreationTime','Expiration','SubmitTime')
  
  colClasses=list()
  for (c in datetime_cols)  colClasses[[c]] = 'character'
  
  s = read.csv(filename, colClasses=colClasses)
  s = conversions(s, ...)
  s
}

dlanalysis$throughput_report <- function(a) {
  # n_assn = nlevels(a$AssignmentId)
  dur = difftime(max(a$SubmitTime), min(a$SubmitTime), units='hours')
  units_per_hit = nrow(a) / nlevels(a$AssignmentId)
  cat(
    sprintf("Duration: %.2f hr", dur),
    sprintf("Size: %sunits @ %.1f assn/hit = %sJ",
      smart_print(nlevels(a$photo), dash=T), 
      nlevels(a$AssignmentId) / nlevels(a$HITId),
      smart_print(nrow(a))),
    sprintf("%d workers, mean %sJ/W, median %sJ/W, 50%% workload by top %.1f%%", 
      nlevels(a$WorkerId),
      smart_print(mean(table(a$WorkerId))),
      smart_print(median(table(a$WorkerId))),
      100 * sum(cumsum(sort(table.freq(a$WorkerId), decreasing=T)) < .5) / nlevels(a$WorkerId)
    ),
    "Parallelized throughput",
    sprintf("   %sJ/hr", smart_print(nrow(a) / as.numeric(dur))),
    sprintf("   %sU/hr", smart_print(nlevels(a$photo) / as.numeric(dur))),
    sprintf("   %sassn/hr @ %s units/hit", 
      smart_print(nlevels(a$AssignmentId) / as.numeric(dur), dash=T),
      units_per_hit
      ),
    "Individual worker throughput",
    sprintf("   %.1f sec/J = %sJ/hr", 
      mean(a$WorkTimeInSeconds) / units_per_hit,
      smart_print( units_per_hit * 1 / (mean(a$WorkTimeInSeconds)/3600) )),
      
    sep="\n"
  )    
}

dlanalysis$smart_print <- function(x, dash=FALSE) {
  s = {
    if (x < 2e3)     sprintf("%.1f ", x)
    else if (x < 1e7) sprintf("%.1f k", x/1e3)
    else if (x < 1e10) sprintf("%.1f M", x/1e6)
    else if (x < Inf) sprintf("%.1f G", x/1e9)
    else sprintf("%s ", x)    
  }
  if (dash && x>=2e3)
    s = paste(s,"-", sep='')
  s
}

dlanalysis$worker_parallelism_plot <- function(a, w_pos=NULL, ...) {
  # a$WorkerId = trim_levels(a)
  
  if (is.null(w_pos)) {
    w_starts = (dfagg(a,a$WorkerId, function(x) min(x$SubmitTime - x$WorkTimeInSeconds)))
    w_pos = rank(w_starts, ties='first')
  }
  
  plot(a$SubmitTime - a$WorkTimeInSeconds, w_pos[a$WorkerId],  type='p', ...)
  segments(a$SubmitTime - a$WorkTimeInSeconds, w_pos[a$WorkerId],   a$SubmitTime, w_pos[a$WorkerId])
  # text(sort(w_starts), 1:length(w_pos), sprintf("%s", 1:length(w_pos)), pos=2)
}

# dlanalysis$make_reinput <- function(urls, name) {
#   filename = paste(name,"csv",  sep=".")
#   outtable = data.frame(id=NA, src=gsub("http://s3.amazonaws.com/|thumb/","", urls))
#   write.csv(outtable, filename, row.names=F)
#   cat("Written to: ", filename, "\n")
# }

dlanalysis$make_reinput <- function(urls, name, ncol=40) {
  filename = paste(name,".ready.csv",  sep="")
  outtable = as.data.frame(matrix(urls, ncol=ncol))
  names(outtable) = sprintf("image%d", 1:ncol)
  write.csv(outtable, filename, row.names=F)
  # outtable = data.frame(id=NA, src=gsub("http://s3.amazonaws.com/|thumb/","", urls))
  # write.csv(outtable, filename, row.names=F)
  cat("Written to: ", filename, "\n")
}



# newamt results file is at the ASSIGNMENT level.



dlanalysis$worker_an.binary <- function(a,  pseudo=list(tp=1, tn=1, fp=1, fn=1)) {
  stopifnot( is.logical(a$response) )
  stopifnot( is.logical(a$gold) )
  # pseudo_instances = list(tp = rep())
  dfagg(a, a$WorkerId, function(x) {
    r = x$response
    g = x$gold
    wg = which(g)
    wng = which(!g)
    list(
      num = nrow(x),
      acc = (sum((r==g)[!is.na(g)]) + 1) / (sum(!is.na(g)) + 2),
      posacc = (sum( (r==g)[wg] ) + pseudo$tp)  /  (length(wg) + pseudo$tp+pseudo$fn),
      negacc = (sum( (r==g)[wng]) + pseudo$tn)  /  (length(wng)+ pseudo$tn+pseudo$fp)

    # posacc = mean( (r==g)[which(g)] ),
    # negacc = mean( (r==g)[which(!g)] )
    )
  })
}

dlanalysis$posterior_given_workers.binary <- function(w,a) {
  if ( ! setequal(levels(a$WorkerId), row.names(w)) )  stop("bad worker info!")
  if ( any(is.na(a$response)) )  stop("dont know how to handle NA responses")
  aw = merge(a, w, by.x='WorkerId', by.y=0, all.x=T, all.y=F)
  print(dim(aw))
  vote_llr = rep(NA, nrow(aw))
  
  r = aw$response
  vote_llr = ( as.integer(aw$response)*2-1 ) * log2(aw$acc / 1-aw$acc)
  # vote_llr[r]  = log2(      aw$posacc[r] / (1-aw$negacc[r]) )
  # vote_llr[!r] = log2( (1-aw$posacc[!r]) / aw$negacc[!r] )
  
  dfagg(data.frame(a$orig_id, vote_llr), a$orig_id, function(x) {
    sum(x$vote_llr)
  })
}


dlanalysis$norm_to_old_format <- function(a) {
  was_logical = is.logical(a$response)
  stopifnot(was_logical)

  # specific to content mod
  a$orig_id = a$photo
  a$gold = a$sex
  
  r = a$response
  g = a$gold
  
  a$response = NA
  a$gold = NA
  a$response[r] = '1'
  a$response[!r] = '0'
  a$gold[g] = '1'
  a$gold[!g] = '0'
  a$response = factor(a$response, levels=c('0','1'))
  a$gold = factor(a$gold, levels=c('0','1'))
  # a$response = factor(a$response, levels=c("FALSE","TRUE"))
  # a$gold = factor(a$gold, levels=c("FALSE","TRUE"))
  attr(a,'candidates') = c('0','1')
  attr(a,'target') = '1'
  attr(a,'data_type') = 'categ'
  a
}


