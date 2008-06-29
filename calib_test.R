a=data.frame(X.amt_worker_ids=rep(paste('w',1:2,sep=''), 10))
a[a$X.amt_w=='w1','response'] = 5 + rnorm(10,0,1)
a[a$X.amt_w=='w2','response'] = 5 + rnorm(10,0,5)
> a[a$X.amt_w=='w1','gold']=jitter(rep(5,10))
> a[a$X.amt_w=='w2','gold']=a[a$X.amt_w=='w1','gold']
> a$orig_id=NA
> a[a$X.amt_w=='w1','orig_id']=paste('u',1:10,sep='')
> a[a$X.amt_w=='w2','orig_id']=paste('u',1:10,sep='')

> class(a)=c('data.frame','anno')
> a$orig_id=factor(a$orig_id)
> a$X.amt_worker_ids=factor(a$X.amt_worker_ids)



> a=trim_levels(a_all[1:18,])
> u=agg_to_unit(a); safecor(u$mean,u$gold)
> m=fit_anno_model(a,u)

