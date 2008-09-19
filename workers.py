"""
AMT worker modeling in support of Snow et al 08.  http://blog.doloreslabs.com/2008/09/amt-fast-cheap-good-machine-learning/
Brendan O'Connor, brendano@doloreslabs.com, www.anyall.org
[ this is actually a reimplementation done after the paper's experiments were finished; it is only does binary problems at the moment. ]

I run all this inside ipython.  E.g.

>>> from workers import *
>>> d = DataSet.load("data/rte.standardized.tsv")
>>> eval_acc(naive_vote(d), d.units)
0.89687499999999998


though actually i do this, to support easy reloading:
>>> sys.path.insert(0,"dlanalysis")
>>> execfile("dlanalysis/workers.py")

See the functions at the bottom for bigger evaluation runs.
"""

from __future__ import division
from math import *
import numpy
from numpy import *
import random
from copy import *   # clobber numpy.copy
from collections import defaultdict
import sys,os
import util
from util import counter

# sys.stdout=os.fdopen(1,'w',0)

def log2(x):  return numpy.log2(x)

def uniq_c(seq):
  ret = defaultdict(lambda:0)
  for x in seq:
    ret[x] += 1
  return dict(ret)

def load_tsv(filename):
  infile = open(filename)
  header = infile.readline().strip()
  cols = header.split("\t")
  ret = []
  for line in infile:
    parts = line.strip().split("\t")
    record = dict(zip(cols,parts))
    ret.append(record)
  return ret

def sanity_rename(anno):
  """ CamelCaps is used in the new AMT csv format """
  ret = []
  for rec in anno:
    rec2 = copy(rec)
    rec2['WorkerId'] = rec2.pop('!amt_worker_ids')
    rec2['AssignmentId'] = rec2.pop('!amt_annotation_ids')
    ret.append(rec2)
  return ret

class DataSet:
  def __init__(self):
    self._workers = None
    
  @classmethod
  def load(klass, filename):
    self = klass()
    self.anno = sanity_rename(load_tsv(filename))
    all_u = set( a['orig_id'] for a in self.anno)
    self.units = dict( (u, {'u':u}) for u in all_u )
    for u in all_u:
      myanno = [a for a in self.anno if a['orig_id']==u]
      self.units[u]['anno'] = myanno
      assert len(set([a['gold'] for a in myanno])) == 1
      self.units[u]['gold'] = myanno[0]['gold']

    self.workers_units = defaultdict(lambda:[])
    for u in self.units.itervalues():
      for a in u['anno']:
        self.workers_units[a['WorkerId']].append(u)
    self.workers_units = dict(self.workers_units)

    self.anno = util.DataFrame(self.anno)
    return self
  
  @property
  def workers(self):
    if not self._workers:
      self._workers = set(a['WorkerId'] for a in self.anno)
    return self._workers

def anno_from_units(units):
  if type(units) == dict:
    units = units.itervalues()
  anno = []
  for u in units:
    anno += u['anno']
  return anno




LABELS = ['0','1']

def worker_an(anno, **kwds):
  workers = set(r['WorkerId'] for r in anno)
  worker_info = defaultdict(lambda:None,  ((w,{}) for w in workers) )
  for w in workers:
    myanno = [r for r in anno if r['WorkerId'] == w]
    if myanno != []:
      worker_info[w] = one_worker_model(myanno, **kwds)
  
  unk_model = one_worker_model(anno, **kwds)
  worker_info.default_factory = lambda: unk_model
  return worker_info

  # def one_worker_model(myanno, pseudoc=1):
def one_worker_model(myanno, pseudoc=1):
  ret = {}
  confusion = {}
  for pred in LABELS:
    for real in LABELS:
      confusion[ (pred,real) ] = len([1 for r in myanno if r['response']==pred and r['gold']==real])
  ret['confusion'] = confusion
  
  right = wrong = 0
  for (pred,real) in confusion:
    if pred==real:  right += confusion[(pred,real)]
    else:           wrong += confusion[(pred,real)]
  # ret['acc'] = right / (right+wrong + .001)
  ret['acc'] = right / (right+wrong)


  # BINARY ONLY
  tp = confusion[('1','1')]  + pseudoc
  fp = confusion[('1','0')]  + pseudoc
  tn = confusion[('0','0')]  + pseudoc
  fn = confusion[('0','1')]  + pseudoc
  
  # ret['sens'] = tp / (tp+fn)
  # ret['spec'] = tn / (tn+fp)
  
  # log lik ratio of being right vs. being wrong when you say '1', or when you say '0'
  ret['llr_weights'] = {}
  ret['llr_weights']['1'] = \
      log2( tp/(tp+fn) ) - log2( fp/(fp+tn) )
  ret['llr_weights']['0'] = \
      log2( tn/(tn+fp) ) - log2( fn/(fn+tp) )
  return ret
  
def uniform_worker_an(anno=None):
  return defaultdict(lambda: dict(llr_weights={'1':1, '0':1}))

def predict(worker_model, units):
  pred = dict( (u,{}) for u in units)
  for u in units:
    # compute the logit posterior that label is '1'
    logit = 0
    for a in units[u]['anno']:
      r = a['response']
      vote_weight = worker_model[a['WorkerId']]['llr_weights'][r]
      # mult = {'0':-1, '1':1}[a['response']]
      if r=='1':
        logit += vote_weight
      else:
        logit -= vote_weight
    pred[u]['logit_1'] = logit
    pred[u]['num_votes'] = len(units[u]['anno'])
  return pred

def naive_vote(data):
  return predict(uniform_worker_an(data), data.units)
  
# def vote_tabulate(units):
#   pred = dict( (u,{}) for u in units)
#   for u in units:
#     pred[u]['votes'] = {'1':0, '0':0}
#     for a in units[u]['anno']:
#       pred[u]['votes'][a['response']] += 1
#   return pred
      
def eval_acc(pred, units):
  score = 0
  for u in units:
    if pred[u]['logit_1'] == 0:
      score += 0.5
    elif (pred[u]['logit_1'] >0)  == (units[u]['gold']=='1'):
      score += 1
  return score / len(units)

def merge(x,y):
  " Two dictionaries of dictionaries, with approximately same set of keys. "
  # assert set(x.keys()) == set(y.keys())
  ret = deepcopy(x)
  for (k,v) in y.iteritems():
    ret[k].update((v))
  return ret


###

def simple_sample(data, k, random=random):
  d2 = DataSet()
  d2.anno = []
  d2.units = dict( (u, copy(data.units[u]))  for u in data.units )
  for u in d2.units.values():
    if k >= len(u['anno']):
      u['anno'] = copy(u['anno'])
    else:
      u['anno'] = random.sample(u['anno'], k)
    d2.anno += u['anno']
  return d2

class CachedSampler:
  """ i think this has a bug, it was supposed to enhance replicability """
  def __init__(self, data, seed=20):
    self.samples = dict((k,[]) for k in range(2,11))
    self.data = data
    self.rgens = dict((k,random.Random(seed)) for k in range(2,11))
  
  def sample(self, i, k):
    while i >= len(self.samples[k]):
      self.samples[k].append( simple_sample(self.data, k=k, random=self.rgens[k]))
    return self.samples[k][i]
  
  def n_samples(self, n, k):
    for i in range(n):  yield self.sample(i,k)
    

def worker_serial_sample(data, k):
  """ the complicated thing explained at http://blog.doloreslabs.com/2008/09/amt-fast-cheap-good-machine-learning/#comment-542"""
  d2 = DataSet()
  # d2.anno = []
  d2.units = dict( (u, copy(data.units[u]))  for u in data.units )
  for u in d2.units.values():  u['anno'] = []
  
  workers = set( a['WorkerId'] for a in data.anno )
  w2anno = dict( (w,[]) for w in workers)
  for a in data.anno:
    w2anno[a['WorkerId']].append(a)
  workers = list(workers)
  random.shuffle(workers)
  
  done_units = set()
  
  count = 0
  for w in workers:
    for a in w2anno[w]:
      if a['orig_id'] in done_units:  continue
      unit_annos = d2.units[a['orig_id']]['anno']
      unit_annos.append(a)
      if len(unit_annos) == k:  done_units.add(a['orig_id'])
    # print len(done_units)
    if len(done_units) == len(d2.units):  break
    count += 1
  d2.anno = anno_from_units(d.units)
  print "number of workers used: %d" % count
  return d2


def acc_thresh_cutout(data, thresh=0.6):
  w_an = worker_an(data.anno)
  keep = set(w for w in w_an if w_an[w]['acc'] >= thresh)
  return trim_data_via_workers(data, keep)
  
def trim_data_via_workers(data, workers_to_keep):
  d2 = DataSet()
  d2.units = deepcopy(data.units)
  for u in d2.units.itervalues():
    u['anno'] = [a for a in u['anno'] if a['WorkerId'] in workers_to_keep]
  d2.anno = anno_from_units(d2.units)
  return d2

def loo(data, **kwds):
  allpred = {}
  for u in counter(data.units, name="unit"):
    ws = set(a['WorkerId'] for a in data.units[u]['anno'])
    train_anno = []    
    for u2 in data.units.itervalues():
      if u2['u'] == u: continue
      train_anno += [a for a in u2['anno'] if a['WorkerId'] in ws]
    w_model = worker_an(train_anno, **kwds)
    allpred[u] = predict(w_model, {u:data.units[u]})[u]
    allpred[u]['worker_model'] = w_model
  return allpred

def loo_thresh(data, thresh):
  allpred = {}
  for u in counter(data.units, name="unit"):
    ws = set(a['WorkerId'] for a in data.units[u]['anno'])
    train_anno = []    
    for u2 in data.units.itervalues():
      if u2['u'] == u: continue
      train_anno += [a for a in u2['anno'] if a['WorkerId'] in ws]
    
    w_model = worker_an(train_anno)
    keep = set(w for w in w_model if w_model[w]['acc'] >= thresh)
    # print keep
    trimmed_unit = deepcopy(data.units[u])
    trimmed_unit['anno'] = [a for a in trimmed_unit['anno'] if a['WorkerId'] in keep]
    # pprint(trimmed_unit)

    pred = predict( uniform_worker_an(), {u: trimmed_unit} )
    # print pred
    allpred.update(pred)
  return allpred
 
def xval(data, numfolds=20, worker_an=worker_an):
  """ Better to use loo() instead """
  fold_inds = repeat( range(0,numfolds), 1 + int(len(data.units)/numfolds) )
  random.shuffle(fold_inds)
  fold_inds = fold_inds[0:len(data.units)]
  assert len(fold_inds) == len(data.units)
  # return fold_inds
  
  calib_accs = []
  for fold in range(0,numfolds):
    # print fold,
    units = array(data.units.values())
    train = units[fold_inds == fold]
    test = units[fold_inds != fold]
    
    worker_model = worker_an(anno_from_units(train))
    test = dict( (u['u'],u) for u in test )
    pred = predict(worker_model, test)
    # print "fold %d calib_acc: %.3f" % (fold, eval_acc(pred, test))
    calib_accs.append(eval_acc(pred,test))
  return calib_accs

def resample_runner(data, iter, xvaller, sampler=None, sample_iter=None, subsetter=lambda d:d, do_calib=True):
  ret = []
  # for iterk in counter(range(0,iter), "sample"):
  for d in counter(sample_iter, "sample"):
    # d = sampler(data)
    naive_pred = naive_vote(d)
    raw_acc = eval_acc(naive_pred, d.units)
    
    if do_calib:
      calib_acc = mean(xvaller(subsetter(d)))
    else:
      calib_acc = -1
    print "  raw %.3f  calib %.3f  delta %+.1f%%" % (raw_acc, calib_acc, 100*(calib_acc-raw_acc))

    ret.append({'raw_acc':raw_acc, 'calib_acc':calib_acc})
  ret = util.DataFrame(ret)
  print "FINAL"
  print "raw acc: %.3f +/- %.3f   calib acc: %.3f +/- %.3f  delta: %+.2f%% +/- %.2f" % (
      mean(ret.raw_acc), std(ret.raw_acc), mean(ret.calib_acc), std(ret.calib_acc),
      mean(100*(ret.calib_acc - ret.raw_acc)),
      std( 100*(ret.calib_acc - ret.raw_acc)),
    )
  return ret


## For subsampling runs.  
## FIRST subsample, THEN apply worker modelling or thresholding techniques.
## Worker modelling needs to proceed under crossvalidation (either xval() or loo())
## 

SEED = 42
def gox(data, iter=20, k=5, **kwds):
  random.seed(SEED)
  cached_sampler = CachedSampler(data)
  return resample_runner(data, iter=iter, 
      xvaller=lambda d: xval(d, **kwds),
      # sampler=lambda d: sampler(d, k),
      sample_iter=cached_sampler.n_samples(iter,k)
    )

def goloo(data, iter=5, k=5, **kwds):
  random.seed(SEED)
  cached_sampler = CachedSampler(data)
  return resample_runner(data, iter=iter, 
      xvaller=lambda d: [eval_acc(loo(d, **kwds), d.units)],
      # sampler=lambda d: cached_sampler(d, k)
      sample_iter=cached_sampler.n_samples(iter,k)
    )

def gothresh(data, iter=20, k=5, sampler=simple_sample, thresh=0.6):
  random.seed(SEED)
  cached_sampler = CachedSampler(data)
  return resample_runner(data, iter=iter, 
      # xvaller=lambda d: xval(d, worker_an=uniform_worker_an),
      xvaller=lambda d: [eval_acc(naive_vote(d), d.units)],
      # sampler=lambda d: sampler(d,k),
      sample_iter=cached_sampler.n_samples(iter,k),
      subsetter=lambda d: acc_thresh_cutout(d, thresh=thresh)  )

# def goraw(data, iter=20, k=5, sampler=simple_sample):
#   random.seed(SEED)
#   cached_sampler = CachedSampler(data)
#   return resample_runner(data, iter=iter,
#       xvaller=lambda d: [eval_acc(naive_vote(d), d.units)],
#       # sampler=lambda d: simple_sample(d,k),
#       sample_iter=cached_sampler.n_samples(iter,k),
#       do_calib=False
#       )

# 


###### no subsampling, for comparison to bob carpenter's experiment
# http://lingpipe-blog.com/2008/09/15/dolores-labs-text-entailment-data-from-amazon-mechanical-turk/
# results written up at http://blog.doloreslabs.com/2008/09/amt-fast-cheap-good-machine-learning/#comment-583

def gothresh2(data, threshes=arange(.4,1, step=.05)):
  global accs  # dictionary
  # threshes = list(threshes) + list(arange(.45,1, step=.01))
  # print threshes
  for t in threshes:
    if t in accs:  print "Skipping thresh %.3f, already done" % t
    p = loo_thresh(data, t)
    num_anno = sum(u['num_votes'] for u in p.itervalues())
    acc = eval_acc(p, data.units)
    print "thresh %.2f  num anno %d  acc %.3f"  % (t, num_anno, acc)
    accs[ t ] = {'acc':acc, 'num_anno':num_anno}
    
    # print " ",; pprint(uniq_c([len(u['anno']) for u in d2.units.itervalues()]))
  # d2 = 

# def 
  
  
