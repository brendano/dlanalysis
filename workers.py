# reimplementation ...
from __future__ import division
from math import *
import numpy
from numpy import *
from random import *
from copy import *   # clobber numpy.copy
from collections import defaultdict
import sys,os


# sys.stdout=os.fdopen(1,'w',0)

def log2(x):  return numpy.log2(x)

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
  ret = []
  global x
  for rec in anno:
    rec2 = copy(rec)
    rec2['WorkerId'] = rec2.pop('!amt_worker_ids')
    rec2['AssignmentId'] = rec2.pop('!amt_annotation_ids')
    ret.append(rec2)
  return ret

LABELS = ['0','1']

def worker_an(anno, **kwds):
  workers = set(r['WorkerId'] for r in anno)
  worker_info = defaultdict(lambda:None,  ((w,{}) for w in workers) )
  for w in workers:
    myanno = [r for r in anno if r['WorkerId'] == w]
    worker_info[w] = one_worker_model(myanno)
  
  unk_model = one_worker_model(anno, **kwds)
  worker_info.default_factory = lambda: unk_model
  return worker_info

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
  
def uniform_worker_an(anno):
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
  return pred
  
def vote_tabulate(units):
  pred = dict( (u,{}) for u in units)
  for u in units:
    pred[u]['votes'] = {'1':0, '0':0}
    for a in units[u]['anno']:
      pred[u]['votes'][a['response']] += 1
  return pred
      
class DataSet:
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
    return self

    
def eval_acc(pred, units):
  return mean( [ (pred[u]['logit_1']>0) == (units[u]['gold']=='1')  for u in units] )

def merge(x,y):
  " Two dictionaries of dictionaries, with approximately same set of keys. "
  assert set(x.keys()) == set(y.keys())
  ret = deepcopy(x)
  for (k,v) in y.iteritems():
    ret[k].update((v))
  return ret


###

def simple_sample(data, k):
  d2 = DataSet()
  d2.anno = []
  d2.units = dict( (u, copy(data.units[u]))  for u in data.units )
  for u in d2.units.values():
    if k >= len(u['anno']):
      u['anno'] = copy(u['anno'])
    else:
      u['anno'] = sample(u['anno'], k)
    d2.anno += u['anno']
  return d2

def worker_serial_sample(data, k):
  d2 = DataSet()
  d2.anno = []
  d2.units = dict( (u, copy(data.units[u]))  for u in data.units )
  for u in d2.units.values():  u['anno'] = []
  
  workers = set( a['WorkerId'] for a in data.anno )
  w2anno = dict( (w,[]) for w in workers)
  for a in data.anno:
    w2anno[a['WorkerId']].append(a)
  workers = list(workers)
  shuffle(workers)
  
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
  print "number of workers used: %d" % count
  return d2

def acc_thresh_cutout(data, thresh=0.6):
  w_an = worker_an(data.anno)
  keep = set(w for w in w_an if w_an[w]['acc'] >= thresh)
  d2 = trim_data_via_workers(data, keep)
  return d2
  

def trim_data_via_workers(data, workers_to_keep):
  d2 = DataSet()
  d2.units = deepcopy(data.units)
  for u in d2.units.values():
    u['anno'] = [a for a in u['anno'] if a['WorkerId'] in workers_to_keep]
  d2.anno = anno_from_units(d2.units)
  return d2

def anno_from_units(units):
  if type(units) == dict:
    units = units.itervalues()
  anno = []
  for u in units:
    anno += u['anno']
  return anno

def xval(data, numfolds=20, worker_an=worker_an):
  fold_inds = repeat( range(0,numfolds), 1 + int(len(data.units)/numfolds) )
  shuffle(fold_inds)
  fold_inds = fold_inds[0:len(data.units)]
  assert len(fold_inds) == len(data.units)
  # return fold_inds
  
  calib_accs = []
  for fold in range(0,numfolds):
    print fold,
    units = array(data.units.values())
    train = units[fold_inds == fold]
    test = units[fold_inds != fold]
    
    worker_model = worker_an(anno_from_units(train))
    test = dict( (u['u'],u) for u in test )
    pred = predict(worker_model, test)
    # print "fold %d calib_acc: %.3f" % (fold, eval_acc(pred, test))
    calib_accs.append(eval_acc(pred,test))
  return calib_accs

def bootstrapper(data, iter, xvaller, sampler=None, subsetter=lambda d:d, do_calib=True):
  ret = []
  for iterk in range(0,iter):
    d = sampler(data)
    naive_pred = predict(uniform_worker_an(d.anno), d.units)
    raw_acc = eval_acc(naive_pred, d.units)
    
    if do_calib:
      calib_acc = mean(xvaller(subsetter(d)))
    else:
      calib_acc = -1
    print "  raw %.3f  calib %.3f" % (raw_acc, calib_acc)

    ret.append({'raw_acc':raw_acc, 'calib_acc':calib_acc})
  print "FINAL"
  print "raw acc: %.3f +/- %.3f   calib acc: %.3f +/- %.3f" % (
      mean([x['raw_acc'] for x in ret]), 
      std([x['raw_acc'] for x in ret]), 
      mean([x['calib_acc'] for x in ret]), 
      std([x['calib_acc'] for x in ret]), 
    )
  return ret

def gogo1(data, iter=20, k=5, sampler=simple_sample, **kwds):
  return bootstrapper(data, iter=iter, 
      xvaller=lambda d: xval(d, **kwds),
      sampler=lambda d: sampler(d, k)
    )

def gogo2(data, iter=20, k=5, sampler=simple_sample, thresh=0.6):
  return bootstrapper(data, iter=iter, 
      xvaller=lambda d: xval(d, worker_an=uniform_worker_an),
      sampler=lambda d: sampler(d,k),
      subsetter=lambda d: acc_thresh_cutout(d, thresh=thresh)  )

def gogo3(data, iter=20, k=5):
  return bootstrapper(data, iter=iter, 
      xvaller=lambda d: xval(d, worker_an=uniform_worker_an),
      sampler=lambda d: simple_sample(d,k),
      do_calib=False
      )
