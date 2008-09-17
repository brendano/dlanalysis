from __future__ import division
import sys, time, math
import numpy

class DataFrame(list):
  " simplest implementation: list of hashes plus syntactic sugar "

  def __getitem__(self, i):
    if type(i) == str:
      return numpy.array([x[i] for x in self])
    else:
      return list.__getitem__(self, i)
      
  def __getattr__(self, attr):
    return self[attr]

class Counter:
  """ Usages
  
  # wrap any iterator
  for x in counter(range(20)):
    time.sleep(.1)
  
  for x in counter(x for x in range(20)):
    time.sleep(.1)
  
  # generator doesn't know its length; but you can fill in
  for x in counter((x for x in range(20)), max=20)):
    time.sleep(.1)
  
  # name it
  for x in counter(range(20), name="trial"):
    time.sleep(.1)
  
  # manual, non-wrapper usage.  API is: start, next, end
  counter.start()
  for x in range(0,50):
    time.sleep(.1)
    counter.next()
  counter.end()
  
  # if you know the max, can not bother with end
  counter.start(max=50)
  for x in range(0,50):
    time.sleep(.1)
    counter.next()
  """

  def __init__(self):
    self.out = sys.stdout
    self.need_restart = True
  
  def start(self, max=None, name="iter"):
    self.count = 0
    self.name = name
    self.max = max
    self.last_size = None
    self.start_time = self.when_last_line = time.time()
    self.need_restart = False
    self.show_line("Starting ")
  
  def next(self):
    if self.need_restart:  self.start()
    
    self.count += 1
    since_last = time.time() - self.when_last_line
    if since_last < 0.05:  return
    self.show_progress_line("%s %d" % (self.name, self.count))
    if self.max and self.count >= self.max: self.end()
  
  def end(self):
    if self.need_restart: return  # idempotent..
    elapsed = time.time() - self.start_time
    self.show_line("Done at %s %d, %s total  %s" % (
        self.name,
        self.count, smart_time_fmt(elapsed), self.rate_str(self.count/elapsed),))
    self.out.write("\n")
    self.out.flush()
    self.need_restart = True
  
  def __call__(self, iterator, **kwds):
    if 'max' not in kwds and hasattr(iterator, '__len__'):
      kwds['max'] = len(iterator)
    self.start(**kwds)
    for x in iterator:
      yield x
      self.next()
    self.end()
    
    
  # privates below
  
  def rate_str(self, rate):
    if rate <= 0:  return "(rate N/A)"
    rate_strs = []
    rate_strs.append("%s %s/sec" % (smart_fmt(rate), self.name))
    if rate < 1: rate_strs.append("%s %s/min" % (smart_fmt(rate*60), self.name))
    if rate < 1/60: rate_strs.append("%s %s/hr" % (smart_fmt(rate*60*60), self.name))
    return "(%s)" %  (", ".join(rate_strs))
    
  def show_progress_line(self, s):
    if self.max:
      s += " of %d" % self.max
    
    rate = self.count / (time.time() - self.start_time)
    s += " " + self.rate_str(rate)
    
    if self.max and rate > 0:
      projection = (self.max - self.count) / rate
      s += "  %s remaining" % smart_time_fmt(projection)
    s += " "
    self.show_line(s.capitalize())

  def show_line(self, s):
    if self.last_size:
      self.out.write("\b" * self.last_size)
      self.out.flush()
    self.out.write(s)
    self.out.flush()
    
    self.last_size = len(s)
    self.when_last_line = time.time()

counter = Counter()
  

def smart_fmt(x, space=False):
  def fmt1():
    is_neg = x < 0
    d = int((math.log10(abs(x))))
    if x >= 1:
      shelf = 3 * (d//3)
    else:
      shelf = 3 * (d//3)
    if shelf>9: shelf=9
    if shelf<-6: shelf=-6
    num_dec = max(0,  2 - abs( abs(d)-abs(shelf)))
    if x<1:  num_dec+=1
    fmt = "%." +str(num_dec)+ "f"
    post_sym = {-6:"micro", -3:"milli", 0:"", 3:"k", 6:"M", 9:"G"}
    return (fmt % (x / 10**shelf), post_sym[shelf])
  
  s,sym = fmt1()
  if sym != "": s += " "+sym
  if space and not s.endswith(" "): s += " "
  return s
  
    # if x < 1e-6: return "%.1f micro" % (x*1e6)
    # if x < 1e-3: return "%.1f milli" % (x*1e3)
    # if x < 1:    return "%.3f" % x
    # if x < 10:   return "%.2f" % x
    # if x < 100:  return "%.1f" % x
    # if x < 1000: return "%d" % x
    # if x < 10*1000:  return "%.2f k" % (x/1e3)
    # if x < 1e10: return "%.1f M" % (x/1e6)
    # if x < 1e13: return "%.1f G" % (x/1e9)
    # else: return "%s" % x
  # s = fmt1()

def smart_time_fmt(secs):
  if secs < 60:
    return "%ds" % secs
  if secs < 60*60:
    return "%dm:%.2ds" % ((secs//60) % 60, secs % 60)
  else:
    return "%d:%.2d:%.2d" % (secs//(60*60), (secs//60) % 60, secs % 60)
  
## counter test
if 0:
  import time,random
  util.counter.start(100)
  for x in range(0,100):
    time.sleep(0 + random.random() * 2)
    #time.sleep(0.4)
    util.counter.next()
  util.counter.end()
  