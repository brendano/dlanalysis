add_crap <- function(x) {
  attr(x, 'data_type') = 'categ'
  attr(x, 'candidates') = levels(x$response)
  x
}
d1 = data.frame(
  X.amt_worker_ids = paste('w', c(1,2,3, 1,2,3, 1,2,3),  sep=''),
  orig_id = paste('u', c(1,1,1, 2,2,2, 3,3,3),  sep=''),
  response = paste('', c(1,1,1, 1,1,1, 1,0,0),  sep='')
  )
d1 = add_crap(d1)
# attr(em_test_d1, 'data_type') = 'categ'
# attr(em_test_d1, 'candidates') = levels(em_test_d$response)
# labels should converge to:  1 1 0


d2 = load_categ_anno(pipe("cat <<EOF
X.amt_worker_ids orig_id response

# test case: we want smart to outvote the mob
smart u1 1
dumb1 u1 0
dumb2 u1 0

# everyone is accurate when saying '1'
smart u2_unan 1
dumb1 u2_unan 1
dumb2 u2_unan 1
smart u3_unan 1
dumb1 u3_unan 1
dumb2 u3_unan 1

# # but when saying '0', dumbs are always wrong
smart u4_confuse1 1
dumb1 u4_confuse1 0
dumb2 u4_confuse1 1
smart u5_confuse1 1
dumb1 u5_confuse1 0
dumb2 u5_confuse1 1

smart u6_confuse2 1
dumb1 u6_confuse2 1
dumb2 u6_confuse2 0
# smart u7_confuse2 1
# dumb1 u7_confuse2 1
# dumb2 u7_confuse2 0

EOF
"), sep=" ", comment.char='#')


d3 = load_categ_anno(pipe("cat <<EOF
X.amt_worker_ids orig_id response

sparseman u1 1
dumb1 u1 1
dumb2 u1 0

dumb1 u2 1
dumb2 u2 0
dumb1 u3 1
dumb2 u3 0
dumb1 u4 1
dumb2 u4 0
EOF
"), sep=" ", comment.char='#')

# very knife edgy.  sparseman is tie breaker, he can flip either way
# sparseman tie breaks, so labels=[1  1 1 1]   is stable
# labels=[0  1 1 1]  ==>  converge to: 1 1 1 1
#  .. on 1st iter, sparseman's default pos vote flips u1 to 1
# labels=[0  0 1 1]  ==>  converge to: 0 0 0 0
#  .. on 1st iter we learn that sparseman has negative vote (on uniform accuracy priors)



d4 = load_categ_anno(pipe("cat <<EOF
X.amt_worker_ids orig_id response

sparseman 1consensus 1
dumb1 1consensus 1
dumb2 1consensus 1

sparseman 2v1 1
dumb1 2v1 1
dumb2 2v1 0

dumb1 noise1 1
dumb2 noise1 0
dumb1 noise1.2 0
dumb2 noise1.2 1

dumb1 noise2 1
dumb2 noise2 0
dumb1 noise2.2 0
dumb2 noise2.2 1
EOF
"), sep=" ", comment.char='#')

# consensus point anchors sparseman positive (even on uniform acc priors)
