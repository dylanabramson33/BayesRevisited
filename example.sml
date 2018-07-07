(*  COMP 212 Homework 4:  Programming with lists and Bayesian learning.
*   
*   N. Danner
*   Spring 2018
*)

structure Hw4 =
struct

  (*  Naming conventions.  I found it useful to consistently use the same names
  *   for the same types of values:
  *   - c : category
  *   - d : doc
  *   - ccs : ctycounts
  *   - wcs : wordcounts
  *   - wps : wordprobs
  *   - keyws : string list (list of keywords)
  *)

  (*  These type declarations just let us use meaningful names for the types
  *  that we use here.
  *)

  type category = string
  type doc = string list

  (*  Think of a value wcs : wordcounts as a function from strings to ints.
  *   If wcs = [(s_0, i_0),...,(s_n, i_n)], then the value wcs on string s
  *   is computed as follows:
  *   - if s = s_j, then the value is i_j
  *   - if s <> s_j for any j, then the value is undefined.
  *   In the documentation below, we will write wcs(s) for the value of wcs
  *   on string s.
  *
  *   Think of each of the list types below in the same way.
  *)
  type wordcounts = (string*int) list

  (*  The type of functions from categories to wordcounts.
  *)
  type ctycounts = (category*wordcounts) list

  (*  A type of functions from strings to reals.
  *)
  type wordprobs = (string*real) list 

  (*  A type of functions from categories to wordprobs.
  *)
  type ctyprobs = (category*wordprobs) list

  (*  remove(xs, x) = ys, where ys is a copy of xs with all occurrences of x
  *  removed.
  *)
  fun remove (xs : ''a list, x : ''a) : ''a list =
    case xs of
         [] => []
       | y :: ys => if x = y then remove(ys, x) else y :: remove(ys, x)

  fun 


  



  (*  removeDups xs = ys, where ys is a copy of xs with all duplicates removed.
  *)
  fun removeDups (xs : ''a list) : ''a list =
    case xs of
         [] => []
       | x :: xs => x :: removeDups(remove(xs, x))

  

  (*  getCtyCounts (ccs, c) = ccs(c).
  *)
  fun getCtyCounts (ccs : ctycounts, c : category) : wordcounts =
    case ccs of
         [] => raise Fail "getCtyCounts"
       | (cty, wcs) :: ccs' => if c = cty then wcs else getCtyCounts(ccs', c)

  fun getCtyCounts(c,xs) = 
    let 
      (newC,wordcount) = List.find(fn(c') => c = c')
    in 
      case (newC,wordcount) of 
        NONE => raise Fail
        |SOME => wordcount 

  (*  updCtyCounts (ccs, c, wcs) = ccs', where:
  *   - ccs'(c) = wcs
  *   - ccs'(c') = ccs(c') if c' <> c.
  *   In other words, updCtyCounts(ccs, c, wcs) is just like ccs, except
  *   the value on category c is wcs.
  *)
  fun updCtyCounts (
      ccs : ctycounts, 
      c : category, 
      wcs : wordcounts) : ctycounts =
    case ccs of
         [] => []
       | (cty, wcs') :: ccs' =>
           if c = cty then (cty, wcs) :: ccs'
           else (cty, wcs') :: updCtyCounts(ccs', c, wcs)

  fun updCtyCounts (
      ccs : ctycounts, 
      c : category, 
      wcs : wordcounts) : ctycounts =
    List.map(fn(c',wcs') = if c = c' then (c,wcs) else (c,wcs') ccs


  (*  incrCount (w, wcs) = wcs', where:
  *   - wcs'(w) = 1 + wcs(w)
  *   - wcs'(w') = if w' <> w.
  *   In other words, incrCount keyws (w, wcs) is just like wcs, except the
  *   count for w is incremented.
  *)
  fun incrCount (w : string, wcs : wordcounts) : wordcounts =
    case wcs of
         [] => []
       | (word, cnt) :: wcs' => if w = word then (word, cnt+1) :: wcs'
                                else (word, cnt) :: incrCount(w, wcs')

  
                         
  (*  addToCounts(wcs, d) = wcs', where
  *     wcs'(w) = wcs(w) + (number of occurrences of w in d)
  *   In other words, addToCounts(wcs, d) is just like wcs, except
  *   all the counts of all the words are incremented by the number of
  *   times they occur in d.
  *)
  fun addToCounts (
      wcs : wordcounts,
      d : doc) : wordcounts =
    case d of
         [] => wcs
       | w :: ws => incrCount(w, addToCounts(wcs, ws))

    

  (*  initCounts(keyws) = wcs, where wcs(w) = 0 for each w in keys.
  *)
  fun initCounts (keyws : string list) : wordcounts =
    case keyws of
         [] => []
       | w :: ws => (w, 0) :: initCounts(ws)

  (*  count (keyws, cds) = ccs, where ccs(c) is the word count for all documents
  *   in cds with category c.
  *)
  fun count (keyws : string list, cds : (category*doc) list) : ctycounts =
  let
    val initKCounts = initCounts keyws

    (*  makeInitCtyCounts(ctys) = ccs, where ccs(c) = initKCounts for each
    *   c in ctys.
    *)
    fun makeInitCtyCounts(ctys : category list) : ctycounts =
      case ctys of
           [] => []
         | cty :: ctys' => (cty, initKCounts) :: makeInitCtyCounts(ctys')

    (*  getCtys([(c_0, d_0),..., (c_{n-1}, d_{n-1})] = [c_0,...,c_{n-1}].
    *)
    fun getCtys(cds : (category*doc) list) : category list =
      case cds of
           [] => []
         | (cty, _) :: cds' => cty :: getCtys(cds')



    (*  initCtyCounts = ccs, where ccs(c) = initKCounts for each (c, d) in cds,
    *   without duplicates.
    *)
    val initCtyCounts = makeInitCtyCounts(removeDups(getCtys(cds)))

    (*  countWords(cds) = ccs, where ccs(c) is the word count for all documents
    *   in cds with category c.
    *)
    fun countWords(cds : (category*doc) list) : ctycounts =
      case cds of
           [] => initCtyCounts
         | (c, d) :: cds' =>
           let
             val ccs = countWords(cds')
           in
             updCtyCounts(ccs, c, addToCounts(getCtyCounts(ccs, c),d)
           end
  in
    countWords(cds)
  end

  (*  getProb (wps, w) = wps(w).
  *)
  fun getProb (wps : wordprobs, w : string) : real =
    case wps of
         [] => raise Fail "getProb"
       | (word, p) :: wps' => if w = word then p else getProb(wps', w)

  (*  makeWordProbs wcs = wps, where wps(s) = wcs(s)/n and n is the sum
  *   of the counts of all the words in wcs.
  *)
  fun makeWordProbs (wcs : wordcounts) : wordprobs =
  let
    
    fun countWords(wcs : wordcounts) : int =
      case wcs of
           [] => 0
         | (_, n) :: wcs' => n + countWords(wcs')

    (*  nWords = n_0 + ... + n_{k-1}, where 
    *     wcs = [(w_0,n_0),...,(w_{k-1}, n_{k-1})].
    *)
    val nWords = countWords(wcs)

    (*  countProbs([(w_0, n_0),...,(w_{k-1},n_{k-1})]) =
    *     [(w_0,p_0),...,(w_{n-1}, p_{n-1})], where 
    *     p_i = max(1, n_i)/(n_0 + ... + n_{k-1}).
    *)
    fun countProbs(wcs : wordcounts) : wordprobs =
      case wcs of
           [] => []
         | (w, n) :: wcs' => 
             (w, real(Int.max(1, n))/real(nWords)) :: countProbs(wcs')
  in
    countProbs(wcs)
  end

  (*  makeCtyProbs ccs = cps, where cps(c) = makeWordProbs(ccs(c))
  *)
  fun makeCtyProbs(ccs : ctycounts) : ctyprobs =
    case ccs of
         [] => []
       | (c, wcs) :: ccs' => (c, makeWordProbs(wcs)) :: makeCtyProbs(ccs')

  (*  computeLL (keyws, d, wps) = the log-likelihood of the document d
  *   being produced by the model wps.  See assignment description for details.
  *)
  fun computeLL (keyws : string list, d : doc, wps : wordprobs) : real =
  let
    (*  dCounts(w) = the number of occurrences of w in d, for each w in keyws.
    *)
    val dCounts : wordcounts = addToCounts(initCounts keyws, d)

    (*  getCountsWProbs([(w_0,n_0),...,(w_{k-1}, n_{k-1})]) =
    *     [(n_0, p_0),..., (n_{k-1}, p_{k-1})], where p_i = getProb(wps, w_i).
    *)
    fun getCountsWProbs(wcs : wordcounts) : (int*real) list =
      case wcs of
           [] => []
         | (w, n) :: wcs' => (n, getProb(wps, w)) :: getCountsWProbs(wcs')

    (*  countsWProbs = [(n_0, p_0),..., (n_{k-1}, p_{k-1})], where
    *     keyws = [w_0,..., w_{k-1}],
    *     n_i   = the number of occurrences of w_i in d
    *     p_i   = getProb(wps, w_i)
    *)
    val countsWProbs = getCountsWProbs(dCounts)

    (*  getLL([(n_0, p_0),..., (n_{k-1}, p_{k-1})]) =
    *     n_0*log(p_0) + ... + n_{k-1}*log(p_{k-1}), where log = log base 10.
    *)
    fun getLL(cps : (int*real) list) : real =
      case cps of
           [] => 0.0
         | (n, p) :: cps' => real(n)*Math.log10(p) + getLL(cps')
  in
    getLL(countsWProbs)
  end

  (*  makeClassifier (keyws, cds) = cl, where cl(d) = [...,(c, r),...],
  *   where the list ranges over pairs (c, r) such that c is a category
  *   in cds and r is the log-likelihood that the document d is produced
  *   by the model computed from c.
  *)
  fun makeClassifier (
      keyws : string list, 
      cds : (category*doc) list,
      d : doc) : (category*real) list =
  let
    (*  cps(c) = makeWordProbs(ccs(c)) for each category c in cds, where
    *     ccs(c) is the word count for all documents in cds with category c.
    *)
    val cps : ctyprobs = makeCtyProbs (count(keyws, cds))

    (*  makeClassifier(cps) = [(c_0, p_0),..., (c_{k-1}, p_{k-1})], where
    *    {c_0,...,c_{k-1}} are the categories in cps and p_i is the 
    *    log-likelihood that d belongs to category c_i.
    *)
    fun makeClassifier(cps : ctyprobs) : (category*real) list =
      case cps of
           [] => []
         | (c, wps) :: cps' => 
             (c, computeLL(keyws, d, wps)) :: makeClassifier(cps')

  in
    makeClassifier(cps)
  end

end
