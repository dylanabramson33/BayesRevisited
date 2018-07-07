(*  COMP 212 Homework 8:  Higher-order programming
*   
*   N. Danner
*)

structure Hw8 =
struct

  exception Unimplemented
  exception Fail
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

  (*  ********************
  *   Un-Curried list functionals.
  *
  *   You may use these versions of the standard list functionals if you wish,
  *   but for full credit you must use the functionals from the List structure.
  *)

  (*  filter0(p, [x_0,...,x_{n-1}]) = ys, where ys is the sublist of xs
  *  consisting of those elements x of xs such that p(x) = true.
  *
  *  This specification can be written much more formally; do you see how?
  *)
  fun filter0(p : 'a -> bool, xs : 'a list) : 'a list =
    case xs of
         [] => []
       | y :: ys => if p y then y :: filter0(p, ys) else filter0(p, ys)

  (*  find0(0, [x_0,...,x_{n-1}]) = SOME(x_i), if p(x_i) = true and 
  *                                            p(x_j) = false for j < i
  *                               = NONE,      if there is no i such that 
  *                                            p(x_i) = true
  *)
  fun find0(p : 'a -> bool, xs : 'a list) : 'a option =
    case xs of
         [] => NONE
       | y :: ys => if p y then SOME y else find0(p, ys)

  (*  map0(f, [x_0,...,x_{n-1}]) = [f(x_0),...,f(x_{n-1})].
  *)
  fun map0(f : 'a -> 'b, xs : 'a list) : 'b list =
    case xs of
         [] => []
       | y :: ys => f(y) :: map0(f, ys)

  (*  fold0(f, b, [x_0,...,x_{n-1}]) =
  *   f(x_0, f(x_1, f(..., f(x_{n-1}, b)...))).
  *)
  fun fold0(f : 'a*'b -> 'b, b : 'b, xs : 'a list) : 'b =
    case xs of
         [] => b
       | y :: ys => f(y, fold0(f, b, ys))


  (* checkAndAdd(x,ys) = the union of x and ys
  *)
  fun checkAndAdd(x,ys) = if List.exists(fn(y) => x = y) ys then ys else x::ys
  
  (*  incrCount (w, wcs) = wcs', where:
  *   - wcs'(w) = 1 + wcs(w)
  *   - wcs'(w') = if w' <> w.
  *   In other words, incrCount keyws (w, wcs) is just like wcs, except the
  *   count for w is incremented.
  *)
  fun incrCount(w : string, wcs : wordcounts) = 
    List.map(fn(w',wc) => if  w = w' then (w',wc+1) else (w',wc)) wcs 

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
    List.map(fn(c',wcs') => if c = c' then (c,wcs) else (c',wcs')) ccs  

  (*  addToCounts(wcs, d) = wcs', where
  *   wcs'(w) = wcs(w) + (number of occurrences of w in d)
  *   In other words, addToCounts(wcs, d) is just like wcs, except
  *   all the counts of all the words are incremented by the number of
  *   times they occur in d.
  *)  
  fun addToCounts(wcs : wordcounts, d : doc) = List.foldr incrCount wcs d
  
  (*  initCounts(keyws) = wcs, where wcs(w) = 0 for each w in keys.
  *)
  fun initCounts(keyws) = List.map(fn x => (x,0)) keyws  

  fun count (keyws : string list, cds : (category*doc) list) : ctycounts =
      let
        val initKCounts = initCounts(keyws)
        val initCtyCounts = List.map(fn(cat,_) => (cat,initKCounts)) cds  
        val removedDups = List.foldr checkAndAdd [] initCtyCounts   
        
        (*  getCtyCounts (ccs, c) = ccs(c) if c is a category in ccs 
        *)
        fun getCtyCounts(ccs : ctycounts, c : category) : (string*int) list = 
          let 
            val ctycount = List.find(fn((c',_)) => c = c') ccs 
          in 
            case ctycount of 
              NONE => raise Fail
              |SOME(c,wc) => wc 
          end 

          (* updCtyCounts'((c,d) : category*doc, ccs : ctycounts) = 
          * ccs with updated for c and d 
          *)
          fun updCtyCounts'((c,d) : category*doc, ccs : ctycounts) = 
            updCtyCounts(ccs,c,addToCounts(getCtyCounts(ccs,c),d))       
      
      in 
        List.foldr updCtyCounts' removedDups cds
      end 
    

    (*  getProb (wps, w) = wps(w).
    *)
    fun getProb(wps : wordprobs, w : string) : real =
    let 
      val wordprob = List.find(fn((w',prob)) => w = w') wps
    in 
      case wordprob of 
        NONE => raise Fail
        |SOME(w,prob) => prob
    end 


  fun makeWordProbs(wcs: wordcounts) : wordprobs = 
    let 
      
      (*  nWords = n_0 + ... + n_{k-1}, 
      *   where wcs = [(w_0,n_0),...,(w_{k-1}, n_{k-1})].
      *     
      *)
      val nWords = List.foldr(fn((_,count),sum) => count + sum) 0 wcs 

    in 
      List.map(fn((w,n)) => (w,real(Int.max(1, n))/real(nWords))) wcs
    end 


  (*  makeCtyProbs ccs = cps, where cps(c) = makeWordProbs(ccs(c))
  *)
  fun makeCtyProbs(ccs : ctycounts) : ctyprobs =
    List.map(fn(c,wcs) => (c,makeWordProbs wcs)) ccs



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
      List.map(fn((w,n))=> (n,getProb(wps,w))) wcs 

    (*  getCountsWProbs([(w_0,n_0),...,(w_{k-1}, n_{k-1})]) =
    *     [(n_0, p_0),..., (n_{k-1}, p_{k-1})], where p_i = getProb(wps, w_i).
    *)
    val countsWProbs : (int*real) list = getCountsWProbs(dCounts)
     
  in  
    List.foldr (fn((n,p), sum) => 
      real(n)*Math.log10(p) + sum) 0.0 countsWProbs
  end 
  

  (*  makeClassifier (keyws, cds) = cl, where cl(d) = [...,(c, r),...],
  *   where the list ranges over pairs (c, r) such that c is a category
  *   in cds and r is the log-likelihood that the document d is produced
  *   by the model computed from c.
  *)
  fun makeClassifier
      (keyws : string list, cds : (category*doc) list) 
      : doc -> (category*real) list =
    let 
      
      (*  cps(c) = makeWordProbs(ccs(c)) for each category c in cds, where
      *     ccs(c) is the word count for all documents in cds with category c.
      *)
      val cps : ctyprobs = makeCtyProbs (count(keyws, cds))
      
      (* classifier(d) = the classifier created from document d
      *)
      fun classifer(d: doc) : (category*real) list  = 
        List.map(fn(c,wps) => (c,computeLL(keyws,d,wps))) cps

    in 
      classifer
    end   


end
