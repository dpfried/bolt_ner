;;;; file utilities
(defun read-lfs (filename)
  "filename should be a file that contains a list of responses. Each response is also a list - the car is the original string, and the remaining elements are the parse trees returned by TRIPS for that string"
  (with-open-file (file filename)
    (read file nil)))

(defun scene-lfs (sequence-id scene-id &optional (base-path "~/bolt_gt/dumps_raw/") (filename "responses.parsed"))
  "load the logical forms from a given sequence and scene"
  (read-lfs (format nil "~A~A/~A/~A" base-path sequence-id scene-id filename)))

(defun read-scene-schematic (sequence-id scene-id &optional (base-path "~/bolt_gt/dumps_raw/") (filename "schematic.lisp"))
  (with-open-file (file (format nil "~A~A/~A/~A" base-path sequence-id scene-id filename))
    (read file nil)))

(defun read-scene (sequence-id scene-id &optional (base-path "~/bolt_gt/dumps_raw/"))
  (make-scene 
   :schematic (read-scene-schematic sequence-id scene-id base-path)
   :parse-forest (mapcan #'cdr (scene-lfs sequence-id scene-id base-path))))

(defun camera (schematic)
  (car schematic))

(defun objects (schematic)
  (cdr schematic))

(defun last-object (schematic)
  (car (last (objects schematic))))

(defun object-shape (object)
  (cdr (assoc 'type object)))

(defun object-color (object)
  (cdr (assoc 'color (cdr (assoc 'settings object)))))

(defun export-nps (sequence-id scene-id &optional (base-path "~/bolt_gt/dumps_raw/") (filename "noun-phrases"))
  (let ((scene-lfs (scene-lfs sequence-id scene-id)))
    (with-open-file (stream (format nil "~A~A/~A/~A" base-path sequence-id scene-id filename) :direction :output :if-exists :supersede)
      (dolist (response-list scene-lfs)
	(let* ((responses (cdr response-list))
	       (nps (mapcan #'get-nps responses))
	       (innermost-nps (remove-if #'(lambda (sexps) (find-if #'(lambda (sexp) (has-internal-node? sexp 'np)) sexps)) nps))
	       (word-list (mapcar #'(lambda (sexps) (words-from-forest sexps)) innermost-nps)))
	  (print "new set")
	  (print "nps")
	  (print nps)
	  (print "innermost:")
	  (print innermost-nps)
	  (print "words:")
	  (print word-list)
	  (print word-list stream))))))

;;;; general utilities

(defmacro while (test &rest body)
  `(do ()
       ((not ,test))
     ,@body))

(defun comparator (test &optional (key #'identity))
  "Comparison operator: auxilliary function used by EXTREMUM"
  (declare (optimize (speed 3) (safety 0) (space 0) (debug 1)))
  (lambda (a b) 
    (let ((f_a (funcall key a))
	  (f_b (funcall key b)))
      (if (funcall test f_a f_b)
	  (values a f_a)
	  (values b f_b)))))

(defun extremum (sequence predicate
		  &key (key #'identity) (start 0) end)
  "Returns the element of SEQUENCE that would appear first if the
sequence were ordered according to SORT using PREDICATE and KEY using
an unstable sorting algorithm. See http://www.cliki.net/EXTREMUM for
the full specification."
    (reduce (comparator predicate key) sequence
	        :start start :end end))

;(defun argmax (lst key)
;  (extremum lst #'> :key key))

(defun argmax (lst key)
  (let (max-item max-val)
    (dolist (x lst)
      (if (or (not max-val)
	      (> (funcall key x) max-val))
	  (setf max-item x
		max-val (funcall key x))))
    (values max-item max-val)))
    

(defun inclusion-exclusion? (lst one-of none-of)
  "return true iff the lst contain all the atoms in one-of and none of the atoms in none-of"
  (and (> (length (intersection one-of lst)) 0)
       (= (length (intersection none-of lst)) 0)))

;;;; statistics
(defun mean (vals)
  (/ (reduce #'+ vals)
     (length vals)))

(defun standard-deviation (vals &optional mean)
  (let ((m (or mean (mean vals))))
    (sqrt (/ (reduce #'+ (mapcar #'(lambda (v) (expt (- v m) 2))
			      vals))
	     (length vals)))))

(defun standardize (vals &optional mean standard-deviation)
  (let ((m (or mean 
	       (mean vals)))
	(sd (or standard-deviation 
		(standard-deviation vals))))
  (mapcar #'(lambda (v)
	      (/ (- v m)
		 sd))
	  vals)))

;;;; hash-table utilities

(defun hash-table->alist (ht)
  (let (alist)
    (maphash #'(lambda (key value) (setf alist (acons key value alist))) ht)
    alist))

(defun alist->hash-table (alist)
  (let ((ht (make-hash-table)))
    (dolist (pair alist)
      (destructuring-bind (key . value) pair
	(setf (gethash key ht) value)))
    ht))

(defun sum-ht-values (ht)
  (let ((sum 0))
    (maphash #'(lambda (key value)
		 key ; ignore
		 (incf sum value))
	     ht)
    sum))

(defun sort-alist-descending (alist)
  (sort alist #'> :key #'cdr))

(defun sort-ht-descending (ht)
  (sort-alist-descending (hash-table->alist ht)))

(set-pprint-dispatch 'hash-table
  (lambda (str ht)
    (format str "{~{~{~S => ~S~}~^ ~}}"
	    (mapcar #'(lambda (assoc-pair)
			(list (car assoc-pair) (cdr assoc-pair)))
		    (sort-ht-descending ht)))))

(defun merge-hash-tables (&rest tables)
  "http://pleac.sourceforge.net/pleac_commonlisp/hashes.html"
  (let ((union
         (make-hash-table
          :test (first
                 (sort (mapcar #'hash-table-test tables) #'>
                       :key (lambda (test)
                              (ecase test
                                (eq 0)
                                (eql 1)
                                (equal 2)
                                (equalp 3)))))
          :size (reduce #'max (mapcar #'hash-table-size tables)))))
    (dolist (table tables)
      (maphash (lambda (key val) (setf (gethash key union) val)) table))
    union))

(defun skim-ht-mass (ht mass-to-retain)
  (let ((mass-sum (sum-ht-values ht)) 
	(acc-mass 0)
	(new-hash (make-hash-table)))
    (let ((pairs (sort-ht-descending ht))) ; pairs sorted by value, descending
      (while (and pairs
		  (< (/ acc-mass mass-sum) mass-to-retain)) ; may go over mass-to-retain. not sure if this is a problem
	(destructuring-bind (key . value) (car pairs)
	  (setf (gethash key new-hash) value)
	  (incf acc-mass value))
	(setf pairs (cdr pairs))))
    new-hash))

(defun skim-ht-threshold (ht min-value)
  (let ((new-hash (make-hash-table)))
    (maphash #'(lambda (key value) (if (>= value min-value)
				       (setf (gethash key new-hash)
					     value)))
	     ht)
    new-hash))


;;;; building distributions from lists of items
(defun frequencies (word-list)
  "return a hash-table containing the frequency counts of the items in the list"
  (let ((ht (make-hash-table)))
    (dolist (word word-list ht)
      (incf (gethash word ht 0)))))

(defun normalize (ht)
  "new hash table that preserves ratios of values in ht but all values sum to 1"
  (let ((sum-values (sum-ht-values ht))
	(normalized-table (make-hash-table)))
    (maphash #'(lambda (key value) (setf (gethash key normalized-table) 
					 (/ value sum-values))) ht)
    normalized-table))

;;;; tree-utilities
;;; Trees are represented by lists. car of list is the parent, each element in the cdr is a child
(defun leaves (tree)
  "get a list of all leaves of a tree"
  (if (listp tree)
      (mapcan #'leaves (cdr tree))
      (list tree)))

(defun find-children (tree parent-node)
  "get a list of all children of the parent-node in the tree. will match multiple if the parent-node appears multiple times"
  (if (listp tree)
      (if (eq (car tree) parent-node)
	  (cons (cdr tree) (mapcan #'(lambda (sub-tree) (find-children sub-tree parent-node)) (cdr tree)))
	  (mapcan #'(lambda (sub-tree) (find-children sub-tree parent-node)) (cdr tree)))))

(defun ancestor-list (tree &optional ancestors)
  "get a list of (leaf (ancestors)) for all leaves in the tree"
  (if (atom tree)
      (list (cons tree (list ancestors)))
      (mapcan #'(lambda (s) (ancestor-list s (cons (car tree) ancestors))) (cdr tree))))

(defun has-internal-node? (tree node &optional exclude-root)
  (if exclude-root
      (some #'(lambda (s) (has-internal-node? s node)) (cdr tree))
      (if (listp tree)
	  (or (eq (car tree) node)
	      (some #'(lambda (s) (has-internal-node? s node)) (cdr tree))))))


;;;; parse-tree specific
(defun words-from-tree (parse-tree)
  (leaves parse-tree))

(defun words-from-forest (parse-trees)
  (mapcan #'words-from-tree parse-trees))

(defun get-nps (parse-tree)
  "list of sub-trees within any noun-phrase in the parse-tree"
  (find-children parse-tree 'np))

(defun get-innermost-nps (parse-tree)
  "list of sub-trees within any innermost noun-phrase in the parse-tree"
  (remove-if #'(lambda (sexps) 
		 (find-if #'(lambda (sexp) (has-internal-node? sexp 'np)) sexps)) 
	     (get-nps parse-tree)))


;;;; noun-phrase filtering
(defun words-in-subject-filter (parse-tree)
  "return a list of the words that appear to be in a subject noun-phrase of the parse-tree. 
   Useful in bootstrapping scenes to identify the object that was added -- this object is 
   generally the subject of the sentence"
  (let ((one-of  '(adjp np))
	(none-of '(quan pred pp pro art advbl)))
    (mapcar #'car (remove-if-not #'(lambda (leaf-ancestors-pair)
				     (inclusion-exclusion? (cadr leaf-ancestors-pair) ; the ancestors list
							   one-of
							   none-of))
				 (ancestor-list parse-tree)))))


;;;; naive bayes classifier with additive smoothing
(defun laplace-smoothed-probability (feature class-distribution size-of-feature-domain)
  "add-one smoothing"
  (/ (1+ (gethash feature class-distribution 0))
     (+ (sum-ht-values class-distribution) size-of-feature-domain)))

(defstruct nbc-class
  label ; can be anything
  distribution ; a hash table of frequencies
  prior ; a number, the prior probability
)

(defun nbc-class-from-feature-bag (feature-bag &key label (prior 1) (ht-skimming-fn #'identity))
  (make-nbc-class :distribution (funcall ht-skimming-fn (frequencies feature-bag))
		  :prior prior
		  :label (or label feature-bag)))


(defun nbc-score (feature-bag class-distribution class-prior size-of-feature-domain)
  "get the nbc score for a bag of features and the distribution for a given class
   feature-bag -- a list of features present in the instance we're classifying
   class-distribution -- a hash table that counts the frequencies of features in the class we're scoring on
   size-of-feature-domain -- the number of possible features, used for smoothing"
  (let* ((probs-list (mapcar #'(lambda (feature)
				 (log (laplace-smoothed-probability feature 
								    class-distribution
								    size-of-feature-domain)))
			     feature-bag))
	 (product (if probs-list (reduce #'+ probs-list) nil))
	 (likelihood (if product (exp product) 0)))
    (* class-prior likelihood)))

(defun nbc-score-feature-bag (feature-bag classes)
  (let ((size-of-feature-domain (hash-table-count
				 (apply #'merge-hash-tables
					(mapcar #'nbc-class-distribution classes)))))
    (mapcar #'(lambda (class) (nbc-score feature-bag
					 (nbc-class-distribution class)
					 (nbc-class-prior class)
					 size-of-feature-domain))
	    classes)))

(defun nbc-classify (feature-bag classes &optional score-list)
  (let* ((scores (or score-list (nbc-score-feature-bag feature-bag classes)))
	 (classes-scores (mapcar #'cons scores classes)))
    (cdr (argmax classes-scores #'car))))

;;;; in progress
(defun ground-lfs () (read-lfs "~/bolt_gt/dumps_raw/base_obj.parsed"))

(defun identify-nouns (sexp &rest classes)
  (if (listp sexp)
      (if (and (eq (car sexp) 'np)
	       (not (has-internal-node? sexp 'np T)))
	  (cons (car sexp) (list (nbc-class-label (nbc-classify (words-from-tree sexp)
							  classes))))
	  (cons (car sexp) (mapcar #'(lambda (s) (apply #'identify-nouns s classes)) (cdr sexp))))
      sexp))

;(defun scene-parse-forest (scene-lfs)
;  (mapcan #'cdr scene-lfs))

(defun scene-class-all-words (scene-lfs &rest key-args &key label prior)
  label ; ignore, passed to class constructor through key-args
  prior ; ignore, passed to class constructor through key-args
  (let ((word-bag (words-from-forest (scene-parse-forest scene-lfs))))
    (apply #'nbc-class-from-feature-bag word-bag key-args)))

(defun scenes-class-all-words (scene-lfs-list &rest key-args &key label prior)
  label ; ignore, passed to class constructor through key-args
  prior ; ignore, passed to class constructor through key-args
  (let ((word-bag (words-from-forest (mapcan #'scene-parse-forest scene-lfs-list))))
    (apply #'nbc-class-from-feature-bag word-bag key-args)))

(defun scene-class-all-noun-phrases (scene-lfs &rest key-args &key label prior)
  label ; ignore, passed to class constructor through key-args
  prior ; ignore, passed to class constructor through key-args
  (let* ((np-list (mapcan #'get-nps (scene-parse-forest scene-lfs)))
	 (word-bag (mapcan #'words-from-forest np-list)))
    (apply #'nbc-class-from-feature-bag word-bag key-args)))

(defun scenes-class-all-noun-phrases (scene-lfs-list &rest key-args &key label prior)
  label ; ignore, passed to class constructor through key-args
  prior ; ignore, passed to class constructor through key-args
  (let* ((np-list (mapcan #'get-nps (mapcan #'scene-parse-forest scene-lfs-list)))
	(word-bag (mapcan #'words-from-forest np-list)))
    (apply #'nbc-class-from-feature-bag word-bag key-args)))

(defun scene-class-innermost-noun-phrases (scene-lfs &rest key-args &key label prior)
  label ; ignore, passed to class constructor through key-args
  prior ; ignore, passed to class constructor through key-arrgs
  (let* ((np-list (mapcan #'get-innermost-nps (scene-parse-forest scene-lfs)))
	 (word-bag (mapcan #'words-from-forest np-list)))
    (apply #'nbc-class-from-feature-bag word-bag key-args)))

(defun scenes-class-innermost-noun-phrases (scene-lfs &rest key-args &key label prior)
  label ; ignore, passed to class constructor through key-args
  prior ; ignore, passed to class constructor through key-arrgs
  (let* ((np-list (mapcan #'get-innermost-nps (mapcan #'scene-parse-forest scene-lfs)))
	 (word-bag (mapcan #'words-from-forest np-list)))
    (apply #'nbc-class-from-feature-bag word-bag key-args)))

(defun scene-class-subject-noun-phrases (scene-lfs &rest key-args &key label prior)
  label ; ignore, passed to class constructor through key-args
  prior ; ignore, passed to class constructor through key-arrgs
  (let ((word-bag (mapcan #'words-in-subject-filter (scene-parse-forest scene-lfs))))
    (apply #'nbc-class-from-feature-bag word-bag key-args)))

(defun reciprocal (x)
  (/ 1 x))

(defun foo-np (np &rest scene-lfs)
  (let ((n -1))
    (apply #'identify-nouns np 
	   (mapcar #'(lambda (lfs) (scene-class-innermost-noun-phrases lfs
								       :label (incf n)
								       :prior (reciprocal (length scene-lfs))))
		   scene-lfs))))

(defun range (start end)
  (if (> start end)
      nil
      (cons start (range (1+ start) end))))

(defun object-class-features (sequence-index starting-scene-index ending-scene-index &key label prior class-fn)
  label ; ignore
  prior ;ignore
  (funcall class-fn (mapcar #'(lambda (n) (scene-lfs sequence-index n))
					       (range starting-scene-index ending-scene-index))
				       :label label
				       :prior prior))

(defun sequence-length (sequence-index)
  (ecase sequence-index
    (1 2)
    (2 3)
    (3 4)
    (4 5)
    (5 3)
    (6 3)
    (7 4)
    (8 6)
    (9 3)
    (10 5)
    (11 2)
    (12 5)
    (13 3)
    (14 8)))

(defun classes-all-objects-all-scenes (&optional (class-fn #'scene-class-innermost-noun-phrases))
  (mapcan #'(lambda (sequence-index)
	      (let ((seq-len (sequence-length sequence-index)))
		(mapcar #'(lambda (scene-index)
			    (let ((class 
			    (funcall #'object-class-features sequence-index scene-index
								 (1- seq-len)
								 :class-fn class-fn
								 :label (format nil "~A ~A" sequence-index scene-index)
								 ;:prior (/ (- seq-len scene-index) seq-len)))
								 :prior 1)))
;			      (setf (nbc-class-label class)(format nil "~A" (most-common-features-class class)))
			      class))
			(range 0 (1- seq-len)))))
	  (range 1 14)))

(defun classes-subject-isolation-all ()
  (mapcan #'(lambda (sequence-index)
	      (let ((seq-len (sequence-length sequence-index)))
		(mapcar #'(lambda (scene-index)
			    (let ((class
			    (scene-class-subject-noun-phrases (scene-lfs sequence-index scene-index)
							      :prior 1)))
			      (setf (nbc-class-label class)(format nil "~A" (most-common-features-class
								       class)))
			      class))
			(range 0 (1- seq-len)))))
	  (range 1 14)))

(defun classes-subject-isolation-sequence (sequence-index &optional end-scene)
  (let ((last-scene (or end-scene (1- (sequence-length sequence-index)))))
    (cons (scene-class-all-words (ground-lfs) :label "(GROUND)"
				  :prior 1)
    (mapcar #'(lambda (scene-index)
		(let ((class
		       (scene-class-subject-noun-phrases (scene-lfs sequence-index scene-index)
							 :prior 1)))
		  (setf (nbc-class-label class)(format nil "~A" (most-common-features-class
								 class)))
		  class))
	    (range 0 last-scene)))))

(defun partition (sequence &key (key #'identity) (equivalence-test #'eq) (transform #'identity))
  (let (partitions)
    (dolist (elem sequence)
      (let* ((key (funcall key elem))
	     (lookup (assoc key partitions :test equivalence-test)))
	(if lookup
	    (rplacd lookup (cons (funcall transform elem) (cdr lookup)))
	    (setf partitions (cons (cons key (list (funcall transform elem))) partitions)))))
    partitions))

(defstruct scene
    schematic
    parse-forest
 )


(defun read-scenes-sequence (sequence-id)
  (mapcar #'(lambda (scn-id) (read-scene sequence-id scn-id))
	  (range 0 (1- (sequence-length sequence-id)))))

(defun read-scenes-all ()
  (mapcan #'read-scenes-sequence
	  (range 1 14)))			    

(defun train-classes-by-feature (scene-list &key schematic-key-fn parse-tree-winnowing-fn (ht-skimming-fn #'identity))
  "scene-key-fn should return a list of features from a schematic"
  (let* ((scene-feature-list (mapcan #'(lambda (scene) 
					(mapcar #'(lambda (feature) (cons feature scene))
						(funcall schematic-key-fn (scene-schematic scene))))
				    scene-list))
	 (feature-partition (partition scene-feature-list :key #'car :transform #'cdr))
	 (num-classifications (reduce #'+ (mapcar #'(lambda (feature-list) (length (cdr feature-list)))
						  feature-partition))))
    (mapcar #'(lambda (feature-list)
		(nbc-class-from-feature-bag (mapcan #'(lambda (scene) (mapcan parse-tree-winnowing-fn 
									      (scene-parse-forest scene))) 
						    (cdr feature-list))
					    :label (format nil "~A" (car feature-list))
					    :prior (/ (length (cdr feature-list))
						      num-classifications)
					    :ht-skimming-fn ht-skimming-fn))
	    
	    feature-partition)))

(defun train-classes-shape-subject (scene-list &optional (mass-to-retain 3/10))
  (train-classes-by-feature scene-list 
			    :parse-tree-winnowing-fn #'words-in-subject-filter
			    :schematic-key-fn #'(lambda (scm)
						  (list (object-shape (last-object scm))))
			    :ht-skimming-fn #'(lambda (ht) (skim-ht-mass ht mass-to-retain))))

(defun train-classes-color-subject (scene-list &optional (mass-to-retain 3/10))
  (train-classes-by-feature scene-list 
			    :parse-tree-winnowing-fn #'words-in-subject-filter
			    :schematic-key-fn #'(lambda (scm)
						  (list (object-color (last-object scm))))
			    :ht-skimming-fn #'(lambda (ht) (skim-ht-mass ht mass-to-retain))))
      
(defun most-common-features-class (class &optional (n 3))
  (reverse (mapcar #'car (last (reverse (sort-ht-descending (nbc-class-distribution class))) n))))

(defun identify-nouns-in-parse-forest (parse-forest &rest classes)
  (mapcar #'(lambda (pt) (apply #'identify-nouns pt classes)) parse-forest))

(defun foo-all-objects-all-scenes (sequence-index scene-index &optional (class-fn #'scene-class-innermost-noun-phrases))
  (let ((classes (classes-all-objects-all-scenes class-fn)))
    (mapcar #'words-from-tree (apply #'identify-nouns-in-parse-forest (scene-parse-forest (scene-lfs sequence-index scene-index))
				     classes))))

(defun foo-subject-isolation (sequence-index scene-index)
  (mapcar #'words-from-tree (apply #'identify-nouns-in-parse-forest (scene-parse-forest (scene-lfs sequence-index scene-index))
				   (classes-subject-isolation-sequence sequence-index scene-index))))

(defun all-innermost-nps (&rest scene-list)
  (mapcar #'words-from-forest (mapcan #'(lambda (scene) (mapcan #'get-innermost-nps (scene-parse-forest scene))) scene-list)))

(defun standardize-scores (feature-bag-list &rest classes)
  (let* ((all-scores (mapcar #'(lambda (fb) (nbc-score-feature-bag fb classes)) feature-bag-list))
	 (flattened (apply #'append all-scores))
	 (mean (mean flattened))
	 (sd (standard-deviation flattened)))
    (mapcar #'(lambda (lst) (standardize lst mean sd)) all-scores)))

(defun foo (feature-bag &rest classes)
  (mapcar #'(lambda (a b) (list feature-bag a b))
	  (identity (nbc-score-feature-bag feature-bag classes)) (mapcar #'nbc-class-label classes)))
