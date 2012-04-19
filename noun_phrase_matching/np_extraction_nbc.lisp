;;;; file utilities
(defun read-lfs (filename)
  "filename should be a file that contains a list of responses. Each response is also a list - the car is the original string, and the remaining elements are the parse trees returned by TRIPS for that string"
  (with-open-file (file filename)
    (read file nil)))

(defun scene-lfs (sequence-id scene-id &optional (base-path "~/bolt_gt/dumps_raw/") (filename "responses.parsed"))
  "load the logical forms from a given sequence and scene"
  (read-lfs (format nil "~A~A/~A/~A" base-path sequence-id scene-id filename)))


(defun export-nps (sequence-id scene-id &optional (base-path "~/bolt_gt/dumps_raw/") (filename "noun-phrases"))
  (let ((scene-lfs (scene-lfs sequence-id scene-id)))
    (with-open-file (stream (format nil "~A~A/~A/~A" base-path sequence-id scene-id filename) :direction :output :if-exists :supersede)
      (dolist (response-list scene-lfs)
	(let* ((responses (cdr response-list))
	       (nps (mapcan #'get-nps responses))
	       (innermost-nps (remove-if #'(lambda (sexps) (find-if #'(lambda (sexp) (has-internal-node? sexp 'np)) sexps)) nps))
	       (word-list (mapcar #'(lambda (sexps) (mapcan #'words sexps)) innermost-nps)))
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
  (lambda (a b) (if (funcall test
                             (funcall key a)
                             (funcall key b))
                    a
                    b)))

(defun extremum (sequence predicate
		  &key (key #'identity) (start 0) end)
  "Returns the element of SEQUENCE that would appear first if the
sequence were ordered according to SORT using PREDICATE and KEY using
an unstable sorting algorithm. See http://www.cliki.net/EXTREMUM for
the full specification."
    (reduce (comparator predicate key) sequence
	        :start start :end end))

(defun argmax (lst key)
  (extremum lst #'> :key key))

(defun inclusion-exclusion? (lst one-of none-of)
  "return true iff the lst contain all the atoms in one-of and none of the atoms in none-of"
  (and (> (length (intersection one-of lst)) 0)
       (= (length (intersection none-of lst)) 0)))

;;;; hash-table utilities
(set-pprint-dispatch 'hash-table
 (lambda (str ht)
  (format str "{~{~{~S => ~S~}~^, ~}}"
   (loop for key being the hash-keys of ht
         for value being the hash-values of ht
         collect (list key value)))))

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
    (let ((pairs (sort-ht-descending ht))) ; pairs is sorted by value, descending
      (while (and pairs
		  (< (/ acc-mass mass-sum) mass-to-retain)) ; may go over mass-to-retain. not sure if this is a problem
	(destructuring-bind (key . value) (car pairs)
	  (setf (gethash key new-hash) value))
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
	  (cons (cdr tree) (mapcan #'(lambda (sub-tree) (find-children sub-tree parent-node)) (cdr tree)))p
	  (mapcan #'(lambda (sub-tree) (find-children sub-tree parent-node)) (cdr tree)))))

(defun ancestor-list (tree &optional ancestors)
  "get a list of (leaf (ancestors)) for all leaves in the tree"
  (if (atom tree)
      (list (cons tree (list ancestors)))
      (mapcan #'(lambda (s) (ancestor-list s (cons (car tree) ancestors))) (cdr tree))))

;;;; parse-tree specific
(defun words (parse-tree)
  (leaves parse-tree))

(defun get-nps (parse-tree)
  "list of sub-trees within any noun-phrase in the parse-tree"
  (find-children parse-tree 'np))

;;;; noun-phrase filtering
(defun words-in-subject-filter (parse-tree)
  "return a list of the words that appear to be in a subject noun-phrase of the parse-tree. 
   Useful in bootstrapping scenes to identify the object that was added -- this object is 
   generally the subject of the sentence"
  (let ((one-of  '(adjp np))
	(none-of '(quan pred pp pro art advbl)))
    (mapcar #'car (remove-if-not #'(lambda (leaf-ancestors-pair)
				     (inclusion-exclusion? (cdr leaf-ancestors-pair) ; the ancestors list
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

(defun nbc-classify (feature-bag classes)
  (let ((size-of-feature-domain (hash-table-count 
				 (apply #'merge-hash-tables 
					(mapcar #'nbc-class-distribution classes)))))

    (argmax #'(lambda (class) (nbc-score feature-bag 
					 (nbc-class-distribution class) 
					 (nbc-class-prior class) 
					 size-of-feature-domain))
	    classes)))


	    

  
  




  
    