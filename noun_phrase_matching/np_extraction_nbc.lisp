(ql:quickload "cl-json")
(ql:quickload "cl-plplot")

;;;; logical form utilities
(defun read-lfs (filename &key (sampling-rate 1) seed)
  "filename should be a file that contains a list of responses. Each response is also a list - the car is the original string, and the remaining elements are the parse trees returned by TRIPS for that string"
  (with-open-file (file filename)
    (sample (read file nil) sampling-rate :seed seed)))


(defparameter *base-path* "~/bolt_gt/dumps_raw/")

(defparameter *responses-path* "~/Dropbox/bolt/blockworld/annotations/")

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

(defun responses-path (sequence scene)
  (format nil "~A~A/~A/responses.json" *responses-path* sequence scene))

(defun read-responses (sequence scene)
  (mapcar #'(lambda (json)  (response-from-json json 
						:sequence sequence
						:scene scene))
	  (json:decode-json (open (responses-path sequence scene)))))

(defstruct response-word
#|  (:print-function 
   (lambda (struct stream depth)
     (declare (ignore depth))
     (format stream "~:[~a~;~a (~a)~]" 
	     (response-word-object-binding struct)
	     (response-word-text struct)
	     (response-word-object-binding struct))))
|#
  text
  object-binding
  sequence
  object)

(defstruct (response
	     (:print-function 
	      (lambda (struct stream depth)
		(declare (ignore depth))
		(format stream "~A" (response-word-list struct)))))
  sequence
  scene
  word-list
  id
  string)

(defun word-from-json (json &key sequence)
  (let ((object-index (lookup json :object--binding)))
    (make-response-word :text (lookup json :word)
			:sequence sequence
			:object-binding object-index
			:object (when object-index
				  (object-schematic sequence object-index)))))


(defvar *objects* (make-hash-table :test #'equal))

(defun clear-objects ()
  (setf *objects* (make-hash-table :test #'equal)))
  
(defun object-schematic (sequence scene)
  (last-object (scene-schematic (or (gethash (list sequence scene)
					     *objects*)
				    (setf (gethash (list sequence scene)
						   *objects*)
					  (read-scene sequence scene))))))

(defun response-from-json (json &key sequence scene)
  (make-response :word-list (mapcar #'(lambda (json)
					(word-from-json json 
							:sequence sequence))
				    (lookup json :word--list))
		 :id (lookup json :id)
		 :string (lookup json :string)
		 :sequence sequence
		 :scene scene))

;;;; scene functions
(defstruct scene
  sequence-id
  scene-id
  schematic
  parse-forest
  )

(defun read-scenes-sequence (sequence-id &key num-to-read (sampling-rate 1) seed)
  (mapcar #'(lambda (scn-id) (read-scene sequence-id scn-id :sampling-rate sampling-rate :seed seed))
	  (range 0 (1- (or num-to-read (sequence-length sequence-id))))))

(defun read-scenes-all (&key (sampling-rate 1) seed)
  (mappend (lambda (sequence-num) (read-scenes-sequence sequence-num :sampling-rate sampling-rate :seed seed))
	   (range 1 14)))

(defun scene-lfs (sequence-id scene-id &key (base-path *base-path*) (filename "responses.parsed") (sampling-rate 1) seed)
  "load the logical forms from a given sequence and scene"
  (read-lfs (format nil "~A~A/~A/~A" base-path sequence-id scene-id filename) 
	    :sampling-rate sampling-rate :seed seed))

(defun read-schematic (sequence-id scene-id &key (base-path *base-path*) (filename "schematic.lisp"))
  (with-open-file (file (format nil "~A~A/~A/~A" base-path sequence-id scene-id filename))
    (read file nil)))

(defun read-scene (sequence-id scene-id &key (sampling-rate 1) (base-path *base-path*) seed)
  (make-scene
   :sequence-id sequence-id
   :scene-id scene-id
   :schematic (read-schematic sequence-id scene-id :base-path base-path)
   :parse-forest (mappend #'cdr (scene-lfs sequence-id scene-id 
					   :base-path base-path 
					   :sampling-rate sampling-rate
					   :seed seed))))

(defun read-ground-scene (&optional path)
  (setf path (or path (format nil "~A~A" *base-path* "base_obj.parsed")))
  (make-scene
   :parse-forest (mappend #'cdr (read-lfs path))))

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

;;;; general utilities
(defun compose (&rest functions)
  "Compose FUNCTIONS right-associatively, returning a function"
  #'(lambda (x)
      (reduce #'funcall functions
              :initial-value x
              :from-end t)))

(defun avg (lst &key val-fn weight-fn)
  (when lst
    (let ((vals (if val-fn (mapcar val-fn lst) lst))
	  (weights (if weight-fn 
		       (mapcar weight-fn lst)
		       (mapcar (lambda (x) (declare (ignore x)) 1) lst))))
      (/ (reduce #'+ vals)
	 (reduce #'+ weights)))))

(defun lookup (alist key)
  (cdr (assoc key alist)))

(defun take-while (pred list)
  (loop for x in list
        while (funcall pred x)
        collect x))

(defun take (n seq)
  (loop for x in seq
       while (>= (decf n) 0)
       collect x))
	 

(defstruct partition
  key
  values
)

(defmacro while (test &rest body)
  `(do ()
       ((not ,test))
     ,@body))

(defun partition-sequence (f seq &key (equality #'equal))
  "applies f to each item in seq, splitting it each time f returns a new value"
  (when seq
    (let* ((fv (funcall f (car seq)))
	   (ptr seq)
	   acc)
      (while (and ptr (funcall equality fv (funcall f (car ptr))))
	(push (pop ptr) acc))
      (cons (make-partition :key fv
			    :values acc)
	    (partition-sequence f ptr)))))

(defun partition-set (set &key key (equivalence-test #'eq) transform)
  "return a list of partitions of the set.
key should return a unique value for each partition. equivalence-test is used
to compare key values. transform will be applied to each element in the partitions"
  (let ((partitions (make-hash-table :test equivalence-test)))
    (dolist (elem set)
      (push (if transform 
		(funcall transform elem)
		elem)
	    (gethash (if key (funcall key elem) elem)
		     partitions)))
    (maphashl (lambda (k v)
		(make-partition
		 :key k
		 :values v)) partitions)))

(defun range (start end &optional (step 1))
  "list of ints from start to end, inclusive"
  (if (> start end)
      nil
      (cons start (range (+ start step) end step))))

(defun mappend (fn &rest lsts)
  "non-destructive version of mapcan"
  (apply #'append (apply #'mapcar fn lsts)))

(defun argmax (lst key)
  "max item in the list according to the key function"
  (let (max-item max-val)
    (dolist (x lst)
      (if (or (not max-val)
	      (> (funcall key x) max-val))
	  (setf max-item x
		max-val (funcall key x))))
    (values max-item max-val)))

(defun zip (&rest lists)
  "like python's zip"
  (apply #'mapcar #'list lists))

(defun inclusion-exclusion? (lst one-of none-of)
  "return true iff the lst contain all the atoms in one-of and none of the atoms in none-of"
  (and (> (length (intersection one-of lst)) 0)
       (= (length (intersection none-of lst)) 0)))

;;;; statistics
(defstruct distribution-parameters
  n ; count
  mean ; mean
  sd ; standard deviation
  )

(defun mean (vals)
  (/ (reduce #'+ vals)
     (length vals)))

(defun standard-deviation (vals &optional mean)
  "if mean is passed, will use it in the calculation"
  (let ((m (or mean (mean vals))))
    (sqrt (/ (reduce #'+ (mapcar #'(lambda (v) (expt (- v m) 2))
				 vals))
	     (length vals)))))

(defun standardize (vals &optional mean standard-deviation)
  "if mean and standard-deviation are passed, will use them in the calculation"
  (let ((m (or mean 
	       (mean vals)))
	(sd (or standard-deviation 
		(standard-deviation vals))))
    (if (= sd 0) ; all values equal the mean
	(mapcar #'(lambda (v) 
		    v ; ignore
		    0)
		vals)
	(mapcar #'(lambda (v)
		    (/ (- v m)
		       sd))
		vals))))

;;;; hash-table utilities
(defun maphashl (fn ht)
  "functional version of maphash that returns a list"
  (let (acc)
    (maphash (lambda (k v)
	       (push (funcall fn k v) acc))
	     ht)
    (reverse acc)))

(defun hash-table->alist (ht)
  "turn a hash table in to an association list"
  (maphashl #'cons ht))

(defun alist->hash-table (alist)
  "turn an alist into a hash table"
  (let ((ht (make-hash-table)))
    (dolist (pair alist)
      (destructuring-bind (key . value) pair
	(setf (gethash key ht) value)))
    ht))

(defun sum-ht-values (ht)
  "sum the values in a hash-table"
  (reduce #'+ (maphashl
	       (lambda (k v) 
		 (declare (ignore k)) 
		 v) 
	       ht)))

(defun sort-alist-descending (alist)
  "sort an alist by its values, descending"
  (sort alist #'(lambda (&rest vals) 
		  (when (every #'numberp vals)
		    (apply #'> vals)))
	:key #'cdr))

(defun sort-ht-descending (ht)
  "sort a hash-table by its values, descending (returns an alist)"
  (sort-alist-descending (hash-table->alist ht)))

; to let us see what's in a hash-table we print
(set-pprint-dispatch 'hash-table
		     (lambda (str ht)
		       (format str "{~{~{~S => ~S~}~^ ~}}"
			       (mapcar #'(lambda (assoc-pair)
					   (list (car assoc-pair) (cdr assoc-pair)))
				       (sort-ht-descending ht)))))


(defun merge-hash-tables (tables)
  "return a hash-table that has the union of the keys in its arguments. Don't expect any useful behavior for the values
  http://pleac.sourceforge.net/pleac_commonlisp/hashes.html"
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
  "only keep keys with the largest values in ht, up to the fraction mass-to-retain of the original value sum"
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
  "only keep keys with values above min-value in ht"
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
      (mappend #'leaves (cdr tree))
      (list tree)))

(defun find-children (tree parent-node)
  "get a list of all children of the parent-node in the tree. will match multiple if the parent-node appears multiple times"
  (if (listp tree)
      (if (eq (car tree) parent-node)
	  (cons (cdr tree) (mappend #'(lambda (sub-tree) (find-children sub-tree parent-node)) (cdr tree)))
	  (mappend #'(lambda (sub-tree) (find-children sub-tree parent-node)) (cdr tree)))))

(defun ancestor-list (tree &optional ancestors)
  "get a list of (leaf (ancestors)) for all leaves in the tree"
  (if (atom tree)
      (list (cons tree (list ancestors)))
      (mappend #'(lambda (s) (ancestor-list s (cons (car tree) ancestors))) (cdr tree))))

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
  (mappend #'words-from-tree parse-trees))

(defun get-nps (parse-tree)
  "list of sub-trees within any noun-phrase in the parse-tree"
  (find-children parse-tree 'np))

					;(defparameter *words-to-remove* '(THE THERE A))
(defparameter *words-to-remove* nil)

(defun get-innermost-nps (parse-tree)
  "list of sub-trees within any innermost noun-phrase in the parse-tree"
  (remove-if #'(lambda (sexps) 
		 (find-if #'(lambda (sexp) (has-internal-node? sexp 'np)) sexps)) 
	     (get-nps parse-tree)))

(defun all-innermost-nps (scene-list)
  (remove-if #'null (mapcar #'(lambda (forest) (remove-if 
						#'(lambda (word) (member word *words-to-remove*))
						(words-from-forest forest)))
			    (mappend #'(lambda (scene) (mappend #'get-innermost-nps (scene-parse-forest scene))) scene-list))))

;;;; noun-phrase filtering
(defun words-in-subject-filter (parse-tree)
  "return a list of the words that appear to be in a subject noun-phrase of the parse-tree. 
   Useful in bootstrapping scenes to identify the object that was added -- this object is 
   generally the subject of the sentence"
  (let ((one-of  '(np adjp))
	(none-of '(pp advbl)))
    (remove-if #'(lambda (word) (member word *words-to-remove*)) 
	       (mapcar #'car (remove-if-not #'(lambda (leaf-ancestors-pair)
						(inclusion-exclusion? (cadr leaf-ancestors-pair) ; the ancestors list
								      one-of
								      none-of))
					    (ancestor-list parse-tree))))))

(defun words-in-innermost-np (parse-tree)
  (mappend #'words-from-forest (get-innermost-nps parse-tree)))

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

(defun nbc-class-from-feature-bag (feature-bag &key label (prior 1) ht-skimming-fn)
  (make-nbc-class :distribution (if ht-skimming-fn
				    (funcall ht-skimming-fn (frequencies feature-bag))
				    (frequencies feature-bag))
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
				 (funcall #'merge-hash-tables
					  (mapcar #'nbc-class-distribution classes)))))
    (mapcar #'(lambda (class) (nbc-score feature-bag
					 (nbc-class-distribution class)
					 (nbc-class-prior class)
					 size-of-feature-domain))
	    classes)))

(defun nbc-classify (feature-bag classes &optional score-list)
  (let* ((scores (or score-list (nbc-score-feature-bag feature-bag classes)))
	 (classes-scores (mapcar #'cons scores classes))
	 (max-class-score (argmax classes-scores #'car)))
    (values (cdr max-class-score)
	    (car max-class-score))))

(defun nbc-classify-return-all (feature-bag classes &optional score-list)
  (let* ((scores (or score-list (nbc-score-feature-bag feature-bag classes)))
	 (classes-scores (mapcar #'cons scores (mapcar #'(lambda (class) (nbc-class-label class)) classes))))
    (sort classes-scores #'> :key #'car)))

#|
(defun identify-nouns (sexp &rest classes)
  (if (listp sexp)
      (if (and (eq (car sexp) 'np)
	       (not (has-internal-node? sexp 'np T)))
	  (cons (car sexp) (list (nbc-class-label (nbc-classify (words-from-tree sexp)
								classes))))
	  (cons (car sexp) (mapcar #'(lambda (s) (apply #'identify-nouns s classes)) (cdr sexp))))
      sexp))
|#



;;;; training functions: take a scene list. for each scene, 
;;;; extract words from all response parse trees according 
;;;; to a parse tree winnowing function, then generate a 
;;;; class for each scene using these words as a bag of 
;;;; features. Labels and creates a prior for each class 
;;;; too, this behavior varies from function to function

(defparameter *ht-min-threshold* 1)

(defun train-classes-subject-isolation (scene-list &key ground-scene 
					(ht-skimming-fn #'(lambda (ht) (skim-ht-threshold ht *ht-min-threshold*))))
  (let ((scene-classes 
	 (mapcar #'(lambda (scene count)
		     (nbc-class-from-feature-bag (mappend #'words-in-subject-filter (scene-parse-forest scene))
						 :prior (/ 1 (if ground-scene 
								 (1+ (length scene-list))
								 (length scene-list)))
						 :label count
						 :ht-skimming-fn ht-skimming-fn))
		 scene-list (range 0 (1- (length scene-list))))))
    (if ground-scene
	(cons (nbc-class-from-feature-bag (mappend #'words-in-subject-filter (scene-parse-forest ground-scene))
					  :prior (/ 1 (1+ (length scene-list)))
					  :label 'ground)
	      scene-classes)
	scene-classes)))

#|
(defun train-classes-by-feature (scene-list &key 
				 schematic-key-fn 
				 parse-tree-winnowing-fn 
				 (ht-skimming-fn #'(lambda (ht) 
						     (skim-ht-threshold ht *ht-min-threshold*))))
  "scene-key-fn should return a list of features from a schematic"
  (let* ((scene-feature-list (mappend #'(lambda (scene) 
					  (mapcar #'(lambda (feature) (cons feature scene))
						  (funcall schematic-key-fn (scene-schematic scene))))
				      scene-list))
	 (feature-partition (mapcar #'partition-values
				    (partition-set scene-feature-list 
						   :key #'car 
						   :transform #'cdr)))
	 (num-classifications (reduce #'+ (mapcar #'(lambda (feature-list) (length (cdr feature-list)))
						  feature-partition))))
    (mapcar #'(lambda (feature-list)
		(nbc-class-from-feature-bag (mappend #'(lambda (scene) (mappend parse-tree-winnowing-fn 
										(scene-parse-forest scene))) 
						     (cdr feature-list))
					    :label (format nil "~A" (car feature-list))
					    :prior (/ (length (cdr feature-list))
						      num-classifications)
					    :ht-skimming-fn ht-skimming-fn))
	    
	    feature-partition)))
|#

(defun ensure-list (o)
  (if (listp o) 
      o
      (list o)))

(defun winnow-scene (scene parse-tree-winnowing-fn)
  "takes a scene and a function that maps a "
  (remove-if #'null (mapcar parse-tree-winnowing-fn
			    (scene-parse-forest scene))))

(defun train-classes-by-feature (scene-list &key
				  feature-fn
				  winnowing-fn
				  ht-skimming-fn)
  (labels ((count-feature-instances (partition)
	     (reduce #'+ (mapcar #'length (partition-values partition))))
	   (features-cross-scenes (scene)
	     (mapcar (lambda (feature)
		       (cons feature scene))
		     (ensure-list (funcall feature-fn scene)))))
    (let* ((scene-feature-list (mappend #'features-cross-scenes scene-list))
	   (partitions (partition-set scene-feature-list
				      :key (lambda (feature-scene-pair)
					     (car feature-scene-pair))
				      :transform (lambda (feature-scene-pair)
						   (winnow-scene (cdr feature-scene-pair)
								 winnowing-fn))))
	   (total-instance-count (reduce #'+ (mapcar #'count-feature-instances partitions))))
      (mapcar (lambda (partition)
					; partition key : the feature 
					; partition value : lists of feature bags from parse-tree-winnowing-fn, 
					; one for each scene
		(nbc-class-from-feature-bag
		 (mapcan (lambda (feature-list) 
			   (apply #'append feature-list)) 
			 (partition-values partition))
		 :label (partition-key partition)
		 :prior (/ (count-feature-instances partition)
			   total-instance-count)
		 :ht-skimming-fn ht-skimming-fn))
	      partitions))))


(defun train-classes-shape-subject (scene-list)
  (train-classes-by-feature scene-list 
			    :winnowing-fn #'words-in-subject-filter
			    :feature-fn #'(lambda (scene)
						  (object-shape (last-object (scene-schematic scene))))))

(defun train-classes-color-all (scene-list)
  (train-classes-by-feature scene-list 
			    :winnowing-fn #'words-in-innermost-np
			    :feature-fn #'(lambda (scene)
						  (mapcar #'object-color (objects (scene-schematic scene))))))

(defun train-classes-color-subject (scene-list)
  (train-classes-by-feature scene-list 
			    :winnowing-fn #'words-in-subject-filter
			    :feature-fn #'(lambda (scene)
					    (object-color (last-object (scene-schematic scene))))))

(defun train-classes-shape-all (scene-list)
  (train-classes-by-feature scene-list 
			    :winnowing-fn #'words-in-innermost-np
			    :feature-fn #'(lambda (scene)
						  (mapcar #'object-shape (objects (scene-schematic scene))))))

(defun most-common-features-class (class &optional (n 3))
  (reverse (mapcar #'car (last (reverse (sort-ht-descending (nbc-class-distribution class))) n))))

#|
(defun identify-nouns-in-parse-forest (parse-forest &rest classes)
  (mapcar #'(lambda (pt) (apply #'identify-nouns pt classes)) parse-forest))
|#

(defun classify-threshold-with-parameters (feature-bag distribution-parameters classes)
  (let* ((sd (distribution-parameters-sd distribution-parameters))
	 (mean (distribution-parameters-mean distribution-parameters))
	 (raw-scores (nbc-score-feature-bag feature-bag classes))
	 (std-scores (standardize raw-scores mean sd)))
    (if (find-if #'(lambda (n) (>= n 0)) std-scores)
	(nbc-classify feature-bag classes std-scores)
	(apply #'values nil (multiple-value-list (nbc-classify feature-bag classes std-scores))))))

(defun classify-threshold (feature-bag model-distribution-parameters classes)
  (classify-threshold-with-parameters feature-bag (cdr (assoc (length feature-bag)
							      model-distribution-parameters))
				      classes))

(defun classify-threshold-with-parameters-return-all (feature-bag distribution-parameters classes)
  (let* ((sd (distribution-parameters-sd distribution-parameters))
	 (mean (distribution-parameters-mean distribution-parameters))
	 (raw-scores (nbc-score-feature-bag feature-bag classes))
	 (std-scores (standardize raw-scores mean sd)))
    (nbc-classify-return-all feature-bag classes std-scores)))

(defun classify-threshold-return-all (feature-bag model-distribution-parameters classes)
  (classify-threshold-with-parameters-return-all feature-bag (cdr (assoc (length feature-bag)
									 model-distribution-parameters))
						 classes))

;;;; calculate model distributions
(defun model-distribution-parameters-by-length (feature-bag-list classes)
  (let ((length-partition 
	 (partition-set feature-bag-list :key #'length)))
    (mapcar #'(lambda (partition)
		(let* ((length (partition-key partition))
		       (feature-bag-list-sub (partition-values partition))
		       (all-scores (mapcar #'(lambda (fb) 
					       (nbc-score-feature-bag fb classes))
					   feature-bag-list-sub))
		       (n (length all-scores))
		       (flattened (apply #'append all-scores)))
		  (cons length
			(make-distribution-parameters
			 :mean (mean flattened)
			 :sd (standard-deviation flattened)
			 :n n))))
	    length-partition)))

(defun model-distribution-parameters (training-fn &key (feature-extraction-fn #'all-innermost-nps) 
				      (classification-scenes (read-scenes-all)) 
				      (training-scenes (read-scenes-all)))
  (model-distribution-parameters-by-length 
   (funcall feature-extraction-fn classification-scenes)
   (funcall training-fn training-scenes)))

(defun shape-distribution-subject ()
  (model-distribution-parameters #'train-classes-shape-subject))

(defun color-distribution-subject ()
  (model-distribution-parameters #'train-classes-color-subject))

(defun color-distribution-all ()
  (model-distribution-parameters #'train-classes-color-all))

(defun shape-distribution-all ()
  (model-distribution-parameters #'train-classes-shape-all))

(defun object-distribution (training-scenes classification-scenes)
  (model-distribution-parameters #'train-classes-subject-isolation
				 :training-scenes training-scenes 
				 :classification-scenes classification-scenes))

(defun train-and-classify (training-fn &key 
			   (training-scenes (read-scenes-all))
			   (classification-scenes (read-scenes-all))
			   (feature-extraction-fn #'all-innermost-nps))
  (let* ((classification-features (funcall feature-extraction-fn classification-scenes))
	 (classes (funcall training-fn training-scenes))
	 (dist-params (model-distribution-parameters training-fn
						     :feature-extraction-fn
						     feature-extraction-fn
						     :classification-scenes 
						     classification-scenes
						     :training-scenes
						     training-scenes)))
    (zip classification-features
	 (mapcar #'(lambda (fb) (multiple-value-list
				 (funcall #'classify-threshold fb dist-params classes)))
		 classification-features))))


(defun train-and-classify-return-all (training-fn &key 
				      (training-scenes (read-scenes-all))
				      (classification-scenes (read-scenes-all))
				      (feature-extraction-fn #'all-innermost-nps))
  (let* ((classification-features (funcall feature-extraction-fn classification-scenes))
	 (classes (funcall training-fn training-scenes))
	 (dist-params (model-distribution-parameters training-fn
						     :feature-extraction-fn
						     feature-extraction-fn
						     :classification-scenes 
						     classification-scenes
						     :training-scenes
						     training-scenes)))
    (zip classification-features
	 (mapcar #'(lambda (fb)
		     (funcall #'classify-threshold-return-all fb dist-params classes))
		 classification-features))))


(defun display-t-and-c-list (list)
  (dolist (elem list)
    (print (format nil "nounp: ~A" (car elem)))
    (if (null (caadr elem))
	(progn 
	  (pprint "no class match above mean")
	  (pprint (format nil "class: ~A" (nbc-class-label (second (cadr elem)))))
	  (pprint (format nil "std posterior: ~A" (car (last (cadr elem))))))
	(progn
	  (pprint (format nil "class: ~A" (nbc-class-label (caadr elem))))
	  (pprint (format nil "std posterior: ~A" (car (last (cadr elem)))))))
    (terpri)))


;;;; evaluation

(defstruct object-reference
  binding ; object referred to 
  schematic
  sequence
  words ; a list of symbols, not strings
)

(defstruct classifier-stats
  id
  avg-vocabulary-size
  (recognized-instances-count 0)
  (total-instances-count 0)
)
    
(defun group-response-words-by-object (resp)
  (remove-if-not #'object-reference-binding
		 (mapcar (lambda (partition) 
			   (make-object-reference
			    :binding (partition-key partition)
			    :sequence (response-word-sequence (first (partition-values partition)))
			    :schematic (response-word-object 
					       (first (partition-values partition)))
			    :words (mapcar (lambda (response-word)
					     (intern (string-upcase 
						      (response-word-text response-word))))
					   (partition-values partition))))
			 (partition-sequence #'response-word-object-binding
					     (response-word-list resp)))))

(defun proof-sequence (sequence &key override-training-scenes override-classes success-test (discount-most-recent t) (verbose-level 0) (sampling-rate 1) seed)
					; sequence - the index of the sequence to proof
					; success-test - a function that takes a classifier struct and object-reference struct, returns true iff they match. If omitted, success is judged on classifier label matching object-reference-binding
					; discount-most-recent - if non-nil, will not classify objects that were just added in a scene
					; verbose-level - 0-2
					; sampling-rate - rate at which to sample responses from scenes
					; seed - allows deterministic sampling
  (let* ((num-scenes (sequence-length sequence))
	 (training-fn (lambda (scn-list) (train-classes-subject-isolation scn-list :ground-scene (read-ground-scene))))
	 (sequence-stats (make-hash-table :test #'equal))
	 (sequence-match-count 0)
	 (sequence-total-count 0))
					; Go through all scenes, running accuracy figures on each
    (dolist (scene (range 0 (1- num-scenes)))
      (let* ((training-scenes (or override-training-scenes 
				  (read-scenes-sequence sequence 
							:num-to-read (1+ scene)
							:sampling-rate sampling-rate
							:seed seed)))
	     (classes (or override-classes
			  (funcall training-fn training-scenes)))
	     (goldstandard (mapcan #'group-response-words-by-object 
				   (read-responses sequence scene)))
	     (scene-match-count 0)
	     (scene-total-count 0)
	     (class (car (last classes)))
	     (object-label (nbc-class-label class))
	     (stats (make-classifier-stats
		     :id (list sequence object-label)
		     :avg-vocabulary-size (hash-table-count (nbc-class-distribution class)))))
					; store in sequence-stats just by label
	(unless (and discount-most-recent (= scene (1- num-scenes)))
	  (setf (gethash scene sequence-stats) stats))
					; for each scene, go through the gold standard responses, classifying each labeled np
	(dolist (object-reference goldstandard)
					; exclude any objects that were just added in this scene, if discount-most-recent is T
	  (unless (and discount-most-recent (= (object-reference-binding object-reference)
					       scene))
	    (let ((classification (nbc-classify (object-reference-words object-reference)
						classes)))
	      (if (and (numberp verbose-level) (> verbose-level 1))
		  (format t "~A (~A) -> ~A~%"
			  (object-reference-words object-reference)
			  (object-reference-binding object-reference)
			  (if classification (nbc-class-label classification))))
	      (if (and classification 
		       (if success-test
			   (funcall success-test classification object-reference)
			   (eq (nbc-class-label classification)
			       (object-reference-binding object-reference))))
		  (progn
					; increase match for this scene
		    (incf scene-match-count)
					; increase match for this object
		    (incf (classifier-stats-recognized-instances-count 
			   (gethash (object-reference-binding object-reference) sequence-stats))))))
					; increase total for this scene
	    (incf scene-total-count)
					; increase total for this object
	    (incf (classifier-stats-total-instances-count  
		   (gethash (object-reference-binding object-reference) sequence-stats)))))
	(if (and (numberp verbose-level) (> verbose-level 0))
	    (format t "~A ~A: identified ~A of ~A (~$)~%" sequence scene scene-match-count scene-total-count
		    (if (= scene-total-count 0) "--" (/ scene-match-count scene-total-count))))
	(incf sequence-match-count scene-match-count)
	(incf sequence-total-count scene-total-count)))
    (make-classifier-stats :id sequence
			   :avg-vocabulary-size (funcall #'avg (maphashl (lambda (k v)
									   (declare (ignore k))
									   (classifier-stats-avg-vocabulary-size v))
									 sequence-stats))
			   :recognized-instances-count sequence-match-count
			   :total-instances-count sequence-total-count)))
  
(defun proof-all (&key (discount-most-recent t) (verbose-level 0) (sampling-rate 1) seed)
  (let ((matched 0)
	(total 0)
	avg-vocabs)
    (dolist (sequence (range 1 14))
      (let ((classifier-stats (proof-sequence sequence 
					      :discount-most-recent discount-most-recent
					      :verbose-level verbose-level
					      :sampling-rate sampling-rate
					      :seed seed)))
	(incf matched (classifier-stats-recognized-instances-count classifier-stats))
	(incf total (classifier-stats-total-instances-count classifier-stats))
	(push (classifier-stats-avg-vocabulary-size classifier-stats) avg-vocabs)))
    (format t "~A of ~A (~$) overall" matched total (/ matched total))
    (make-classifier-stats :id 'all
			   :avg-vocabulary-size (avg avg-vocabs)
			   :recognized-instances-count matched
			   :total-instances-count total)))

(defun read-responses-all ()
    (mapcan (lambda (sequence)
	      (mapcan (lambda (scene)
			(read-responses sequence scene))
		      (range 0 (1- (sequence-length sequence)))))
     
     (range 1 14)))

(defun proof-all-feature (feature-fn
			  &key (discount-most-recent t) (verbose-level 0) (sampling-rate 1) seed)
  (let* ((scenes (read-scenes-all :sampling-rate sampling-rate
				  :seed seed))
	 (classes (train-classes-by-feature scenes 
					    :feature-fn 
					    (compose feature-fn #'last-object #'scene-schematic)
					    :winnowing-fn
					    #'words-in-subject-filter))
	 (goldstandard (mapcan #'group-response-words-by-object 
			       (read-responses-all)))
	 (feature-groups (partition-set goldstandard :key (compose feature-fn #'object-reference-schematic))))
    (dolist (feature-group feature-groups)
      (format t "proofing ~A~%" (partition-key feature-group))
      (let ((num-matches 0)
	    (num-instances 0))
	(dolist (object-reference (partition-values feature-group))
	  (let ((classification (nbc-classify (object-reference-words object-reference)
					      classes)))
	    (unless (and discount-most-recent
			 (= (object-reference-binding object-reference)
			    (1- (sequence-length (object-reference-sequence object-reference)))))
	      (if (> verbose-level 1)
		  (format t "~A [~A] -> ~A~%"
			  (object-reference-words object-reference)
			  (partition-key feature-group)
			  (nbc-class-label classification)))
	      (if (equal (nbc-class-label classification)
			 (partition-key feature-group))
		  
		  (incf num-matches))
	      (incf num-instances))))
	(if (> verbose-level 0)
	    (format t "matched ~A of ~A (~$)" 
		    num-matches
		    num-instances
		    (/ num-matches num-instances)))
	))))
  
(defun learning-curves (&key (discount-most-recent t) (seed 1) (resolution 20))
  (let* ((sampling-rates (range 0 1 (/ 1 resolution))) 
	 (proof-results (mapcar (lambda (p)
				  (proof-all :discount-most-recent discount-most-recent
					     :sampling-rate p
					     :seed seed))
				sampling-rates)))
    (series-graph (make-series
		   :x (mapcar #'classifier-stats-avg-vocabulary-size proof-results)
		   :y (mapcar (lambda (class)
				(/ (classifier-stats-recognized-instances-count class)
				   (if (> (classifier-stats-total-instances-count class) 0)
				       (classifier-stats-total-instances-count class)
				       1)))
			      proof-results)))))

(defun sample (lst sampling-rate &key seed)
  (let ((rs (if seed 
		(seed-random-state seed)
		*random-state*)))
  (remove-if (lambda (e)
	       (declare (ignore e))
	       (> (random 1.0 rs) sampling-rate))
	     lst)))
	      
;;;; for graphing with cl-plplot

(defstruct series
  label
  x
  y)

(defun series-graph (&rest series)
  (let ((w (cl-plplot:basic-window)))
    (dolist (s series)
      (let* ((xa (map 'vector #'identity (series-x s)))
	     (ya (map 'vector #'identity (series-y s)))
	     (ba (cl-plplot:new-x-y-plot xa ya)))
	(cl-plplot:add-plot-to-window w ba)))
    (cl-plplot:render w "tk")))