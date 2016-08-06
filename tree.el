
(defmacro valid-node! (list &rest body)
  `(if (not (null ,list))
       (if (tr:is-node ,list)
	   ,@body
	 (display-warning 'ignore  "value is not type of 'node struct in VALID-NODE! macro." :warning))
     (display-warning 'ignore  "list is empty in VALID-NODE!." :warning)))

(defmacro not-nil! (value &rest body)
  `(if (not (null ,value))
       ,@body
     (display-warning 'ignore "Insering nil value." :warning)))

(defun tr:is-node (list)
  (if (car list)
      (if (typep (car list) 'node)
	  (tr:is-node (cdr list))
	nil)
    't))



;tree walk in org mo
;; root

(defvar *tr:root* nil)

;; current

(defvar *tr:current* nil)

(defun tr:add-root (node)
  "adding root"
  (setf *tr:root* (tr:create-new node nil nil nil nil nil)))

(defstruct node                      ;; node structure
  self
  leaf
  parent
  first-child
  last-child
  previous-sibling
  next-sibling)

(defun tr:create-new (child parent first-child last-child previous-sibling next-sibling)
  "create new node"
  (let ((node (make-node :leaf child
			 :parent parent
			 :first-child first-child
			 :last-child last-child
			 :previous-sibling previous-sibling
			 :next-sibling next-sibling)))
    (setf (node-self node) node)
    (setf *tr:current* node)
    node))



;; Getter methods

(defun tr:first-child (parent)
  (valid-node! (list parent)
       (node-first-child parent)))

(defun tr:last-child  (parent)
  (valid-node! (list parent)
       (node-last-child parent)))

(defun  tr:next-sibling (node)
  (valid-node! (list node)
       (node-next-sibling node)))

(defun  tr:previous-sibling (node)
  (valid-node! (list node)
       (node-previous-sibling node)))

(defun tr:parent (node)
  (valid-node! (list node)
       (node-parent node)))

(defun tr:leaf (node)
  (valid-node! (list node)
       (node-leaf node)))

;; setter methods 

(defun tr:set-next-sibling (previous-sibling node)
  (valid-node! (list previous-sibling node)
       (setf (node-next-sibling previous-sibling) node)))

(defun tr:set-previous-sibling (next-sibling node)
  (valid-node! (list next-sibling node)
       (setf (node-previous-sibling next-sibling) node)))

(defun tr:set-last-child (parent node)
  (valid-node! (list parent node)
       (setf (node-last-child parent) node)))

(defun tr:set-first-child (parent node)
  (valid-node! (list parent node)
       (setf (node-first-child parent) node)))

;; insertion

(defun tr:insert (child parent)
  "insert as last child"
  (valid-node! (list parent)
	       (not-nil! child
			 (let* ((last-child (tr:last-child parent))
				(new-node (tr:create-new child parent nil nil last-child nil)))
			   (tr:set-last-child parent new-node)
			   (if last-child
			       (tr:set-next-sibling last-child new-node)
			     (tr:set-first-child parent new-node))))))

(defun tr:insert-after (node previous-sibling)
  "insert after certain node (add right sibling)"
  (valid-node! (list previous-sibling)
	       (not-nil! node
			 (let* ((next-sibling (tr:next-sibling previous-sibling))
				(parent (tr:parent previous-sibling))
				(new-node (tr:create-new node parent nil nil previous-sibling next-sibling)))
			   (tr:set-next-sibling previous-sibling new-node)   ;; set new-node as right sibling of left side node
			   (if next-sibling
			       (tr:set-previous-sibling next-sibling new-node) ;; set new-node as left sibling of right node
			     (tr:set-last-child parent new-node))))))    ;; new-node as last child

(defun tr:insert-before (node next-sibling)
  "insert before certain node (add left sibling to node)"
  (valid-node! (list next-sibling)
	       (not-nil! node
			 (let* ((previous-sibling (tr:previous-sibling next-sibling))
				(parent (tr:parent next-sibling))
				(new-node (tr:create-new node parent nil nil previous-sibling next-sibling)))
			   (tr:set-previous-sibling next-sibling new-node)   ;; set new-node as left sibling of right side node
			   (if previous-sibling
			       (tr:set-next-sibling previous-sibling new-node) ;; set new-node as right sibling of left side node
			     (tr:set-first-child parent new-node))))))    ;; new-node as first child

;; traverse

(defun tr:traverse (node spaces)
  "traveling through tree (preorder traversal method)"
  (cond ((not node)
	 nil)
	(t (let ((child (tr:first-child node)))
	     (tr:print-node node child spaces)          ;; print node
	     (when child                               ;; if tree ,, start over ,recursively 
	       (tr:traverse child (+ spaces 2))))
	   (tr:traverse (tr:next-sibling node) spaces))))  ;; to next sibling

;; print

(defun tr:print-node (node child spaces)
  (let ((symbol (if child                       ;; if node has child
		    "+"
		  "-")))
    (message (concat "%"  (number-to-string spaces) "s %s")  symbol (tr:leaf node))))

