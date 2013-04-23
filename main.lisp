;;;Copyright (C) 2004,2012 by Eugene Zaikonnikov <eugene@funcall.org>
;;;This file is distributed under the terms of LLGPL license (see LICENSE for details)

;;; Produces Tulip (.tlp format) graph descriptions

(in-package #:org.funcall.cl-tulip-graph)

(eval-when (:load-toplevel :execute)
(defvar *current-graph*)

(defvar *graph-nodes*)

(defvar *graph-edges*)

(defvar *graph-clusters*)

(defvar *graph-properties*)

(defvar *object-count* 0)

(pushnew :cl-tulip-graph *features*)
)

(defgeneric register-node (node)
  (:documentation "Registers a node and returns its handle object"))

(defgeneric register-edge (handle1 handle2)
  (:documentation "Registers a directed edge from handle1 to handle2, and returns its handle"))

(defgeneric node (node)
  (:documentation "Looks up a node handle in nodes registry"))

(defgeneric set-property (property node value)
  (:documentation "Sets the specified property of node to the value"))
  
(defgeneric add-to-cluster (cluster object)
  (:documentation "Adds object to cluster; returns the object, to allow composition"))

(defclass graph ()
  ((nodes :accessor nodes
	  :initarg :nodes
	  :initform (make-hash-table))
   (edges :accessor edges
	  :initarg :edges
	  :documentation "Lookup by node pairs"
	  :initform (make-hash-table :test #'equal))
   (clusters :reader clusters
	     :initarg :clusters
	     :documentation "Lookup by strings"
	     :initform (make-hash-table :test #'equal))
   (properties :reader properties
	       :initarg :properties
	       :documentation "Lookup by strings"
	       :initform (make-hash-table :test #'equal))
   (date :accessor date
	 :documentation "Date of the generated document"
	 :initform (multiple-value-bind 
			 (sec min hour day month year)
		       (get-decoded-time)
		     (format nil "~2,'0d-~d-~d" day month year))))
  (:documentation "Contains the data sufficient to render a graph"))

(defun current-graph ()
  "Returns the current graph instance"
  *current-graph*)

(defun set-current-graph (graph)
  "Set the current graph to a new instance and return it"
  (setf *current-graph* graph
	*graph-nodes* (nodes graph)
	*graph-edges* (edges graph)
	*graph-clusters* (clusters graph)
	*graph-properties* (properties graph))
  *current-graph*)

(defun new-graph ()
  "Set the current graph to a new instance and return it; also, attach labels property"
  (prog1
      (set-current-graph (make-instance 'graph))
    (make-property "viewLabel" nil 'string "" "")))

(defclass tulip-object ()
  ((numeric-id :initform (incf *object-count*)
	       :reader numeric-id)))

(defclass tulip-node (tulip-object)
  ((edges :initarg :edges
	  :accessor edges
	  :initform '())))

(defclass tulip-edge (tulip-object)
  ((from :initarg :from
	 :accessor from)
   (to :initarg :to
       :accessor to)))

(defclass tulip-cluster (tulip-object)
  ((name :initarg :name
	 :accessor name)
   (subclusters :initarg :subclusters
		:initform '()
		:accessor subclusters)
   (nodes :initarg :nodes
	  :initform '()
	  :accessor nodes)
   (edges :initarg :edges
	  :initform '()
	  :accessor edges)))

(defclass tulip-property ()
  ((property-type :initarg :type
		  :accessor property-type)
   (cluster :initarg :cluster
	    :accessor cluster
	    :initform nil)
   (name :initarg :name
	 :accessor name)
   (nodes-default :initform ""
		  :initarg :nodes-default
		  :accessor nodes-default)
   (edges-default :initform ""
		  :initarg :edges-default
		  :accessor edges-default)
   (node-elements :initform (make-hash-table)
		  :accessor node-elements)
   (edge-elements :initform (make-hash-table)
		  :accessor edge-elements)))

(defmethod register-node (node)
  (when (node node)
    (error "Graph node ~A is already defined" node))
  (setf (gethash node *graph-nodes*)
	(make-instance 'tulip-node)))

(defmethod register-edge ((from tulip-node) (to tulip-node))
  (when (edge from to)
    (error "Graph edge from ~A to ~A is already defined" from to))
  (let ((edge (make-instance 'tulip-edge :from from :to to)))
    (setf (gethash (cons from to) *graph-edges*) edge)
    (push edge (edges from))
    (push edge (edges to))
    edge))

(defmethod node (node)
  (gethash node *graph-nodes*))

(defmethod edge (from to)
  (gethash (cons (node from) (node to)) *graph-edges*))

(defmethod make-cluster ((name string) &optional nodes edges &key subclusters)
  "Creates a cluster from the given nodes, edges and optional subcluster objects"
  (setf (gethash name (clusters (current-graph))) (make-instance 'tulip-cluster :name name :nodes nodes :edges edges :subclusters subclusters)))

(defmethod add-to-cluster ((cluster tulip-cluster) (node tulip-node))
  (push node (nodes cluster))
  node)

(defmethod add-to-cluster ((cluster tulip-cluster) (edge tulip-edge))
  (push edge (edges cluster))
  edge)

(defmethod cluster ((name string))
  (gethash name *graph-clusters*))

(defmethod print-object ((edge tulip-edge) stream)
  (format stream "(edge ~D ~D ~D)" (numeric-id edge) (numeric-id (from edge)) (numeric-id (to edge))))

(defmethod print-object ((cluster tulip-cluster) stream)
  (format stream
	  "(cluster ~D ~S~%(nodes~{ ~D~})~%(edges~{ ~D~})~{~%~S~})"
	  (numeric-id cluster)
	  (name cluster)
	  (mapcar #'(lambda (item) (numeric-id item)) (nodes cluster))
	  (mapcar #'(lambda (item) (numeric-id item)) (edges cluster))
	  (subclusters cluster)))

(defmethod make-property ((name string) cluster type &optional (nodes-default "") (edges-default ""))
  "Creates a property"
  (setf (gethash name (properties (current-graph)))
	(make-instance 'tulip-property :name name :nodes-default nodes-default :edges-default edges-default :type type :cluster cluster)))

(defmethod set-property ((property tulip-property) (node tulip-node) value)
  (setf (gethash node (node-elements property)) value))

(defmethod set-property ((property tulip-property) (edge tulip-edge) value)
  (setf (gethash edge (edge-elements property)) value))

(defmethod property ((name string))
  (gethash name *graph-properties*))

(defmethod reset ((property tulip-property))
  (setf (edge-elements property) (make-hash-table)
	(node-elements property) (make-hash-table)))

(defmethod print-object ((property tulip-property) stream)
  (with-slots (cluster name node-elements edge-elements) property
      (format stream
	  "(property ~D ~(~A~) ~S~%(default ~S ~S)~%~{(node ~D ~S)~%~}~{(edge ~D ~S)~%~})"
	  (if cluster (numeric-id cluster) 0)
	  (property-type property)
	  name
	  (nodes-default property)
	  (edges-default property)
	  (let ((nodelist '()))
	    (maphash #'(lambda (key val)
			 (push val nodelist)
			 (push (numeric-id key) nodelist))
		     node-elements)
	    nodelist)
	  (let ((edgelist '()))
	    (maphash #'(lambda (key val)
			 (push val edgelist)
			 (push (numeric-id key) edgelist))
		     edge-elements)
	    edgelist))))

(defun render-graph (&optional (stream *standard-output*))
  "Prints whole graph description to a stream, or to stdout if none specified"
  (let ((all-nodes '())
	(all-edges '())
	(all-clusters '())
	(all-properties '()))
    (maphash #'(lambda (key val)
		 (declare (ignore key))
		 (push (numeric-id val) all-nodes))
	     *graph-nodes*)
    (maphash #'(lambda (key val)
		 (declare (ignore key))
		 (push val all-edges))
	     *graph-edges*)
    (maphash #'(lambda (key val)
		 (declare (ignore key))
		 (push val all-clusters))
	     *graph-clusters*)
    (maphash #'(lambda (key val)
		 (declare (ignore key))
		 (push val all-properties))
	     *graph-properties*)
    (format stream "(tlp \"2.0\"~%")
    (format stream "(date \"~A\")~%" (date *current-graph*))
    (format stream "(nodes~{ ~D~})~%" all-nodes)
    (format stream "~{~S~%~}" all-edges)
    (format stream "~{~S~%~}" all-clusters)
    (format stream "~{~S~%~}" all-properties)
    (format stream ")~%")))


(defmethod label (object value)
  (set-property (property "viewLabel") object value))

(defmethod label (object (value symbol))
  (call-next-method object (symbol-name value)))

(defun register-node-unless-exists (node)
  (cond ((node node))
	(t (register-node node))))
