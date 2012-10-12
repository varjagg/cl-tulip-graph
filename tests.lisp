(in-package #:org.funcall.bouquet)

(defun test ()
  "Small test for the package"
  (new-graph)
  (register-node 'a)
  (label (node 'a) "Start")
  (register-node 'b)
  (register-node 3)
  (register-node 'c)
  (label (node 'c) "Finish")
  (register-edge (node 'a) (node 3))
  (label (register-edge (node 'c) (node 'a)) "woo hoo!")
  (register-edge (node 'b) (node 'c))
  (make-cluster "sniff" (list (node 'a) (node 'c)) (list (edge 'c 'a)))
  (make-cluster "puff")
  (add-to-cluster (cluster "puff") (node 'a))
  (add-to-cluster (cluster "puff") (node 3))
  (render-graph))

#+cmu
(defun render-subclasses (class)
  "In CMUCL, expects a PCL class as the argument"
  (label (register-node-unless-exists class) (pcl:class-name class))
  (loop for subclass in (pcl:class-direct-subclasses class)
	unless (node subclass) do
	(render-subclasses subclass)
	(register-edge (node subclass) (node class))))
