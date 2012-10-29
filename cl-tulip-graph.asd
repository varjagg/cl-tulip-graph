(defpackage #:cl-tulip-graph-system (:use #:asdf #:cl))
(in-package #:cl-tulip-graph-system)

(asdf:defsystem :cl-tulip-graph
    :version "0.4.2"
    :author "Eugene Zaikonnikov"
    :licence "LLGPL"
    :description "A graph generator that produces files readable by Tulip graph visualizer"
    :properties ((#:author-email . "eugene@funcall.org"))
    :components ((:file "package")
		 (:file "main" :depends-on ("package"))
		 (:file "tests" :depends-on ("main"))))


