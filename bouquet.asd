(defpackage #:bouquet-system (:use #:asdf #:cl))
(in-package #:bouquet-system)

(asdf:defsystem :bouquet
    :version "0.3.0"
    :author "Eugene Zaikonnikov"
    :licence "LLGPL"
    :properties ((#:author-email . "viking@funcall.org")
		 ((#:albert #:output-dir) . "albert-docs/")
		 ((#:albert #:presentation #:funcallable #:calledby) t)
		 ((#:albert #:presentation #:class #:related-methods) t)
		 ((#:albert #:docbook #:cvs-viewurl) . "http://cvs.sourceforge.net/cgi-bin/viewcvs.cgi/bouquet/bouquet/")
		 ((#:albert #:docbook #:cvs-tag) . "HEAD"))
    :components ((:file "package")
		 (:file "bouquet" :depends-on ("package"))
		 (:file "tests" :depends-on ("bouquet"))))


