;;;Copyright (C) 2004,2012 by Eugene Zaikonnikov <eugene@funcall.org>
;;;This file is distributed under the terms of LLGPL license (see LICENSE for details)

;;; Produces Tulip (.tlp format) graph descriptions

(defpackage #:org.funcall.cl-tulip-graph
  (:use "COMMON-LISP")
  (:export node edge property register-node register-edge make-cluster
	   make-property set-property label reset-graph render-graph
	   cluster add-to-cluster register-node-unless-exists
	   render-subclasses current-graph set-current-graph new-graph)
  (:nicknames #:bouquet #:cl-tulip-graph))

