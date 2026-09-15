(asdf:defsystem "regex-library"
  :version "0.1.0"
  :author "Tikhon"
  :license "MIT"
  :description "Regular expression library in Common Lisp (SBCL) with guaranteed O(N) linear search complexity using Lazy DFA and Unicode equivalence classes."
  :depends-on ("cl-ppcre")
  :serial t
  :components ((:file "package")
               (:module "src"
                :serial t
                :components ((:module "ast"
                              :serial t
                              :components ((:file "ast")
                                           (:file "ast-printer")))
                             (:file "builtin-char-classes")
                             (:module "parser"
                              :serial t
                              :components ((:file "state")
                                           (:file "range-quantifier")
                                           (:file "unicode-char")
                                           (:file "char-class")
                                           (:file "grammar")
                                           (:file "main")))
                             (:module "unicode"
                              :serial t
                              :components ((:file "endpoints-collector")
                                           (:file "endpoints-converter")
                                           (:file "equivalence-table")
                                           (:file "char-class-to-class-ids")
                                           (:file "main")))
                             (:module "nfa"
                              :serial t
                              :components ((:file "nfa")
                                           (:file "nfa-builder")
                                           (:file "ast-to-nfa")
                                           (:file "reverse-nfa")
                                           (:file "nfa-visualizer")
                                           (:file "compute-transitions")
                                           (:file "main")))
                             (:module "dfa"
                              :serial t
                              :components ((:file "dfa")
                                           (:file "state-registry")
                                           (:file "cache")
                                           (:file "start-states")
                                           (:file "step")
                                           (:file "main")))
                             (:module "engine"
                              :serial t
                              :components ((:file "compiler")
                                           (:file "runner")
                                           (:file "api-utils")
                                           (:file "predicates")
                                           (:file "finders")
                                           (:file "transform"))))))
  :in-order-to ((asdf:test-op (asdf:test-op "regex-library/tests"))))

(asdf:defsystem "regex-library/tests"
  :version "0.1.0"
  :author "Tikhon"
  :license "MIT"
  :description "Test suite for regex-library"
  :depends-on ("regex-library")
  :serial t
  :components ((:module "tests"
                :serial t
                :components ((:file "test-utils")
                             (:module "parser"
                              :serial t
                              :components ((:file "state-test")
                                           (:file "char-class-test")
                                           (:file "range-quantifier-test")
                                           (:file "grammar-test")))
                             (:module "nfa"
                              :serial t
                              :components ((:file "nfa-thompson-test")
                                           (:file "closure-test")
                                           (:file "reverse-nfa-test")))
                             (:module "unicode"
                              :serial t
                              :components ((:file "endpoints-collector-test")
                                           (:file "endpoints-converter-test")
                                           (:file "char-class-to-class-ids")
                                           (:file "unicode-test")))
                             (:module "dfa"
                              :serial t
                              :components ((:file "state-registry-test")
                                           (:file "start-states-test")
                                           (:file "cache-test")
                                           (:file "dfa-test")))
                             (:module "engine"
                              :serial t
                              :components ((:file "contains-p-test")
                                           (:file "matches-p-test")
                                           (:file "first-match-span-test")
                                           (:file "all-match-spans-test")
                                           (:file "split-test")
                                           (:file "count-disjoint-matches-test")
                                           (:file "ppcre-comparison-tests")
                                           (:file "replace-all-test"))))))
  :perform (asdf:test-op (op c)
             (uiop:symbol-call :regex-library "RUN-ALL-TESTS")))