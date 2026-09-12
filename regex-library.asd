(asdf:defsystem "regex-library"
  :version "0.1.0"
  :author "Tikhon"
  :license "MIT"
  :description "Regular expression library in Common Lisp (SBCL) with guaranteed O(N) linear search complexity using Lazy DFA and Unicode equivalence classes."
  :components ((:file "package")
               (:module "src"
                :depends-on ("package")
                :components ((:module "ast"
                              :components ((:file "ast")
                                           (:file "ast-printer" :depends-on ("ast"))
                              )
                            )
                            (:file "builtin-char-classes")
                             (:module "parser"
                              :depends-on ("ast" "builtin-char-classes")
                              :components ((:file "state")
                                           (:file "range-quantifier" :depends-on ("state"))
                                           (:file "unicode-char" :depends-on ("state"))
                                           (:file "char-class"       :depends-on ("state" "unicode-char"))
                                           (:file "grammar"          :depends-on ("state" "char-class" "range-quantifier"))
                                           (:file "main"             :depends-on ("grammar"))))
                             (:module "unicode"
                              :depends-on ("ast")
                              :components ((:file "endpoints-collector")
                                           (:file "endpoints-converter" :depends-on ("endpoints-collector"))
                                           (:file "equivalence-table" :depends-on ("endpoints-converter"))
                                           (:file "char-class-to-class-ids" :depends-on ("equivalence-table"))
                                           (:file "main" :depends-on ("endpoints-collector" "endpoints-converter"
                                                                      "equivalence-table" "char-class-to-class-ids"))))
                             (:module "nfa"
                              :depends-on ("ast" "unicode")
                              :components ((:file "nfa")
                                           (:file "nfa-builder"  :depends-on ("nfa"))
                                           (:file "ast-to-nfa"  :depends-on ("nfa" "nfa-builder"))
                                           (:file "reverse-nfa" :depends-on ("nfa"))
                                           (:file "nfa-visualizer" :depends-on ("nfa"))
                                           (:file "compute-transitions" :depends-on ("nfa"))
                                           (:file "main" :depends-on ("nfa" "nfa-builder" "ast-to-nfa" "reverse-nfa" "compute-transitions"))))
                             (:module "dfa"
                              :depends-on ("nfa")
                              :components ((:file "dfa")
                                           (:file "state-registry" :depends-on ("dfa"))
                                           (:file "cache" :depends-on ("dfa"))
                                           (:file "start-states" :depends-on ("dfa" "state-registry" "cache"))
                                           (:file "step" :depends-on ("dfa" "state-registry" "cache"))
                                           (:file "main" :depends-on ("dfa" "state-registry" "start-states" "step" "cache"))))
                             (:module "engine"
                              :depends-on ("ast" "builtin-char-classes" "parser" "unicode" "nfa" "dfa")
                              :serial t
                              :components
                              ((:file "compiler")
                              (:file "runner")
                              (:file "predicates")
                              (:file "finders")
                              (:file "transform")))
                             )))
  :in-order-to ((asdf:test-op (asdf:test-op "regex-library/tests"))))

(asdf:defsystem "regex-library/tests"
  :version "0.1.0"
  :author "Tikhon"
  :license "MIT"
  :description "Test suite for regex-library"
  :depends-on ("regex-library")
  :components ((:module "tests"
                :components (
                  (:file "test-utils")
                  (:module "parser"
                   :components ((:file "state-test")
                                (:file "char-class-test"       :depends-on ("state-test"))
                                (:file "range-quantifier-test" :depends-on ("state-test"))
                                (:file "grammar-test"          :depends-on ("state-test"))
                   )
                  )
                  (:module "nfa"
                   :components ((:file "nfa-thompson-test")
                                 (:file "closure-test")
                                 (:file "reverse-nfa-test")
                   )
                  )
                  (:module "unicode"
                   :components ((:file "endpoints-collector-test")
                                (:file "endpoints-converter-test" :depends-on ("endpoints-collector-test"))
                                (:file "char-class-to-class-ids" :depends-on ("endpoints-converter-test"))
                                (:file "unicode-test"             :depends-on ("endpoints-collector-test" "endpoints-converter-test"))))
                  (:module "dfa"
                  :components ((:file "state-registry-test")
                                (:file "start-states-test")
                                (:file "cache-test")
                                (:file "dfa-test")
                  ))
                  (:module "engine"
                  :components ((:file "contains-p-test")
                               (:file "match-p-test")
                               (:file "first-match-span-test")
                               (:file "all-match-spans-test")
                               (:file "split-test")
                               (:file "replace-all-test"))))))
  :perform (asdf:test-op (op c)
             (uiop:symbol-call :regex-library/tests :#run-tests)))