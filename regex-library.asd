(asdf:defsystem "regex-library"
  :version "0.1.0"
  :author "Tikhon"
  :license "MIT"
  :description "Regular expression library in Common Lisp (SBCL) with guaranteed O(N) linear search complexity using Lazy DFA and Unicode equivalence classes."
  :components ((:file "packages")
               (:module "src"
                :depends-on ("packages")
                :components ((:module "ast"
                              :components ((:file "ast")
                                           (:file "ast-printer" :depends-on ("ast"))
                              )
                            )
                             (:module "parser"
                              :depends-on ("ast")
                              :components ((:file "state")
                                           (:file "range-quantifier" :depends-on ("state"))
                                           (:file "char-class"       :depends-on ("state"))
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
                                           (:file "closure" :depends-on ("nfa"))
                                           (:file "main" :depends-on ("nfa" "nfa-builder" "ast-to-nfa" "reverse-nfa" "closure"))))
                             (:file "dfa"     :depends-on ("nfa" "unicode"))
                             (:file "engine"  :depends-on ("parser" "unicode" "nfa" "dfa")))))
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
                                (:file "unicode-test"             :depends-on ("endpoints-collector-test" "endpoints-converter-test"))
                   )
                  )
                )
              )
              )
  :perform (asdf:test-op (op c)
             (uiop:symbol-call :regex-library/tests :#run-tests)))