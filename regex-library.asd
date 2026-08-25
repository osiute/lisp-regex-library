(asdf:defsystem "regex-library"
  :version "0.1.0"
  :author "Tikhon"
  :license "MIT"
  :description "Regular expression library in Common Lisp (SBCL) with guaranteed O(N) linear search complexity using Lazy DFA and Unicode equivalence classes."
  :components ((:file "packages")
               (:module "src"
                :depends-on ("packages")
                :components ((:file "ast")
                             (:module "parser"
                              :depends-on ("ast")
                              :components ((:file "state")
                                           (:file "char-class" :depends-on ("state"))
                                           (:file "grammar"    :depends-on ("state" "char-class"))
                                           (:file "main"       :depends-on ("grammar"))))
                             (:file "unicode" :depends-on ("ast"))
                             (:file "nfa"     :depends-on ("ast" "unicode"))
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
                :components ((:file "test"))))
  :perform (asdf:test-op (op c)
             (uiop:symbol-call :regex-library/tests :#run-tests)))