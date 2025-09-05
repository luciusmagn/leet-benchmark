(load "solution.lisp")
(sb-ext:save-lisp-and-die "solution-lisp"
                          :toplevel #'anagram-steps:main
                          :executable t
                          :compression t)
