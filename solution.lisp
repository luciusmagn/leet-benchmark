(ql:quickload :serapeum)
(defpackage #:anagram-steps
  (:use #:cl)
  (:import-from :serapeum #:->)
  (:export #:min-steps
           #:run-benchmark
           #:main))

(in-package #:anagram-steps)

(declaim (optimize (speed 3) (safety 0) (debug 0) (compilation-speed 0)))

(-> min-steps (simple-string simple-string) fixnum)
(defun min-steps (source target)
  "Calculate minimum steps to make TARGET an anagram of SOURCE.
   Counts characters needed to be replaced in TARGET."
  (declare (type simple-string source target))
  (let ((frequency-table (make-hash-table :test #'eql))
        (steps           0))
    (declare (type hash-table frequency-table)
             (type fixnum     steps))

    (loop for character across source do
      (incf (gethash character frequency-table 0)))

    (loop for character across target do
      (let ((current-count (gethash character frequency-table 0)))
        (if (plusp current-count)
            (decf (gethash character frequency-table))
            (incf steps))))

    steps))

;;;; benchmarking

(defparameter *test-cases*
  '(("bab" "aba")
    ("leetcode" "practice")
    ("anagram" "mangaar")))

(defun generate-large-test-case ()
  (list (make-string 50000 :initial-element #\a)
        (make-string 50000 :initial-element #\b)))

(defun run-benchmark ()
  (let ((test-cases (append *test-cases*
                            (list (generate-large-test-case)))))

    ;; warmup
    (dotimes (iteration 100)
      (dolist (test-case test-cases)
        (min-steps (first test-case) (second test-case))))

    ;; benchmark
    (let ((start-time (get-internal-real-time)))
      (dotimes (iteration 10000)
        (dolist (test-case test-cases)
          (min-steps (first test-case) (second test-case))))
      (let* ((end-time      (get-internal-real-time))
             (elapsed-ticks (- end-time start-time))
             (elapsed-ms    (* 1000.0 (/ elapsed-ticks internal-time-units-per-second))))
        (format t "Common Lisp internal benchmark: ~,2Fms~%" elapsed-ms)))))

;;;; main
(defun main ()
  (run-benchmark)
  (sb-ext:exit :code 0))
