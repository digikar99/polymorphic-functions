(in-package :polymorphic-functions)

(5am:in-suite :polymorphic-functions)

(defmacro time-it (&body body)
  (with-gensyms (start end body-result)
    `(let (,start ,end ,body-result)
       (setq ,start (get-internal-real-time))
       (setq ,body-result (progn ,@body))
       (setq ,end (get-internal-real-time))
       (values (- ,end ,start) ,body-result))))

(defmacro time-it/normalize (&body body)
  (with-gensyms (base-time real-time body-result)
    `(let ((,base-time (locally (declare (optimize (debug 1) (speed 1) (safety 1)))
                         (time-it (loop repeat 1000000000 do (progn t))))))
       (multiple-value-bind (,real-time ,body-result)
           (time-it ,@body)
         (values (/ ,real-time ,base-time 1.0)
                 ,body-result)))))

#+(and (or :sbcl :ccl)
       (not :travis))
(def-test performance ()
  (unwind-protect (progn
                    (ignoring-error-output
                      (eval `(locally (declare (optimize (debug 1) (speed 1)))
                               (define-polymorphic-function my= (a b) :overwrite t)
                               (defpolymorph my= ((a string) (b string)) t
                                 (string= a b)))))

                    (eval

                     `(let ((a "hello")
                            (b "world"))

                        (macrolet
                            ((expect-time ((expected) &body body)
                               (with-gensyms (expected-sym actual-sym percent-diff)
                                 `(let* ((,expected-sym ,expected)
                                         (,actual-sym
                                           (time-it/normalize ,@body))
                                         (,percent-diff (/ (abs (- ,actual-sym ,expected-sym))
                                                           ,expected-sym 0.01)))

                                    (5am:is (< ,percent-diff 10)
                                            "Expected: ~D~%Actual:   ~D~%%diff:    ~D%"
                                            ,expected-sym ,actual-sym ,percent-diff)))))

                          #-extensible-compound-types ; too slow
                          (expect-time (#+sbcl 5.1
                                        #+ccl  2.47)
                                       (locally (declare (optimize (debug 1) (speed 1)))
                                         (loop :repeat 50000000 :do (my= a b))))

                          (expect-time (#+sbcl 0.05
                                        #+ccl  0.75)
                                       (locally (declare (optimize (debug 1) (speed 3))
                                                         (type string a b))
                                         (loop :repeat 50000000 :do (my= a b))))))))

    (undefine-polymorphic-function 'my=)
    (unintern 'my=)))
