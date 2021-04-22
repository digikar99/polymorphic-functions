# adhoc-polymorphic-functions

Provides `polymorphic-functions` to allow for dispatching on types instead of classes. See [examples](#examples).

>This library is still experimental. Interface can change in backward-incompatible ways.

The name does capture what it is: [Ad hoc polymorphism](https://en.wikipedia.org/wiki/Ad_hoc_polymorphism). This is not [Parametric Polymorphism](https://en.wikipedia.org/wiki/Parametric_polymorphism) since individual implementations do differ. I also do not see how the latter is implementable without the former. See [specialized-function](https://github.com/numcl/specialized-function) for parametric polymorphism.

## Rationale

`polymorphic-function` are implemented using the metaclass `closer-mop:funcallable-standard-class` and `closer-mop:set-funcallable-instance-function`.

As per [CLHS](http://www.lispworks.com/documentation/HyperSpec/Body/t_generi.htm#generic-function),

>A generic function is a function whose behavior depends on the classes or identities of the arguments supplied to it.

By contrast, adhoc-polymorphic-functions dispatch on the types of the arguments supplied to it. This helps dispatching on specialized arrays as well as user-defined types.

apf comprises of the `polymorphic-functions` (analogous to `generic-functions`) and its associated `polymorphs` (analogous to `methods`). Wherever possible, apf provides compiler notes for [inline optimizations](#inline-optimizations) as well as a certain amount of type checking.

In contrast to [sealable-metaobjects](https://github.com/marcoheisig/sealable-metaobjects) and [fast-generic-functions](https://github.com/marcoheisig/fast-generic-functions), adhoc-polymorphic-functions does not make any assumptions about the sealedness of a domain for purposes of inlining. Thus, users are expected to abide by the same precautions for inline optimizations here as they do while inlining normal functions. In particular, users are expected to recompile their code after additional polymorphs are defined, and also accordingly manage the compilation order of their files and systems.

A related project [specialization-store](https://github.com/markcox80/specialization-store) also provides support for type-based dispatch:

>A premise of specialization store is that all specializations should perform the same task. Specializations should only differ in how the task is performed. This premise resolves ambiguities that arise when using types, rather than classes, to select the most specific specialization to apply.

However, the implications of this assumption are that individual specializations in each store-object of specialization-store [do not have initializer forms for optional or keyword arguments](https://github.com/markcox80/specialization-store/wiki/Tutorial-2:-Optional,-Keyword-and-Rest-Arguments).

By contrast, like usual generic-functions, apf does allow initializer forms for optional and keywords arguments for individual polymorphs.

In addition to being dispatched on types, apf also provides the ability to install compiler-macros for individual `polymorphs`.

## Inline Optimizations

A compiler-note-providing compiler-macro has also been provided for compile-time optimization guidelines.

- A speed=3 optimization coupled with debug<3 optimization results in (attempts to) inline-optimizations.
- A debug=3 optimization leaves things as they are and avoids any sort of optimizations.
- Inline optimizations may be avoided by `(declare (notinline my-polymorph))` - however, inlining is the only way to optimize polymorphs.

It is up to the user to ensure that a polymorph that specializes (or generalizes) another polymorph should have the same behavior, under some definition of same-ness.

For instance, consider

```lisp
(define-polymorphic-function my-type (obj))
(defpolymorph my-type ((obj vector)) symbol
  (declare (ignore obj))
  'vector)
(defpolymorph my-type ((obj string)) symbol
  (declare (ignore obj))
  'string)
```

Then, the behavior of `my-type-caller` depends on optimization policies:

```lisp
(defun my-type-caller (a)
  (declare (optimize debug))
  (my-type a))
(my-type-caller "hello") ;=> STRING

;;; VS

(defun my-type-caller (a)
  (declare (optimize speed)
           (type vector a))
  (my-type a))
(my-type-caller "hello") ;=> VECTOR
```

The mistake here is polymorph with type list `(vector)` produces a different behavior as compared to polymorph with type list `(string)`. (The behavior is "same" in the sense that `"hello"` is indeed a `vector`; perspective matters?)

This problem also arises with [static-dispatch](https://github.com/alex-gutev/static-dispatch) and [inlined-generic-functions](https://github.com/guicho271828/inlined-generic-function). The way to avoid it is to either maintain discipline on the part of the user (the way adhoc-polymorphic-functions [currently] assumes) or to seal domains (the way of fast-generic-functions and sealable-metaobjects).

Inlining especially becomes necessary for mathematical operations, wherein a call to `generic-+` on SBCL can be a 3-10 times slower than the optimized calls to `fixnum +` or `single-float +` etc. `generic-cl` (since `static-dispatch` version 0.5) overcomes this on SBCL by using `sb-c:deftransform`; for portable projects, one could use `inlined-generic-functions` [superseded by `fast-generic-functions`] subject to the limitation that there are no separate classes for (array single-float) and (array double-float) at least until SBCL 2.1.1.

## Related Projects

- [static-dispatch](https://github.com/alex-gutev/static-dispatch)
- [specialization-store](https://github.com/markcox80/specialization-store)
- [fast-generic-functions](https://github.com/marcoheisig/fast-generic-functions)
- [inlined-generic-functions](https://github.com/guicho271828/inlined-generic-function)

## Comparison of generics, specializations and adhoc-polymorphic-functions

For the run-time performance, consider the below definitions

```lisp
(defpackage :perf-test
  (:use :cl :specialization-store :adhoc-polymorphic-functions))
(in-package :perf-test)

(defun function-= (a b)
  (string= a b))

(defmethod generic-= ((a string) (b string))
  (string= a b))

(defstore specialized-= (a b))
(defspecialization (specialized-= :inline t) ((a string) (b string)) t
  (string= a b))

(defstore specialized-=-key (&key a b))
(defspecialization (specialized-=-key :inline t) (&key (a string) (b string)) t
  (string= a b))

(define-polymorphic-function polymorphic-= (a b))
(defpolymorph polymorphic-= ((a string) (b string)) t
  (string= a b))

(define-polymorphic-function polymorphic-=-key (&key a b))
(defpolymorph polymorphic-=-key (&key ((a string) "") ((b string) "")) t
  (string= a b))
```

For a 5,000,000 calls to each in the format

```lisp
(let ((a "hello")
      (b "world"))
  (time (loop repeat 5000000 do (string= a b))))
```

the performance results come out on my machine as:

```
0.471 sec   | function-=
0.531 sec   | generic-=
0.615 sec   | specialized-=
0.671 sec   | polymorphic-=
0.855 sec   | polymorphic-=-key
1.647 sec   | specialized-=-key
```

Note that `adhoc-polymorphic-functions` do a fair bit of cheating, and performance might decrease rapidly (linearly) with increasing number of polymorphs. (Readers are welcome to write benchmarks :)).

All of `specialization-store` and `adhoc-polymorphic-functions` as well as `fast-generic-functions` or `static-dispatch` provide support for compile-time optimizations via type-declarations and/or inlining. If performance is a concern, one'd therefore rather want to use compile-time optimizations.

| Feature                         | cl:generic-function | specialization-store | adhoc-polymorphic-functions |
|:--------------------------------|:--------------------|:---------------------|:----------------------------|
| Method combination              | Yes                 | No                   | No                          |
| Precedence                      | Yes                 | Partial*             | Yes                         |
| &optional, &key, &rest dispatch | No                  | Yes                  | Yes^                        |
| Run-time Speed                  | Fast                | Fast                 | Depends                     |
| Compile-time support            | Partial**           | Yes                  | Yes                         |

^See [#comparison-with-specialization-store](#comparison-with-specialization-store).
Well...

\*\*Using [fast-generic-functions](https://github.com/marcoheisig/fast-generic-functions) - but this apparantly has a few limitations like requiring non-builtin-classes to have an additional metaclass. This effectively renders it impossible to use for the classes in already existing libraries.

## Examples

See [src/misc-tests.lisp](src/misc-tests.lisp) for some more examples.

```lisp
(use-package :adhoc-polymorphic-functions)
(define-polymorphic-function my= (a b))
(defpolymorph my= ((a string) (b string)) boolean
  (string= a b))
(defpolymorph my= ((a character) (b character)) boolean
  (char= a b))
(defpolymorph my= ((a (simple-array single-float))
                  (b (simple-array single-float))) symbol
  ;; possible here; not possible with cl:defmethod without some MOP-fu
  ;; do something
  'hello)
```

```lisp
CL-USER> (defun foo (a b)
           (declare (optimize speed)
                    (type string a b))
           (string= a b))

FOO
CL-USER> (disassemble 'foo)
; disassembly for FOO
; Size: 39 bytes. Origin: #x5300D1B3                          ; FOO
; B3:       31F6             XOR ESI, ESI
; B5:       48C745F017011050 MOV QWORD PTR [RBP-16], #x50100117  ; NIL
; BD:       488975E8         MOV [RBP-24], RSI
; C1:       48C745E017011050 MOV QWORD PTR [RBP-32], #x50100117  ; NIL
; C9:       B90C000000       MOV ECX, 12
; CE:       FF7508           PUSH QWORD PTR [RBP+8]
; D1:       B8E25A3550       MOV EAX, #x50355AE2              ; #<FDEFN SB-KERNEL:STRING=*>
; D6:       FFE0             JMP RAX
; D8:       CC10             INT3 16                          ; Invalid argument count trap
NIL
CL-USER> (defun bar (a b)
           (declare (optimize speed)
                    (type string a b))
           (my= a b))
BAR
CL-USER> (disassemble 'bar)
; disassembly for BAR
; Size: 39 bytes. Origin: #x5300D283                          ; BAR
; 83:       31F6             XOR ESI, ESI
; 85:       48C745F017011050 MOV QWORD PTR [RBP-16], #x50100117  ; NIL
; 8D:       488975E8         MOV [RBP-24], RSI
; 91:       48C745E017011050 MOV QWORD PTR [RBP-32], #x50100117  ; NIL
; 99:       B90C000000       MOV ECX, 12
; 9E:       FF7508           PUSH QWORD PTR [RBP+8]
; A1:       B8E25A3550       MOV EAX, #x50355AE2              ; #<FDEFN SB-KERNEL:STRING=*>
; A6:       FFE0             JMP RAX
; A8:       CC10             INT3 16                          ; Invalid argument count trap
NIL
CL-USER> (my= (make-array 1 :element-type 'single-float)
              (make-array 1 :element-type 'single-float))
HELLO
CL-USER> (defun baz (a b)
           (declare (type string a)
                    (type integer b)
                    (optimize safety))
           (my= a b))
; While compiling (MY= A B):
;
;   No applicable POLYMORPH discovered for TYPE-LIST (STRING INTEGER).
;   Available TYPE-LISTs include:
;      ((SIMPLE-ARRAY SINGLE-FLOAT) (SIMPLE-ARRAY SINGLE-FLOAT))
;      (CHARACTER CHARACTER)
;      (STRING STRING)
BAZ
CL-USER> (my= 5 "hello")
; Evaluation aborted on #<ADHOC-POLYMORPHIC-FUNCTIONS::NO-APPLICABLE-POLYMORPH {103A713D13}>.
```

## Dependencies outside quicklisp

- SBCL 2.0.9+
- [trivial-types:function-name](https://github.com/digikar99/trivial-types)
- [trivial-form-type](https://github.com/digikar99/trivial-form-type)
  - [cl-environments 0.3](https://github.com/alex-gutev/cl-environments)

### Getting it from ultralisp

[Ultralisp](https://ultralisp.org/) recently added a feature to allow [custom dists](https://github.com/ultralisp/ultralisp/pull/87). While quicklisp will take a while to update trivial-types (and cl-syntax which several other projects depend upon) to the new repositories since the originals have been archived and trivial-types is still incomplete wrt CLHS, we can use the custom dists to distribute this (and related) libraries.

To do this, add the following to your implementation init file (since you'll possibly need this to keep with the project updates):

```lisp
;;; An attempt was made to include the enumeration function natively at
;;;   https://github.com/quicklisp/quicklisp-client/pull/206
;;; but it was rejected, so we do this:
(defun ql-dist::dist-name-pathname (name)
  "Return the pathname that would be used for an installed dist with
the given NAME."
  (ql-dist::qmerge (make-pathname :directory (list* :relative "dists"
                                             (uiop:split-string name :separator "/")))))
(defun digikar99-dist-enumeration-function ()
  "The default function used for producing a list of dist objects."
  (loop for file in (directory (ql-dist::qmerge "dists/digikar99/*/distinfo.txt"))
        collect (ql-dist::make-dist-from-file file)))
(push 'digikar99-dist-enumeration-function ql::*dist-enumeration-functions*)
```

Once the function is pushed, install the dist:

```lisp
;;; See https://ultralisp.org/dists/digikar99/specialized-array-dispatch for related projects
(ql-dist:install-dist "http://dist.ultralisp.org/digikar99/specialized-array-dispatch.txt"
                      :prompt nil)
;;; If the install-dist step gives a "can't create directory" error, manually
;;; create the directory $QUICKLISP_HOME/dists/digikar99
(ql:update-dist "digikar99/specialized-array-dispatch")
(ql:quickload "adhoc-polymorphic-functions")
(asdf:test-system "adhoc-polymorphic-functions")
```

## Tests

Tests are distributed throughout the system. Run `(asdf:test-system "adhoc-polymorphic-functions")`.

## Limitations

At least one limitation stems from the limitations of the implementations (and CL?) themselves. On SBCL, this is mitigated by the use of `sb-c:deftransform`:

```lisp
CL-USER> (defun bar ()
           (declare (optimize speed))
           (my= "hello" (macrolet ((a () "hello"))
                          (a))))
; Optimization of
;   (MY= "hello"
;        (MACROLET ((A ()
;                     "hello"))
;          (A)))
; is left to SBCL because ADHOC-POLYMORPHIC-FUNCTIONS is unable to optimize it
; because
;
;  Type of
;    (MACROLET ((A ()
;                 "hello"))
;      (A))
;  could not be determined
;
BAR
CL-USER> (disassemble 'bar)               ; on SBCL
; disassembly for BAR
; Size: 13 bytes. Origin: #x5300799B                          ; BAR
; 9B:       BA4F011050       MOV EDX, #x5010014F              ; T
; A0:       488BE5           MOV RSP, RBP
; A3:       F8               CLC
; A4:       5D               POP RBP
; A5:       C3               RET
; A6:       CC10             INT3 16                          ; Invalid argument count trap
NIL
```

And while this is a simpler case that could possibly be optimizable, a nuanced discussion pertaining to the same is at [https://github.com/Bike/compiler-macro/pull/6#issuecomment-643613503](https://github.com/Bike/compiler-macro/pull/6#issuecomment-643613503).
