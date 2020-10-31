# adhoc-polymorphic-functions

Provides {macro, structures and hash-table}-based wrappers around normal functions to allow for dispatching on types instead of classes. See [examples](#examples).

>This library is still experimental. There have been two renamings so far. Please use package-local-nicknames, and be okay to use global search replace.
> However, I have no intention of renaming it further.

The name does capture what it is: [Ad hoc polymorphism](https://en.wikipedia.org/wiki/Ad_hoc_polymorphism). This is not [Parametric Polymorphism](https://en.wikipedia.org/wiki/Parametric_polymorphism) since individual implementations do differ. I also do not see how the latter is implementable without the former.

## Why?

- ANSI standard provided generic functions work do not work on anything more than classes and structure classes, for example, say `(array double-float)`. However, Common Lisp does provide a rich type system.
- As of 2020, there exists [fast-generic-functions](https://github.com/marcoheisig/fast-generic-functions) that allows generic-functions to be, well, fast.
- [static-dispatch](https://github.com/alex-gutev/static-dispatch) for allowing static-functions to be generic functions to be dispatched at compile-time; this, in turn, is used by [generic-cl](https://github.com/alex-gutev/generic-cl).
- With MOP, it might be possible to enable `cl:defmethod` on parametric types. No one I know of has tried this yet. I'm unfamiliar with MOP, and felt it might just be quicker to put this together. 
- There also exists [specialization-store](https://github.com/markcox80/specialization-store) that provides support for types. `specialization-store` has its own MOP and runs into about 3.5k LOC without tests. Besides the seeming code complexity, there are some aspects of `specialization-store` I didn't find very convenient. See [the section below](#comparison-with-specialization-store).
- `fast-generic-functions` runs into about 900 LOC without tests.
- `adhoc-polymorphic-functions` takes about 1.6k LOC with tests and, to me, it seems that is also fulfils the goal of `fast-generic-functions`.
- No complaints about `static-dispatch` other than it not being suitable for types

## Comparison with specialization-store

A more broader concern is the premise of specialization-store: no matter what specialization is invoked, the final-result should be the same. What may differ is the *process* in which the result is computed.

This [manifests itself](https://github.com/markcox80/specialization-store/issues/8) in differing approaches about how the `&optional` and `&key` dispatching plays out. For instance, consider

```lisp
(defstore foo (a &key b))
(defspecialization foo ((a string) &key (b string)) t
  'hello)
```

This does not allow for a default-value for `b` in the specialization, without redefining the `(defstore foo ...)` form to include a newer `init-form` for `b`. (Though, one could very well, write a wrapper macro for this. Of the 1.6k LoC in `adhoc-polymorphic-functions`, 950 LoC are for processing lambda lists(!))

```lisp
(define-polymorphic-function foo (a &key b))
(defpolymorph foo ((a string) &key ((b string) "hello")) t
  'hello)
```

If you do not require extensibility on anything other than `required` arguments, you should be happy with `specialization-store`. It also provides great-enough run-time performance comparable to the standard `cl:generic-function`s. (See the next section.)

Further, at the moment, `adhoc-polymorphic-functions` provides no support for dispatching on `&rest` arguments. Raise an issue (and discuss the semantics!) if this support is needed!

`adhoc-polymorphic-functions` should provide quite a few compiler-notes to aid the user in debugging and optimizing; it should be possible to provide this for `specialization-store` using a wrapper macro. See [this discussion](https://github.com/markcox80/specialization-store/issues/6#issuecomment-692958498) for a start.

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
  (time (loop repeat 1000000 do (string= a b))))
```

the performance results come out as:

```
0.494 sec   | function-=
0.577 sec   | generic-=
0.616 sec   | specialized-=
1.620 sec   | specialized-=-key
0.761 sec   | polymorphic-=
0.918 sec   | polymorphic-=-key
```

All of `specialization-store` and `adhoc-polymorphic-functions` as well as `fast-generic-functions` or `static-dispatch` provide support for compile-time optimizations via type-declarations and/or inlining. If performance is a concern, one'd therefore rather want to use compile-time optimizations.

| Feature                         | cl:generic-function | specialization-store | adhoc-polymorphic-functions |
|:--------------------------------|:--------------------|:---------------------|:---------------|
| Method combination              | Yes                 | No                   | No             |
| Precedence                      | Yes                 | Partial*             | No             |
| &optional, &key, &rest dispatch | No                  | Yes                  | Yes^           |
| Run-time Speed                  | Fast                | Fast                 | Slow           |
| Compile-time support            | Partial**           | Yes                  | Yes            |

\*`specialization-store` allows dispatching on the most specialized specialization; `adhoc-polymorphic-functions` provides no such support. In fact, as of this commit, this library wouldn't allow you to define polymorphs whose type lists are a subtype of each other. While specialization could have been provided, `adhoc-polymorphic-functions` relies on SBCL deftransforms on SBCL for better compile-time support, and this does not (seem to) provide such specialization support. (See [Limitations](#limitations).)

^See [#comparison-with-specialization-store](#comparison-with-specialization-store).
Well...

\*\*Using [fast-generic-functions](https://github.com/marcoheisig/fast-generic-functions) - but this apparantly has a few limitations like requiring non-builtin-classes to have an additional metaclass. This effectively renders it impossible to use for the classes in already existing libraries.

## An Ideal Way Forward

- The designer should know MOP to extend `cl:generic-function` to provide dispatching based on typed
- The resulting work should not have the above limitation of `fast-generic-functions`
- Should provide compile-time optimizations, as well as compiler-notes to help user optimize / debug their code

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
                    (type integer b))
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
- [compiler-macro](https://github.com/Bike/compiler-macro)

## Other Usage Notes

- For the sake of runtime performance, `adhoc-polymorphic-function`'s compiler macros do some things that may make debugging harder. To avoid such ugly things, use `(debug 3)`.
- `adhoc-polymorphic-functions` intends to check for intersecting type-lists to keep the type-lists of the polymorphs disjoint. However, this support is currently limited to the case wherein one is a proper subtype or supertype of the other.

### Limitations

At least one limitation stems from the limitations of the implementations (and CL?) themselves. On SBCL, this is mitigated by the use of `sb-c:deftransform`:

```lisp
CL-USER> (defun bar ()
           (declare (optimize speed))
           (my= "hello" (macrolet ((a () "hello"))
                          (a)))) ; on non-SBCL systems
; Unable to optimize
;  (MY= "hello" (MACROLET ((A NIL "hello")) (A)))
; because
;
;  Type of
;    (MACROLET ((A NIL "hello")) (A))
;  could not be determined
BAR
CL-USER> (defun bar () 
           (declare (optimize speed))
           (my= "hello" (the string                           ; this declaration
                             (macrolet ((a () "hello"))
                               (a)))))
BAR
CL-USER> (disassemble 'bar)
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
