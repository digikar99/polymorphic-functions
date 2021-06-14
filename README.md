# polymorphic-functions

>This library is still experimental. Interface can change in backward-incompatible ways.
>Please wait until a proper release to use in production code. (Or track commits and use git submodules and/or qlot!)

The library provides both [Ad hoc polymorphism](https://en.wikipedia.org/wiki/Ad_hoc_polymorphism) and AOT (IIUC) [Parametric Polymorphism](https://en.wikipedia.org/wiki/Parametric_polymorphism) to dispatch and/or optimize on the basis of types rather than classes. See [this](https://www.reddit.com/r/lisp/comments/nho7gr/polymorphicfunctions_possibly_aot_dispatch_on/gyyovkh?utm_source=share&utm_medium=web2x&context=3) and nearby comments for an attempt at explaining this.

## Usage

- Users are expected to define a `polymorphic-function` (analogous to `cl:generic-function`) with one or more `polymorph` (similar to `cl:method`). These may be dispatched at runtime or at compile time if optimization policy is `(and (= speed 3) (/= debug 3))` abbreviated as `optim-speed`.
- Adhoc Polymorphism is supported in the sense that different polymorphs can have different implementations.
- Parametric Polymorphism is supported in the sense that once a polymorph is defined, then when a call to it is being compiled, then the type declarations inside the lambda-body of the polymorph are enhanced using the more specific type declarations in the environment. Thus, a polymorph that was defined for `vector` when compiled with arguments declared to be `simple-string`, then the body is made aware at *compiler/macroexpansion time* that the arguments are actually `simple-string` rather than just `vector`. Code further in the succeeding compiler/macroexpansion phases can then make use of this information. For code in the inner scopes of the polymorph, a `type-like` declaration is provided with example usage in [src/misc-tests.lisp](src/misc-tests.lisp).
- Individual polymorphs may also additionally have compiler macros. However, the policy under which these may be invoked is undefined. In essence, user code must not rely on compiler macros for *correctness*.


### Examples

See [src/misc-tests.lisp](src/misc-tests.lisp) for some more examples.

```lisp
(use-package :polymorphic-functions)
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
; While compiling
;     (MY= A B)
;   Following notes were encountered:
;     
;     No applicable POLYMORPH discovered for polymorphic-function
;       MY=
;     and ARG-LIST:
;     
;       (A B)
;     
;     Available Effective-Type-Lists include:
;     
;       (STRING STRING)
;       (CHARACTER CHARACTER)
;       ((SIMPLE-ARRAY SINGLE-FLOAT) (SIMPLE-ARRAY SINGLE-FLOAT))
BAZ
CL-USER> (my= 5 "hello")
; Evaluation aborted on #<POLYMORPHIC-FUNCTIONS::NO-APPLICABLE-POLYMORPH/ERROR {103A713D13}>.
```

### Libraries / Projects currently using polymorphic-functions

- [abstract-arrays](https://github.com/digikar99/abstract-arrays) and [dense-arrays](https://github.com/digikar99/dense-numericals/)
- [dense-numericals](https://github.com/digikar99/dense-numericals/): this makes extensive use of parametric polymorphism to avoid code repetition in the *packaged* provided code, cutting down on initial compile times. 
- lisp-polymorph with currently working 
  - [polymorph.maths](https://github.com/lisp-polymorph/polymorph.maths)
  - [polymorph.access](https://github.com/lisp-polymorph/polymorph.access)
  - [polymorph.copy-cast](https://github.com/lisp-polymorph/polymorph.copy-cast)
  - and more...


## Rationale

`polymorphic-function` are implemented using the metaclass `closer-mop:funcallable-standard-class` and `closer-mop:set-funcallable-instance-function`.

As per [CLHS](http://www.lispworks.com/documentation/HyperSpec/Body/t_generi.htm#generic-function),

>A generic function is a function whose behavior depends on the classes or identities of the arguments supplied to it.

By contrast, polymorphic-functions dispatch on the types of the arguments supplied to it. This helps dispatching on specialized arrays as well as user-defined types.

In contrast to [sealable-metaobjects](https://github.com/marcoheisig/sealable-metaobjects) and [fast-generic-functions](https://github.com/marcoheisig/fast-generic-functions), polymorphic-functions does not make any assumptions about the sealedness of a domain for purposes of inlining. Thus, users are expected to abide by the same precautions for inline optimizations here as they do while inlining normal functions. In particular, users are expected to recompile their code after additional polymorphs are defined, and also accordingly manage the compilation order of their files and systems.

IIUC, [specialized-function](https://github.com/numcl/specialized-function) provides a JIT variant of parametric polymorphism. By contrast, PF provides an AOT variant.

A related project [specialization-store](https://github.com/markcox80/specialization-store) also provides support for type-based dispatch:

>A premise of specialization store is that all specializations should perform the same task. Specializations should only differ in how the task is performed. This premise resolves ambiguities that arise when using types, rather than classes, to select the most specific specialization to apply.

However, the implications of this assumption are that individual specializations in each store-object of specialization-store [do not have initializer forms for optional or keyword arguments](https://github.com/markcox80/specialization-store/wiki/Tutorial-2:-Optional,-Keyword-and-Rest-Arguments).

By contrast, like usual generic-functions, PF does allow initializer forms for optional and keywords arguments for individual polymorphs.

In addition to being dispatched on types, PF also provides the ability to install compiler-macros for individual `polymorphs`.

## Dependencies outside quicklisp

- SBCL 2.0.9+
- [trivial-types:function-name](https://github.com/digikar99/trivial-types)
- [cl-form-types](https://github.com/alex-gutev/cl-form-types)
  - [cl-environments](https://github.com/alex-gutev/cl-environments)
- [compiler-macro-notes](https://github.com/digikar99/compiler-macro-notes)
- and more... better use ultralisp until then!

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
(ql:quickload "polymorphic-functions")
(asdf:test-system "polymorphic-functions")
```

## Tests

Tests are distributed throughout the system. Run `(asdf:test-system "polymorphic-functions")`.

## Inline Optimizations

A compiler-note-providing compiler-macro has also been provided for compile-time optimization guidelines.

- A speed=3 optimization coupled with debug<3 optimization results in (attempts to) inline-optimizations.
- Inline optimizations may be avoided by `(declare (notinline my-polymorph))` - however, inlining is the only way to optimize polymorphic-functions.

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

This problem also arises with [static-dispatch](https://github.com/alex-gutev/static-dispatch) and [inlined-generic-functions](https://github.com/guicho271828/inlined-generic-function). The way to avoid it is to either maintain discipline on the part of the user (the way polymorphic-functions [currently] assumes) or to seal domains (the way of fast-generic-functions and sealable-metaobjects).

Inlining especially becomes necessary for mathematical operations, wherein a call to `generic-+` on SBCL can be a 3-10 times slower than the optimized calls to `fixnum +` or `single-float +` etc. `generic-cl` (since `static-dispatch` version 0.5) overcomes this on SBCL by using `sb-c:deftransform`; for portable projects, one could use `inlined-generic-functions` [superseded by `fast-generic-functions`] subject to the limitation that there are no separate classes for (array single-float) and (array double-float) at least until SBCL 2.1.1.

## Related Projects

- [static-dispatch](https://github.com/alex-gutev/static-dispatch)
- [specialization-store](https://github.com/markcox80/specialization-store)
- [fast-generic-functions](https://github.com/marcoheisig/fast-generic-functions)
- [inlined-generic-functions](https://github.com/guicho271828/inlined-generic-function)
- [specialized-function](https://github.com/numcl/specialized-function)



## Feature Parity

The runtime dispatch performance of all the three of polymorphic-functions, cl:generic-function and specialization-store is comparable at least for a small number of polymorphs/methods/specializations.

| Feature                         | cl:generic-function | specialization-store | polymorphic-functions |
|:--------------------------------|:--------------------|:---------------------|:----------------------|
| Method combination              | Yes                 | No                   | No                    |
| Precedence                      | Yes                 | Partial*             | Yes                   |
| &optional, &key, &rest dispatch | No                  | Yes                  | Yes^                  |
| Run-time Speed                  | Fast                | Fast                 | Fast                  |
| Compile-time support            | Partial**           | Yes                  | Yes                   |
| Parametric Polymorphism         | Partial**           | No                   | Yes                   |

^See [#comparison-with-specialization-store](#comparison-with-specialization-store).
Well...

\*\*Using [fast-generic-functions](https://github.com/marcoheisig/fast-generic-functions) - but this apparantly has a few limitations like requiring non-builtin-classes to have an additional metaclass. This effectively renders it impossible to use for the classes in already existing libraries. But, there's also [static-dispatch](https://github.com/alex-gutev/static-dispatch).

## Limitations

- For form-type-inference, polymorphic-functions depends on cl-form-types. Thus, this works as long as cl-form-types succeeds, and [cl-form-types](https://github.com/alex-gutev/cl-form-types) does get pretty extensive. In cases wherein it does fail, we also rely on `sb-c:deftransform` on SBCL.
- Integration with SLIME is yet to be thought about; etags could work, but this needs more thinking given the apparant non-extensibility of internals of `slime-edit-definition`. imenu is also another option.
- ANSI is insufficient for our purposes: we need `introspect-environment:policy-quality` and CLTL2 and more for cl-form-types; if someone needs a reduced feature version within the bounds of ANSI standard, please raise an issue!
- A [bug on CCL](https://github.com/Clozure/ccl/pull/369) may not let PF work as correctly on CCL; subjectively dirty workarounds are possible until it gets fixed.
- A `polymorphic-functions.extended-types` package (not system!) is also provided based on [ctype](https://github.com/s-expressionists/ctype). This allows one to extend the CL type system to define types beyond what `cl:deftype` can do to some extent. While these cannot be used inside an arbitrary CL form with `cl:declare`, these can be used in the type lists of polymorphs. See [src/extended-types/supertypep.lisp](src/extended-types/supertypep.lisp) for an example put to use in [trivial-coerce](https://github.com/digikar99/trivial-coerce).

## Acknowledgements

- [Alex Gutev](https://github.com/alex-gutev/) for an extensive [cl-form-types](https://github.com/alex-gutev/cl-form-types)!
- [Andrew](https://github.com/commander-trashdin/) for extensively putting polymorphic-functions to test at a brewing project on [lisp-polymorph](https://github.com/lisp-polymorph/)!
