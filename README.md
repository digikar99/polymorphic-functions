# typed-dispatch

>WARNING: No tests have been set up yet. The below worked as of the commit the README was updated.
>Requires latest SBCL (post 2.0.8 even) due to a [local call context dumping](https://github.com/sbcl/sbcl/commit/135afdf39381266ffd4baeeeb285fb11868fd57b).


## Why?

(Also see the section on [Examples](#examples))

Well...

```lisp
(use-package :typed-dispatch)
(define-typed-function my= (a b))
(defun-typed my= ((a string) (b string))
  (string= a b))
(defun-typed my= ((a character) (b character))
  (char= a b))
(defun-typed my= ((a (simple-array single-float))
                  (b (simple-array single-float)))
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
; Size: 47 bytes. Origin: #x52D405E3                          ; FOO
; 5E3:       498B4510         MOV RAX, [R13+16]               ; thread.binding-stack-pointer
; 5E7:       488945F8         MOV [RBP-8], RAX
; 5EB:       31F6             XOR ESI, ESI
; 5ED:       48C745F017011050 MOV QWORD PTR [RBP-16], #x50100117  ; NIL
; 5F5:       488975E8         MOV [RBP-24], RSI
; 5F9:       48C745E017011050 MOV QWORD PTR [RBP-32], #x50100117  ; NIL
; 601:       B90C000000       MOV ECX, 12
; 606:       FF7508           PUSH QWORD PTR [RBP+8]
; 609:       B8E2E83450       MOV EAX, #x5034E8E2             ; #<FDEFN SB-KERNEL:STRING=*>
; 60E:       FFE0             JMP RAX
; 610:       CC10             INT3 16                         ; Invalid argument count trap
NIL
CL-USER> (defun bar (a b)
           (declare (optimize speed)
                    (type string a b))
           (my= a b))
BAR
CL-USER> (disassemble 'bar)
; disassembly for BAR
; Size: 47 bytes. Origin: #x52D40883                          ; BAR
; 83:       498B4510         MOV RAX, [R13+16]                ; thread.binding-stack-pointer
; 87:       488945F8         MOV [RBP-8], RAX
; 8B:       31F6             XOR ESI, ESI
; 8D:       48C745F017011050 MOV QWORD PTR [RBP-16], #x50100117  ; NIL
; 95:       488975E8         MOV [RBP-24], RSI
; 99:       48C745E017011050 MOV QWORD PTR [RBP-32], #x50100117  ; NIL
; A1:       B90C000000       MOV ECX, 12
; A6:       FF7508           PUSH QWORD PTR [RBP+8]
; A9:       B8E2E83450       MOV EAX, #x5034E8E2              ; #<FDEFN SB-KERNEL:STRING=*>
; AE:       FFE0             JMP RAX
; B0:       CC10             INT3 16                          ; Invalid argument count trap
NIL
CL-USER> (my= (make-array 1 :element-type 'single-float)
              (make-array 1 :element-type 'single-float))
HELLO
CL-USER> (defun baz (a b)
           (declare (type string a)
                    (type integer b))
           (my= a b))
; While compiling (MY= A B): 
;   No applicable TYPED-FUNCTION discovered for TYPE-LIST (STRING INTEGER).
;   Available TYPE-LISTs include:
;      ((SIMPLE-ARRAY SINGLE-FLOAT) (SIMPLE-ARRAY SINGLE-FLOAT))
;      (CHARACTER CHARACTER)
;      (STRING STRING)
BAZ
CL-USER> (my= 5 "hello")
; Evaluation aborted on #<SIMPLE-ERROR "~%No applicable TYPED-FUNCTION discovered for TYPE-LIST ~D.~%Available TYPE-LISTs include:~%   ~{~D~^~%   ~}" {1004FC50D3}>.
```

## Hmm... Aren't there existing works?

I know of exactly one: [specialization-store](https://github.com/markcox80/specialization-store). It does a [few more things](https://github.com/markcox80/specialization-store/wiki):

- Dispatch on rest and keyword args
- Allow for explicitly naming "specialized function"

Honestly, I'd be on the lookout for something based on MOP. I spent half an hour on the book; then, gave up, and spent two hours on the first commit of this.

**What does this do differently?**

- Current implementation in <600 lines vs 3400+ for specialization-store. Yes the latter has more features, but I feel it can be done in <1000 LOC
- I (actually [we](https://github.com/commander-trashdin/cl-overload)) wanted some better compile time reporting; I spent 1.5+ hours on specialization-store, and then gave up
- I wanted a bit better compile time warnings/suggestions/notes

## Dependencies outside quicklisp

- [trivial-types:function-name](https://github.com/digikar99/trivial-types)

## Examples

As of this commit, there are exactly three exported symbols:

- define-typed-function
- defun-typed
- define-compiler-macro-typed

```lisp
CL-USER> (use-package :typed-dispatch)
T
CL-USER> (define-typed-function my= (a b))
MY=
CL-USER> (defun-typed my= ((a string) (b string))
           (string= a b))
MY=
CL-USER> (defun foo (a b)
           (declare (optimize speed))
           (my= a b))

; Unable to optimize (MY= A B) because:
;  Type of A is not declared
FOO
CL-USER> (defun foo (a b)
           (declare (optimize speed)
                    (type string a b))
           (my= a b))
WARNING: redefining COMMON-LISP-USER::FOO in DEFUN
FOO
CL-USER> (defun foo (a b)
           (declare (type string a)
                    (type integer b))
           (my= a b))

; While compiling (MY= A B): 
;   No applicable TYPED-FUNCTION discovered for TYPE-LIST (STRING INTEGER).
;   Available TYPE-LISTs include:
;      (STRING STRING)
WARNING: redefining COMMON-LISP-USER::FOO in DEFUN
FOO
CL-USER> (defun foo (a b)
           (declare (optimize speed)
                    (type string a)
                    (type integer b))
           (my= a b))

; Unable to optimize (MY= A B) because:
;  No applicable TYPED-FUNCTION discovered for TYPE-LIST (STRING INTEGER).
;  Available TYPE-LISTs include:
;     (STRING STRING)
WARNING: redefining COMMON-LISP-USER::FOO in DEFUN
FOO
CL-USER> (defun-typed my= ((a integer) (b integer))
           (= a b))
MY=
CL-USER> (defun foo (a b)
           (declare (optimize speed)
                    (type integer a b))
           (my= a b))
WARNING: redefining COMMON-LISP-USER::FOO in DEFUN
FOO
CL-USER> (define-compiler-macro-typed my= (integer integer)
             (&whole form &rest args)
           (declare (ignore args))
           (format t "Inside compiler macro ~D" form)
           form)
MY=
CL-USER> (defun foo (a b)
           (declare (optimize speed)
                    (type integer a b))
           (my= a b))
Inside compiler macro ((LAMBDA (A B)
                         (DECLARE (TYPE INTEGER B)
                                  (TYPE INTEGER A))
                         (= A B))
                       A B)
WARNING: redefining COMMON-LISP-USER::FOO in DEFUN
FOO
```

Note that while the compiler macro of the typed function checks the types during compilation, and signals a note if a compatible type-list is not registered, it does not error. And it will not error, to handle the case for circular dependencies.

**A note about &optional args**

(Note the lambda-lists.)

```lisp
CL-USER> (define-typed-function bar (a &optional b))
BAR
CL-USER> (defun-typed bar ((a string) &optional (b integer)) 
           (list a b))
; Evaluation aborted on #<TYPE-ERROR expected-type: (SATISFIES TYPED-DISPATCH::TYPED-LAMBDA-LIST-P)
             datum: ((A STRING) &OPTIONAL (B INTEGER))>.
CL-USER> (defun-typed bar ((a string) &optional ((b integer) 5))
           (list a b))
BAR
CL-USER> (defun-typed bar ((a string) &optional ((b integer) 5 bp))
           (declare (ignore bp))
           (list a b))
BAR
CL-USER> (bar "hello")
("hello" 5)
CL-USER> (bar "hello" 7)
("hello" 7)
```
