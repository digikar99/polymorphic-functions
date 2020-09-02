# typed-dispatch

>WARNING: No tests have been set up yet. The below worked as of the very first commit.

## Why?

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
CL-USER> (my= (make-array 1 :element-type 'single-float)
              (make-array 1 :element-type 'single-float))
HELLO
CL-USER> (defun baz (a b)
           (declare (type string a)
                    (type integer b))
           (my= a b))
; in: DEFUN BAZ
;     (MY= A B)
; 
; caught WARNING:
;   Error during compiler-macroexpansion of (MY= A B). Use *BREAK-ON-SIGNALS* to
;   intercept.
;   
;    No applicable TYPED-FUNCTION discovered. Available TYPE-LISTs include
;   (((SIMPLE-ARRAY SINGLE-FLOAT) (SIMPLE-ARRAY SINGLE-FLOAT))
;    (CHARACTER CHARACTER) (STRING STRING))
; 
; compilation unit finished
;   caught 1 WARNING condition
BAZ
CL-USER> (my= 5 "hello")
; Evaluation aborted on #<SIMPLE-ERROR "No applicable TYPED-FUNCTION discovered. Available TYPE-LISTs include~%~D" {101073D8C3}>.
```

## Hmm... Aren't there existing works?

I know of exactly one: [specialization-store](https://github.com/markcox80/specialization-store). It does a few more things:

- Dispatch on optional and keyword args
- Allow for naming "specialized function"

Honestly, I'd be on the lookout for something based on MOP. I spent half an hour on the book; then, gave up, and spent two hours on this.

What does this do differently?

- Current implementation in <150 lines vs 3400+ for specialization-store. Yes the latter has more features, but I feel it can be done in <1000 LOC.
- I wanted one feature (warn/error at compile time, if error can be determined at run time; otherwise compile successfully); I spent 1.5+ hours on specialization-store, and then gave up.


