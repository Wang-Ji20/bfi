#lang racket

(require rackunit)

(define-struct bf-runtime (memory pointer pc stack))

(define BFMEMORY 32768)

(define (init-state)
  (make-bf-runtime
   (make-vector BFMEMORY 0)
   0
   0
   '()))

(define (dump bf-state)
  (begin (println (bf-runtime-memory bf-state))
         (println (bf-runtime-pointer bf-state))
         (println (bf-runtime-pc bf-state))
         (println (bf-runtime-stack bf-state))))

(define (incr x) (+ 1 x))

(define (decr x) (- x 1))

(define (change-memory inst memory pointer)
  (let ([*p (vector-ref memory pointer)])
    (cond
      [(equal? #\+ inst)
       (begin (vector-set! memory pointer (incr *p))
              memory)]
      [(equal? #\- inst)
       (begin (vector-set! memory pointer (decr *p))
              memory)]
      [(equal? #\, inst)
       (let ([in (read-char (current-input-port))])
         (begin (vector-set! memory pointer in)
                memory))]
      [else memory])))

(check-equal? (let ([init-state (init-state)])
                (change-memory #\+ (bf-runtime-memory init-state) (bf-runtime-pointer init-state)))
              (let ([v (make-vector BFMEMORY 0)])
                (begin (vector-set! v 0 1)
                       v)))

(define (change-pointer inst pointer)
  (cond
    [(equal? #\> inst) (incr pointer)]
    [(equal? #\< inst) (decr pointer)]
    [else pointer]))

; tape is char sequence, pc is current position of #\[
(define (find-matching-rparen tape pc)
  (local [(define (find-rec current-pc lparen-seen)
            (let ([current-inst (vector-ref tape current-pc)]
                  [spc (incr current-pc)])
              (cond
                [(and (equal? lparen-seen 1)
                      (equal? current-inst #\])) spc]
                [(equal? current-inst #\[)
                 (find-rec spc (incr lparen-seen))]
                [(equal? current-inst #\])
                 (find-rec spc (decr lparen-seen))]
                [else (find-rec spc lparen-seen)])))]
    (find-rec (incr pc) 1) ))

(let* ([s "[1[[1]1]]11"]
       [v (list->vector (string->list s))])
  (check-equal? (find-matching-rparen v 0)
                9))

(define (change-pc inst tape *p stack pc)
  (let  ([spc (+ 1 pc)])
    (cond
      [(and (equal? #\[ inst) (equal? *p 0)) (find-matching-rparen tape pc)]
      [(and (equal? #\] inst) (not (equal? *p 0))) (car stack)]
      [else spc])))

(define (change-stack inst *p pc stack)
  (cond
    [(equal? inst #\[) (if (equal? 0 *p) stack (cons (incr pc) stack))]
    [(equal? inst #\]) (if (equal? 0 *p) (cdr stack) stack)]
    [else stack]))

(define (handle-side-effect inst *p)
  (if (equal? inst #\.) (begin (display (if (integer? *p) (integer->char *p) *p)) '()) '()))

(define (eval-bf runtime tape)
  (let* ([memory (bf-runtime-memory runtime)]
         [pointer (bf-runtime-pointer runtime)]
         [pc (bf-runtime-pc runtime)]
         [inst (vector-ref tape pc)]
         [stack (bf-runtime-stack runtime)]
         [*p (vector-ref memory pointer)])
    (begin (handle-side-effect inst *p)
           (make-bf-runtime (change-memory inst memory pointer)
                            (change-pointer inst pointer)
                            (change-pc inst tape *p stack pc)
                            (change-stack inst *p pc stack)))))

(define (to-tape s) (list->vector (string->list s)))

(define (eval-bf-all runtime tape)
  (let ([pc (bf-runtime-pc runtime)]
        [len (vector-length tape)])
    (if (equal? pc len) runtime (eval-bf-all (eval-bf runtime tape) tape))))

(define hello-world "++++++++++/[/>+++++++>++++++++++>+++>+<<<<-/]>++.>+.+++++++..+++.>++.<<+++++++++++++++.>.+++.------.--------.>+.>.")

(eval-bf-all (init-state) (to-tape hello-world))
