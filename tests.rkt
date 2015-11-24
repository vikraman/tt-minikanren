#lang racket

(require rackunit "main.rkt")

(define-syntax inhabits
  (λ (stx)
    (syntax-case stx ()
      [(inhabits e τ)
       #'(let ([msg (format "~a :: ~a" e τ)])
           (begin
             (displayln msg)
             (check member τ (run* (q) (⊢ '() e q)) msg)
             (check member e (run 100 (q) (⊢ '() q τ)) msg)))]
      [(inhabits e τ e′)
       #'(let ([msg (format "~a :: ~a" e τ)])
           (begin
             (displayln msg)
             (check member τ (run* (q) (⊢ '() e q)) msg)
             (check member e′ (run 100 (q) (⊢ '() q τ)) msg)))])))

(define ⊢tests
  (test-suite
   "Tests for ⊢"
   (inhabits '⊤ '⊤)
   (inhabits '(λ (x) x) '(_.0 → _.0)
             '((λ (_.0) _.0) (sym _.0)))
   (inhabits '((λ (x) x) (λ (y) y)) '(_.0 → _.0)
             '(((λ (_.0) _.0) (λ (_.1) _.1)) (sym _.0 _.1)))
   (inhabits '(λ (x) ⊤) '(_.0 → ⊤)
             '((λ (_.0) ⊤) (sym _.0)))
   (inhabits '(inl ⊤) '(⊤ + _.0))
   (inhabits '(inr ⊤) '(_.0 + ⊤))
   ;; FIXME:
   (inhabits '(match (inl ⊤) ((inl a) ⊤) ((inr b) ⊤)) '⊤
             '⊤)
   (inhabits '(cons ⊤ ⊤) '(⊤ × ⊤))
   (inhabits '(car (cons ⊤ ⊤)) '⊤)
   (inhabits '(cdr (cons ⊤ ⊤)) '⊤)))

(require rackunit/text-ui)
(run-tests ⊢tests)
