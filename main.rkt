#lang racket

(require "mk/mk.rkt")

(define apply-Γ
  (λ (Γ e τ)
    (symbolo e)
    (fresh (aa da Γ′)
      (== `((,aa . ,da) . ,Γ′) Γ)
      (conde
        ((== aa e) (== da τ))
        ((=/= aa e) (apply-Γ Γ′ e τ))))))

(define ⊢
  (λ (Γ e τ)
    (conde
      ((symbolo e) (apply-Γ Γ e τ))
      ((== '⊤ τ) (== '⊤ e))
      ((== '⊥ τ) (== #t #f))
      ((fresh (x b τ₁ τ₂)
         (== `(λ (,x) ,b) e)
         (== `(,τ₁ → ,τ₂) τ)
         (symbolo x)
         (⊢ `((,x . ,τ₁) . ,Γ) b τ₂)))
      ((fresh (e₁ e₂ τ₂)
         (== `(e₁ e₂) e)
         (⊢ Γ e₁ `(,τ₂ → ,τ))
         (⊢ Γ e₂ τ₂))))))
