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
         (== `(,e₁ ,e₂) e)
         (⊢ Γ e₁ `(,τ₂ → ,τ))
         (⊢ Γ e₂ τ₂)))
      ((fresh (a τ₁ τ₂)
         (== `(inl ,a) e)
         (== `(,τ₁ + ,τ₂) τ)
         (⊢ Γ a τ₁)))
      ((fresh (b τ₁ τ₂)
         (== `(inr ,b) e)
         (== `(,τ₁ + ,τ₂) τ)
         (⊢ Γ b τ₂)))
      ((fresh (x a l b r τ₁ τ₂)
         (== `(match ,x ((inl ,a) ,l) ((inr ,b) ,r)) e)
         (symbolo a)
         (symbolo b)
         (⊢ Γ x `(,τ₁ + ,τ₂))
         (⊢ `((,a . τ₁) . ,Γ) l τ)
         (⊢ `((,b . τ₂) . ,Γ) r τ))))))
