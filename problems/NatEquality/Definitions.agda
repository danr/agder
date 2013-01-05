module Definitions where

data ℕ : Set where
  zero : ℕ
  suc  : (n : ℕ) → ℕ

data _≡_ (x : ℕ) : ℕ → Set where
  refl : x ≡ x

data _≢_ : ℕ → ℕ → Set where
  z≢s : ∀ {n} → zero ≢ suc n
  s≢z : ∀ {n} → suc n ≢ zero
  s≢s : ∀ {m n} → m ≢ n → suc m ≢ suc n

data Equal? (m n : ℕ) : Set where
  yes : m ≡ n → Equal? m n
  no  : m ≢ n → Equal? m n

data ⊥ : Set where
