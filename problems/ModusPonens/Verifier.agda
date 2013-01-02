module Verifier where

open import ModusPonens using (modusPonens)

check : ∀ {P Q : Set} → (P → Q) → P → Q
check = modusPonens
