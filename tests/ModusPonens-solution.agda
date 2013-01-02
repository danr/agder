module ModusPonens where

modusPonens : ∀ {P Q : Set} → (P → Q) → P → Q
modusPonens x = x
