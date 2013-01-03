module Verifier where

open import DeMorgan using (dm ; ¬_ ; _∨_ ; _∧_)

check : {A B : Set} → ¬ A ∧ ¬ B → ¬ (A ∨ B)
check = dm
