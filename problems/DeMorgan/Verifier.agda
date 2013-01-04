module Verifier where

open import Definitions
open import DeMorgan using (deMorgan)

check : {A B : Set} → ¬ A ∧ ¬ B → ¬ (A ∨ B)
check = deMorgan
