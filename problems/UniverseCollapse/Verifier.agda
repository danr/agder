module Verifier
    (down : Set₁ -> Set)
    (up : Set → Set₁)
    (iso : ∀ {A} → down (up A) -> A)
    (osi : ∀ {A} → up (down A) -> A) where

import UniverseCollapse as UC
open UC down up iso osi using (anything)

check : (A : Set) -> A
check = anything
