module UniverseCollapse
    (down : Set₁ -> Set)
    (up : Set → Set₁)
    (iso : ∀ {A} → down (up A) → A)
    (osi : ∀ {A} → up (down A) → A) where

anything : (A : Set) → A
anything = {!!}
