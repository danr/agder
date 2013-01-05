module Verifier where

open import Definitions
open import NatEquality using (_≟_ ; equality-disjoint)

check1 : (m n : ℕ) → Equal? m n
check1 = _≟_

check2 : (m n : ℕ) → m ≡ n → m ≢ n → ⊥
check2 = equality-disjoint

