\begin{code}
module Scoped.Reduction where
\end{code}

\begin{code}
open import Scoped
open import Scoped.RenamingSubstitution
open import Builtin
open import Builtin.Constant.Type

open import Utils

open import Agda.Builtin.String using (primStringFromList; primStringAppend)
import Data.List as List
open import Data.Sum renaming (inj₁ to inl; inj₂ to inr)
open import Data.Vec using ([];_∷_;_++_)
open import Data.Product
open import Function
open import Data.Integer as I
open import Data.Nat as N hiding (_<?_;_>?_;_≥?_)
open import Relation.Nullary
open import Relation.Binary.PropositionalEquality hiding ([_];trans)
open import Data.Bool using (Bool;true;false)
import Debug.Trace as Debug
\end{code}

\begin{code}
infix 2 _—→_
\end{code}

\begin{code}
data _≤W'_ {n}(w : Weirdℕ n) : ∀{n'} → Weirdℕ n' → Set where
 base : w ≤W' w
 skipT : ∀{n'}{w' : Weirdℕ n'} → (T w) ≤W' w' → w ≤W' w'
 skipS : ∀{n'}{w' : Weirdℕ n'} → (S w) ≤W' w' → w ≤W' w'

-- the number of arguments for builtin, type arguments and then term
-- arguments type arguments and term arguments can be interspersed
ISIG : Builtin → Σ ℕ λ n → Weirdℕ n
ISIG addInteger = 0 , S (S Z)
ISIG subtractInteger = 0 , S (S Z)
ISIG multiplyInteger = 0 , S (S Z)
ISIG divideInteger = 0 , S (S Z)
ISIG quotientInteger = 0 , S (S Z)
ISIG remainderInteger = 0 , S (S Z)
ISIG modInteger = 0 , S (S Z)
ISIG lessThanInteger = 0 , S (S Z)
ISIG lessThanEqualsInteger = 0 , S (S Z)
ISIG greaterThanInteger = 0 , S (S Z)
ISIG greaterThanEqualsInteger = 0 , S (S Z)
ISIG equalsInteger = 0 , S (S Z)
ISIG concatenate = 0 , S (S Z)
ISIG takeByteString = 0 , S (S Z)
ISIG dropByteString = 0 , S (S Z)
ISIG sha2-256 = 0 , S Z
ISIG sha3-256 = 0 , S Z
ISIG verifySignature = 0 , S (S (S Z))
ISIG equalsByteString = 0 , S (S Z)
ISIG ifThenElse = 1 , S (S (T Z))
ISIG charToString = 0 , S (S Z)
ISIG append = 0 , S (S Z)
ISIG trace = 0 , S Z

data Value {n}{w : Weirdℕ n} : ScopedTm w → Set where
  V-ƛ : ∀ (A : ScopedTy n)(t : ScopedTm (S w)) → Value (ƛ A t)
  V-Λ : ∀ {K}(t : ScopedTm (T w)) → Value (Λ K t)
  V-con : (tcn : TermCon) → Value (con {n} tcn)
  V-wrap : (A B : ScopedTy n){t : ScopedTm w} → Value t → Value (wrap A B t)
  V-builtin : (b : Builtin)
            → (t : ScopedTm w)
            → ∀{m m'}{v : Weirdℕ m}{v' : Weirdℕ m'}
            -- the next arg expected is a term arg
            → let m'' , v'' = ISIG b in
              (p : m'' ≡ m')
            → (q : subst Weirdℕ p v'' ≡ v')
            → S v ≤W' v'
            → Sub v w
            → Value t
  V-builtin⋆ : (b : Builtin)
             → (t : ScopedTm w)
             -- the next arg expected is a type arg
             → Value t

--we could process the arity of t...


-- (b : Builtin) → Sub v' w → v < snd (ISIG b)

voidVal : ∀ {n}(w : Weirdℕ n) → Value {w = w} (con unit)
voidVal w = V-con {w = w} unit

deval : ∀{n}{w : Weirdℕ n}{t : ScopedTm w} → Value t → ScopedTm w
deval {t = t} v = t

open import Data.Unit
VTel : ∀{n} m (w : Weirdℕ n) → Tel w m → Set
VTel 0       w []       = ⊤
VTel (suc m) w (t ∷ ts) = Value t × VTel m w ts

-- a term that satisfies this predicate has an error term in it somewhere
-- or we encountered a rumtime type error
data Error {n}{w : Weirdℕ n} : ScopedTm w → Set where
   -- a genuine runtime error returned from a builtin
   E-error : (A : ScopedTy n) → Error (error A)

data Any {n : ℕ}{w : Weirdℕ n}(P : ScopedTm w → Set) : ∀{m} → Tel w m → Set
  where
  here  : ∀{m t}{ts : Tel w m} → P t → Any P (t ∷ ts)
  there : ∀{m t}{ts : Tel w m} → Value t → Any P ts → Any P (t ∷ ts)

VERIFYSIG : ∀{n}{w : Weirdℕ n} → Maybe Bool → ScopedTm w
VERIFYSIG (just false) = con (bool false)
VERIFYSIG (just true)  = con (bool true)
VERIFYSIG nothing      = error (con bool)

open import Data.List using (List;[];_∷_)
open import Type using (Kind)

{-
data _≤W'_ : ℕ → ℕ → Set where
 base : 0 ≤W' 0
 skip : ∀{n n'} → Kind → ℕ.suc n ≤W' n' → n ≤W' n'


sig2type⇒ : ∀{Φ} → List (ScopedTy Φ) → ScopedTy Φ → ScopedTy Φ
sig2type⇒ []       C = C
sig2type⇒ (A ∷ As) C = A ⇒ sig2type⇒ As C

sig2type' : ∀{Φ Φ'} → Φ ≤W' Φ' → List (ScopedTy Φ') → ScopedTy Φ' → ScopedTy Φ
sig2type' base       As C = sig2type⇒ As C
sig2type' (skip K p) As C = Π K (sig2type' p As C)
-}

IBUILTIN : ∀{n}{w : Weirdℕ n}(b : Builtin) → Sub (proj₂ (ISIG b)) w → ScopedTm w
IBUILTIN b σ = {!!}

IBUILTIN' : ∀{n n'}{w : Weirdℕ n}{w' : Weirdℕ n'}(b : Builtin) → (p : proj₁ (ISIG b) ≡ n') → subst Weirdℕ p (proj₂ (ISIG b)) ≡ w' → Sub w' w → ScopedTm w
IBUILTIN' = {!!}



-- this is currently in reverse order...
BUILTIN : ∀{n}{w : Weirdℕ n}
  → (b : Builtin)
  → Tel⋆ n (arity⋆ b) → (ts : Tel w (arity b)) → VTel (arity b) w ts → ScopedTm w
BUILTIN addInteger _ (_ ∷ _ ∷ []) (V-con (integer i) , V-con (integer i') , tt) =
  con (integer (i I.+ i'))
BUILTIN addInteger _ _ _ = error (con integer)
BUILTIN subtractInteger  _ (_ ∷ _ ∷ []) (V-con (integer i) , V-con (integer i') , tt) =
  con (integer (i I.- i'))
BUILTIN subtractInteger _ _ _ = error (con integer)
BUILTIN multiplyInteger _ (_ ∷ _ ∷ []) (V-con (integer i) , V-con (integer i') , tt) =
  con (integer (i I.* i'))
BUILTIN multiplyInteger _ _ _ = error (con integer)
BUILTIN divideInteger _ (_ ∷ _ ∷ []) (V-con (integer i) , V-con (integer i') , tt) =
  decIf (∣ i' ∣ N.≟ 0) (error (con integer)) (con (integer (div i i')))
BUILTIN divideInteger _ _ _ = error (con integer)
BUILTIN quotientInteger _ (_ ∷ _ ∷ []) (V-con (integer i) , V-con (integer i') , tt) =
  decIf (∣ i' ∣ N.≟ 0) (error (con integer)) (con (integer (quot i i')))
BUILTIN quotientInteger _ _ _ = error (con integer)
BUILTIN remainderInteger _ (_ ∷ _ ∷ []) (V-con (integer i) , V-con (integer i') , tt) =
    decIf (∣ i' ∣ N.≟ 0) (error (con integer)) (con (integer (rem i i')))
BUILTIN remainderInteger _ _ _ = error (con integer)
BUILTIN modInteger _ (_ ∷ _ ∷ []) (V-con (integer i) , V-con (integer i') , tt) =
    decIf (∣ i' ∣ N.≟ 0) (error (con integer)) (con (integer (mod i i')))
BUILTIN modInteger _ _ _ = error (con integer)
-- Int -> Int -> Bool
BUILTIN lessThanInteger _ (_ ∷ _ ∷ []) (V-con (integer i) , V-con (integer i') , tt) =
  decIf (i <? i') (con (bool true)) (con (bool false))
BUILTIN lessThanInteger _ _ _ = error (con bool)
BUILTIN lessThanEqualsInteger _ (_ ∷ _ ∷ []) (V-con (integer i) , V-con (integer i') , tt) =
  decIf (i I.≤? i') (con (bool true)) (con (bool false))
BUILTIN lessThanEqualsInteger _ _ _ = error (con bool)
BUILTIN greaterThanInteger _ (_ ∷ _ ∷ []) (V-con (integer i) , V-con (integer i') , tt) =
  decIf (i >? i') (con (bool true)) (con (bool false))
BUILTIN greaterThanInteger _ _ _ = error (con bool)
BUILTIN greaterThanEqualsInteger _ (_ ∷ _ ∷ []) (V-con (integer i) , V-con (integer i') , tt) =
  decIf (i ≥? i') (con (bool true)) (con (bool false))
BUILTIN greaterThanEqualsInteger _ _ _ = error (con bool)
BUILTIN equalsInteger _ (_ ∷ _ ∷ []) (V-con (integer i) , V-con (integer i') , tt) =
  decIf (i I.≟ i') (con (bool true)) (con (bool false))
BUILTIN equalsInteger _ _ _ = error (con bool)
-- BS -> BS -> BS
BUILTIN concatenate _ (_ ∷ _ ∷ []) (V-con (bytestring b) , V-con (bytestring b') , tt) = con (bytestring (concat b b'))
BUILTIN concatenate _ _ _ = error (con bytestring)
-- Int -> BS -> BS
BUILTIN takeByteString _ (_ ∷ _ ∷ []) (V-con (integer i) , V-con (bytestring b) , tt) = con (bytestring (take i b))
BUILTIN takeByteString _ _ _ = error (con bytestring)
BUILTIN dropByteString _ (_ ∷ _ ∷ []) (V-con (integer i) , V-con (bytestring b) , tt) = con (bytestring (drop i b))
BUILTIN dropByteString _ _ _ = error (con bytestring)
-- BS -> BS
BUILTIN sha2-256 _ (_ ∷ []) (V-con (bytestring b) , tt) = con (bytestring (SHA2-256 b))
BUILTIN sha2-256 _ _ _ = error (con bytestring)
BUILTIN sha3-256 _ (_ ∷ []) (V-con (bytestring b) , tt) = con (bytestring (SHA3-256 b))
BUILTIN sha3-256 _ _ _ = error (con bytestring)
BUILTIN verifySignature _ (_ ∷ _ ∷ _ ∷ []) (V-con (bytestring k) , V-con (bytestring d) , V-con (bytestring c) , tt) = VERIFYSIG (verifySig k d c)
BUILTIN verifySignature _ _ _ = error (con bytestring)
-- Int -> Int
BUILTIN equalsByteString _ (_ ∷ _ ∷ []) (V-con (bytestring b) , V-con (bytestring b') , tt) =
  con (bool (equals b b'))
BUILTIN equalsByteString _ _ _ = error (con bool)
BUILTIN ifThenElse (A ∷ []) (.(con (bool true)) ∷ t ∷ u ∷ []) (V-con (bool true) , vt , vu , tt) = t
BUILTIN ifThenElse (A ∷ []) (.(con (bool false)) ∷ t ∷ u ∷ []) (V-con (bool false) , vt , vu , tt) = u
BUILTIN ifThenElse (A ∷ []) _ _ = error A
BUILTIN charToString _ (_ ∷ []) (V-con (char c) , tt) = con (string (primStringFromList List.[ c ]))
BUILTIN charToString _ _ _ = error (con string)
BUILTIN append _ (_ ∷ _ ∷ []) (V-con (string s) , V-con (string t) , tt) =
  con (string (primStringAppend s t))
BUILTIN append _ _ _ = error (con string)
BUILTIN trace _ (_ ∷ []) (V-con (string s) , tt) = con (Debug.trace s unit)
BUILTIN trace _ _ _ = error (con unit)

data _—→T_ {n}{w : Weirdℕ n} : ∀{m} → Tel w m → Tel w m → Set

data _—→_ {n}{w : Weirdℕ n} : ScopedTm w → ScopedTm w → Set where
  ξ-·₁ : {L L' M : ScopedTm w} → L —→ L' → L · M —→ L' · M
  ξ-·₂ : {L M M' : ScopedTm w} → Value L → M —→ M' → L · M —→ L · M'
  ξ-·⋆ : {L L' : ScopedTm w}{A : ScopedTy n} → L —→ L' → L ·⋆ A —→ L' ·⋆ A
  ξ-wrap : {A B : ScopedTy n}{L L' : ScopedTm w}
    → L —→ L' → wrap A B L —→ wrap A B L'
  β-ƛ : ∀{A : ScopedTy n}{L : ScopedTm (S w)}{M : ScopedTm w} → Value M
      → (ƛ A L) · M —→ (L [ M ])
  β-Λ : ∀{K}{L : ScopedTm (T w)}{A : ScopedTy n}
      → (Λ K L) ·⋆ A —→ (L [ A ]⋆)
  ξ-unwrap : {t t' : ScopedTm w} → t —→ t' → unwrap t —→ unwrap t'
  β-wrap : {A B : ScopedTy n}{t : ScopedTm w}
    → Value t → unwrap (wrap A B t) —→ t

  β-builtin : (b : Builtin)
            → (t u : ScopedTm w)
            → ∀{m}{v : Weirdℕ m}
            -- the next arg expected is a term arg
            → let m' , v' = ISIG b in
              (p : m' ≡ m)
            → (q : subst Weirdℕ p v' ≡ S v)
            → (σ : Sub v w)
            → t · u —→ IBUILTIN' b p q (sub-cons σ u)

  E-·₁ : {A : ScopedTy n}{M : ScopedTm w} → error A · M —→ error missing
  E-·₂ : {A : ScopedTy n}{L : ScopedTm w} → Value L → L · error A —→ error missing

  -- error inside somewhere

  E-·⋆ : {A B : ScopedTy n} → error A ·⋆ B —→ error missing
--  E-Λ : ∀{K}{A : ScopedTy (N.suc n)} → Λ K (error A) —→ error missing

  E-unwrap : {A : ScopedTy n}
    → unwrap (error A) —→ error missing
  E-wrap : {A B C : ScopedTy n}
    → wrap A B (error C) —→ error missing

  -- runtime type errors
  -- these couldn't happen in the intrinsically typed version
  E-Λ·    : ∀{K}{L : ScopedTm (T w)}{M : ScopedTm w}
    → Λ K L · M —→ error missing
  E-ƛ·⋆   : ∀{B : ScopedTy n}{L : ScopedTm (S w)}{A : ScopedTy n}
    → ƛ B L ·⋆ A —→ error missing
  E-con·  : ∀{tcn}{M : ScopedTm w} → con tcn · M —→ error missing
  E-con·⋆ : ∀{tcn}{A : ScopedTy n} → con tcn ·⋆ A —→ error missing
  E-wrap· : {A B : ScopedTy n}{t M : ScopedTm w}
    → wrap A B t · M —→ error missing
  E-wrap·⋆ : {A' B A : ScopedTy n}{t : ScopedTm w}
    → wrap A' B t ·⋆ A —→ error missing
  E-ƛunwrap : {A : ScopedTy n}{t : ScopedTm (S w)}
    → unwrap (ƛ A t) —→ error missing
  E-Λunwrap : ∀{K}{t : ScopedTm (T w)} → unwrap (Λ K t) —→ error missing
  E-conunwrap : ∀{tcn} → unwrap (con tcn) —→ error missing

data _—→T_ {n}{w} where
  here  : ∀{m t t'}{ts : Tel w m} → t —→ t' → (t ∷ ts) —→T (t' ∷ ts)
  there : ∀{m t}{ts ts' : Tel w m}
    → Value t → ts —→T ts' → (t ∷ ts) —→T (t ∷ ts')
\end{code}

\begin{code}
data _—→⋆_ {n}{w : Weirdℕ n} : ScopedTm w → ScopedTm w → Set where
  refl  : {t : ScopedTm w} → t —→⋆ t
  trans : {t t' t'' : ScopedTm w} → t —→ t' → t' —→⋆ t'' → t —→⋆ t''
\end{code}

\begin{code}
data Progress {n}{i : Weirdℕ n}(t : ScopedTm i) : Set where
  step : ∀{t'} → t —→ t' → Progress t
  done : Value t → Progress t
  error : Error t → Progress t

data TelProgress {m}{n}{w : Weirdℕ n} : Tel w m → Set where
  done : {tel : Tel w m}(vtel : VTel m w tel) → TelProgress tel
  step : {ts ts' : Tel w m} → ts —→T ts' → TelProgress ts
  error : {ts : Tel w m} → Any Error ts → TelProgress ts

\end{code}

\begin{code}
progress·V : ∀{n}{i : Weirdℕ n}
  → {t : ScopedTm i} → Value t
  → {u : ScopedTm i} → Progress u
  → Progress (t · u)
progress·V v                     (step q)            = step (ξ-·₂ v q)
progress·V v                     (error (E-error A)) = step (E-·₂ v)
progress·V (V-ƛ A t)             (done v)            = step (β-ƛ v)
progress·V (V-Λ p)               (done v)            = step E-Λ·
progress·V (V-con tcn)           (done v)            = step E-con·
progress·V (V-wrap A B t)        (done v)            = step E-wrap·
progress·V (V-builtin⋆ b t)   (done v)            =
  {!!} --  step (E-builtin⋆· b As q _)
progress·V (V-builtin b t p q base σ) (done v) =
  step (β-builtin b t (deval v) p q σ)
progress·V (V-builtin b t p q (skipT r) σ) (done v) = done {!V-built!}
progress·V (V-builtin b t p q (skipS r) σ) (done v) = done (V-builtin b (t · deval v) p q r (sub-cons σ (deval v)))

progress· : ∀{n}{i : Weirdℕ n}
  → {t : ScopedTm i} → Progress t
  → {u : ScopedTm i} → Progress u
  → Progress (t · u)
progress· (done v)            q = progress·V v q
progress· (step p)            q = step (ξ-·₁ p)
progress· (error (E-error A)) q = step E-·₁

progress·⋆ : ∀{n}{i : Weirdℕ n}{t : ScopedTm i}
  → Progress t → (A : ScopedTy n) → Progress (t ·⋆ A)
progress·⋆ (step p)                     A = step (ξ-·⋆ p)
progress·⋆ (done (V-ƛ B t))             A = step E-ƛ·⋆
progress·⋆ (done (V-Λ p))               A = step β-Λ
progress·⋆ (done (V-con tcn))           A = step E-con·⋆
progress·⋆ (done (V-wrap pat arg t))    A = step E-wrap·⋆
progress·⋆ (done (V-builtin⋆ b t))   A = {!!} -- step sat⋆-builtin
progress·⋆ (done (V-builtin b t p q r s)) A = {!!} -- step E-builtin·⋆

progress·⋆ (error (E-error A))          B = step E-·⋆

progress-unwrap : ∀{n}{i : Weirdℕ n}{t : ScopedTm i}
  → Progress t → Progress (unwrap t)
progress-unwrap (step p)                     = step (ξ-unwrap p)
progress-unwrap (done (V-ƛ A t))             = step E-ƛunwrap
progress-unwrap (done (V-Λ p))               = step E-Λunwrap
progress-unwrap (done (V-con tcn))           = step E-conunwrap
progress-unwrap (done (V-wrap A B v))        = step (β-wrap v)
progress-unwrap (done (V-builtin b t p q r s)) = {!!} -- step E-builtinunwrap
progress-unwrap (done (V-builtin⋆ b t))   = {!!} -- step E-builtin⋆unwrap
progress-unwrap (error (E-error A))          = step E-unwrap

progress : (t : ScopedTm Z) → Progress t

progress (Λ K t)           = done (V-Λ t)
progress (t ·⋆ A)          = progress·⋆ (progress t) A
progress (ƛ A t)           = done (V-ƛ A t)
progress (t · u)           = progress· (progress t) (progress u)
progress (con c)           = done (V-con c)
progress (error A)         = error (E-error A)
-- type telescope is full
progress (ibuiltin b) = {!!}
progress (wrap A B t) with progress t
progress (wrap A B t)          | step  q           = step (ξ-wrap q)
progress (wrap A B t)          | done  q           = done (V-wrap A B q)
progress (wrap A B .(error C)) | error (E-error C) = step E-wrap
progress (unwrap t)        = progress-unwrap (progress t)
\end{code}

\begin{code}
open import Data.Nat

Steps : ScopedTm Z → Set
Steps t = Σ (ScopedTm Z) λ t' → t —→⋆ t' × (Maybe (Value t') ⊎ Error t')

run—→ : {t t' : ScopedTm Z} → t —→ t' → Steps t' → Steps t
run—→ p (t' , ps , q) = _ , ((trans p ps) , q)

run : (t : ScopedTm Z) → ℕ → Steps t
runProg : ℕ → {t : ScopedTm Z} → Progress t → Steps t

run t 0       = t , (refl , inl nothing) -- out of fuel
run t (suc n) = runProg n (progress t)

runProg n (step {t' = t'} p)  = run—→ p (run t' n)
runProg n (done V)  = _ , refl , inl (just V)
runProg n (error e) = _ , refl , inr e
\end{code}
