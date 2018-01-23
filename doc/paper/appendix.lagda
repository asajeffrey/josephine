section{Appendix}

[Appendix goes here]

\begin{comment}

\begin{code}
postulate String : Set
{-# BUILTIN STRING  String #-}

data /Nat/ : Set where
  zero : /Nat/
  suc  : /Nat/ -> /Nat/
{-# BUILTIN NATURAL /Nat/  #-}

infixr 3 _+_
_+_ : /Nat/ → /Nat/ → /Nat/
zero + y = y
suc x + y = suc (x + y)

_⊔_ : /Nat/ → /Nat/ → /Nat/
zero ⊔ y = y
x ⊔ zero = x
suc x ⊔ suc y = suc (x ⊔ y)

data _/equals/_ {A : Set} (x : A) : A → Set where
  refl : (x /equals/ x)

data ⊥ : Set where

infixr 5 _,_
record Σ (A : Set) (B : A → Set) : Set where
  constructor _,_
  field /fst/ : A
  field /snd/ : B /fst/
syntax Σ A (λ x → M) = /exists/ x /in/ A /st/ M

∃ : {A : Set} → (A → Set) → Set
∃ {A} B = Σ A B
syntax ∃ (λ x → M) = /exists/ x /st/ M
infixr 10 ∃

∄ : {A : Set} → (A → Set) → Set
∄ P = (∃ P) → ⊥
syntax ∄ (λ x → M) = /nexists/ x /st/ M
infixr 10 ∄

infixr 5 _/times/_
_/times/_ : Set → Set → Set
(A /times/ B) = Σ A (λ _ → B)

infixr 9 _/andalso/_
_/andalso/_ = _/times/_

_/fun/_ : Set → Set → Set
(A /fun/ B) = A → B

infixr 8 _/implies/_
_/implies/_ : Set → Set → Set
(A /implies/ B) = A → B

infixl 7 _/where/_
_/where/_ : Set → Set → Set
(A /where/ B) = B → A

infixl 7 _/wherenot/_
_/wherenot/_ : Set → Set → Set
(A /wherenot/ B) = (A /where/ (B → ⊥))

data /Lift/ (A : Set) : Set where
  /bot/ : /Lift/ A
  /lift/ : A → /Lift/ A

/emptyset/ : ∀ {A B : Set} → A → /Lift/(B) 
/emptyset/(x) = /bot/

_/override/_ : ∀ {A B : Set} → (A → /Lift/(B)) → (A → /Lift/(B)) → (A → /Lift/(B))
(f /override/ g)(x) with f(x)
... | /bot/ = g(x)
... | /lift/(y) = /lift/(y)

data /List/ (A : Set) : Set where
  /epsilon/ : /List/(A)
  _,_ : A → /List/(A) → /List/(A)

/singleton/ : ∀ {A} → A → /List/(A)
/singleton/ x = (x , /epsilon/)

/length/ : ∀ {A} → /List/(A) → /Nat/
/length/ /epsilon/ = 0
/length/ (x , xs) = 1 + /length/(xs)

data _/member/_ {A : Set} (x : A) : /List/(A) → Set where
  here : ∀ {xs} → (x /member/ (x , xs))
  there : ∀ {y xs} → (x /member/ xs) → (x /member/ (y , xs))

max : /List/(/Nat/) → (/Nat/ → /Nat/) → /Nat/
max /epsilon/ f = 0
max (x , xs) f = f(x) ⊔ max xs f
syntax max cs (λ c → N) = /max/ c /in/ cs /st/ N

infixr 3 _/append/_
_/append/_ : ∀ {A} → /List/(A) → /List/(A) → /List/(A)
/epsilon/ /append/ ys = ys
(x , xs) /append/ ys = (x , (xs /append/ ys))
\end{code}

\begin{code}
/Word/ : Set
/Word/ = /Nat/

data /Var/ : Set where
  /name/ : String → /Var/

data /Type/ : Set where
  /enum/_/st/_ : /List/(/Word/) → (/Word/ → /Type/) → /Type/

data /Pat/ : Set where
  /var/ : /Var/ → /Pat/
  /tagged/_/with/ : /Type/ → /Word/ → /Pat/ → /Pat/
  
data /Exp/ : Set where
  /var/ : /Var/ → /Exp/
  /tagged/_/with/ : /Type/ → /Word/ → /Exp/ → /Exp/
  /iflet/_/equals/_/thn/_/els/_/telfi/ : /Pat/ → /Exp/ → /Exp/ → /Exp/ → /Exp/
\end{code}

\begin{code}
_\\ : ∀ {ℓ} {A : Set(ℓ)} → A → A
x \\ = x
\end{code}

\end{comment}

\begin{code}
/Memory/ = (/Word/ /fun/ /Lift/(/Word/))
\end{code}

\begin{code}
/init/ : /Memory/ \\
/init/(p) = /bot/
\end{code}

\begin{code}
/sizeof/ : /Type/ /fun/ /Nat/ \\
/sizeof/ (/enum/ /vec/c /st/ T) = 1 + (/max/ c /in/ /vec/c /st/ /sizeof/(T(c)))
\end{code}

\begin{code}
/Value/ = /List/(/Word/) \\
/Subst/ = /Var/ /fun/ /Lift/(/Value/)
\end{code}

\begin{comment}
\begin{code}
_/mapsto/_ : /Var/ → /Value/ → /Subst/
(x /mapsto/ V) y = {!!}
\end{code}
\end{comment}

\begin{comment}
\begin{code}
data _/bigSubst/_ : (/Memory/ /times/ /Pat/ /times/ /Value/) → (/Memory/ /times/ /Subst/) → Set where
\end{code}
\end{comment}

\begin{code}
  %var : ∀ /rho/ x V →
    (/rho/ , /var/ x , V) /bigSubst/ (/rho/ , (x /mapsto/ V)) \\
  %tagged : ∀ /sigma/ /rho/ /rhoP/ X T c V W /vec/c U →
    (/rho/ , X , V) /bigSubst/ (/rhoP/ , /sigma/) /implies/
    (/rho/ , /tagged/ T /with/ c(X) , (c , V /append/ W)) /bigSubst/ (/rhoP/ , /sigma/) /where/
    T /equals/ /enum/ /vec/c /st/ U /andalso/
    /length/(V) /equals/ /sizeof/(U(c))
\end{code}

\begin{comment}
\begin{code}
data _/bigStep/_ : (/Subst/ /times/ /Memory/ /times/ /Exp/) → (/Memory/ /times/ /Value/) → Set where
\end{code}
\end{comment}

\begin{code}
  %tagged : ∀ /sigma/ /rho/ /rhoP/ T c M V W →
    (/sigma/ , /rho/ , M) /bigStep/ (/rhoP/ , V) /implies/
    (/sigma/ , /rho/ , /tagged/ T /with/ c(M)) /bigStep/ (/rhoP/ , (c , V /append/ W)) /where/
    /length/(c , V /append/ W) /equals/ /sizeof/(T) \\
  %ifletT : ∀ /sigma/ /rho/ X L M N /sigmaP/ /rhoP/ /rhoPP/ /rhoPPP/ V W →
    (/sigma/ , /rho/ , L) /bigStep/ (/rhoP/ , V) /andalso/
    ((/sigma/ /override/ /sigmaP/) , /rhoPP/ , M) /bigStep/ (/rhoPPP/ , W) /implies/
    (/sigma/ , /rho/ , /iflet/ X /equals/ L /thn/ M /els/ N /telfi/) /bigStep/ (/rhoPPP/ , W) /where/
    (/rhoP/ , X , V) /bigSubst/ (/rhoPP/ , /sigmaP/) \\
  %ifletF : ∀ /sigma/ /rho/ X L M N /rhoP/ /rhoPP/ V W →
    (/sigma/ , /rho/ , L) /bigStep/ (/rhoP/ , V) /andalso/
    (/sigma/ , /rhoP/ , N) /bigStep/ (/rhoPP/ , W) /implies/
    (/sigma/ , /rho/ , /iflet/ X /equals/ L /thn/ M /els/ N /telfi/) /bigStep/ (/rhoPP/ , W)
\end{code}

\begin{code}
/SType/ = (/Var/ /fun/ /Lift/(/Type/)) \\
/MType/ = (/Word/ /fun/ /Lift/(/Type/)) \\
\end{code}

\begin{comment}
\begin{code}
data _/dashv/_/in/_ : /SType/ → /Pat/ → /Type/ → Set where
\end{code}
\end{comment}

\begin{comment}
\begin{code}
data _/vdash/_/in/_ : /SType/ → /Exp/ → /Type/ → Set where
\end{code}
\end{comment}

\begin{code}
  %tagged : ∀ /Gamma/ T c M /vec/c U →
    /Gamma/ /vdash/ M /in/ U(c) /implies/
    /Gamma/ /vdash/ /tagged/ T /with/ c(M) /in/ T /where/
    T /equals/ (/enum/ /vec/c /st/ U) /andalso/
    c /member/ /vec/c \\
  %iflet : ∀ /Gamma/ /GammaP/ X L M N T U →
    /Gamma/ /vdash/ L /in/ U /andalso/
    (/Gamma/ /override/ /GammaP/) /vdash/ M /in/ T /andalso/
    /Gamma/ /vdash/ N /in/ T /implies/
    /Gamma/ /vdash/ /iflet/ X /equals/ L /thn/ M /els/ N /telfi/ /in/ T /where/
    /GammaP/ /dashv/ X /in/ U
\end{code}

\begin{comment}
\begin{code}
data _/vdashv/_/in/_ : /MType/ → /Value/ → /Type/ → Set where
\end{code}
\end{comment}

\begin{code}
  %tagged : ∀ /Delta/ T c V W /vec/c U →
    /Delta/ /vdashv/ V /in/ U(c) /implies/
    /Delta/ /vdashv/ (c , V /append/ W) /in/ T /where/
    T /equals/ (/enum/ /vec/c /st/ U) /andalso/
    c /member/ /vec/c \\
\end{code}

\begin{code}
%safety : ∀ /Gamma/ (/Delta/ : /MType/) {M} {T} /sigma/ /rho/ /rhoP/ V →
  (/Gamma/ /vdash/ M /in/ T) /andalso/
  (/sigma/ , /rho/ , M) /bigStep/ (/rhoP/ , V) /implies/
  (/exists/ /DeltaP/ /st/ (/DeltaP/ /vdashv/ V /in/ T))
\end{code}

\begin{comment}
\begin{code}
%safety Γ Δ σ ρ ρ′ ._ (%tagged .Γ .(/enum/ cs /st/ U) c M cs U (refl , c∈cs) M∈Uc , %tagged .σ .ρ .ρ′ .(/enum/ cs /st/ U) .c .M V W _ M⇓V)
  with %safety Γ Δ σ ρ ρ′ V (M∈Uc , M⇓V)
... | (Δ′ , V∈T)
  = (Δ′ , %tagged Δ′ (/enum/ cs /st/ U) c V W cs U (refl , c∈cs) V∈T)
%safety Γ Δ σ ρ ρ‴ V (%iflet .Γ Γ′ X L M N T U X∈U (L∈U , M∈T , N∈T) , %ifletT .σ .ρ .X .L .M .N σ′ ρ′ ρ″ .ρ‴ W .V x (L⇓W , M⇓V))
  = %safety (Γ /override/ Γ′) Δ (σ /override/ σ′) ρ″ ρ‴ V (M∈T , M⇓V)
%safety Γ Δ σ ρ ρ″ V (%iflet .Γ Γ′ X L M N T U X∈U (L∈U , M∈T , N∈T) , %ifletF .σ .ρ .X .L .M .N ρ′ .ρ″ W .V (L⇓W , N⇓V))
  = %safety Γ Δ σ ρ′ ρ″ V (N∈T , N⇓V)
\end{code}
\end{comment}
