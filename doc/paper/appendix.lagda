\begin{comment}
\begin{code}
infixl 1 _\\
_\\ : ∀ {ℓ} {A : Set(ℓ)} → A → A
x \\ = x

infixl 1 _\\\quad
_\\\quad : ∀ {ℓ} {A : Set(ℓ)} → A → A
x \\\quad = x
\end{code}
\end{comment}

\begin{comment}
\begin{code}
data Bool : Set where
  true false : Bool
{-# BUILTIN BOOL Bool #-}
{-# BUILTIN TRUE true #-}
{-# BUILTIN FALSE false #-}

postulate /String/ : Set
{-# BUILTIN STRING  /String/ #-}

private
 primitive
  primStringAppend   : /String/ → /String/ → /String/
  primStringEquality : /String/ → /String/ → Bool

data /Nat/ : Set where
  zero : /Nat/
  1+  : /Nat/ -> /Nat/
{-# BUILTIN NATURAL /Nat/  #-}

infixr 13 _/equals/_
data _/equals/_ {a} {A : Set a} (x : A) : A → Set a where
  refl : (x /equals/ x)
{-# BUILTIN EQUALITY _/equals/_  #-}
{-# BUILTIN REFL refl #-}

private
 primitive
  primTrustMe : ∀ {a} {A : Set a} {x y : A} → x /equals/ y

infixr 3 _+_
_+_ : /Nat/ → /Nat/ → /Nat/
zero + y = y
1+ x + y = 1+ (x + y)

_⊔_ : /Nat/ → /Nat/ → /Nat/
zero ⊔ y = y
x ⊔ zero = x
1+ x ⊔ 1+ y = 1+ (x ⊔ y)

data ⊥ : Set where

_/neq/_ : ∀ {A : Set} → A → A → Set
(x /neq/ y) = (x /equals/ y) → ⊥

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
  /lift/_ : A → /Lift/ A

/emptyset/ : ∀ {A B : Set} → A → /Lift/(B) 
/emptyset/(x) = /bot/

_/override/_ : ∀ {A B : Set} → (A → /Lift/(B)) → (A → /Lift/(B)) → (A → /Lift/(B))
(f /override/ g)(x) with f(x)
... | /bot/ = g(x)
... | /lift/(y) = /lift/(y)

_=S?_ : (x y : /String/) → /Lift/(x /equals/ y)
x =S? y with primStringEquality x y
... | true = /lift/ primTrustMe
... | false = /bot/

_=ℕ?_ : (x y : /Nat/) → /Lift/(x /equals/ y)
zero =ℕ? zero = /lift/ refl
zero =ℕ? 1+ y = /bot/
1+ x =ℕ? zero = /bot/
1+ x =ℕ? 1+ y  with x =ℕ? y
1+ x =ℕ? 1+ .x | /lift/(refl) = /lift/(refl) 
1+ x =ℕ? 1+ y  | /bot/ = /bot/

infixr 5 _/cons/_
data /List/ (A : Set) : Set where
  /epsilon/ : /List/(A)
  _/cons/_ : A → /List/(A) → /List/(A)

/singleton/ : ∀ {A} → A → /List/(A)
/singleton/ x = (x /cons/ /epsilon/)

/length/ : ∀ {A} → /List/(A) → /Nat/
/length/ /epsilon/ = 0
/length/ (x /cons/ xs) = 1 + /length/(xs)

data _/member/_ {A : Set} (x : A) : /List/(A) → Set where
  here : ∀ {xs} → (x /member/ (x /cons/ xs))
  there : ∀ {y xs} → (x /member/ xs) → (x /member/ (y /cons/ xs))

max : /List/(/Nat/) → (/Nat/ → /Nat/) → /Nat/
max /epsilon/ f = 0
max (x /cons/ xs) f = f(x) ⊔ max xs f
syntax max cs (λ c → N) = /max/ c /in/ cs /st/ N

infixr 3 _/append/_
_/append/_ : ∀ {A} → /List/(A) → /List/(A) → /List/(A)
/epsilon/ /append/ ys = ys
(x /cons/ xs) /append/ ys = (x /cons/ (xs /append/ ys))
\end{code}
\end{comment}

\section{Formalization}

\begin{code}
/Word/ = /Nat/ \\
/Val/ = /List/(/Word/) \\
/Memory/ = (/Word/ /fun/ /Lift/(/Word/))
\end{code}

\begin{comment}
\begin{code}
/Var/ = /String/

data /Lifetime/ : Set where
  /always/ : /Lifetime/
  /never/ : /Lifetime/
  /var/_ : /Word/ → /Lifetime/

data /Type/ : Set where
  /unit/ : /Type/
  _,_  : /Type/ → /Type/ → /Type/
  /enum/_/st/_ : /List/(/Word/) → (/Word/ → /Type/) → /Type/
  /reft/_/after/_/of/_ : /Lifetime/ → /Lifetime/ → /Type/ → /Type/

data /Pat/ : Set where
  /varref/_/in/_ : /Var/ → /Type/ → /Pat/
  /varref//mut/_/in/_ : /Var/ → /Type/ → /Pat/
  /var/_/in/_ : /Var/ → /Type/ → /Pat/
  /addr/_ : /Pat/ → /Pat/
  /tagged/_/with/ : /Type/ → /Word/ → /Pat/ → /Pat/
  /unit/ : /Pat/
  _,_ : /Pat/ → /Pat/ → /Pat/
  /Box/_ : /Pat/ → /Pat/ 

data /Exp/ : Set where
  /var/_ : /Var/ → /Exp/
  /val/_ : /Val/ → /Exp/
  /tagged/_/with/ : /Type/ → /Word/ → /Exp/ → /Exp/
  /unit/ : /Exp/
  _,_ : /Exp/ → /Exp/ → /Exp/
  /lett/_/equals/_/semi/_ : /Pat/ → /Exp/ → /Exp/ → /Exp/
  /Box/_ : /Exp/ → /Exp/ 
  /set/_/equals/_ : /Exp/ → /Exp/ → /Exp/
  _/with//drop/_ : /Type/ → /Exp/ → /Exp/
  /forget/_ : /Exp/ → /Exp/
infixr 8 /val/_
infixr 8 /lett/_/equals/_/semi/_
infixr 8 /set/_/equals/_
infixr 8 _/with//drop/_
infixr 8 /forget/_
infixr 8 /Box/_
infixr 8 /addr/_

_[_:=_] : /Exp/ → /Var/ → /Val/ → /Exp/
_[_:=_] = {!!}

_/oplus/_/mapsto/_ : /Memory/ → /Word/ → /Val/ → /Lift/(/Memory/)
(ρ /oplus/ p /mapsto/ V) = {!!}
\end{code}
\end{comment}

\begin{comment}
\begin{code}
/sizet/ : /Type/ /fun/ /Nat/ \\
\end{code}
\end{comment}
\begin{code}
/sizet/ (/unit/) = 0 \\
/sizet/ (T , U) = /sizet/(T) + /sizet/(U) \\
/sizet/ (/enum/ /vec/c /st/ T) = 1 + (/max/ c /in/ /vec/c /st/ /sizet/ (T(c))) \\
/sizet/ (/reft/ /alpha/ /after/ /beta/ /of/ T) = 1
\end{code}

\begin{comment}
\begin{code}
/sizep/ : /Pat/ /fun/ /Nat/ \\
\end{code}
\end{comment}
\begin{code}
/sizep/ (/var/ x /in/ T) = /sizet/(T) \\
/sizep/ (/varref/ x /in/ T) = /sizet/(T) \\
/sizep/ (/varref//mut/ x /in/ T) = /sizet/(T) \\
/sizep/ (/unit/) = 0 \\
/sizep/ (X , Y) = /sizep/(X) + /sizep/(Y) \\
/sizep/ (/tagged/ T /with/ c(X)) = /sizet/(T) \\
/sizep/ (/addr/ X) = 1
/sizep/ (/Box/ X) = 1
\end{code}

\begin{comment}
\begin{code}
/Boxt/ : /Type/ → /Type/
/reft/_/of/_ : /Word/ → /Type/ → /Type/
/reft/_/mut//of/_ : /Word/ → /Type/ → /Type/
\end{code}
\end{comment}
\begin{code}
/Boxt/ T = /reft/ /always/ /after/ /never/ /of/ T \\
/reft/ /alpha/ /of/ T = /reft/ /var/ /alpha/ /after/ /always/ /of/ T \\
/reft/ /alpha/ /mut//of/ T = /reft/ /var/ /alpha/ /after/ /never/ /of/ T
\end{code}

\section{Operational semantics}

\begin{comment}
\begin{code}
data _/step/_ : (/Memory/ /times/ /Exp/) → (/Memory/ /times/ /Exp/) → Set where
\end{code}
\end{comment}

\subsection{Values}

\begin{code}
  %val-unit : ∀ /rho/ →
    (/rho/ , /unit/) /step/ (/rho/ , /val/ /epsilon/) \\
  %val-pair : ∀ /rho/ V W →
    (/rho/ , (/val/ V , /val/ W)) /step/ (/rho/ , /val/ (V /append/ W)) \\
  %val-tagged : ∀ /rho/ T c V W →
    (/rho/ , /tagged/ T /with/ c(/val/ V)) /step/ (/rho/ , /val/ (c /cons/ V /append/ W)) /where/
    /sizet/(T) /equals/ /length/(c /cons/ V /append/ W) \\
\end{code}

\subsection{Pattern matching}

\begin{code}
  %let-var : ∀ /rho/ x T V M →
    (/rho/ , /lett/ /var/ x /in/ T /equals/ /val/ V /semi/ M) /step/ (/rho/ , M [ x := V ]) \\
  %let-unit : ∀ /rho/ M →
    (/rho/ , /lett/ /unit/ /equals/ /val/ /epsilon/ /semi/ M) /step/ (/rho/ , M) \\
  %let-pair : ∀ /rho/ X Y V W M →
    (/rho/ , /lett/ (X , Y) /equals/ /val/ (V /append/ W) /semi/ M) /step/ (/rho/ , /lett/ X /equals/ /val/ V /semi/ /lett/ Y /equals/ /val/ W /semi/ M) /where/
    /sizep/(X) /equals/ /length/(V) \\
  %let-tagged : ∀ /rho/ T c X M V W →
    (/rho/ , /lett/ /tagged/ T /with/ c(X) /equals/ /val/ (c /cons/ V /append/ W) /semi/ M) /step/ (/rho/ , /lett/ X /equals/ /val/ V /semi/ M) /where/
    /sizep/(X) /equals/ /length/(V) \\
\end{code}

\begin{code}
  %let-ctxt : ∀ /rho/ X M N /rho//p/ M/p/ →
    (/rho/ , /lett/ X /equals/ M /semi/ N) /step/ (/rho//p/ , /lett/ X /equals/ M/p/ /semi/ N) /where/
    (/rho/ , M) /step/ (/rho//p/ , M/p/)
\end{code}

\subsection{References}

\begin{code}
  %let-varref : ∀ /rho/ /rhoP/ x T V M p →
    (/rho/ , /lett/ /varref/ x /in/ T /equals/ /val/ V /semi/ M) /step/ (/rhoP/ , M [ x := /singleton/ p ]) /where/
    /lift/ /rhoP/ /equals/ (/rho/ /oplus/ p /mapsto/ V) \\
  %let*-var : ∀ /rho/ /rhoP/ x T p V M →
    (/rho/ , /lett/ /addr/ /var/ x /in/ T /equals/ /val/ /singleton/ p /semi/ M) /step/ (/rho/ , M [ x := V ]) /where/
    /lift/ /rho/ /equals/ /rhoP/ /oplus/ p /mapsto/ V /andalso/
    /sizet/(T) /equals/ /length/(V) \\
  %let*-varref : ∀ /rho/ x T p M →
    (/rho/ , /lett/ /addr/ /varref/ x /in/ T /equals/ /val/ /singleton/ p /semi/ M) /step/ (/rho/ , M [ x := /singleton/ p ]) \\
  %let*-unit : ∀ /rho/ p M →
    (/rho/ , /lett/ /addr/ /unit/ /equals/ /val/ /singleton/ p /semi/ M) /step/ (/rho/ , M) \\
  %let*-pair : ∀ /rho/ X Y p M n →
    (/rho/ , /lett/ /addr/ (X , Y) /equals/ /val/ /singleton/ p /semi/ M) /step/ (/rho/ , /lett/ X /equals/ /val/ /singleton/ p /semi/ /lett/ Y /equals/ /val/ /singleton/ (n + p) /semi/ M) /where/
    n /equals/ /sizep/(X) \\
  %let*-tagged : ∀ /rho/ T c X p M →
    (/rho/ , /lett/ /addr/ /tagged/ T /with/ c(X) /equals/ /val/ /singleton/ p /semi/ M) /step/ (/rho/ , /lett/ /addr/ X /equals/ /val/ /singleton/ (1 + p) /semi/ M) /where/
    /lift/ c /equals/ /rho/(p) \\
  %let** : ∀ /rho/ X p q M →
    (/rho/ , /lett/ /addr/ /addr/ X /equals/ /val/ /singleton/ p /semi/ M) /step/ (/rho/ , /lett/ /addr/ X /equals/ /val/ /singleton/ q /semi/ M) /where/
    /lift/ q /equals/ /rho/(p)
\end{code}

\subsection{Mutable references}

\begin{code}
  %let-varref-mut : ∀ /rho/ /rhoP/ x T V M p →
    (/rho/ , /lett/ /varref//mut/ x /in/ T /equals/ /val/ V /semi/ M) /step/ (/rhoP/ , M [ x := /singleton/ p ]) /where/
    /lift/ /rhoP/ /equals/ (/rho/ /oplus/ p /mapsto/ V) \\
  %let*-varref-mut : ∀ /rho/ x T p M →
    (/rho/ , /lett/ /addr/ /varref//mut/ x /in/ T /equals/ /val/ /singleton/ p /semi/ M) /step/ (/rho/ , M [ x := /singleton/ p ]) \\
  %set : ∀ /rho/ /rho//p/ /rho//pp/ p V W →
    (/rho/ , /set/ /val/ /singleton/ p /equals/ /val/ V) /step/ (/rho//p/ , /val/ W) /where/
    /lift/ /rho/ /equals/ (/rho//pp/ /oplus/ p /mapsto/ W) /andalso/
    /lift/ /rho//p/ /equals/ (/rho//pp/ /oplus/ p /mapsto/ V) /andalso/
    /length/(V) /equals/ /length/(W)
\end{code}

\subsection{Owned references}

\begin{code}
  %box : ∀ /rho/ /rhoP/ V p →
    (/rho/ , /Box/ /val/ V) /step/ (/rhoP/ , /val/ /singleton/ p) /where/
    /lift/ /rhoP/ /equals/ (/rho/ /oplus/ p /mapsto/ V) \\
  %let-box : ∀ /rho/ /rhoP/ X T p V M →
    (/rho/ , /lett/ /Box/ X /equals/ /val/ /singleton/ p /semi/ M) /step/ (/rhoP/ , /lett/ X /equals/ /val/ V /semi/ M) /where/
    /lift/ /rho/ /equals/ /rhoP/ /oplus/ p /mapsto/ V /andalso/
    /sizet/(T) /equals/ /length/(V) \\
  %let*-box : ∀ /rho/ X p q M →
    (/rho/ , /lett/ /addr/ /Box/ X /equals/ /val/ /singleton/ p /semi/ M) /step/ (/rho/ , /lett/ /addr/ X /equals/ /val/ /singleton/ q /semi/ M) /where/
    /lift/ q /equals/ /rho/(p) \\
\end{code}

\subsection{Discard}

\begin{code}
  %forget : ∀ /rho/ V →
    (/rho/ , /forget/ /val/(V)) /step/ (/rho/ , /val/ /epsilon/) \\
\end{code}

\endinput



-- \begin{comment}
-- \begin{code}
-- _/comma/_ : ∀ {A} → /Lift/(A) → /Lift/(/List/(A)) → /Lift/(/List/(A))
-- /bot/ /comma/ _ = /bot/
-- /lift/ x /comma/ /bot/ = /bot/
-- /lift/ x /comma/ /lift/ xs = /lift/ (x , xs)

-- /sem//slice/_[_/dots/_]/mes/ : /Word/ → /Word/ → /Word/ → /Memory/ → /Lift/(/Value/)
-- \end{code}
-- \end{comment}

-- \begin{code}
-- /sem//slice/ p [ x /dots/ 0 ]/mes/ /rho/ = /lift/ /unit/ \\
-- /sem//slice/ p [ 0 /dots/ (1+ y) ]/mes/ /rho/ = (/rho/(p) /comma/ /sem//slice/ (1+ p) [ 0 /dots/ y ]/mes/ /rho/) \\
-- /sem//slice/ p [ (1+ x) /dots/ (1+ y) ]/mes/ /rho/ = /sem//slice/ (1+ p) [ x /dots/ y ]/mes/ /rho/
-- \end{code}

-- \begin{comment}
-- \begin{code}
-- _/oplus/_/mapsto/_ : /Memory/ → /Word/ → /Value/ → /Lift/(/Memory/)
-- (ρ /oplus/ p /mapsto/ /unit/) = /lift/(ρ)
-- (ρ /oplus/ p /mapsto/ (v , V)) with ρ(p) | (ρ /oplus/ (1+ p) /mapsto/ V)
-- ... | /bot/ | /lift/(ρ′) = /lift/ ρ″ where
--   ρ″ : /Memory/
--   ρ″(q)  with p =ℕ? q
--   ρ″(.p) | /lift/(refl) = /lift/(v)
--   ρ″(q)  | /bot/ = ρ′(q)
-- ... | _     | _          = /bot/

-- _[_:=_] : /Exp/ → /Var/ → /Value/ → /Exp/
-- _[_:=_] = {!!}

-- \end{code}
-- \end{comment}

-- \begin{comment}
-- \begin{code}
-- data _/step/_ : (/Memory/ /times/ /Exp/) → (/Memory/ /times/ /Exp/) → Set where
-- \end{code}
-- \end{comment}

-- \begin{code}
--   %iflet*-addr-var : ∀ /rho/ x T p V M N n →
--     (/rho/ , /iflet/ /var/ x /in/ T /equals//ast/ /val/ /singleton/ p /thn/ M /els/ N /telfi/) /step/ (/rho/ , M [ x := V ]) /where/
--     /sem//slice/ p [ 0 /dots/ n ]/mes/ /rho/ /equals/ /lift/ V /andalso/
--     n /equals/ /sizeof/(T) \\
--   %iflet*-ref : ∀ /rho/ x M N p → 
--     (/rho/ , /iflet/ /varref/ x /equals//ast/ /val/ /singleton/ p /thn/ M /els/ N /telfi/) /step/ (/rho/ , M [ x := /singleton/ p ]) \\
--   %iflet*-tagged : ∀ /rho/ T c X M N p → 
--     (/rho/ , /iflet/ /tagged/ T /with/ c(X) /equals//ast/ /val/ /singleton/ p /thn/ M /els/ N /telfi/) /step/ (/rho/ , /iflet/ X /equals//ast/ /val/ /singleton/ (1+ p) /thn/ M /els/ N /telfi/) \\
--   %iflet*-tagged-fail : ∀ /rho/ T c X M N p → 
--     (/rho/ , /iflet/ /tagged/ T /with/ c(X) /equals//ast/ /val/ /singleton/ p /thn/ M /els/ N /telfi/) /step/ (/rho/ , N)  /where/
--     /lift/ c /neq/ /rho/(p) \\
--   %iflet*-addr-unit : ∀ /rho/ p M N →
--     (/rho/ , /iflet/ /unit/ /equals//ast/ /val/ /singleton/ p /thn/ M /els/ N /telfi/) /step/ (/rho/ , M) \\
--   %iflet*-addr : ∀ /rho/ X M N p q → 
--     (/rho/ , /iflet/ /addr/ X /equals//ast/ /val/ /singleton/ p /thn/ M /els/ N /telfi/) /step/ (/rho/ , /iflet/ X /equals//ast/ /val/ /singleton/ q /thn/ M /els/ N /telfi/) /where/
--     /lift/ q /equals/ /rho/(p)
-- \end{code}


-- \begin{code}
--   %iflet-alloc : ∀ /rho/ X V M N /rhoP/ p →
--     (/rho/ , /iflet/ X /equals/ /val/ V /thn/ M /els/ N /telfi/) /step/ (/rhoP/ , /iflet/ X /equals//ast/ /val/ /singleton/ p /thn/ M /els/ N /telfi/) /where/
--     /lift/ /rhoP/ /equals/ (/rho/ /oplus/ p /mapsto/ V)
-- \end{code}

-- \begin{code}
--   %iflet-redn : ∀ /rho/ X L M N /rhoP/ /LP/ →
--     (/rho/ , /iflet/ X /equals/ L /thn/ M /els/ N /telfi/) /step/ (/rhoP/ , /iflet/ X /equals/ /LP/ /thn/ M /els/ N /telfi/) /where/
--     (/rho/ , L) /step/ (/rhoP/ , /LP/) \\
--   %iflet*-redn : ∀ /rho/ X L M N /rhoP/ /LP/ →
--     (/rho/ , /iflet/ X /equals//ast/ L /thn/ M /els/ N /telfi/) /step/ (/rhoP/ , /iflet/ X /equals//ast/ /LP/ /thn/ M /els/ N /telfi/) /where/
--     (/rho/ , L) /step/ (/rhoP/ , /LP/) \\
-- \end{code}

-- \begin{comment}
-- \begin{code}
-- /Subst/ = /Var/ → /Lift/ /Slice/
-- _/mapsto/_ : /Var/ → /Slice/ → /Subst/
-- (x /mapsto/ S) y  with x =S? y
-- (x /mapsto/ S) y  | /bot/ = /bot/
-- (x /mapsto/ S) .x | /lift/ refl = /lift/ S
-- \end{code}
-- \end{comment}

-- \begin{comment}
-- \begin{code}
-- data _/bigSubst/_ : (/Memory/ /times/ /Pat/ /times/ /Value/) → (/Memory/ /times/ /Subst/) → Set where
-- \end{code}
-- \end{comment}

-- \begin{code}
--   %var : ∀ /rho/ x T V p n →
--     (/rho/ , /var/ x /in/ T , V) /bigSubst/ (/rho/ , (x /mapsto/ /slice/ p [ 0 /dots/ n ])) /where/
--     n /equals/ /sizeof/(T) \\
--   %tagged : ∀ /sigma/ /rho/ /rhoP/ X T c V W /vec/c U →
--     (/rho/ , X , V) /bigSubst/ (/rhoP/ , /sigma/) /implies/
--     (/rho/ , /tagged/ T /with/ c(X) , (c , V /append/ W)) /bigSubst/ (/rhoP/ , /sigma/) /where/
--     T /equals/ /enum/ /vec/c /st/ U /andalso/
--     /length/(V) /equals/ /sizeof/(U(c))
-- \end{code}

-- \begin{comment}
-- \begin{code}
-- data _/bigStep/_ : (/Subst/ /times/ /Memory/ /times/ /Exp/) → (/Memory/ /times/ /Value/) → Set where
-- \end{code}
-- \end{comment}

-- \begin{code}
--   %var : ∀  /sigma/ /rho/ x V p i j →
--     (/sigma/ , /rho/ , /var/ x) /bigStep/ (/rho/ , V) /where/
--     /sigma/(x) /equals/ /lift/ /slice/ p [ i /dots/ j ]  /andalso/
--     /sem//slice/ p [ i /dots/ j ]/mes/ /rho/ /equals/ /lift/ V \\
--   %tagged : ∀ /sigma/ /rho/ /rhoP/ T c M V W →
--     (/sigma/ , /rho/ , M) /bigStep/ (/rhoP/ , V) /implies/
--     (/sigma/ , /rho/ , /tagged/ T /with/ c(M)) /bigStep/ (/rhoP/ , (c , V /append/ W)) /where/
--     /length/(c , V /append/ W) /equals/ /sizeof/(T) \\
--   %ifletT : ∀ /sigma/ /rho/ X L M N /sigmaP/ /rhoP/ /rhoPP/ /rhoPPP/ V W →
--     (/sigma/ , /rho/ , L) /bigStep/ (/rhoP/ , V) /andalso/
--     ((/sigma/ /override/ /sigmaP/) , /rhoPP/ , M) /bigStep/ (/rhoPPP/ , W) /implies/
--     (/sigma/ , /rho/ , /iflet/ X /equals/ L /thn/ M /els/ N /telfi/) /bigStep/ (/rhoPPP/ , W) /where/
--     (/rhoP/ , X , V) /bigSubst/ (/rhoPP/ , /sigmaP/) \\
--   %ifletF : ∀ /sigma/ /rho/ X L M N /rhoP/ /rhoPP/ V W →
--     (/sigma/ , /rho/ , L) /bigStep/ (/rhoP/ , V) /andalso/
--     (/sigma/ , /rhoP/ , N) /bigStep/ (/rhoPP/ , W) /implies/
--     (/sigma/ , /rho/ , /iflet/ X /equals/ L /thn/ M /els/ N /telfi/) /bigStep/ (/rhoPP/ , W)
-- \end{code}

-- \begin{code}
-- /SType/ = (/Var/ /fun/ /Lift/(/Type/)) \\
-- /MType/ = (/Word/ /fun/ /Lift/(/Type/)) \\
-- \end{code}

-- \begin{comment}
-- \begin{code}
-- data _/dashv/_/in/_ : /SType/ → /Pat/ → /Type/ → Set where
-- \end{code}
-- \end{comment}

-- \begin{comment}
-- \begin{code}
-- data _/vdash/_/in/_ : /SType/ → /Exp/ → /Type/ → Set where
-- \end{code}
-- \end{comment}

-- \begin{code}
--   %tagged : ∀ /Gamma/ T c M /vec/c U →
--     /Gamma/ /vdash/ M /in/ U(c) /implies/
--     /Gamma/ /vdash/ /tagged/ T /with/ c(M) /in/ T /where/
--     T /equals/ (/enum/ /vec/c /st/ U) /andalso/
--     c /member/ /vec/c \\
--   %iflet : ∀ /Gamma/ /GammaP/ X L M N T U →
--     /Gamma/ /vdash/ L /in/ U /andalso/
--     (/Gamma/ /override/ /GammaP/) /vdash/ M /in/ T /andalso/
--     /Gamma/ /vdash/ N /in/ T /implies/
--     /Gamma/ /vdash/ /iflet/ X /equals/ L /thn/ M /els/ N /telfi/ /in/ T /where/
--     /GammaP/ /dashv/ X /in/ U
-- \end{code}

-- \begin{comment}
-- \begin{code}
-- data _/vdashv/_/in/_ : /MType/ → /Value/ → /Type/ → Set where
-- \end{code}
-- \end{comment}

-- \begin{code}
--   %tagged : ∀ /Delta/ T c V W /vec/c U →
--     /Delta/ /vdashv/ V /in/ U(c) /implies/
--     /Delta/ /vdashv/ (c , V /append/ W) /in/ T /where/
--     T /equals/ (/enum/ /vec/c /st/ U) /andalso/
--     c /member/ /vec/c \\
-- \end{code}

-- \begin{code}
-- %safety : ∀ /Gamma/ (/Delta/ : /MType/) {M} {T} /sigma/ /rho/ /rhoP/ V →
--   (/Gamma/ /vdash/ M /in/ T) /andalso/
--   (/sigma/ , /rho/ , M) /bigStep/ (/rhoP/ , V) /implies/
--   (/exists/ /DeltaP/ /st/ (/DeltaP/ /vdashv/ V /in/ T))
-- \end{code}

-- \begin{comment}
-- \begin{code}
-- %safety Γ Δ σ ρ ρ′ ._ (%tagged .Γ .(/enum/ cs /st/ U) c M cs U (refl , c∈cs) M∈Uc , %tagged .σ .ρ .ρ′ .(/enum/ cs /st/ U) .c .M V W _ M⇓V)
--   with %safety Γ Δ σ ρ ρ′ V (M∈Uc , M⇓V)
-- ... | (Δ′ , V∈T)
--   = (Δ′ , %tagged Δ′ (/enum/ cs /st/ U) c V W cs U (refl , c∈cs) V∈T)
-- %safety Γ Δ σ ρ ρ‴ V (%iflet .Γ Γ′ X L M N T U X∈U (L∈U , M∈T , N∈T) , %ifletT .σ .ρ .X .L .M .N σ′ ρ′ ρ″ .ρ‴ W .V x (L⇓W , M⇓V))
--   = %safety (Γ /override/ Γ′) Δ (σ /override/ σ′) ρ″ ρ‴ V (M∈T , M⇓V)
-- %safety Γ Δ σ ρ ρ″ V (%iflet .Γ Γ′ X L M N T U X∈U (L∈U , M∈T , N∈T) , %ifletF .σ .ρ .X .L .M .N ρ′ .ρ″ W .V (L⇓W , N⇓V))
--   = %safety Γ Δ σ ρ′ ρ″ V (N∈T , N⇓V)
-- \end{code}
-- \end{comment}
