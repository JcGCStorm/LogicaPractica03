module FormulasProposicionales where

import Operadores
import Data.List

-- Representa variables p, q, r, s...
type Atom = String

-- Representa las variables que se evalúan a True.
type State = [Atom]

newtype Tablita a b = Tablita (a, b)

-- data Prop
data Prop = Var Atom
          | Neg Prop
          | Conj Prop Prop
          | Disy Prop Prop
          | Impl Prop Prop
          | Syss Prop Prop

-- instance Show
instance Show Prop where
  show (Var p) = show p
  show (Neg p) = "¬(" ++ show p ++ ")"
  show (Conj p q) = "(" ++ show p ++ " ^ " ++ show q ++ ")"
  show (Disy p q) = "(" ++ show p ++ " v " ++ show q ++ ")"
  show (Impl p q) = "(" ++ show p ++ " -> " ++ show q ++ ")"
  show (Syss p q) = "(" ++ show p ++ " <-> " ++ show q ++ ")"

instance Operadores Prop where
  (¬) p = Neg p
  (\/) p p1 = Disy p p1
  (/\) p p1 = Conj p p1
  (-->) p p1 = Impl p p1
  (<-->) p p1 = Syss p p1

---------------------------------------------------------------------------------
--------                             FUNCIONES                           --------
---------------------------------------------------------------------------------

-- Funcion que dada una formula, regresa el conjunto de todos los
-- símbolos que aparecen en ella.
vars :: Prop -> [Atom]
vars (Var a) = [a]
vars (Neg a) = vars a
vars (Conj p p2) = vars p ++ vars p2
vars (Disy p p2) = vars p ++ vars p2
vars (Impl p p2) = vars p ++ vars p2
vars (Syss p p2) = vars p ++ vars p2

-- Funcion que evalua una proposicion dado un estado.
interp :: State -> Prop -> Bool
interp est (Var a) = contains a est
interp est (Neg a) = not (interp est a)
interp est (Conj p p2) = interp est p && interp est p2
interp est (Disy p p2) = interp est p || interp est p2
interp est (Impl p p2) = not (interp est p) || interp est p2
interp est (Syss p p2) = (not (interp est p) || interp est p2) && (not (interp est p) || interp est p2)

{-
State = ["p"]
Prop  = Conj (Var "p") (Var "q")
-}

-- Funcion que elimina las equivalencias (<->).
elimEquiv :: Prop -> Prop
elimEquiv (Var at) = Var at
elimEquiv (Neg a) = Neg (elimEquiv a)
elimEquiv (Conj p q) = Conj (elimEquiv p) (elimEquiv q)
elimEquiv (Disy p q) = Disy (elimEquiv p) (elimEquiv q)
elimEquiv (Impl p q) = Impl (elimEquiv p) (elimEquiv q)
elimEquiv (Syss p q) = Conj (Impl p q) (Impl q p)

-- Funcion que elimina las implicaciones, puedes suponer que no hay
-- equivalencias.
elimImpl :: Prop -> Prop
elimImpl (Var at) = Var at
elimImpl (Neg a) = Neg (elimImpl a)
elimImpl (Conj p p1) = Conj (elimImpl p) (elimImpl p1)
elimImpl (Disy p p1) = Disy (elimImpl p) (elimImpl p1)
elimImpl (Impl p p1) = Disy (Neg (elimImpl(p))) (elimImpl p1)
elimImpl (Syss p p1) = Syss (elimImpl p) (elimImpl p1)

{-
P -> (Q -> R) => ¬P v (Q -> R) => ¬P v (¬Q v R)
-}

-- Funcion que da TODAS las posibles interpretaciones que podria tomar
-- una formula.
posiblesInterp :: Prop -> [State]
posiblesInterp prop = potencia (vars prop)

-- Funicion que nos dice si un estado es modelo de una proposicion.
esModelo :: Prop -> State -> Bool
esModelo prop est = interp est prop

-- Funcion que nos da TODOS los modelos de una proposicion.
todosModelos :: Prop -> [State]
todosModelos prop = [q | q <- posiblesInterp prop, esModelo prop q]

-- Funcion que nos dice si una proposicion es satifacible.
esSatisfacible :: Prop -> Bool
esSatisfacible prop = todosModelos prop /= []

-- Funcion que nos dice si una proposicion es instisfacible.
esInsatisfacible :: Prop -> Bool
esInsatisfacible prop = not (esSatisfacible prop)

-- Funcion que nos dice si una proposicion es una tautologia.
esTautologia :: Prop -> Bool
esTautologia prop = todosModelos prop == posiblesInterp prop

-- Funcion que nos dice si una proposicion es una contradiccion.
esContradiccion :: Prop -> Bool
esContradiccion = esInsatisfacible

---------------------------------------------------------------------------------
--------                           AUXILIARES                            --------
---------------------------------------------------------------------------------

potencia :: [a] -> [[a]]
potencia [] = [[]]
potencia (x:xs) = [ x:ys | ys <- xss] ++ xss
  where
    xss = potencia xs

contains ::Eq a => a -> [a] -> Bool
contains t [] = False
contains t (x:xs) = (t == x) || contains t xs

---------------------------------------------------------------------------------
--------                             EJEMPLOS                            --------
---------------------------------------------------------------------------------

i  = Conj (Var "p") (Var "q")
lol = (Var "p") <--> (Var "q")

lol2 = (Var "p") /\ (¬)(Var "p")

p = Var "p"
q = Var "q"
r = Var "r"
varsrp = ["r", "p"]
form1 = ((p \/ q) --> (((¬) q) --> r))
interp1 = interp varsrp form1

taut1 = (p \/ (¬) p)
taut2 = ((p \/ q) \/ ((¬) p /\ (¬) q))

cont1 = ((p \/ q) /\ ((¬) p /\ (¬) q))

potencia1 = potencia [1,2,3]
