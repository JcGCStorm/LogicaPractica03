module FormasNormales where

import FormProp
import Operadores

-- Función que dada una fórmula f, regresa una fórmula equivalente a f
-- en su Forma Normal Negativa.
fnn :: Prop -> Prop
fnn :: Prop -> Prop
fnn (Var p) = Var p
fnn (Neg (Neg p)) = fnn p
fnn (Neg (Conj p q)) = Disy (fnn (Neg p)) (fnn (Neg q))
fnn (Neg (Disy p q)) = Conj (fnn (Neg p)) (fnn (Neg q))
fnn (Conj p q) = Conj (fnn p) (fnn q)
fnn (Disy p q) = Disy (fnn p) (fnn q)

-- Función que dada una fórmula f, regresa una fórmula equivalente a f
-- en su Forma Normal Conjuntiva.
fnc :: Prop -> Prop
fnc = error "D:"

-- Función que dada una fórmula f, regresa una fórmula equivalente a f
-- en su Forma Normal Disyuntiva.
fnd :: Prop -> Prop
fnd = error "D:"

-- ejemplo 1 para forma normal negativa
fnn1 = error "D:"
-- Ejemplo: ¬(p ∨ q)
formulaEjemplo :: Prop
formulaEjemplo = Neg (Disy (Var "p") (Var "q"))

formaNormalNegativa :: Prop
formaNormalNegativa = fnn formulaEjemplo

-- ejemplo 2 para forma normal negativa
fnn2 = error "D:"

-- ejemplo 1 para forma normal conjuntiva
fnc1 = error "D:"

-- ejemplo 2 para forma normal conjuntiva
fnc2 = error "D:"

-- ejemplo 1 para forma normal disyuntiva
fnd1 = error "D:"
