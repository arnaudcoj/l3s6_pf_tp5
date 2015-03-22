{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-
Matthieu Caron
Arnaud Cojez
TP5 Interpretation
-}
--PREMIERE PARTIE

import Parser
import Data.Maybe
import System.IO

type Nom = String

data Expression = Lam Nom Expression
                | App Expression Expression
                | Var Nom
         	       | Lit Litteral
                deriving (Show,Eq)

data Litteral = Entier Integer
              | Bool   Bool
              deriving (Show,Eq)
                       
--Q1
                       
espacesP :: Parser ()
espacesP = (car ' ' >>= \_ -> 
               espacesP >>= \s ->
               return s) ||| return ()
           
--Q2

isMin :: Char -> Bool
isMin = flip elem ['a'..'z']

minP :: Parser [Char]
minP = unOuPlus (carCond isMin)

nomP :: Parser Nom
nomP = ( minP >>= \s ->
         espacesP >>= \_ ->
         return s) ||| echoue
       
--Q3

varP :: Parser Expression
varP = (nomP >>= \s ->
         return (Var s))
       
--Q4

applique :: [Expression] -> Expression
applique (e:es) = foldl (\x y -> App x y) e es

--Q5

exprP :: Parser Expression
exprP = booleenP ||| nombreP ||| varP ||| lambdaP ||| exprParentheseeP         

exprsP :: Parser Expression
exprsP = (unOuPlus exprP >>= \s ->
           return (applique s))
         
--Q6

flecheP :: Parser ()
flecheP = (car '-' >>= \_ ->
           car '>' >>= \s ->
           espacesP >>= \_ ->
           return ()) ||| echoue

lambdaP :: Parser Expression
lambdaP = (carCond (\x -> x=='λ' || x=='\\') >>= \_ ->
           espacesP >>= \_ ->
           nomP >>= \p ->
           flecheP >>= \_ ->
           exprsP >>= \s -> 
           return (Lam p s)) ||| echoue

--Q7 voir Q5

--Q8

exprParentheseeP :: Parser Expression
exprParentheseeP = (car '(' >>= \_  ->
                    espacesP >>= \_ ->
                    unOuPlus exprP >>= \e ->
                    car ')' >>= \_ ->
                    espacesP >>= \_ ->
                    return (applique e)
                   )
             
--Q9

isChiffre :: Char -> Bool
isChiffre = flip elem ['0'..'9']

nombreP :: Parser Expression
nombreP = ( unOuPlus (carCond isChiffre) >>= \n ->
            espacesP >>= \_ ->
            return (Lit (Entier (read n))))

--Q10

booleenP :: Parser Expression
booleenP = (chaine "True" >>= \_ ->
            espacesP >>= \_ ->
            return (Lit (Bool True))) |||
           (chaine "False" >>= \_ ->
            espacesP >>= \_ ->
            return (Lit (Bool False)))

--Q11

expressionP :: Parser Expression
expressionP = (espacesP >>= \_ ->
               exprsP >>= \r ->
               return r)

--SECONDE PARTIE
--Q12

getExp Nothing = error "error parse"
getExp (Just (e,"")) = e
getExp _ = error "error parse"

ras :: String -> Expression
ras s = e
  where e = getExp (parse expressionP s)
        
--Q13
                            
data ValeurA = VLitteralA Litteral
             | VFonctionA (ValeurA -> ValeurA) 

--Q14

instance Show ValeurA where
    show (VFonctionA _) = "λ"    
    show (VLitteralA (Entier i)) = show i
    show (VLitteralA (Bool b)) = show b

type Environnement a = [(Nom, a)]
    
--Q15

interpreteA :: Environnement ValeurA -> Expression -> ValeurA
interpreteA _ (Lit l) = VLitteralA l
interpreteA xs (Var k) = fromJust (lookup k xs)
interpreteA xs (Lam nom expr) = VFonctionA (\v -> interpreteA ((nom,v):xs) expr)
interpreteA xs (App e1 e2) = f v2
  where VFonctionA f = interpreteA xs e1
        v2 = interpreteA xs e2

--Q16

negA :: ValeurA
negA = VFonctionA (\(VLitteralA (Entier n)) -> (VLitteralA (Entier (-n))))

--Q17

addA :: ValeurA
addA = VFonctionA (\(VLitteralA (Entier n1)) -> VFonctionA (\(VLitteralA (Entier n2)) -> VLitteralA (Entier (n1 + n2))))

--Q18

releveBinOpEntierA :: (Integer -> Integer -> Integer) -> ValeurA 
releveBinOpEntierA op = VFonctionA (\(VLitteralA (Entier n1)) -> VFonctionA (\(VLitteralA (Entier n2)) -> VLitteralA (Entier (n1 `op` n2))))


envA :: Environnement ValeurA
envA = [ ("neg",   negA)
       , ("add",   releveBinOpEntierA (+))
       , ("soust", releveBinOpEntierA (-))
       , ("mult",  releveBinOpEntierA (*))
       , ("quot",  releveBinOpEntierA quot)
       , ("if",    ifthenelseA )	]
       
--Q19
 
ifthenelseA :: ValeurA
ifthenelseA = VFonctionA (\(VLitteralA (Bool b)) -> VFonctionA (\(VLitteralA (Entier n1)) -> VFonctionA (\(VLitteralA (Entier n2)) -> if b then 
                                           (VLitteralA (Entier n1)) 
                                         else (VLitteralA (Entier n2)))))

--Q20

{-
main :: IO ()
main = ( putStr "minilang>" >>= \_ ->
          hFlush stdout >>= \_ ->
          isEOF >>= \b ->
          if b then
            putStr "\nsee you later !\n"
          else
            getLine >>= \l ->
            putStr (show (interpreteA envA (ras l)) ++ "\n") >>= \_ ->
            main)
-}

--Interpreteur avec Erreurs

data ValeurB = VLitteralB Litteral
             | VFonctionB (ValeurB -> ErrValB)

type MsgErreur = String
type ErrValB   = Either MsgErreur ValeurB

--Q21

instance Show ValeurB where
    show (VFonctionB _) = "λ"
    show (VLitteralB (Entier i)) = show i
    show (VLitteralB (Bool b)) = show b
    
--Q22

interpreteB :: Environnement ValeurB -> Expression -> ErrValB
interpreteB _   (Lit l)     = Right (VLitteralB l)
interpreteB env (Var n)     =  let j = lookup n env
                               in case isJust j of  True  -> Right (fromJust j)
                                                    False -> Left ("variable "++ n ++" non definie")
interpreteB env (Lam n e)   = Right (VFonctionB f)
                where f v = interpreteB ((n,v) : env) e                                                   
interpreteB env (App e1 e2) = case interpreteB env e1 of
                                Right (VFonctionB f) -> case interpreteB env e2 of  Right r -> f r
                                                                                    Left  m -> Left m
                                Right (VLitteralB (Entier n)) -> Left (show n ++ " n'est pas une fonction, application impossible")
                                Right (VLitteralB (Bool b))   -> Left (show b ++ " n'est pas une fonction, application impossible")
                                Left  m              -> Left m

--Q23
addB :: ValeurB
addB = VFonctionB f
    where   f (VLitteralB (Entier n1)) = Right (VFonctionB h)
                            where  h (VLitteralB (Entier n2)) = Right (VLitteralB (Entier (n1 + n2)))
                                   h (VLitteralB (Bool b))    = Left (show b ++ " n'est pas un entier")
                                   h (VFonctionB _)           = Left ("λ n'est pas un entier")
            f (VLitteralB (Bool b))    = Left (show b ++ " n'est pas un entier")
            f (VFonctionB _)           = Left ("λ n'est pas un entier")
                
--Q24
quotB :: ValeurB                
quotB = VFonctionB f
    where   f (VLitteralB (Entier n1)) = Right (VFonctionB h)
                            where  h (VLitteralB (Entier 0))  = Left "division par zero"
                                   h (VLitteralB (Entier n2)) = Right (VLitteralB (Entier (quot n1 n2)))
                                   h (VLitteralB (Bool b))    = Left (show b ++ " n'est pas un entier")
                                   h (VFonctionB _)           = Left ("λ n'est pas un entier")
            f (VLitteralB (Bool b))    = Left (show b ++ " n'est pas un entier")
            f (VFonctionB _)           = Left ("λ n'est pas un entier")

--Interprete traçant

data ValeurC = VLitteralC Litteral
             | VFonctionC (ValeurC -> OutValC)

type Trace   = String
type OutValC = (Trace, ValeurC)

--Q25

instance Show ValeurC where
    show (VFonctionC _) = "λ"    
    show (VLitteralC (Entier i)) = show i
    show (VLitteralC (Bool b)) = show b

--Q26
    
interpreteC :: Environnement ValeurC -> Expression -> OutValC
interpreteC _ (Lit l) = ("", VLitteralC l)
interpreteC xs (Var k) = ("", fromJust (lookup k xs))
interpreteC xs (Lam nom expr) = ("", VFonctionC (\v -> interpreteC ((nom,v):xs) expr))
interpreteC xs (App e1 e2) = ((s1 ++ s2 ++ "." ++ sr), vr)
  where (s1, VFonctionC f) = interpreteC xs e1
        (s2, v2) = interpreteC xs e2
        (sr, vr) = f v2

--Q27

pingC :: ValeurC
pingC = VFonctionC (\v -> ("p", v))

--Interprete Monadique

--Q28

data ValeurM m = VLitteralM Litteral
               | VFonctionM (ValeurM m -> m (ValeurM m))
                 
instance Show (ValeurM m) where
  show (VFonctionM _) = "λ"
  show (VLitteralM (Entier i)) = show i
  show (VLitteralM (Bool b)) = show b

--Q29
  
data SimpleM v = S v
               deriving Show

interpreteSimpleM :: Environnement (ValeurM SimpleM) -> Expression -> SimpleM (ValeurM SimpleM)
interpreteSimpleM _ (Lit l) = S (VLitteralM l)
interpreteSimpleM xs (Var k) = S (fromJust (lookup k xs))
interpreteSimpleM xs (Lam nom expr) = S (VFonctionM (\v -> interpreteSimpleM ((nom,v):xs) expr))
interpreteSimpleM xs (App e1 e2) = f v2
  where S (VFonctionM f) = interpreteSimpleM xs e1
        S v2 = interpreteSimpleM xs e2

instance Monad SimpleM where
    return      = S
    (S v) >>= f = f v

--Q30

interpreteM :: Monad m => Environnement (ValeurM m) -> Expression -> m (ValeurM m)
interpreteM _ (Lit l) = return (VLitteralM l)
interpreteM xs (Var k) = return (fromJust (lookup k xs))
interpreteM xs (Lam nom expr) = return (VFonctionM (\v -> interpreteM ((nom,v):xs) expr))
interpreteM xs (App e1 e2) = (interpreteM xs e1 >>= \(VFonctionM f) ->
                               interpreteM xs e2 >>= \v2 ->
                               f v2)

type InterpreteM m = Environnement (ValeurM m) -> Expression -> m (ValeurM m)

interpreteS :: InterpreteM SimpleM
interpreteS = interpreteM

--Q31

{-
*Main> interpreteS [] (ras "12")
S 12
*Main> interpreteS [] (ras "(\\x -> x) 12")
S 12
*Main> interpreteS [] (ras "(\\x -> x)")
S λ
*Main> interpreteS [("x", (VLitteralM (Bool True)))] (ras "x")
S True
*Main> interpreteS [("x", (VLitteralM (Entier 3)))] (ras "x")
S 3
-}

data TraceM v = T (Trace, v)
              deriving Show

--Q32

instance Monad TraceM where
    return v      = T ("",v)
    T (t ,v) >>= f = T (t ++ tr, vr)
      where T (tr,vr) = f v

interpreteMT :: InterpreteM TraceM
interpreteMT = interpreteM

pingM :: ValeurM TraceM
pingM = VFonctionM (\v -> T ("p", v))

--Q33
interpreteMT' :: InterpreteM TraceM
interpreteMT' xs (App e1 e2) = T ((s1 ++ s2 ++ "." ++ sr), vr)
  where T (s1, VFonctionM f) = interpreteMT' xs e1
        T (s2, v2) = interpreteMT' xs e2
        T (sr, vr) = f v2
interpreteMT' xs (Lam nom expr) = T ("", VFonctionM (\v -> interpreteMT' ((nom,v):xs) expr))
interpreteMT' xs v = interpreteMT xs v

--Variante avec Erreurs

data ErreurM v = Succes v
               | Erreur String
               deriving Show

--Q34

instance Monad ErreurM where
    fail e = Erreur e
    return = Succes
    (Succes v) >>= f = f v
    (Erreur m) >>= _ = fail m

--Q35
    
interpreteE :: InterpreteM ErreurM
interpreteE _ (Lit l) = return (VLitteralM l)
interpreteE xs (Var v) = let j = lookup v xs
                         in case isJust j of True -> return (fromJust j)
                                             False -> fail ("La variable " ++ v ++ " n'est pas definie")
interpreteE xs (Lam n e) = return (VFonctionM f)
  where f v = interpreteE ((n,v):xs) e
interpreteE xs (App e1 e2) =
  (interpreteE xs e1 >>= \r ->
    case r of
      VFonctionM f ->
        (interpreteE xs e2 >>= \r2 ->
          f r2)
      VLitteralM (Entier n) -> fail (show n ++ " n'est pas une fonction, application impossible")
      VLitteralM (Bool b) -> fail (show b ++ " n'est pas une fonction, application impossible")
  )

--Cerise sur le gâteau

class Monad m => Injectable m t where
  injecte :: t -> ValeurM m

instance Monad m => Injectable m Bool where
  injecte = VLitteralM . Bool

--Q36

instance Monad m => Injectable m Integer where
  injecte = VLitteralM . Entier

--Q37

instance (Monad m, Injectable m t) => Injectable m (Bool -> t) where
    injecte f = VFonctionM (\(VLitteralM (Bool b)) -> return (injecte (f b)))

instance (Monad m, Injectable m t) => Injectable m (Integer -> t) where
    injecte f = VFonctionM (\(VLitteralM (Entier v)) -> return (injecte (f v)))

--Q38
    
envM :: Monad m => Environnement (ValeurM m)
envM = [ ("add",   injecte ((+) :: Integer -> Integer -> Integer))
       , ("soust", injecte ((-) :: Integer -> Integer -> Integer))
       , ("mult",  injecte ((*) :: Integer -> Integer -> Integer))
       , ("quot",  injecte (quot :: Integer -> Integer -> Integer))
       , ("et",    injecte (&&))
       , ("ou",    injecte (||))
       , ("non",   injecte not) ]


main :: IO ()
main = ( putStr "minilang>" >>= \_ ->
          hFlush stdout >>= \_ ->
          isEOF >>= \b ->
          if b then
            putStr "\nsee you later !\n"
          else
            getLine >>= \l ->
            putStr (show (interpreteE envM (ras l)) ++ "\n") >>= \_ ->
            main)
