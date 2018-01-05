{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
 
module Interpreter
  (
    -- * Types
    Prog,
    Asgn,

    -- * Functions
    evalRaw,
    evalAdt,
  ) where


-------------------------------------------------------------------------------
--------------------------------- The Expr ADT  -------------------------------
-------------------------------------------------------------------------------
data Expr = Add Expr Expr
          | Sub Expr Expr
          | Mult Expr Expr
          | Equal Expr Expr
          | Smaller Expr Expr
          | Symbol String
          | Value Int deriving (Show, Read)

-- TODO Implement a parser for the Expr ADT.
--

-------------------------------------------------------------------------------
---------------------------------- The Prog ADT -------------------------------
-------------------------------------------------------------------------------
data Asgn = Asgn String Expr deriving (Show, Read)

data Prog = Eq Asgn
          | Seq Prog Prog
          | If Expr Prog Prog
          | For Asgn Expr Asgn Prog
          | Assert Expr
          | Return Expr deriving (Show, Read)

-- TODO Implement a parser for the Prog ADT.
--

-- TODO The *parse* function. It receives a String - the program in
-- a "raw" format and it could return *Just* a program as an instance of the
-- *Prog* data type if no parsing errors are encountered, or Nothing if parsing
-- failed.
--
-- This is composed with evalAdt to yield the evalRaw function.
parse :: String -> Maybe Prog
parse = undefined

-------------------------------------------------------------------------------
-------------------------------- The Interpreter ------------------------------
-------------------------------------------------------------------------------

-- TODO The *evalAdt* function.  It receives the program as an instance of the
-- *Prog* data type and returns an instance of *Either String Int*; that is,
-- the result of interpreting the program.
--
-- The result of a correct program is always an Int.  This is a simplification
-- we make in order to ease the implementation.  However, we wrap this Int with
-- a *Either String* type constructor to handle errors.  The *Either* type
-- constructor is defined as:
--
-- data Either a b = Left a | Right b
--
-- and it is generally used for error handling.  That means that a value of
-- *Left a* - Left String in our case - wraps an error while a value of *Right
-- b* - Right Int in our case - wraps a correct result (notice that Right is a
-- synonym for "correct" in English).
-- 
-- For further information on Either, see the references in the statement of
-- the assignment.
data Result  = Valoare Int | Error | TTrue | FFalse
type Dictionary = [(String, Int)]
instance Show (Result) where
	show Error = "Error"
	show (Valoare a) = show a
	show TTrue = "TRUE"
	show FFalse = "FALSE"

class Eval where
	eval :: Dictionary -> Expr -> Result 

valueOf :: Dictionary -> String -> Result 
valueOf d s = let l = filter (\x -> (fst x) == s ) d 
            in if length l == 0 then Error else Valoare (snd (head l))		
instance  Eval where
	eval d (Value x) = Valoare x
	eval d (Symbol x) = valueOf d x
	eval d (Add x y) = let --Adunare
				firstEvaluation = eval d x
				secondEvaluation = eval d y
				isError :: Result -> Bool
				isError(Error) = True
				isError _ = False
				f (Valoare a) = a
			    in if (isError firstEvaluation || isError secondEvaluation) then Error
			       else Valoare(f firstEvaluation + f secondEvaluation)
	
	eval d (Sub x y) = let --Scadere
				firstEvaluation = eval d x
				secondEvaluation = eval d y
				isError :: Result -> Bool
				isError(Error) = True
				isError _ = False
				f (Valoare a) = a
			    in if (isError firstEvaluation || isError secondEvaluation) then Error
			       else Valoare(f firstEvaluation - f secondEvaluation)

	eval d (Mult x y) = let --Inmultire
				firstEvaluation = eval d x
				secondEvaluation = eval d y
				isError :: Result -> Bool
				isError(Error) = True
				isError _ = False
				f (Valoare a) = a
			    in if (isError firstEvaluation || isError secondEvaluation) then Error
			       else Valoare(f firstEvaluation * f secondEvaluation)
	
	eval d (Equal x y) = let --Equal
				firstEvaluation = eval d x
				secondEvaluation = eval d y
				isError :: Result -> Bool
				isError(Error) = True
				isError _ = False
				f(Valoare a) = a
			     in if (isError firstEvaluation || isError secondEvaluation) then Error
				      else if (f firstEvaluation == f secondEvaluation) then TTrue
				           else FFalse
	
	eval d (Smaller x y) = let --Smaller
				firstEvaluation = eval d x
				secondEvaluation = eval d y
				isError :: Result -> Bool
				isError(Error) = True
				isError _ = False
				f(Valoare a) = a
			     in if (isError firstEvaluation || isError secondEvaluation) then Error
				      else if (f firstEvaluation < f secondEvaluation) then TTrue
				           else FFalse


insert a b d = let l = filter (\x -> (fst x) == a) d --Inserare unica
	           m = filter (\x -> (fst x) /= a) d    
	       in if length l == 0 then (a,b):d
	          else (a,b):m 	  

evalAssign d (Asgn a b) = let --Evaluare assign
			     rez = eval d b
			     isError :: Result -> Bool
			     isError(Error) = True
		             isError _ = False
			     f(Valoare a) = a			      
			  in if(isError rez) then ("Uninitialized variable", 0):d
			     else insert a (f rez) d
			          
	
evalAux d (Seq a b) = let
			  dict = evalAux d a
			  dict2 = evalAux dict b
		      in dict2
evalAux d (Eq a) = let
	              dict = evalAssign d a
	           in dict

evalAux d (Return a) = let
			  rez = eval d a
			  isError :: Result -> Bool
			  isError(Error) = True
		          isError _ = False
			  f(Valoare a) = a
			in if(isError rez) then ("Uninitialized variable", 0):d	
			   else ("Return",f rez):d

evalAux d (If a b c) = let
			  rez = eval d a
		       	  isError :: Result -> Bool
			  isError(Error) = True
		          isError _ = False
			  isTrue :: Result -> Bool
			  isTrue(TTrue) = True
		          isTrue _ = False
			in if(isError rez) then ("Uninitialized variable", 0):d
			   else if(isTrue rez) then evalAux d b
				      else evalAux d c
				
			 	
evalAux d (For a b c e) = let
			     rez = evalAssign d a --facem primul assign
			     isError :: Result -> Bool
			     isError(Error) = True
		             isError _ = False
			  in evalFor rez b c e
evalAux d (Assert a) = let
			   rez = eval d a 
			   isError :: Result -> Bool
		      	   isError(Error) = True
		           isError _ = False
		           isTrue :: Result -> Bool
		           isTrue(TTrue) = True
		           isTrue _ = False
			in if(isError rez) then ("Uninitialized variable", 0):d
			   else if(isTrue rez) then d
				      else
				 	("Assert failed",0):d
--EvalFOr primeste un dictionar, o expresie un assign si programul --Am facut asta ca sa fac initializarea doar o data
evalFor d e c p = let 
		      rez = eval d e --Facem conditia
		      rez2 = evalAux d p --Facem programul
		      rez3 = evalAssign rez2 c --Facem al doilea assign
		      isError :: Result -> Bool
		      isError(Error) = True
		      isError _ = False
		      isTrue :: Result -> Bool
		      isTrue(TTrue) = True
		      isTrue _ = False
		   in if(isError rez) then d 
		      else if(isTrue rez) then (evalFor rez3 e c p)
		      	   else
				d



evalAdt :: Prog -> Either String Int
evalAdt prog = let 
		   d = evalAux [] prog   
		   l = filter (\x -> (fst x) == "Uninitialized variable") d
	 	   m = filter (\x -> (fst x) == "Return") d
		   n = filter (\x -> (fst x) == "Assert failed") d 
            --in if length l == 0 then Error else Valoare (snd (head l))  
	       
		--in if length l == 0 then if length m == 0 then (Left "Missing return")
					 
		--			 else if length n == 0 then Right(snd (head (reverse m)))
		  --			      else (Left "Assert failed")
		  -- else (Left "Uninitialized variable") 

		in if length n /= 0 then (Left "Assert failed")
		   else if length l /= 0 then (Left "Uninitialized variable")
		    	  else if length m == 0 then (Left "Missing return")
			           else Right(snd (head (reverse m)))
					     

-- The *evalRaw* function is already implemented, but it relies on the *parse*
-- function which you have to implement.
--
-- Of couse, you can change this definition.  Only its name and type are
-- important.
evalRaw :: String -> Either String Int
evalRaw rawProg =
    case parse rawProg of
        Just prog -> evalAdt prog
        Nothing   -> Left "Syntax error"
