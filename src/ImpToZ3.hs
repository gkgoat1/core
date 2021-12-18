{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}
module ImpToZ3 where

import           Control.Monad (foldM, forM_, (=<<))

import           Data.Map      (Map, (!))
import qualified Data.Map      as Map
import           Data.Maybe    (fromJust)

import           Z3.Monad      (AST, Z3, (+?))
import qualified Z3.Monad      as Z3

import           Imp
import Data.Either
import Data.Sequence (mapWithIndex)
import ImpToZ3 (makeFnTab)

type Z3Var = (Either AST (Z3.FuncDecl,AST))
data Z3State = Z3State {mem :: AST}
type Vars = Map Name Z3Var

getZ3Var :: Name -> Vars -> Z3Var
getZ3Var name scope = case Map.lookup name scope of
  Just x  -> x
  Nothing -> error $ "Variable " ++ show name ++ " does not exist!"

makeVar :: Name -> Z3 AST
makeVar name = Z3.mkFreshBvVar (show name) width

makeVars :: [Name] -> Z3 Vars
makeVars names = foldM addVar Map.empty names
  where addVar vars name = do var <- makeVar name
                              return (Map.insert name var vars)
makeFnTab scope = Map.fromList $ (zip [0..] $ map (\(x,y) -> (x,fromRight undefined y)) $ filter (\(x,y) -> isRight y) Map.elems scope) >>= (\(i,(k,v)) -> [((k,v),i)])

fnIdAt scope val = makeFnTab scope Map.!! (scope Map.!! val)

makeFnTabSwitch scope val target args = (Map.elems $ makeFnTab scope) >>= (\((k,v),i) x -> Seq x (If (val :==: (Lit i)) (Set target (Call (Z3.name k) args) )))

width :: Int
width = 32

bound :: Int
bound = 30

unroll :: Int -> Cmd -> Cmd
unroll bound = \case
  While cond body -> unrollLoop bound (While cond body)
  c_1 `Seq` c_2   -> unroll bound c_1 `Seq` unroll bound c_2
  If cond c_1 c_2 -> If cond (unroll bound c_1) (unroll bound c_2)
  cmd             -> cmd
  where unrollLoop 0 _                      = Skip
        unrollLoop n loop@(While cond body) =
          If cond (body `Seq` unrollLoop (n - 1) loop) Skip


op :: (AST -> AST -> Z3 AST) -> Z3 AST -> Z3 AST -> Z3 AST
op f a b = do a' <- a; b' <- b; f a' b'

aexp scope = \case
  Lit n       -> Z3.mkBvNum width n >>= (\x -> return (x,scope))
  Var name    -> return . (\x -> (x,scope)) . fromLeft undefined . fromJust $ Map.lookup name scope
  e_1 :+: e_2 -> do
    (v_1, scope') <- aexp scope e_1
    (v_2,scope'') <- aexp scope' e_2
    op Z3.mkBvadd (return v_1) (return v_2) >>= (\x -> return (x,scope''))
  e_1 :-: e_2 -> do
    (v_1, scope') <- aexp scope e_1
    (v_2,scope'') <- aexp scope' e_2
    op Z3.mkBvsub (return v_1) (return v_2) >>= (\x -> return (x,scope''))
  e_1 :*: e_2 -> do
    (v_1, scope') <- aexp scope e_1
    (v_2,scope'') <- aexp scope' e_2
    op Z3.mkBvmul (return v_1) (return v_2) >>= (\x -> return (x,scope''))
  e_1 :/: e_2 -> do
    (v_1, scope') <- aexp scope e_1
    (v_2,scope'') <- aexp scope' e_2
    op Z3.mkBvsdiv (return v_1) (return v_2) >>= (\x -> return (x,scope''))
  Bracket c n -> cmd scope c >>= (\s -> return $ (fromLeft undefined $ fromJust $ Map.lookup n s,s))
  SetA name val -> do (newVal,scope') <- aexp scope val
                      newVar <- makeVar name
                      Z3.assert =<< Z3.mkEq newVar newVal
                      return $ (newVar,Map.insert name (Left newVar) scope')
  Call name args -> case fromJust $ Map.lookup name scope of
    Right x -> func (x) (mapM (aexp scope) args) scope
    Left y -> aexp scope $ Bracket (makeFnTabSwitch scope y "_target" args) "_target"
  FnPtr name r -> if r == 0 then 
    makeFnTab scope Map.!! (fromRight undefined $ fromJust $ Map.lookup n s) else
      get >>= (\x -> x ! (len x - r + 1)) >>= (\s -> makeFnTab s Map.!! (fromRight undefined $ fromJust $ Map.lookup n s))
  Raw x -> x >>= (\y -> return (y,scope))
  Load x -> do
    (v,scope') <- aexp scope x
    g <- get
    let m = mem g
    l <- Z3.mkSelect m v
    return (l,scope')
  Store x y -> do
    (v,scope') <- aexp scope x
    (vt,scope'') <- aexp scope' y
    g <- get
    let m = mem g
    n <- Z3.mkStore m x y
    modify (\x -> x{mem = n})
    return (vt,scope'')
  All -> mkVar width >>= (\x -> return (x,scope))
  Bound b -> do
    (c,scope') <- bexp scope b
    Z3.assert =<< c
    Z3.mkBvNum width 1 >>= (\x -> return (x,scope))



bexp scope = \case
  True'        -> Z3.mkBool True >>= (\x -> return (x,scope))
  False'       -> Z3.mkBool False >>= (\x -> return (x,scope))
  e_1 :<=: e_2 -> do
    (v_1, scope') <- aexp scope e_1
    (v_2,scope'') <- aexp scope' e_2
    op Z3.mkBvsle (return v_1) (return v_2) >>= (\x -> return (x,scope''))
  e_1 :==: e_2 -> do
    (v_1, scope') <- aexp scope e_1
    (v_2,scope'') <- aexp scope' e_2
    op Z3.mkEq (return v_1) (return v_2) >>= (\x -> return (x,scope''))
  b_1 :|: b_2  -> do (b_1,scope') <- bexp scope b_1
                     (b_2,scope'') <- bexp scope' b_2
                     Z3.mkOr [b_1, b_2] >>= (\x -> return (s,scope''))
  b_1 :&: b_2  -> do (b_1,scope') <- bexp scope b_1
                     (b_2,scope'') <- bexp scope' b_2
                     Z3.mkAnd [b_1, b_2] >>= (\x -> return (s,scope''))
  Not b        -> do 
    (b,scope') <- bexp scope b
    n <- Z3.mkNot b
    return (n,scope')
func fn args inputs = do
  let (zfn,ret) = fn
  a <- Z3.mkApp zfn $ map (\x -> fromLeft undefined $ fromJust $ Map.lookup x inputs) args
  nv <- makeVar $ Name "Return"
  Z3.assert  =<< Z3.mkEq a nv
  Z3.assert =<< Z3.mkEq nv ret
  return nv

newZfn name cmd' args = do
  a <- mapM (const $ Z3.mkBvSort width) args
  b <- Z3.mkBvSort width
  c <- Z3.mkFuncDecl name (a) $ b
  modify (++[Map.fromList $ (mapWithIndex (\a b -> (b,Raw $ getAppArg a)) args ++ [("self",Right (c,d))])])
  d <- cmd (Map.fromList $ (mapWithIndex (\a b -> (b,Raw $ getAppArg a)) args ++ [("self",Right (c,d))])) cmd' >>= (\(a,_) -> a)
  modify (head)
  return (c,d)

cmd inputs = compile inputs . unroll bound
  where compile scope = \case
          Skip            -> return scope
          Set name val    -> do (newVal,scope') <- aexp scope val
                                newVar <- makeVar name
                                Z3.assert =<< Z3.mkEq newVar newVal
                                return $ Map.insert name newVar scope'
          Fn name cmd args -> do nfn <- newZfn name cmd args
                                 return $ Map.insert name (Right $ nfn) scope
          Seq c_1 c_2     -> do scope'  <- compile scope c_1
                                compile scope' c_2
          Do x -> aexp scope x >>= (\(_,y) -> return $ y)
          If cond c_1 c_2 -> do (cond',scope')   <- bexp scope cond
                                scope''  <- compile scope' c_1
                                scope''' <- compile scope' c_2
                                makePhis cond' scope scope'' scope'''
          _               -> error "Loops have to be unrolled before compiling to SMT!"

constrainVars :: Map Name Int -> Vars -> Z3 ()
constrainVars values scope = forM_ (Map.keys values) $ \ name -> do
  val <- Z3.mkBvNum width (values ! name)
  Z3.assert =<< Z3.mkEq (scope ! name) val

forwards :: Map Name Int -> Cmd -> Z3 ()
forwards values program = do initialScope <- makeVars $ Map.keys values
                             constrainVars values initialScope
                             cmd initialScope program
                             return ()

backwards :: Map Name Int -> Cmd -> Z3 ()
backwards values program = do initialScope <- makeVars $ Map.keys values
                              finalScope   <- cmd initialScope program
                              constrainVars values finalScope
                              return ()

opts = Z3.opt "MODEL" True

-- | Encodes the result of a conditional by asserting new values for
-- each variable depending on which branch was taken. Example:
--
--  y := 10;
--  if cond { x := 1; y := y + 10 }
--     else { y := y + 11; z := y }
--
--  x_1 = 1
--  y_2 = y_1 + 10
--  y_3 = y_ 1 + 11
--  z_1 = y_3
--  x_2 = ite(cond, x_1, x_0)
--  y_4 = ite(cond, y_2, y_3)
--  z_2 = ite(cond, z_0, z_1)
makePhis :: AST -> Vars -> Vars -> Vars -> Z3 Vars
makePhis cond original scope' scope'' = foldM go original $ Map.keys original
  where go scope name = do
          newVar     <- makeVar name
          ite        <- Z3.mkIte cond (getZ3Var name scope') (getZ3Var name scope'')
          constraint <- Z3.mkEq newVar ite
          Z3.assert constraint
          return $ Map.insert name newVar scope
