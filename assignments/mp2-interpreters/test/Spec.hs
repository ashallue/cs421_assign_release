module Spec where

import Test.Framework ( defaultMainWithOpts, testGroup, TestOptions, RunnerOptions
                      , topt_maximum_generated_tests, ropt_test_options
                      )
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Test.QuickCheck

import Data.Foldable

import Lib
import Data.HashMap.Strict as H (HashMap, empty, singleton, toList, fromList, insert, lookup, union, delete, null)

main :: IO ()    
main = do {
       ; let empty_test_opts = mempty :: TestOptions
       ; let my_test_opts = empty_test_opts {
                              topt_maximum_generated_tests = Just 500
                            }
       ; let empty_runner_opts = mempty :: RunnerOptions
       ; let my_runner_opts = empty_runner_opts {
                                ropt_test_options = Just my_test_opts
                              }
       ; defaultMainWithOpts tests my_runner_opts
       }

tests =
  [ testGroup "=G= Lift Functions"
    [ testProperty "=P= Lifts Boolean Operations (2 points)\t\t"    (forAll arbLiftBool liftBool_prop)
    , testProperty "=P= Lifts Comparison Operations (2 points)\t\t" (forAll arbLiftComp liftComp_prop)
    ]
  , testGroup "=G= eval Function"
    [ testProperty "=P= Constant Expressions (2 points)\t\t\t"         (forAll arbConstExp      anyNonEnvExp_prop)
    , testProperty "=P= Variable Expressions (2 points)\t\t\t"         (forAll arbVarExp              varExp_prop)
    , testProperty "=P= Integer Operation Expressions (3 points)\t\t"  (forAll (arbIntOpExp 3)  anyNonEnvExp_prop)
    , testProperty "=P= Comparison Operation Expressions (3 points)\t" (forAll (arbCompOpExp 3) anyNonEnvExp_prop)
    , testProperty "=P= Boolean Operation Expressions (3 points)\t\t"  (forAll (arbBoolOpExp 3) anyNonEnvExp_prop)
    , testProperty "=P= If Expressions (3 points)\t\t\t\t"             (forAll arbIfExp         anyNonEnvExp_prop)
    , testProperty "=P= Function Expressions (3 points)\t\t\t"         (forAll arbFunExp           anyEnvExp_prop)
    , testProperty "=P= Function Application (4 points)\t\t\t"         (forAll arbAppExp           anyEnvExp_prop)
    , testProperty "=P= Let Expressions (4 points)\t\t\t"              (forAll arbLetExp           anyEnvExp_prop)
    ]
  , testGroup "=G= exec Function"
    [ testProperty "=P= Assign Statements (2 points)\t\t\t"       setStmt_prop
    , testProperty "=P= Sequence Statements (3 points)\t\t\t"     seqStmt_prop
    , testProperty "=P= If Statements (3 points)\t\t\t\t"         ifStmt_prop
    , testProperty "=P= Procedure Statements (2 points)\t\t\t"    procStmt_prop
    , testProperty "=P= Call Procedure Statements (6 points)\t\t" callStmt_prop
    ]
  ]

-- Common Exception Values
liftExn = ExnVal "Cannot lift"
divExn  = ExnVal "Division by 0"
varExn  = ExnVal "No match in env"
ifExn   = ExnVal "Condition is not a Bool"
funExn  = ExnVal "Apply to non-closure"

                
--Generators
arbIntOp :: Gen (String, Int -> Int -> Int)
arbIntOp = elements [ ("+", (+))
                    , ("-", (-))
                    , ("*", (*))
                    , ("/", (div))
                    ]
            
arbBoolOp :: Gen (String, Bool -> Bool -> Bool)
arbBoolOp = elements [ ("and", (&&))
                     , ("or", (||))
                     ]

arbCompOp :: Gen (String, Int -> Int -> Bool)
arbCompOp = elements [ ("<", (<))
                     , (">", (>))
                     , ("<=", (<=))
                     , (">=", (>=))
                     , ("/=", (/=))
                     , ("==", (==))
                     ]


--- Properties
--- ========
isException ::  Val   -> Bool
isException (ExnVal _) = True
isException         _  = False

liftBool_prop :: (Val, Val, Blind (Bool -> Bool -> Bool), Val) -> Property
liftBool_prop (v1,v2,Blind op,v) = liftBoolOp op v1 v2 === v

liftComp_prop :: (Val, Val, Blind (Int -> Int -> Bool), Val) -> Property
liftComp_prop (v1,v2,Blind op,v) = liftCompOp op v1 v2 === v
  
anyNonEnvExp_prop :: (Exp, Val) -> Property
anyNonEnvExp_prop (e,v)  = classify (isException v) "Exception" $ eval e H.empty === v


anyEnvExp_prop :: (Exp, Val, Env) -> Property
anyEnvExp_prop (e,v,env) = classify (isException v) "Exception" $ eval e env     === v
                           
--- Generators

--- Lifting Functions
--- -----------------


arbMostlyBoolVal :: Gen Val
arbMostlyBoolVal = frequency [ (9, BoolVal <$> arbitrary)
                             , (1, IntVal  <$> arbitrary) ]
                                                         
arbLiftBool :: Gen (Val, Val, Blind (Bool -> Bool -> Bool), Val)
arbLiftBool = wrap <$> arbMostlyBoolVal <*> arbMostlyBoolVal <*> arbBoolOp
  where wrap v1 v2 (_,op) = (v1,v2,Blind op,
                             case (v1,v2) of
                               (BoolVal b1, BoolVal b2) -> BoolVal (b1 `op` b2)
                               _                        -> liftExn
                            )

arbMostlyIntVal :: Gen Val
arbMostlyIntVal = frequency [ (9,  IntVal <$> arbitrary)
                            , (1, BoolVal <$> arbitrary) ]

arbLiftComp :: Gen (Val, Val, Blind (Int -> Int -> Bool), Val)
arbLiftComp = wrap <$> arbMostlyIntVal <*> arbMostlyIntVal <*> arbCompOp
  where wrap v1 v2 (_,op) = (v1,v2,Blind op,
                             case (v1,v2) of
                               (IntVal i1, IntVal i2) -> BoolVal (i1 `op` i2)
                               _                      -> liftExn
                            )


--- eval
--- ----

--- ### Constants
arbBoolExp = ((,) <$> BoolExp <*> BoolVal) <$> arbitrary

arbIntExp  = ((,) <$> IntExp <*> IntVal) <$> arbitrary 


arbConstExp :: Gen (Exp,Val)
arbConstExp = oneof [ ((,) <$> IntExp  <*>  IntVal) <$> arbitrary
                    , ((,) <$> BoolExp <*> BoolVal) <$> arbitrary ] 

                    
--- ### Variables
arbParamList :: Gen [String]
arbParamList = shuffle (map (:"") ['a'..'z'])

arbParam :: Gen String
arbParam = elements $ map (:"") ['a'..'z']

arbVal :: Gen Val
arbVal = oneof [IntVal <$> arbitrary, BoolVal <$> arbitrary] -- add closures

arbVarExp :: Gen (Exp,Val)
arbVarExp = (,) <$> (VarExp <$> arbParam) <*> arbVal

varExp_prop :: (Exp, Val) -> Property
varExp_prop (VarExp p,val) = (eval (VarExp p) env === val) .&. (eval (VarExp p) H.empty === varExn)
    where env = H.singleton p val

  
--- ### Arithmetic
arbMostlyIntExp :: Gen (Exp, Val)
arbMostlyIntExp = frequency [ (19, ((,) <$> IntExp <*> IntVal) <$> arbitrary )
                            , (1, (\ x -> (x,liftExn)) <$> (BoolExp <$> arbitrary) ) ]

arbIntOpExp :: Int -> Gen (Exp,Val)
arbIntOpExp 0 = arbMostlyIntExp
arbIntOpExp n
    | n > 0 = do { (e1,v1)  <- choose (0, n-1) >>= arbIntOpExp
                 ; (e2,v2)  <- choose (0, n-1) >>= arbIntOpExp
                 ; (opS,op) <- arbIntOp
                 ; let val = case (v1,v2,opS) of
                               (_,         IntVal  0,"/") -> divExn
                               (IntVal i1, IntVal i2,  _) -> IntVal $ i1 `op` i2
                               (_        , _        ,  _) -> liftExn
                 ; return (IntOpExp opS e1 e2, val)
                 }



--- ### Boolean and Comparison Operators
arbCompOpExp :: Int -> Gen (Exp,Val)
arbCompOpExp 0 = arbMostlyIntExp
arbCompOpExp n
    | n > 0 = do { (e1,v1)  <- choose (0, n-1) >>= arbIntOpExp
                 ; (e2,v2)  <- choose (0, n-1) >>= arbIntOpExp
                 ; (opS,op) <- arbCompOp
                 ; let val = case (v1,v2) of
                               (IntVal i1, IntVal i2) -> BoolVal $ i1 `op` i2
                               (_        , _        ) -> liftExn
                 ; return (CompOpExp opS e1 e2, val)
                 }

arbMostlyBoolExp :: Gen (Exp, Val)
arbMostlyBoolExp = frequency [ (19, ((,) <$> BoolExp <*> BoolVal) <$> arbitrary )
                             , (1, (\ x -> (x,liftExn)) <$> (IntExp <$> arbitrary) ) ]
      
arbBoolOpExp :: Int -> Gen (Exp,Val)
arbBoolOpExp 0 = arbMostlyBoolExp
arbBoolOpExp n
    | n > 0 = do { (e1,v1)  <- choose (0, n-1) >>= arbBoolOpExp
                 ; (e2,v2)  <- choose (0, n-1) >>= arbBoolOpExp
                 ; (opS,op) <- arbBoolOp
                 ; let val = case (v1,v2) of
                               (BoolVal b1, BoolVal b2) -> BoolVal $ b1 `op` b2
                               (_        , _          ) -> liftExn
                 ; return (BoolOpExp opS e1 e2, val)
                 }                                          
    
--- ### If Expressions

arbIfExp :: Gen (Exp, Val)
arbIfExp = do { (ce,cv) <- choose (0, 2) >>= arbBoolOpExp
              ; (e1,v1) <- arbConstExp
              ; (e2,v2) <- arbConstExp
              ; let val = case cv of
                            BoolVal b -> if b then v1 else v2
                            _         -> ifExn
              ; return (IfExp ce e1 e2, val)
              }
                      


                                  
--- ### Functions

arbEnv :: Gen Env
arbEnv = do { ps <- resize 3 $ listOf arbParam
            ; vs <- listOf arbVal
            ; return $ H.fromList $ zip ps vs
            }

-- replace with arbitrary expressions
arbFunExp :: Gen (Exp, Val, Env)
arbFunExp = do { (e,_) <- arbExp
               ; ps <- resize 3 $ listOf arbParam
               ; env <- arbEnv
               ; return (FunExp ps e, CloVal ps e env, env)
               }
    where arbExp = oneof [ arbIntOpExp 1, arbCompOpExp 1
                         , arbBoolOpExp 1,  arbConstExp, arbVarExp ]

arbEnvList :: Gen [(String, Bool)]
arbEnvList = do { ps <-  arbParamList
                ; length <- choose (0,5)
                ; vs <- listOf arbitrary
                ; return $ zip (take length ps)  vs
                }

arbNestedVarExp :: [(String, Bool)] -> Gen (Exp,Val,[(String,Exp)])
arbNestedVarExp [] = do { (e,v) <- arbBoolExp
                        ; return (e,v,[])
                        }
arbNestedVarExp ((p,b1):ps) =
    do { (opS,op) <- arbBoolOp
       ; (e, BoolVal b2, env) <- arbNestedVarExp ps
       ; return ( BoolOpExp opS (VarExp p) e
                , BoolVal (b1 `op` b2)
                , (p, BoolExp b1):env )
       }

arbAppExp :: Gen (Exp,Val,Env)
arbAppExp = do { env <- arbEnv
               ; argParam <- arbEnvList
               ; (body, val, argExpList) <- arbNestedVarExp argParam
               ; let (ps,expList) = unzip argExpList
               ; frequency [ (9, return (AppExp (FunExp ps body) expList, val, env))
                           , (1, return (AppExp body expList, funExn, env))  ]
               }

--- ### Let Expressions
arbLetExp :: Gen (Exp,Val,Env)
arbLetExp = do { env <- arbEnv
               ; argParam <- arbEnvList
               ; (body, val, argExpList) <- arbNestedVarExp argParam
               ; return (LetExp argExpList body, val, env)
               }



-- ~~~~~~~~~~~~~ here be dragons ~~~~~~~~~~~~~~
-- TODO: Exec properties need to be converted
--       to the `forall generator property` style

-- ######## 
-- newtypes 
-- ########


----- Operator Strings -------
newtype IntOp = IntOp (String, Int -> Int -> Int)
instance Show IntOp where
    show (IntOp (s,_))  = s

newtype BoolOp = BoolOp (String, Bool -> Bool -> Bool)
instance Show BoolOp where
    show (BoolOp (s,_))  = s

newtype CompOp = CompOp (String, Int -> Int -> Bool)
instance Show CompOp where
    show (CompOp (s,_))  = s
------------------------------
-- Variable names
newtype Param = Param { getString :: String }
    deriving (Show)

newtype ParamList = ParamList { getStrings :: [String] }
instance Show ParamList where
    show (ParamList ps) =  init ( show (take 7 ps) ) ++ "..."

---------------------
--Arbitrary Intances-
---------------------

-- Operator Strings
instance Arbitrary IntOp where
    arbitrary =  IntOp <$> elements (H.toList  intOps)
instance Arbitrary BoolOp where
    arbitrary = BoolOp <$> elements (H.toList boolOps)
instance Arbitrary CompOp where
    arbitrary = CompOp <$> elements (H.toList compOps)

-- Values
instance Arbitrary Val where
  arbitrary = oneof [ IntVal <$> arbitrary
                    , BoolVal <$> arbitrary
                    ]
                
instance Arbitrary Param where
    arbitrary = Param <$> (elements $ map (:"") ['a'..'z'])

instance Arbitrary ParamList where
    arbitrary = ParamList <$> (shuffle $ map (:"") ['a'..'z'])

--- exec
--- ----

--- ### Assignment Statements

setStmt_prop :: ParamList -> NonZero Int -> IntOp -> Property
setStmt_prop (ParamList (f:x:_)) (NonZero i) (IntOp (iopS, iop)) = 
    (exec (SeqStmt [(SetStmt f (FunExp [x] (IntOpExp iopS (VarExp x) (IntExp i))))
                   , PrintStmt (AppExp (VarExp f) [IntExp i])]) H.empty H.empty ===
     (show (i `iop` i), H.empty, H.fromList [(f, CloVal [x] (IntOpExp iopS (VarExp x) (IntExp i)) H.empty)])) .&.
    (exec (SetStmt x (IntExp i)) H.empty H.empty == ("", H.empty, H.fromList [(x, IntVal i)]))


--- ### Sequence Statements

seqStmt_prop :: Int -> Int -> Param -> Property
seqStmt_prop i1 i2 (Param p) =
    ((exec (SeqStmt [PrintStmt (IntExp i1), PrintStmt (IntExp i2)]) H.empty H.empty === (show i1 ++ show i2, H.empty, H.empty)) .&.
     (exec (SeqStmt [PrintStmt (VarExp p), PrintStmt (IntExp i2)]) H.empty H.empty === ("exn: No match in env" ++ show i2, H.empty, H.empty)))



--- ### If Statements

ifStmt_prop :: Int -> Int -> ParamList -> CompOp -> Property
ifStmt_prop i1 i2 (ParamList (f:p1:p2:_)) (CompOp (copS,cop)) =
    (( (exec (IfStmt (AppExp (VarExp f) [IntExp i1, IntExp i2]) (PrintStmt (IntExp i1)) (PrintStmt (IntExp i2))) H.empty env) === (if i1 `cop` i2 then show i1 else show i2, H.empty, env)
     ) .&.
     ( (exec (IfStmt (FunExp [] (IntExp 0)) (PrintStmt (IntExp 5)) (PrintStmt (IntExp 10))) H.empty H.empty) === ("exn: Condition is not a Bool", H.empty, H.empty)))
    where env = H.fromList [(f, CloVal [p1,p2] (CompOpExp copS (VarExp p1) (VarExp p2)) H.empty)]


--- ### Procedure Declaration

testPenv f1 f2 f3 p1 p2 p3 i1 i2 b =
    (H.fromList
     [ (f1, ProcedureStmt f1 [] (SetStmt p1 (IntExp 5)))
     , (f2, ProcedureStmt f2 [p1] (IfStmt (CompOpExp "<" (VarExp p1) (IntExp i2))
                                   (SeqStmt [PrintStmt (VarExp p1),
                                             SetStmt p1 (IntOpExp "+" (VarExp p1) (IntExp 1)),
                                             CallStmt f2 [VarExp p1]]) (PrintStmt (BoolExp b))))
     , (f3, ProcedureStmt f3 [p1, p2, p3] (SetStmt p3 (AppExp (VarExp p1) [(AppExp (VarExp p2) [VarExp p3])])))
     ]
    )

procStmt_prop :: ParamList -> Int -> Positive Int -> Bool -> Property
procStmt_prop (ParamList (f1:f2:f3:p1:p2:p3:_)) i1 (Positive i2) b =
    ((exec (ProcedureStmt f2 [] (PrintStmt (BoolExp b))) penv H.empty === ("", H.insert f2 (ProcedureStmt f2 [] (PrintStmt (BoolExp b))) penv, H.empty)) .&.
     (exec (SeqStmt [SetStmt p1 (IntExp i1), ProcedureStmt f1 [] (PrintStmt (VarExp p1))]) H.empty H.empty ===
      ("", H.singleton f1 (ProcedureStmt f1 [] (PrintStmt (VarExp p1))), H.singleton p1 (IntVal i1)) )
    )
    where penv = testPenv f1 f2 f3 p1 p2 p3 i1 i2 b


                 
--- ### Procedure Call

callStmt_prop :: ParamList -> Int -> Positive Int -> Bool -> Property
callStmt_prop (ParamList (f1:f2:f3:p1:p2:p3:p0:_)) i1 (Positive i2) b =
    ( (i1 <= i2 ==>
              (exec (CallStmt f2 [IntExp i1]) penv H.empty === (concatMap (show) (init [i1..i2]) ++ show b, penv, H.singleton p1 (IntVal i2)))
      ) .&.
      (exec (CallStmt p0 []) penv H.empty === ("Procedure " ++ p0 ++ " undefined", penv, H.empty))
    )
    where penv = testPenv f1 f2 f3 p1 p2 p3 i1 i2 b
