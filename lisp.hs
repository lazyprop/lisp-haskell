import Data.HashMap.Strict (HashMap, (!?))
import qualified Data.HashMap.Strict as HashMap
import Data.Maybe (fromJust)

data LispExpr = Symbol String
                | Number Integer
                | List   [LispExpr]
                | Bool   Bool
                | Func   LispFunc
                | Cons   LispExpr LispExpr
                | Null
                deriving (Show, Eq)

data LispFunc = LispFunc [LispExpr] [LispExpr] -- params, body
              | PrimitiveFunc String
              deriving (Show, Eq)

type LispEnv = [HashMap String LispExpr]


-- ** extract stuff **

extractFunc :: LispExpr -> Maybe LispFunc
extractFunc (Func f) = Just f
extractFunc _        = Nothing

extractSymbol :: LispExpr -> Maybe String
extractSymbol (Symbol s) = Just s
extractSymbol _          = Nothing

extractNum :: LispExpr -> Maybe Integer
extractNum (Number s) = Just s
extractNum _          = Nothing

extractBool :: LispExpr -> Maybe Bool
extractBool (Bool s) = Just s
extractBool _        = Nothing



-- ** environment stuff **

findEnv :: LispEnv -> String -> Maybe LispExpr
findEnv []         _    = Nothing
findEnv (top:rest) name = case (top !? name) of
                            Nothing -> findEnv rest name
                            Just expr -> Just expr

defaultEnv :: LispEnv
defaultEnv = [ HashMap.fromList
                [ ("first"
                  , Func (LispFunc [ Symbol "x" , Symbol "y" ]
                                   [ Symbol "x" ]))
                , ("second"
                  , Func (LispFunc [ Symbol "x" , Symbol "y" ]
                                   [ Symbol "y" ]))
                , ("+", Func (PrimitiveFunc "+"))
                , ("cons", Func (PrimitiveFunc "cons"))
                , ("null", Func (PrimitiveFunc "null"))
                , ("eq?", Func (PrimitiveFunc "eq?")) ] ]



-- ** eval stuff **

applyPrimitive :: String -> [LispExpr] -> Maybe LispExpr
applyPrimitive "+" args = sequence (map extractNum args) >>= Just . Number . sum
applyPrimitive "cons" args = if length args == 2
                                then Just $ Cons (head args) (head $ tail args)
                                else Nothing
applyPrimitive "eq?" args = if length args == 2
                               then Just $ Bool $ (head args) == (head $ tail args)
                               else Nothing

applyFunc :: LispFunc -> [LispExpr] -> LispEnv -> Maybe LispExpr
applyFunc (PrimitiveFunc name)     args env = applyPrimitive name args
applyFunc (LispFunc argNames body) args env = do
    names <- sequence $ map extractSymbol argNames
    let newFrame = HashMap.fromList $ zip names args
    eval (last body) (newFrame : env)


eval :: LispExpr -> LispEnv -> Maybe LispExpr
eval (List (e:es)) env = do
    res <- eval e env
    func <- extractFunc res
    args <- sequence $ map (flip eval env) es -- no need to eval in applyFunc
    applyFunc func args env
eval (Symbol s) env    = findEnv env s
eval expr       _      = Just expr


-- ** testing stuff ** 

testExpr1 = List [Symbol "first", Number 1, Number 2]
testExpr2 = List [Symbol "second", Number 3, Number 4]
testExpr3 = List [Symbol "first", testExpr1, testExpr2]
testExpr4 = List [Symbol "second", testExpr1, testExpr2]
testExpr5 = List [Symbol "+", Number 1, Number 2]
testExpr6 = List [Symbol "+", Number 1, Symbol "x"]  -- Nothing
testExpr7 = List [Symbol "cons", Number 1, Number 2]
testExpr8 = List [Symbol "eq?", Number 1, Number 1]
testExpr9 = List [Symbol "eq?", Number 1, Number 2]
