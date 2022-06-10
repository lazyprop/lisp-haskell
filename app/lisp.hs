import Data.HashMap.Strict (HashMap, (!?))
import qualified Data.HashMap.Strict as HashMap
import Data.Maybe (fromJust)
import Control.Monad.State

data LispExpr = Symbol String
              | Number Integer
              | List   [LispExpr]
              | LBool  Bool
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
extractBool (LBool s) = Just s
extractBool _        = Nothing

extractCons :: LispExpr -> Maybe (LispExpr, LispExpr)
extractCons (Cons l r) = Just (l, r)
extractCons _          = Nothing



-- ** environment stuff **

getEnv :: LispEnv -> String -> Maybe LispExpr
getEnv []         _    = Nothing
getEnv (top:rest) name = case (top !? name) of
                           Nothing -> getEnv rest name
                           Just expr -> Just expr


primitiveFromName :: String -> (String, LispExpr)
primitiveFromName name = (name, Func (PrimitiveFunc name))

defaultEnv :: LispEnv
defaultEnv = [HashMap.fromList $ map primitiveFromName
                [ "+", "*", "*", "-"
                , "<", "<=", ">", ">=", "eq?"
                , "cons", "null", "null?", "car", "cdr"
                ]]


-- ** eval stuff **

applyPrimitive :: String -> [LispExpr] -> Maybe LispExpr
applyPrimitive name args =
    case name of
      -- arithmetic stuff
      "+" -> sequence (map extractNum args) >>= Just . Number . sum
      "*" -> sequence (map extractNum args) >>= Just . Number . product
      "-" -> getTwoNums >>= \(l, r) -> Just . Number $ l - r
      "eq?" -> ifArgc (==2) $ LBool $ (args !! 0) == (args !! 1)
      "=" -> getTwoNums >>= \(l, r) -> Just . LBool $ l == r
      "<" -> getTwoNums >>= \(l, r) -> Just . LBool $ l < r
      "<=" -> getTwoNums >>= \(l, r) -> Just . LBool $ l <= r
      ">" -> getTwoNums >>= \(l, r) -> Just . LBool $ l > r
      ">=" -> getTwoNums >>= \(l, r) -> Just . LBool $ l >= r

      -- cons stuff
      "cons" -> ifArgc (==2) $ Cons (args !! 0) (args !! 1)
      "null" -> Just Null
      "null?" -> ifArgc (==1) $ LBool (head args == Null)
      -- make this beautiful
      "car" -> case args of
                 [Cons l r] -> Just l
                 _          -> Nothing
      "cdr" -> case args of
                 [Cons l r] -> Just r
                 _          -> Nothing
  where
      ifArgc :: (Int -> Bool) -> LispExpr -> Maybe LispExpr
      ifArgc pred res = if (pred (length args))
                           then Just res
                           else Nothing

      getTwoNums :: Maybe (Integer, Integer)
      getTwoNums = if (length args == 2)
                      then sequence (map extractNum args) >>=
                          \[l, r] -> Just (l, r)
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
eval (Symbol s) env    = getEnv env s
eval expr       _      = Just expr


-- ** testing stuff ** 

testExpr1  = List [Symbol "+", Number 1, Number 2]
testExpr2  = List [Symbol "+", Number 1, Symbol "x"]  -- Nothing
testExpr3  = List [Symbol "cons", Number 1, Number 2]
testExpr4  = List [Symbol "eq?", Number 1, Number 1]
testExpr5  = List [Symbol "null?", Null]
testExpr6  = List [Symbol "car", testExpr3]
testExpr7  = List [Symbol "cdr", testExpr3]
testExpr8  = List [Symbol "=", Number 1, Number 1]
testExpr9  = List [Symbol "eq?", Symbol "+", Symbol "+"]
testExpr10 = List [Symbol "eq?", Symbol "f", Symbol "f"] -- Nothing
