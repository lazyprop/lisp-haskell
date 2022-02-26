import Data.HashMap.Strict (HashMap, (!?))
import qualified Data.HashMap.Strict as HashMap
import Data.Maybe (fromJust)

data LispExpr = Symbol String
              | Number Integer
              | List   [LispExpr]
              | LBool   Bool
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



-- ** environment stuff **

findEnv :: LispEnv -> String -> Maybe LispExpr
findEnv []         _    = Nothing
findEnv (top:rest) name = case (top !? name) of
                            Nothing -> findEnv rest name
                            Just expr -> Just expr


primitiveFromName :: String -> (String, LispExpr)
primitiveFromName name = (name, Func (PrimitiveFunc name))

defaultEnv :: LispEnv
defaultEnv = [HashMap.fromList $ map primitiveFromName
                [ "+", "*", "*", "-"
                , "<", "<=", ">", ">="
                , "cons", "null", "eq?", "null?"
                ]]


-- ** eval stuff **

applyPrimitive :: String -> [LispExpr] -> Maybe LispExpr
applyPrimitive name args =
    case name of
      "+" -> sequence (map extractNum args) >>= Just . Number . sum
      "*" -> sequence (map extractNum args) >>= Just . Number . product
      "-" -> getTwoNums >>= \(l, r) -> Just . Number $ l - r
      "eq?" -> ifArgc (==2) $ LBool $ (args !! 0) == (args !! 1)
      "cons" -> ifArgc (==2) $ Cons (args !! 0) (args !! 1)
      "<" -> getTwoNums >>= \(l, r) -> Just . LBool $ l < r
      "<=" -> getTwoNums >>= \(l, r) -> Just . LBool $ l <= r
      ">" -> getTwoNums >>= \(l, r) -> Just . LBool $ l > r
      ">=" -> getTwoNums >>= \(l, r) -> Just . LBool $ l >= r
      "null?" -> ifArgc (==1) $ LBool (head args == Null)
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
eval (Symbol s) env    = findEnv env s
eval expr       _      = Just expr


-- ** testing stuff ** 

testExpr1 = List [Symbol "+", Number 1, Number 2]
testExpr2 = List [Symbol "+", Number 1, Symbol "x"]  -- Nothing
testExpr3 = List [Symbol "cons", Number 1, Number 2]
testExpr4 = List [Symbol "eq?", Number 1, Number 1]
testExpr5 = List [Symbol "eq?", Number 1, Number 2]
testExpr6 = List [Symbol "null?", Null]
