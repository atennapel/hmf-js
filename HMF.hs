type Name = String

data Type
  = TVar Name
  | TMeta Name
  | TApp Type Type
  | TForall Name Type
  deriving (Eq)

tfunC :: Type
tfunC = TVar "->"

instance Show Type where
  show (TVar x) = x
  show (TMeta x) = "?" ++ x
  show (TApp (TApp c a) b) | c == tfunC = "(" ++ (show a) ++ " -> " ++ (show b) ++ ")"
  show (TApp a b) = "(" ++ (show a) ++ " " ++ (show b) ++ ")"
  show (TForall x b) = "(forall " ++ x ++ ". " ++ (show b) ++ ")"

tfun :: Type -> Type -> Type
tfun a b = TApp (TApp tfunC a) b

data Term
  = Var Name
  | Abs Name (Maybe Type) Term
  | App Term Term
  | Ann Term Type

instance Show Term where
  show (Var x) = x
  show (Abs x Nothing b) = "(\\" ++ x ++ " -> " ++ (show b) ++ ")"
  show (Abs x (Just t) b) = "(\\(" ++ x ++ " : " ++ (show t) ++ " -> " ++ (show b) ++ "))"
  show (App a b) = "(" ++ (show a) ++ " " ++ (show b) ++ ")"
  show (Ann e t) = "(" ++ (show e) ++ " : " ++ (show t) ++ ")"

term :: Term
term = Abs "x" (Just $ TForall "t" $ tfun (TVar "t") (TVar "t")) (Var "x")

main :: IO ()
main = putStrLn $ show term
