type Name = String

-- types
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

substTVar :: Name -> Type -> Type -> Type
substTVar x s t@(TVar y) = if x == y then s else t
substTVar _ _ t@(TMeta _) = t
substTVar x s (TApp a b) = TApp (substTVar x s a) (substTVar x s b)
substTVar x s t@(TForall y b) = if x == y then t else TForall y (substTVar x s b)

hasTMeta :: Name -> Type -> Bool
hasTMeta _ (TVar _) = False
hasTMeta x (TMeta y) = x == y
hasTMeta x (TApp a b) = hasTMeta x a || hasTMeta x b
hasTMeta x (TForall _ b) = hasTMeta x b

hasTVar :: Name -> Type -> Bool
hasTVar x (TVar y) = x == y
hasTVar _ (TMeta _) = False
hasTVar x (TApp a b) = hasTVar x a || hasTVar x b
hasTVar x (TForall y b) = x /= y && hasTVar x b

namesInType :: Type -> [Name]
namesInType (TVar x) = [x]
namesInType (TMeta x) = [x]
namesInType (TApp a b) = (namesInType a) ++ (namesInType b)
namesInType (TForall x b) = x : namesInType b

-- terms
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

-- Context
data Elem
  = CTVar Name
  | CTMeta Name (Maybe Type)
  | CMarker Name
  | CVar Name Type
  deriving (Eq)

instance Show Elem where
  show (CTVar x) = x
  show (CTMeta x Nothing) = "?" ++ x
  show (CTMeta x (Just t)) = "?" ++ x ++ " = " ++ (show t)
  show (CMarker x) = "|>" ++ x
  show (CVar x t) = x ++ " : " ++ (show t)

newtype Context = Context [Elem]

instance Show Context where
  show (Context a) = show (reverse a)

context :: [Elem] -> Context
context = Context . reverse

names :: Context -> [Name]
names (Context c) = go c
  where
    go [] = []
    go (CTVar x : r) = x : go r
    go (CTMeta x _ : r) = x : go r
    go (CMarker x : r) = x : go r
    go (CVar x _ : r) = x : go r

lookupTMeta :: Context -> Name -> Maybe (Maybe Type)
lookupTMeta (Context ctx) x = go ctx x
  where
    go [] _ = Nothing
    go (CTMeta x' t : _) y | x' == y = return t
    go (_ : r) y = go r y

apply :: Context -> Type -> Type
apply _ t@(TVar _) = t
apply ctx m@(TMeta x) =
  case lookupTMeta ctx x of
    Just t -> maybe m (apply ctx) t
    Nothing -> m
apply ctx (TApp a b) = TApp (apply ctx a) (apply ctx b)
apply ctx (TForall x t) = TForall x (apply ctx t)

add :: Elem -> Context -> Context
add e (Context c) = Context (e : c)

-- monad
type Infer t = Either String t

err :: String -> Infer t
err msg = Left msg

freshIn :: Name -> [Name] -> Name
freshIn x ns = if elem x ns then freshIn (x ++ "'") ns else x

freshName :: Name -> Context -> [Type] -> Name
freshName x ctx t = freshIn x (names ctx ++ (t >>= namesInType))

-- unification
unifyTMeta :: Context -> Name -> Type -> Infer Context
unifyTMeta ctx x (TMeta y) | x == y = return ctx
unifyTMeta _ x t | hasTMeta x t = err $ "?" ++ x ++ " occurs in " ++ (show t)
unifyTMeta ctx x t = err $ "unimplemented"

unify :: Context -> Type -> Type -> Infer Context
unify ctx (TVar x) (TVar y) | x == y = return ctx
unify ctx (TApp a1 b1) (TApp a2 b2) = do
  ctx' <- unify ctx a1 a2
  unify ctx' (apply ctx' b1) (apply ctx' b2)
unify ctx t1@(TForall x1 b1) t2@(TForall x2 b2) = do
  let x = freshName x1 ctx [t1, t2]
  let tv = TVar x
  ctx' <- unify (add (CTVar x) ctx) (substTVar x1 tv b1) (substTVar x2 tv b2)
  check (not $ hasTVar x (apply ctx' t1)) $ ""
  check (not $ hasTVar x (apply ctx' t2)) $ ""
  return ctx'
unify ctx (TMeta x) t = unifyTMeta ctx x t
unify ctx t (TMeta x) = unifyTMeta ctx x t
unify ctx a b = err $ "failed to unify " ++ (show a) ++ " ~ " ++ (show b) ++ " in " ++ (show ctx)

-- testing
initialctx :: Context
initialctx = context [CTVar "->"]

term :: Term
term = Abs "x" (Just $ TForall "t" $ tfun (TVar "t") (TVar "t")) (Var "x")

main :: IO ()
main = putStrLn $ show term
