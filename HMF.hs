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

isMono :: Type -> Bool
isMono (TForall _ _) = False
isMono (TApp a b) = isMono a && isMono b
isMono _ = True

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
  | Let Name Term Term

instance Show Term where
  show (Var x) = x
  show (Abs x Nothing b) = "(\\" ++ x ++ " -> " ++ (show b) ++ ")"
  show (Abs x (Just t) b) = "(\\(" ++ x ++ " : " ++ (show t) ++ " -> " ++ (show b) ++ "))"
  show (App a b) = "(" ++ (show a) ++ " " ++ (show b) ++ ")"
  show (Ann e t) = "(" ++ (show e) ++ " : " ++ (show t) ++ ")"
  show (Let x a b) = "(let " ++ x ++ " = " ++ (show a) ++ " in " ++ (show b) ++ ")"

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

hasElem :: Context -> Elem -> Bool
hasElem (Context c) e = go c e
  where
    go [] e = False
    go (e' : _) e | e == e' = True
    go (_ : t) e = go t e

lookupTMeta :: Context -> Name -> Maybe (Maybe Type)
lookupTMeta (Context ctx) x = go ctx x
  where
    go [] _ = Nothing
    go (CTMeta x' t : _) y | x' == y = return t
    go (_ : r) y = go r y

lookupVar :: Context -> Name -> Maybe Type
lookupVar (Context ctx) x = go ctx x
  where
    go [] _ = Nothing
    go (CVar x' t : _) y | x' == y = return t
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

split :: Elem -> Context -> (Context, Context)
split e (Context c) = let (a, b) = go e c [] in (Context a, context b)
  where
    go e [] a = ([], a)
    go e (e' : t) a | e == e' = (t, a)
    go e (e' : t) a = go e t (e' : a)

dropTo :: Elem -> Context -> Context
dropTo e c = fst (split e c) 

-- monad
type Infer t = Either String t

err :: String -> Infer t
err msg = Left msg

freshIn :: Name -> [Name] -> Name
freshIn x ns = if elem x ns then freshIn (x ++ "'") ns else x

freshName :: Name -> Context -> [Type] -> Name
freshName x ctx t = freshIn x (names ctx ++ (t >>= namesInType))

-- wellformedness
wfType :: Context -> Type -> Infer ()
wfType ctx (TVar x) =
  if hasElem ctx (CTVar x) then
    return ()
  else
    err $ "wfType: tvar " ++ x ++ " not found in " ++ (show ctx)
wfType ctx (TMeta x) =
  case lookupTMeta ctx x of
    Just _ -> return ()
    Nothing -> err $ "wfType: tmeta ?" ++ x ++ " not found in " ++ (show ctx)
wfType ctx (TApp a b) = do
  wfType ctx a
  wfType ctx b
wfType ctx (TForall x b) =
  let x' = freshName x ctx [b] in
  wfType (add (CTVar x') ctx) (substTVar x (TVar x') b)

-- unification
solve :: Context -> Name -> Type -> Infer Context
solve ctx@(Context c) x t = fmap Context (go c x t [] [])
  where
    go [] x _ _ _ = err $ "tmeta ?" ++ x ++ " not found in " ++ (show ctx)
    go (CTMeta x' (Just _) : _) x _ _ _ | x == x' = err $ "tmeta ?" ++ x ++ " is already solved in " ++ (show ctx)
    go (CTMeta x' Nothing : r) x t a b | x == x' =
      return $ (reverse b) ++ (CTMeta x (Just t) : reverse a) ++ r
    go (CTMeta x' t' : r) x t a b | hasTMeta x' t =
      case t' of
        Just _ -> err $ "unapplied type " ++ (show t) ++ " in solve in " ++ (show ctx)
        Nothing -> go r x t (CTMeta x' t' : a) b
    go (e : r) x t a b = go r x t a (e : b)

unifyTMeta :: Context -> Name -> Type -> Infer Context
unifyTMeta ctx x (TMeta y) | x == y = return ctx
unifyTMeta _ x t | hasTMeta x t = err $ "?" ++ x ++ " occurs in " ++ (show t)
unifyTMeta ctx x t = solve ctx x t

unify :: Context -> Type -> Type -> Infer Context
unify ctx (TVar x) (TVar y) | x == y = return ctx
unify ctx (TApp a1 b1) (TApp a2 b2) = do
  ctx' <- unify ctx a1 a2
  unify ctx' (apply ctx' b1) (apply ctx' b2)
unify ctx t1@(TForall x1 b1) t2@(TForall x2 b2) = do
  let x = freshName x1 ctx [t1, t2]
  let tv = TVar x
  ctx' <- unify (add (CTVar x) ctx) (substTVar x1 tv b1) (substTVar x2 tv b2)
  let ctx'' = dropTo (CTVar x) ctx'
  wfType ctx'' (apply ctx'' t1)
  wfType ctx'' (apply ctx'' t2)
  return ctx''
unify ctx (TMeta x) t = unifyTMeta ctx x t
unify ctx t (TMeta x) = unifyTMeta ctx x t
unify ctx a b = err $ "failed to unify " ++ (show a) ++ " ~ " ++ (show b) ++ " in " ++ (show ctx)

subsume :: Context -> Type -> Type -> Infer Context
subsume ctx (TForall x b) t =
  let x' = freshName x ctx [b, t] in
  subsume (add (CTMeta x' Nothing) ctx) (substTVar x (TMeta x') b) t
subsume ctx t t'@(TForall x b) = do
  let x' = freshName x ctx [b, t]
  ctx' <- subsume (add (CTVar x') ctx) t (substTVar (TVar x') b)
  let ctx'' = dropTo (CTVar x') ctx'
  wfType ctx'' (apply ctx'' t)
  wfType ctx'' (apply ctx'' t')
  return ctx''
subsume ctx t t' = unify ctx t t'

-- inference
synth :: Context -> Term -> Infer (Context, Type)
synth ctx (Var x) =
  case lookupVar ctx x of
    Just t -> return (ctx, t)
    Nothing -> err $ "undefined var " ++ x ++ " in " ++ (show ctx)
synth ctx (Let x a b) = do
  (ctx', ty) <- synth ctx a
  synth (add (CVar x ty) ctx') b
synth ctx (Ann a t) = do
  ctx' <- check ctx a t
  return (ctx', t)
synth ctx (Abs x Nothing b) =

synth ctx (Abs x (Just t) b) =

synth ctx (App a b) =
synth ctx t = err $ "cannot synth " ++ (show t) ++ " in " ++ (show ctx)

check :: Context -> Term -> Type -> Infer Context
check ctx a t = do
  (ctx', t') <- synth ctx a
  subsume ctx' t' t

-- testing
initialctx :: Context
initialctx = context [CTVar "->"]

term :: Term
term = Abs "x" (Just $ TForall "t" $ tfun (TVar "t") (TVar "t")) (Var "x")

main :: IO ()
main = do
  putStrLn $ show term
  case synth initialctx term of
    Left msg -> putStrLn msg
    Right (ctx, ty) -> do
      putStrLn $ show ty
      putStrLn $ show ctx
