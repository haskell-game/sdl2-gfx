{-|

Module      : SDL.Raw.Helper
Copyright   : (c) 2015 Siniša Biđin
License     : MIT
Maintainer  : sinisa@bidin.cc
Stability   : experimental

Exposes a way to automatically generate a foreign import alongside its lifted,
inlined MonadIO variant. Use this to simplify the package's SDL.Raw.* modules.

-}

{-# LANGUAGE BangPatterns    #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE TemplateHaskell #-}

module SDL.Raw.Helper where

import Control.Monad           (replicateM)
import Control.Monad.IO.Class  (MonadIO, liftIO)
import Language.Haskell.TH

-- | Given a name @fname@, a name of a C function @cname@ and the desired
-- Haskell type @ftype@, this function generates:
--
-- * A foreign import of @cname@, named as @fname'@.
-- * An always-inline MonadIO version of @fname'@, named @fname@.
liftF :: String -> String -> Q Type -> Q [Dec]
liftF fname cname ftype = do
  let f' = mkName $ fname ++ "'" -- Direct binding.
  let f  = mkName fname          -- Lifted.
  t' <- ftype                    -- Type of direct binding.
  t  <- liftToMonadIO t'         -- Type of lifted binding.

  -- The generated function accepts n arguments.
  args <- replicateM (countArgs t') $ newName "x"

  return
    [
      PragmaD $ InlineP f Inline FunLike AllPhases
    , SigD f t
    , FunD f
        [ Clause
            (map VarP args)
            (NormalB $ 'liftIO `applyTo` [f' `applyTo` map VarE args])
            []
        ]
    , ForeignD $ ImportF CCall Unsafe cname f' t'
    ]

-- | How many arguments does a function of a given type take?
countArgs :: Type -> Int
countArgs = count 0
  where
    count !n = \case
      (AppT (AppT ArrowT _) t) -> count (n+1) t
      (ForallT _ _ t) -> count n t
      (SigT t _)      -> count n t
      _               -> n

-- | Changes a function's type to return a MonadIO action instead of an IO one.
liftToMonadIO :: Type -> Q Type
liftToMonadIO t' = do
  m <- newName "m"
  return $ ForallT
    [PlainTV m]
    [AppT (ConT ''MonadIO) (VarT m)]
    (replaceLast m t')

-- | Replaces the last type constructor to a given one.
replaceLast :: Name -> Type -> Type
replaceLast m = \case
  (ForallT a b t)            -> ForallT a b $ replaceLast m t
  (SigT t k)                 -> SigT (replaceLast m t) k
  (AppT a@(AppT ArrowT _) t) -> AppT a $ replaceLast m t
  (AppT (ConT _) r)          -> AppT (VarT m) r
  _                          -> error "SDL.Raw.Helper.liftF: I goofed up."

-- | An expression where f is applied to n arguments.
applyTo :: Name -> [Exp] -> Exp
applyTo f [] = VarE f
applyTo f es = loop (tail es) . AppE (VarE f) $ head es
  where
    loop []     e = e
    loop (a:as) e = loop as $ AppE e a
