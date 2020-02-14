{-# LANGUAGE LambdaCase, TupleSections #-}
module Language.Java.Syntax.Util where

import Language.Haskell.TH
import Data.Maybe
import Control.Monad

-- | @replaceTypeWhere needle replacement haystack@ replaces every occurrence of
-- @needle@ in @haystack@ with @replacement@
--
-- Caveats:
--    * Does not investigate some of the newer constructors of 'Type', such
--      as 'AppKindT' and 'ImplicitParamT'.
--    * If @needle@ is a type variable this does not correctly handle 'ForallT' (it
--      will variables bound by the forall)
replaceTypeWhere :: Type -> Type -> Type -> Type
replaceTypeWhere target replacement = go
  where
    go = \case
        a | a == target -> replacement
        ForallT b c t -> ForallT b c (go t)
        AppT a b -> AppT (go a) (go b)
        SigT t k -> SigT (go t) k
        InfixT t1 n t2 -> InfixT (go t1) n (go t2)
        UInfixT t1 n t2 -> UInfixT (go t1) n (go t2)
        ParensT t -> ParensT (go t)
        other -> other

-- | Given a type declaration such as
-- @
--   data MyTypeF a b
--       = Con1F T1 b a
--       | Con2F T2
-- @
-- This creates the following definitions
-- @
--   type MyType b = MyType b ()
--   pattern Con1 b :: -> b -> MyType
--   pattern Con1 a b = Con1F a b ()
--   pattern Con2 :: T2 -> MyType
--   pattern Con2 a = Con2F b
-- @
-- i.e. it
-- 1. creates a type alias that is the original type name with the last character
--    removed and the last type parameter instantiated as @()@
-- 2. for each constructor it creates a pattern (and signature) which omits all
--    fields of the same type as the last, stripped type parameter removed and
--    on the rhs filled with @()@
unfunctor :: Name -> Q [Dec]
unfunctor =
  reify >=> \case
    TyConI dec -> do
      fpats <- concatMap snd <$> filterM (fmap isNothing . lookupValueName . fst) pats
      return $
          TySynD unfunctorName (reverse rest) (functorType `AppT` unitT) : fpats
      where pats =
                map rewriteConstr constrs
            functorType = applyVars functorName
            unfunctorName = rename functorName
            unfunctorType = applyVars unfunctorName
            applyVars name =
              foldl AppT (ConT name) $ map (VarT . unVarBndr) $ reverse rest
            unitT = ConT $ tupleTypeName 0
            unVarBndr (PlainTV a) = a
            unVarBndr (KindedTV a _) = a
            rename = mkName . renameStr
            renameStr name' = newName'
              where
                base = nameBase name'
                newName' = take (length base - 1) base
            functorVar:rest = reverse bndrs
            functorVarT = VarT $ unVarBndr functorVar
            rewriteConstr con@(NormalC conName fields) = (patternNameStr,)
              [ PatSynSigD
                  patternName
                  (foldr (\x y -> ArrowT `AppT` x `AppT` y) unfunctorType newTypes)
              , PatSynD
                  patternName
                  (PrefixPatSyn newVars)
                  ImplBidir
                  (ConP
                     conName
                     (map (maybe (ConP (tupleDataName 0) []) (VarP . snd)) newFields))
              ]
              where
                (newTypes, newVars) = unzip $ catMaybes newFields
                patternNameStr = renameStr conName
                patternName = mkName patternNameStr
                newFields :: [Maybe (Type, Name)]
                newFields =
                  reverse $
                  fst $ foldl f ([], map (mkName . pure) ['a' .. 'z']) fields
                f (vars, reservoir@(newVar:restVars)) (_, ty')
                  | ty' == functorVarT =
                    (Nothing : vars, reservoir)
                  | otherwise =
                    (Just (rewriteType ty', newVar) : vars, restVars)
                f _ _ =
                  error $
                  "Too many type variables needed for constructor " ++ show con
            rewriteConstr con =
              error $ "Cannot handle constructor type " ++ show con
            rewriteType = replaceTypeWhere functorVarT unitT
            (functorName, bndrs, constrs) =
              case dec of
                DataD _ctx name' bndrs' _kind constrs' _derives ->
                  (name', bndrs', constrs')
                NewtypeD _ctx name' bndrs' _kind constr' _derives ->
                  (name', bndrs', [constr'])
                _ -> error $ "Unrecognized declaration type " ++ show dec
    info -> fail $ "Unrecognized entity " ++ show info
