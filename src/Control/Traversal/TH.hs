{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
module TH.Traversal where

import Control.Monad
import Data.Bool
import Data.Foldable
import qualified Data.List as List
import Data.Traversable
import Language.Haskell.TH.Syntax
import Util ((∈))

makeTraversal :: Type -> Exp -> Name -> Q Exp
makeTraversal theType fExpr = reify >=> \ case
    TyConI (DataD _ _ [] _ cons _derivs) -> LamCaseE <$> for cons \ con ->
      [Match
       (ConP conName $ VarP . fst <$> xs)
       (NormalB $
        foldl' (AppE . AppE apExpr) (AppE pureExpr $ ConE conName)
        [AppE (bool pureExpr fExpr (theType ∈ typeHeads t)) (VarE v)
        | (v, t) <- xs]) []
      | (conName, xs) <- (traverse . traverse) ((<$> newName "a") . flip (,)) $
            let go = \ case
                    NormalC name bts -> (name, [t | (_, t) <- bts])
                    RecC name vbts -> (name, [t | (_, _, t) <- vbts])
                    InfixC lbt name rbt -> (name, [t | (_, t) <- [lbt, rbt]])
                    ForallC _ _ con -> go con
                    GadtC names bts _ -> (head names, [t | (_, t) <- bts])
                    RecGadtC names vbts _ -> (head names, [t | (_, _, t) <- vbts])
            in go con
      , pureExpr <- [|pure|], apExpr <- [|(<*>)|]
      ]
    info -> error $ show info

typeHeads :: Type -> [Type]
typeHeads = (:) <*> List.unfoldr \ case
    AppT t _ -> Just (t, t)
    _ -> Nothing
