{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE GADTs                 #-}
{-# OPTIONS_GHC -Wno-orphans       #-}
module Data.HDiff.Base.Show where

import           System.IO
import           Data.Proxy
import           Data.Functor.Const
import           Data.Functor.Sum
import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Render.Terminal
import qualified Data.Text.Prettyprint.Doc.Render.Text as Text
import qualified Data.Text as T

import Generics.MRSOP.Base hiding (Infix)
import Generics.MRSOP.Holes
import Generics.MRSOP.HDiff.Holes
import Generics.MRSOP.HDiff.Renderer

import           Data.Exists
import qualified Data.HDiff.Base       as D
import qualified Data.HDiff.Base.Merge as D
import qualified Data.HDiff.MetaVar    as D

-- |Given a label and a doc, @spliced l d = "[" ++ l ++ "|" ++ d ++ "|]"@
spliced :: Doc ann -> Doc ann -> Doc ann
spliced lbl d = brackets (lbl <> surround d (pretty "| ") (pretty " |")) 

metavarPretty :: (Doc AnsiStyle -> Doc AnsiStyle) -> D.MetaVarIK ki ix -> Doc AnsiStyle
metavarPretty sty (NA_I (Const i)) 
  = sty $ spliced (pretty "I") (pretty i)
metavarPretty sty (NA_K (D.Annotate i _)) 
  = sty $ spliced (pretty "K") (pretty i)

-- when using emacs, the output of the repl is in red;
-- hence, life is easier when we show a different color isntead.
-- btw, emacs only interprets dull colors.
myred , mygreen , mydullred , mydullgreen :: AnsiStyle
myred       = colorDull Yellow
mygreen     = colorDull Green
mydullred   = colorDull Yellow
mydullgreen = colorDull Green

{-
-- |Shows a conflict in a pretty fashion  
conflictPretty :: (HasDatatypeInfo ki fam codes)
               => (forall k . ki k -> Doc AnsiStyle)
               -> Sum D.MetaVar (D.Conflict ki codes) at -> Doc AnsiStyle
conflictPretty renderK (InL v)
  = metavarPretty v
conflictPretty renderK (InR (D.Conflict l r))
  = let dl = utxPretty (Proxy :: Proxy fam) metavarPretty renderK l
        dr = utxPretty (Proxy :: Proxy fam) metavarPretty renderK r
     in annotate (color Red)
      $ spliced (annotate bold $ pretty "C")
                (hsep [dl , pretty "<|>" , dr ])
-}

-- |Pretty prints a patch on the terminal
showRawPatch :: (HasDatatypeInfo ki fam codes , RendererHO ki)
             => Holes ki codes (D.Chg ki codes) v
             -> [String]
showRawPatch patch 
  = doubleColumn 75
      (holesPretty (Proxy :: Proxy fam) id prettyCChangeDel renderHO patch)
      (holesPretty (Proxy :: Proxy fam) id prettyCChangeIns renderHO patch)
  where
    prettyCChangeDel :: (HasDatatypeInfo ki fam codes , RendererHO ki)
                    => D.Chg ki codes at
                    -> Doc AnsiStyle
    prettyCChangeDel (D.Chg del _)
      = holesPretty (Proxy :: Proxy fam)
                  (annotate myred)
                  (metavarPretty (annotate mydullred))
                  renderHO
                  del

    prettyCChangeIns :: (HasDatatypeInfo ki fam codes , RendererHO ki)
                    => D.Chg ki codes at
                    -> Doc AnsiStyle
    prettyCChangeIns (D.Chg _ ins)
      = holesPretty (Proxy :: Proxy fam)
                  (annotate mygreen)
                  (metavarPretty (annotate mydullgreen))
                  renderHO
                  ins

instance {-# OVERLAPPING #-} (HasDatatypeInfo ki fam codes , RendererHO ki)
      => Show (Holes ki codes (D.Chg ki codes) at) where
  show = unlines . showRawPatch

instance {-# OVERLAPPING #-} (HasDatatypeInfo ki fam codes , RendererHO ki , ShowHO phi)
      => Show (Delta (Holes ki codes phi) at) where
  show (del :*: ins)
    = unlines $ doubleColumn 75
        (holesPretty (Proxy :: Proxy fam) id (pretty . showHO) renderHO del)
        (holesPretty (Proxy :: Proxy fam) id (pretty . showHO) renderHO ins)
  show _ = undefined -- ghc seems to really want this to see the patterns are complete.

instance (ShowHO ki , HasDatatypeInfo ki fam codes , RendererHO ki)
      => ShowHO (D.Conflict ki codes) where
  showHO (D.FailedContr exs)
    = "(FailedContr " ++ unwords (map (exElim showHO) exs) ++ ")"
  showHO (D.Conflict str x y) = unlines
    $   [ "{ " ++ show str ++ " }"
        , "<<<<<<<<<<<<<<<<<<<<<<"
        , showHO x
        , "======================"
        , showHO y
        , ">>>>>>>>>>>>>>>>>>>>>>"
        ]

instance  (HasDatatypeInfo ki fam codes , RendererHO ki)
      => Show (D.Chg ki codes at) where
  show = showHO

instance  (HasDatatypeInfo ki fam codes , RendererHO ki)
      => ShowHO (D.Chg ki codes) where
  showHO (D.Chg del ins) = unlines $ doubleColumn 75
    (holesPretty (Proxy :: Proxy fam) id (metavarPretty (annotate mydullred)) renderHO  del)
    (holesPretty (Proxy :: Proxy fam) id (metavarPretty (annotate mydullgreen)) renderHO ins)


-- |Outputs the result of 'showRawPatch' to the specified handle
displayRawPatch :: (HasDatatypeInfo ki fam codes , RendererHO ki)
                => Handle
                -> Holes ki codes (D.Chg ki codes) at
                -> IO ()
displayRawPatch hdl = mapM_ (hPutStrLn hdl) . showRawPatch

-- |Displays two docs in a double column fashion
--
--  This is a hacky function. We need to render both the colored
--  and the non-colored versions to calculate
--  the width spacing correctly (see @complete@ in the where clause)
--
doubleColumn :: Int -> Doc AnsiStyle -> Doc AnsiStyle -> [String]
doubleColumn maxWidth da db
  = let pgdim = LayoutOptions (AvailablePerLine maxWidth 1)
        lyout = layoutSmart pgdim
        -- colored versions
        ta    = T.lines . renderStrict $ lyout da
        tb    = T.lines . renderStrict $ lyout db
        -- non colored versions
        sta   = T.lines . Text.renderStrict $ lyout da
        w     = 1 + maximum (0 : map T.length sta)
        stb   = T.lines . Text.renderStrict $ lyout db
        compA = if length ta >= length tb
                then 0
                else length tb - length ta
        compB = if length tb >= length ta
                then 0
                else length ta - length tb
        fta   = (zip ta sta) ++ replicate compA ((id &&& id) $ T.replicate w $ T.singleton ' ')
        ftb   = (zip tb stb) ++ replicate compB ((id &&& id) $ T.empty)
     in map (\(la , lb) -> T.unpack . T.concat
                         $ [ complete w la
                           , T.pack " -|+ "
                           , fst lb
                           ])
              (zip fta ftb)
  where
    complete n (clr , nocolor)
      = T.concat [clr , T.replicate (n - T.length nocolor) $ T.singleton ' ']
