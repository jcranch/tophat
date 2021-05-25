module Text where

{-
  Helper functions for text processing

  Part of this is just patching the Text.Lazy module with some
  slightly more general functions.
-}


import Prelude (Char, Int, Maybe(..), (+), (.), ($))
import qualified Prelude as P
import Data.String (IsString, fromString)
import Data.Text.Lazy (Text, length, drop, foldl', unpack,
                       inits, tails, null, breakOn)


-- | How many line breaks are there in this text?
linebreaks :: Text -> Int
linebreaks = foldl' f 0 where
  f n '\n' = n+1
  f n _ = n


-- | Like breakOn, but indicates clearly whether there was a match or
-- not. It's surprising this isn't in the standard library.
breakOnOnce :: Text -> Text -> Maybe (Text, Text)
breakOnOnce pat src = let (a, b) = breakOn pat src in
  if null b then Nothing else Just (a, b)


-- | As above, but omits the match text. It's even more surprising
-- that this isn't in the standard library.
splitOnOnce :: Text -> Text -> Maybe (Text, Text)
splitOnOnce pat src = case breakOnOnce pat src of
  Nothing     -> Nothing
  Just (a, b) -> Just (a, drop (length pat) b)


-- | The characters of t, together with the complete text of t up to
-- that point, and the complete text thereafter. So, for all (c,(u,v))
-- in splits t, we have t = u <> c <> v.
splits :: Text -> [(Char, (Text, Text))]
splits t = P.zip (unpack t) $ P.zip (inits t) (P.tail $ tails t)


-- | As above, but this time the former includes the character just
-- gone. So, for all (c,(u,v)) in splits' t, we have t = u <> v.
splits' :: Text -> [(Char, (Text, Text))]
splits' t = P.zip (unpack t) . P.tail $ P.zip (inits t) (tails t)


-- | Formats text as a literal string
textLiteral :: (IsString s) => Text -> s
textLiteral = fromString . P.show . unpack
