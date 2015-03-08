module Harold.TextOutput
(
  TextOutput,
  getText,
  line1,
  line,
  blank,
  indent,
)
where


import Control.Monad.Writer


type TextOutput = Writer [String] ()

getText :: TextOutput -> String
getText = unlines . execWriter

line1 :: String -> TextOutput
line1 = tell . (:[])

line :: [String] -> TextOutput
line = line1 . concat

blank :: TextOutput
blank = line1 ""

indent :: Int -> TextOutput -> TextOutput
indent n = censor (map (pref ++))
  where pref = replicate n ' '