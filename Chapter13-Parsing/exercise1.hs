import Parsing

comment :: Parser ()
comment = do
  symbol "--"
  identifier
  symbol "\n"-- Read string until it reaches "\n"
  return ()

