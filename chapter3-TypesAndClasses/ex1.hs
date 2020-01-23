abc_list :: [Char]
abc_list = ['a', 'b', 'c']

abc_tuple :: (Char, Char, Char)
abc_tuple = ('a', 'b', 'c')

booleanCharTuple :: [(Bool, Char)]
booleanCharTuple = [(False, '0'), (True, '1')]

booleanChar :: ([Bool], [Char])
booleanChar = ([False, True], ['0', '1'])

-- This may be wrong 
funcList :: [[a] -> [a]]
funcList = [tail, init, reverse]