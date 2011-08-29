beadSort :: (Integral a) => [a] -> [a]
beadSort [] = []
beadSort (x:xs) = first (reader (snd (sorter((x:xs),(repeat 0))), 0,(snd (sorter(x:xs,repeat 0))) !! 0))

sorter :: (Integral a) => ([a], [a]) -> ([a], [a])
sorter ([], z) = ([], z)
sorter ((x:xs), z) = sorter(xs, (adder x z))

adder :: (Integral a) => a -> [a] -> [a]
adder _ [] = []
adder 0 z = z
adder x (y:ys) = (y + 1) : adder (x-1) ys

reader :: (Integral a) => ([a], a, a) -> ([a], a, a)
reader ([], y, z) = (rep y z, 0, 0)
reader (_, _, 0) = ([], 0, 0)
reader ((x:xs), y, z) = (rep y (z - x) ++ first (reader (xs,y + 1, x)), y, x)

first :: ([a], a, a) -> [a]
first (x, _, _) = x

rep :: (Integral a) => a -> a -> [a]
rep _ 0 = []
rep y x = y : rep y (x - 1)