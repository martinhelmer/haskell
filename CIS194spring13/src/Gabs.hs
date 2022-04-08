import GhcPlugins (xFlags)

-- f u d c j h = d ++ u ++ "food " ++ h ++ j ++ c ++ "fat" ++ e

-- m = "more "
-- y = "you because "
-- b =" youre to "
-- x = "no "
-- a = "for "
-- e = reverse "hctib "

f2 a s r w d g u c b t j  = a+r - w + d -u * s - t * g - c + b * j

sumUp 0 = 0
sumUp x = sumUp (x-1) + x

sumDown x = sum [1..x]