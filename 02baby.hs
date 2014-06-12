doubleMe(x) = x + x
doubleUs x y = x * 2 + y * 2

doubleSmallNumber x = if x > 100
    then x
    else x * 2

isIncludeK [] = False
isIncludeK (x:xs) = if x == 'K'
    then True
    else isIncludeK xs

conc [] a = [a]
conc (x:xs) a = x : conc xs a

lastButOne (x:xs) = if length xs == 1
    then x
    else lastButOne xs

listCount [] = 0
listCount (x:xs) = listCount xs + 1

sumlist [] = 0
sumlist (x:xs) = x + sumlist xs
average x = (sumlist x) / fromIntegral (length x)

kaibun a = a ++ tail (reverse a)
kaibunCheck [] = True
kaibunCheck (x:[]) = True
kaibunCheck (x:xs) = x == last xs && kaibunCheck (init xs)

intersperse c (x:xs) = if length xs == 0
    then x
    else x ++ [c] ++ intersperse c xs
