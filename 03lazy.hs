
removeNonUpper :: [Char] -> [Char]
removeNonUpper st = [ c | c<-st, c ` elem` ['A'..'Z'] ]

addThree :: Int -> Int -> Int -> Int
addThree x y z = x + y + z

leapYear = [ x | x<-[2014..2114], x`mod`4==0 && x`mod`100/=0 || x`mod`400==0 ]

withLength :: [[a]] -> [([a], Int)]
withLength list = [(x, length x) | x<-list]
