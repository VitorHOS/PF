fatorialDuplo :: Int -> Int
fatorialDuplo n 
        |n == 0 = 1
        |n == 1 =1
        |n > 0 = fatorialDuplo (n-2) * n

resto :: Int -> Int -> Int
resto a b
        |b == 0 = 0
        |a >= b = resto (a-b) b
        |a < b = a

quociente :: Int -> Int -> Int -> Int
quociente a b q
        |a < b = 0
        |b == 0 = 0
        |a >= b = quociente (a-b) b (q+1)
        |a < b = q

potencia :: Int -> Int -> Int
potencia n e 
        |e == 0 = 1
        |e == 1 = n
        |n > 0 = n * potencia n (e-1)

nand :: Bool -> Bool -> Bool
nand p q = if (p == False || q == False) then True else False

nandG :: Bool -> Bool -> Bool
nandG p q
        |(p == False || q == False) = True
        |(p == True && q == True) = False

nandP :: Bool -> Bool -> Bool
nandP False _ = True
nandP _ False = True
nandP True True = False

imprimeNVezes :: Int -> IO ()
imprimeNVezes n = if n > 1 
	then do
	putStrLn "frase"
	imprimeNVezes (n-1)
	else putStrLn "frase"

eLogico :: Bool -> Bool -> Bool
eLogico p q
        |(p == True && q == True) = True
        |otherwise = False

eLogico2 :: Bool -> Bool -> Bool
eLogico2 p q
        |(p == True && q) = q
        |(q == True && p) = p
        |(p == False || q == False) = False