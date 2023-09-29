import Data.Char

converte :: Char -> (Char,Char,Int)
converte x = (x,toUpper x,ord x)

equacao :: (Float,Float,Float) -> (Float,Float)
equacao (a,b,c) = if d < 0 then error("sem raizes")
	else (x1, x2)
	where 
		d = b*b - 4*a*c
		x1 = (-b + sqrt d) / (2*a)
		x2 = (-b - sqrt d) / (2*a)

type Nome = String
type Idade = Int
type Sexo = String

type Pessoa = (Nome,Idade,Sexo)

pessoa :: Int -> Pessoa
pessoa 1 = ("a",10,"m")
pessoa 2 = ("b",20,"f")
pessoa 3 = ("c",30,"m")
pessoa 4 = ("d",40,"f")

retornaIdade :: Pessoa -> Int
retornaIdade (_,i,_) = i

somaIdade :: Int -> Int -> Int
somaIdade x c = if x > 0 && x < 5 then somaIdade (x-1) (retornaIdade(pessoa x)+c) else c

mediaIdade :: Float -> Float
mediaIdade n = (somaIdade n)/n