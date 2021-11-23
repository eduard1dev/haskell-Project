--Nome : Gabriel Miranda Oliva
--Matrícula : 202100011430
--Nome : Carlos Eduardo Dias dos Santos
--Matrícula : 202100104941

{-# LANGUAGE OverloadedStrings #-}
import CodeWorld

type World = ((Double, Double, Velocidade, Double), (Double, Double, Velocidade, Double))
type Velocidade = (Double,Double)
type Vetor = (Double,Double)
type Ponto = (Double,Double)
type Reta  = (Ponto,Ponto)

accNave = 1.15          -- Magnitude da aceleração da nave quando
                     -- está acelerando
velAngNave = pi/6       -- Velocidade angular da nave quando está girando

velSaidaArma = 1     -- Magnitude da velocidade de saída da arma
cadArma = 4          -- Cadência de disparos da arma
durMaxProjetil = 3   -- Tempo máximo de duração de um projétil
tempoMaxFora = 5     -- Tempo máximo que nave pode ficar fora
                     -- do espaço visível
durExplosao = 3       -- Tempo de duração de uma explosão  


main = activityOf ((-4, 0,(0,1), 0), (4, 0,(0,1), 0)) update visualization

nave::[Ponto]
nave = [(-1,0),(1,0),(0,2)]

nave2::[Ponto]
nave2 = [(1,0),(3,0),(2,2)]

visualization :: World -> Picture
visualization ((x, y, _, a), (x1, y1, _, a1)) = translated x y (rotated a (translated (-xa0) (-ya0) (polygon nave))) & translated x1 y1 (rotated a1 (translated (-xb0) (-yb0) (polygon nave2)))
  where 
    xa0 = fst (calculaCentroide nave)
    ya0 = snd (calculaCentroide nave)
    xb0 = fst (calculaCentroide nave2)
    yb0 = snd (calculaCentroide nave2)
  
update:: Event -> World  -> World 
--Nave 1
update (KeyPress "Up") ((x, y, (vx,vy), a), rest) = ((x, y, (vx + (cos (vectorDirection (vx,vy)))*1.15, vy + (sin (vectorDirection (vx,vy)))*1.15), a), rest)
update (KeyPress "Right") ((x, y, (vx,vy), a), rest) = ((x, y, (rotatedVector (-pi/6) (vx,vy)), a - pi/6), rest)
update (KeyPress "Left") ((x, y, (vx,vy), a), rest) = ((x, y, (rotatedVector (pi/6) (vx,vy)), a + pi/6), rest)

--Nave 2
update (KeyPress "W") (rest, (x1, y1, (vx1,vy1), a)) = (rest, (x1, y1, (vx1 + (cos (vectorDirection (vx1,vy1)))*1.15, vy1 + (sin (vectorDirection (vx1,vy1)))*1.15), a))
update (KeyPress "D") (rest, (x1, y1, (vx1,vy1), a)) = (rest, (x1, y1, (rotatedVector (-pi/6) (vx1,vy1)), a - pi/6))
update (KeyPress "A") (rest, (x1, y1, (vx1,vy1), a)) = (rest, (x1, y1, (rotatedVector (pi/6) (vx1,vy1)), a + pi/6))

update (TimePassing t) ((x, y, (vx,vy), a), (x1, y1, (vx1,vy1), a1)) = ((x+vx*t, y+vy*t, (vx,vy), a), (x1+vx1*t, y1+vy1*t, (vx1,vy1), a1))
update _ w = w

calculaAreaPoligono :: [Ponto] -> Double
calculaAreaPoligono poligono = if area >= 0 then area else negate area
      where
            listaNova = concat [poligono, take 1 poligono]
            area = sum [fst (listaNova!!x) * snd (listaNova!!(x + 1)) - snd (listaNova!!x) * fst (listaNova!!(x + 1)) | x<-[0..length listaNova - 2]] / 2

calculaCentroide :: [Ponto] -> Ponto
calculaCentroide poligono = (s1/(calculaAreaPoligono poligono * 6), s2/(calculaAreaPoligono poligono * 6))
      where 
            listaNova = concat [poligono, take 1 poligono]
            s1 = sum [(fst (listaNova!!x) + fst (listaNova!!(x + 1)))*(fst (listaNova!!x) * snd (listaNova!!(x + 1)) - snd (listaNova!!x) * fst (listaNova!!(x + 1))) | x<-[0..length listaNova - 2]]
            s2 = sum [(snd (listaNova!!x) + snd (listaNova!!(x + 1)))*(fst (listaNova!!x) * snd (listaNova!!(x + 1)) - snd (listaNova!!x) * fst (listaNova!!(x + 1))) | x<-[0..length listaNova - 2]]