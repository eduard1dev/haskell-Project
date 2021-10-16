--Nome : Gabriel Miranda Oliva
--Matrícula : 202100011430
--Nome : Carlos Eduardo Dias dos Santos
--Matrícula : 202100104941

{-# LANGUAGE OverloadedStrings #-}
import CodeWorld

type World = (Double, Double, Velocidade, Double)
type Velocidade = (Double,Double)
type Vetor = (Double,Double)
type Ponto = (Double,Double)
type Reta  = (Ponto,Ponto)

main = activityOf (0, 0,(0,1), 0) update visualization

nave::[Ponto]
nave = [(-1,0),(1,0),(0,2)]

visualization :: World -> Picture
visualization (x, y, _, a) = translated x y (rotated a (translated (-x0) (-y0) (polygon nave)))
  where 
    x0 = fst (calculaCentroide nave)
    y0 = snd (calculaCentroide nave)
  
update:: Event -> World  -> World 
update (KeyPress "Up") (x, y, (vx,vy), a) = (x, y, (vx + (cos (vectorDirection (vx,vy)))*1.15, vy + (sin (vectorDirection (vx,vy)))*1.15), a)
update (KeyPress "Right") (x, y, (vx,vy), a) = (x, y, (rotatedVector (-pi/6) (vx,vy)), a - pi/6)
update (KeyPress "Left") (x, y, (vx,vy), a) = (x, y, (rotatedVector (pi/6) (vx,vy)), a + pi/6)
update (TimePassing t) (x, y, (vx,vy), a)
        |x > 9.5 = (-9.5, y, (vx,vy), a)
        |x < -9.5 = (9.5, y, (vx,vy), a)
        |y > 9.5 = (x, -9.5, (vx,vy), a)
        |y < -9.5 = (x, 9.5, (vx,vy), a)
        |otherwise = (x+vx*t, y+vy*t, (vx,vy), a)
update _ w = w

calculaPontoIntersec :: Reta -> Reta -> Ponto
calculaPontoIntersec (px,py) (qx,qy) = (x, y)
      where
            determinanteX1 = (snd px - snd py)
            determinanteY1 = (fst py - fst px)
            resto1 = fst px*snd py - fst py*snd px
            determinanteX2 = (snd qx - snd qy)
            determinanteY2 = (fst qy - fst qx )
            resto2 = fst qx*snd qy - fst qy*snd qx
            y = negate (resto1*negate determinanteX2  + resto2*determinanteX1) / (determinanteY1*negate determinanteX2 + determinanteY2*determinanteX1)
            x = negate (resto1*negate determinanteY2  + resto2*determinanteY1) / (determinanteX1*negate determinanteY2 + determinanteX2*determinanteY1)

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