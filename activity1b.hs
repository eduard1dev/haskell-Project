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
update (KeyPress "Up") (x, y, (vx,vy), a) = if modulo vx >= 5 || modulo vy >= 5 then (x, y, (vx,vy), a) else (x, y, (vx + (cos (vectorDirection (vx,vy)))*1.15, vy + (sin (vectorDirection (vx,vy)))*1.15), a)
update (KeyPress "Right") (x, y, (vx,vy), a) = (x, y, (rotatedVector (-pi/6) (vx,vy)), a - pi/6)
update (KeyPress "Left") (x, y, (vx,vy), a) = (x, y, (rotatedVector (pi/6) (vx,vy)) + pi/6)), a + pi/6)
update (TimePassing t) (x, y, (vx,vy), a) = (x+vx*t, y+vy*t, (vx,vy), a)
update _ w = w

modulo :: Double -> Double
modulo a
     |a >= 0 = a
     |otherwise = negate a

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