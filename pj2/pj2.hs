{-# LANGUAGE OverloadedStrings #-}
import CodeWorld

main = activityOf mundoInicial atualiza apresenta

type Velocidade = Vector
type Aceleracao = Vector
type Direcao = Double    -- ângulo em radianos
type Giro = Char

data Nave = Nave {
  posNave:: Point, -- localização da nave 
  velNave:: Vector, -- vetor de velocidade
  dirNave:: Double, -- direção da ponta da nave
  girNave:: Char,
  accNave:: Bool, -- 'a', 'h', 's' (anti-horário, horário, sem girar)
  disp:: Bool, -- True se a nave está acelerada
  vida :: Int -- Quantos projeteis a nave pode ser atingida
} deriving (Show) 

data Projetil = Projetil {
  posProjetil:: Point,
  dirProjetil:: Double
} deriving (Show) 

data Espaco = Espaco {
  nave1, nave2 :: Nave,
  projeteis :: [Projetil]
} deriving (Show) 

vRotacao = pi/2
mAceleracao = 2

mundoInicial = Espaco {
   nave1 = Nave {posNave = (0,0),  
   velNave = (0, 0), 
   dirNave = pi/2, 
   girNave = 's',   
   accNave = False,
   disp = False,
   vida = 10
   },
   projeteis = []
}

nave :: Picture
nave = translated (-x) (-y) (solidPolygon navePol)
  where
    navePol = [(0,0.7),(0,-0.7),(1.5, 0)]
    (x, y) = centroide navePol

projetil::Picture
projetil = solidPolygon [(0,0),(0.25,0),(0.25,0.25),(0,0.25)]

apresenta :: Espaco -> Picture
apresenta (Espaco {nave1 = (Nave {posNave = (x, y), dirNave = d}), projeteis = pjts}) = 
  translated x y (rotated d nave) & if length pjts == 0 then blank else foldr1 (&) (map f pjts)
  where f (Projetil {posProjetil = (x, y)}) = translated x y projetil
  


atualiza :: Event -> Espaco -> Espaco

--Nave 1
atualiza (KeyPress "W") espaco = espaco { nave1 = update espaco}
  where update (Espaco { nave1 = nave}) = nave {accNave = True}
atualiza (KeyRelease "W") espaco = espaco { nave1 = update espaco}
  where update (Espaco { nave1 = nave}) = nave {accNave = False}
atualiza (KeyPress "A") espaco = espaco { nave1 = update espaco}
  where update (Espaco { nave1 = nave}) = nave {girNave = 'a'}
atualiza (KeyRelease "A") espaco = espaco { nave1 = update espaco}
  where update (Espaco { nave1 = nave}) = nave {girNave = 's'}
atualiza (KeyPress "D") espaco = espaco { nave1 = update espaco}
  where update (Espaco { nave1 = nave}) = nave {girNave = 'h'}
atualiza (KeyRelease "D") espaco = espaco { nave1 = update espaco}
  where update (Espaco { nave1 = nave}) = nave {girNave = 's'}
atualiza (KeyPress "E") espaco = update espaco espaco
  where update (Espaco {nave1 = (Nave {posNave = p, dirNave = d}), projeteis = pjts}) espaco = espaco {projeteis = Projetil {posProjetil = p, dirProjetil = d}:pjts}

atualiza (TimePassing t) espaco = espaco { nave1 = updateNave1 espaco, projeteis = atualizaProjeteis espaco}
  where
    atualizaProjeteis (Espaco {projeteis = pjts}) = map f pjts 
      where 
        f (Projetil {posProjetil = p, dirProjetil = d}) = Projetil {posProjetil = p1 p (velocidade d), dirProjetil = d}
        p1 p v = mruvPos p v (0,0) t
        v1 v = mruvVel v (0,0) t
        velocidade d = rotatedVector d (1,0)
        
    updateNave1 (Espaco { nave1 = nave}) = updateValues nave nave
    updateValues (Nave {posNave = p, velNave = v, accNave = a, dirNave = d, girNave = g}) nave = nave {posNave = p1 p v a d, velNave = v1 v a d, dirNave = d1 d g}
    p1 p v a d = mruvPos p v (acc a d) t
    
    v1 v a d = mruvVel v (acc a d) t
    
    acc a d= if a
          then scaledVector mAceleracao (unitVector d)
          else (0, 0)
    
    d1 d g = mcuAng d (va g) t

    va g
      | g == 'a'  = vRotacao
      | g == 'h'  = -vRotacao
      | otherwise = 0
          
atualiza _ m = m    
 
 
mruvPos :: Point -> Velocidade ->  Aceleracao -> Double -> Point    
mruvPos p v acc t =
  vectorSum p  (vectorSum (scaledVector t v)
                          (scaledVector (1/2 * t^2) acc))

mruvVel :: Velocidade -> Aceleracao -> Double -> Vector
mruvVel v acc t = vectorSum v (scaledVector t acc)

mcuAng :: Direcao -> Double -> Double -> Direcao
mcuAng d va t = d + va * t


---------------------------------

unitVector :: Double -> Vector
unitVector a = (cos a, sin a)

-------------------------------

type Segmento = (Point, Point)
type Poligono = [Point]

segmentosDoPol :: Poligono -> [Segmento]
segmentosDoPol pol = zip pol (tail pol ++ [head pol])

-- 4 Área de polígono regular
-- A = 1/2 Sum(i=0, i=n-1, x_i y_{i+1} - x_{i+1} y_i)
-- Se os pontos estão listados anti-horariamente A será positivo, senão será negativo.
areaPoligonoR :: Poligono -> Double
areaPoligonoR ps = 1/2 * ( sum [ fst p1 * snd p2 -fst p2 * snd p1 |
                                 (p1, p2) <- zip ps (tail ps ++ [head ps]) ] )

-- 5 Centróide de um polígono regular
-- Centródie
centroide :: Poligono -> Point
centroide pol = (cx, cy)
  where
    cx = 1 / (6 * area) * sum [ (x1 + x2) * (x1*y2 - x2*y1) |
                                    ((x1,y1), (x2,y2)) <- segmentos ]
    cy = 1 / (6 * area) * sum [ (y1 + y2) * (x1*y2 - x2*y1) |
                                    ((x1,y1), (x2,y2)) <- segmentos ]
    segmentos = segmentosDoPol pol
    area = areaPoligonoR pol
    

-----------------------------------