--Nome : Gabriel Miranda Oliva
--Matrícula : 202100011430
--Nome : Carlos Eduardo Dias dos Santos
--Matrícula : 202100104941

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
  girNave:: Char, -- 'a', 'h', 's' (anti-horário, horário, sem girar)
  accNave:: Bool, -- True se a nave está acelerada
  disp:: Bool, -- True se vai disparar, False se nao
  vida :: Int, -- Quantos projeteis a nave pode ser atingida
  clockArma :: Double -- tempo para atirar 1 projetil (4 tiros/seg)
} deriving (Show) 

data Projetil = Projetil {
  posProjetil:: Point, -- localização do projetil
  dirProjetil:: Double, -- direção do projetil
  timer:: Double -- tempo de vida do projetil
} deriving (Show, Eq) 

data Espaco = Espaco {
  nave1, nave2 :: Nave,
  projeteis :: [Projetil]
} deriving (Show) 

vRotacao = pi/2
mAceleracao = 2
cadencia = 1/4

mundoInicial = Espaco {
   nave1 = Nave {posNave = (0,0),  
   velNave = (0, 0), 
   dirNave = pi/2, 
   girNave = 's',   
   accNave = False,
   disp = False,
   vida = 10,
   clockArma = 0.0
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
atualiza (KeyPress "E") espaco = espaco {nave1 = update espaco}
  where update (Espaco {nave1 = nave}) = nave {disp = True}
atualiza (KeyRelease "E") espaco = espaco {nave1 = update espaco}
  where update (Espaco {nave1 = nave}) = nave {disp = False}

atualiza (TimePassing t) espaco = (espaco {nave1 = updateNave1 espaco, projeteis = destroiBalaLocal . destroiBalaTempo . atualizaProjeteis . vaiAtirar $ espaco})
  where
    atualizaProjeteis (Espaco {projeteis = pjts}) = map f pjts
      where 
        f (Projetil {posProjetil = p, dirProjetil = d, timer = tim}) = Projetil {posProjetil = p1 p (velocidade d), dirProjetil = d, timer = atualizaTimer tim}
        p1 p v = mruvPos p v (0,0) t
        v1 v = mruvVel v (0,0) t
        velocidade d = rotatedVector d (1,0)
    
    destroiBalaTempo pjts = filter removedor pjts
               where removedor (Projetil {posProjetil = p, dirProjetil = d, timer = tim}) = if tim <= 0 then False else True
    
    destroiBalaLocal pjts = filter remover pjts
               where remover (Projetil {posProjetil = p, dirProjetil = d, timer = tim}) = if fst p >= 10 || fst p <= -10 || snd p >= 10 || snd p <= -10 then False else True
               
    vaiAtirar (Espaco {nave1 = (Nave {posNave = p, dirNave = d, disp = dis, clockArma = k}), projeteis = pjts})
            |dis == True && k >= cadencia = espaco {projeteis = Projetil{posProjetil = p, dirProjetil = d, timer = 5.0}:pjts} 
            |otherwise = espaco {nave1 = (Nave {posNave = p, dirNave = d, disp = dis, clockArma = k}), projeteis = pjts}

    updateNave1 (Espaco { nave1 = nave}) = updateValues nave nave
    updateValues (Nave {posNave = p, velNave = v, accNave = a, dirNave = d, girNave = g, clockArma = clock}) nave = nave {posNave = p1 p v a d, velNave = v1 v a d, dirNave = d1 d g, clockArma = cronometro clock}
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
    
    cronometro l -- modifica o clockArma
             |l <= cadencia = l + 0.5 * t
             |otherwise = 0.0
    
    atualizaTimer f -- atualiza o timer do projetil
                |f >= 0.0 = f - t
                |otherwise = f
                

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