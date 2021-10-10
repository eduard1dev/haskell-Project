type Vetor = (Double,Double)
type Ponto = (Double,Double)
type Reta  = (Ponto,Ponto)

calculaVetor :: Ponto -> Ponto -> Vetor
calculaVetor (p1,p2) (q1,q2) = (q1 - p1,p2 - q2)

calculaVetorProjecao :: Reta -> Vetor
calculaVetorProjecao (r1,r2) = calculaVetor r1 r2

produtoVetorialDois::Vetor -> Vetor -> Double
produtoVetorialDois (p1,p2) (q1,q2) = p1*q2 - p2*q1

orientacao :: Double -> Int
orientacao x
         | x > 0    = 1
         | x < 0    = -1
         |otherwise = 0

projecaoX :: Reta -> Reta 
projecaoX ((p1,p2),(q1,q2)) = ((p1,0),(q1,0))

projecaoY :: Reta -> Reta
projecaoY ((p1,p2),(q1,q2)) = ((0,p2),(0,q2))

-- Questao 1
retasIntersectam :: Reta -> Reta -> Bool
retasIntersectam (r1,r2) (t1,t2)
               | ( (orientacao pv1) /= (orientacao pv2) ) && ( (orientacao pw1) /= (orientacao pw2) ) = True
               | (pv1 == 0) && (pv2 == 0) && (pw1 == 0) && (pw2 == 0) && ( ((orientacao px1) /= (orientacao px2) ) && ( (orientacao py1) /= (orientacao py2)) ) = True
               | otherwise = False
                where pv1 = produtoVetorialDois (calculaVetor r1 r2) (calculaVetor r2 t1) 
                      pv2 = produtoVetorialDois (calculaVetor r1 r2) (calculaVetor r2 t2) 
                      pw1 = produtoVetorialDois (calculaVetor t1 t2) (calculaVetor t2 r1) 
                      pw2 = produtoVetorialDois (calculaVetor t1 t2) (calculaVetor t2 r2) 
                      px1 = produtoVetorialDois (calculaVetorProjecao (projecaoX (r1,r2))) (calculaVetorProjecao (projecaoX (r2,t1)))
                      px2 = produtoVetorialDois (calculaVetorProjecao (projecaoX (r1,r2))) (calculaVetorProjecao (projecaoX (r2,t2)))
                      py1 = produtoVetorialDois (calculaVetorProjecao (projecaoX (t1,t2))) (calculaVetorProjecao (projecaoX (t2,r1)))
                      py2 = produtoVetorialDois (calculaVetorProjecao (projecaoX (t1,t2))) (calculaVetorProjecao (projecaoX (t2,r1)))

-- Questao 2
poligonoIntersecta :: Reta -> [Ponto] -> Bool
poligonoIntersecta reta pontosPoligono
      | [] /= [True | x<-[0..length listaNova - 2], retasIntersectam reta (listaNova!!x, listaNova!!(x + 1))] = True
      | otherwise = False
      where
            listaNova = concat [pontosPoligono, take 1 pontosPoligono]

-- Questao 3
poligonosIntersecta :: [Ponto] -> [Ponto] -> Bool
poligonosIntersecta poligono1 poligono2 = [] /= [True | x<-[0..length listaNova - 2], poligonoIntersecta (listaNova!!x, listaNova!!(x + 1)) poligono2]
      where
            listaNova = concat [poligono1, take 1 poligono1]

-- Questao 4
calculaAreaPoligono :: [Ponto] -> Double
calculaAreaPoligono poligono = if area >= 0 then area else negate area
      where
            listaNova = concat [poligono, take 1 poligono]
            area = sum [fst (listaNova!!x) * snd (listaNova!!(x + 1)) - snd (listaNova!!x) * fst (listaNova!!(x + 1)) | x<-[0..length listaNova - 2]] / 2
            --n = listaNova!!x
            --nMais1 = listaNova!!(x + 1)

-- Questao 5
calculaCentroide :: [Ponto] -> Ponto
calculaCentroide poligono = (s1/(calculaAreaPoligono poligono * 6), s2/(calculaAreaPoligono poligono * 6))
      where 
            listaNova = concat [poligono, take 1 poligono]
            s1 = sum [(fst (listaNova!!x) + fst (listaNova!!(x + 1)))*(fst (listaNova!!x) * snd (listaNova!!(x + 1)) - snd (listaNova!!x) * fst (listaNova!!(x + 1))) | x<-[0..length listaNova - 2]]
            s2 = sum [(snd (listaNova!!x) + snd (listaNova!!(x + 1)))*(fst (listaNova!!x) * snd (listaNova!!(x + 1)) - snd (listaNova!!x) * fst (listaNova!!(x + 1))) | x<-[0..length listaNova - 2]]