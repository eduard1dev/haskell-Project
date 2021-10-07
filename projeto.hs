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

retasIntersectam :: Reta -> Reta -> Bool
retasIntersectam (r1,r2) (t1,t2)
               | ( (orientacao pv1) /= (orientacao pv2) ) && ( (orientacao pw1) /= (orientacao pw2) ) = True
               | (pv1 == 0) && (pv2 == 0) && (pw1 == 0) && (pw2 == 0) && ( ((orientacao px1) /= (orientacao px2) ) && ( (orientacao py1) /= (orientacao py2)) ) = True
               |otherwise = False
                where pv1 = produtoVetorialDois (calculaVetor r1 r2) (calculaVetor r2 t1) 
                      pv2 = produtoVetorialDois (calculaVetor r1 r2) (calculaVetor r2 t2) 
                      pw1 = produtoVetorialDois (calculaVetor t1 t2) (calculaVetor t2 r1) 
                      pw2 = produtoVetorialDois (calculaVetor t1 t2) (calculaVetor t2 r2) 
                      px1 = produtoVetorialDois (calculaVetorProjecao (projecaoX (r1,r2))) (calculaVetorProjecao (projecaoX (r2,t1)))
                      px2 = produtoVetorialDois (calculaVetorProjecao (projecaoX (r1,r2))) (calculaVetorProjecao (projecaoX (r2,t2)))
                      py1 = produtoVetorialDois (calculaVetorProjecao (projecaoX (t1,t2))) (calculaVetorProjecao (projecaoX (t2,r1)))
                      py2 = produtoVetorialDois (calculaVetorProjecao (projecaoX (t1,t2))) (calculaVetorProjecao (projecaoX (t2,r1)))