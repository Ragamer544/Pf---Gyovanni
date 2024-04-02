1°
import CodeWorld
import CodeWorld.Sketches
import CodeWorld
main :: IO ()
main = drawingOf (apartamento & coordinatePlane)
apartamento  :: Picture
apartamento  = rua red blue purple black
rua :: Color -> Color -> Color -> Color ->  Picture
rua c1 c2 c3 c4 = translated (-15) 0 (casa2 c1) & casa1 c2 & translated (-15) (-10) (casa1 c3) & translated 0 (-10) (casa2 c4)

casa1 :: Color -> Picture
casa1 cor = teto & chamine  & janela & porta & parede & carro & arvore  
 where parede = colored cor (solidRectangle 6 8)       
       teto   = colored orange (translated 0 3 (solidPolygon [(0,3), (-3,1), (3,1)]))       
       porta  = colored brown (translated 0 (-3) (solidRectangle 1.5 2))      
       janela = colored white (translated 0 2(solidCircle 1.2))       
       carro  = colored cor (translated (-5) (-1.5)(scaled 1 1 (sketchedCar)))       
       arvore =  colored (light green) (translated 5 (1)(scaled 0.3 0.6 (solidCircle 5))) & colored brown (translated 5 (-1)(scaled 0.3 0.6 (solidRectangle 1 6)))    
       chamine = colored brown (translated 2.7 4.7 ( scaled 0.5 0.5 (solidRectangle 1 3)))
       
casa2 :: Color -> Picture
casa2 cor = teto & janela & porta & parede & carro & arvore  
 where parede = colored cor (solidRectangle 6 8)       
       teto   = colored orange (translated 0 3 (solidPolygon [(0,3), (-3,1), (3,1)]))       
       porta  = colored brown (translated 0 (-3) (solidRectangle 1.5 2))      
       janela = colored white (translated 0 2(solidCircle 1.2))       
       carro  = colored cor (translated (-5) (-1.5)(scaled 1 1 (sketchedCar)))       
       arvore =  colored (dark green) (translated 5 (1)(scaled 0.3 0.6 (solidCircle 5))) & colored brown (translated 5 (-1)(scaled 0.3 0.6 (solidRectangle 1 6)))    

2°:
import CodeWorld
import CodeWorld.Sketches
import CodeWorld

main :: IO ()
main = drawingOf (olhoBoca 's' 'n' & coordinatePlane)

olhoBoca ::Char->Char->Picture
olhoBoca c1 c2 = translated (-3) 0 (olho c1) & translated 3 0 (olho c1) & boca c2

olho ::Char->Picture
olho 'a' = colored black (solidCircle 1)
olho 'f' = arc (pi/3) (pi/3)  1 
olho 's' = translated 0 1 (scaled 1 2 (colored white (solidCircle (1)))) & scaled 1 2 ( colored black (solidCircle (1)))
olho _ = blank

boca :: Char-> Picture
boca 'n' = colored black (translated 0 (-5)(polyline [(-1,-1), (1,-1)]))
boca 's' = (translated 0 (-5)(arc (pi) (2*pi) 1))
boca 't' = (translated 0 (-5)(arc (0) (pi) 1))
boca _  = blank

3° a) e B) :

1° emoji :import CodeWorld
import CodeWorld.Sketches
import CodeWorld

main :: IO ()
main = drawingOf emojidascavernas
emojidascavernas = sombrancelha & cabelo & olhos & boca & rosto & background 
  where background = colored white (solidRectangle 100 200)
        rosto = translated 0.8 0 (colored yellow (solidCircle 2))
        olhos = colored black (solidCircle 0.2) & colored white (solidCircle 0.5) & (translated 1.5 0 (colored black(solidCircle 0.2) & colored white (solidCircle 0.5)))
        boca = colored black (curve [(-0.5, -1.2), (0.8, -1), (2, -1.2)])
        sombrancelha = translated 1.8 1.2 (colored black (scaled 0.4 0.4 (sketchedGrass))) & translated (-0.3) 1.2 (colored black (scaled 0.4 0.4 (sketchedGrass)))
        cabelo = translated 0.75 2.4 (colored (dark black) (scaled 0.5 0.5 (sketchedGrass)))

2° emoji:import CodeWorld
import CodeWorld.Sketches
import CodeWorld

main :: IO ()
main = drawingOf emojipaixãodesgastante
emojipaixãodesgastante = heart  & boca & rosto & background 
  where background = colored white (solidRectangle 100 200)
        rosto = translated 0.8 0 (colored yellow (solidCircle 2))
        boca = colored black (curve [(-0.5, -1.2), (0.8, -1), (2, -1.2)])
        heart = translated 1.6 0 (colored (dark red) (scaled 0.5 0.5 (sketchedHeart))) & (colored (dark red) (scaled 0.5 0.5 (sketchedHeart)))