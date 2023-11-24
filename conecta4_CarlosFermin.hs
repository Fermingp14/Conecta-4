--Indicación:   Máquina = Jugador X     ||    Usuario = Jugador O

data Posicion = Vacio | X | O --Estructura de datos que define las fichas del tablero
    deriving Eq

instance Show Posicion --Clase Show para Posicion
  where show = showPosicion

showPosicion:: Posicion -> String -- Función que muestra las fichas del tablero
showPosicion x 
  | x== Vacio = "."
  | x== X = "X"
  |otherwise=  "O"

showLinea:: [Posicion] -> String  -- Función que muestra cada fila del tablero
showLinea [x] = show x
showLinea (x:xs) = showPosicion x ++ " | " ++ showLinea xs


data Tablero = ConsT [[Posicion]] --Estructura de datos que define el tablero 
    deriving Eq

instance Show Tablero  --Clase Show para Tablero
  where show = showTablero


showTab:: [[Posicion]] -> String -- Función auxiliar que muestra el tablero
showTab [] =  " 1   2   3   4   5   6   7"
showTab (xs:xss) = " "++ showLinea xs ++ "\n"++ separador++ "\n" ++ showTab xss
  where separador = " - + - + - + - + - + - + - "

showTablero:: Tablero -> String 
showTablero (ConsT xss) = showTab xss
    

tabInicial:: Tablero --Función que crea el tablero vacío
tabInicial = ConsT (replicate 6 (replicate 7 Vacio))

longitud:: Posicion -> Int --Función que devuelve 1 si la posicion está ocupada y 0 en caso contrario
longitud Vacio = 0
longitud _ = 1

trasponerPosicion::[[Posicion]] -> [[Posicion]] -- Función auxiliar para trasponer el tablero
trasponerPosicion xss
    |null (head xss) = []
    | otherwise = (map head xss):(trasponerPosicion (map tail xss))

traspuesta::Tablero -> Tablero --Función que traspone el tablero
traspuesta (ConsT xss) = ConsT (trasponerPosicion xss)

profColumna::Tablero-> Int->Int  --Función que devuelve el número de elementos en la columna escogida
profColumna (ConsT xss) col= sum (map longitud (head (map snd (filter ((==col).fst) tab))))
    where tab= zip [1..7] (trasponerPosicion xss)

turnodeX::Tablero-> Bool --Función que indica si le toca al jugador X.
turnodeX (ConsT xss) = even (sum [profColumna (ConsT xss) n | n<-[1..7] ])

empate::Tablero->Bool --Función que indica si la partida ha acabado en empate
empate (ConsT xss) = sum [profColumna (ConsT xss) n | n<-[1..7] ] == 42

columnaLibre::Tablero->Int->Bool --Función que indica si se puede añadir ficha en la columna escogida
columnaLibre (ConsT xss) col = profColumna (ConsT xss) col < 6


anadeFicha::Tablero->Int->Tablero   --Función que añade la ficha a la columna escogida del tablero
anadeFicha (ConsT xss) col 
    | not (columnaLibre (ConsT xss) col) = ConsT xss
    | col==7 = ConsT (trasponerPosicion (iz ++ [columna]))
    | otherwise = ConsT (trasponerPosicion (iz++ [columna] ++ (tail dr)))
    where 
        tab= zip [1..7] (trasponerPosicion xss)
        (i,d) = span ((/=col).fst) tab
        iz =map snd i
        dr= map snd d
        columna = columNueva (ConsT xss) (head dr) f
        f = (profColumna (ConsT xss) col) +1

columNuevaAux:: [Posicion]->Posicion -> Int -> [Posicion]  --Función que cambia la posición vacía por la ficha indicada
columNuevaAux xs pos f
    |f==1 = iz ++ [pos]
    |otherwise = iz ++ [pos] ++ (tail dr)
    where 
        tab= zip [6,5,4,3,2,1] xs
        (i,d) = span ((/=f).fst) tab
        iz= map snd i
        dr = map snd d


columNueva:: Tablero -> [Posicion] -> Int -> [Posicion] --Función auxiliar que añade la ficha en la fila correspondiente teniendo en cuenta el turno del jugador
columNueva (ConsT xss) xs f 
    | turnodeX (ConsT xss) = columNuevaAux xs X f 
    | otherwise = columNuevaAux xs O f 


tieneLineaAux::[Posicion] -> Posicion -> Bool  --Función que indica si el jugador indicado tiene 4 seguidas en una lista de posiciones (fila, columna, diagonal en forma de vector)
tieneLineaAux xs pos
  | length iz >= 4 = True
  | length iz < 4 && length dr < 4 = False
  | otherwise = tieneLineaAux (tail dr) pos
  where (iz,dr) = span (==pos) xs
        
tieneLineaH::Tablero -> Posicion -> Bool --Función que indica si el jugador indicado tiene 4 seguidas en horizontal
tieneLineaH (ConsT xss) pos
  | xss==[] = False
  | otherwise = tieneLineaAux (head xss) pos || tieneLineaH (ConsT (tail xss)) pos
  
tieneLineaV::Tablero -> Posicion -> Bool --Función que indica si el jugador indicado tiene 4 seguidas en vertical
tieneLineaV (ConsT xss) pos = tieneLineaH tab pos
  where tab = traspuesta (ConsT xss)
  
diagonalPrincipal::Tablero -> [Posicion]  --Función que obtiene la diagonal principal de un tablero, la que empieza en la ultima fila y primera columna
diagonalPrincipal (ConsT [[]]) = []
diagonalPrincipal (ConsT (xs:[])) = [head xs]
diagonalPrincipal (ConsT (xs:xss)) = head xs : diagonalPrincipal (ConsT (map tail xss))

diagInf::Tablero -> [[Posicion]] --Función que devuelve en forma de matriz las diagonales desde la diagonal principal hacia abajo
diagInf (ConsT (xs:xss))
  | length (xs:xss) >= 4 = diagonalPrincipal (ConsT (xs:xss)) : diagInf (ConsT xss)
  | otherwise = []

diagSup::Tablero -> [[Posicion]] --Función que devuelve en forma de matriz las diagonales desde la diagonal principal hacia arriba
diagSup (ConsT xss) =  diagInf (traspuesta (ConsT xss))

diagonales::Tablero -> [[Posicion]] -- Función que devuelve en forma de vector cada diagonal del tablero 
diagonales (ConsT xss) = diagInf (ConsT xss) ++ diagSup (ConsT xss) ++ diagInf (ConsT yss) ++ diagSup (ConsT yss)
  where yss = map reverse xss 

tieneLineaDiag:: Tablero ->Posicion -> Bool --Función que indica si el jugador indicado tiene 4 seguidas en diagonal
tieneLineaDiag (ConsT xss) pos= length (filter (==True) (map (flip tieneLineaAux pos) yss))>=1
  where yss=diagonales (ConsT xss) 
  
tieneLinea::Tablero -> Bool --Función que indica si ha hecho línea el último en añadir la ficha 
tieneLinea (ConsT xss)
  | turnodeX (ConsT xss) = tieneLineaH (ConsT xss) O|| tieneLineaV (ConsT xss) O|| tieneLineaDiag (ConsT xss) O --Si le toca a X vemos si O ha hecho linea pues ha sido el ultimo en añadir ficha
  | otherwise = tieneLineaH (ConsT xss) X|| tieneLineaV (ConsT xss) X|| tieneLineaDiag (ConsT xss) X
  

tieneSeguidasAux::[Posicion]->Posicion->Int -> Bool  --Función que indica si el jugador indicado tiene n fichas seguidas en una lista de posiciones y se puede poner una n+1 ficha seguida
tieneSeguidasAux xs pos n
  | null dr = False
  | length dr >0 && length iz == n && head dr == Vacio = True
  | otherwise = tieneSeguidasAux (tail dr) pos n 
  where (iz,dr) = span (==pos) xs

--Funciones auxiliares que indican si el jugador indicado tiene n seguidas con opción de poner una n+1 ficha seguida en la dirección indicada 

tieneSeguidasH::Tablero -> Posicion-> Int -> Bool --El jugador indicado tiene n en  linea horizontal y puede poner una n+1
tieneSeguidasH (ConsT xss) pos n 
  | xss==[] = False
  | otherwise = tieneSeguidasAux (head xss) pos n || tieneSeguidasH (ConsT (tail xss)) pos n

tieneSeguidasV::Tablero->Posicion-> Int -> Bool --El jugador indicado tiene n en  linea vertical y puede poner una n+1
tieneSeguidasV (ConsT xss) pos n= tieneSeguidasH tab pos n
  where  
    tab = ConsT (map reverse (trasponerPosicion xss)) 

flip3::(a->b->c->d)->b->c->a->d  --Vamos a emplear más adelante flip generalizado a 3 parámetros de esta manera
flip3 f y z x = f x y z 

identidad::a->a
identidad x = x

tieneSeguidasDiag:: Tablero -> ([[Posicion]]->[[Posicion]])-> Posicion-> Int-> Bool --tiene n seguidas en diagonal y puede colocar la n+1 como primer elemento de la diagonal. La función f será la identidad o (map reverse) como se observa en tieneSeguidasFinal.
tieneSeguidasDiag (ConsT xss) f pos n= length (filter (==True) (map (flip3 tieneSeguidasAux pos n) (f yss)))>=1
  where yss=diagonales (ConsT xss) 

----

tieneSeguidasFinal::Tablero -> Posicion -> Int -> Bool --Función que utiliza las funciones anteriores para indicar si en alguna dirección, el jugador indicado tiene n fichas seguidas con posibilidad de añadir una ficha más seguida
tieneSeguidasFinal (ConsT xss) pos n = tieneSeguidasH (ConsT xss) pos n || tieneSeguidasH tabInvert pos n|| tieneSeguidasV (ConsT xss) pos n|| tieneSeguidasDiag (ConsT xss) identidad pos n || tieneSeguidasDiag (ConsT xss) (map reverse) pos n
  where tabInvert = ConsT (map reverse xss)
  
tieneSeguidasGanar::Tablero-> Bool --Función que indica si el jugador al que le toca tiene opción de ganar
tieneSeguidasGanar (ConsT xss) = not (null (filter (==True) (map tieneLinea (siguientesTableros (ConsT xss))))) 

tiene2Seguidas::Tablero -> Posicion ->Bool --Función que indica si el jugador indicado tiene opción de tener 3 seguidas
tiene2Seguidas (ConsT xss) pos = not (null (filter (==True) (map (flip3 tieneSeguidasFinal pos 3) (siguientesTableros (ConsT xss)))))

tiene1Seguidas::Tablero-> Posicion->Bool  --Función que indica si el jugador indicado tiene opción de tener 2 seguidas
tiene1Seguidas (ConsT xss) pos= not (null (filter (==True) (map (flip3 tieneSeguidasFinal pos  2) (siguientesTableros (ConsT xss)))))

tieneSeguidasGanarProf2::Tablero->Bool  --Función que indica si el jugador al que no le toca tiene opción de ganar
tieneSeguidasGanarProf2 (ConsT xss) = not (null (filter (==True) (map tieneSeguidasGanar (siguientesTableros (ConsT xss)))))

valorar:: Tablero -> Int --Función que valora el tablero
valorar (ConsT xss) 
  | tieneLinea (ConsT xss) = -20
  | turnodeX (ConsT xss) && tieneSeguidasGanar (ConsT xss) = 10
  | not (turnodeX (ConsT xss)) && tieneSeguidasGanar (ConsT xss) = -10
  | turnodeX (ConsT xss) && tieneSeguidasGanarProf2 (ConsT xss) = -7
  | not (turnodeX (ConsT xss)) && tieneSeguidasGanarProf2 (ConsT xss) =7
  | turnodeX (ConsT xss) && tiene2Seguidas (ConsT xss) X = 5
  | not (turnodeX (ConsT xss)) && tiene2Seguidas (ConsT xss) O = -5
  | turnodeX (ConsT xss) && tiene1Seguidas (ConsT xss) X= 3
  | not (turnodeX (ConsT xss)) && tiene1Seguidas (ConsT xss) O= -3
  |otherwise = 0


siguientesTableros :: Tablero -> [Tablero] --Función que devuelve una lista con los posibles siguientes tableros
siguientesTableros (ConsT xs) 
    | tieneLinea (ConsT xs) = [] 
    | otherwise = [anadeFicha (ConsT xs) n | n<- [1..7], columnaLibre (ConsT xs) n] 

minimax:: Ord b => Int -> (a-> [a]) -> (a-> b) -> ([b]-> b) -> ([b] -> b) -> a -> b --Función minimax
minimax prof expandir evaluar peor mejor probl
   | (prof ==0) || (null siguientes) = evaluar probl
   | otherwise = mejor (map (minimax (prof -1) expandir evaluar mejor peor) siguientes)
   where siguientes = expandir probl

minimaxMain :: Ord b => Int -> (a-> [a]) -> (a-> b) -> a -> a
minimaxMain prof expandir evaluar probl
   | (prof ==0) || (null siguientes) = probl
   | otherwise = snd (maximum' sigVals) 
   where siguientes = expandir probl
         valoraciones = map (minimax (prof -1) expandir evaluar maximum minimum) siguientes
         sigVals = zip valoraciones siguientes


minimaxNuestra:: Int-> Tablero -> Tablero --Función que aplica el algoritmo minimax a nuestro juego
minimaxNuestra prof (ConsT xss) 
  |valorar (ConsT xss) == 10 = snd (head (filter ((==True).fst) (zip (map tieneLinea sigtabs) sigtabs))) 
  |otherwise = minimaxMain prof siguientesTableros valorar (ConsT xss)
  where sigtabs=siguientesTableros (ConsT xss)

maximum':: Ord a => [(a,b)] -> (a,b) --Función que coge el tablero de mayor valoración
maximum' xs
  | length opciones >2 = opciones!!d
  | otherwise = head opciones
    where 
      opciones = [(x,y) | (x,y)<- xs , x == maximum (map fst xs)]
      d = (div (length opciones) 2) 

isPosicion:: String -> [Char] --Función que a partir de un tablero en forma de String devuelve una lista de los Char que están representados como posiciones
isPosicion "" = []
isPosicion (x:xs) 
  | x=='.' || x=='X' || x=='O' = x:(isPosicion xs)
  |otherwise = isPosicion xs

readPosicion:: Char->Posicion --Función que convierte un Char en una Posicion
readPosicion x 
  | x=='X' = X
  | x== 'O' = O
  | otherwise  = Vacio


readFila::[Char] -> [Posicion] --Función que convierte un conjunto de Chars, que representan las posiciones, en una fila de posiciones
readFila xs = map readPosicion xs

readTablero::String -> Tablero --Función que, a partir de un tablero en forma de String, devuelve un tablero con la estructura ya definida
readTablero tab =ConsT (map readFila (filter (/="") (map isPosicion (lines tab))))


introduccion:: IO Tablero
introduccion = do putStrLn "\nComienza el juego de Conecta-4. Si estás preparado, elige una de las siguientes opciones:"
                  putStrLn "1 - Empezar una partida desde el principio"
                  putStrLn "2 - Cargar una partida empezada"
                  opcion <- leeIntEnRango 1 2
                  case opcion of
                    1 -> return (tabInicial)
                    2 -> do partida <- readFile "partidaConecta4.txt"
                            return (readTablero partida) 

                  
    
dificultad :: IO Int
dificultad = do putStrLn "¿En qué nivel quieres jugar?"
                putStrLn "1- Fácil"
                putStrLn "2- Díficil (La duración de las primeras 5 tiradas puede durar 2 minutos cada una) "
                n <- leeIntEnRango 1 2
                return (2*n)

jugar::Int ->Tablero -> IO() --Comenzamos metiendo el tabInicial
jugar p tab = do if (turnodeX tab)
                      then do let tablero =  minimaxNuestra p tab
                              if (tieneLinea tablero)
                                    then do putStrLn (show tablero)
                                            putStrLn "Has perdido :( ¡a practicar más!"
                                    else do if (empate tablero)
                                               then do putStrLn (show tablero)
                                                       putStr "¡Empate! ¡Vaya partida emocionante!"
                                               else do jugar p tablero
                    else do putStrLn "Turno de la máquina"
                            putStrLn (show tab)
                            putStrLn "Tu turno"
                            putStrLn "¿En qué columna quieres jugar? En caso de querer seguir en otro momento, marca 0. "
                            m <-leeIntEnRango 0 7
                            if m==0 
                                then do putStrLn "¡Gracias por jugar! Hasta la próxima :)"
                                        writeFile "partidaConecta4.txt" (show tab)
                                else do let tab1 =  anadeFicha tab m
                                        if (tieneLinea tab1)
                                            then do putStrLn (show tab1)
                                                    putStr "¡Has ganado! Eres demasiado viciad@..."
                                        else do putStrLn (show tab1)
                                                jugar p tab1

main::IO()
main  = do  putStr "Bienvenido al juego del Conecta-4."
            tab <- introduccion 
            p <- dificultad 
            jugar p tab

leeInt::IO Int
leeInt = do c<-getLine
            return (read c)
           
leeIntEnRango::Int -> Int ->IO Int
leeIntEnRango men may =  do putStr ("Introduce un entero entre: "++ show men ++ "   y   " ++ show may ++ ":")
                            n <- leeInt
                            if (n>=men) && (n<=may)
                              then return n 
                              else do putStr "Mal, repite\n"
                                      leeIntEnRango men may
