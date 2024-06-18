data animal = animal {
   nombre :: String
   tipo :: 
   peso :: Number
   edad :: Number
   estaEnfermo :: Bool
   diasDeRecuperacion :: Number
   costo :: Number
} Deriving (Show, Eq)

data veterinario = Veterinario{
    diagnostico :: visitaMedica
    diasDeRecuperacion :: Number
    costo :: Number
    vitaminas :: Number
} Deriving (Show, Eq)
    

type visitaMedica :: Animal -> Animal


laPasoMal :: Animal -> Bool

laPasoMal animal = (>30) $ diasDeRecuperacion animal


nombreFalopa animal = (=='i') . last $ nombre animal


engorde :: kilosComida -> animal -> animal

engorde kilosComida animal = animal {
   peso = (+) max (5, kilosComida/2) $ peso animal
}


visitaMedica veterinario animal = animal {
   estaEnfermo = False
   diasDeRecuperacion = diasDeRecuperacion veterinario
   costo = costo veterinario
}
   


revisacion :: veterinario -> animal -> animal

revisacion veterinario animal = (+2) peso . (diagnostico veterinario) $ animal


festejoCumple :: animal -> animal

festejoCumple animal = animal {
 edad = (+1) edad animal
 peso = (-1) peso animal
}


chequeoDePeso :: pesoLimite -> animal -> animal

chequeoDePeso pesoLimite animal
   | peso animal < pesoLimite = animal{estaEnfermo=True}
   | otherwise = animal{estaEnfermo=False}


type proceso = [actividad]

proceso animal  = foldl ($) animal proceso


mejoraONoMejora [] _ = animal
mejoraONoMejora proceso animal = (<3) && (> peso animal) head proceso $ mejoraONoMejora proceso animal



type animales = [animal]

giveMeThree animales = take 3 . filter (nombreFalopa animal) animales


//seria posible por el lazy evaluation y la funcion "take" que solo toma los 3 primeros de cualquier lista que le deseemos pasar sin importarle la cantidad de elementos que esta misma posea

