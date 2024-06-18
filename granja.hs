data animal = animal {
   nombre :: String
   tipo :: 
   peso :: Number
   edad :: Number
   estaEnfermo :: Bool
   diasDeRecuperacion :: Number
} Deriving (Show, Eq)

data veterinario = Veterinario{
    diagnostico :: visitaMedica
    costo :: Number
} Deriving (Show, Eq)
    

type visitaMedica :: Animal -> Animal


laPasoMal :: Animal -> Bool

laPasoMal animal = (>30) $ diasDeRecuperacion animal


nombreFalopa animal = (=='i') . last $ nombre animal

