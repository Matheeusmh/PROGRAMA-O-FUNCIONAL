horarioTextual :: Int -> Int -> Int -> String

listExt :: Int -> String
listaExt a = ["Zero", "Um", "Dois", "Três", "Quatro", "Cinco", "Seis", "Sete", "Oito", "Nove", "Dez",
              "Onze", "Doze", "Treze", "Quatorze", "Quinze", "Dezesseis", "Dezessete", "Dezoito", "Dezenove",
              "Vinte", "Vinte e um", "Vinte e dois", "Vinte e três", "Vinte e quatro", "Vinte e cinco",
              "Vinte e seis", "Vinte e sete", "Vinte e oito", "Vinte e nove", "Trinta", "Trinta e um",
              "Trinta e dois", "Trinta e três", "Trinta e quatro", "Trinta e cinco", "Trinta e seis",
              "Trinta e sete", "Trinta e oito", "Trinta e nove", "Quarenta", "Quarenta e um", "Quarenta e dois",
              "Quarenta e três", "Quarenta e quatro", "Quarenta e cinco", "Quarenta e seis", "Quarenta e sete",
              "Quarenta e oito", "Quarenta e nove", "Cinquenta", "Cinquenta e um", "Cinquenta e dois",
              "Cinquenta e três", "Cinquenta e quatro", "Cinquenta e cinco", "Cinquenta e seis", "Cinquenta e sete",
              "Cinquenta e oito", "Cinquenta e nove", "Sessenta"] !! a

horarioTextual a b c = listaExt a ++ listaExt b ++ listaExt c

