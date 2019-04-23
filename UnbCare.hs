type Nome = String
type Quantidade = Int
type HorarioProximo = Int
type HoraAtual = Int
type Horario = [Int]
type Medicamento = (Nome,Quantidade)
type Medicamentos = [Medicamento]
type Prescricao = (Nome,Horario,HorarioProximo)
type PlanoMedicamento = [Prescricao]

isInList :: Medicamento -> Medicamentos -> Bool
isInList med [] = False
isInList med (x:xs) 
 | fst med == fst x = True
 | otherwise = isInList med xs

adicionarMedicamento :: Medicamento -> Medicamentos -> Medicamentos
adicionarMedicamento newMed meds 
 | isInList newMed meds = map (\x -> if fst x == fst newMed then (fst x, snd x + snd newMed) else x) meds 
 | otherwise = newMed: meds 
