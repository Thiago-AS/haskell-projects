type Nome = String
type Quantidade = Int
type HorarioProximo = Int
type HoraAtual = Int
type Horario = [Int]
type Medicamento = (Nome,Quantidade)
type Medicamentos = [Medicamento]
type Prescricao = (Nome,Horario,HorarioProximo)
type PlanoMedicamento = [Prescricao]

isInList :: Nome -> Medicamentos -> Bool
isInList _ [] = False
isInList med (x:xs) 
 | med == fst x = True
 | otherwise = isInList med xs

adicionarMedicamento :: Medicamento -> Medicamentos -> Medicamentos
adicionarMedicamento newMed meds 
 | isInList (fst newMed) meds = map (\x -> if fst x == fst newMed then (fst x, snd x + snd newMed) else x) meds 
 | otherwise = newMed: meds

removerMedicamento :: Nome -> Medicamentos -> Medicamentos
removerMedicamento _ [] = []
removerMedicamento name meds
 | isInList name meds = filter (\x -> if fst x /= name then True else False) meds
 | otherwise = meds

consultarMedicamento :: Nome -> Medicamentos -> Medicamento
consultarMedicamento name meds
 | isInList name meds = head $ filter (\x -> if fst x == name then True else False) meds   
 | otherwise = ("", 0)

alterarMedicamento :: Medicamento -> Medicamentos -> Medicamentos
alterarMedicamento newMed meds 
 | isInList (fst newMed) meds = map (\x -> if fst x == fst newMed then newMed else x) meds 
 | otherwise = meds



