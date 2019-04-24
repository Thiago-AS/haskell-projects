import Data.List
import Data.Ord

-- DATA TYPES

type Nome = String
type Quantidade = Int
type HorarioProximo = Int
type HoraAtual = Int
type Horario = [Int]
type Medicamento = (Nome,Quantidade)
type Medicamentos = [Medicamento]
type Prescricao = (Nome,Horario,HorarioProximo)
type PlanoMedicamento = [Prescricao]

type Preco = Int
type Farmacia = (Nome,[(Medicamento,Preco)])
type Mercado = [Farmacia]
type Compra = (Preco, Nome)

isInList :: Nome -> Medicamentos -> Bool
isInList _ [] = False
isInList med (x:xs) 
 | med == fst x = True
 | otherwise = isInList med xs

-- QUESTÃO 1

adicionarMedicamento :: Medicamento -> Medicamentos -> Medicamentos
adicionarMedicamento newMed meds 
 | isInList (fst newMed) meds = map (\x -> if fst x == fst newMed then (fst x, snd x + snd newMed) else x) meds 
 | otherwise = newMed: meds

-- QUESTÃO 2

removerMedicamento :: Nome -> Medicamentos -> Medicamentos
removerMedicamento name meds
 | isInList name meds = filter (\x -> if fst x /= name then True else False) meds
 | otherwise = meds

-- QUESTÃO 3

consultarMedicamento :: Nome -> Medicamentos -> Medicamento
consultarMedicamento name meds
 | isInList name meds = head $ filter (\x -> if fst x == name then True else False) meds   
 | otherwise = ("", 0)

-- QUESTÃO 4

alterarMedicamento :: Medicamento -> Medicamentos -> Medicamentos
alterarMedicamento newMed meds 
 | isInList (fst newMed) meds = map (\x -> if fst x == fst newMed then newMed else x) meds 
 | otherwise = meds

-- QUESTÃO 5

tomarMedicamentoSOS :: Nome -> Medicamentos -> Medicamentos
tomarMedicamentoSOS name meds
 | isInList name meds = map (\x -> if fst x == name then (fst x, snd x - 1) else x) meds
 | otherwise = meds

-- QUESTÃO 6

getTime :: Int -> HoraAtual -> Horario -> HorarioProximo
getTime initHour _ [] = initHour 
getTime initHour aTime (x:xs)
 | x > aTime = x
 | otherwise = getTime initHour aTime xs

tomarMedicamentosHorario :: PlanoMedicamento -> Medicamentos -> HoraAtual -> (PlanoMedicamento, Medicamentos) 
tomarMedicamentosHorario medScd meds time = (newPlan, newMeds) where 
 newPlan = [ (name, hourList, if aTime == time then getTime (head hourList) aTime hourList else aTime) | (name, hourList, aTime) <- medScd]
 newMeds = [ (name1, if aTime == time then amnt - 1 else amnt) | (name1, _, aTime) <- medScd, (name2, amnt) <- meds, name1 == name2] 

-- QUESTÃO 7

cadastrarAlarmes :: PlanoMedicamento -> Horario
cadastrarAlarmes [] = []
cadastrarAlarmes medSqd = sort $ nub $ concat $ [hour |(_, hour, _) <- medSqd]

-- QUESTÃO 8

listarMedicamentosComprar :: Medicamentos ->  Medicamentos
listarMedicamentosComprar [] = []
listarMedicamentosComprar meds = [(name, amnt)| (name, amnt) <- meds, amnt == 0]

-- QUESTÃO 9

comprarMedicamentosDias ::  PlanoMedicamento -> Medicamentos -> Int -> Medicamentos
comprarMedicamentosDias medSqd meds days = [ (name1, if totalAmnt < 0 then 0 else totalAmnt) | (name1, hours, _) <- medSqd ,(name2,amnt) <- meds, let totalAmnt = (length hours * days - amnt), name1 == name2]

-- QUESTÃO 10

isAllInList :: Medicamentos -> [(Medicamento,Preco)] -> (Bool, Int)
isAllInList meds drugStock = (length avaibleMeds == length meds, foldl (+) 0 (map snd avaibleMeds)) where
 avaibleMeds = [ medPrice | med <- meds, medPrice <- drugStock, fst med == fst (fst medPrice) && snd (fst medPrice) >= snd med]

comprarMedicamentosPreco :: Medicamentos -> Mercado -> Compra
comprarMedicamentosPreco meds market = minimumBy (comparing fst) [ (snd boolPrice, fst drugStore) | drugStore <- market, let boolPrice = isAllInList meds (snd drugStore), fst boolPrice]
