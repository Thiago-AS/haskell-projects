import Data.List
import Data.Ord

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

adicionarMedicamento :: Medicamento -> Medicamentos -> Medicamentos
adicionarMedicamento newMed meds 
 | isInList (fst newMed) meds = map (\x -> if fst x == fst newMed then (fst x, snd x + snd newMed) else x) meds 
 | otherwise = newMed: meds

removerMedicamento :: Nome -> Medicamentos -> Medicamentos
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

tomarMedicamentoSOS :: Nome -> Medicamentos -> Medicamentos
tomarMedicamentoSOS name meds
 | isInList name meds = map (\x -> if fst x == name then (fst x, snd x - 1) else x) meds
 | otherwise = meds

cadastrarAlarmes :: PlanoMedicamento -> Horario
cadastrarAlarmes [] = []
cadastrarAlarmes medSqd = sort $ nub $ concat $ [hour |(_, hour, _) <- medSqd]

listarMedicamentosComprar :: Medicamentos ->  Medicamentos
listarMedicamentosComprar [] = []
listarMedicamentosComprar meds = [(name, amnt)| (name, amnt) <- meds, amnt == 0]

comprarMedicamentosDias ::  PlanoMedicamento -> Medicamentos -> Int -> Medicamentos
comprarMedicamentosDias medSqd meds days = [ (name1, if totalAmnt < 0 then 0 else totalAmnt) | (name1, hours, _) <- medSqd ,(name2,amnt) <- meds, let totalAmnt = (length hours * days - amnt), name1 == name2]

isAllInList :: Medicamentos -> [(Medicamento,Preco)] -> (Bool, Int)
isAllInList meds drugStock = (length avaibleMeds == length meds, foldl (+) 0 (map snd avaibleMeds)) where
 avaibleMeds = [ medPrice | med <- meds, medPrice <- drugStock, fst med == fst (fst medPrice) && snd (fst medPrice) >= snd med]
 
comprarMedicamentosPreco :: Medicamentos -> Mercado -> Compra
comprarMedicamentosPreco meds market = minimumBy (comparing fst) [ (snd boolPrice, fst drugStore) | drugStore <- market, let boolPrice = isAllInList meds (snd drugStore), fst boolPrice]







