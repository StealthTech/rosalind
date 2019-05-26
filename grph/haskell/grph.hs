data DnaSequence = DnaSequence String String
           | Empty
    deriving Show

-- Перебор совпадений по списку
grph :: [DnaSequence] -> [(DnaSequence, DnaSequence)]
grph fs = concat $ map (\x -> map (\y -> (x, y)) (filter (match x) fs)) fs

-- Поиск совпадений
match :: DnaSequence -> DnaSequence -> Bool
match (DnaSequence n1 d1) (DnaSequence n2 d2) = if d1 == d2 then False else first == last
    where
        first = take 3 d2
        last = drop (length d1 - 3) d1

-- Загрузка последовательностей из файла
loadDnaSequences :: IO String
loadDnaSequences = readFile "grph.input.csv"

-- Текстовое представление результата
reprDnaSequence :: [(DnaSequence, DnaSequence)] -> [Char]
reprDnaSequence x = concat $ map (\(a, b) -> getDnaSequenceName a ++ " " ++ getDnaSequenceName b ++ "\n") x

-- Парсинг строк в список последовательностей ДНК
parseDnaSequences :: [String] -> [DnaSequence]
parseDnaSequences xs = foldl parseLine [] xs

parseLine :: [DnaSequence] -> String -> [DnaSequence]
parseLine xs [] = xs
parseLine [] ys = if isDnaSequenceName ys then [(parseDnaSequenceName ys)] else []
parseLine ((DnaSequence n d):xs) ys = if isDnaSequenceName ys then (parseDnaSequenceName ys):((DnaSequence n d):xs)
                                            else (DnaSequence n (d ++ ys)):xs

-- Получение имени из последовательности ДНК
getDnaSequenceName :: DnaSequence -> String
getDnaSequenceName Empty = "Empty"
getDnaSequenceName (DnaSequence n d) = n

-- Проверка, что строка это название последовательности ДНК
isDnaSequenceName :: String -> Bool
isDnaSequenceName [] = False
isDnaSequenceName (x:xs) = (x == '>')

-- Парсинг названия последовательности ДНК
parseDnaSequenceName :: String -> DnaSequence 
parseDnaSequenceName (x:xs) = DnaSequence xs ""

main = do
    inputFile <- loadDnaSequences

    let fileLines = lines inputFile
    let sequenceList = parseDnaSequences fileLines
    let graphList = grph sequenceList
    let result = reprDnaSequence . reverse $ graphList

    writeFile "grph.output.csv" result
