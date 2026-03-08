import Data.List (find,nub,elem,length)
import Data.Maybe (fromMaybe)

data NFA = NFA { m_edges :: [(Char,[(Int,Int)])] , m_accepts :: [Int], m_nstates :: Int}

-- general helpers
offsetNFA :: NFA -> Int -> NFA
offsetNFA (NFA e a n) offset = NFA (map (\(x,xs) -> (x,map (\(u,v) -> (u + offset,v + offset)) xs )) e) (map (+offset) a) n

findLetterEdges :: Char -> [(Char,[(Int,Int)])] ->  [(Int,Int)]
findLetterEdges chr l = (\(x,y) -> y) $ fromMaybe ('p',[]) m
    where
        m = find (\(x,y) -> x == chr) l

combineEdges :: [(Char,[(Int,Int)])] -> [(Char,[(Int,Int)])] -> [(Char,[(Int,Int)])]
combineEdges l1 l2 =
    [ (letter, findLetterEdges letter l1 ++ findLetterEdges letter l2) | letter <- alphabet]
    where 
        alphabet :: String
        alphabet = "abcdefghijklmnopqrstuvwxyz@"

-- NFA operations
concatNFA :: NFA -> NFA -> NFA
concatNFA (NFA e1 a1 n1) (NFA e2 a2 n2) = 
    let 
        (NFA e2' a2' n2') = offsetNFA (NFA e2 a2 n2) n1
        e = combineEdges (combineEdges e1 e2') [('@',[(node,n1) | node <- a1])]
    in 
        NFA e a2' (n1 + n2)

starNFA :: NFA -> NFA
starNFA (NFA e a n) = newNFA {m_accepts = 0 : m_accepts newNFA}
    where
        e' = combineEdges e [('@',[(node,0) | node <- a])]
        newNFA = concatNFA (NFA [] [0] 1) (NFA e' a n)

unionNFA :: NFA -> NFA -> NFA 
unionNFA (NFA e1 a1 n1) (NFA e2 a2 n2) = 
    let 
        (NFA e1' a1' n1') = offsetNFA (NFA e1 a1 n1) 1
        (NFA e2' a2' n2') = offsetNFA (NFA e2 a2 n2) (n1+1)
        -- nub incase one of the NFA has 0 states
        e3 = [('@',nub [(0,1),(0,n1+1)])]
    in
        NFA (combineEdges (combineEdges e1' e2') e3) (a1' ++ a2') (n1 + n2 + 1)

-- traverse
-- Lambdafy then traverse
lambdafy :: [Int] -> [(Int,Int)] -> [Int]
lambdafy states edges = if newstates == [] then states else lambdafy (states ++ newstates) edges
    where
        newstates = [y | (x,y) <- edges, x `elem` states, not $ y `elem` states]

findnextstates :: [Int] -> [(Int,Int)] -> [Int]
findnextstates states edges = nub [y | (x,y) <- edges, x `elem` states]

traverseNFA :: String -> [Int] -> NFA -> [Int]
traverseNFA "" states (NFA e a n) = lambdafy states $ findLetterEdges '@' e
traverseNFA (x:xs) states (NFA e a n) = traverseNFA xs states'' (NFA e a n)
    where 
        states' = lambdafy states $ findLetterEdges '@' e
        states'' = findnextstates states' $ findLetterEdges x e

acceptNFA :: NFA -> String -> Bool
acceptNFA (NFA e a n) str = foldr (||) False [ x `elem` a | x <- traverseNFA str [0] (NFA e a n)]

data StrWithOp = StrWithOp Int String -- 0 no op ,1 star,2 union
-- brackets square brackets currentBuffer overall left string
partitionString :: Int -> Int -> String -> String -> [String]
partitionString _ _ prev "" = []
partitionString 0 0 prev (x:xs)
    | x == '(' = partitionString 1 0 [x] xs
    | x == '[' = partitionString 0 1 [x] xs
    | otherwise = [x]: partitionString 0 0 "" xs
partitionString a b prev (x:xs) 
    | x == ')' = if a == 1 && b == 0 then
            reverse (x:prev):partitionString 0 0 "" xs
        else
            partitionString (a-1) b (x:prev) xs
    | x == ']' = if a == 0 && b == 1 then
            reverse (x:prev):partitionString 0 0 "" xs
        else
            partitionString a (b-1) (x:prev) xs
    | x == '(' = partitionString (a+1) b (x:prev) xs
    | x == '[' = partitionString a (b+1) (x:prev) xs
    | otherwise = partitionString a b (x:prev) xs

data E = E Int String
createNFAfromStr :: String -> NFA
createNFAfromStr "" = NFA [] [0] 1
createNFAfromStr [x] = NFA [(x,[(0,1)])] [1] 2
createNFAfromStr (x:xs)
    | x == '(' = starNFA $ createNFAfromStr $ init xs
    | x == '[' = foldl (\acc elem -> unionNFA acc (createNFAfromStr elem)) (NFA [] [] 0) [[chr] | chr <- init xs]
    | otherwise = 
        let   
            (l:ls) = map createNFAfromStr $ partitionString 0 0 "" (x:xs)  
        in
            if null ls then
                l
            else
                foldl concatNFA l ls
            
main :: IO()
main = do
    putStrLn "Enter RegEx: "
    regex <- getLine
    putStrLn "Enter String: "
    str <- getLine
    let nfa = createNFAfromStr regex
    let output = if acceptNFA nfa str then "ACCEPTED\n" else "REJECTED\n"
    putStr output