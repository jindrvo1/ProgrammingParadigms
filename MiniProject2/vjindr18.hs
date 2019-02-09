-- Name: Vojtěch Jindra
-- Email: vjindr18@student.aau.dk

-- Definition of the Tree data type; for our purpose, argument 'a' will always be Char.
data Tree a = Node (Tree a) (Tree a) | Leaf a deriving (Show, Eq)
-- Weighted tree defined as a tuple of a tree and the root's weight as an integer.
type LTree a = (Tree a, Int)
-- Definition of association list of types 'a' and 'b' ('a' is the key, 'b' is the value).
type AssocList a b = [(a, b)]

-- isKeyInLst returns True if a key is present in given association list, False otherwise.
isKeyInLst :: Char -> AssocList Char Int -> Bool
isKeyInLst _ [] = False
isKeyInLst key ((x, _):xs) = if key == x 
                                then True 
                                else isKeyInLst key xs

-- increaseCnt increases the number of ocurrences of given letter in the AssocList                           
increaseCnt :: Char -> AssocList Char Int -> AssocList Char Int                               
increaseCnt _ [] = []
increaseCnt key ((x1, x2):xs) = if key == x1 
                                    then [(x1, x2 + 1)] ++ xs 
                                    else [(x1, x2)] ++ increaseCnt key xs

-- letterCnt' is used for counting the letters in input sentence.
-- Each letter is represented as a key in the AssocList Char Int, where the value stands for the number of times the letter is present.  
letterCnt' :: String -> AssocList Char Int -> AssocList Char Int                            
letterCnt' [] aLst = aLst
letterCnt' (x:xs) aLst = if isKeyInLst x aLst 
                            then letterCnt' xs (increaseCnt x aLst)
                            else letterCnt' xs (aLst ++ [(x, 1)])

-- letterCnt provides more convenient call of letterCnt' which requires more arguments.                     
letterCnt :: String -> AssocList Char Int
letterCnt str = letterCnt' str []

-- convertAssocListToLTrees converts given association list to a list of wighted Leaves.
convertAssocListToLTrees :: AssocList Char Int -> [LTree Char]
convertAssocListToLTrees [] = []
convertAssocListToLTrees ((letter, val):xs) = [(Leaf letter, val)] ++ (convertAssocListToLTrees xs)

-- min' finds the LTree with the smallest weight in an array of LTrees.
min' :: [LTree Char] -> LTree Char
min' [x] = x
min' (x:xs) = smaller x (min' xs)

-- smaller is a helping function for min'.
smaller :: LTree Char -> LTree Char -> LTree Char
smaller x y = if (snd x) < (snd y) then x else y

-- sortLTrees ascendingly sorts an array of LTrees by their weight.
sortLTrees :: [LTree Char] -> [LTree Char]
sortLTrees [] = []
sortLTrees lst = [min' lst] ++ (sortLTrees [x | x <- lst, x /= (min' lst)])

-- createTree recursively creates a weighted Note out of first two LTrees in sorted array.
-- Returns one LTree created out of all Nodes accordingly to the Huffman encoding.
createTree :: [LTree Char] -> Tree Char
createTree [x] = fst x
createTree (x1:x2:xs) = createTree (sortLTrees ([(Node (fst x1) (fst x2), (snd x1) + (snd x2))] ++ xs))

-- extractBinary' assigns a binary representation of a letter to an association list with keys being the letter and value the binary representation.
extractBinary' :: Tree Char -> String -> AssocList Char String
-- Following line ensures that sentences with only one unique letter are handled correctly.
extractBinary' (Leaf a) str = if length(str) < 1
                                then [(a, "0")]
                                else [(a, str)]
extractBinary' (Node a b) str = [] ++ extractBinary' a (str ++ "0") ++ extractBinary' b (str ++ "1")

-- extractBinary provides more convenient call of extractBinary' which requires more arguments.
extractBinary :: Tree Char -> AssocList Char String
extractBinary lst = extractBinary' lst ""

-- assocListLookup finds the value of a given key in provided association list.
assocListLookup :: Char -> AssocList Char String -> String
-- The assocListLookup _ [] should never be used in our case, however it is provided as a fallback.
assocListLookup _ [] = []
assocListLookup key ((x1,x2):xs) = if key == x1
                                    then x2
                                    else assocListLookup key xs

-- encode converts every letter in given sentence to its binary representation based on provided association list with binary representations of every letter.                                
encode :: String -> AssocList Char String -> String
encode [] _ = []
encode (x:xs) aLst = (assocListLookup x aLst) ++ (encode xs aLst)

-- dedcode' converts the binary representation back to text by searching the created tree.
decode' :: String -> Tree Char -> Tree Char -> String -> String
decode' [] (Leaf a) (Node b c) res = res ++ [a]
-- Following two lines ensure that sentences with only one unique letter are handled correctly.
decode' [] (Leaf a) (Leaf b) res = res
decode' (x:xs) (Leaf a) (Leaf b) res = decode' xs (Leaf a) (Leaf b) (res ++ [a])
decode' text (Leaf a) root res = decode' text root root (res ++ [a])
decode' (x:xs) (Node a b) root res = if x == '0'
                                        then decode' xs a root res
                                        else decode' xs b root res

-- decode provides more convenient call of decode' which requires more arguments.                                        
decode :: String -> Tree Char -> String
decode encodedText encoding = decode' encodedText encoding encoding ""

main = do
    putStrLn "Input text to encode:"
    -- First we take the sentence to encode from stdin.
    text <- getLine
    
    -- We count the number of occurrences of each letter.
    let letterCount = letterCnt text
    -- Then create appropriate weighted Leaves (of type Char with Int weight).
    let leaves = convertAssocListToLTrees letterCount
    -- We sort it ascendingly.
    let sortedLeaves = sortLTrees leaves
    -- We create a binary tree out of the leaves.
    let mainTree = createTree sortedLeaves
    -- And we find out the correct encoding of each letter.
    let encoding = extractBinary mainTree

    -- Then we encode the text given by user.
    let encodedText = encode text encoding
    -- And we decode it back by using the binary tree.
    let decodedText = decode encodedText mainTree

    putStrLn ("Encoded text: " ++ encodedText)
    putStrLn ("Decoded text: " ++ decodedText)
