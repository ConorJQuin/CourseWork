{-
- Name: Conor John Quin.
- Number: 114400402.
- Assignment: 03.
-}



--this function takes in a string(s) and returns the pig latin version of 
--that string
translate_both::String ->String
translate_both s = unwords(bothLst (words s))

--this function takes in a list of strings and translates each string
--in the list to pig latin
bothLst  :: [String] -> [String]
bothLst []= []
bothLst (s:ss)  | isVowel (s!!0) = voweler s : bothLst ss
                | isLetter(s!!0) = consonanter s : bothLst ss
                | otherwise = s : bothLst ss

--this function takes in a string(s) and returns a string with every word in that string
--starting with a vowel to pig latin and leaves the words which don't start
--with a vowel unchanged
vowel_translate :: String->String
vowel_translate [] = ""
vowel_translate s = unwords(vowelerLst (words s))

--this function takes in a string(s) and translates the first word
--in the string to pig latin if it starts with a vowel, if the first
--word does not start with a vowel then the first word remains unchanged
voweler :: String -> String
voweler s  | notVowel (s!!0) = s
           | otherwise = (lett++"way" ++noLett) 
           where (lett,noLett) =span (isLetter) (s)

--this function takes in a list of strings(s) and translates each string starting 
--with a vowel to pig latin. Any string not starting with a vowel is unchanged
vowelerLst :: [String] -> [String]
vowelerLst [] = []
vowelerLst (s:ss) = voweler s : vowelerLst ss

--this function takes in a string and returns a string with every word in that string
--starting with a consonant to pig latin and leaves the words which don't start
--with a consonant unchanged
consonant_translate :: String->String
consonant_translate [] =[] 
consonant_translate s = unwords(consonanterLst (words s))

--this function takes in a list of strings(s) and translates each string starting 
--with a consonant to pig latin. Any string not starting with a vowel is unchanged
consonanterLst::[String]->[String]
consonanterLst [] = []
consonanterLst (s:ss) = consonanter s : consonanterLst ss

--this function takes in a string(s) and translates the first word
--in the string to pig latin if it starts with a consonant, if the first
--word does not start with a consonant then the word remains unchanged
consonanter::String->String
consonanter s  | isVowel (s!!0) = s
               | otherwise = (vow++cons++"ay"++ender(s)) 
            where (cons,vow) =span (notVowel) (lett) 
                    where (lett,noLett) = span (isLetter) s

--this function takes in a string(s) and checks if each character in the string is
--a letter. If the function finds a character that is not a letter it returns
--the rest of the string starting from this none letter character
ender:: String ->String
ender s = noLett where (lett,noLett) = span isLetter (s)

--this function takes in a character(c) and checks if it's a vowel
isVowel :: Char -> Bool
isVowel c = (elem [c] [['a'],['e'],['i'],['o'],['u']])

--this function takes in a character(c) and checks if it's a letter
isLetter :: Char->Bool
isLetter c = (elem [c] [['a'],['b'],['c'],['d'],['e'],['f'],['g'],['h'],['i'],
        ['j'],['k'],['l'],['m'],['n'],['o'],['p'],['q'],['r'],['s'],['t'],
        ['u'],['v'],['w'],['x'],['y'],['z']])

--this function takes in a character(c) and checks if it's not a vowel
notVowel :: Char -> Bool
notVowel c = not(isVowel c)

main :: IO()
main = do line <- getLine
          putStrLn(translate_both line)