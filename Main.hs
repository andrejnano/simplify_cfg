-- FLP 2019/2020
-- Funkcionalni projekt
-- SIMPLIFY-BKG
-- Author: Andrej Nano (xnanoa00)

-- Required modules
import System.IO
import System.Environment
import Data.Char
import Data.List

-- rule data type record syntax declaration
data Rule = Rule {
  leftSide :: Char,
  rightSide :: String
}

-- context-free grammar data type record syntax declaration
data CFG = CFG {
  nonTerminals :: String,
  terminals :: String,
  rules :: [Rule],
  startSymbol :: Char
}

{---------------------------------------------------
  ALGORITHM PART 1:  4.1
----------------------------------------------------}

-- apply algorithm 4.1. with the goal of deriving set Nt
-- output grammar is going to be G' = { Nt U {S} }
-- generativeNonTerminals ~= Ni
algorithmPart1 :: CFG -> CFG
algorithmPart1 cfg@CFG {
  nonTerminals = n,
  terminals = t,
  rules = r,
  startSymbol = s
} = CFG {
  nonTerminals = filterNonTerminals (generativeNonTerminals ++ [s]),
  terminals = filterTerminals t,
  rules = filterRules r generativeNonTerminals (filterTerminals t),
  startSymbol = s
} where generativeNonTerminals = findGenerativeNonTerminals cfg []

-- Expansion loop until Ni = Ni-1
findGenerativeNonTerminals :: CFG -> String -> String
findGenerativeNonTerminals cfg ni | ni == expandGenerativeNonTerminals cfg ni = ni
                                  | otherwise  = findGenerativeNonTerminals cfg (expandGenerativeNonTerminals cfg ni)

-- This is a single iteration of the Ni set predicate
-- find rules, for which it is true that their right side can be constructed using the lexicon set
-- lexicon set is an iterated union of terminals and Ni
-- from these satisfactory rules, extract their left sides and therefore create a set of nonterminals
expandGenerativeNonTerminals :: CFG -> String -> String
expandGenerativeNonTerminals cfg ni = extractLeftSidesFromRules satisfiedRules
  where satisfiedRules = filter (\rule -> canStringBeConstructedFrom (rightSide rule) lexicon || isEpsilonRule rule) (rules cfg)
        lexicon = ni ++ terminals cfg

-- take a list of rules and extract their left sides and join them into a string
extractLeftSidesFromRules :: [Rule] -> String
extractLeftSidesFromRules = foldr ((:) . leftSide) []

-- By default, for empty string (empty set) it should return True, as * iteration always includes eps
canStringBeConstructedFrom :: String -> String -> Bool
canStringBeConstructedFrom alpha lexicon | null alpha = True
                                         | otherwise = all (`elem` lexicon) alpha

-- Filter rules which are made only from the symbols from Nt and t
filterRules :: [Rule] -> String -> String -> [Rule]
filterRules rules nt t = filter constructedFromLexicon rules
  where lexicon = nt ++ t
        constructedFromLexicon rule = leftSide rule `elem` nt && canStringBeConstructedFrom (rightSide rule) lexicon || isEpsilonRule rule

-- Check if this rule is an epsilon rule (i.e. its right side is "#")
isEpsilonRule :: Rule -> Bool
isEpsilonRule rule = rightSide rule == "#"


{---------------------------------------------------
  ALGORITHM PART 2:  4.2
----------------------------------------------------}

algorithmPart2 :: CFG -> CFG
algorithmPart2 cfg@CFG {
  nonTerminals = n,
  terminals = t,
  rules = r,
  startSymbol = s
} = CFG {
  nonTerminals = n',
  terminals = t',
  rules = r',
  startSymbol = s
} where availableSymbols = findAvailableSymbols cfg [s]
        n' = filterNonTerminals availableSymbols
        t' = filterTerminals availableSymbols
        r' = filterRules r n' t'

-- Vi loop until it ist the same as previous iteration
findAvailableSymbols :: CFG -> String -> String
findAvailableSymbols cfg vi | vi == expandAvailableSymbols cfg vi = vi
                            | otherwise = findAvailableSymbols cfg (expandAvailableSymbols cfg vi)
-- Vi single iteration step, calculating the Vi set
expandAvailableSymbols :: CFG -> String -> String
expandAvailableSymbols cfg vi = nub (vi ++ concatMap rightSide satisfiedRules)
  where satisfiedRules = filter (\rule -> leftSide rule `elem` vi) (rules cfg)


{---------------------------------------------------
  UTILITY FUNCTIONS
----------------------------------------------------}

-- Filter only unique NonTerminal symbols (which are uppercase letters)
filterNonTerminals :: String -> String
filterNonTerminals symbols = nub (filter isUpper symbols)

-- Filter only unique Terminal symbols (which are lowercase letters)
filterTerminals :: String -> String
filterTerminals symbols = nub (filter isLower symbols)

-- Removes comma separators
removeSeparators :: String -> String
removeSeparators = filter (/= ',')

-- Construct new rule from a string
parseRuleFromString :: String -> Rule
parseRuleFromString (left:'-':'>':right) = Rule { leftSide = left, rightSide = right}
parseRuleFromString _ = error "Unable to parse rule from this string input."

-- Prepare nonterminals for printing (add comma separators)
nonTerminalsToPrintFormat :: String -> String
nonTerminalsToPrintFormat n = intercalate "," (map (:[]) n)

-- Prepare terminals for printing (add comma separators)
terminalsToPrintFormat :: String -> String
terminalsToPrintFormat t = intercalate "," (map (:[]) t)

-- ~
startSymbolToPrintFormat :: Char -> String
startSymbolToPrintFormat s = [s]

-- Prepare rules for printing add line separators and arrows between left and right side
rulesToPrintFormat :: [Rule] -> String
rulesToPrintFormat rules = intercalate "\n" (map (\s -> [leftSide s] ++ "->" ++ rightSide s) rules)

-- Prepare the whole CFG for printing
cfgToPrintFormat :: CFG -> String
cfgToPrintFormat cfg =
  nonTerminalsToPrintFormat (nonTerminals cfg) ++ "\n" ++
  terminalsToPrintFormat (terminals cfg) ++ "\n" ++
  startSymbolToPrintFormat (startSymbol cfg) ++ "\n" ++
  rulesToPrintFormat (rules cfg) ++ "\n"

-- take the string representation of CFG and build an internal data representation
parseCFG :: String -> String -> String -> String -> CFG
parseCFG nt t s r = CFG {
  nonTerminals = (filterNonTerminals . removeSeparators) nt,
  terminals = (filterTerminals . removeSeparators) t,
  startSymbol = head s,
  rules = map parseRuleFromString (lines r)
}

-- open file and load in CFG grammar from it
loadCFGFromFile :: String -> IO CFG
loadCFGFromFile filePath = do
  h <- openFile filePath ReadMode
  nt <- hGetLine h
  t <- hGetLine h
  s <- hGetLine h
  r <- hGetContents h
  return (parseCFG nt t s r)

-- open stdin and load in CFG grammar
loadCFGFromStdin :: IO CFG
loadCFGFromStdin = do
  nt <- getLine
  t <- getLine
  s <- getLine
  r <- getContents
  return (parseCFG nt t s r)

-- Check argument count
checkArguments :: [String] -> Bool
checkArguments args | null args = error "No options specified, please specify at least one option"
                    | length args > 2 = error "You specified too many arguments, max 2 is allowed."
                    | otherwise = True

-- Switch on the execution options
execSwitch :: String -> CFG -> IO ()
execSwitch opt cfg  | opt == "-i" = putStr (cfgToPrintFormat cfg)
                    | opt == "-1" = putStr (cfgToPrintFormat $ algorithmPart1 cfg)
                    | opt == "-2" = putStr (cfgToPrintFormat $ algorithmPart2 $ algorithmPart1 cfg)
                    | otherwise = error "Invalid option argument, these options are allowed: -i, -1 and -2."


-- Main and initial function, that checks argument validity, parses options
-- and logically decides how to execute the algorithms on the input
main = do
  arguments <- getArgs
  if checkArguments arguments
    -- if arguments are valid
    then
      if length arguments == 2 then do
         -- if there are 2 arguments read from the specified file path
        let filePath = last arguments
        let option = head arguments
        cfg <- loadCFGFromFile filePath
        execSwitch option cfg
      else do
        -- if the file path is missing, read from stdin
        let option = head arguments
        cfg <- loadCFGFromStdin
        execSwitch option cfg
    else
      -- arguments are invalid
      error "Invalid arguments"