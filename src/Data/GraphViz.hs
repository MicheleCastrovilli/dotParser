module Data.GraphViz where

import Text.Trifecta
import Data.List
import Control.Applicative
import Control.Monad (void, when)
import Text.Parser.LookAhead

type Strictness = Bool
newtype ID = ID String
  deriving Eq

instance Show ID where
  show (ID x) = x

data Graph = Graph Strictness GraphType (Maybe ID) Dot
  deriving (Eq)

data GraphType = 
    TypeGraph
  | TypeDigraph
  deriving (Eq)

instance Show GraphType where
  show TypeGraph = "graph"
  show TypeDigraph = "digraph"

instance Show Graph where
  show (Graph s t i d) = showStrict s ++ show t 
                      ++ ' ':showID i ++ "{\n" ++ show d ++ "\n}"
    where showStrict True = "strict "
          showStrict False = ""

showID :: Maybe ID -> String
showID (Just x) = show x ++ " "
showID Nothing = ""

data NodeID = NodeID ID (Maybe Port)
  deriving (Eq)

instance Show NodeID where
  show (NodeID i p) = 
    case p of 
      Just po -> show i ++ ' ': show po
      Nothing -> show i

instance Show Port where
  show (PortID p (Just c)) = ": " ++ show p ++ ' ': show c
  show (PortID p Nothing)  = ": " ++ show p
  show (PortComp c) = ": " ++ show c

data Port = 
    PortID ID (Maybe CompassDir)
  | PortComp CompassDir
  deriving (Eq)

data CompassDir =
  CN | CNE | CE | CSE | CS | CSW | CW | CNW | CC | CU
  deriving (Eq)

instance Show CompassDir where
  show CN = "n"
  show CNE = "ne"
  show CE = "e"
  show CSE = "se"
  show CS = "s"
  show CSW = "sw"
  show CW = "w"
  show CNW = "nw"
  show CC = "c"
  show CU = "_"

data SubGraph = SubGraph (Maybe ID) Dot
  deriving (Eq)

instance Show SubGraph where
  show (SubGraph i d) = "subgraph " ++ showID i ++ '{': show d ++ "}"

type Node = Either NodeID SubGraph
type EdgeNode = Either Node Edge

data Edge = 
    Edge Node EdgeOP EdgeNode
  deriving (Eq)

instance Show Edge where
  show (Edge a op (Right b)) = showNode a ++ ' ': show op ++ ' ': show b
  show (Edge a op (Left b))  = showNode a ++ ' ': show op ++ ' ': showNode b

showNode :: Node -> String
showNode (Left x) = show x
showNode (Right x) = show x

data EdgeOP = 
    EdgeDir
  | EdgeUndir
  deriving (Eq)

instance Show EdgeOP where
  show EdgeDir = "->"
  show EdgeUndir = "--"

newtype AttrList = AttrList [Attribute]
  deriving (Eq)

instance Monoid AttrList where
  mempty = AttrList mempty
  mappend (AttrList a) (AttrList b) = AttrList (mappend a b)

instance Show AttrList where
  show (AttrList []) = ""
  show (AttrList a) = " [" ++ intercalate " ; " (map show a) ++ "]"

data Attribute = Attribute ID ID
  deriving (Eq)

instance Show Attribute where
  show (Attribute a b) = show a ++ " = " ++ show b

data AttrType = 
    AttrGraph
  | AttrNode
  | AttrEdge
  deriving (Eq)

instance Show AttrType where
  show AttrGraph = "graph"
  show AttrNode = "node"
  show AttrEdge = "edge"

data Dot = DotNode NodeID AttrList
         | DotEdge Edge AttrList
         | DotAttr AttrType AttrList
         | DotID Attribute
         | DotSub SubGraph
         | DotCons Dot Dot
         | DotEmpty
  deriving (Eq)

instance Monoid Dot where
  mempty = DotEmpty
  mappend DotEmpty x = x
  mappend x DotEmpty = x
  mappend x (DotCons y z) = DotCons (x `mappend` y) z
  mappend x y = DotCons x y

instance Show Dot where
  show (DotNode i a) = show i ++ show a
  show (DotAttr at a) = show at ++ show a
  show (DotID a) = show a
  show (DotSub s) = show s
  show (DotEmpty) = ""
  show (DotCons a b) = show a ++ "\n" ++ show b
  show (DotEdge e a) = show e ++ ' ' : show a

parseGraph :: Parser Graph
parseGraph = Graph 
  <$> (skipOptional intLineComment *>
       ignoreStuff *>
       parseStrict)
  <*> parseGraphType
  <*> optional parseID
  <*> (parseDot <* eof)

parseStrict :: Parser Strictness
parseStrict = option False 
  (const True <$> sToken "strict")

sToken :: String -> Parser String
sToken = mToken . string

mToken :: Parser a -> Parser a
mToken a = a <* ignoreStuff

ignoreStuff :: Parser ()
ignoreStuff = void $ many (
      try comments 
  <|> try whitespace
  <|> void newline
  )

comments :: Parser ()
comments = 
      try lineComment
  <|> cSingleComment 
  <|> cMultiComment

whitespace :: Parser () 
whitespace = void (some whitespaceChars)

whitespaceChars :: Parser Char 
whitespaceChars = char ' ' <|> char '\t'

lineComment :: Parser ()
lineComment = newline *> many whitespaceChars *> intLineComment 
  
intLineComment :: Parser ()
intLineComment = 
  char '#' *> 
  (void $ manyTill anyChar $ lookAhead $
                   void newline <|> eof)

cSingleComment :: Parser ()
cSingleComment = 
  string "//" *>
  (void $ manyTill anyChar (lookAhead (void newline <|> eof)))

cMultiComment :: Parser ()
cMultiComment = 
  string "/*" *>
  (void $ manyTill anyChar (string "*/"))

parseGraphType :: Parser GraphType
parseGraphType =
  const TypeDigraph <$> sToken "digraph" <|> 
  const TypeGraph   <$> sToken "graph"

parseSubGraph :: Parser SubGraph
parseSubGraph = 
  SubGraph <$> optTry optSub
           <*> parseDot
  where optTry = option Nothing . try
        optSub = sToken "subgraph" *>
                 optional parseID

parseNodeID :: Parser NodeID
parseNodeID = NodeID 
  <$> parseID
  <*> optional parsePort

parsePort :: Parser Port 
parsePort = 
  sToken ":" *>
      try (PortID <$> parseID 
              <*> optional 
                  (sToken ":" *> parseCompass)
      )
  <|> PortComp <$> (parseCompass)

parseCompass :: Parser CompassDir
parseCompass = 
    choice [ ch CN  "n"
           , ch CNE "ne"
           , ch CE  "e"
           , ch CSE "se"
           , ch CS  "s"
           , ch CSW "sw"
           , ch CW  "w"
           , ch CNW "nw"
           , ch CC  "c"
           , ch CU  "_"
           ]
    where ch a b = const a <$> try (sToken b)

parseID :: Parser ID
parseID = (ID <$> 
  (   parseBareString 
  <|> parseNumeric
  <|> stringLiteral
  <|> parseHTMLString
  ))

parseBareString :: Parser String
parseBareString = mToken $ do
  a <- oneOf allowedChars
  b <- many (oneOf (allowedChars ++ digits))
  let res = a : b
  when (res `elem` keywords)
    (fail "Identifier cannot be a keyword")
  return res

parseNumeric :: Parser String
parseNumeric = show <$> (mToken double)

parseHTMLString :: Parser String
parseHTMLString = sToken "<" *> fail "Not yet implemented"

allowedChars :: [Char]
allowedChars = '_':['a'..'z'] ++ ['A'..'Z'] 
               ++ ['\200'..'\377'] 

digits :: [Char]
digits = ['0'..'9']

keywords :: [String]
keywords = ["strict", "graph", "digraph",
            "node",   "edge",  "subgraph"]

parseDot :: Parser Dot
parseDot = 
  sToken "{" *> parseStList <* sToken "}"

parseStList :: Parser Dot
parseStList = 
  mconcat <$> many (parseStat <* 
                    optional (sToken ";"))

parseStat :: Parser Dot
parseStat =  
  choice [ try nodeStat
         , try edgeStat
         , try attrStat
         , try ididStat
         , try subgStat
         ]

nodeStat :: Parser Dot
nodeStat = 
  DotNode <$> parseNodeID
          <*> (option mempty parseAttrList <* 
                 notFollowedBy (void (sToken "=") <|>
                                void parseEdgeOP))
                                              

edgeStat :: Parser Dot
edgeStat = 
  DotEdge <$> parseEdge
          <*> option mempty parseAttrList

attrStat :: Parser Dot
attrStat =
  DotAttr <$> parseAttrType
          <*> parseAttrList

ididStat :: Parser Dot
ididStat =
  DotID <$> parseAttribute

subgStat :: Parser Dot
subgStat =
  DotSub <$> parseSubGraph

parseAttribute :: Parser Attribute
parseAttribute =
  Attribute <$> parseID
            <*> (sToken "=" *> parseID)

parseEdge :: Parser Edge
parseEdge = 
  Edge <$> parseEdgeNode 
       <*> parseEdgeOP
       <*> (Right <$> (try parseEdge)
           <|> Left <$> parseEdgeNode)
  
parseEdgeNode :: Parser Node
parseEdgeNode = 
      Left  <$> parseNodeID
  <|> Right <$> parseSubGraph

parseEdgeOP :: Parser EdgeOP
parseEdgeOP =
      const EdgeDir   <$> try (sToken "->")
  <|> const EdgeUndir <$> try (sToken "--")

parseAttrList :: Parser AttrList
parseAttrList = 
 toAttrList <$> some (aList)
  where toAttrList = (AttrList . concat . concat)
        aList = sToken "[" *> some parseAList <* sToken "]"

parseAList :: Parser [Attribute]
parseAList = 
  some (parseAttribute <*
        skipOptional (sToken ";" <|> sToken ",") 
       )

parseAttrType :: Parser AttrType
parseAttrType = 
      const AttrGraph <$> sToken "graph"
  <|> const AttrNode  <$> sToken "node"
  <|> const AttrEdge  <$> sToken "edge"

parseFileGraph :: FilePath -> IO (Result Graph)
parseFileGraph f = parseFromFileEx parseGraph f

parse :: String -> Result Graph
parse = parseString parseGraph mempty
