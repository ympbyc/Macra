module Macra.Parser (runTimeExpr,
                     compileTimeExpr,
                     Identifier(..),
                     MacCxtNode(..),
                     Node(..),
                     CxtId,
                     MacSig,
                     MacParams) where

import Control.Monad
import Control.Applicative hiding ( (<|>)
                                  , many )
import qualified Text.ParserCombinators.Parsec as P
import Text.ParserCombinators.Parsec hiding (spaces)

-- シンボルの id
type Identifier = String

-- マクロ定義や、将来追加されるかもしれない
-- #include など、 `#' から始まるコンパイル時の命令
data MacCxtNode -- 普通のマクロ定義。
                -- #[ m a : t -> u = a ]
                = MacDef1MNode Identifier MacSig MacParams Node
                -- #!/usr/bin/env macra -opt
                -- であれば、 "/usr/bin/env" が FilePath 、 "macra -opt" が String となる
                -- 通常は使用しないだろう、いちおうパースして使用できるようにしておく
                | Shebang FilePath String
                -- # include prelude.macra
                -- `#' と include の間に空白はあってもなくてもいい。
                -- 拡張子は省略不可能。
                -- もし https://github.com/pasberth/Macra/issues/40 で拡張子に意味を持たせるなら、
                -- .macra だけ省略できるというのは不自然
                | Include FilePath
                -- # require prelude.macra
                | Require FilePath
                deriving (Show, Eq)

type CxtId = String              -- マクロのコンテキストのid
type MacSig = [CxtId]            -- マクロのシグネチャ
type MacParams = [Identifier]    -- マクロの仮引数

data Node = SymNode Identifier
          | CharNode Char
          | NumNode  Double
          | NilNode
          | IfNode Node Node Node
          | LambdaNode Identifier Node
          | DefineNode Identifier Node
          | FuncallNode Node Node
          | PrintNode Node
          | ConsNode Node Node
          | CarNode Node
          | CdrNode Node
          | DoNode Node Node
          | NativeNode Integer
          | EqualNode Node Node
          | DelayNode Node
          deriving (Eq)

instance Show Node where
  show NilNode = "nil"
  show (SymNode sym) = concat ["'", sym]
  show (CharNode c) = show c
  show (NumNode n) = show n
  show (IfNode a b c) = concat ["!if", (indent2 $ show a), (indent2 $ show b), (indent2 $ show c)]
  show (LambdaNode a b) = concat ["!lambda", (indent2 $ show a), (indent2 $ show b)]
  show (DefineNode a b) = concat ["!define", (indent2 $ show a), (indent2 $ show b)]
  show (FuncallNode a b) = concat ["!funcall", (indent2 $ show a), (indent2 $ show b)]
  show (PrintNode a) = concat ["!print", (indent2 $ show a)]
  show (ConsNode a b) = concat ["!cons", (indent2 $ show a), (indent2 $ show b)]
  show (CarNode a) = concat ["!car", show a]
  show (CdrNode a) = concat ["!cdr", show a]
  show (DoNode a b) = concat ["!do", (indent2 $ show a), (indent2 $ show b)]
  show (NativeNode a) = "!native " ++ (show a)
  show (EqualNode a b) = concat ["!equal", (indent2 $ show a), (indent2 $ show b)]

indent :: String -> String -> String
indent idt node = foldl (\str x -> concat [str, "\n", idt,  x]) "" (lines node)
indent2 node = indent "  " node


----------------------------------------
-- Compile Time Statements
----------------------------------------
compileTimeExpr :: Parser [MacCxtNode]
compileTimeExpr = many $ try $ do
                    skipProgram
                    string "#"
                    compileTimeExprNonSharp
                 where skipProgram = skipMany $ noneOf "#"

compileTimeExprNonSharp = macDef <|> include <|> require <|> shebang

shebang :: Parser MacCxtNode
shebang = try $ Shebang
                <$> ( char '!' >> skipMany (oneOf " \t") >> many1 (noneOf " \t\n") )
                <*> ( skipMany (oneOf " \t") >> many (noneOf "\n"))

include :: Parser MacCxtNode
include = try $ Include <$> ( skipSpaces >> string "include" >> skipSpaces >> many1 (noneOf " \t\n"))

require :: Parser MacCxtNode
require = try $ Require <$> ( skipSpaces >> string "require" >> skipSpaces >> many1 (noneOf " \t\n"))

macDef :: Parser MacCxtNode
macDef = macDef1 <?> "macro defination"
       where macDef1 = try $ (\(id, params) sig defi end -> MacDef1MNode id sig params defi)
                             <$> ( skipSpaces >> string "["
                                >> skipSpaces >> idAndParams )
                             <*> ( requireSpaces >> string ":"
                                >> requireSpaces >> macSig )
                             <*> ( requireSpaces >> string "="
                                >> requireSpaces >> semicolon )
                             <*> (skipSpaces >> string "]")

             macSig :: Parser MacSig
             macSig = macSig' <?> "signature"
                    where macSig' = try $ do
                                  -- toplevel の名前は '*'
                                  cxt <- string "*" <|> symbol
                                  (try $ (\list -> cxt:list)
                                         <$> (requireSpaces >> string "->" >> requireSpaces >> macSig'))
                                    <|> return [cxt]

             idAndParams :: Parser (Identifier, MacParams)
             idAndParams = brackets <|> infixMacDef <|> prefixMacDef <|> suffixMacDef
                         where brackets = bracket "(" ")" <|>
                                          bracket "[" "]" <|>
                                          bracket "{" "}"
                               bracket beg end = try $ (\beg param end -> (beg, [param]))
                                                       <$> (string beg)
                                                       <*> (skipSpaces >> symbol)
                                                       <*> (skipSpaces >> (string end))
                               infixOpList = [ (++) <$> string ":" <*> mark
                                             , string "=>"
                                             , string "->"
                                             , string ","
                                             , string ";"
                                             ]
                               infixOp = foldl (<|>)
                                               (head infixOpList)
                                               (tail infixOpList)
                               infixMacDef = try $ (\param1 id param2 -> (id, [param1, param2]))
                                                   <$> symbol
                                                   <*> (skipSpaces >> infixOp)
                                                   <*> (skipSpaces >> symbol)
                               prefixMacDef = try $ (\id params -> (id, params))
                                                    <$> symbol
                                                    <*> many (try $ requireSpaces >> symbol)
                               suffixMacDef = try $ (\params id -> (id, params))
                                                    <$> many1 symbol
                                                    <*> (skipSpaces >> ((++) <$> string "@" <*> mark))

----------------------------------------
-- Runtime Expression
----------------------------------------
runTimeExpr :: Parser Node
runTimeExpr = do { skipSpaces
                 ; expr <- semicolon
                 ; skipSpaces
                 ; eof
                 ; return expr
                 } <?> "a program containing at least one expression."

-- もっとも優先順位の低い中置関数。
-- a; b; c は a ; (b ; c) のように右に再帰する。
-- semicolon-expression:
--   coloninfix-expression; semicolon-expression
--   coloninfix-expression
semicolon :: Parser Node
semicolon = try semicolon' <|> coloninfix <?> "semicolon-expression"
          where semicolon' = (\expr1 sym -> FuncallNode (FuncallNode (SymNode sym) expr1))
                           <$> coloninfix
                           <*> (skipSpaces >> string ";")
                           <*> (skipSpaces >> semicolon)

-- coloninfix-expression:
--   coloninfix-expression :identifier funcall-expression
--   funcall-expression
coloninfix :: Parser Node
coloninfix = coloninfix' <?> "coloninfix-expression"
           where infixOp = do { skipSpaces
                              ; x <- ((++) <$> string ":" <*> mark)
                              ; skipSpaces
                              ; return (FuncallNode . (FuncallNode . SymNode) x)
                              }
                 coloninfix' = try $ do
                             expr <- funcall
                             exprs <- many (try $ flip <$> infixOp <*> (skipSpaces >> funcall))
                             return $ foldl (\a b -> b a) expr exprs

-- funcall-expression:
--   funcall-expression arrow-expression
--   arrow-expression
funcall :: Parser Node
funcall = funcall' <?> "funcall-expression"
        where prefixOp = requireSpaces >> return FuncallNode
              funcall' = try $ do
                       expr <- arrow
                       exprs <- many (try $ flip <$> prefixOp <*> (skipSpaces >> arrow))
                       return $ foldl (\a b -> b a) expr exprs


-- arrow-expression:
--   primary-expression => funcall-expression
--   primary-expression -> funcall-expression
--   primary-expression , funcall-expression
--   primary-expression
arrow :: Parser Node
arrow = arrow' <|> prim <?> "arrow-expression"
      where arrow' = try $ (\expr1 sym -> FuncallNode (FuncallNode (SymNode sym) expr1))
                           <$> prim
                           <*> (skipSpaces >> arrowMark)
                           <*> (skipSpaces >> funcall)

            arrowMark = foldl (\x mark -> x <|> (try $ string mark))
                              (try $ string $ head arrowList)
                              (tail arrowList)

            arrowList = [ "=>"
                        , "->"
                        , ","
                        ]


-- primary-expression:
--   [ semicolon-expression ]
--   { semicolon-expression }
--   ( semicolon-expression )
--   exclam-expression
--   identifier
--   constant
prim :: Parser Node
prim = bracket <|> exclamExpr <|> strLit <|> charLit <|> id <|> num
     where bracket = try $ do { (beg, end) <- bracketBeg
                              ; skipSpaces
                              ; expr <- semicolon
                              ; skipSpaces
                              ; string end
                              ; return (FuncallNode (SymNode beg) expr)
                              }
           brackets = [ ("[", "]")
                      , ("(", ")")
                      , ("{", "}")
                      ]
           bracketBeg :: Parser (String, String)
           bracketBeg = bracketBeg' brackets

           bracketBeg' :: [(String, String)] -> Parser (String, String)
           -- satisfy (const False) は常に失敗するので、 ("", "") が返る
           -- ことはあり得ない
           bracketBeg' [] = satisfy (const False) >> return ("", "")
           bracketBeg' ((beg, end):brackets) =
             (string beg >> return (beg, end)) <|> bracketBeg' brackets

           id :: Parser Node
           id = SymNode <$> try symbol

           strLit :: Parser Node
           strLit = do
                  str <- str'
                  return $ foldr ConsNode NilNode str
                  where char' :: Parser Node
                        char' = liftM CharNode (try (string "\\\"" >> return '"')
                                             <|> noneOf ['"'])
                        str' = (between (char '"') (char '"') (many char')) <?> "a string"

           charLit :: Parser Node
           charLit = liftM CharNode (prefix >> anyChar)
                   where prefix = try $ string "$'"

           num :: Parser Node
           num = num' <?> "a number"
               where num' = try $ do
                          sign  <- char '-' <|> return ' '
                          int   <- string "0" <|> many1 digit
                          float <- (char '.' >> many1 digit) <|> return "0"
                          return $ NumNode $ read $ concat [[sign], int, ".", float]

-- hoge :<> fuga とかの構文で使える記号の id
--   使える記号はまだ仕様が曖昧なので
--   ruby -e 'puts [*33..47, *58..64, *91..96, *123..126].map(&:chr).join'
-- で出力したものを使えるようにしてる。
mark :: Parser Identifier
mark = mark' <|> symbol
     where mark' = many1 letter
           letter = oneOf "!\"#$%&'()*+,-./;<=>?@[\\]^_`{|}~"


symbol :: Parser Identifier
symbol = symbol' <?> "symbol"
       where symbol'       = try (pure (\beg end -> beg:end)) <*> beginLetter <*> symbolEnd
             beginLetter   = letter <|> oneOf "_"             -- シンボルの開始として許される文字。 abc の a
             containLetter = letter <|> digit <|> oneOf "-_"  -- シンボルに含める文字。 abc の b
             endLetter     = letter <|> digit <|> oneOf "_"   -- シンボルの終わりに含める文字。 abc の c
             symbolEnd     = symbolEnd1 <|> return []
             symbolEnd1    = (try $ do { lett <- containLetter
                                       ; last <- symbolEnd1
                                       ; return (lett:last)
                                       })
                             <|> try ((\lett -> [lett]) <$> endLetter)

exclamExpr :: Parser Node
exclamExpr = try $ string "!" >> ( excIf <|> excLambda <|> excDefine <|>
                                   excFuncall <|> excPrint <|> excCons <|>
                                   excCar <|> excCdr <|> excDo <|> excNative
                                   <|> excEqual )
            where excIf :: Parser Node
                  excIf = IfNode
                          <$> (try $ string "if" >> requireSpaces >> parseExpr)
                          <*> (skipSpaces >> parseExpr)
                          <*> (skipSpaces >> parseExpr)

                  excLambda :: Parser Node
                  excLambda = LambdaNode
                              <$> (try $ string "lambda" >> requireSpaces >> symbol)
                              <*> (skipSpaces >> parseExpr)

                  excDefine :: Parser Node
                  excDefine = DefineNode
                              <$> (try $ string "define" >> requireSpaces >> symbol)
                              <*> (skipSpaces >> parseExpr)

                  excFuncall :: Parser Node
                  excFuncall = FuncallNode
                               <$> (try $ string "funcall" >> requireSpaces >> parseExpr)
                               <*> (skipSpaces >> parseExpr)

                  excPrint :: Parser Node
                  excPrint = PrintNode
                             <$> (try $ string "print" >> requireSpaces >> parseExpr)

                  excCons :: Parser Node
                  excCons = ConsNode
                            <$> (try $ string "cons" >> requireSpaces >> parseExpr)
                            <*> (requireSpaces >> parseExpr)

                  excCar :: Parser Node
                  excCar = CarNode
                           <$> (try $ string "car" >> requireSpaces >> parseExpr)

                  excCdr :: Parser Node
                  excCdr = CdrNode
                           <$> (try $ string "cdr" >> requireSpaces >> parseExpr)

                  excDo :: Parser Node
                  excDo = DoNode
                          <$> (try $ string "do" >> requireSpaces >> parseExpr)
                          <*> (requireSpaces >> parseExpr)

                  -- nativeはIntのidで指定する
                  excNative :: Parser Node
                  excNative = NativeNode
                              <$> (try $ string "native" >> requireSpaces >> nativeId)
                            where idList = ["1001", "1002"]
                                  accept id = try $ read <$> string id
                                  nativeId = foldl (\x id -> x <|> accept id)
                                                   (accept $ head idList)
                                                   (tail idList) <?> "native id"

                  excEqual :: Parser Node
                  excEqual = EqualNode
                            <$> (try $ string "equal" >> requireSpaces >> parseExpr)
                            <*> (requireSpaces >> parseExpr)


                  parseExpr :: Parser Node
                  parseExpr = prim <?> "a expression"


skipComment :: Parser ()
skipComment = try $ do
            string "----"
            begMark <- many (char '-')
            skip ("----" ++ begMark)
            return ()
            where skip begMark = do
                       skipMany (noneOf "-")
                       endMark <- many1 (char '-')
                       if begMark == endMark
                         then return ()
                         else skip begMark

spaces = oneOf " \t\n" >> return ()
skipCompileTimeExpr = try (string "#" >> compileTimeExprNonSharp >> return ())
skipSpaces = skipMany ( spaces <|> skipComment <|> skipCompileTimeExpr) <?> "skipped spaces"
requireSpaces = eof <|> (skipMany1 (spaces <|> skipComment <|> skipCompileTimeExpr)) <?> "spaces"
