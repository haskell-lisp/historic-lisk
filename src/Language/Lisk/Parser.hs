{-# LANGUAGE FlexibleContexts,
             NoMonomorphismRestriction,
             ViewPatterns,
             FlexibleInstances #-}
{-# OPTIONS -fno-warn-missing-signatures #-}
module Language.Lisk.Parser where

import Data.List
import Data.Either
import Control.Monad.Reader
import Control.Monad.Error
import Control.Arrow
import Control.Applicative
import Control.Monad.Identity
import Data.Char
import Text.Parsec hiding ((<|>),many,token)
import Text.Parsec.Combinator
import Language.Haskell.Exts.Syntax
import Language.Haskell.Exts.Pretty
import qualified Language.Haskell.Exts.Parser as P (parseExp,parse,ParseResult(..))
 
data LiskExpr = LSym SrcLoc String
              | LTCM SrcLoc String
              | LList SrcLoc [LiskExpr]
  deriving Show

type LP = Parsec String ()

printLiskToHaskell = prettyPrint

parseLisk = parse (spaces *> liskModule <* spaces)

printLiskFragment p = either (putStrLn.show) (putStrLn.prettyPrint) . parse p ""

printLisk str =
  case parse liskModule "" str of
    Left e   -> error $ show e ++ suggest
    Right ex -> putStrLn $ prettyPrint ex

liskModule = do
  loc <- getLoc
  string "("
  symbolOf "module" <?> "module"
  spaces1
  name <- liskModuleName
  docstring <- try (spaces1 *> liskString) <|> return (String "")
  string ")"
  importDecls <- concat <$> many (try $ spaces *> liskImportDecl)
  spaces
  decls <- many $ try $ spaces *> liskDecl
  spaces
  eof
  return $ Module loc name [] Nothing Nothing importDecls decls

  
symbolOf = string

liskDecl = try liskTypeSig <|> try liskFunBind <|> liskPatBind

liskTypeSig = parens $ do
  loc <- getLoc
  symbolOf "::" <?> "type signature e.g. (:: x 'string)"
  spaces1
  idents <- pure <$> liskIdent <|>
            parens (sepBy1 liskIdent spaces1)
  spaces1
  typ <- liskType
  return $ TypeSig loc idents typ

liskType = try liskTyCon <|> try liskTyVar  <|> liskTyApp

liskTyApp = parens $ do
  op <- liskType
  spaces1
  args <- sepBy1 liskType spaces1
  let op' =
        case op of
          TyCon (Special (TupleCon b n)) -> TyCon $ Special $ TupleCon b $ length args
          _ -> op
  case op of
    TyCon o@(Special FunCon) -> 
      return $ TyParen $ foldl1 (flip TyInfix o) args
    _            -> return $ TyParen $ foldl TyApp op' args

liskTyCon = TyCon <$> liskQName

liskTyVar = TyVar <$> liskName

liskPatBind = parens $ do
  loc <- getLoc
  symbolOf "=" <?> "pattern binding e.g. (= hello-world \"Hello, World!\")"
  spaces1
  pat <- liskPat
  typ <- return Nothing -- liskType -- TODO
  spaces1
  rhs <- liskRhs
  binds <- liskBinds
  return $ PatBind loc pat Nothing rhs binds

liskFunBind = FunBind <$> many1 (try $ spaces *> liskMatch)

liskMatch = parens $ do
  loc <- getLoc
  symbolOf "=" <?> "function binding e.g. (= id (x) x)"
  spaces1
  name <- liskName
  spaces1
  pats <- (pure <$> liskSimplePat) <|> parens (sepBy1 liskPat spaces1)
  typ <- return Nothing -- liskType -- TODO
  spaces1
  rhs <- liskRhs
  binds <- liskBinds
  return $ Match loc name pats typ rhs binds

liskBinds = try liskBDecls <|> liskIPBinds

liskBDecls = BDecls <$> many (spaces *> decls) where
  decls = try liskTypeSig <|> try liskFunBind <|> liskPatBind

liskIPBinds = IPBinds <$> pure [] -- TODO

liskSimplePat = liskPVar
      <|> liskPLit
      
liskPat = liskPVar
      <|> liskPLit
      <|> try liskPTuple
      <|> liskPApp
      -- TODO: There are a lot more.

liskPTuple = parens $ do
  char ','
  args <- many1 $ spaces1 *> liskPat
  return $ PTuple $ args

liskPApp = parens $ do
  op <- liskQName -- TODO: Restrict to constructor
  args <- many1 $ spaces1 *> liskPat
  return $ PApp op $ args

liskPLit = PLit <$> liskLit

liskRhs = liskUnguardedRhs

liskUnguardedRhs = UnGuardedRhs <$> liskExp

 -- TODO
liskExp = try liskVar
          <|> Lit <$> try liskLit
          <|> try liskUnit
          <|> try liskDo
          <|> try liskApp
          <|> Paren <$> parens liskExp
          
liskUnit = parens $ return $ Con (Special UnitCon)

liskDo = parens $ do
  string "do" <?> "do expression e.g. (do (<- x y) (return x))"
  spaces1
  stmts <- many $ spaces *> liskStmt
  return $ Do stmts

liskStmt = try liskGenerator <|> 
           try liskLetStmt <|> 
           liskQualifier -- TODO: There are more.

liskQualifier = Qualifier <$> liskExp

liskGenerator = parens $ do
  loc <- getLoc
  string "<-" <?> "do binding e.g. (<- x (return k))"
  spaces1
  pat <- liskPat <?> "binding pattern e.g. (<- ('just x) (return 1))"
  spaces1
  e <- liskExp
  return $ Generator loc pat e

liskLetStmt = parens $ do
  loc <- getLoc
  string "let" <?> "do let e.g. (let (= x 2))"
  spaces1
  binds <- liskBinds
  return $ LetStmt binds

liskApp = try liskTupleApp <|> try liskOpApp <|> try liskIdentApp <|> liskOpPartial

liskTupleApp = parens $ do
  string ","
  args <- (spaces1 *> sepBy1 liskExp spaces1) <|> pure []
  let op = Var $ Special $ TupleCon Boxed $ max 2 (length args)
      paren | null args = id
            | otherwise = Paren
  return $ paren $ foldl App op $ args

liskIdentApp = parens $ do
  op <- liskExp
  spaces1
  args <- sepBy1 liskExp spaces1
  return $ Paren $ foldl App op $ args
  
liskOpApp = parens $ do
  op <- QVarOp <$> liskOp
  spaces1
  args <- (:) <$> (liskExp <* spaces) <*> sepBy1 liskExp spaces1
  return $ Paren $ foldl1 (flip InfixApp op) args

liskOpPartial = parens $ do
  op <- Var <$> liskOp
  spaces1
  e <- liskExp
  return $ App op e

liskOp = UnQual . Symbol <$> many1 (oneOf ".*-+/\\=<>")

liskLit = liskChar <|> try liskString <|> liskInt

liskChar = Char <$> (string "\\" *> (space <|> newline <|> noneOf "\n \t"))
    where space = const ' ' <$> string "Space"
                  <|> const '\n' <$> string "Newline"

liskString = do
  strRep <- char '\"' *> (concat <$> many liskStringSeq) <* char '\"'
  case P.parseExp $ "\"" ++ strRep ++ "\"" of
    P.ParseOk (Lit s@String{}) -> return s
    P.ParseFailed _ msg -> parserFail msg
  where liskStringSeq = ("\\"++) <$> (char '\\' *> (pure <$> noneOf "\n"))
                        <|> pure <$> noneOf "\n\""

liskInt = Int <$> (read <$> many1 digit)

liskPVar = PVar <$> liskName

liskQName = try liskSpecial <|> try liskQual <|> try liskUnQual

liskQual = do
  prime <- (const True <$> (lookAhead (string "'") *> string "'"))
          <|> pure False
  word <- liskModuleName <?> "module name e.g. data.char"
  let (ModuleName word') = word
      (name,mod) = (downFirst . reverse *** reverse . drop 1) $
                   span (/='.') $ reverse word'
      downFirst (x:xs) | not prime = toLower x : xs
      downFirst xs                 = xs
  return $ if null mod 
     then UnQual (Ident name)
     else Qual (ModuleName mod) (Ident name)

liskUnQual = UnQual <$> liskName

liskSpecial = Special <$> spec where
    spec = string "()"  *> pure UnitCon
       <|> string "[]"  *> pure ListCon
       <|> string "->"  *> pure FunCon
       <|> string ","   *> pure (TupleCon Boxed{-TODO:boxed-} 0)

liskName = try liskIdent <|> liskSymbol

liskVar = Var <$> liskQName

liskIdent = Ident . hyphenToCamelCase . colonToConsTyp <$> ident where
    ident = ((++) <$> (string "'" <|> pure "")
                  <*> many1 liskIdentifierToken)

colonToConsTyp ('\'':x:xs) = toUpper x : xs
colonToConsTyp xs = xs

liskSymbol = Symbol <$> many1 liskIdentifierToken

liskList = mzero -- TODO

liskImportDecl = parens $ do
  symbolOf "import" <?> "import list e.g. (import prelude data.list (system.char is-upper to-lower))"
  spaces1
  sepBy1 liskImportDeclModule spaces1
  
liskImportDeclModule =
    liskImportDeclModuleName <|> liskImportDeclModuleSpec
    
liskImportDeclModuleSpec = parens $ do
  imp <- liskImportDeclModuleSpec
  return imp
    
liskImportDeclModuleName = do
  loc <- getLoc
  name <- liskModuleName
  return $ ImportDecl { 
      importLoc = loc
    , importModule = name
    , importQualified = False
    , importSrc = False
    , importPkg = Nothing
    , importAs = Nothing
    , importSpecs = Nothing
    }

liskModuleName = (<?> "module name (e.g. `module.some-name')") $ do
  parts <- sepBy1 modulePart (string ".")
  return $ ModuleName $ intercalate "." parts
  where modulePart = format <$> many1 liskIdentifierToken
        format = hyphenToCamelCase . upperize
        upperize (x:xs) = toUpper x : xs

liskDefIdentifier = do
  ident <- many1 liskIdentifierToken
  return $ Ident ident

liskIdentifierToken = letter <|> digit <|> oneOf "-"

hyphenToCamelCase ('-':'-':xs) = hyphenToCamelCase ('-':xs)
hyphenToCamelCase ('-':x:xs)   = toUpper x : hyphenToCamelCase xs
hyphenToCamelCase ('-':xs)     = hyphenToCamelCase xs
hyphenToCamelCase (x:xs)       = x : hyphenToCamelCase xs
hyphenToCamelCase []           = []

getLoc = posToLoc <$> getPosition where
  posToLoc pos =
    SrcLoc { srcFilename = sourceName pos 
           , srcLine     = sourceLine pos
           , srcColumn   = sourceColumn pos
           }

parens = between (char '(') (char ')')

suggest = "\n(are you trying to use not-currently-supported syntax?)"

bi f g = f . g . f

spaces1 = many1 space
