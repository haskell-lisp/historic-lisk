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

liskModule = parens $ do
  loc <- getLoc
  symbolOf "module" <?> "module"
  spaces1
  name <- liskModuleName
  spaces
  importDecls <- liskImportDecl
  spaces
  decls <- sepBy liskDecl spaces
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
  return $ foldl TyApp op' args

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

liskFunBind = FunBind <$> sepBy1 liskMatch spaces1

liskMatch = parens $ do
  loc <- getLoc
  symbolOf "=" <?> "function binding e.g. (= id (x) x)"
  spaces1
  name <- liskName
  spaces1
  pats <- (pure <$> liskPat) <|> parens (sepBy1 liskPat spaces1)
  typ <- return Nothing -- liskType -- TODO
  spaces1
  rhs <- liskRhs
  binds <- liskBinds
  return $ Match loc name pats typ rhs binds

liskBinds = try liskBDecls <|> liskIPBinds

liskBDecls = BDecls <$> pure [] -- TODO

liskIPBinds = IPBinds <$> pure [] -- TODO

liskPat = liskPVar -- TODO

liskRhs = liskUnguardedRhs

liskUnguardedRhs = UnGuardedRhs <$> liskExp

 -- TODO
liskExp = try liskVar
          <|> try liskLit
          <|> try liskApp

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

liskLit = Lit <$> (liskChar <|> try liskString <|> liskInt)

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

liskQual = mzero -- TODO

liskUnQual = UnQual <$> liskName

liskSpecial = Special <$> spec where
    spec = string "()"  *> pure UnitCon
       <|> string "[]"  *> pure ListCon
       <|> string "->"  *> pure FunCon
       <|> string ","   *> pure (TupleCon Boxed{-TODO:boxed-} 0)

liskName = try liskIdent <|> liskSymbol

liskVar = Var <$> liskUnQual

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
