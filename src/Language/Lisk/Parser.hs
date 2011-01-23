{-# LANGUAGE FlexibleContexts,
             NoMonomorphismRestriction,
             ViewPatterns,
             FlexibleInstances #-}
{-# OPTIONS -fno-warn-missing-signatures #-}
module Language.Lisk.Parser where

import Data.Maybe
import Data.List
import Data.Either
import Control.Monad.Reader
import Control.Monad.Error
import Control.Arrow
import Control.Applicative
import Control.Monad.Identity
import Data.Char
import Text.Parsec hiding ((<|>),many,token,optional,spaces)
import Text.Parsec.Combinator hiding (optional)
import Language.Haskell.Exts.Syntax
import Language.Haskell.Exts.Pretty
import qualified Language.Haskell.Exts.Parser as P (parseExp,parse,ParseResult(..))
 
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

liskDecl = try liskTypeInsDecl
         <|> try liskDataDecl
         <|> try liskTypeSig
         <|> try liskFunBind <|> liskPatBind

liskTypeInsDecl = parens $ do
  loc <- getLoc
  string "instance" <?> "type instance e.g. (instance ('show a) (show x \"foo\"))"
  spaces1
  (name,vars) <- parens $ do name <- liskQName
                             spaces1
                             vars <- many1 $ spaces *> liskTyVar
                             return (name,vars)
  decls <- many ((InsDecl <$> (try $ spaces *> liskDecl)) <?> "method declaration e.g. (= x y)")
  return $ InstDecl loc [] name vars decls

liskDataDecl = parens $ do
  loc <- getLoc
  ty <- (<|>) (string "data" <?> "data declaration e.g. (data ('maybe a) ('just a) 'nothing)")
        (string "newtype" <?> "newtype declaration e.g. (newtype 'x 'x 'int)")
  spaces1
  (vars,name) <- ((,) [] <$> liskName) <|> 
                 parens (do name <- liskName
                            vars <- many1 $ spaces *> liskName
                            return (map UnkindedVar vars,name))
  spaces1
  conts <- many $ try $ spaces *> liskQualConDecl
  let dat | ty == "data" = DataType
          | otherwise    = NewType
      context = [] -- TODO: context
      derivin = []
  derivin <- (spaces1 *> liskDerivings) <|> pure []
  return $ DataDecl loc dat context name vars conts derivin

liskDerivings = do
  string ":deriving" <?> "deriving clause e.g. :deriving ('read 'show) or \
                         \:deriving (('monad-state 'io))"
  spaces1
  parens $ many $ try $ spaces *> (liskDeriving <|> liskDerivingNoParams)

liskDerivingNoParams = do
  name <- liskQName
  return $ (name,[])

liskDeriving = parens $ do
  name <- liskQName
  typs <- many $ spaces *> liskType
  return $ (name,typs)

liskQualConDecl = try liskRecDecl <|> liskConDecl

liskConDecl = liskConDeclName <|> liskConDeclParams

liskConDeclParams = parens $ do
  loc <- getLoc
  name <- liskName
  spaces1
  ps <- many $ spaces *> liskType
  return $ QualConDecl loc ([]::[TyVarBind]) ([]::Context) $
              ConDecl name $ map UnBangedTy ps

liskConDeclName = do
  loc <- getLoc
  name <- liskName
  return $ QualConDecl loc ([]::[TyVarBind]) ([]::Context) $
             ConDecl name []

liskRecDecl = parens $ do
  loc <- getLoc
  name <- liskName
  spaces1
  string ":fields"
  spaces1
  fields  <- parens $ many1 $ spaces *> liskRecDeclField
  pure $ QualConDecl loc ([]::[TyVarBind]) ([]::Context) $
          RecDecl name fields

liskRecDeclField = parens $ do
  name <- liskName
  spaces1
  ty <- liskType
  pure ([name],UnBangedTy ty)

liskTypeSig = parens $ do
  loc <- getLoc
  symbolOf "::" <?> "type signature e.g. (:: x 'string)"
  spaces1
  idents <- pure <$> liskIdent <|>
            parens (sepBy1 liskIdent spaces1)
  spaces1
  typ <- liskType
  return $ TypeSig loc idents typ

liskType = try liskTyCon <|> try liskTyVar  <|> 
         try liskForall <|>
         liskTyApp

liskForall = parens $ do
  string "=>"
  spaces1
  context <- pure <$> liskConstraint <|> many1 (try $ spaces *> liskConstraint)
  spaces1
  typ <- liskType
  return $ TyForall Nothing context typ

liskConstraint = parens $ do
  class' <- liskQName
  spaces1
  vars <- sepBy1 liskTyVar spaces1
  return $ ClassA class' vars

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
  pats <- (pure <$> try liskSimplePat) <|> parens (sepBy1 liskPat spaces1)
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
      <|> liskPatTypeSig
      
liskPat = liskPVar
      <|> liskWildCard
      <|> liskPLit
      <|> try liskPatTypeSig
      <|> try liskPTuple
      <|> liskPList
      <|> liskPApp
      -- TODO: There are a lot more.

liskPList = do
  char '['
  els <- many $ try $ spaces *> liskPat
  char ']'
  return $ PList els

liskPatTypeSig = fmap PParen $ parens $ do
  loc <- getLoc
  string "::"
  spaces1
  pat <- liskPat
  spaces1
  typ <- liskType
  return $ PatTypeSig loc pat typ

liskPTuple = parens $ do
  char ','
  args <- many1 $ spaces1 *> liskPat
  return $ PTuple $ args

liskPApp = fmap PParen $ parens $ do
  op <- liskQName -- TODO: Restrict to constructor
  args <- many1 $ spaces1 *> liskPat
  return $ PApp op $ args

liskPLit = PLit <$> liskLit

liskRhs = liskUnguardedRhs

liskUnguardedRhs = UnGuardedRhs <$> liskExp

 -- TODO
liskExp = try liskVar
      <|> Lit <$> try liskLit
      <|> try liskList
      <|> try liskUnit
      <|> try liskLet
      <|> try liskDo
      <|> try liskLambda
      <|> try liskCase
      <|> try liskApp
      <|> Paren <$> parens liskExp

liskList = do
  char '['
  els <- many $ try $ spaces *> liskExp
  char ']'
  return $ List els

liskLet = parens $ do
  loc <- getLoc
  string "let" <?> "let expression e.g. (let ((= x 1)) x)"
  spaces1
  binds <- parens $ liskBinds
  spaces1
  exp <- liskExp
  return $ Let binds exp

liskCase = parens $ do
  loc <- getLoc
  string "case" <?> ("case expression e.g. (case (0 0)), (case :of x (0 0))"
                     ++ ", (case :do get-line (\"foo\" True))")
  value <- optional $ try $ spaces1 *> char ':' *>
             (Left <$> liskCaseOf <|> Right <$> liskCaseDo)
  spaces1
  alts <- sepBy1 (try liskAlt) spaces1
  case value of
    Just e -> case e of
      Left of' -> return $ Case of' alts
      Right do' -> do 
        sym <- genSym
        return $ (Paren (InfixApp do'
                                  (QVarOp (UnQual (Symbol ">>=")))
                                  (Lambda loc [PVar sym]
                                    (Case (Var $ UnQual sym) alts))))
    Nothing -> do 
      sym <- genSym
      return $ Lambda loc [PVar sym]
                 $ Case (Var $ UnQual sym) alts

liskAlt = parens $ do
  loc <- getLoc
  pat <- liskPat
  spaces1
  alts <- liskGuardedAlts
  binds <- pure (BDecls [])
  return $ Alt loc pat alts binds

liskGuardedAlts = try liskGuardUnless <|> try liskGuardedAltsList <|> liskUnGuardedAlt

liskGuardUnless = do
  string ":unless" <?> "unless clause e.g. (xs :unless (null xs) (map (+ 1) xs))"
  spaces1
  alt <- liskGuardedAlt
  let GuardedAlt loc [Qualifier stmts] exp = alt
      alt' = GuardedAlt loc [Qualifier $ App (Var (UnQual (Symbol "not")))
                                             stmts]
                            exp
  return $ GuardedAlts [alt']

liskGuardedAltsList = do
  string ":when" <?> "when clause e.g. (xs :when ((null xs) []))"
  spaces1
  alts <- many1 $ try $ spaces *> parens liskGuardedAlt
  return $ GuardedAlts alts

liskGuardedAlt = do
  loc <- getLoc
  quals <- pure <$> liskQualifier
  spaces1
  exp <- liskExp
  return $ GuardedAlt loc quals exp

liskUnGuardedAlt = UnGuardedAlt <$> liskExp

genSym = do
  loc <- getLoc
  return $ Ident $
    "_lisk_" ++ show (srcLine loc) ++ "_" ++ show (srcColumn loc)

liskCaseOf = do
  string "of"
  spaces1
  liskExp
  
liskCaseDo = do
  string "do"
  spaces1
  liskExp

liskLambda = parens $ do
  loc <- getLoc
  string "fn" <?> "fn expression e.g. (fn (x y) (+ x y))"
  spaces1
  pats <- (try $ pure <$> liskSimplePat) <|> parens (many1 (spaces *> liskPat))
  spaces1
  e <- liskExp
  return $ Lambda loc pats e

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

liskOp = UnQual . Symbol <$> 
         many1 (oneOf ".*-+/\\=<>$#&")

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

liskWildCard = pure (PWildCard) <* char '_'

liskQual = do
  prime <- isJust <$> optional (string "'")
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
       <|> string ":"   *> pure Cons
       <|> string "->"  *> pure FunCon
       <|> do cs <- many1 (char ',') 
              pure (TupleCon Boxed (1 + length cs))

liskName = try liskIdent <|> liskSymbol

liskVar = Var <$> (liskOp <|> liskQName)

liskIdent = Ident . hyphenToCamelCase . colonToConsTyp <$> ident where
    ident = ((++) <$> (string "'" <|> pure "")
                  <*> many1 liskIdentifierToken)

colonToConsTyp ('\'':x:xs) = toUpper x : xs
colonToConsTyp xs = xs

liskSymbol = Symbol <$> many1 liskIdentifierToken

liskImportDecl = parens $ do
  symbolOf "import" <?> "import list e.g. (import prelude data.list (system.char is-upper to-lower))"
  spaces1
  sepBy1 liskImportDeclModule spaces1
  
liskImportDeclModule =
    liskImportDeclModuleName <|> liskImportDeclModuleSpec
    
liskImportDeclModuleSpec = parens $ do
  name <- liskImportDeclModuleName
  qualification <- optional $ spaces1 *> string ":as" *> spaces1 *>
                              liskModuleName
  return name { importQualified = isJust qualification
              , importAs = qualification }
    
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

spaces = do
  (many1 space *> spaces)
  <|> ((string "--" <|> string ";;") *> manyTill anyChar ((newline *> pure ()) <|> eof) *> spaces)
  <|> pure ()

spaces1 = do
 space
 spaces

spaced1 p = (:) <$> p <*> (try (spaces1 *> spaced p) <|> pure [])

spaced p = go where
  go = do x <- Just <$> p <|> pure Nothing
          case x of
            Just x' -> do
              a <- spaces *> go
              pure (x':a)
            Nothing -> pure []
