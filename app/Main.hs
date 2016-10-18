module Main where

import Language.Haskell.Exts.Annotated (
  fromParseResult, parseModuleWithMode, defaultParseMode,
  parseFilename)
import Language.Haskell.Exts (
  Name(Ident, Symbol), ModuleName(ModuleName))
import Language.Haskell.Names (
  loadBase, annotate, symbolName,
  Scoped(Scoped), NameInfo(GlobalSymbol),
  Symbol(Method, Value), symbolModule, className)

import qualified Data.Map as Map (
  lookup)

import Data.Maybe (
  fromMaybe, listToMaybe,
  fromJust, isJust)
import Data.List (
  group)
import qualified Data.Foldable as Foldable (
  toList)
import Control.Monad (
  guard)

type Fn = (String, String)

isFunction xs = case head(xs) of
  Scoped (GlobalSymbol (Value {symbolModule = ModuleName modname,
    symbolName = Ident fname}) _) _ -> Just (modname, fname)
  Scoped (GlobalSymbol (Value {symbolModule = ModuleName modname,
    symbolName = Symbol fname}) _) _ -> Just (modname, fname)
  Scoped (GlobalSymbol (Method {symbolModule = ModuleName modname,
    symbolName = Symbol fname, className = _}) _) _ -> Just (modname, fname)
  otherwise -> Nothing

main :: IO ()
main = do
  -- read the program's source from stdin
  source <- getContents

  -- parse the program (using haskell-src-exts)
  let ast = fromParseResult (
        parseModuleWithMode defaultParseMode {parseFilename="stdin"} source)

  -- get base environment
  baseEnvironment <- loadBase

  -- get symbols defined in prelude
  let preludeSymbols = fromMaybe (error "Prelude not found") (
        Map.lookup (ModuleName "Prelude") baseEnvironment)

  -- annotate the AST
  let annotatedAST = annotate baseEnvironment ast

  ---- get all annotations
  let annotations = Foldable.toList annotatedAST
  let functions = map fromJust $ filter isJust $ map isFunction
                                               $ group annotations
  mapM_ print functions

