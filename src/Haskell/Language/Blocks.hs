-- |
-- Module:     Haskell.Language.Blocks
-- Copyright:  (c) Sergey Vinokurov 2025
-- License:    Apache-2.0 (see LICENSE)
-- Maintainer: serg.foo@gmail.com

module Haskell.Language.Blocks
  ( breakBlocks
  , DirectivesMode(..)
  , filterBlank
  ) where

import Data.List.NonEmpty (NonEmpty(..))

import Haskell.Language.Lexer.Types

data DirectivesMode = KeepDirectives | StripDirectives

-- | Break the input up into blocks based on indentation.
breakBlocks :: ProcessMode -> DirectivesMode -> [Pos ServerToken] -> [NonEmpty (Pos ServerToken)]
breakBlocks mode dirMode
  = go
  . stripSemicolonsNotInBraces
  . (case mode of
      ProcessVanilla   -> id
      ProcessAlexHappy -> uncurry (++) . firstLastBracedBlock)
  . (case dirMode of
      KeepDirectives  -> id
      StripDirectives -> stripToplevelHscDirectives)
  . filterBlank
  where
    go :: [Pos ServerToken] -> [NonEmpty (Pos ServerToken)]
    go []     = []
    go tokens = case pre of
      []     -> go post
      x : xs -> (x :| xs) : go post
      where
        (pre, post) = breakBlock tokens

-- Blank lines mess up the indentation.
filterBlank :: [Pos ServerToken] -> [Pos ServerToken]
filterBlank = \case
  []                                             -> []
  Pos _ (Newline _) : xs@(Pos _ (Newline _) : _) -> filterBlank xs
  x : xs                                         -> x : filterBlank xs

-- | Collect tokens between toplevel braces. Motivated by Alex/Happy
-- file format that uses braced blocks to separate Haskell source from
-- other directives.
firstLastBracedBlock :: [Pos ServerToken] -> ([Pos ServerToken], [Pos ServerToken])
firstLastBracedBlock tokens =
  (first, last')
  where
    (first, rest) = forward 0 [] tokens
    last'         = backward 0 [] $ reverse rest
    forward :: Int -> [Pos ServerToken] -> [Pos ServerToken] -> ([Pos ServerToken], [Pos ServerToken])
    forward !_ acc []                       = (reverse acc, [])
    forward  0 acc (Pos _ LBrace      : ts) = forward 1 acc ts
    forward  0 acc (_                 : ts) = forward 0 acc ts
    forward  1 acc (Pos _ RBrace      : ts) = (reverse acc, ts)
    forward  n acc (t@(Pos _ LBrace)  : ts) = forward (n + 1) (t : acc) ts
    forward  n acc (t@(Pos _ HSCEnum) : ts) = forward (n + 1) (t : acc) ts
    forward  n acc (t@(Pos _ RBrace)  : ts) = forward (n - 1) (t : acc) ts
    forward  n acc (t                 : ts) = forward n (t : acc) ts

    backward :: Int -> [Pos ServerToken] -> [Pos ServerToken] -> [Pos ServerToken]
    backward !_ acc []                       = acc
    backward  0 acc (Pos _ RBrace      : ts) = backward 1 acc ts
    backward  0 acc (_                 : ts) = backward 0 acc ts
    backward  1 acc (Pos _ LBrace      : _)  = acc
    backward  n acc (t@(Pos _ LBrace)  : ts) = backward (n - 1) (t : acc) ts
    backward  n acc (t@(Pos _ HSCEnum) : ts) = backward (n - 1) (t : acc) ts
    backward  n acc (t@(Pos _ RBrace)  : ts) = backward (n + 1) (t : acc) ts
    backward  n acc (t                 : ts) = backward n (t : acc) ts

-- | Take until a newline, then take lines until the indent established after
-- that newline decreases. Or, alternatively, if "{" is encountered then count
-- it as a block until closing "}" is found taking nesting into account.
breakBlock :: [Pos ServerToken] -> ([Pos ServerToken], [Pos ServerToken])
breakBlock = go []
  where
    go :: [Pos ServerToken] -> [Pos ServerToken] -> ([Pos ServerToken], [Pos ServerToken])
    go acc [] =
      (reverse acc, [])
    go acc (Pos _ Newline{} : t@(Pos _ KWModule) : ts) =
      (reverse acc ++ t : importList, drop 1 rest)
      where
        (importList, rest) = span ((/= KWWhere) . valOf) ts
    go acc (t@(Pos _ tok) : ts) =
      case tok of
        Newline indent -> collectIndented acc indent ts
        LBrace         -> collectBracedBlock (t : acc) go ts 1
        HSCEnum        -> collectBracedBlock (t : acc) go ts 1
        _              -> go (t : acc) ts

    collectIndented :: [Pos ServerToken] -> Int -> [Pos ServerToken] -> ([Pos ServerToken], [Pos ServerToken])
    collectIndented acc indent = goIndented acc
      where
        goIndented acc' toks = case toks of
          Pos _ Newline{} : Pos _ KWModule : _ ->
            (reverse acc', toks)

          []                -> (reverse acc', [])

          t : []            -> case t of
            Pos _ (Newline n)
              | n <= indent
              -> (reverse acc', toks)
            _ -> (reverse $ t : acc', [])
          t : ts@(t2 : ts') -> case t of
            Pos _ (Newline n)
              | Pos _ CppDefine{} <- t2
              -> goIndented (t2 : t : acc') ts'
              | n <= indent
              -> (reverse acc', toks)
            Pos _ LBrace ->
              collectBracedBlock (t : acc') goIndented ts 1
            _ ->
              goIndented (t : acc') ts

    collectBracedBlock
        :: Show b
        => [Pos ServerToken]
        -> ([Pos ServerToken] -> [Pos ServerToken] -> ([Pos ServerToken], [b]))
        -> [Pos ServerToken]
        -> Int
        -> ([Pos ServerToken], [b])
    collectBracedBlock acc cont = goBraced acc
      where
        goBraced acc' []       _ = (reverse acc', [])
        goBraced acc' ts       0 = cont acc' ts
        goBraced acc' (t : ts) n = goBraced (t : acc') ts $! case t of
          Pos _ LBrace -> n + 1
          Pos _ RBrace -> n - 1
          _            -> n

stripSemicolonsNotInBraces :: [Pos ServerToken] -> [Pos ServerToken]
stripSemicolonsNotInBraces =
  go False 0 0
  where
    go  :: Bool -- Whether inside let or where block or case expression
        -> Int -- Indent of last newline
        -> Int -- Parenthesis nesting depth
        -> [Pos ServerToken]
        -> [Pos ServerToken]
    go !_     !_ !_ []                                                       = []
    go  b      k  n (tok@(Pos _ KWWhere)     : tok'@(Pos _ LBrace) : ts)     = tok : tok' : skipBalancedParens b k (inc n) ts
    go  _      k  n (tok@(Pos _ KWWhere)     : ts)                           = tok : go True k n ts
    go  b      k  n (tok@(Pos _ KWLet)       : tok'@(Pos _ LBrace) : ts)     = tok : tok' : skipBalancedParens b k (inc n) ts
    go  _      k  n (tok@(Pos _ KWLet)       : ts)                           = tok : go True k n ts
    go  b      k  n (tok@(Pos _ KWDo)        : tok'@(Pos _ LBrace) : ts)     = tok : tok' : skipBalancedParens b k (inc n) ts
    go  _      k  n (tok@(Pos _ KWDo)        : ts)                           = tok : go True k n ts
    go  b      k  n (tok@(Pos _ KWOf)        : tok'@(Pos _ LBrace) : ts)     = tok : tok' : skipBalancedParens b k (inc n) ts
    go  _      k  n (tok@(Pos _ KWOf)        : ts)                           = tok : go True k n ts
    go  _      k  n (tok@(Pos _ KWIn)        : ts)                           = tok : go False k n ts
    go  _      _  n (tok@(Pos _ (Newline k)) : ts)                           = tok : go False k n ts
    go  _      _  0 (     Pos _ Semicolon    : tok@(Pos _ (Newline k)) : ts) = tok : go False k 0 ts
    go  False  k  0 (     Pos p Semicolon    : ts)                           = Pos p (Newline k) : go False k 0 ts
    go  b      k  n (tok@(Pos _ LParen)      : ts)                           = tok : skipBalancedParens b k (inc n) ts
    go  b      k  n (tok@(Pos _ SpliceStart) : ts)                           = tok : skipBalancedParens b k (inc n) ts
    go  b      k  n (tok@(Pos _ LBracket)    : ts)                           = tok : skipBalancedParens b k (inc n) ts
    go  b      k  n (tok@(Pos _ LBrace)      : ts)                           = tok : skipBalancedParens b k (inc n) ts
    go  b      k  n (tok@(Pos _ LBanana)     : ts)                           = tok : skipBalancedParens b k (inc n) ts
    go  b      k  n (tok@(Pos _ RParen)      : ts)                           = tok : go b k (dec n) ts
    go  b      k  n (tok@(Pos _ RBracket)    : ts)                           = tok : go b k (dec n) ts
    go  b      k  n (tok@(Pos _ RBrace)      : ts)                           = tok : go b k (dec n) ts
    go  b      k  n (tok@(Pos _ RBanana)     : ts)                           = tok : go b k (dec n) ts
    go  b      k  n (tok : ts)                                               = tok : go b k n       ts

    skipBalancedParens
        :: Bool -- Whether inside where block or after equals sign
        -> Int -- Indent of last newline
        -> Int -- Parenthesis nesting depth
        -> [Pos ServerToken]
        -> [Pos ServerToken]
    skipBalancedParens b k = skip
      where
        skip :: Int -> [Pos ServerToken] -> [Pos ServerToken]
        skip _ []                          = []
        skip 0 ts                          = go b k 0 ts
        skip n (tok@(Pos _ LParen)      : ts) = tok : skip (inc n) ts
        skip n (tok@(Pos _ SpliceStart) : ts) = tok : skip (inc n) ts
        skip n (tok@(Pos _ LBracket)    : ts) = tok : skip (inc n) ts
        skip n (tok@(Pos _ LBrace)      : ts) = tok : skip (inc n) ts
        skip n (tok@(Pos _ LBanana)     : ts) = tok : skip (inc n) ts
        skip n (tok@(Pos _ RParen)      : ts) = tok : skip (dec n) ts
        skip n (tok@(Pos _ RBracket)    : ts) = tok : skip (dec n) ts
        skip n (tok@(Pos _ RBrace)      : ts) = tok : skip (dec n) ts
        skip n (tok@(Pos _ RBanana)     : ts) = tok : skip (dec n) ts
        skip n (tok : ts)                     = tok : skip n ts

    inc :: Int -> Int
    inc n = n + 1
    dec :: Int -> Int
    dec n = max 0 (n - 1)

stripToplevelHscDirectives :: [Pos ServerToken] -> [Pos ServerToken]
stripToplevelHscDirectives = scan
  where
    scan :: [Pos ServerToken] -> [Pos ServerToken]
    scan = \case
      []                            -> []
      Pos _ HSCDirectiveBraced : ts -> skip 1 ts
      t : ts                        -> t : scan ts

    skip :: Int -> [Pos ServerToken] -> [Pos ServerToken]
    skip !_ []                              = []
    skip  0 ts                              = scan ts
    skip  n (Pos _ HSCDirectiveBraced : ts) = skip (n + 1) ts
    skip  n (Pos _ LBrace       : ts)       = skip (n + 1) ts
    skip  n (Pos _ RBrace       : ts)       = skip (n - 1) ts
    skip  n (_                  : ts)       = skip n ts

