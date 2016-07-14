{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternGuards #-}
module Icicle.Sorbet.Lexical.Layout (
    layoutProgram

  , LayoutError(..)
  ) where

import           Icicle.Sorbet.Lexical.Syntax
import           Icicle.Sorbet.Position

import           P


data LayoutError =
    UnexpectedEndOfScope !ScopeType !ScopeEnv !(Positioned Token)
    deriving (Eq, Ord, Show)

data ScopeEnv =
  ScopeEnv {
    -- | The token at the start of the current line.
      envLineStart :: !(Positioned Token)

    -- | The bracket type we're currently inside.
    , _envScopes :: ![Scope]
    } deriving (Eq, Ord, Show)

-- | Implicit scoping from layout rules, or explicit scoping from brackets.
data Scope =
    Implicit !Int
  | Explicit !ScopeType
    deriving (Eq, Ord, Show)

-- | The type of an explicit scope.
data ScopeType =
    Brace   -- {..}
  | Paren   -- (..)
  | Bracket -- [..]
  | Context -- let/windowed/etc .. in
  | If      -- if .. then
    deriving (Eq, Ord, Show)

data Line =
    NewLine
  | SameLine
    deriving (Eq, Ord, Show)

layoutProgram :: [Positioned Token] -> Either LayoutError [Positioned Token]
layoutProgram xs =
  case xs of
    [] ->
      pure []

    -- If the first token is 'from' then we assume repl mode, so we don't need
    -- an outer layout block.
    x@(Positioned _ _ Tok_From) : _ ->
      layoutLine (ScopeEnv x []) xs

    -- If the first token is '{' then we assume an extras file with explicit
    -- layout, so we don't need an outer layout block.
    x@(Positioned _ _ Tok_LBrace) : _ ->
      layoutLine (ScopeEnv x []) xs

    -- All other cases mean that we must be processing an extras file with
    -- implicit layout, so we start a layout block for declarations.
    x : _ ->
      before x Tok_LBrace <:> layoutLine (startImplicit x $ ScopeEnv x []) xs

-- Update the start of line token when the line number changes.
layoutLine :: ScopeEnv -> [Positioned Token] -> Either LayoutError [Positioned Token]
layoutLine env xs =
  case xs of
    [] ->
      -- End of file.
      case env of
        -- Close implicit scopes.
        ScopeEnv ls (Implicit _ : ss) ->
          nextLine (before ls Tok_RBrace) <:> layoutLine (ScopeEnv ls ss) []

        -- Ignore other scopes, they'll become a parse error.
        _ ->
          pure []

    x : _ ->
      let
        n = columnOfPositioned x
      in
        case (compareLine x env, env) of
          -- We started a new line and we're inside an implicit scope.
          (NewLine, ScopeEnv ls ss0@(Implicit m : ss))
            -- Token in the same column, insert a semi-colon and continue the scope.
            | n == m
            -> do
              before x Tok_Semi <:> layoutLine (ScopeEnv x ss0) xs

            -- Token to the left, insert a brace and close the scope.
            | n < m
            -> do
              before x Tok_RBrace <:> layoutLine (ScopeEnv ls ss) xs

            -- Token to the right, continue the scope.
            | otherwise
            -> do
              layoutTokens (ScopeEnv x ss0) xs

          -- New line, but we're not inside an implicit scope.
          (NewLine, ScopeEnv _ ss) ->
            layoutTokens (ScopeEnv x ss) xs

          -- Tokens are still on the same line.
          (SameLine, _) ->
            layoutTokens env xs

layoutTokens :: ScopeEnv -> [Positioned Token] -> Either LayoutError [Positioned Token]
layoutTokens env = \case
  --
  -- Layout for 'of' depends on whether the next token is on the same line.
  --
  x@(Positioned _ _ Tok_Of) : y : xs
    | notLBrace y
    , x `onSameLineAs` y
    ->
      --
      -- If the token after 'of' is on the same line, then left edge of the
      -- layout block starts at the beginning of that line. This implies that
      -- the 'of' expression can only have one case.
      --
      --   foo =
      --     |xyz abc of x then
      --     |  x
      --
      --   ==>
      --
      --   foo =
      --     xyz abc of {x then
      --       x
      --   }
      --
      x <:> before y Tok_LBrace <:>
      layoutTokens (startImplicit (envLineStart env) env) (y : xs)

    | notLBrace y
    ->
      --
      -- If the token after 'of' is not on the same line, then the left edge of
      -- the layout block starts at the beginning of that token.
      --
      --   foo =
      --     xyz abc of
      --       |None then
      --       |  0
      --       |Some x then
      --       |  x
      --
      --   ==>
      --
      --   foo =
      --     xyz abc of
      --       {None then
      --          10
      --       ;Some x then
      --          x
      --   }
      --
      x <:> before y Tok_LBrace <:>
      layoutTokens (startImplicit y env) (y : xs)

  --
  -- Layout for 'let' is simpler than 'of', the layout block always starts at
  -- the beginning of the next token.
  --
  --   foo =
  --     let
  --       |xyz =
  --       |  123
  --       |abc =
  --       |  457
  --     in
  --
  --   ==>
  --
  --   foo =
  --     let
  --       {xyz =
  --         123
  --       ;abc =
  --         457
  --     }in
  --
  x@(Positioned _ _ Tok_Let) : y : xs
    | (notLBrace y)
    ->
      x <:> before y Tok_LBrace <:>
      layoutTokens (startImplicit y $ startExplicit Context env) (y : xs)

  --
  -- Open brace / paren / bracket / context / if.
  --
  x@(Positioned _ _ tok) : xs
    | Just styp <- takeOpenScope tok
    ->
      x <:> layoutLine (startExplicit styp env) xs

  --
  -- Close brace / paren / bracket / context.
  --
  -- Note 5 of the Haskell 98/2010 layout rule states that if you encounter a
  -- syntax error, insert a closing brace, then try again.
  --
  -- To implement this, we follow DDC's method and explicitly track the bracket
  -- type that we're inside. If a closing bracket which is not a brace is
  -- found, and our most recent open bracket was a brace, we regard that as the
  -- "syntax error" and insert a closing brace before the non-brace.
  --
  x@(Positioned _ _ tok) : xs
    | Just styp <- takeCloseScope tok
    ->
      case env of
        ScopeEnv ls (Implicit _ : ss) ->
          before x Tok_RBrace <:> layoutLine (ScopeEnv ls ss) (x : xs)

        ScopeEnv ls (Explicit s : ss)
          | styp == s
          ->
            x <:> layoutLine (ScopeEnv ls ss) xs

        _ ->
          Left $ UnexpectedEndOfScope styp env x

  --
  -- Then in guards / of.
  --
  x@(Positioned _ _ Tok_Then) : xs
    | ScopeEnv ls (Explicit If : ss) <- env
    ->
      x <:> layoutLine (ScopeEnv ls ss) xs

    | ScopeEnv ls (Implicit _ : ss) <- env
    , Just If <- nextExplicit ss
    ->
      before x Tok_RBrace <:> layoutLine (ScopeEnv ls ss) (x : xs)

  --
  -- Commas in tuples / guards.
  --
  x@(Positioned _ _ Tok_Comma) : xs
    | ScopeEnv ls (Implicit _ : ss) <- env
    , Just Paren <- nextExplicit ss
    ->
      before x Tok_RBrace <:> layoutLine (ScopeEnv ls ss) (x : xs)

  [] ->
    layoutLine env []

  x : xs ->
    x <:> layoutLine env xs

notLBrace :: Positioned Token -> Bool
notLBrace = \case
  Positioned _ _ Tok_LBrace ->
    False
  _ ->
    True

nextExplicit :: [Scope] -> Maybe ScopeType
nextExplicit = \case
  [] ->
    Nothing
  Implicit _ : ss ->
    nextExplicit ss
  Explicit t : _ ->
    Just t

-- | Returns the scope type if the token is an open brace, paren, bracket or
--   context.
takeOpenScope :: Token -> Maybe ScopeType
takeOpenScope = \case
  Tok_LBrace ->
    Just Brace
  Tok_LParen ->
    Just Paren
  Tok_LBracket ->
    Just Bracket
  Tok_If ->
    Just If
  Tok_From ->
    Just Context
  Tok_Let ->
    Just Context
  Tok_Windowed ->
    Just Context
  Tok_Group ->
    Just Context
  Tok_Distinct ->
    Just Context
  Tok_Filter ->
    Just Context
  Tok_Latest ->
    Just Context
  _ ->
    Nothing

-- Returns the scope type if the token is a close brace, paren, bracket or 'in'.
takeCloseScope :: Token -> Maybe ScopeType
takeCloseScope = \case
  Tok_RBrace ->
    Just Brace
  Tok_RParen ->
    Just Paren
  Tok_RBracket ->
    Just Bracket
  Tok_In ->
    Just Context
  _ ->
    Nothing

compareLine :: Positioned Token -> ScopeEnv -> Line
compareLine x (ScopeEnv ls _) =
  if ls `onSameLineAs` x then
    SameLine
  else
    NewLine

startImplicit :: Positioned a -> ScopeEnv -> ScopeEnv
startImplicit tok (ScopeEnv ls ss) =
  ScopeEnv ls $ Implicit (columnOfPositioned tok) : ss

startExplicit :: ScopeType -> ScopeEnv -> ScopeEnv
startExplicit s (ScopeEnv ls ss) =
  ScopeEnv ls $ Explicit s : ss

columnOfPositioned :: Positioned a -> Int
columnOfPositioned =
  posColumn . posStart

onSameLineAs :: Positioned a -> Positioned b -> Bool
onSameLineAs x y =
  posLine (posEnd x) == posLine (posStart y)

before :: Positioned b -> a -> Positioned a
before (Positioned s _ _) x =
  Positioned s s x

nextLine :: Positioned a -> Positioned a
nextLine (Positioned s e x) =
  Positioned (nextLine' s) (nextLine' e) x

nextLine' :: Position -> Position
nextLine' (Position file line col) =
  Position file (line + 1) col

-- Cons on to a list inside a Functor
(<:>) :: Functor f => a -> f [a] -> f [a]
(<:>) x fs =
  (x :) <$> fs

infixr <:>
