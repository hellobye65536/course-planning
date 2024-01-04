{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module CoursePlanning.Course where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Text.Read (Lexeme (Ident), Read (readPrec), lift, parens, prec, step)
import Text.Read.Lex (expect)

data Season = Fall | Winter | Summer
  deriving (Eq, Show, Read)

data CourseName = CourseName
  { courseSubject :: Text
  , courseCode :: Text
  }
  deriving (Eq)

instance Show CourseName where
  showsPrec prec (CourseName subj code) =
    showParen (prec > 10)
      $ showString "CourseName "
      . showsPrec 11 subj
      . showString " "
      . showsPrec 11 code

instance Read CourseName where
  readPrec = parens $ prec 10 $ do
    lift (expect $ Ident "CourseName")
    CourseName <$> step readPrec <*> step readPrec

data Course = Course
  { courseName :: CourseName
  , courseId :: Text
  , courseComponents :: [Text]
  , courseUnits :: Float
  , courseTitle :: Text
  , courseDescription :: Text
  , courseNote :: Text
  , courseOffered :: [Season]
  , courseReq :: Req
  , courseCrosslist :: [CourseName]
  }
  deriving (Show, Read)

data Req
  = ReqCourse CourseName (Maybe Float)
  | ReqCoCourse CourseName
  | ReqNot Req
  | ReqAnd [Req]
  | ReqOr [Req]
  | ReqNote Text
  deriving (Show, Read)

pattern ReqTrue, ReqFalse :: Req
pattern ReqTrue = ReqAnd []
pattern ReqFalse = ReqOr []

pattern ReqCourse' :: Text -> Text -> Maybe Float -> Req
pattern ReqCourse' subj code grade = ReqCourse (CourseName subj code) grade

pattern ReqCoCourse' :: Text -> Text -> Req
pattern ReqCoCourse' subj code = ReqCoCourse (CourseName subj code)

simplifyReq :: Req -> Req
simplifyReq r = case r of
  ReqCourse _ _ -> r
  ReqCoCourse _ -> r
  ReqNot (ReqNot r) -> simplifyReq r
  ReqNot ReqTrue -> ReqFalse
  ReqNot ReqFalse -> ReqTrue
  ReqNot r -> ReqNot $ simplifyReq r
  ReqAnd rs -> desingleton ReqAnd $ fromMaybe [] $ foldr stepAnd (Just []) rs
  ReqOr rs -> desingleton ReqOr $ fromMaybe [] $ foldr stepOr (Just []) rs
  ReqNote _ -> r
 where
  desingleton _ [r] = r
  desingleton f rs = f rs
  stepAnd r acc = case simplifyReq r of
    ReqTrue -> acc
    ReqFalse -> Nothing
    ReqAnd rs -> (rs ++) <$> acc
    r -> (r :) <$> acc
  stepOr r acc = case simplifyReq r of
    ReqTrue -> Nothing
    ReqFalse -> acc
    ReqOr rs -> (rs ++) <$> acc
    r -> (r :) <$> acc
