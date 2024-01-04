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

