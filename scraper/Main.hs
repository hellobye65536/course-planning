{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Main where

import Prelude hiding (lex)

import Control.Monad.Trans.State (State, get, put, runState)
import qualified Data.ByteString as B
import Data.Char (isAlpha, isAlphaNum, isAsciiUpper, isDigit, isSpace)
import Data.Foldable (asum)
import Data.Functor (($>))
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Text.HTML.Scalpel.Core (
  ScraperT,
  Selector,
  TagName (AnyTag),
  chroots,
  scrapeStringLike,
  texts,
  (@:),
  (@=),
 )
import Text.Pretty.Simple (pPrint)

import CoursePlanning.Course

classesSelector :: Selector
classesSelector = AnyTag @: ["class" @= "divTable"]

courseScraper :: (Monad m) => ScraperT Text m Course
courseScraper = do
  subs <- map T.strip <$> texts "div"
  let
    (subj, code, comp, units) = case T.splitOn " " (subs !! 1) of
      [subj, code, comp, units] -> (subj, code, comp, units)
      _ -> error "failed to find course info"
    cid = case T.stripPrefix "Course ID: " (subs !! 2) of
      Just cid -> cid
      _ -> error "failed to find course id"
    course =
      scrapeRest
        (drop 5 subs)
        Course
          { courseName = CourseName subj code
          , courseId = cid
          , courseComponents = T.splitOn "," comp
          , courseUnits = read $ T.unpack units
          , courseTitle = subs !! 3
          , courseDescription = subs !! 4
          , courseNote = ""
          , courseOffered = []
          , courseReq = ReqTrue
          , courseCrosslist = []
          }
    course' = course{courseReq = simplifyReq $ courseReq course}
  pure course'
 where
  scrapeRest :: [Text] -> Course -> Course
  scrapeRest [] course = course
  scrapeRest (sub : subs) course
    | T.all isSpace sub =
        scrapeRest subs course
    | "[Note: " `T.isPrefixOf` sub =
        scrapeRest subs course{courseNote = sub}
    | Just prereqs <- T.stripPrefix "Prereq:" sub =
        scrapeRest subs course{courseReq = ReqAnd [courseReq course, parse parseCtxPrereq $ lex prereqs]}
    | Just coreqs <- T.stripPrefix "Coreq:" sub =
        scrapeRest subs course{courseReq = ReqAnd [courseReq course, parse parseCtxCoreq $ lex coreqs]}
    | Just antireqs <- T.stripPrefix "Antireq:" sub =
        scrapeRest subs course{courseReq = ReqAnd [courseReq course, ReqNot (parse parseCtxAntireq $ lex antireqs)]}
    | Just crosslisted <- T.stripPrefix "(Cross-listed with " sub =
        scrapeRest subs course{courseCrosslist = parseCrosslist $ T.init crosslisted}
    | Just _online <- T.stripSuffix " offered Online" sub =
        scrapeRest subs course
    | Just _consent <- T.stripSuffix " Consent Required" sub =
        scrapeRest subs course
    | Just _offered <- T.stripPrefix "Offered at " sub =
        scrapeRest subs course
    | Just _offered <- T.stripPrefix "Also offered at " sub =
        scrapeRest subs course
    | otherwise =
        error $ "unknown text: " ++ show sub

parseCrosslist :: Text -> [CourseName]
parseCrosslist = map (go . T.splitOn " ") . T.splitOn ", "
 where
  go [subj, code] = CourseName subj code
  go _ = error "bad course in crosslist"

data LexToken
  = LexTokenText Text
  | LexTokenSubj Text
  | LexTokenCode Text
  | LexTokenOr
  | LexTokenAnd
  | LexTokenOneOf
  | LexTokenGrade Float
  | LexTokenSep
  | LexTokenBigSep
  | LexTokenSlash
  | LexTokenGroup [LexToken]
  deriving (Show)

tokenKeyword :: [(Text, LexToken)]
tokenKeyword =
  [ ("or", LexTokenOr)
  , ("and", LexTokenAnd)
  , ("&", LexTokenAnd)
  , ("one", LexTokenOneOf)
  , ("either", LexTokenOneOf)
  , ("/", LexTokenSlash)
  , (",", LexTokenSep)
  , ("taken", LexTokenText "taken")
  , ("fall", LexTokenText "fall")
  , ("spring", LexTokenText "spring")
  , ("summer", LexTokenText "summer")
  , ("winter", LexTokenText "winter")
  ]

tokenIgnore :: [Text]
tokenIgnore =
  [ "a"
  , "at"
  , "grade"
  , "higher"
  , "in"
  , "least"
  , "minimum"
  , "of"
  , "with"
  ]

tokenSpecialText :: [Text]
tokenSpecialText =
  [
  ]

tokenCode :: Text -> Bool
tokenCode t =
  (2 <= T.length t)
    && (T.length t <= 4)
    && T.all isDigit (T.init t)
    -- eliminate term codes, like "1A", and high school codes, like "4U"
    && not
      ( (T.length t == 2)
          && isAlpha (T.last t)
      )
    -- eliminate percentage grades
    && (T.last t /= '%')

tokenSubj :: Text -> Bool
tokenSubj t =
  (2 <= T.length t)
    && (T.length t < 10)
    && T.all isAsciiUpper t

tokenSubjCode :: Text -> Maybe (Text, Text)
tokenSubjCode t =
  let (subj, code) = T.span isAsciiUpper t
   in if tokenSubj subj && tokenCode code
        then Just (subj, code)
        else Nothing

tokenGrade :: Text -> Bool
tokenGrade t =
  T.all isDigit (T.init t)
    && (T.last t == '%')

tokenTermSeason :: Text -> Bool
tokenTermSeason (a :> b :> c :> "") = isAlpha a && isDigit b && isDigit c
tokenTermSeason _ = False

tokenTerm :: Text -> Bool
tokenTerm (a :> b :> "") =
  a
    `elem` ("1234" :: String)
    && b
    `elem` ("AB" :: String)
tokenTerm _ = False

pattern (:>) :: Char -> Text -> Text
pattern c :> t <- (T.uncons -> Just (c, t))
infixr 5 :>

data LexRaw
  = LexRawParen [LexToken]
  | LexRawText Text
  | LexRawPureText Text
  | LexRawSubj Text
  | LexRawCode Text
  deriving (Show)

lexSub :: State Text a -> State Text (a, Text)
lexSub s = do
  t <- get
  ret <- s
  t' <- get
  pure (ret, T.take (T.length t - T.length t') t)

lex :: Text -> [LexToken]
lex t = case runState lexChunks t of
  (ts, "") -> ts
  (_, t) -> error $ "leftover text: " ++ show t

lexChunks :: State Text [LexToken]
lexChunks = (get >>=) $ \case
  c :> t'
    | isSpace c -> put (T.dropWhile isSpace t') *> lexChunks
    | ')' <- c -> pure []
    | ';' <- c -> put t' *> fmap (LexTokenBigSep :) lexChunks
    | '.' <- c -> put t' *> fmap (LexTokenBigSep :) lexChunks
    | otherwise -> do
        (rs, used) <- lexSub lexChunk
        let ts =
              if not (any lexRawNonText rs)
                then [LexTokenText $ T.strip used]
                else lexResolve rs
        ts' <- lexChunks
        pure $ ts ++ ts'
   where
    lexRawNonText (LexRawCode _) = True
    lexRawNonText (LexRawParen ts) = any (\case LexTokenText _ -> False; _ -> True) ts
    lexRawNonText _ = False
  _ -> pure []

lexResolve :: [LexRaw] -> [LexToken]
lexResolve [] = []
lexResolve (LexRawParen ts : rs) = LexTokenGroup ts : lexResolve rs
lexResolve (LexRawSubj subj : rs) = LexTokenSubj subj : lexResolve rs
lexResolve (LexRawCode code : rs) = LexTokenCode code : lexResolve rs
lexResolve (LexRawPureText text : rs) = LexTokenText text : lexResolve rs
lexResolve
  ( LexRawText (T.toLower -> "level")
      : LexRawText (T.toLower -> "at")
      : LexRawText (T.toLower -> "least")
      : LexRawText level@(tokenTerm -> True)
      : rs
    )
    | Just rest <- T.intercalate " " <$> traverse detext rs = [LexTokenText $ "Level at least " <> level <> " " <> rest]
   where
    detext (LexRawParen _) = Nothing
    detext (LexRawText t) = Just t
    detext (LexRawPureText t) = Just t
    detext (LexRawSubj t) = Just t
    detext (LexRawCode t) = Just t
lexResolve (LexRawText t : rs)
  | Just tk <- lookup tl tokenKeyword = tk : lexResolve rs
  | t `elem` tokenSpecialText = LexTokenText t : lexResolve rs
  | tl `elem` tokenIgnore = lexResolve rs
  | tokenGrade t = LexTokenGrade (read $ T.unpack $ T.init t) : lexResolve rs
  | tokenTermSeason t = LexTokenText t : lexResolve rs
  | otherwise = error $ "unexpected token: " ++ show t
 where
  tl = T.toLower t

lexChunk :: State Text [LexRaw]
lexChunk = (get >>=) $ \case
  t@(c :> t')
    | isSpace c -> put (T.dropWhile isSpace t') *> lexChunk
    | Just (r, t') <- asum $ map (\p -> (p,) <$> T.stripPrefix p t) tokenSpecialText -> do
        put t'
        rs <- lexChunk
        pure (LexRawPureText r : rs)
    | '(' <- c -> do
        put t'
        ts <- lexChunks
        get >>= \case
          ')' :> t' ->
            put t' *> fmap (LexRawParen ts :) lexChunk
          t -> error $ "expected ')', got: " ++ show t
    | ')' <- c -> pure []
    | ';' <- c -> pure []
    | '.' <- c -> pure []
    | '"' <- c -> do
      let (r, t') = T.break (/='"') t'
      put $ T.tail t'
      rs <- lexChunk
      pure $ (LexRawPureText r : rs)
    | isText c -> do
        let (r, t') = munchText $ liftA2 zip T.inits T.tails t
        put t'
        rs' <- lexChunk
        let rs =
              if
                | tokenSubj r -> [LexRawSubj r]
                | tokenCode r -> [LexRawCode r]
                | Just (subj, code) <- tokenSubjCode r ->
                    [LexRawSubj subj, LexRawCode code]
                | otherwise -> [LexRawText r]
        pure $ rs ++ rs'
    | otherwise ->
        put t' *> fmap (LexRawText (T.singleton c) :) lexChunk
   where
    isText c = isAlphaNum c || c == '%' || c == ':' || c == '-'
    munchText ((_, a :> '.' :> b :> _) : _ : _ : z')
      | isDigit a && isDigit b = munchText z'
    munchText (p : z')
      | (_, c :> _) <- p
      , isText c =
          munchText z'
      | otherwise = p
    munchText [] = error "unreachable"
  _ -> pure []

type CourseReqBuilder = Text -> Text -> Maybe Float -> Req

data ParseCtx = ParseCtx
  { parseCtxCRB :: CourseReqBuilder
  , parseCtxGrouper :: [Req] -> Req
  , parseCtxGrade :: Maybe Float
  }

data ParseState = ParseState
  { parseStateTk :: [LexToken]
  , parseStateSubj :: Maybe Text
  }

type ParseM = State ParseState

getTk :: ParseM [LexToken]
getTk = parseStateTk <$> get

putTk :: [LexToken] -> ParseM ()
putTk tk = get >>= (\ps -> put ps{parseStateTk = tk})

getSubj :: ParseM Text
getSubj = fromMaybe (error "missing subject") . parseStateSubj <$> get

putSubj :: Text -> ParseM ()
putSubj subj = get >>= (\ps -> put ps{parseStateSubj = Just subj})

parseCtxDefault, parseCtxPrereq, parseCtxCoreq, parseCtxAntireq :: ParseCtx
parseCtxDefault =
  ParseCtx
    { parseCtxCRB = \_ _ _ -> error "undefined parseCtxCRB"
    , parseCtxGrouper = ReqAnd
    , parseCtxGrade = Nothing
    }
parseCtxPrereq = parseCtxDefault{parseCtxCRB = ReqCourse'}
parseCtxCoreq = parseCtxDefault{parseCtxCRB = coreqCRB}
parseCtxAntireq = parseCtxDefault{parseCtxCRB = coreqCRB, parseCtxGrouper = ReqOr}

parseCtxGrouperForce :: ParseCtx -> [Req] -> Req
parseCtxGrouperForce = parseCtxGrouper

coreqCRB :: CourseReqBuilder
coreqCRB subj code Nothing = ReqCoCourse' subj code
coreqCRB _ _ (Just _) = error "unexpected grade in corequisite req"

parse :: ParseCtx -> [LexToken] -> Req
parse ctx ts = case parseStateTk <$> runState (parseOuter ctx) (ParseState ts Nothing) of
  (r, []) -> r
  (_, ts) -> error $ "unparsed tokens: " ++ show ts

parseFin :: ParseCtx -> [Req] -> Req
parseFin ctx = \case
  [] -> ReqTrue
  [r] -> r
  rs -> parseCtxGrouperForce ctx $ reverse rs

parseOuter :: ParseCtx -> ParseM Req
parseOuter ctx = go ctx []
 where
  go :: ParseCtx -> [Req] -> ParseM Req
  go ctx acc = (getTk >>=) $ \case
    [] -> pure $ parseFin ctx acc
    (LexTokenBigSep : ts) -> putTk ts *> go ctx acc
    (LexTokenAnd : ts) ->
      putTk ts *> go ctx{parseCtxGrouper = ReqAnd} acc
    (LexTokenOr : ts) ->
      putTk ts *> go ctx{parseCtxGrouper = ReqOr} acc
    _ -> parseInner ctx >>= (go ctx . (: acc))

parseInner :: ParseCtx -> ParseM Req
parseInner ctx = go ctx []
 where
  go :: ParseCtx -> [Req] -> ParseM Req
  go ctx acc = step ctx
   where
    step :: ParseCtx -> ParseM Req
    step ctx' = (getTk >>=) $ \case
      [] -> pure $ parseFin ctx acc
      (LexTokenBigSep : _) -> pure $ parseFin ctx acc
      (LexTokenGroup group : ts) -> do
        putTk group
        r <- parseOuter ctx'{parseCtxGrouper = ReqAnd}
        getTk >>= \case
          [] -> pure ()
          ts -> error $ "unparsed tokens: " ++ show ts
        putTk ts
        go ctx (r : acc)
      (LexTokenOneOf : ts) -> do
        putTk ts
        r <- parseInner ctx'{parseCtxGrouper = ReqOr}
        pure $ parseFin ctx (r : acc)
      (LexTokenSep : ts) ->
        putTk ts *> go ctx acc
      (LexTokenAnd : ts) ->
        putTk ts *> go ctx{parseCtxGrouper = ReqAnd} acc
      (LexTokenOr : ts) ->
        putTk ts *> go ctx{parseCtxGrouper = ReqOr} acc
      (LexTokenText t : ts) ->
        putTk ts *> go ctx (ReqNote t : acc)
      (LexTokenGrade grade : LexTokenOr : ts) ->
        putTk ts *> step ctx'{parseCtxGrade = Just grade}
      (LexTokenGrade grade : ts) ->
        putTk ts *> step ctx'{parseCtxGrade = Just grade}
      (LexTokenSubj _ : _) -> do
        r <- parseCourse ctx'
        go ctx (r : acc)
      (LexTokenCode _ : _) -> do
        r <- parseCourse ctx'
        go ctx (r : acc)
      ts ->
        error $ "unexpected tokens in parseInner: " ++ show ts

parseCourse :: ParseCtx -> ParseM Req
parseCourse ctx = do
  cs <- go ctx
  grade <- (getTk >>=) $ \case
    (LexTokenGrade grade : ts) ->
      putTk ts $> Just grade
    (LexTokenGroup [LexTokenGrade grade] : ts) ->
      putTk ts $> Just grade
    _ -> pure $ parseCtxGrade ctx
  extra <- do
    section <- (getTk >>=) $ \case
      (LexTokenCode section : ts) ->
        putTk ts $> Just section
      (LexTokenGroup [LexTokenCode section] : ts) ->
        putTk ts $> Just section
      (LexTokenGroup [LexTokenSubj component, LexTokenCode section] : ts) ->
        putTk ts $> Just ("(" <> component <> " " <> section <> ")")
      _ -> pure Nothing
    taken <- (getTk >>=) $ \case
      (LexTokenText t : ts)
        | tokenTermSeason t ->
            putTk ts $> Just t
      (LexTokenText "taken" : LexTokenText season : LexTokenText year : ts) ->
        putTk ts $> Just (season <> " " <> year)
      (LexTokenText season : LexTokenText year : ts) ->
        putTk ts $> Just (season <> " " <> year)
      _ -> pure Nothing
    pure $ case (section, taken) of
      (Just section, Just taken) -> Just $ section <> " " <> taken
      (Just section, Nothing) -> Just section
      (Nothing, Just taken) -> Just taken
      (Nothing, Nothing) -> Nothing
  let crb (CourseName subj code) = parseCtxCRB ctx subj code grade
      r = case cs of
        (c :| []) -> crb c
        (c :| cs) -> ReqOr $ map crb (c : cs)
      r' = maybe r (\extra -> ReqAnd [r, ReqNote extra]) extra
  pure r'
 where
  go :: ParseCtx -> ParseM (NonEmpty CourseName)
  go ctx = (getTk >>=) $ \case
    (LexTokenSubj subj : LexTokenCode code : LexTokenSlash : ts) -> do
      putTk ts
      putSubj subj
      c :| cs <- go ctx
      pure (CourseName subj code :| c : cs)
    (LexTokenSubj subj : LexTokenSlash : ts) -> do
      putTk ts
      putSubj subj
      (c@(CourseName _ code) :| cs) <- go ctx
      pure (CourseName subj code :| c : cs)
    (LexTokenCode code : LexTokenSlash : ts) -> do
      putTk ts
      subj <- getSubj
      (c :| cs) <- go ctx
      pure (CourseName subj code :| c : cs)
    (LexTokenSubj subj : LexTokenCode code : ts) -> do
      putTk ts
      putSubj subj
      pure (CourseName subj code :| [])
    (LexTokenCode code : ts) -> do
      putTk ts
      subj <- getSubj
      pure (CourseName subj code :| [])
    ts -> error $ "unexpected tokens in parseCourse: " ++ show ts

normalizeSpace :: Char -> Char
normalizeSpace c = if isSpace c then ' ' else c

main :: IO ()
main = do
  file <- T.map normalizeSpace . T.decodeLatin1 <$> B.getContents
  let courses = concat $ scrapeStringLike file $ chroots classesSelector courseScraper
  pPrint courses