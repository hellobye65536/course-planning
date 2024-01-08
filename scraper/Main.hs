{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Main (main) where

import Control.Monad.Trans.State (State, execState, get, put, runState)
import CoursePlanning.Course
import qualified Data.ByteString as B
import Data.Char (isAlpha, isAlphaNum, isAsciiUpper, isDigit, isSpace)
import Data.Foldable (asum, traverse_)
import Data.Functor (($>))
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Optics (assign, makeFieldLabelsNoPrefix, modifying, over, use, view, (&))
import Text.HTML.Scalpel.Core
  ( ScraperT,
    Selector,
    TagName (AnyTag),
    chroots,
    scrapeStringLike,
    texts,
    (@:),
    (@=),
  )
import Text.Pretty.Simple (pPrint)
import Prelude hiding (lex)

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
  [ ("or", LexTokenOr),
    ("and", LexTokenAnd),
    ("&", LexTokenAnd),
    ("one", LexTokenOneOf),
    ("either", LexTokenOneOf),
    ("/", LexTokenSlash),
    (",", LexTokenSep),
    ("taken", LexTokenText "taken"),
    ("fall", LexTokenText "fall"),
    ("spring", LexTokenText "spring"),
    ("summer", LexTokenText "summer"),
    ("winter", LexTokenText "winter")
  ]

tokenIgnore :: [Text]
tokenIgnore =
  [ "a",
    "at",
    "grade",
    "higher",
    "in",
    "least",
    "minimum",
    "of",
    "with"
  ]

tokenSpecialText :: [Text]
tokenSpecialText =
  [ "4U Calculus and Vectors", -- MATH
    "CO 370 taken prior to winter 2004", -- CO 372
    "any STAT course" -- STAT 316
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
  a `elem` ("1234" :: String)
    && b `elem` ("AB" :: String)
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
                else concatMap lexResolve rs
        fmap (ts ++) lexChunks
    where
      lexRawNonText (LexRawCode _) = True
      lexRawNonText (LexRawParen ts) = any (\case LexTokenText _ -> False; _ -> True) ts
      lexRawNonText _ = False
  _ -> pure []

lexResolve :: LexRaw -> [LexToken]
lexResolve (LexRawParen ts) = [LexTokenGroup ts]
lexResolve (LexRawSubj subj) = [LexTokenSubj subj]
lexResolve (LexRawCode code) = [LexTokenCode code]
lexResolve (LexRawPureText text) = [LexTokenText text]
lexResolve (LexRawText t)
  | Just tk <- lookup tl tokenKeyword = [tk]
  | t `elem` tokenSpecialText = [LexTokenText t]
  | tl `elem` tokenIgnore = []
  | tokenGrade t = [LexTokenGrade (read $ T.unpack $ T.init t)]
  | tokenTermSeason t = [LexTokenText t]
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
        let (r, t'') = T.span (/= '"') t'
        put $ T.tail t''
        rs <- lexChunk
        pure (LexRawPureText r : rs)
    | isText c -> do
        let (r, t') = munchText $ liftA2 zip T.inits T.tails t
        put t'
        rs <- lexChunk
        pure $
          if
            | tokenSubj r -> LexRawSubj r : rs
            | tokenCode r -> LexRawCode r : rs
            | Just (subj, code) <- tokenSubjCode r ->
                LexRawSubj subj : LexRawCode code : rs
            | "level" <- T.toLower r,
              ( LexRawText (T.toLower -> "at")
                  : LexRawText (T.toLower -> "least")
                  : LexRawText (tokenTerm -> True)
                  : _
                ) <-
                rs ->
                [LexRawPureText t]
            | otherwise -> LexRawText r : rs
    | otherwise ->
        put t' *> fmap (LexRawText (T.singleton c) :) lexChunk
    where
      isText c = isAlphaNum c || c == '%' || c == ':' || c == '-'
      munchText ((_, a :> '.' :> b :> _) : _ : _ : z')
        | isDigit a && isDigit b = munchText z'
      munchText (p : z')
        | (_, c :> _) <- p,
          isText c =
            munchText z'
        | otherwise = p
      munchText [] = error "unreachable"
  _ -> pure []

type CourseReqBuilder = Text -> Text -> Maybe Float -> Req

data ParseCtx = ParseCtx
  { crb :: CourseReqBuilder,
    grouper :: [Req] -> Req,
    grade :: Maybe Float
  }

data ParseState = ParseState
  { tokens :: [LexToken],
    subj :: Maybe Text
  }

type ParseM = State ParseState

makeFieldLabelsNoPrefix ''ParseCtx
makeFieldLabelsNoPrefix ''ParseState

getTokens :: ParseM [LexToken]
getTokens = use #tokens

putTokens :: [LexToken] -> ParseM ()
putTokens = assign #tokens

getSubj :: ParseM Text
getSubj = fromMaybe (error "missing subject") <$> use #subj

putSubj :: Text -> ParseM ()
putSubj = assign #subj . Just

parseCtxDefault, parseCtxPrereq, parseCtxCoreq, parseCtxAntireq :: ParseCtx
parseCtxDefault =
  ParseCtx
    { crb = \_ _ _ -> error "undefined parseCtxCRB",
      grouper = ReqAnd,
      grade = Nothing
    }
parseCtxPrereq = parseCtxDefault {crb = ReqCourse'}
parseCtxCoreq = parseCtxDefault {crb = coreqCRB}
parseCtxAntireq = parseCtxDefault {crb = coreqCRB, grouper = ReqOr}

coreqCRB :: CourseReqBuilder
coreqCRB subj code Nothing = ReqCoCourse' subj code
coreqCRB _ _ (Just _) = error "unexpected grade in corequisite req"

parse :: ParseCtx -> [LexToken] -> Req
parse ctx ts = case view #tokens <$> runState (parseOuter ctx) (ParseState ts Nothing) of
  (r, []) -> r
  (_, ts) -> error $ "unparsed tokens: " ++ show ts

parseFin :: ParseCtx -> [Req] -> Req
parseFin ctx = \case
  [] -> ReqTrue
  [r] -> r
  rs -> ctx.grouper $ reverse rs

parseOuter :: ParseCtx -> ParseM Req
parseOuter ctx = go ctx []
  where
    go :: ParseCtx -> [Req] -> ParseM Req
    go ctx acc = (getTokens >>=) $ \case
      [] -> pure $ parseFin ctx acc
      (LexTokenBigSep : ts) -> putTokens ts *> go ctx acc
      (LexTokenAnd : ts) ->
        putTokens ts *> go ctx {grouper = ReqAnd} acc
      (LexTokenOr : ts) ->
        putTokens ts *> go ctx {grouper = ReqOr} acc
      _ -> parseInner ctx >>= (go ctx . (: acc))

parseInner :: ParseCtx -> ParseM Req
parseInner ctx = go ctx []
  where
    go :: ParseCtx -> [Req] -> ParseM Req
    go ctx acc = step ctx
      where
        step :: ParseCtx -> ParseM Req
        step ctx' = (getTokens >>=) $ \case
          [] -> pure $ parseFin ctx acc
          (LexTokenBigSep : _) -> pure $ parseFin ctx acc
          (LexTokenGroup group : ts) -> do
            putTokens group
            r <- parseOuter ctx' {grouper = ReqAnd}
            getTokens >>= \case
              [] -> pure ()
              ts -> error $ "unparsed tokens: " ++ show ts
            putTokens ts
            go ctx (r : acc)
          (LexTokenOneOf : ts) -> do
            putTokens ts
            r <- parseInner ctx' {grouper = ReqOr}
            pure $ parseFin ctx (r : acc)
          (LexTokenSep : ts) ->
            putTokens ts *> go ctx acc
          (LexTokenAnd : ts) ->
            putTokens ts *> go ctx {grouper = ReqAnd} acc
          (LexTokenOr : ts) ->
            putTokens ts *> go ctx {grouper = ReqOr} acc
          (LexTokenText t : ts) ->
            putTokens ts *> go ctx (ReqNote t : acc)
          (LexTokenGrade grade : LexTokenOr : ts) ->
            putTokens ts *> step ctx' {grade = Just grade}
          (LexTokenGrade grade : ts) ->
            putTokens ts *> step ctx' {grade = Just grade}
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
  grade <- (getTokens >>=) $ \case
    (LexTokenGrade grade : ts) ->
      putTokens ts $> Just grade
    (LexTokenGroup [LexTokenGrade grade] : ts) ->
      putTokens ts $> Just grade
    _ -> pure ctx.grade
  extra <- do
    section <- (getTokens >>=) $ \case
      (LexTokenCode section : ts) ->
        putTokens ts $> Just section
      (LexTokenGroup [LexTokenCode section] : ts) ->
        putTokens ts $> Just section
      (LexTokenGroup [LexTokenSubj component, LexTokenCode section] : ts) ->
        putTokens ts $> Just ("(" <> component <> " " <> section <> ")")
      _ -> pure Nothing
    taken <- (getTokens >>=) $ \case
      (LexTokenText t : ts)
        | tokenTermSeason t ->
            putTokens ts $> Just t
      (LexTokenText "taken" : LexTokenText season : LexTokenText year : ts) ->
        putTokens ts $> Just (season <> " " <> year)
      (LexTokenText season : LexTokenText year : ts) ->
        putTokens ts $> Just (season <> " " <> year)
      _ -> pure Nothing
    pure $ case (section, taken) of
      (Just section, Just taken) -> Just $ section <> " " <> taken
      (Just section, Nothing) -> Just section
      (Nothing, Just taken) -> Just taken
      (Nothing, Nothing) -> Nothing
  let crb (CourseName subj code) = ctx.crb subj code grade
      r = case cs of
        (c :| []) -> crb c
        (c :| cs) -> ReqOr $ map crb (c : cs)
      r' = maybe r (\extra -> ReqAnd [r, ReqNote extra]) extra
  pure r'
  where
    go :: ParseCtx -> ParseM (NonEmpty CourseName)
    go ctx = (getTokens >>=) $ \case
      (LexTokenSubj subj : LexTokenCode code : LexTokenSlash : ts) -> do
        putTokens ts
        putSubj subj
        c :| cs <- go ctx
        pure (CourseName subj code :| c : cs)
      (LexTokenSubj subj : LexTokenSlash : ts) -> do
        putTokens ts
        putSubj subj
        (c@(CourseName _ code) :| cs) <- go ctx
        pure (CourseName subj code :| c : cs)
      (LexTokenCode code : LexTokenSlash : ts) -> do
        putTokens ts
        subj <- getSubj
        (c :| cs) <- go ctx
        pure (CourseName subj code :| c : cs)
      (LexTokenSubj subj : LexTokenCode code : ts) -> do
        putTokens ts
        putSubj subj
        pure (CourseName subj code :| [])
      (LexTokenCode code : ts) -> do
        putTokens ts
        subj <- getSubj
        pure (CourseName subj code :| [])
      ts -> error $ "unexpected tokens in parseCourse: " ++ show ts

scrapeSelector :: Selector
scrapeSelector = AnyTag @: ["class" @= "divTable"]

scrapeCourse :: (Monad m) => ScraperT Text m Course
scrapeCourse = do
  subs <- map T.strip <$> texts "div"
  let (subj, code, comp, units) = case T.splitOn " " (subs !! 1) of
        [subj, code, comp, units] -> (subj, code, comp, units)
        _ -> error "failed to find course info"
      cid = case T.stripPrefix "Course ID: " (subs !! 2) of
        Just cid -> cid
        _ -> error "failed to find course id"
      course =
        Course
          { name = CourseName subj code,
            id = cid,
            components = T.splitOn "," comp,
            units = read $ T.unpack units,
            title = subs !! 3,
            description = subs !! 4,
            note = "",
            offered = [],
            req = ReqTrue,
            crosslist = []
          }
      course' = execState (traverse_ scrapeCourseOther $ drop 5 subs) course & over #req simplifyReq
  pure course'

scrapeCourseOther :: Text -> State Course ()
scrapeCourseOther sub
  | T.all isSpace sub = pure ()
  | "[Note: " `T.isPrefixOf` sub = assign #note sub
  | Just prereqs <- T.stripPrefix "Prereq:" sub =
      modifying #req (\req -> ReqAnd [req, parse parseCtxPrereq $ lex prereqs])
  | Just coreqs <- T.stripPrefix "Coreq:" sub =
      modifying #req (\req -> ReqAnd [req, parse parseCtxCoreq $ lex coreqs])
  | Just antireqs <- T.stripPrefix "Antireq:" sub =
      modifying #req (\req -> ReqAnd [req, parse parseCtxAntireq $ lex antireqs])
  | Just crosslist <- T.stripPrefix "(Cross-listed with " sub =
      assign #crosslist $ parseCrosslist $ T.init crosslist
  | Just _online <- T.stripSuffix " offered Online" sub = pure ()
  | Just _consent <- T.stripSuffix " Consent Required" sub = pure ()
  | Just _offered <- T.stripPrefix "Offered at " sub = pure ()
  | Just _offered <- T.stripPrefix "Also offered at " sub = pure ()
  | otherwise = error $ "unknown text: " ++ show sub

parseCrosslist :: Text -> [CourseName]
parseCrosslist = map (go . T.splitOn " ") . T.splitOn ", "
  where
    go [subj, code] = CourseName subj code
    go _ = error "bad course in crosslist"

normalizeSpace :: Char -> Char
normalizeSpace c = if isSpace c then ' ' else c

main :: IO ()
main = do
  file <- T.map normalizeSpace . T.decodeLatin1 <$> B.getContents
  let courses = concat $ scrapeStringLike file $ chroots scrapeSelector scrapeCourse
  pPrint courses
