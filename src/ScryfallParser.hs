{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module ScryfallParser where

import           Card
import           Colors
import           Control.Lens            hiding ((.=))
import           Data.Aeson              hiding (encode)
-- import           Data.Aeson.BetterErrors
import           Data.Text               (pack, unpack)
import           Data.UUID
import qualified Data.Vector             as V
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import           Network.HTTP.Simple
import           Network.URI
import           Network.URI.Encode      (encode)
import           Text.Parsec.Char
import           Types

-- (ArtifactType (..), CardType (..),
--                                           SubType (..), SuperType (..),
--                                           TypeLine (..))

data ErrorObject = ErrorObject
  {_status :: Int, _code :: String, _details :: String, _errtype :: Maybe String, _warnings :: Maybe [String]}

data Legalities = Legal | NotLegal | Restricted | Banned deriving Show

data LegalObject = LegalObject {
  _standard          :: Legalities
  , _future          :: Legalities
  , _historic        :: Legalities
  , _gladiator       :: Legalities
  , _pioneer         :: Legalities
  , _explorer        :: Legalities
  , _modern          :: Legalities
  , _legacy          :: Legalities
  , _pauper          :: Legalities
  , _vintage         :: Legalities
  , _penny           :: Legalities
  , _commander       :: Legalities
  , _brawl           :: Legalities
  , _historicbrawl   :: Legalities
  , _alchemy         :: Legalities
  , _paupercommander :: Legalities
  , _duel            :: Legalities
  , _oldschool       :: Legalities
  , _premodern       :: Legalities } deriving Show

data ListObject = ListObject {
    _cardData      :: [CardObject]
  , _has_more      :: Bool
  , _next_page     :: Maybe URI
  , _total_cards   :: Maybe Int
  , _list_warnings :: Maybe [String]}

data CardObject = CardObject {
    _c_arena_id            :: Maybe Int
  , _c_id                  :: UUID
  , _c_lang                :: String
  , _c_mtgo_id             :: Maybe Int
  , _c_mtgo_foil_id        :: Maybe Int
  , _c_multiverse_ids      :: Maybe [Int]
  , _c_tcgplayer_id        :: Maybe Int
  , _c_tcgplayer_etched_id :: Maybe Int
  , _c_cardmarket_id       :: Maybe Int
  , _c_object              :: String
  , _c_oracle_id           :: UUID
  , _c_prints_search_uri   :: URI
  , _c_rulings_uri         :: URI
  , _c_scryfall_uri        :: URI
  , _c_uri                 :: URI
  , _c_all_parts           :: Maybe [CardObject]
  , _c_card_faces          :: Maybe [CardFaceObject]
  , _c_cmc                 :: Double
  , _c_color_identity      :: [String]
  , _c_color_indicator     :: Maybe [String]
  , _c_colors              :: Maybe [String]
  , _c_edhrec_rank         :: Maybe Int
  , _c_hand_modifier       :: Maybe String
  , _c_keywords            :: [String]
  , _c_layout              :: String
  , _c_legalities          :: LegalObject
  , _c_life_modifier       :: Maybe String
  , _c_loyalty             :: Maybe String
  , _c_mana_cost           :: Maybe String
  , _c_name                :: String
  , _c_oracle_text         :: Maybe String
  , _c_oversized           :: Bool
  , _c_penny_rank          :: Maybe Int
  , _c_power               :: Maybe String
  , _c_produced_mana       :: Maybe [String]
  , _c_reserved            :: Bool
  , _c_type_line           :: String} deriving Show

data CardFaceObject = CardFaceObject {
    _cf_artist            :: Maybe String
  , _cf_cmc               :: Maybe Int
  , _cf_color_indicator   :: Maybe String
  , _cf_colors            :: Maybe String
  , _cf_flavor_text       :: Maybe String
  , _cf_illustration_id   :: Maybe UUID
  , _cf_image_uris        :: Maybe [CardFaceObject]
  , _cf_layout            :: Maybe String
  , _cf_loyalty           :: Maybe String
  , _cf_mana_cost         :: String
  , _cf_name              :: String
  , _cf_object            :: String
  , _cf_oracle_id         :: Maybe UUID
  , _cf_oracle_text       :: Maybe String
  , _cf_power             :: Maybe String
  , _cf_printed_name      :: Maybe String
  , _cf_printed_text      :: Maybe String
  , _cf_printed_type_line :: Maybe String
  , _cf_toughness         :: Maybe String
  , _cf_type_line         :: Maybe String
  , _cf_watermark         :: Maybe String } deriving Show

data SetObject = SetObject {
  _s_id                :: UUID
  , _s_code            :: String
  , _s_mtgo_code       :: Maybe String
  , _s_tcgplayer_id    :: Maybe Int
  , _s_name            :: String
  , _s_set_type        :: String
  , _s_released_at     :: Maybe String
  , _s_block_code      :: Maybe String
  , _s_block           :: Maybe String
  , _s_parent_set_code :: Maybe String
  , _s_card_count      :: Int
  , _s_printed_size    :: Maybe Int
  , _s_digital         :: Bool
  , _s_foil_only       :: Bool
  , _s_nonfoil_only    :: Bool
  , _s_scryfall_uri    :: URI
  , _s_icon_svg_uri    :: URI
  , _s_search_uri      :: URI }

$(makeLenses ''ErrorObject)
$(makeLenses ''LegalObject)
$(makeLenses ''ListObject)
$(makeLenses ''CardObject)
$(makeLenses ''CardFaceObject)
$(makeLenses ''SetObject)

-- TODO: Switch to using Aeson better errors
-- TODO: Fix aeson parsing error for queries like t:goblin and t:angel. Error gotten was Expected String


importString = ["import " ++ x | x <- imports]
  where imports = ["Data.Maybe", "Colors", "Control.Lens", "Types", "Card"]

scryfallSearch :: String -> IO ListObject
scryfallSearch s = do
  manager <- newManager $ managerSetProxy noProxy tlsManagerSettings
  request <- parseRequest $ baseUrl ++ encode s
  response <- httpJSON request
  -- a <- getResponseBody response
  -- return a
  print "success"
  return $ getResponseBody response
  where baseUrl = "https://api.scryfall.com/cards/search?q="

test :: String -> IO ()
test s = do
  lo <- scryfallSearch s
  print $ head (lo ^. cardData)

-- testing :: String -> String -> IO a
-- testing s v = do
--   manager <- newManager $ managerSetProxy noProxy tlsManagerSettings
--   request <- parseRequest $ baseUrl ++ encode s
--   response <- httpJSON request
--   body <- getResponseBody response
--   print "success"
--   return body
--   where baseUrl = "https://api.scryfall.com/cards/search?q="


arrayToList :: Array -> [String]
arrayToList =
  V.foldr (\x xs -> case x of
                     (String s) -> unpack s : xs
                     _          -> xs) []

instance FromJSON Color where
  parseJSONList (Array a) = return $ map colorString (arrayToList a)

instance FromJSON PT where
  parseJSON (String t) = case unpack t of
    "*" -> return Star
    xs  -> case reverse xs of
             '*':'+':ys -> return $ StarPlus (read (reverse ys))
             _          -> return $ PT (read xs)

instance FromJSON Keyword where
  parseJSONList (Array a) = undefined

instance FromJSON Legalities where
  parseJSON (String s) = return $ f (unpack s)
    where f s = case s of
                 "legal"      -> Legal
                 "not_legal"  -> NotLegal
                 "restricted" -> Restricted
                 "banned"     -> Banned

instance FromJSON URI where
  parseJSON (String s) =
    case parseURI $ unpack s of
      Just x -> return x

instance FromJSON ListObject where
  parseJSON (Object o) = do
    cd <- o .: "data"
    has_more <- o .: "has_more"
    next_page <- o .:? "next_page"
    total_cards <- o .:? "total_cards"
    list_warnings <- o .:? "warnings"
    return ListObject{
      _cardData = cd
      , _has_more = has_more
      , _next_page = next_page
      , _total_cards = total_cards
      , _list_warnings = list_warnings }

instance FromJSON CardFaceObject where
  parseJSON (Object o) = do
    artist <- o .:? "artist"
    cmc <- o .:? "cmc"
    color_indicator <- o .:? "color_indicator"
    colors <- o .:? "colors"
    flavor_text <- o .:? "flavor_text"
    illustration_id <- o .:? "illustration_id"
    image_uris <- o .:? "image_uris"
    layout <- o .:? "layout"
    loyalty <- o .:? "loyalty"
    mana_cost <- o .: "mana_cost"
    name <- o .: "name"
    obj <- o .: "object"
    oracle_id <- o .:? "oracle_id"
    oracle_text <- o .:? "oracle_text"
    power <- o .:? "power"
    printed_name <- o .:? "printed_name"
    printed_text <- o .:? "printed_text"
    printed_type_line <- o .:? "printed_type_line"
    toughness <- o .:? "toughness"
    type_line <- o .:? "type_line"
    watermark <- o .:? "watermark"
    return CardFaceObject {
        _cf_artist = artist
      , _cf_cmc = cmc
      , _cf_color_indicator = color_indicator
      , _cf_colors = colors
      , _cf_flavor_text = flavor_text
      , _cf_illustration_id = illustration_id
      , _cf_image_uris = image_uris
      , _cf_layout = layout
      , _cf_loyalty = loyalty
      , _cf_mana_cost = mana_cost
      , _cf_name = name
      , _cf_object = obj
      , _cf_oracle_id = oracle_id
      , _cf_oracle_text = oracle_text
      , _cf_power = power
      , _cf_printed_name = printed_name
      , _cf_printed_text = printed_text
      , _cf_printed_type_line = printed_type_line
      , _cf_toughness = toughness
      , _cf_type_line = type_line
      , _cf_watermark = watermark }

instance FromJSON CardObject where
  parseJSON (Object o) = do
    c_arena_id <- o .:? "arena_id"
    id <- o .: "id"
    lang <- o .: "lang"
    mtgo_id <- o .:? "mtgo_id"
    mtgo_foil_id <- o .:? "mtgo_foil_id"
    multiverse_ids <- o .:? "multiverse_ids"
    tcgplayer_id <- o .:? "tcgplayer_id"
    tcgplayer_etched <- o .:? "tcgplayer_etched_id"
    cardmarket_id <- o .:? "cardmarket_id"
    cobject <- o .: "object"
    oracle_id <- o .: "oracle_id"
    printsSearchURI <- o .: "prints_search_uri"
    rulingsURI <- o .: "rulings_uri"
    scryfallURI <- o .: "scryfall_uri"
    uri <- o .: "uri"
    all_parts <- o .:? "all_parts"
    card_faces <- o .:? "card_faces"
    cmc <- o .: "cmc"
    color_identity <- o .: "color_identity"
    color_indicator <- o .:? "color_indicator"
    colors <- o .:? "colors"
    edhrec_rank <- o .:? "edhrec_rank"
    hand_modifier <- o .:? "hand_modifier"
    keywords <- o .: "keywords"
    layout <- o .: "layout"
    legalities <- o .: "legalities"
    life_modifier <- o .:? "life_modifier"
    loyalty <- o .:? "loyalty"
    mana_cost <- o .:? "mana_cost"
    name <- o .: "name"
    oracle_text <- o .:? "oracle_text"
    oversized <- o .: "oversized"
    penny_rank <- o .:? "penny_rank"
    power <- o .:? "power"
    produced_mana <- o .:? "produced_mana"
    reserved <- o .: "reserved"
    type_line <- o .: "type_line"
    return CardObject {
        _c_arena_id = c_arena_id
      , _c_id = id
      , _c_lang = lang
      , _c_mtgo_id = mtgo_id
      , _c_mtgo_foil_id = mtgo_foil_id
      , _c_multiverse_ids = multiverse_ids
      , _c_tcgplayer_id = tcgplayer_id
      , _c_tcgplayer_etched_id = tcgplayer_etched
      , _c_cardmarket_id = cardmarket_id
      , _c_object = cobject
      , _c_oracle_id = oracle_id
      , _c_prints_search_uri = printsSearchURI
      , _c_rulings_uri = rulingsURI
      , _c_scryfall_uri = scryfallURI
      , _c_uri = uri
      , _c_all_parts = all_parts
      , _c_card_faces = card_faces
      , _c_cmc = cmc
      , _c_color_identity = color_identity
      , _c_color_indicator = color_indicator
      , _c_colors = colors
      , _c_edhrec_rank = edhrec_rank
      , _c_hand_modifier = hand_modifier
      , _c_keywords = keywords
      , _c_layout = layout
      , _c_legalities = legalities
      , _c_life_modifier = life_modifier
      , _c_loyalty = loyalty
      , _c_mana_cost = mana_cost
      , _c_name = name
      , _c_oracle_text = oracle_text
      , _c_oversized = oversized
      , _c_penny_rank = penny_rank
      , _c_power = power
      , _c_produced_mana = produced_mana
      , _c_reserved = reserved
      , _c_type_line = type_line }
  parseJSON x = error $ "Tried to parse " ++ show x

instance FromJSON LegalObject where
  parseJSON (Object o) = do
    standard <- o .: "standard"
    future <- o .: "future"
    historic <- o .: "historic"
    gladiator <- o .: "gladiator"
    pioneer <- o .: "pioneer"
    explorer <- o .: "explorer"
    modern <- o .: "modern"
    legacy <- o .: "legacy"
    pauper <- o .: "pauper"
    vintage <- o .: "vintage"
    penny <- o .: "penny"
    commander <- o .: "commander"
    brawl <- o .: "brawl"
    historicbrawl <- o .: "historicbrawl"
    alchemy <- o .: "alchemy"
    paupercommander <- o .: "paupercommander"
    duel <- o .: "duel"
    oldschool <- o .: "oldschool"
    premodern <- o .: "premodern"
    return LegalObject {
        _standard = standard
      , _future = future
      , _historic = historic
      , _gladiator = gladiator
      , _pioneer = pioneer
      , _explorer = explorer
      , _modern = modern
      , _legacy = legacy
      , _pauper = pauper
      , _vintage = vintage
      , _penny = penny
      , _commander = commander
      , _brawl = brawl
      , _historicbrawl = historicbrawl
      , _alchemy = alchemy
      , _paupercommander = paupercommander
      , _duel = duel
      , _oldschool = oldschool
      , _premodern = premodern }

instance FromJSON SetObject where
  parseJSON (Object o) = do
    id <- o .: "id"
    code <- o .: "code"
    mtgo_code <- o .:? "mtgo_code"
    tcgplayer_id <- o .:? "tcgplayer_id"
    name <- o .: "name"
    set_type <- o .: "set_type"
    released_date <- o .:? "released_at"
    block_code <- o .:? "block_code"
    block <- o .:? "block"
    parent_set_code <- o .:? "parent_set_code"
    card_count <- o .: "card_count"
    printed_size <- o .:? "printed_size"
    digital <- o .: "digital"
    foil_only <- o .: "foil_only"
    nonfoil_only <- o .: "nonfoil_only"
    scryfallURI <- o .: "scryfall_uri"
    icon_svgURI <- o .: "icon_svg_uri"
    searchURI <- o .: "search_uri"
    return SetObject {
                _s_id = id
              , _s_code = code
              , _s_mtgo_code = mtgo_code
              , _s_tcgplayer_id = tcgplayer_id
              , _s_name = name
              , _s_set_type = set_type
              , _s_released_at = released_date
              , _s_block_code = block_code
              , _s_block = block
              , _s_parent_set_code = parent_set_code
              , _s_card_count = card_count
              , _s_printed_size = printed_size
              , _s_digital = digital
              , _s_foil_only = foil_only
              , _s_nonfoil_only = nonfoil_only
              , _s_scryfall_uri = scryfallURI
              , _s_icon_svg_uri = icon_svgURI
              , _s_search_uri = searchURI }

enumeration :: Enum a => [a]
enumeration = enumFrom (toEnum 0)

-- assocType :: Enum a =>  CardType -> (a -> SubType, a)
-- assocType Artifact = (AType, ArtifactType)

findFirst :: Show a => String -> [a] -> Maybe a
findFirst _ []     = Nothing
findFirst s (x:xs) = if show x == s then Just x else findFirst s xs

-- TODO: FIX this somehow
findAType :: String -> Maybe SubType
findAType s = case findFirst s (enumeration :: [ArtifactType]) of
  Just x  -> Just $ AType x
  Nothing -> Nothing

findEType :: String -> Maybe SubType
findEType s = case findFirst s (enumeration :: [EnchantmentType]) of
  Just x  -> Just $ EType x
  Nothing -> Nothing

findLType :: String -> Maybe SubType
findLType s = case findFirst s (enumeration :: [LandType]) of
  Just x  -> Just $ LType x
  Nothing -> Nothing

findWType :: String -> Maybe SubType
findWType s = case findFirst s (enumeration :: [PlaneswalkerType]) of
  Just x  -> Just $ WType x
  Nothing -> Nothing

findSType :: String -> Maybe SubType
findSType s = case findFirst s (enumeration :: [SpellType]) of
  Just x  -> Just $ SType x
  Nothing -> Nothing

findCType :: String -> Maybe SubType
findCType s = case findFirst s (enumeration :: [CreatureType]) of
  Just x  -> Just $ CType x
  Nothing -> Nothing

findPType :: String -> Maybe SubType
findPType s = case findFirst s (enumeration :: [PlaneType]) of
  Just x  -> Just $ PType x
  Nothing -> Nothing

parseTL :: String -> TypeLine
parseTL s = go (TypeLine [] [] []) False (words s)
  where go x _ [] = x
        go tl@(TypeLine sup ct sub) False (s:ss)
          | s == "—" = go tl True ss
          | otherwise = case findFirst s (enumFrom(toEnum 0 :: SuperType)) of
              Just super -> go (TypeLine (sup++[super]) ct sub) False ss
              Nothing -> case findFirst s (enumFrom (toEnum 0 :: CardType)) of
                Just card -> go (TypeLine sup (ct++[card]) sub) False ss
                _         -> go tl False ss
        go tl@(TypeLine sup ct sub) True (s:ss) =
          let add = (\x (TypeLine sup ct sub) -> TypeLine sup ct (sub++[x])) in
          case findAType s of
            Just x -> go (add x tl) True ss
            Nothing ->
              case findEType s of
                Just x -> go (add x tl) True ss
                Nothing ->
                  case findLType s of
                    Just x -> go (add x tl) True ss
                    Nothing ->
                      case findWType s of
                        Just x -> go (add x tl) True ss
                        Nothing ->
                          case findSType s of
                            Just x -> go (add x tl) True ss
                            Nothing ->
                              case findCType s of
                                Just x -> go (add x tl) True ss
                                Nothing ->
                                  case findPType s of
                                    Just x  -> go (add x tl) True ss
                                    Nothing -> go tl True ss

parseColor s = case s of
  "W" -> White
  "U" -> Blue
  "B" -> Black
  "R" -> Red
  "G" -> Green

parsePip :: [String] -> Pip
parsePip [x] | x `elem` ["W", "U", "B", "R", "G"] = CSym (Colored (parseColor x))
             | x == "X" = XSym
             | x == "P" = PhySym
             | x == "S" = SnowSym
             | otherwise = GenSym (read x)
parsePip (x:xs) = HyPip (parsePip [x]) (parsePip xs)

parseMC :: String -> Maybe [Pip]
parseMC "" = Nothing
parseMC s  = Just $ map (parsePip . sep) (sepPip s)

sep :: String -> [String]
sep ""       = []
sep ('/':xs) = takeWhile (/= '/') xs : sep (dropWhile (/= '/') xs)
sep xs       = takeWhile (/= '/') xs : sep (dropWhile (/= '/') xs)

sepPip :: String -> [String]
sepPip ""       = []
sepPip ('{':xs) = takeWhile (/= '}') xs : sepPip (dropWhile (/= '{') xs)

-- TODO: ManaCost FromJSON Instance
instance FromJSON Properties where
  parseJSON (Object v) = do
    name <- v .: "name"
    color <- v .: "colors"
    identity <- v .: "color_identity"
    manaCost <- v .: "mana_cost"
    oracleText <- v .: "oracle_text"
    typeline <- v .: "type_line"
    power <- v .:? "power"
    toughness <- v .:? "toughness"
    keywords <- v .: "keywords"
    loyalty <- v .:? "loyalty"
    -- legality <- v .: "legalities"
    return (Properties
            {_name = name
            ,_color = color
            ,_identity = identity
            ,_manaCost = parseMC manaCost
            ,_oracleText = oracleText
            ,_typeLine = parseTL typeline
            ,_power = power
            ,_toughness = toughness
            ,_keywords = keywords
            ,_loyalty = loyalty})
