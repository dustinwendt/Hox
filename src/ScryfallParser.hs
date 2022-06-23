{-# LANGUAGE OverloadedStrings #-}
module ScryfallParser where

import           Card
import           Colors
import           Data.Aeson
import           Data.Dates
import qualified Data.Text               as T
import           Data.UUID
import qualified Data.Vector             as V
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import           Network.HTTP.Simple
import           Network.URI
import           Network.URI.Encode      as E
import           Prelude                 as P
import           Text.Parsec.Char
import           Types

data ErrorObject = ErrorObject
  {_status :: Int, _code :: String, _details :: String, _type :: Maybe String, _warnings :: Maybe [String]}

-- mkLenses $(ErrorObject)

data Legalities = Legal | NotLegal | Restricted | Banned

legalString :: String -> Legalities
legalString s = case s of
  "legal"      -> Legal
  "not_legal"  -> NotLegal
  "restricted" -> Restricted
  "banned"     -> Banned

data LegalObject = LegalObject {
  standard          :: Legalities
  , future          :: Legalities
  , historic        :: Legalities
  , gladiator       :: Legalities
  , pioneer         :: Legalities
  , explorer        :: Legalities
  , modern          :: Legalities
  , legacy          :: Legalities
  , pauper          :: Legalities
  , vintage         :: Legalities
  , penny           :: Legalities
  , commander       :: Legalities
  , brawl           :: Legalities
  , historicbrawl   :: Legalities
  , alchemy         :: Legalities
  , paupercommander :: Legalities
  , duel            :: Legalities
  , oldschool       :: Legalities
  , premodern       :: Legalities }

data ListObject = ListObject {
  cardData      :: [CardObject]
  , has_more    :: Bool
  , next_page   ::  Maybe URI
  , total_cards :: Maybe Int
  , warnings    :: [String]}

data CardObject = CardObject {
  c_arena_id              :: Maybe Int
  , c_id                  :: UUID
  , c_lang                :: String
  , c_mtgo_id             :: Maybe Int
  , c_mtgo_foil_id        :: Maybe Int
  , c_multiverse_ids      :: Maybe [Int]
  , c_tcgplayer_id        :: Maybe Int
  , c_tcgplayer_etched_id :: Maybe Int
  , c_cardmarket_id       :: Maybe Int
  , c_object              :: String
  , c_oracle_id           :: UUID
  , c_prints_search_uri   :: URI
  , c_rulings_uri         :: URI
  , c_scryfall_uri        :: URI
  , c_uri                 :: URI
  , c_all_parts           :: Maybe [CardObject]
  , c_card_faces          :: Maybe [CardFaceObject]
  , c_cmc                 :: Double
  , c_color_identity      :: [Color]
  , c_color_indicator     :: Maybe [Color]
  , c_colors              :: Maybe [Color]
  , c_edhrec_rank         :: Maybe Int
  , c_hand_modifier       :: Maybe String
  , c_keywords            :: [String]
  , c_layout              :: String
  , c_legalities          :: LegalObject
  , c_life_modifier       :: Maybe String
  , c_loyalty             :: Maybe String
  , c_mana_cost           :: Maybe String
  , c_name                :: String
  , c_oracle_text         :: Maybe String
  , c_oversized           :: Bool
  , c_penny_rank          :: Maybe Int
  , c_power               :: Maybe String
  , c_produced_mana       :: Maybe [Color]
  , c_reserved            :: Bool
  , c_type_line           :: String}

data CardFaceObject = CardFaceObject {
  cf_artist              :: Maybe String
  , cf_cmc               :: Maybe Int
  , cf_color_indicator   :: Maybe Color
  , cf_colors            :: Maybe Color
  , cf_flavor_text       :: Maybe String
  , cf_illustration_id   :: Maybe UUID
  , cf_image_uris        :: Maybe [CardFaceObject]
  , cf_layout            :: Maybe String
  , cf_loyalty           :: Maybe String
  , cf_mana_cost         :: String
  , cf_name              :: String
  , cf_object            :: String
  , cf_oracle_id         :: Maybe UUID
  , cf_oracle_text       :: Maybe String
  , cf_power             :: Maybe String
  , cf_printed_name      :: Maybe String
  , cf_printed_text      :: Maybe String
  , cf_printed_type_line :: Maybe String
  , cf_toughness         :: Maybe String
  , cf_type_line         :: Maybe String
  , cf_watermark         :: Maybe String }

data SetObject = SetObject {
  s_id                :: UUID
  , s_code            :: String
  , s_mtgo_code       :: Maybe String
  , s_tcgplayer_id    :: Maybe Int
  , s_name            :: String
  , s_set_type        :: String
  , s_released_at     :: Maybe DateTime
  , s_block_code      :: Maybe String
  , s_block           :: Maybe String
  , s_parent_set_code :: Maybe String
  , s_card_count      :: Int
  , s_printed_size    :: Maybe Int
  , s_digital         :: Bool
  , s_foil_only       :: Bool
  , s_nonfoil_only    :: Bool
  , s_scryfall_uri    :: URI
  , s_icon_svg_uri    :: URI
  , search_uri        :: URI}
-- subTypes :: a
-- subTypes = do
--   char '—'
--   space

importString = ["import " ++ x | x <- imports]
  where imports = ["Data.Maybe", "Colors", "Control.Lens", "Types", "Card"]

scryfallSearch :: String -> IO ()
scryfallSearch s = do
  manager <- newManager $ managerSetProxy noProxy tlsManagerSettings
  request <- parseRequest $ baseUrl ++ E.encode s
  response <- httpLBS request
  case getResponseStatusCode response of
    200 -> print $ getResponseBody response
    x   -> print $ "Error: " ++ show x
  where baseUrl = "https://api.scryfall.com/cards/search?q="

arrayToList :: Array -> [String]
arrayToList =
  V.foldr (\x xs -> case x of
                     (String s) -> T.unpack s : xs
                     _          -> xs) []

mcString :: String -> [Pip]
mcString = go "" []
  where go "" ps "" = ps

instance FromJSON Color where
  parseJSONList (Array a) = return $ map colorString (arrayToList a)

instance FromJSON Pip where
  parseJSON = undefined
 --   parseJSONList (String t) = return $ mcString (T.unpack t)

instance FromJSON PT where
  parseJSON (String t) = case T.unpack t of
    "*" -> return Star
    xs  -> case reverse xs of
             '*':'+':ys -> return $ StarPlus (read (reverse ys))
             _          -> return $ PT (read xs)

instance FromJSON TypeLine where
  parseJSON (String s) = undefined

instance FromJSON Keyword where
  parseJSONList (Array a) = undefined

instance FromJSON Legality where
  parseJSON (Object o) = undefined

instance FromJSON Properties where
  parseJSON (Object v) = do
    name <- v .: "name"
    color <- v .: "colors"
    identity <- v .: "color_identity"
    manaCost <- v .: "mana_cost"
    oracleText <- v .: "oracle_text"
    typeLine <- v .: "type_line"
    power <- v .:? "power"
    toughness <- v .:? "toughness"
    keywords <- v .: "keywords"
    loyalty <- v .:? "loyalty"
    legality <- v .: "legalities"
    return (Properties
            {_name = name
            ,_color = color
            ,_identity = identity
            ,_manaCost = manaCost
            ,_oracleText = oracleText
            ,_typeLine = typeLine
            ,_power = power
            ,_toughness = toughness
            ,_keywords = keywords
            ,_loyalty = loyalty
            ,_legality = legality})
