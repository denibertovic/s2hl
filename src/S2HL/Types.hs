{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module S2HL.Types where
import           Data.Csv
import           Data.Data          (Data)
import           Data.Text.Encoding as E
import           Data.Text.Lazy     as T
import           Data.Typeable      (Typeable)


data Currency = HRK | USD | EUR deriving (Eq, Show, Read, Data, Typeable)

data HledgerContext = HledgerContext {
                 hlDate        :: T.Text
               , hlCurrency    :: Currency
               , hlDescription :: T.Text
               , hlExpenses    :: T.Text
               , hlAssets      :: T.Text
               } deriving (Eq, Show, Typeable, Data)

data ErsteCSV = ErsteCSV {
                tNo               :: !T.Text
              , account           :: !T.Text
              , pnbFrom           :: !T.Text
              , pnbTo             :: !T.Text
              , dateOfTransaction :: !T.Text
              , dateOfCurrency    :: !T.Text
              , amountIn          :: !T.Text
              , amountOut         :: !T.Text
              , location          :: !T.Text
              , description       :: !T.Text
              , pp                :: !T.Text
              , refNo             :: !T.Text
              , state             :: !T.Text
              } deriving (Eq, Show)

instance FromNamedRecord ErsteCSV where
    parseNamedRecord m = ErsteCSV
        <$> m .: E.encodeUtf8 "Redni broj"
        <*> m .: E.encodeUtf8 "Broj računa platitelja"
        <*> m .: E.encodeUtf8 "PNB platitelja"
        <*> m .: E.encodeUtf8 "PNB primatelja"
        <*> m .: E.encodeUtf8 "Datum izvršenja"
        <*> m .: E.encodeUtf8 "Datum valute"
        <*> m .: E.encodeUtf8 "Uplate"
        <*> m .: E.encodeUtf8 "Isplate"
        <*> m .: E.encodeUtf8 "Mjesto"
        <*> m .: E.encodeUtf8 "Opis plaćanja, tečaj"
        <*> m .: E.encodeUtf8 "Platitelj/primatelj"
        <*> m .: E.encodeUtf8 "Referenca plaćanja"
        <*> m .: E.encodeUtf8 "Stanje"

instance ToNamedRecord ErsteCSV where
    toNamedRecord (ErsteCSV {..}) = namedRecord [
          E.encodeUtf8 "Redni broj"             .= tNo
        , E.encodeUtf8 "Broj računa platitelja" .= account
        , E.encodeUtf8 "PNB platitelja"         .= pnbFrom
        , E.encodeUtf8 "PNB primatelja"         .= pnbTo
        , E.encodeUtf8 "Datum izvršenja"        .= dateOfTransaction
        , E.encodeUtf8 "Datum valute"           .= dateOfCurrency
        , E.encodeUtf8 "Uplate"                 .= amountIn
        , E.encodeUtf8 "Isplate"                .= amountOut
        , E.encodeUtf8 "Mjesto"                 .= location
        , E.encodeUtf8 "Opis plaćanja, tečaj"   .= description
        , E.encodeUtf8 "Platitelj/primatelj"    .= pp
        , E.encodeUtf8 "Referenca plaćanja"     .= refNo
        , E.encodeUtf8 "Stanje"                 .= state
        ]

instance DefaultOrdered ErsteCSV where
    headerOrder (undefined :: ErsteCSV) = header [
              E.encodeUtf8 "Redni broj"
            , E.encodeUtf8 "Datum valute"
            , E.encodeUtf8 "Datum izvršenja"
            , E.encodeUtf8 "Opis plaćanja, tečaj"
            , E.encodeUtf8 "Broj računa platitelja"
            , E.encodeUtf8 "Isplate"
            , E.encodeUtf8 "Uplate"
            , E.encodeUtf8 "Stanje"
            , E.encodeUtf8 "PNB platitelja"
            , E.encodeUtf8 "PNB primatelja"
            , E.encodeUtf8 "Platitelj/primatelj"
            , E.encodeUtf8 "Mjesto"
            , E.encodeUtf8 "Referenca plaćanja"
            ]


