{-# Language OverloadedStrings #-}

-- | This is a form giving a yes/no radio control and if the radio
--   is set to yes, then there will be optional text to be entered.
--   UI components should hide/empty the optional text box when the
--   yes/no is set to no.
module OptionalTextForm where

import qualified Text.Blaze.Html5 as B
import Text.Blaze.Html5 ( (!) )
import qualified Text.Blaze.Html5.Attributes as BA

import qualified Text.Digestive as DF
import Text.Digestive ( (.:) )

import Text.Digestive.Blaze.Html5 as DB

-- A form

data OptionalTextValue = OptionalTextValue {
  disclose :: Bool,
  declaration :: String
}

-- A validator for this should be:
-- if you've chosen True then s must be non-empty
-- if you've chosen False then s must be empty.
-- validation interaction between two
innerOptionalTextForm :: Monad m => Maybe String -> DF.Form B.Html m OptionalTextValue
innerOptionalTextForm def = case def of
  Nothing ->  OptionalTextValue
    <$> "disclose" .: DF.bool Nothing
    <*> "declaration" .: DF.string Nothing
  Just "" -> OptionalTextValue
    <$> "disclose" .: DF.bool (Just False)
    <*> "declaration" .: DF.string (Just "")
  Just s -> OptionalTextValue
    <$> "disclose" .: DF.bool (Just True)
    <*> "declaration" .: DF.string (Just s)

optionalTextForm :: Monad m => Maybe String -> DF.Form B.Html m OptionalTextValue
optionalTextForm def =
  innerOptionalTextForm def

-- can I make this?
-- and if it works, then TODO: database etc this needs to change to a
-- Maybe String with NULL/Nothing representing no declaration.
optionalTextMaybeForm :: Monad m => Maybe String -> DF.Form B.Html m String
optionalTextMaybeForm def = f <$> optionalTextForm def
  where f (OptionalTextValue False _) = ""
        f (OptionalTextValue True s) = s

optionalTextInputAreaParagraph editable fieldname parentView description = 
  if editable
    then do B.p $ do
                DB.label "disclose" view description
            B.p $ do
                "Do you wish to disclose information here?"
                ": "
                DB.inputCheckbox "disclose" view
                DB.errorList "disclose" view
            B.p $ do
                DB.inputTextArea (Just 8) (Just 80) "declaration" view
                DB.errorList "declaration" view
    else do B.p $ do
                DB.label "declaration" view description
                ": "
                DB.errorList "declaration" view
                let b = DF.fieldInputBool "disclose" view
                if b
                  then B.p $ DB.inputTextArea (Just 8) (Just 80) "declaration" view
                  else B.p "Nothing disclosed"
  where view = DF.subView fieldname parentView
