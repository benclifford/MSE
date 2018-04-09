{-# Language OverloadedStrings #-}

-- | This is a form giving a yes/no radio control and if the radio
--   is set to yes, then there will be optional text to be entered.
--   UI components should hide/empty the optional text box when the
--   yes/no is set to no.
module OptionalTextForm where

import Data.Monoid ( (<>) )
import qualified Data.Text as T

import qualified Text.Blaze.Html5 as B
import Text.Blaze.Html5 ( (!) )
import qualified Text.Blaze.Html5.Attributes as BA

import qualified Text.Digestive as DF
import Text.Digestive ( (.:) )

import Text.Digestive.Blaze.Html5 as DB

import Lib (escapeDots)

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
innerOptionalTextForm def = 
  let (discloseDef, stringDef) =
        case def of
          Nothing -> (Nothing, Nothing)
          Just "" -> (Just False, Nothing)
          Just s -> (Just True, Just s)
      in OptionalTextValue
           <$> "disclose" .: DF.choiceWith [("Y", (True, "yes")), ("N", (False, "no"))] discloseDef
           <*> "declaration" .: DF.string stringDef

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

optionalTextInputAreaParagraph editable fieldName parentView description = 
  if editable
    then do B.p $ do
                DB.label "disclose" view description
            B.p $ do
                "Do you wish to disclose information here?"
                ": "
                DB.inputRadio False "disclose" view
                DB.errorList "disclose" view
            B.p $ do
                (DB.inputTextArea (Just 8) (Just 80) "declaration" view) ! BA.placeholder "Please enter information here"
                DB.errorList "declaration" view
                let ar = T.unpack $ escapeDots $ DF.absoluteRef "disclose" view
                let ar_t = T.unpack $ escapeDots $ DF.absoluteRef "declaration" view
                B.script $ B.toHtml $
                  " $(\"input[type=radio][name=" ++ ar ++ "]\").change( \
                  \ function () { \
                  \   var s = $(this).val(); \
                  \   var t = $(\"#" ++ ar_t ++ "\"); \
                  \   if (s == 'Y') { \
                  \     t.show(); \
                  \   } else { \
                  \     t.hide(); \
                  \   } \
                  \ } \
                  \ ); \
                  \ $(\"input[type=radio][name="
                  ++ ar ++ "]\").trigger(\"change\");"

    else do B.p $ do
                DB.label "declaration" view description
                ": "
                DB.errorList "declaration" view
                B.p $ B.toHtml $ DF.fieldInputText "declaration" view
  where view = DF.subView fieldName parentView

