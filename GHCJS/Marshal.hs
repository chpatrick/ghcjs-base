{-# LANGUAGE DefaultSignatures,
             TypeOperators,
             ScopedTypeVariables,
             DefaultSignatures,
             FlexibleContexts,
             FlexibleInstances,
             OverloadedStrings,
             TupleSections,
             MagicHash,
             CPP,
             JavaScriptFFI,
             ForeignFunctionInterface,
             UnliftedFFITypes,
             BangPatterns
  #-}

module GHCJS.Marshal ( FromJSVal(..)
                     , ToJSVal(..)
                     , toJSVal_pure
                     ) where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)

import           Data.Attoparsec.Number (Number(..))
import           Data.Bits ((.&.))
import           Data.Char (chr, ord)
import qualified Data.HashMap.Strict as H
import           Data.Int (Int8, Int16, Int32)
import qualified Data.JSString as JSS
import qualified Data.JSString.Text as JSS
import           Data.Maybe
import           Data.Scientific (Scientific, scientific, fromFloatDigits)
import           Data.Text (Text)
import qualified Data.Vector as V
import           Data.Word (Word8, Word16, Word32, Word)
import           Data.Primitive.ByteArray

import           Unsafe.Coerce (unsafeCoerce)

import           GHC.Int
import           GHC.Word
import           GHC.Types
import           GHC.Float
import           GHC.Prim
import           GHC.Generics

import           GHCJS.Types
import           GHCJS.Foreign.Internal
import           GHCJS.Marshal.Pure


import qualified JavaScript.Array           as A
import qualified JavaScript.Array.Internal  as AI
import qualified JavaScript.Object          as O
import qualified JavaScript.Object.Internal as OI

import           GHCJS.Marshal.Internal

instance FromJSVal JSVal where
  fromJSValUnchecked x = return x
  {-# INLINE fromJSValUnchecked #-}
  fromJSVal = return . Just
  {-# INLINE fromJSVal #-}
instance FromJSVal () where
  fromJSValUnchecked = fromJSValUnchecked_pure
  {-# INLINE fromJSValUnchecked #-}
  fromJSVal = fromJSVal_pure
--    {-# INLINE fromJSVal #-}
instance FromJSVal a => FromJSVal [a] where
    fromJSVal = fromJSValListOf
    {-# INLINE fromJSVal #-}
instance FromJSVal a => FromJSVal (Maybe a) where
    fromJSValUnchecked x | isUndefined x || isNull x = return Nothing
                         | otherwise = fromJSVal x
    {-# INLINE fromJSValUnchecked #-}
    fromJSVal x | isUndefined x || isNull x = return (Just Nothing)
                | otherwise = fmap (fmap Just) fromJSVal x
    {-# INLINE fromJSVal #-}
instance FromJSVal JSString where
    fromJSValUnchecked = fromJSValUnchecked_pure
    {-# INLINE fromJSValUnchecked #-}
    fromJSVal = fromJSVal_pure
    {-# INLINE fromJSVal #-}
instance FromJSVal Text where
    fromJSValUnchecked = fromJSValUnchecked_pure
    {-# INLINE fromJSValUnchecked #-}
    fromJSVal = fromJSVal_pure
    {-# INLINE fromJSVal #-}
instance FromJSVal Char where
    fromJSValUnchecked = fromJSValUnchecked_pure
    {-# INLINE fromJSValUnchecked #-}
    fromJSVal = fromJSVal_pure
    {-# INLINE fromJSVal #-}
    fromJSValUncheckedListOf = fromJSValUnchecked_pure
    {-# INLINE fromJSValListOf #-}
    fromJSValListOf = fromJSVal_pure
    {-# INLINE fromJSValUncheckedListOf #-}
instance FromJSVal Bool where
    fromJSValUnchecked = fromJSValUnchecked_pure
    {-# INLINE fromJSValUnchecked #-}
    fromJSVal = fromJSVal_pure
    {-# INLINE fromJSVal #-}
instance FromJSVal Int where
    fromJSValUnchecked = fromJSValUnchecked_pure
    {-# INLINE fromJSValUnchecked #-}
    fromJSVal = fromJSVal_pure
    {-# INLINE fromJSVal #-}
instance FromJSVal Int8 where
    fromJSValUnchecked = fromJSValUnchecked_pure
    {-# INLINE fromJSValUnchecked #-}
    fromJSVal = fromJSVal_pure
    {-# INLINE fromJSVal #-}
instance FromJSVal Int16 where
    fromJSValUnchecked = fromJSValUnchecked_pure
    {-# INLINE fromJSValUnchecked #-}
    fromJSVal = fromJSVal_pure
    {-# INLINE fromJSVal #-}
instance FromJSVal Int32 where
    fromJSValUnchecked = fromJSValUnchecked_pure
    {-# INLINE fromJSValUnchecked #-}
    fromJSVal = fromJSVal_pure
    {-# INLINE fromJSVal #-}
instance FromJSVal Word where
    fromJSValUnchecked = fromJSValUnchecked_pure
    {-# INLINE fromJSValUnchecked #-}
    fromJSVal = fromJSVal_pure
    {-# INLINE fromJSVal #-}
instance FromJSVal Word8 where
    fromJSValUnchecked = fromJSValUnchecked_pure
    {-# INLINE fromJSValUnchecked #-}
    fromJSVal = fromJSVal_pure
    {-# INLINE fromJSVal #-}
instance FromJSVal Word16 where
    fromJSValUnchecked = fromJSValUnchecked_pure
    {-# INLINE fromJSValUnchecked #-}
    fromJSVal = fromJSVal_pure
    {-# INLINE fromJSVal #-}
instance FromJSVal Word32 where
    fromJSValUnchecked = fromJSValUnchecked_pure
    {-# INLINE fromJSValUnchecked #-}
    fromJSVal = fromJSVal_pure
    {-# INLINE fromJSVal #-}
instance FromJSVal Float where
    fromJSValUnchecked = fromJSValUnchecked_pure
    {-# INLINE fromJSValUnchecked #-}
    fromJSVal = fromJSVal_pure
    {-# INLINE fromJSVal #-}
instance FromJSVal Double where
    fromJSValUnchecked = fromJSValUnchecked_pure
    {-# INLINE fromJSValUnchecked #-}
    fromJSVal = fromJSVal_pure
    {-# INLINE fromJSVal #-}
instance (FromJSVal a, FromJSVal b) => FromJSVal (a,b) where
    fromJSVal r = runMaybeT $ (,) <$> jf r 0 <*> jf r 1
    {-# INLINE fromJSVal #-}
instance (FromJSVal a, FromJSVal b, FromJSVal c) => FromJSVal (a,b,c) where
    fromJSVal r = runMaybeT $ (,,) <$> jf r 0 <*> jf r 1 <*> jf r 2
    {-# INLINE fromJSVal #-}
instance (FromJSVal a, FromJSVal b, FromJSVal c, FromJSVal d) => FromJSVal (a,b,c,d) where
    fromJSVal r = runMaybeT $ (,,,) <$> jf r 0 <*> jf r 1 <*> jf r 2 <*> jf r 3
    {-# INLINE fromJSVal #-}
instance (FromJSVal a, FromJSVal b, FromJSVal c, FromJSVal d, FromJSVal e) => FromJSVal (a,b,c,d,e) where
    fromJSVal r = runMaybeT $ (,,,,) <$> jf r 0 <*> jf r 1 <*> jf r 2 <*> jf r 3 <*> jf r 4
    {-# INLINE fromJSVal #-}
instance (FromJSVal a, FromJSVal b, FromJSVal c, FromJSVal d, FromJSVal e, FromJSVal f) => FromJSVal (a,b,c,d,e,f) where
    fromJSVal r = runMaybeT $ (,,,,,) <$> jf r 0 <*> jf r 1 <*> jf r 2 <*> jf r 3 <*> jf r 4 <*> jf r 5
    {-# INLINE fromJSVal #-}
instance (FromJSVal a, FromJSVal b, FromJSVal c, FromJSVal d, FromJSVal e, FromJSVal f, FromJSVal g) => FromJSVal (a,b,c,d,e,f,g) where
    fromJSVal r = runMaybeT $ (,,,,,,) <$> jf r 0 <*> jf r 1 <*> jf r 2 <*> jf r 3 <*> jf r 4 <*> jf r 5 <*> jf r 6
    {-# INLINE fromJSVal #-}
instance (FromJSVal a, FromJSVal b, FromJSVal c, FromJSVal d, FromJSVal e, FromJSVal f, FromJSVal g, FromJSVal h) => FromJSVal (a,b,c,d,e,f,g,h) where
    fromJSVal r = runMaybeT $ (,,,,,,,) <$> jf r 0 <*> jf r 1 <*> jf r 2 <*> jf r 3 <*> jf r 4 <*> jf r 5 <*> jf r 6 <*> jf r 7
    {-# INLINE fromJSVal #-}

jf :: FromJSVal a => JSVal -> Int -> MaybeT IO a
jf r n = MaybeT $ do
  r' <- AI.read n (AI.SomeJSArray r)
  if isUndefined r
    then return Nothing
    else fromJSVal r'

instance ToJSVal JSVal where
  toJSVal = toJSVal_pure
  {-# INLINE toJSVal #-}
instance ToJSVal JSString where
    toJSVal = toJSVal_pure
    {-# INLINE toJSVal #-}
instance ToJSVal Text where
    toJSVal = toJSVal_pure
    {-# INLINE toJSVal #-}
instance ToJSVal Char where
    toJSVal = return . pToJSVal
    {-# INLINE toJSVal #-}
    toJSValListOf = return . pToJSVal
    {-# INLINE toJSValListOf #-}
instance ToJSVal Bool where
    toJSVal = toJSVal_pure
    {-# INLINE toJSVal #-}
instance ToJSVal Int where
    toJSVal = toJSVal_pure
    {-# INLINE toJSVal #-}
instance ToJSVal Int8 where
    toJSVal = toJSVal_pure
    {-# INLINE toJSVal #-}
instance ToJSVal Int16 where
    toJSVal = toJSVal_pure
    {-# INLINE toJSVal #-}
instance ToJSVal Int32 where
    toJSVal = toJSVal_pure
    {-# INLINE toJSVal #-}
instance ToJSVal Word where
    toJSVal = toJSVal_pure
    {-# INLINE toJSVal #-}
instance ToJSVal Word8 where
    toJSVal = toJSVal_pure
    {-# INLINE toJSVal #-}
instance ToJSVal Word16 where
    toJSVal = toJSVal_pure
    {-# INLINE toJSVal #-}
instance ToJSVal Word32 where
    toJSVal = toJSVal_pure
    {-# INLINE toJSVal #-}
instance ToJSVal Float where
    toJSVal = toJSVal_pure
    {-# INLINE toJSVal #-}
instance ToJSVal Double where
    toJSVal = toJSVal_pure
    {-# INLINE toJSVal #-}
instance ToJSVal a => ToJSVal [a] where
    toJSVal = toJSValListOf
    {-# INLINE toJSVal #-}
instance ToJSVal a => ToJSVal (Maybe a) where
    toJSVal Nothing  = return jsNull
    toJSVal (Just a) = toJSVal a
    {-# INLINE toJSVal #-}
instance (ToJSVal a, ToJSVal b) => ToJSVal (a,b) where
    toJSVal (a,b) = join $ arr2 <$> toJSVal a <*> toJSVal b
    {-# INLINE toJSVal #-}
instance (ToJSVal a, ToJSVal b, ToJSVal c) => ToJSVal (a,b,c) where
    toJSVal (a,b,c) = join $ arr3 <$> toJSVal a <*> toJSVal b <*> toJSVal c
    {-# INLINE toJSVal #-}
instance (ToJSVal a, ToJSVal b, ToJSVal c, ToJSVal d) => ToJSVal (a,b,c,d) where
    toJSVal (a,b,c,d) = join $ arr4 <$> toJSVal a <*> toJSVal b <*> toJSVal c <*> toJSVal d
    {-# INLINE toJSVal #-}
instance (ToJSVal a, ToJSVal b, ToJSVal c, ToJSVal d, ToJSVal e) => ToJSVal (a,b,c,d,e) where
    toJSVal (a,b,c,d,e) = join $ arr5 <$> toJSVal a <*> toJSVal b <*> toJSVal c <*> toJSVal d <*> toJSVal e
    {-# INLINE toJSVal #-}
instance (ToJSVal a, ToJSVal b, ToJSVal c, ToJSVal d, ToJSVal e, ToJSVal f) => ToJSVal (a,b,c,d,e,f) where
    toJSVal (a,b,c,d,e,f) = join $ arr6 <$> toJSVal a <*> toJSVal b <*> toJSVal c <*> toJSVal d <*> toJSVal e <*> toJSVal f
    {-# INLINE toJSVal #-}
instance (ToJSVal a, ToJSVal b, ToJSVal c, ToJSVal d, ToJSVal e, ToJSVal f, ToJSVal g) => ToJSVal (a,b,c,d,e,f,g) where
    toJSVal (a,b,c,d,e,f,g) = join $ arr7 <$> toJSVal a <*> toJSVal b <*> toJSVal c <*> toJSVal d <*> toJSVal e <*> toJSVal f <*> toJSVal g
    {-# INLINE toJSVal #-}

foreign import javascript unsafe "[$1]"                   arr1     :: JSVal -> IO JSVal
foreign import javascript unsafe "[$1,$2]"                arr2     :: JSVal -> JSVal -> IO JSVal
foreign import javascript unsafe "[$1,$2,$3]"             arr3     :: JSVal -> JSVal -> JSVal -> IO JSVal
foreign import javascript unsafe "[$1,$2,$3,$4]"          arr4     :: JSVal -> JSVal -> JSVal -> JSVal -> IO JSVal
foreign import javascript unsafe "[$1,$2,$3,$4,$5]"       arr5     :: JSVal -> JSVal -> JSVal -> JSVal -> JSVal -> IO JSVal
foreign import javascript unsafe "[$1,$2,$3,$4,$5,$6]"    arr6     :: JSVal -> JSVal -> JSVal -> JSVal -> JSVal -> JSVal -> IO JSVal
foreign import javascript unsafe "[$1,$2,$3,$4,$5,$6,$7]" arr7     :: JSVal -> JSVal -> JSVal -> JSVal -> JSVal -> JSVal -> JSVal -> IO JSVal

