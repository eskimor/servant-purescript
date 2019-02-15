-- | A header which gets sent by the browser and is thus of no concern for the client consumer of the API.
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances          #-}

module Servant.API.BrowserHeader where

import Servant.Utils.Links
import Servant
import Servant.Foreign
import Servant.Subscriber.Subscribable
import           GHC.TypeLits

data BrowserHeader (sym :: Symbol) a



type instance IsElem' e (BrowserHeader :> s) = IsElem e s

instance HasLink sub => HasLink (BrowserHeader sym a :> sub) where
#if MIN_VERSION_servant(0,14,0)
  type MkLink (BrowserHeader sym a :> sub) b = MkLink (Header sym a :> sub) b
  toLink toA Proxy = toLink toA (Proxy :: Proxy (Header sym a :> sub))
#else
  type MkLink (BrowserHeader sym a :> sub) = MkLink (Header sym a :> sub)
  toLink _ = toLink (Proxy :: Proxy (Header sym a :> sub))
#endif

instance (KnownSymbol sym, FromHttpApiData a, HasServer sublayout context)
      => HasServer (BrowserHeader sym a :> sublayout) context where

  type ServerT (BrowserHeader sym a :> sublayout) m = ServerT (Header sym a :> sublayout) m

  route Proxy = route (Proxy :: Proxy (Header sym a :> sublayout))


-- Ignore BrowserHeader in HasForeign:
instance (KnownSymbol sym, HasForeign lang ftype sublayout)
  => HasForeign lang ftype (BrowserHeader sym a :> sublayout) where
  type Foreign ftype (BrowserHeader sym a :> sublayout) = Foreign ftype sublayout

  foreignFor lang p Proxy = foreignFor lang p (Proxy :: Proxy sublayout)

type instance IsSubscribable' endpoint (BrowserHeader sym a :> sub ) = IsSubscribable endpoint sub
