-- | The public facade. Import this and you get the friendly session API, plus
-- the lower-level packet primitives for talking to the access point directly.
module Spotify
  ( -- * High-level sessions
    module Spotify.Session
    -- * Lower-level access
  , Connection
  , sendPacket
  , recvPacket
  , resolveAccessPoint
  ) where

import           Spotify.Connection   (Connection, recvPacket, sendPacket)
import           Spotify.Net.ApResolve (resolveAccessPoint)
import           Spotify.Session
