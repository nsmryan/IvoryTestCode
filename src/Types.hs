{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Types where

import Ivory.Language
import Ivory.BitData


{-
[bitdata|
  bitdata RGB :: Bits 3 = rgb
    { redPin :: Bit
    , bluePin  :: Bit
    , greenPin  :: Bit
    }
|]
-}
[ivory|
struct SysTime
  { seconds :: Stored Uint16
  ; periods :: Stored Uint16
  ; ticks :: Stored Uint16
  }
|]
