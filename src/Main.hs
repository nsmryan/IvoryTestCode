{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
import Ivory.Language
import Ivory.Language.Cast
import Ivory.Language.Ref
import Ivory.Stdlib.Operators
import Ivory.Stdlib.Control
import Ivory.BitData
import qualified Ivory.Compile.C.CmdlineFrontend as C (compile)

import Control.Monad hiding (when)

import Types


periodHz = 50
periodTicks = 20

ivorytest_module :: Module
ivorytest_module = package "IvoryTest" $ do
  inclHeader "test.h"
  incl loop
  incl setup
  incl shift16
  incl allLEDs
  incl updateTime
  incl pwm
  defStruct (Proxy :: Proxy "SysTime")
  defMemArea sysTime

--int analogRead(uint8_t);
--void analogReference(uint8_t mode);
--void analogWrite(uint8_t, int);
--int digitalRead(uint8_t);

--unsigned long millis(void);
--unsigned long micros(void);
--void delayMicroseconds(unsigned int us);
--unsigned long pulseIn(uint8_t pin, uint8_t state, unsigned long timeout);
--
--void shiftOut(uint8_t dataPin, uint8_t clockPin, uint8_t bitOrder, uint8_t val);
shiftOut :: Def ('[Uint8, Uint8, Uint8, Uint8] :-> ())
shiftOut = importProc "shiftOut" ""

--uint8_t shiftIn(uint8_t dataPin, uint8_t clockPin, uint8_t bitOrder);

--void attachInterrupt(uint8_t, void (*)(void), int mode);
--void detachInterrupt(uint8_t);

--void pinMode(uint8_t, uint8_t);
pinMode :: Def ('[Uint8, Uint8] :-> ())
pinMode = importProc "pinMode" ""

--void digitalWrite(uint8_t, uint8_t);
digitalWrite :: Def ('[Uint8, Uint8] :-> ())
digitalWrite = importProc "digitalWrite" ""


--void delay(unsigned long);
delay :: Def ('[Uint32] :-> ())
delay = importProc "delay" ""


inputPin = 0
outputPin = 1
ledBuiltin = 13
low = 0
high = 1

msbFirst = 1
lsbFirst = 0

--use IO area and bit data definition
oe = 6
mr = 5
storclk = 4
srclk = 3
serpin = 2


setup :: Def ('[] :-> ())
setup = proc "setup" $ body $ do
  call_ pinMode ledBuiltin outputPin
  call_ pinMode oe outputPin
  call_ pinMode srclk outputPin
  call_ pinMode storclk outputPin
  call_ pinMode mr outputPin
  call_ pinMode serpin outputPin

  call_ digitalWrite mr low
  call_ digitalWrite mr high

  retVoid


shift16 :: Def ('[Uint16] :-> ())
shift16 = proc "shift16" $ \ v -> body $ do
  call_ digitalWrite srclk low
  call_ digitalWrite storclk low

  call_ shiftOut serpin srclk msbFirst $ ubits v
  call_ shiftOut serpin srclk msbFirst $ lbits v

  call_ digitalWrite oe high
  call_ digitalWrite storclk high
  call_ digitalWrite storclk low
  call_ digitalWrite oe low

sysTime :: MemArea (Struct "SysTime")
sysTime = area "sysTime" Nothing

pwm :: Def ('[Uint16] :-> IBool)
pwm = proc "pwm" $ \ pwm -> body $ do
  tickCount <- deref (addrOf sysTime ~> ticks)
  let periodTicks =  tickCount .% periodTicks
  onOff <- local (ival (periodTicks ==? pwm))
  ret =<< deref onOff

allLEDs :: Def ('[Uint8] :-> Uint16)
allLEDs = proc "allLEDs" $ \ led -> body $ do
  outBits <- local (ival 0)
  (5 :: Ix 5) `times` \ ix -> do
    bits <- deref outBits
    store outBits $ (bits `iShiftL` 3) .| (safeCast led)
  ret =<< deref outBits

updateTime :: Def ('[Ref s (Struct "SysTime")] :-> ())
updateTime = proc "updateTime" $ \ time -> body $ do
  (addrOf sysTime ~> ticks) += 1

  currentTicks <- deref (addrOf sysTime ~> ticks)
  when (currentTicks ==? periodTicks) $ do
       (addrOf sysTime ~> ticks) `store` 0
       (addrOf sysTime ~> periods) += 1

  currentPeriod <- deref (addrOf sysTime ~> periods)
  when (currentPeriod ==? periodHz) $ do
       (addrOf sysTime ~> periods) `store` 0
       (addrOf sysTime ~> seconds) += 1
loop :: Def ('[] :-> ())
loop = proc "loop" $ body $ do

  currentTicks <- deref (addrOf sysTime ~> ticks)
  let outBits = (currentTicks .% 2)
  output <- call allLEDs (ivoryCast outBits)
  call_ shift16 output

  call_ updateTime (addrOf sysTime)

  call_ delay 1


  retVoid
main :: IO ()
main = C.compile [ ivorytest_module ]

