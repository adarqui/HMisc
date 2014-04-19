module HMisc.Time (
	toCT,
	getTime,
	diffTimes,
	withinRange) where

import Control.Monad
import System.Time
import Data.Time
import System.Posix.Types
import System.Posix.Time


toCT :: EpochTime -> ClockTime
toCT et = TOD (truncate (toRational et)) 0


getTime :: IO ClockTime
getTime = liftM toCT epochTime


diffTimes :: ClockTime -> ClockTime -> Int
diffTimes a b = tdSec $ diffClockTimes a b


withinRange :: ClockTime -> ClockTime -> Int -> Int -> Bool
withinRange a b c d = d <= (abs e)
	where e = diffTimes a b
