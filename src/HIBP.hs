module HIBP where

import Time (MilliSeconds(..))

-- rate limit as specified by haveibeenpwned
defaultDelay :: MilliSeconds
defaultDelay = MilliSeconds 1600
