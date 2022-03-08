module HW1.T1 where

import Numeric.Natural

data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
  deriving (Show, Eq)

-- | Returns the day that follows the day of the week given as input.
nextDay :: Day -> Day
nextDay Sunday = Monday
nextDay Monday = Tuesday
nextDay Tuesday = Wednesday
nextDay Wednesday = Thursday
nextDay Thursday = Friday
nextDay Friday = Saturday
nextDay Saturday = Sunday

-- | Returns the day of the week after a given number of days has passed.
afterDays :: Natural -> Day -> Day
afterDays x = afterDaysMod (mod x 7)

afterDaysMod :: Natural -> Day -> Day
afterDaysMod 0 day = day
afterDaysMod x day = afterDaysMod (x - 1) (nextDay day)

-- | Checks if the day is on the weekend.
isWeekend :: Day -> Bool
isWeekend day
  | day == Sunday || day == Saturday = True
  | otherwise = False

-- | Computes the number of days until the next Friday.
daysToParty :: Day -> Natural
daysToParty Friday = 0
daysToParty day = succ $ daysToParty $ nextDay day
