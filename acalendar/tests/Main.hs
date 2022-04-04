{-# LANGUAGE OverloadedStrings #-}

import Data.Text ()
import Data.Time.Calendar as Calendar
import DateSpec
import Parser
import Test.Hspec
import qualified Text.Parsec as Parsec
import Weekday as Event

main :: IO ()
main = hspec $ do
  describe "satisfies" $ do
    it "weekdays and day" $ do
      let dateSpec =
            DateSpec.empty
              { days = [28],
                months = [10],
                years = [1990],
                weekdays = [Event.Mon, Event.Tue]
              }
      DateSpec.satisfies (Calendar.fromGregorian 1990 10 28) dateSpec `shouldBe` False
      DateSpec.satisfies (Calendar.fromGregorian 1990 10 29) dateSpec `shouldBe` True
      DateSpec.satisfies (Calendar.fromGregorian 1990 10 30) dateSpec `shouldBe` False
  describe "dateSpec" $ do
    it "2. empty" $ do
      parseDateSpec "" `shouldBe` Right DateSpec.empty
    it "2. day only" $ do
      parseDateSpec "1" `shouldBe` Right (DateSpec.empty {days = [1]})
      parseDateSpec "31" `shouldBe` Right (DateSpec.empty {days = [31]})
    it "3. month" $ do
      parseDateSpec "Feb" `shouldBe` Right (DateSpec.empty {months = [2]})
    it "4. day and month" $ do
      parseDateSpec "6 Jan" `shouldBe` Right (DateSpec.empty {days = [6], months = [1]})
      parseDateSpec "29 Feb" `shouldBe` Right (DateSpec.empty {days = [29], months = [2]})
    it "5. year" $ do
      parseDateSpec "1991" `shouldBe` Right (DateSpec.empty {years = [1991]})
    it "6. year and day" $ do
      parseDateSpec "1 1990" `shouldBe` Right (DateSpec.empty {days = [1], years = [1990]})
      parseDateSpec "23 1992" `shouldBe` Right (DateSpec.empty {days = [23], years = [1992]})
    it "7. year and month" $ do
      parseDateSpec "Feb 1991" `shouldBe` Right (DateSpec.empty {months = [2], years = [1991]})
      parseDateSpec "Sep 1992" `shouldBe` Right (DateSpec.empty {months = [9], years = [1992]})
    it "8. year, month and day" $ do
      parseDateSpec "8 Jan 1991" `shouldBe` Right (DateSpec.empty {days = [8], months = [1], years = [1991]})
      parseDateSpec "9 Mar 1992" `shouldBe` Right (DateSpec.empty {days = [9], months = [3], years = [1992]})
    it "9. weekday" $ do
      parseDateSpec "Sat" `shouldBe` Right (DateSpec.empty {weekdays = [Event.Sat]})
      parseDateSpec "Mon Tue Wed Thu Fri" `shouldBe` Right (DateSpec.empty {weekdays = [Event.Mon, Event.Tue, Event.Wed, Event.Thu, Event.Fri]})
      parseDateSpec "Mon Wed" `shouldBe` Right (DateSpec.empty {weekdays = [Event.Mon, Event.Wed]})
    it "10. weekday and day" $ do
      parseDateSpec "1 Sat" `shouldBe` Right (DateSpec.empty {days = [1], weekdays = [Event.Sat]})
      parseDateSpec "15 Mon Tue Wed Thu Fri" `shouldBe` Right (DateSpec.empty {days = [15], weekdays = [Event.Mon, Event.Tue, Event.Wed, Event.Thu, Event.Fri]})
    it "11. weekday and month" $ do
      parseDateSpec "Mar Mon" `shouldBe` Right (DateSpec.empty {months = [3], weekdays = [Event.Mon]})
      parseDateSpec "Feb Mon Thu Wed Thu Fri" `shouldBe` Right (DateSpec.empty {months = [2], weekdays = [Event.Mon, Event.Thu, Event.Wed, Event.Thu, Event.Fri]})
    it "12. weekday, month and day" $ do
      parseDateSpec "1 Mar Mon" `shouldBe` Right (DateSpec.empty {days = [1], months = [3], weekdays = [Event.Mon]})
      parseDateSpec "15 Jul Sat Sun" `shouldBe` Right (DateSpec.empty {days = [15], months = [7], weekdays = [Event.Sat, Event.Sun]})
    it "13. weekday and year" $ do
      parseDateSpec "1991 Sat Sun" `shouldBe` Right (DateSpec.empty {years = [1991], weekdays = [Event.Sat, Event.Sun]})
    it "14. weekday, day and year" $ do
      parseDateSpec "15 1990 Mon" `shouldBe` Right (DateSpec.empty {days = [15], years = [1990], weekdays = [Event.Mon]})
      parseDateSpec "1 1990 Mon Tue Wed Thu Fri" `shouldBe` Right (DateSpec.empty {days = [1], years = [1990], weekdays = [Event.Mon, Event.Tue, Event.Wed, Event.Thu, Event.Fri]})
    it "15. weekday, month and year" $ do
      parseDateSpec "Feb 1991 Mon Wed" `shouldBe` Right (DateSpec.empty {months = [2], years = [1991], weekdays = [Event.Mon, Event.Wed]})
    it "15. weekday, day month and year" $ do
      parseDateSpec "28 Oct 1990 Mon Tue Wed" `shouldBe` Right (DateSpec.empty {days = [28], months = [10], years = [1990], weekdays = [Event.Mon, Event.Tue, Event.Wed]})
  where
    parseDateSpec =
      Parsec.parse (Parser.dateSpec <* Parsec.eof) ""
