{-# LANGUAGE ScopedTypeVariables #-}

module Scientist.ScientistSpec where

-- base
import Data.Functor.Identity (Identity(..))

-- hspec
import Test.Hspec

-- Quickcheck
import Test.QuickCheck

import Scientist.Scientist

spec :: Spec
spec =
  describe "Experiment" $ do

    describe "runTrial" $ do

      it "does not run the trial if the probability is higher than the chance" $ property $
        forAll (arbitrary `suchThat` \(probability, chance, _, _, _) -> probability > chance) $
          \(probability, chance, input :: String, controlOutput, trialName) ->
            runTrial probability input controlOutput (Trial trialName (Operation (pure :: a -> Identity a)) chance)
              `shouldBe` Nothing

      it "does run the trial is the probability is lower or equal than the chance" $ property $
        forAll (arbitrary `suchThat` \(probability, chance, _, _, _) -> probability <= chance) $
          \(probability, chance, input :: String, controlOutput, trialName) ->
            runTrial probability input controlOutput (Trial trialName (Operation (pure . reverse :: String -> Identity String)) chance)
              `shouldBe` Just (Identity $ Discrepancy trialName input controlOutput (reverse input))
