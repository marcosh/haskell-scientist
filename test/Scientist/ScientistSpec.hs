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
              `shouldBe` Identity Nothing

      it "does run the trial if the probability is lower or equal than the chance and does not detect a discrepancy" $ property $
        forAll (arbitrary `suchThat` \(probability, chance, _, _) -> probability <= chance) $
          \(probability, chance, input :: String, trialName) ->
            runTrial probability input (reverse input) (Trial trialName (Operation (pure . reverse :: String -> Identity String)) chance)
              `shouldBe` Identity Nothing

      it "does run the trial if the probability is lower or equal than the chance and does detect a discrepancy" $ property $
        forAll (arbitrary `suchThat` \(probability, chance, input, controlOutput, _) -> probability <= chance && input /= reverse controlOutput) $
          \(probability, chance, input :: String, controlOutput, trialName) ->
            runTrial probability input controlOutput (Trial trialName (Operation (pure . reverse :: String -> Identity String)) chance)
              `shouldBe` Identity (Just $ Discrepancy trialName input controlOutput (reverse input))
