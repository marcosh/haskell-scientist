{-# LANGUAGE ScopedTypeVariables #-}

module Scientist.ScientistSpec where

-- base
import Data.Functor.Identity (Identity(..))

-- hspec
import Test.Hspec

-- Quickcheck
import Test.QuickCheck

import Scientist.Scientist

data FunTrial m a b = FunTrial
  { _ftName      :: String
  -- | the trial operation
  , _ftOperation :: Fun a (m b)
  -- | the chance that the trial operation is going to be executed inside an experiment
  , _ftChanche   :: Int
  }

instance (Function a, CoArbitrary a, Arbitrary (m b)) => Arbitrary (FunTrial m a b)
  where arbitrary = FunTrial <$> arbitrary <*> arbitrary <*> (arbitrary `suchThat` (>= 0))

instance (Show a, Show (m b)) => Show (FunTrial m a b)
  where show (FunTrial name operation chance) = "FunTrial[ name = " ++ name ++ ", operation = " ++ show operation ++ ", chance = " ++ show chance ++ " ]"

toTrial:: FunTrial m a b -> Trial m a b
toTrial (FunTrial name fun chance) = Trial name (Operation . applyFun $ fun) chance

spec :: Spec
spec =
  describe "Experiment" $ do

    describe "runTrial" $ do

      it "does not run the trial if the probability is higher or equal than the chance" $ property $
        forAll (arbitrary `suchThat` \(probability, chance, _, _, _) -> probability >= chance) $
          \(probability, chance, input :: String, controlOutput, trialName) ->
            runTrial probability input controlOutput (Trial trialName (Operation (pure :: a -> Identity a)) chance)
              `shouldBe` Identity Nothing

      it "does run the trial if the probability is lower than the chance and does not detect a discrepancy" $ property $
        forAll (arbitrary `suchThat` \(probability, chance, _, _) -> probability < chance) $
          \(probability, chance, input :: String, trialName) ->
            runTrial probability input (reverse input) (Trial trialName (Operation (pure . reverse :: String -> Identity String)) chance)
              `shouldBe` Identity Nothing

      it "does run the trial if the probability is lower than the chance and does detect a discrepancy" $ property $
        forAll (arbitrary `suchThat` \(probability, chance, input, controlOutput, _) -> probability < chance && input /= reverse controlOutput) $
          \(probability, chance, input :: String, controlOutput, trialName) ->
            runTrial probability input controlOutput (Trial trialName (Operation (pure . reverse :: String -> Identity String)) chance)
              `shouldBe` Identity (Just $ Discrepancy trialName input controlOutput (reverse input))

    describe "runExperiment" $ do

      it "doesn't run the experiment if the randomInt is 100" $ property $
        forAll arbitrary $
          \(trials, chance, input) ->
            runExperiment 100 (Experiment "" (Operation (pure . reverse :: String -> Identity String)) (toTrial <$> trials) chance) input
              `shouldBe` Identity (reverse input, ExperimentNotRun)
