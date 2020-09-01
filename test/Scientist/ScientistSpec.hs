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
  , _ftFunction :: Fun a (m b)
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
        forAll (arbitrary `suchThat` \(probability, _, _, funTrial) -> probability >= _ftChanche funTrial) $
          \(probability, input :: String, controlOutput :: String, funTrial) ->
            runTrial probability input controlOutput (toTrial funTrial)
              `shouldBe` Identity Nothing

      it "does run the trial if the probability is lower than the chance and does not detect a discrepancy" $ property $
        forAll (arbitrary `suchThat` \(probability, _, funTrial) -> probability < _ftChanche funTrial) $
          \(probability, input :: String, funTrial :: FunTrial Identity String String) ->
            runTrial probability input (runIdentity $ applyFun (_ftFunction funTrial) input) (toTrial funTrial)
              `shouldBe` Identity Nothing

      it "does run the trial if the probability is lower than the chance and does detect a discrepancy" $ property $
        forAll (arbitrary `suchThat` \(probability, input, controlOutput, funTrial) -> probability < _ftChanche funTrial && (runIdentity $ applyFun (_ftFunction funTrial) input) /= controlOutput) $
          \(probability, input :: String, controlOutput, funTrial :: FunTrial Identity String String) ->
            runTrial probability input controlOutput (toTrial funTrial)
              `shouldBe` Identity (Just $ Discrepancy (_ftName funTrial) input controlOutput (runIdentity $ applyFun (_ftFunction funTrial) input))

    describe "runExperiment" $ do

      it "doesn't run the experiment if the randomInt is 100" $ property $
        forAll arbitrary $
          \(funTrial :: FunTrial Identity String String, trials, chance, input) ->
            runExperiment 100 (Experiment "" (_tOperation $ toTrial funTrial) (toTrial <$> trials) chance) input
              `shouldBe` Identity (runIdentity $ applyFun (_ftFunction funTrial) input, ExperimentNotRun)

      it "does run the experiment if the randomInt is 0" $ property $
        forAll (arbitrary `suchThat` \(_, _, chance, _) -> chance > 0) $
          \(funTrial :: FunTrial Identity String String, trials, chance, input) ->
            (snd . runIdentity $ runExperiment 0 (Experiment "" (_tOperation $ toTrial funTrial) (toTrial <$> trials) chance) input)
              `shouldNotBe` ExperimentNotRun

      it "does not return discrepancies if the trials return the same value as the control operation" $ property $
        forAll (arbitrary `suchThat` \(_, chance, _) -> chance > 0) $
          \(funTrial :: FunTrial Identity String String, chance, input :: String) ->
            runExperiment 0 (Experiment "" (_tOperation $ toTrial funTrial) [toTrial funTrial, toTrial funTrial] chance) input
              `shouldBe` Identity (runIdentity $ applyFun (_ftFunction funTrial) input, Discrepancies [])
