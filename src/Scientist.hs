module Scientist where

import Data.Maybe (catMaybes)
import System.Random (randomRIO)

-- | An 'Operation' is just a newtype around a function from 'a' to 'b'
newtype Operation a b = Operation (a -> IO b)

-- | A 'Trial' is an operation in an experiment which has a specific change of being executed
data Trial a b = Trial
  -- | the name of the 'Trail' operation
  { _tName      :: String
  -- | the trial operation
  , _tOperation :: Operation a b
  -- | the chance that the trial operation is going to be executed inside an experiment
  , _tChanche   :: Int
  }

-- | An 'Experiment' describes a set of 'Operation's which need to be performed, measured and compared
data Experiment a b = Experiment
  -- | the name of the 'Experiment'
  { _eName    :: String
  -- | the control 'Operation' of the experiment
  , _eControl :: Operation a b
  -- | the trial 'Operation's of the experiment
  , _eTrials  :: [Trial a b]
  -- | the chanche that the 'Experiment' is going to be executed
  , _eChance  :: Int
  }

-- | A discrepancy between the control result and the trial result for a given input
data Discrepancy a b = Discrepancy
  -- | the name of the trial which generated the discrepancy
  { _dTrialName    :: String
  -- | the input value which generated the discrepancy
  , _dInput        :: a
  -- | the output value of the control 'Operation'
  , _dControlValue :: b
  -- | the output value of the 'Trial' 'Operation'
  , _dTrialValue   :: b
  }

-- | The reported data of the results of an 'Experiment'
data ExperimentResult a b
  -- | experiment was not actually run
  = ExperimentNotRun
  -- | some 'Discrepancy's were detected
  | Discrepancies [Discrepancy a b]

-- | Run a single 'Trial'.
-- Returns `Nothing` if the chance of the 'Trail' is less than the probability
runTrial :: Int -> a -> b -> Trial a b -> Maybe (IO (Discrepancy a b))
runTrial probability input controlOutput (Trial name (Operation f) chance) =
  if probability <= chance
  then do
    Just $ do
      trialResult <- f input
      pure $ Discrepancy name input controlOutput trialResult
  else Nothing

-- | run the 'Experiment'
runExperiment :: Experiment a b -> a -> IO (b, ExperimentResult a b)
runExperiment (Experiment _ (Operation control) trials chance) a = do
  probability <- randomRIO (0, 100)
  controlOutput <- control a
  experimentResult <-
        if probability <= chance
        then do
          trialDiscrepancies <- sequence . catMaybes $ runTrial (div (probability * chance) 100) a controlOutput <$> trials
          -- trialDiscrepancies <- sequence trialDiscrepanciesIO
          pure $ Discrepancies trialDiscrepancies
        else
          pure ExperimentNotRun
  pure (controlOutput, experimentResult)
