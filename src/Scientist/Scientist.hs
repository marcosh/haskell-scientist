module Scientist.Scientist where

-- base
import Data.Maybe (catMaybes)

-- | An 'Operation' is just a newtype around an effectful function from 'a' to 'b'
newtype Operation m a b = Operation (a -> m b)

-- | A 'Trial' is an operation in an experiment which has a specific chance of being executed
data Trial m a b = Trial
  -- | the name of the 'Trail' operation
  { _tName      :: String
  -- | the trial operation
  , _tOperation :: Operation m a b
  -- | the chance that the trial operation is going to be executed inside an experiment
  , _tChanche   :: Int
  }

-- | An 'Experiment' describes a set of 'Operation's which need to be performed, measured and compared
data Experiment m a b = Experiment
  -- | the name of the 'Experiment'
  { _eName    :: String
  -- | the control 'Operation' of the experiment
  , _eControl :: Operation m a b
  -- | the trial 'Operation's of the experiment
  , _eTrials  :: [Trial m a b]
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
  deriving (Eq, Show)

-- | The reported data of the results of an 'Experiment'
data ExperimentResult a b
  -- | experiment was not actually run
  = ExperimentNotRun
  -- | some 'Discrepancy's were detected
  | Discrepancies [Discrepancy a b]

-- | Run a single 'Trial'.
-- Returns `Nothing` if the chance of the 'Trial' is less than the probability and the 'Trial' is not actually run
-- or if the result of the 'Trial' is equal to 'controlOutput'
runTrial :: (Eq b, Monad m) => Int -> a -> b -> Trial m a b -> m (Maybe (Discrepancy a b))
runTrial probability input controlOutput (Trial name (Operation f) chance) =
  if probability <= chance
  then do
    trialResult <- f input
    if trialResult == controlOutput
    then pure Nothing
    else pure . Just $ Discrepancy name input controlOutput trialResult
  else pure Nothing

-- | run the 'Experiment'
runExperiment :: Monad m => Eq b => Int -> Experiment m a b -> a -> m (b, ExperimentResult a b)
runExperiment probability (Experiment _ (Operation control) trials chance) a = do
  controlOutput <- control a
  experimentResult <-
        if probability <= chance
        then do
          trialDiscrepancies <- sequence $ runTrial (div (probability * chance) 100) a controlOutput <$> trials
          pure . Discrepancies $ catMaybes trialDiscrepancies
        else
          pure ExperimentNotRun
  pure (controlOutput, experimentResult)
