module Scientist where

import System.Random

-- | An 'Operation' is just a newtype around a function from 'a' to 'b'
newtype Operation a b = Operation (a -> b)

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
  -- | no discrepancy was deteted
  | NoDiscrepancy
  -- | some 'Discrepancy's were detected
  | Discrepancies [Discrepancy a b]

-- | run the 'Experiment'
runExperiment :: Experiment a b -> a -> IO (b, ExperimentResult a b)
runExperiment (Experiment _ (Operation control) trials chance) a = do
  probability <- randomRIO (0, 100)
  if probability < chance
  then _actuallyRunTheExperiment
  else pure (control a, ExperimentNotRun)
