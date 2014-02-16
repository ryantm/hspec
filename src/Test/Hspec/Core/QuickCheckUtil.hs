module Test.Hspec.Core.QuickCheckUtil where

import           Control.Applicative
import           Control.Exception
import           Data.Int
import           Data.IORef
import           Test.QuickCheck hiding (Result(..))
import           Test.QuickCheck as QC
import           Test.QuickCheck.Property hiding (Result(..))
import           Test.QuickCheck.Gen
import qualified Test.QuickCheck.Property as QCP
import           Test.QuickCheck.IO ()
import           Test.QuickCheck.Random
import           System.Random

aroundProperty :: ((a -> IO ()) -> IO ()) -> (a -> Property) -> Property
aroundProperty action p = MkProperty . MkGen $ \r n -> aroundProp action $ \a -> (unGen . unProperty $ p a) r n

aroundProp :: ((a -> IO ()) -> IO ()) -> (a -> Prop) -> Prop
aroundProp action p = MkProp $ aroundRose action (\a -> unProp $ p a)

aroundRose :: ((a -> IO ()) -> IO ()) -> (a -> Rose QCP.Result) -> Rose QCP.Result
aroundRose action r = ioRose $ do
  ref <- newIORef (return QCP.succeeded)
  action $ \a -> reduceRose (r a) >>= writeIORef ref
  readIORef ref

isUserInterrupt :: QC.Result -> Bool
isUserInterrupt r = case r of
  QC.Failure {theException = me} -> (me >>= fromException) == Just UserInterrupt
  _ -> False

newSeed :: IO Int
newSeed = fromIntegral <$> (fst . randomR (0, maxBound) <$> newQCGen :: IO Int32)
