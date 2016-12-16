module Analyze.Csv where

import Analyze.RFrame (RFrame(..), RFrameUpdate(..), fromUpdate)
import Control.Monad.Catch (Exception, MonadThrow (..))
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Csv as C
import Data.Text (Text)
import Data.Typeable (Typeable)

data CsvError = CsvError String deriving (Eq, Show, Typeable)
instance Exception CsvError

readWithHeader :: MonadThrow m => LBS.ByteString -> m (RFrame Text Text)
readWithHeader = undefined

readWithoutHeader :: MonadThrow m => LBS.ByteString -> m (RFrame Int Text)
readWithoutHeader = undefined
