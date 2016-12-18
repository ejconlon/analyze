module Analyze.Csv where

import           Analyze.Conversions  (projectRows)
import           Analyze.RFrame       (RFrame (..), RFrameUpdate (..), empty, fromUpdate)
import           Control.Monad.Catch  (Exception, MonadThrow (..))
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Csv             as C
import           Data.Text            (Text)
import           Data.Text.Encoding   (decodeUtf8)
import           Data.Typeable        (Typeable)
import qualified Data.Vector          as V

data CsvError = CsvError String deriving (Eq, Show, Typeable)
instance Exception CsvError

decodeWithHeader :: MonadThrow m => LBS.ByteString -> m (RFrame Text Text)
decodeWithHeader bs =
  case C.decodeByName bs of
    Left err -> throwM (CsvError err)
    Right (header, rows) -> do
      let ks = decodeUtf8 <$> header
      projectRows ks rows

decodeWithoutHeader :: MonadThrow m => LBS.ByteString -> m (RFrame Int Text)
decodeWithoutHeader bs =
  case C.decode C.NoHeader bs of
    Left err -> throwM (CsvError err)
    Right rows ->
      if V.null rows
        then return empty
        else do
          let ks = V.imap (\i _ -> i) (V.head rows)
              update = RFrameUpdate ks rows
          fromUpdate update
