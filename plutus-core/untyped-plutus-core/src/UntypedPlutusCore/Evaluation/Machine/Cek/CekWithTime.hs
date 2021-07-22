module UntypedPlutusCore.Evaluation.Machine.Cek.CekWithTime where

import           UntypedPlutusCore.Evaluation.Machine.Cek.Internal

import           Data.Time.Clock                                   (getCurrentTime)
import           System.IO.Unsafe                                  (unsafePerformIO)

emitCekWithTime :: GivenCekEmitter s => String -> CekM uni fun s ()
emitCekWithTime str =
    let withTime =
            "[" ++
            (show $ unsafePerformIO getCurrentTime) ++
            "]" ++ str
    in emitCek withTime
