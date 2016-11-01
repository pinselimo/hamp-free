import Control.Exception (handle, finally, SomeException)
import Control.Monad (forever, forM, liftM, filterM)

import Control.Concurrent.MVar
import Control.Concurrent
import Network
import System.IO


import qualified System.Random as R (RandomGen, next, genRange)

--import Network.Socket hiding (listen)
--import Network.BSD (hostAddress, getHostByName)
import System.Timeout (timeout)
{-
main :: IO ()
main = do
    h <- openFile ReadMode
    putStrLn (show (hGetEncoding h))
                        
                        
    --finally (forever $ threadDelay (1000*2000) >> putStrLn "[+] Testing") $ do
    --    mapM killThread [t3,t4]
        
-}
refreshHandles :: MVar [Handle] -> Handle -> IO Bool
refreshHandles handleVar hand = do
        Just l <- tryTakeMVar handleVar
        list <- filterM hIsOnline l
        putStrLn $ show list
        tryPutMVar handleVar $ hand:list

hIsOnline :: Handle -> IO Bool
hIsOnline h = handle false $ hReady h >> return True
    where false :: SomeException -> IO Bool
          false _ = return False
                        
port :: PortNumber
port = fromInteger 16363


shuffle :: (R.RandomGen g, Eq a) => g -> [a] -> (g,a,[a])
shuffle g ts = (g', t, ts')
                        
                 where step   = space / (fromIntegral $ length ts)
                       space  = case R.genRange g of
                           (x, y) -> fromIntegral (y - x)
                       t   = (ts!!) $ floor (fromIntegral rv / step)
                       ts' = filter (/=t) ts
                       (rv,g') = R.next g

accu c g = accumulator c (AC 0 0 0 0 0 0 0 0 0 0) g            
 where                          
 accumulator 0 e _ = e
 accumulator i e g = accumulator i' e' g'
    where (g',x,_) = shuffle g [1..10]
          e' = case x of
                    1 -> e { one = 1 + one e }
                    2 -> e { two = 1 + two e }
                    3 -> e { three = 1 + three e }
                    4 -> e { four = 1 + four e }
                    5 -> e { five = 1 + five e }
                    6 -> e { six = 1 + six e }
                    7 -> e { seven = 1 + seven e }
                    8 -> e { eight = 1 + eight e }
                    9 -> e { nine = 1 + nine e }
                    10 -> e { ten = 1 + ten e }
          i' = i - 1 
 
data AC = AC {
     one :: Int
  ,  two :: Int
  ,  three :: Int
  ,  four :: Int
  ,  five :: Int
  ,  six :: Int
  ,  seven :: Int
  ,  eight :: Int
  ,  nine :: Int
  ,  ten :: Int
    } deriving ShowShow
