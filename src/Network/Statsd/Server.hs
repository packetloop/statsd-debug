import           Control.Concurrent
import           Control.Concurrent.Chan
import           Data.Either
import           Network.Socket
import           Network.Statsd
import           Network.Statsd.Parser
import           System.IO


port = "8125"

main = do
    chan <- newChan
    forkIO $ networkServer chan
    metricServer chan

metricServer chan = do
    metric <- readChan chan
    print metric
    hFlush stdout
    metricServer chan

networkServer chan = do
    addrinfos <- getAddrInfo (Just (defaultHints {addrFlags = [AI_PASSIVE]})) Nothing (Just port)
    let serveraddr = head addrinfos
    sock <- socket (addrFamily serveraddr) Datagram defaultProtocol
    bind sock (addrAddress serveraddr)
    let handleMessage = do
            (msg, _, addr) <- recvFrom sock 1024
            case parseStats msg of
                Left error    -> print $ "ERROR: " ++ msg
                Right stats -> writeList2Chan chan stats
            handleMessage
    handleMessage
