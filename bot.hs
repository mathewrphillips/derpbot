import Network
import System.IO
import Text.Printf
import Data.List
import System.Exit
import Data.List.Utils(contains)

server = "irc.afternet.org"
port   = 6667
chan   = "#dvcolgan"
nick   = "melongod"

main = do
    h <- connectTo server (PortNumber (fromIntegral port))
    hSetBuffering h NoBuffering
    write h "NICK" nick
    write h "USER" (nick++" matt irc.afternet.org :matt")
    listen h

write :: Handle -> String -> String -> IO ()
write h s t = do
    hPrintf h "%s %s\r\n" s t
    printf    "> %s %s\n" s t

listen :: Handle -> IO ()
listen h = forever $ do
    t <- hGetLine h
    let s = init t
    if ping s then pong s else (if mode s then reply s else eval h (clean s))

    putStrLn s
 where
    forever a = a >> forever a 

    clean     = drop 1 . dropWhile (/= ':') . drop 1
    ping x    = "PING :" `isPrefixOf` x
    pong x    = write h "PONG" (':' : drop 6 x)
    mode x    = ":on" `isInfixOf` x 
    reply x   = write h "JOIN" chan

eval :: Handle -> String -> IO ()
eval h x | "Jordanfitz" `isInfixOf` x = privmsg h "Jordane!"
eval h x | "ChillyFlash" `isInfixOf` x = return ()
eval h x | "!echo" `isPrefixOf` x = privmsg h (drop 6 x) 
eval h x | "NOTICE" `isInfixOf` x = return ()
eval _   _                       = return () -- ignore everything else

privmsg :: Handle -> String -> IO ()
privmsg h s = write h "PRIVMSG" (chan ++ " :" ++ s)
