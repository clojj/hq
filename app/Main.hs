{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Main
where
import           Control.Concurrent             ( MVar
                                                , modifyMVar
                                                , modifyMVar_
                                                , newMVar
                                                , readMVar
                                                , myThreadId
                                                )
import           Control.Exception              ( finally )
import           Control.Monad                  ( forM_
                                                , forever
                                                )
import           Data.Char                      ( isPunctuation
                                                , isSpace
                                                )
import           Data.Monoid                    ( mappend
                                                , (<>)
                                                )
import           Data.Text                      ( Text
                                                , stripPrefix
                                                )
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T

import           Data.FileEmbed                 ( embedDir )
import           Network.HTTP.Types             ( status400 )
import qualified Network.Wai
import qualified Network.Wai.Application.Static
                                               as Static
import qualified Network.Wai.Handler.Warp      as Warp
import qualified Network.Wai.Handler.WebSockets
                                               as WaiWS
import qualified Network.WebSockets            as WS

import qualified Data.ByteString.Lazy          as LBS
import           Data.Text.Encoding             ( decodeUtf8 )

type Client = (Text, WS.Connection)

type ServerState = [Client]

newServerState :: ServerState
newServerState = []

numClients :: ServerState -> Int
numClients = length

clientExists :: Client -> ServerState -> Bool
clientExists client = any ((== fst client) . fst)

addClient :: Client -> ServerState -> ServerState
addClient client clients = client : clients

removeClient :: Client -> ServerState -> ServerState
removeClient client = filter ((/= fst client) . fst)

broadcast :: Text -> ServerState -> IO ()
broadcast message clients = do
    -- T.putStrLn message
    forM_ clients $ \(_, conn) -> WS.sendTextData conn message


main :: IO ()
main = do
    -- putStrLn "http://localhost:9160/index.html"
    state <- newMVar newServerState
    Warp.runSettings (Warp.setPort 9160 Warp.defaultSettings)
        $ WaiWS.websocketsOr WS.defaultConnectionOptions
                             (application state)
                             httpApp

httpApp :: Network.Wai.Application
httpApp _ respond = respond $ Network.Wai.responseLBS status400 [] "Not a websocket request"

staticApp :: Network.Wai.Application
staticApp = Static.staticApp $ Static.embeddedSettings $(embedDir "static")

application :: MVar ServerState -> WS.ServerApp
application state pending = do
    myThreadId >>= putStrLn . ("GHC thread " <>) . show

    conn <- WS.acceptRequest pending
    -- WS.forkPingThread conn 30
    msg     <- WS.receiveData conn
    clients <- readMVar state
    case msg of
        _
            | not (prefix `T.isPrefixOf` msg)
            -> WS.sendTextData conn ("Wrong announcement" :: Text)
            |
    --
    --
              any ($ fst client) [T.null, T.any isPunctuation, T.any isSpace]
            -> WS.sendTextData
                conn
                ("Name cannot "
                `mappend` "contain punctuation or whitespace, and "
                `mappend` "cannot be empty" :: Text
                )
            |
    --
    --
              clientExists client clients
            -> WS.sendTextData conn ("User already exists" :: Text)
            |
    --
    --
              otherwise
            -> flip finally disconnect $ do
                modifyMVar_ state $ \s -> do
                    let s' = addClient client s
                    WS.sendTextData conn
                        $         "Welcome! Users: "
                        `mappend` T.intercalate ", " (map fst s)
                    broadcast (fst client `mappend` " joined") s'
                    return s'
                talk conn state client
--
--
          where
            prefix     = "JOIN "
            client     = (T.drop (T.length prefix) msg, conn)
            disconnect = do
                -- Remove client and return new state
                s <- modifyMVar state
                    $ \s -> let s' = removeClient client s in return (s', s')
                broadcast (fst client `mappend` " disconnected") s

talk :: WS.Connection -> MVar ServerState -> Client -> IO ()
talk conn state (user, _) = forever $ do
    msg <- WS.receiveData conn
    readMVar state >>= broadcast (user <> ": " <> msg)
        -- let [ key, value ] = T.splitOn ":" msg
        --     logMsg = " KEY = " <> key <> " VALUE = " <> value
        --     in
        --     readMVar state >>= broadcast (user <> ": " <> msg <> logMsg)

parseJoin :: LBS.ByteString -> Maybe Text
parseJoin = stripPrefix "JOIN " . decodeUtf8 . LBS.toStrict
