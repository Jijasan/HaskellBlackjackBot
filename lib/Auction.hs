{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Auction (Bot (..), auction) where

import           Prelude           hiding (id)

import           Data.ByteString.Char8      (unpack)
import           Data.ByteString.Lazy.Char8 (pack)

import           Network.Wreq       (FormParam ((:=)))
import           System.Environment (getEnv)
import           Network.Simple.TCP (Socket, serve, recv, HostPreference(Host))
import           Data.Aeson         (FromJSON, decode)
import           Control.Exception  (catchJust)
import           Control.Monad      (guard, void, join)
import           Data.Foldable      (for_)
import           Data.Text          (Text)
import qualified Data.Text          as Text
import           Database           (EntityField (StakeValue, StakeUsername), Stake (..), runDb)
import           Database.Persist   (entityVal, selectList, upsert, (=.), (-=.), (==.))
import           System.IO.Error    (isDoesNotExistError)
import           System.Random
import           Telegram           (Chat (..), Message (..), Telegram (..),
                                    Update (..), User (..), Dice (..), Ok(..), telegramRpc)
import           Text.Read          (readMaybe)

data Bot = Bot
  { databaseFile :: FilePath
  , updateIdFile :: FilePath
  }

foldWhile :: Bool -> Socket -> (Socket -> IO ([Char], Bool)) -> IO [Char]
foldWhile cond socket f = if cond
  then pure []
  else do
    (str, cond1) <- f socket
    (++) <$> pure str <*> foldWhile cond1 socket f

receiveLine :: Socket -> IO ([Char], Bool)
receiveLine socket = do
  str <- recv socket 10000 
  print str
  case str of
    Nothing -> pure ([], True)
    Just s -> pure (Data.ByteString.Char8.unpack s, False)

auction :: Telegram -> Bot -> IO ()
auction telegram@Telegram{getUpdates, putLog, token} bot@Bot{updateIdFile} = do
  url <- getEnv "WEBHOOK_URL"
  _ <- (telegramRpc token "setWebhook" ["url" := url]) :: IO Bool
  updateIdM <- loadUpdateId updateIdFile
  putLog "Waiting for updates..."
  serve (Host "127.0.0.1") "8443" $ \(connectionSocket, remoteAddr) -> do
    Prelude.putStrLn $ "TCP connection established from " ++ show remoteAddr
    str <- foldWhile False connectionSocket receiveLine
    print str
    updates <- case decode (Data.ByteString.Lazy.Char8.pack str) of
        Nothing -> pure []
        Just Ok{ok, result} -> pure (result :: [Update])
    putLog $ "Got " <> show (length updates) <> " updates"
    for_ updates $ validateUpdate telegram bot

validateUpdate :: Telegram -> Bot -> Update -> IO ()
validateUpdate telegram bot update@Update{update_id, message} = case message of
  Nothing  -> pure ()
  Just mes -> handleUpdate telegram bot update_id mes

loadUpdateId :: FilePath -> IO (Maybe Integer)
loadUpdateId updateIdFile =
  catchJust
    (guard . isDoesNotExistError)
    (readMaybe <$> readFile updateIdFile)
    (\_ -> pure Nothing)

handleUpdate :: Telegram -> Bot -> Integer -> Message -> IO ()
handleUpdate telegram bot update_id message =
  do
    case text of
      Just text' -> parseCommand text'
      Nothing    -> putLog $ "No stake for " ++ show message
    writeFile updateIdFile $ show update_id
    putLog $ "Written update_id = " ++ show update_id
    putLog $ show message
  where

    Telegram{sendMessage, putLog} = telegram
    Bot{databaseFile, updateIdFile} = bot

    Message{chat, text, from, dice} = message
    Chat{id = chatId} = chat
    User{first_name, last_name} = from

    username' = case last_name of
                  Just last_name' -> first_name <> " " <> last_name'
                  Nothing         -> first_name

    unpackText :: Text -> String
    unpackText txt = Text.unpack txt

    packText :: String -> Text
    packText str = Text.pack str

    parseCommand :: Text -> IO ()
    parseCommand txt = 
      let (command : xs) = words $ unpackText txt
      in case command of
        "set"  -> setBalance xs
        "coin" -> handleStakeStr $ unwords xs
        "help" -> printHelp
        _      -> sendMessage chatId "Unknown command"

    printHelp :: IO ()
    printHelp = sendMessage chatId "Commands list:\nset *username* *sum* - set *username*'s balance to *sum*\ncoin *bet* - make a bet of amount *bet* to a coin toss\nhelp - get command list"

    cutTail :: [a] -> ([a], a)
    cutTail (x: []) = ([], x)
    cutTail (x: xs) = let (list, tail) = cutTail xs
      in ((x : list), tail)

    setBalance :: [String] -> IO ()
    setBalance []  = pure ()
    setBalance str = 
      let (name, sm) = cutTail str
      in let sum = readMaybe sm
        in case sum of
          Nothing  -> pure ()
          Just sum -> join (handleDbStakes <$> (runDb (Text.pack databaseFile) $ do
            void $
              upsert  -- update or insert
                (Stake (packText $ unwords name) $ sum)  -- insert
                [StakeValue =. sum]  -- update
            map entityVal <$> selectList [] []))

    handleStakeStr :: String -> IO ()
    handleStakeStr text' = case readMaybe text' of
                             Nothing -> pure ()
                             Just value -> makeBet value

    tossCoin :: IO (Bool)
    tossCoin = do
      i       <- getStdRandom (randomR (0 :: Integer, 1 :: Integer))
      if i == 0
        then pure False
        else pure True

    makeBet :: Int -> IO ()
    makeBet bet = do
      (Stake{stakeUsername, stakeValue} : xs) <- runDb (Text.pack databaseFile) $ map entityVal <$> selectList [StakeUsername ==. username'] []
      if bet <= 0 
        then sendMessage chatId "Bet must be positive"
        else if stakeValue < bet
          then sendMessage chatId "You haven't enough money"
          else do
            handleStakeInt bet
            win <- tossCoin
            if win
              then do
                sendMessage chatId "You win" 
                handleStakeInt $ -bet * 2
              else sendMessage chatId "You lose"

    handleDbStakes :: [Stake] -> IO ()
    handleDbStakes stakes = 
      let stakeTable = case stakes of
            [] -> "no stakes"
            _ : _ ->
              Text.unlines
                [ stakeUsername <> ": " <> tshow stakeValue
                | Stake{stakeUsername, stakeValue} <- stakes
                ]
      in do
        void $ sendMessage chatId stakeTable
        putLog $ "Sent results to " ++ show chat ++ " " ++ show from

    handleStakeInt :: Int -> IO ()
    handleStakeInt value = 
      join (handleDbStakes <$> (runDb (Text.pack databaseFile) $ do
        void $
          upsert  -- update or insert
            (Stake username' $ 1000 - value)  -- insert
            [StakeValue -=. value]  -- update
        map entityVal <$> selectList [] []))
      


tshow :: Show a => a -> Text
tshow = Text.pack . show
