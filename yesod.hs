/* Using Yesod to access data and display in Local Host- Not Tested due to technical issues*/



{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ViewPatterns               #-}
import Yesod
import Database.Persist.Sqlite
import Control.Monad.Trans.Resource (runResourceT)
import Control.Monad.Logger (runStderrLoggingT)

-- Define our entities 
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Temperature
    date Strin
    temperature Int
    deriving Show
|]

-- Connection pool in the foundation. At program initialization, we create our initial pool, and each time we need to perform an action we check out a single connection from the pool.

data PersistTest = PersistTest ConnectionPool

-- We'll create a single route, to access temperature data, making use an Id type.
mkYesod "PersistTest" [parseRoutes|
/ HomeR GET
/temperature/#TemperatureId TemperatureR GET[]

instance Yesod PersistTest

-- Define a YesodPersist instance, which will keep track of which backend we're using and how to run an action.

instance YesodPersist PersistTest where
    type YesodPersistBackend PersistTest = SqlBackend

    runDB action = do
        PersistTest pool <- getYesod
        runSqlPool action pool

-- List all temperature in the database
getHomeR :: Handler Html
getHomeR = do
    temperature <- runDB $ selectList []
    defaultLayout
        [whamlet|
            <ul>
                $forall Entity temperatureid temperature <- temperature
                    <li>
               <a href=@{TemperatureR temperatureid}>#{date temperature}[]
  </li>
  </ul>      

-- Return the show value of temperature, or a 404 if the temperature doesn't exist.

getTemperatureR :: TemperatureId -> Handler String
getTemperatureR temperatureId = do
    temperature <- runDB $ get404 temperatureId
    return $ show temperature

openConnectionCount :: Int
openConnectionCount = 10

main :: IO ()
main = runStderrLoggingT $ withSqlitePool "database.hs" openConnectionCount $ \pool -> liftIO $ do
    runResourceT $ flip runSqlPool pool $ do
        runMigration migrateAll
        insert $ Temperature
    warp 8000 $ PersistTest pool

