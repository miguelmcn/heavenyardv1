{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Cliente where

import Import
import Network.HTTP.Types.Status
import Database.Persist.Postgresql

patchCliNomeR :: ClienteId -> Text -> Handler Value
patchCliNomeR cid nome = do
    _ <- runDB $ get404 cid
    runDB $ update cid [ClienteNome =. nome]
    sendStatusJSON noContent204 (object [])

putCliIdR :: ClienteId -> Handler Value
putCliIdR cid = do
    _ <- runDB $ get404 cid
    novoCli <- requireJsonBody :: Handler Cliente
    runDB $ replace cid novoCli
    sendStatusJSON noContent204 (object [])

deleteCliIdR :: ClienteId -> Handler Value
deleteCliIdR cid = do
    _ <- runDB $ get404 cid
    runDB $ delete cid
    sendStatusJSON noContent204 (object [])

getCliIdR :: ClienteId -> Handler Value
getCliIdR cid = do
    cliente <- runDB $ get404 cid
    sendStatusJSON ok200 (object ["resp" .= cliente])

getCliEmailR :: Text -> Handler Value
getCliEmailR email = do
    cliente <- runDB $ getBy $ UniqueEmail email
    sendStatusJSON ok200 (object ["resp" .= cliente])

getHomeR :: Handler Html
getHomeR = undefined

getClienteR :: Handler Value
getClienteR = do
    todosClientes <- runDB $ selectList [] [Asc ClienteNome]
    sendStatusJSON ok200 (object ["resp" .= todosClientes])

postClienteR :: Handler Value
postClienteR = do
    cliente <- requireJsonBody :: Handler Cliente
    cid <- runDB $ insert cliente
    sendStatusJSON created201 (object ["resp" .= fromSqlKey cid])
