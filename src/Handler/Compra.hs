{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Compra where

import Import
import Network.HTTP.Types.Status
import Database.Persist.Postgresql

postCompraR :: Handler Value
postCompraR = do
    compra <- requireJsonBody :: Handler Compra
    cid <- runDB $ insert compra
    sendStatusJSON created201 (object ["resp" .= fromSqlKey cid])

getBuscarLivroR :: ClienteId -> Handler Value
getBuscarLivroR cid = do
    comprasDoCliente <- runDB $ selectList [CompraClienteid ==. cid] [LimitTo 10]
    livrosCompra <- return $ map (compraLivroid . entityVal ) comprasDoCliente
    livros <- runDB $ selectList [LivroId <-. livrosCompra] []
    sendStatusJSON ok200 (object ["resp" .= livros])

getBuscarCliR :: LivroId -> Handler Value
getBuscarCliR lid = do
    comprasDoLivro <- runDB $ selectList [CompraLivroid ==. lid] [LimitTo 10]
    clientesCompra <- return $ map (compraClienteid . entityVal ) comprasDoLivro
    clientes <- runDB $ selectList [ClienteId <-. clientesCompra] []
    sendStatusJSON ok200 (object ["resp" .= clientes])

getVendaR :: LivroId -> Handler Value
getVendaR lid = do
    livros <- runDB $ selectList [CompraLivroid ==. lid] []
    livrosCompra <- return $ map (compraLivroid . entityVal ) livros
    livrosTodos <- runDB $ selectList [LivroId <-. livrosCompra] []
    soma <- return $ fromIntegral (length livrosCompra) * ( sum $ map (livroPreco . entityVal) livrosTodos)
    sendStatusJSON ok200 (object ["resp" .= soma])
