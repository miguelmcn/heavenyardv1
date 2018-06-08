{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Livro where

import Import
import Network.HTTP.Types.Status
import Database.Persist.Postgresql

getLivroIdR :: LivroId -> Handler Value
getLivroIdR lid = do
	livro <- runDB $ get404 lid
	sendStatusJSON ok200 (object ["resp" .= livro])

getLivroR :: Handler Value
getLivroR = do
	todosLivros <- runDB $ selectList [] [Asc LivroNome]
	sendStatusJSON ok200 (object ["resp" .= todosLivros])

postLivroR :: Handler Value
postLivroR = do
	livro <- requireJsonBody :: Handler Livro
	lid <- runDB $ insert livro
	sendStatusJSON created201 (object ["resp" .= fromSqlKey lid])
