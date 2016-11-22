{-# LANGUAGE OverloadedStrings, QuasiQuotes,
             TemplateHaskell, ViewPatterns #-}
 
module Application where

import Yesod
import Foundation
import Handlers.Departamento
import Handlers.Pessoa
import Handlers.Login

import Database.Persist.Postgresql

mkYesodDispatch "Sitio" resourcesSitio

getHelloR :: Handler Html
getHelloR = defaultLayout $ do
     sess <- lookupSession "_ID"
     [whamlet|
         <h1> _{MsgHello}
         <h2> _{MsgBye}
         <button> _{MsgButton} </button>
         $maybe _ <- sess
             <form action=@{LogoutR} method=post>
                 <input type="submit" value="Logout">
     |]
