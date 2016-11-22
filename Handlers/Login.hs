{-# LANGUAGE OverloadedStrings, QuasiQuotes,
             TemplateHaskell #-}
 
module Handlers.Login where
import Yesod
import Foundation
import Control.Monad.Logger (runStdoutLoggingT)
import Control.Applicative
import Data.Text
import Text.Lucius
import Database.Persist.Postgresql

formLogin :: Form (Text, Text)
formLogin = renderDivs $ (,) <$>
             areq emailField "E-mail" Nothing <*>
             areq passwordField "Password" Nothing

getLoginR :: Handler Html
getLoginR = do
    (widget, enctype) <- generateFormPost formLogin
    defaultLayout $ widgetForm LoginR enctype widget "Login"

postLoginR :: Handler Html
postLoginR = do
            ((result, _), _) <- runFormPost formLogin
            case result of
                FormSuccess (email, password) -> do
                   cara <- runDB $ selectFirst [PessoaEmail ==. email,
                                                PessoaSenha ==. password] []
                   case cara of
                       Just (Entity pid pessoa) -> do
                           setSession "_ID" (pack $ show $ fromSqlKey pid)
                           redirect (PessoaR pid)
                       Nothing -> redirect LoginR
                _ -> redirect CadastroR

postLogoutR:: Handler Html
postLogoutR = do
    deleteSession "_ID"
    redirect HelloR