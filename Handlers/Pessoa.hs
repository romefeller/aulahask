{-# LANGUAGE OverloadedStrings, QuasiQuotes,
             TemplateHaskell #-}
 
module Handlers.Pessoa where
import Yesod
import Foundation
import Control.Monad.Logger (runStdoutLoggingT)
import Control.Applicative
import Data.Text
import Text.Lucius
import Database.Persist.Postgresql
import Database.Persist

formPessoa :: Form Pessoa
formPessoa = renderDivs $ Pessoa <$>
             areq textField "Nome" Nothing <*>
             areq intField "Idade" Nothing <*>
             areq doubleField "Salario" Nothing <*>
             areq (selectField dptos) "Depto" Nothing <*>
             areq emailField "E-mail" Nothing <*>
             areq passwordField "Password" Nothing

dptos = do
       entidades <- runDB $ selectList [] [Asc DepartamentoNome] 
       optionsPairs $ fmap (\ent -> (departamentoSigla $ entityVal ent, entityKey ent)) entidades

getCadastroR :: Handler Html
getCadastroR = do
             (widget, enctype) <- generateFormPost formPessoa
             defaultLayout $ do 
                 addStylesheet $ StaticR teste_css
                 -- [whamlet|
                 --    <img src=@{StaticR lobo_jpg}>
                 -- |]
                 widgetForm CadastroR enctype widget "Cadastro de Pessoas"

getPessoaR :: PessoaId -> Handler Html
getPessoaR pid = do
             pessoa <- runDB $ get404 pid 
             dpto <- runDB $ get404 (pessoaDeptoid pessoa)
             defaultLayout [whamlet| 
                 <h1> Seja bem-vindx #{pessoaNome pessoa}
                 <p> Salario: #{pessoaSalario pessoa}
                 <p> Idade: #{pessoaIdade pessoa}
                 <p> Departamento: #{departamentoNome dpto}
             |]

getListarR :: Handler Html
getListarR = do
             listaP <- runDB $ selectList [] [Asc PessoaNome]
             defaultLayout $ do
                 $(whamletFile "templates/lista.hamlet") 
                 toWidget $(luciusFile "templates/lista.lucius")

postCadastroR :: Handler Html
postCadastroR = do
                ((result, _), _) <- runFormPost formPessoa
                case result of
                    FormSuccess pessoa -> do
                       unicoEmail <- runDB $ getBy $ UniqueEmail (pessoaEmail pessoa)
                       case unicoEmail of
                           Just _ -> redirect CadastroR
                           Nothing -> do 
                              pid <- runDB $ insert pessoa 
                              redirect (PessoaR pid)
                    _ -> redirect CadastroR

postPessoaR :: PessoaId -> Handler Html
postPessoaR pid = do
     runDB $ delete pid
     redirect ListarR