{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import AuctionMintingPolicy
import Data.ByteString.Short qualified as Short
import Data.Set qualified as Set
import PlutusLedgerApi.Common (serialiseCompiledCode)
import PlutusTx.Blueprint
import System.Environment (getArgs)
import System.Exit (die)
import Control.Monad (unless)

--------------------------------------------------------------------------------
-- Contract Blueprint Definition
--------------------------------------------------------------------------------

myContractBlueprint :: ContractBlueprint
myContractBlueprint =
  MkContractBlueprint
    { contractId = Just "auction-minting-policy"
    , contractPreamble = myPreamble
    , contractValidators = Set.singleton myValidator
    , contractDefinitions = deriveDefinitions @[AuctionMintingParams, ()]
    }

--------------------------------------------------------------------------------
-- Preamble
--------------------------------------------------------------------------------

myPreamble :: Preamble
myPreamble =
  MkPreamble
    { preambleTitle = "Auction Minting Policy"
    , preambleDescription = Just "A simple minting policy for auctions"
    , preambleVersion = "1.0.0"
    , preamblePlutusVersion = PlutusV2
    , preambleLicense = Just "MIT"
    }

--------------------------------------------------------------------------------
-- Validator Definition
--------------------------------------------------------------------------------

myValidator :: ValidatorBlueprint referencedTypes
myValidator =
  MkValidatorBlueprint
    { validatorTitle = "Auction Minting Validator"
    , validatorDescription = Just "Validator for auction-based minting policy"
    , validatorParameters =
        [ MkParameterBlueprint
            { parameterTitle = Just "Minting Validator Parameters"
            , parameterDescription = Just "Compile-time validator parameters"
            , parameterPurpose = Set.singleton Mint
            , parameterSchema = definitionRef @AuctionMintingParams
            }
        ]
    , validatorRedeemer =
        MkArgumentBlueprint
          { argumentTitle = Just "Redeemer"
          , argumentDescription = Just "Unit redeemer for minting policy"
          , argumentPurpose = Set.singleton Mint
          , argumentSchema = definitionRef @()
          }
    , validatorDatum = Nothing
    , validatorCompiled = Just compiledAuctionPolicy
    }

--------------------------------------------------------------------------------
-- Compiled Policy
--------------------------------------------------------------------------------

compiledAuctionPolicy :: CompiledValidator
compiledAuctionPolicy =
  let script = auctionMintingPolicyScript (error "Replace with seller's public key hash")
      code = Short.fromShort (serialiseCompiledCode script)
  in compiledValidator PlutusV2 code

--------------------------------------------------------------------------------
-- File Writer
--------------------------------------------------------------------------------

writeBlueprintToFile :: FilePath -> IO ()
writeBlueprintToFile path = do
  putStrLn $ "Writing blueprint to: " <> path
  writeBlueprint path myContractBlueprint
  putStrLn "✅ Blueprint successfully written."

--------------------------------------------------------------------------------
-- Main Entry Point
--------------------------------------------------------------------------------

main :: IO ()
main = do
  args <- getArgs
  case args of
    [path] -> writeBlueprintToFile path
    _      -> die "❌ Error: Expected exactly one argument (output file path)."
