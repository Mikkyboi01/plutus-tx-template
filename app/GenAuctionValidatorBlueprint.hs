{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Main module for generating the Auction Validator Blueprint.
module Main where

import           AuctionValidator
import qualified Data.ByteString.Short       as Short
import qualified Data.Set                    as Set
import           PlutusLedgerApi.Common      (serialiseCompiledCode)
import qualified PlutusLedgerApi.V1.Crypto   as Crypto
import qualified PlutusLedgerApi.V1.Time     as Time
import qualified PlutusLedgerApi.V1.Value    as Value
import           PlutusTx.Blueprint
import           PlutusTx.Builtins.HasOpaque (stringToBuiltinByteStringHex)
import           System.Environment          (getArgs)
import           Control.Monad               (when)

-- | Auction parameters used to parameterize the validator.
auctionParams :: AuctionParams
auctionParams = AuctionParams
  { apSeller =
      Crypto.PubKeyHash $
        stringToBuiltinByteStringHex
          "0000000000000000000000000000000000000000\
          \0000000000000000000000000000000000000000"
  , apCurrencySymbol =
      Value.CurrencySymbol $
        stringToBuiltinByteStringHex
          "00000000000000000000000000000000000000000000000000000000"
  , apTokenName = Value.tokenName "MY_TOKEN"
  , apMinBid = 100  -- Minimum bid in lovelace
  , apEndTime = Time.fromMilliSeconds 1_725_227_091_000
  }

-- | Contract blueprint for the auction validator.
myContractBlueprint :: ContractBlueprint
myContractBlueprint = MkContractBlueprint
  { contractId = Just "auction-validator"
  , contractPreamble = myPreamble
  , contractValidators = Set.singleton myValidator
  , contractDefinitions =
      deriveDefinitions @[AuctionParams, AuctionDatum, AuctionRedeemer]
  }

-- | Metadata for the blueprint (title, description, etc.).
myPreamble :: Preamble
myPreamble = MkPreamble
  { preambleTitle = "Auction Validator"
  , preambleDescription = Just "Blueprint for a Plutus script validating auction transactions."
  , preambleVersion = "1.0.0"
  , preamblePlutusVersion = PlutusV2
  , preambleLicense = Just "MIT"
  }

-- | Validator blueprint definition.
myValidator :: ValidatorBlueprint referencedTypes
myValidator = MkValidatorBlueprint
  { validatorTitle = "Auction Validator"
  , validatorDescription = Just "Plutus script validating auction transactions."
  , validatorParameters =
      [ MkParameterBlueprint
          { parameterTitle = Just "Parameters"
          , parameterDescription = Just "Compile-time validator parameters."
          , parameterPurpose = Set.singleton Spend
          , parameterSchema = definitionRef @AuctionParams
          }
      ]
  , validatorRedeemer =
      MkArgumentBlueprint
        { argumentTitle = Just "Redeemer"
        , argumentDescription = Just "Redeemer for the auction validator."
        , argumentPurpose = Set.singleton Spend
        , argumentSchema = definitionRef @()
        }
  , validatorDatum = Nothing
  , validatorCompiled = Just $ compiledValidator PlutusV2 $
      Short.fromShort . serialiseCompiledCode $ auctionValidatorScript auctionParams
  }

-- | Write the generated blueprint to a file.
writeBlueprintToFile :: FilePath -> IO ()
writeBlueprintToFile path = writeBlueprint path myContractBlueprint

-- | Main entry point for CLI.
main :: IO ()
main = do
  args <- getArgs
  when (length args /= 1) $
    fail $ "Usage: runhaskell Main.hs <output-file-path>\nReceived " <> show (length args) <> " arguments."

  let [outputPath] = args
  putStrLn $ "Writing auction validator blueprint to: " <> outputPath
  writeBlueprintToFile outputPath
  putStrLn "✅ Blueprint successfully written!"
