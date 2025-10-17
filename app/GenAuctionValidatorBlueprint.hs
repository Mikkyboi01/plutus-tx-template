{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NumericUnderscores    #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE UndecidableInstances  #-}

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

-- | Customize the auction parameters here.
auctionParams :: AuctionParams
auctionParams =
  AuctionParams
    { apSeller =
        -- Replace with the seller's PubKeyHash in hex.
        Crypto.PubKeyHash $
          stringToBuiltinByteStringHex
            "0000000000000000000000000000000000000000000000000000000000000000"

    , apCurrencySymbol =
        -- Replace with your token's CurrencySymbol (minting policy hash).
        Value.CurrencySymbol $
          stringToBuiltinByteStringHex
            "00000000000000000000000000000000000000000000000000000000"

    , apTokenName =
        -- Replace with your token's name.
        Value.tokenName "MY_TOKEN"

    , apMinBid =
        -- Minimum bid (in lovelace).
        100

    , apEndTime =
        -- Auction end time in milliseconds.
        Time.fromMilliSeconds 1_725_227_091_000
    }

-- | Contract preamble metadata.
myPreamble :: Preamble
myPreamble = MkPreamble
  { preambleTitle       = "Auction Validator"
  , preambleDescription = Just "Blueprint for a Plutus script validating auction transactions"
  , preambleVersion     = "1.0.0"
  , preamblePlutusVersion = PlutusV2
  , preambleLicense     = Just "MIT"
  }

-- | Validator blueprint definition.
myValidator :: ValidatorBlueprint referencedTypes
myValidator = MkValidatorBlueprint
  { validatorTitle = "Auction Validator"
  , validatorDescription = Just "Plutus script validating auction transactions"
  , validatorParameters =
      [ MkParameterBlueprint
          { parameterTitle = Just "Parameters"
          , parameterDescription = Just "Compile-time validator parameters"
          , parameterPurpose = Set.singleton Spend
          , parameterSchema = definitionRef @AuctionParams
          }
      ]
  , validatorRedeemer =
      MkArgumentBlueprint
        { argumentTitle = Just "Redeemer"
        , argumentDescription = Just "Redeemer for the auction validator"
        , argumentPurpose = Set.singleton Spend
        , argumentSchema = definitionRef @()
        }
  , validatorDatum = Nothing
  , validatorCompiled = Just $
      compiledValidator PlutusV2 $
        Short.fromShort $ serialiseCompiledCode $
          auctionValidatorScript auctionParams
  }

-- | Main contract blueprint.
myContractBlueprint :: ContractBlueprint
myContractBlueprint = MkContractBlueprint
  { contractId          = Just "auction-validator"
  , contractPreamble    = myPreamble
  , contractValidators  = Set.singleton myValidator
  , contractDefinitions = deriveDefinitions @[AuctionParams, AuctionDatum, AuctionRedeemer]
  }

-- | Write the contract blueprint to a file.
writeBlueprintToFile :: FilePath -> IO ()
writeBlueprintToFile path = writeBlueprint path myContractBlueprint

-- | Program entry point.
main :: IO ()
main = getArgs >>= \case
  [filePath] -> writeBlueprintToFile filePath
  args       -> fail $ "Expected one argument (output file), but got " <> show (length args) <> ": " <> show args
