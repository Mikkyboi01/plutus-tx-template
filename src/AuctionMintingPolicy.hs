{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost        #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE Strict                     #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE ViewPatterns               #-}
{-# OPTIONS_GHC -fno-full-laziness #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
{-# OPTIONS_GHC -fno-spec-constr #-}
{-# OPTIONS_GHC -fno-specialise #-}
{-# OPTIONS_GHC -fno-strictness #-}
{-# OPTIONS_GHC -fno-unbox-small-strict-fields #-}
{-# OPTIONS_GHC -fno-unbox-strict-fields #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.0.0 #-}

module AuctionMintingPolicy (
    AuctionMintingParams,
    AuctionMintingRedeemer,
    auctionTypedMintingPolicy,
    auctionUntypedMintingPolicy,
    auctionMintingPolicyScript
) where

import PlutusCore.Version           (plcVersion100)
import PlutusLedgerApi.V1.Value     (flattenValue)
import PlutusLedgerApi.V2
    ( PubKeyHash
    , ScriptContext(..)
    , TxInfo(..)
    , ownCurrencySymbol
    , txSignedBy
    )
import PlutusTx
import PlutusTx.Prelude qualified as PlutusTx

------------------------------------------------------------------------------------------
-- | TYPES
------------------------------------------------------------------------------------------

type AuctionMintingParams   = PubKeyHash
type AuctionMintingRedeemer = ()

------------------------------------------------------------------------------------------
-- | TYPED MINTING POLICY
------------------------------------------------------------------------------------------

{-# INLINEABLE auctionTypedMintingPolicy #-}
-- | A minting policy that allows minting exactly one token when signed by a given PubKeyHash.
auctionTypedMintingPolicy ::
    AuctionMintingParams ->
    AuctionMintingRedeemer ->
    ScriptContext ->
    Bool
auctionTypedMintingPolicy pkh _ ctx =
    signedByOwner PlutusTx.&& mintedExactlyOneToken
  where
    txInfo :: TxInfo
    txInfo = scriptContextTxInfo ctx

    -- Ensure the transaction is signed by the provided PubKeyHash
    signedByOwner :: Bool
    signedByOwner = txSignedBy txInfo pkh

    -- Ensure exactly one token of this policy is minted
    mintedExactlyOneToken :: Bool
    mintedExactlyOneToken = case flattenValue (txInfoMint txInfo) of
        [(cs, _tn, q)] ->
            cs PlutusTx.== ownCurrencySymbol ctx PlutusTx.&& q PlutusTx.== 1
        _ -> False

------------------------------------------------------------------------------------------
-- | UNTYPED WRAPPER (for on-chain use)
------------------------------------------------------------------------------------------

{-# INLINEABLE auctionUntypedMintingPolicy #-}
auctionUntypedMintingPolicy ::
    AuctionMintingParams ->
    BuiltinData ->
    BuiltinData ->
    PlutusTx.BuiltinUnit
auctionUntypedMintingPolicy pkh redeemer ctx =
    PlutusTx.check $
        auctionTypedMintingPolicy
            pkh
            (PlutusTx.unsafeFromBuiltinData redeemer)
            (PlutusTx.unsafeFromBuiltinData ctx)

------------------------------------------------------------------------------------------
-- | COMPILED MINTING POLICY SCRIPT
------------------------------------------------------------------------------------------

{-# INLINEABLE auctionMintingPolicyScript #-}
auctionMintingPolicyScript ::
    AuctionMintingParams ->
    CompiledCode (BuiltinData -> BuiltinData -> PlutusTx.BuiltinUnit)
auctionMintingPolicyScript pkh =
    $$(PlutusTx.compile [||auctionUntypedMintingPolicy||])
        `PlutusTx.unsafeApplyCode` PlutusTx.liftCode plcVersion100 pkh
