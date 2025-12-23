{-# LANGUAGE OverloadedStrings #-}

module Main where

import NFT
import Plutus.V2.Ledger.Api (TxOutRef(..), TxId(..), TokenName(..))
import PlutusTx.Builtins (toBuiltin)
import qualified Data.ByteString.Char8 as BS

testSaveMoran :: IO ()
testSaveMoran = do
    let txidBytes = BS.pack "aa72fc2947d06fe3164db3ee71ef6fdc1c64193872d086da5515007a27046a57"
        txid      = TxId (toBuiltin txidBytes)

        tnBytes   = BS.pack "Moran"
        tn        = TokenName (toBuiltin tnBytes)

        oref      = TxOutRef txid 0
    
    let cs = nftCurrencySymbol oref tn

    saveNFTPolicy oref tn
    putStrLn "Saved NFT policy for token 'Moran'"
    putStrLn $ "TxOutRef       : " ++ show oref
    putStrLn $ "Currency symbol: " ++ show cs
    putStrLn "NFT mint simulation complete!"

main :: IO ()
main = do
    putStrLn "Running NFTSpec..."
    testSaveMoran