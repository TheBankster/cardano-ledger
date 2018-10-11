{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell  #-}

module Cardano.Chain.Txp.TxPayload
       ( TxPayload (..)
       , mkTxPayload
       , checkTxPayload
       , txpTxs
       , txpWitnesses
       ) where

import           Cardano.Prelude

import           Control.Lens (makeLenses)
import           Control.Monad.Except (MonadError)

import           Cardano.Binary.Class (Bi (..))
import           Cardano.Chain.Txp.Tx (Tx, checkTx)
import           Cardano.Chain.Txp.TxAux (TxAux (..))
import           Cardano.Chain.Txp.TxWitness (TxWitness)


-- | Payload of Txp component which is part of main block. Constructor is
--   unsafe, because it lets one create invalid payload, for example with
--   different number of transactions and witnesses.
data TxPayload = UnsafeTxPayload
  { _txpTxs       :: ![Tx]
  -- ^ Transactions are the main payload
  , _txpWitnesses :: ![TxWitness]
  -- ^ Witnesses for each transaction
  --
  --   The payload is invalid if this list is not of the same length
  --   as '_txpTxs'. See 'mkTxPayload'.
  } deriving (Show, Eq, Generic)

instance NFData TxPayload

makeLenses ''TxPayload

-- | Smart constructor of 'TxPayload' which ensures that invariants of
--   'TxPayload' hold. Currently there is only one invariant: number of txs must
--   be same as number of witnesses.
mkTxPayload :: [TxAux] -> TxPayload
mkTxPayload txws = UnsafeTxPayload {_txpTxs = txs, _txpWitnesses = witnesses}
  where (txs, witnesses) = unzip $ map (liftA2 (,) taTx taWitness) txws

instance Bi TxPayload where
  encode payload = encode $ zip (_txpTxs payload) (_txpWitnesses payload)
  decode = mkTxPayload <$> decode

-- | Check a TxPayload by checking all of the Txs it contains
checkTxPayload :: MonadError Text m => TxPayload -> m ()
checkTxPayload payload = forM_ (_txpTxs payload) checkTx
