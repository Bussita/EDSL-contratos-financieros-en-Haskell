module PendingContracts where

import Data.Time (Day)

import Types
import AST

-- Contratos pendientes de firma

data PendingContract = PendingContract
  { pcName      :: String
  , pcContract  :: Contract
  , pcPartyA    :: PartyId
  , pcPartyB    :: PartyId
  , pcSignA     :: SignatureStatus
  , pcSignB     :: SignatureStatus
  , pcCreatedAt :: Day
  } deriving (Show)

createPending :: String -> Contract -> PartyId -> PartyId -> Day -> PendingContract
createPending name contract partyA partyB date = PendingContract
  { pcName      = name
  , pcContract  = contract
  , pcPartyA    = partyA
  , pcPartyB    = partyB
  , pcSignA     = Pending
  , pcSignB     = Pending
  , pcCreatedAt = date
  }

signContract :: PartyId -> PendingContract -> Either String PendingContract
signContract party pc
  | party == pcPartyA pc =
      if pcSignA pc == Signed
        then Left $ party ++ " ya firmó este contrato."
        else Right pc { pcSignA = Signed }
  | party == pcPartyB pc =
      if pcSignB pc == Signed
        then Left $ party ++ " ya firmó este contrato."
        else Right pc { pcSignB = Signed }
  | otherwise =
      Left $ party ++ " no es parte de este contrato ("
          ++ pcPartyA pc ++ " / " ++ pcPartyB pc ++ ")."

isFullySigned :: PendingContract -> Bool
isFullySigned pc = pcSignA pc == Signed && pcSignB pc == Signed
