{-# LANGUAGE OverloadedStrings #-}

module PublicFundRelease where

import Language.Marlowe.Extended.V1

------------------------------------------------------------
-- ROLES
------------------------------------------------------------

depositor :: Party
depositor = Role "Depositor"

beneficiary :: Party
beneficiary = Role "Beneficiary"

officials :: [Party]
officials =
  [ Role "Official1"
  , Role "Official2"
  , Role "Official3"
  , Role "Official4"
  ]

------------------------------------------------------------
-- PARAMETERS
------------------------------------------------------------

requiredApprovals :: Integer
requiredApprovals = 3   -- n-of-m

deadline :: Timeout
deadline = POSIXTime 1700000000000

depositAmount :: Integer
depositAmount = 1000000000  -- 1000 ADA

------------------------------------------------------------
-- CHOICE IDS
------------------------------------------------------------

approvalChoice :: Party -> ChoiceId
approvalChoice p = ChoiceId "Approve" p

------------------------------------------------------------
-- COUNT APPROVALS
------------------------------------------------------------

approvalValue :: Party -> Value
approvalValue p =
  ChoiceValue (approvalChoice p)

totalApprovals :: Value
totalApprovals =
  foldr
    (\p acc -> AddValue acc (approvalValue p))
    (Constant 0)
    officials

------------------------------------------------------------
-- CONTRACT LOGIC
------------------------------------------------------------

publicFundContract :: Contract
publicFundContract =
  -- Step 1: Deposit funds
  When
    [ Case
        (Deposit depositor depositor ada (Constant depositAmount))
        approvalPhase
    ]
    deadline
    Close

------------------------------------------------------------
-- APPROVAL PHASE
------------------------------------------------------------

approvalPhase :: Contract
approvalPhase =
  collectApprovals officials

------------------------------------------------------------
-- RECURSIVE COLLECTION
------------------------------------------------------------

collectApprovals :: [Party] -> Contract
collectApprovals [] =
  evaluateOutcome

collectApprovals (p:ps) =
  When
    [ Case
        (Choice (approvalChoice p) [Bound 1 1])
        (collectApprovals ps)
    ]
    deadline
    refundDepositor

------------------------------------------------------------
-- FINAL DECISION
------------------------------------------------------------

evaluateOutcome :: Contract
evaluateOutcome =
  If
    (ValueGE totalApprovals (Constant requiredApprovals))
    releaseFunds
    refundDepositor

------------------------------------------------------------
-- RELEASE FUNDS
------------------------------------------------------------

releaseFunds :: Contract
releaseFunds =
  Pay
    depositor
    (Party beneficiary)
    ada
    (Constant depositAmount)
    Close

------------------------------------------------------------
-- REFUND
------------------------------------------------------------

refundDepositor :: Contract
refundDepositor =
  Pay
    depositor
    (Party depositor)
    ada
    (Constant depositAmount)
    Close

------------------------------------------------------------
-- MAIN
------------------------------------------------------------

main :: IO ()
main = printJSON publicFundContract