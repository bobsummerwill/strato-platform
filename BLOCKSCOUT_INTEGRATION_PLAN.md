# Blockscout Integration Plan

This branch tracks the STRATO-side work needed to support Blockscout through a
private explorer-oriented API, while preserving STRATO's current `/eth/v1.2`
core API.

## Current Baseline

The existing STRATO core API already exposes enough data for partial
Blockscout-style reads:

- `/eth/v1.2/metadata`
- `/eth/v1.2/block`
- `/eth/v1.2/block/last/:num`
- `/eth/v1.2/transaction`
- `/eth/v1.2/transactionResult`
- `/eth/v1.2/account`
- `/eth/v1.2/code/:codeHash`
- `/eth/v1.2/storage`

Those endpoints are implemented through:

- `strato/api/core/src/Core/API.hs`
- `strato/api/core/src/Handlers/Block.hs`
- `strato/api/core/src/Handlers/BlkLast.hs`
- `strato/api/core/src/Handlers/Transaction.hs`
- `strato/api/core/src/Handlers/TransactionResult.hs`
- `strato/api/core/src/Handlers/AccountInfo.hs`
- `strato/api/core/src/Handlers/Storage.hs`

They are enough for Blockscout to consume:

- chain info
- latest block
- block-by-range / block-by-number / block-by-hash
- transaction-by-hash
- derived tx counts
- balances
- nonces

They are not enough for full Blockscout indexing.

## Missing STRATO-side Capabilities

### 1. Explorer-grade receipt batches

Blockscout needs canonical receipts with:

- `transaction_hash`
- `transaction_index`
- `block_hash`
- `block_number`
- `gas_used`
- `cumulative_gas_used`
- `status`
- `logs`
- `logs_bloom`

Current blocker:

- `Handlers.TransactionResult` exposes execution results, not explorer-ready
  receipts.
- the existing JSON-RPC receipt adapter already demonstrates the missing fields
  by hardcoding `transactionIndex = 0`, `logs = []`, and zero bloom.

### 2. Log search

Blockscout depends on range log queries for token transfer extraction and
receipt-driven indexing.

Current blocker:

- there is no log-search endpoint in `Core.API`.

### 3. Historical bytecode reads

Blockscout `codes_at/2` needs deployed bytecode hex by address and block.

Current blocker:

- `Handlers.AccountInfo` exposes `codeHash`, not bytecode.
- `CodeAPI` returns `SourceMap`, which is not sufficient for Blockscout's
  `contract_code` indexing path.

### 4. Traces / internal transactions

Current blocker:

- no explorer-oriented trace endpoint exists in the current STRATO API surface.

## Recommended STRATO-side API

Add a new private Blockscout-oriented API alongside the current core API. Do not
try to overload `/eth/v1.2` with explorer-specific response shapes.

Recommended endpoints:

- `GET /chain-info`
- `GET /blocks/by-tag/:tag?hydrated=true`
- `POST /blocks/range`
- `POST /blocks/by-number`
- `POST /blocks/by-hash`
- `POST /transactions/by-hash`
- `POST /transactions/count/by-block-number`
- `POST /receipts/by-block-number`
- `POST /receipts/by-transaction-hash`
- `POST /logs/search`
- `POST /state/balances`
- `POST /state/nonces`
- `POST /state/codes`

## Implementation Shape

### New module family

Add a new handler family under `strato/api/core/src/Handlers/Blockscout/` or an
adjacent private API package if we want stricter separation.

Suggested modules:

- `Handlers.Blockscout.ChainInfo`
- `Handlers.Blockscout.Blocks`
- `Handlers.Blockscout.Transactions`
- `Handlers.Blockscout.Receipts`
- `Handlers.Blockscout.Logs`
- `Handlers.Blockscout.State`
- `Handlers.Blockscout.Mapper`

### Likely data sources

- blocks / transactions:
  - existing block and transaction SQL queries from `Handlers.Block` and
    `Handlers.Transaction`
- balances / nonces:
  - existing account-state query path from `Handlers.AccountInfo`
- bytecode:
  - new code-resolution path beyond `SourceMap`
- receipts:
  - `TransactionResult` plus additional block/tx ordering and log materialization
- logs:
  - new event/log index or query path

## Suggested Delivery Order

1. Add the private Blockscout API namespace and route skeleton.
2. Implement read paths that are already straightforward:
   - chain info
   - blocks
   - transactions
   - tx counts
   - balances
   - nonces
3. Implement `POST /state/codes` with real bytecode semantics.
4. Implement receipt batch endpoints.
5. Implement log search.
6. Only then evaluate trace/internal transaction support.

## First Practical Tasks

1. Define the Servant API type for the new private Blockscout API.
2. Reuse existing block / tx / account queries wherever possible.
3. Decide the canonical receipt source and how logs will be materialized.
4. Decide how deployed bytecode will be retrieved for a given address and block.

## Branch Intent

All STRATO-side Blockscout integration work for this effort should land on this
branch first:

- `blockscout-support`
