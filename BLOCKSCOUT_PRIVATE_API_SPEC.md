# Blockscout Private API Spec

This document defines the STRATO-side private HTTP API intended for Blockscout.
It is separate from the public `/eth/v1.2` core API and should be implemented as
an internal explorer-oriented surface.

## Goals

This API must provide explorer-grade data for:

- canonical block reads
- canonical transaction reads
- transaction count reads by block
- receipt batches
- log search
- historical balances
- historical nonces
- historical deployed bytecode

It should not try to emulate Ethereum JSON-RPC directly.

## Transport

- HTTP JSON
- `2xx` for success
- non-`2xx` with JSON error body when possible

## Endpoints

### `GET /chain-info`

Response:

```json
{
  "chain_id": 1516,
  "head": 42,
  "safe_head": 40
}
```

### `GET /blocks/by-tag/:tag?hydrated=true`

Response:

```json
{
  "hash": "0x...",
  "number": 42,
  "parent_hash": "0x...",
  "timestamp": "2024-01-01T00:00:00Z",
  "miner_hash": "0x...",
  "gas_limit": 30000000,
  "gas_used": 21000,
  "size": 1234,
  "nonce": 7,
  "difficulty": 1,
  "total_difficulty": 1,
  "base_fee_per_gas": 0,
  "transactions": [],
  "uncles": [],
  "withdrawals": []
}
```

### `POST /blocks/range`

Request:

```json
{
  "from": 16,
  "to": 17,
  "hydrated": true
}
```

Response:

```json
{
  "blocks": [],
  "errors": []
}
```

### `POST /blocks/by-number`

Request:

```json
{
  "block_numbers": [16, 17],
  "hydrated": true
}
```

Response:

```json
{
  "blocks": [],
  "errors": []
}
```

### `POST /blocks/by-hash`

Request:

```json
{
  "hashes": ["0x..."],
  "hydrated": true
}
```

Response:

```json
{
  "blocks": [],
  "errors": []
}
```

### `POST /transactions/by-hash`

Request:

```json
{
  "hashes": ["0xtx1", "0xtx2"]
}
```

Response:

```json
{
  "transactions": [],
  "errors": []
}
```

Transaction shape:

```json
{
  "hash": "0x...",
  "block_hash": "0x...",
  "block_number": 16,
  "index": 0,
  "from_address_hash": "0x...",
  "to_address_hash": "0x...",
  "created_contract_address_hash": "0x...",
  "value": 0,
  "gas": 21000,
  "gas_price": 1,
  "max_fee_per_gas": 1,
  "max_priority_fee_per_gas": 1,
  "input": "0x",
  "nonce": 3,
  "type": 2,
  "status": "success"
}
```

### `POST /transactions/count/by-block-number`

Request:

```json
{
  "block_numbers": [10, 11]
}
```

Response:

```json
{
  "transactions_count_map": {
    "10": 2,
    "11": 0
  },
  "errors": []
}
```

### `POST /receipts/by-block-number`

Request:

```json
{
  "block_numbers": [16]
}
```

Response:

```json
{
  "receipts": [],
  "logs": [],
  "errors": []
}
```

Receipt shape:

```json
{
  "transaction_hash": "0x...",
  "transaction_index": 0,
  "block_hash": "0x...",
  "block_number": 16,
  "cumulative_gas_used": 21000,
  "gas_used": 21000,
  "gas_price": 1,
  "created_contract_address_hash": "0x...",
  "status": "success",
  "logs_bloom": "0x..."
}
```

Log shape:

```json
{
  "address_hash": "0x...",
  "topics": ["0x..."],
  "data": "0x...",
  "block_hash": "0x...",
  "block_number": 16,
  "transaction_hash": "0x...",
  "transaction_index": 0,
  "index": 0
}
```

### `POST /receipts/by-transaction-hash`

Request:

```json
{
  "hashes": ["0xtx1", "0xtx2"]
}
```

Response:

```json
{
  "receipts": [],
  "logs": [],
  "errors": []
}
```

### `POST /logs/search`

Request:

```json
{
  "from_block": 10,
  "to_block": 12,
  "block_hash": null,
  "address": ["0x1", "0x2"],
  "topics": [["0xtopic0"], null]
}
```

Response:

```json
{
  "logs": []
}
```

### `POST /state/balances`

Request:

```json
{
  "requests": [
    {
      "address_hash": "0xabc",
      "block_number": 12
    }
  ]
}
```

Response:

```json
{
  "balances": [
    {
      "address_hash": "0xabc",
      "block_number": 12,
      "value": 100
    }
  ],
  "errors": []
}
```

### `POST /state/nonces`

Request:

```json
{
  "requests": [
    {
      "address_hash": "0xabc",
      "block_number": 12
    }
  ]
}
```

Response:

```json
{
  "nonces": [
    {
      "address_hash": "0xabc",
      "block_number": 12,
      "value": 4
    }
  ],
  "errors": []
}
```

### `POST /state/codes`

Request:

```json
{
  "requests": [
    {
      "address_hash": "0xabc",
      "block_number": 12
    }
  ]
}
```

Response:

```json
{
  "codes": [
    {
      "address_hash": "0xabc",
      "block_number": 12,
      "value": "0x6000"
    }
  ],
  "errors": []
}
```

Important:

- `value` must be deployed bytecode hex
- returning `SourceMap` or source metadata here is not sufficient

## Suggested STRATO Module Layout

Suggested implementation under `strato/api/core/src/Handlers/Blockscout/`:

- `API.hs`
- `ChainInfo.hs`
- `Blocks.hs`
- `Transactions.hs`
- `Receipts.hs`
- `Logs.hs`
- `State.hs`
- `Mapper.hs`

## Initial Mapping Strategy

- chain info:
  - reuse metadata and latest-block queries
- blocks:
  - reuse existing block SQL query logic
- transactions:
  - reuse existing transaction query logic
- balances / nonces:
  - reuse account query logic
- receipts:
  - derive from transaction result data plus block ordering and emitted logs
- logs:
  - new log/event search path required
- codes:
  - new deployed-bytecode path required
