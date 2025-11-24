
;; title: Cross-Chain-Liquidity-Aggregator
;; Constants
(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-INSUFFICIENT-BALANCE (err u101))
(define-constant ERR-INVALID-AMOUNT (err u102))
(define-constant ERR-POOL-NOT-FOUND (err u103))
(define-constant ERR-SLIPPAGE-TOO-HIGH (err u104))
(define-constant ERR-DEADLINE-PASSED (err u105))
(define-constant ERR-INVALID-POOL-ID (err u106))
(define-constant ERR-INVALID-ROUTE (err u107))
(define-constant ERR-YIELD-STRATEGY-NOT-FOUND (err u108))
(define-constant ERR-INVALID-TOKEN (err u109))
(define-constant ERR-INSUFFICIENT-LIQUIDITY (err u110))
(define-constant ERR-PROTOCOL-PAUSED (err u111))
(define-constant ERR-INVALID-FEE-BPS (err u112))
(define-constant ERR-INVALID-REFERRAL (err u113))
(define-constant ERR-MAX-CAPACITY-REACHED (err u114))

;; Data variables
(define-data-var protocol-paused bool false)
(define-data-var protocol-fee-bps uint u20) ;; 0.2% default fee
(define-data-var treasury-address principal CONTRACT-OWNER)
(define-data-var referral-fee-bps uint u5) ;; 0.05% referral fee
(define-data-var protocol-fees-accumulated uint u0)

;; Data maps
(define-map liquidity-pools
  { pool-id: uint }
  {
    name: (string-ascii 32),
    token-a: principal,
    token-b: principal,
    reserve-a: uint,
    reserve-b: uint,
    liquidity-tokens: uint,
    fee-bps: uint,
    last-rebalance-time: uint,
    is-active: bool,
    max-capacity: uint,
    total-volume: uint,
    yield-strategy-id: (optional uint)
  }
)

(define-map pool-providers
  { pool-id: uint, provider: principal }
  { liquidity-tokens: uint }
)

(define-map user-deposits
  { user: principal, token: principal }
  { amount: uint, last-deposit-time: uint }
)

(define-map yield-strategies
  { strategy-id: uint }
  {
    name: (string-ascii 32),
    target-token: principal,
    apy-estimate: uint,
    risk-level: uint,
    is-active: bool,
    protocol: (string-ascii 32),
    min-lock-period: uint,
    rewards-token: (optional principal)
  }
)

(define-map route-configuration
  { route-id: uint }
  {
    name: (string-ascii 32),
    path: (list 10 uint),
    is-optimized: bool
  }
)

(define-map user-referrals
  { user: principal }
  { referrer: principal, fees-earned: uint }
)

(define-map token-whitelist
  { token: principal }
  { is-whitelisted: bool, decimals: uint }
)

;; Counters
(define-data-var next-pool-id uint u1)
(define-data-var next-strategy-id uint u1)
(define-data-var next-route-id uint u1)

;; Protocol status functions
(define-read-only (get-protocol-status)
  {
    paused: (var-get protocol-paused),
    fee-bps: (var-get protocol-fee-bps),
    treasury: (var-get treasury-address),
    referral-fee-bps: (var-get referral-fee-bps),
    fees-accumulated: (var-get protocol-fees-accumulated)
  }
)

(define-public (set-protocol-paused (paused bool))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (ok (var-set protocol-paused paused))
  )
)

(define-public (set-protocol-fee (fee-bps uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (asserts! (<= fee-bps u1000) ERR-INVALID-FEE-BPS) ;; Max 10% fee
    (ok (var-set protocol-fee-bps fee-bps))
  )
)

(define-public (set-referral-fee (fee-bps uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (asserts! (<= fee-bps u500) ERR-INVALID-FEE-BPS) ;; Max 5% referral fee
    (ok (var-set referral-fee-bps fee-bps))
  )
)

(define-public (set-treasury-address (new-address principal))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (ok (var-set treasury-address new-address))
  )
)

;; Token whitelist management
(define-public (whitelist-token (token principal) (decimals uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (ok (map-set token-whitelist { token: token } { is-whitelisted: true, decimals: decimals }))
  )
)

(define-public (remove-token-from-whitelist (token principal))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (ok (map-set token-whitelist { token: token } { is-whitelisted: false, decimals: u0 }))
  )
)

(define-read-only (is-token-whitelisted (token principal))
  (default-to false (get is-whitelisted (map-get? token-whitelist { token: token })))
)

;; Pool management functions
(define-read-only (get-pool (pool-id uint))
  (map-get? liquidity-pools { pool-id: pool-id })
)

(define-read-only (get-pool-count)
  (- (var-get next-pool-id) u1)
)

(define-public (set-pool-active-status (pool-id uint) (is-active bool))
  (let
    (
      (pool (unwrap! (map-get? liquidity-pools { pool-id: pool-id }) ERR-POOL-NOT-FOUND))
    )
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    
    (map-set liquidity-pools
      { pool-id: pool-id }
      (merge pool { is-active: is-active })
    )
    (ok is-active)
  )
)

(define-public (update-pool-fee (pool-id uint) (new-fee-bps uint))
  (let
    (
      (pool (unwrap! (map-get? liquidity-pools { pool-id: pool-id }) ERR-POOL-NOT-FOUND))
    )
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (asserts! (<= new-fee-bps u1000) ERR-INVALID-FEE-BPS) ;; Max 10% fee
    
    (map-set liquidity-pools
      { pool-id: pool-id }
      (merge pool { fee-bps: new-fee-bps })
    )
    (ok new-fee-bps)
  )
)

(define-read-only (get-route (route-id uint))
  (map-get? route-configuration { route-id: route-id })
)

(define-private (calculate-output-for-pool (pool-id uint) (prev-amount uint))
  (let
    (
      (pool (unwrap-panic (map-get? liquidity-pools { pool-id: pool-id })))
      (reserve-a (get reserve-a pool))
      (reserve-b (get reserve-b pool))
      (fee-bps (get fee-bps pool))
      (amount-in-with-fee (* prev-amount (- u10000 fee-bps)))
      (numerator (* amount-in-with-fee reserve-b))
      (denominator (+ (* reserve-a u10000) amount-in-with-fee))
    )
    (/ numerator denominator)
  )
)

;; Yield strategy functions
(define-public (create-yield-strategy
  (name (string-ascii 32))
  (target-token principal)
  (apy-estimate uint)
  (risk-level uint)
  (protocol (string-ascii 32))
  (min-lock-period uint)
  (rewards-token (optional principal))
)
  (let
    (
      (strategy-id (var-get next-strategy-id))
    )
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (asserts! (is-token-whitelisted target-token) ERR-INVALID-TOKEN)
    (asserts! (<= risk-level u10) ERR-INVALID-AMOUNT) ;; Risk scale from 0-10
    
    (map-set yield-strategies
      { strategy-id: strategy-id }
      {
        name: name,
        target-token: target-token,
        apy-estimate: apy-estimate,
        risk-level: risk-level,
        is-active: true,
        protocol: protocol,
        min-lock-period: min-lock-period,
        rewards-token: rewards-token
      }
    )
    
    (var-set next-strategy-id (+ strategy-id u1))
    (ok strategy-id)
  )
)

(define-read-only (get-yield-strategy (strategy-id uint))
  (map-get? yield-strategies { strategy-id: strategy-id })
)

(define-public (update-yield-strategy-apy (strategy-id uint) (new-apy-estimate uint))
  (let
    (
      (strategy (unwrap! (map-get? yield-strategies { strategy-id: strategy-id }) ERR-YIELD-STRATEGY-NOT-FOUND))
    )
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    
    (map-set yield-strategies
      { strategy-id: strategy-id }
      (merge strategy { apy-estimate: new-apy-estimate })
    )
    (ok new-apy-estimate)
  )
)

(define-public (set-yield-strategy-active (strategy-id uint) (is-active bool))
  (let
    (
      (strategy (unwrap! (map-get? yield-strategies { strategy-id: strategy-id }) ERR-YIELD-STRATEGY-NOT-FOUND))
    )
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    
    (map-set yield-strategies
      { strategy-id: strategy-id }
      (merge strategy { is-active: is-active })
    )
    (ok is-active)
  )
)

;; User position and analytics functions
(define-read-only (get-user-liquidity-position (user principal) (pool-id uint))
  (map-get? pool-providers { pool-id: pool-id, provider: user })
)

(define-read-only (get-user-deposit (user principal) (token principal))
  (map-get? user-deposits { user: user, token: token })
)