
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

;; NEW ERROR CONSTANTS
(define-constant ERR-LOAN-NOT-FOUND (err u115))
(define-constant ERR-INSUFFICIENT-COLLATERAL (err u116))
(define-constant ERR-LIQUIDATION-THRESHOLD-REACHED (err u117))
(define-constant ERR-ORACLE-NOT-FOUND (err u118))
(define-constant ERR-STALE-PRICE (err u119))
(define-constant ERR-FARMING-NOT-ACTIVE (err u120))
(define-constant ERR-LOCK-PERIOD-NOT-EXPIRED (err u121))
(define-constant ERR-GOVERNANCE-PROPOSAL-NOT-FOUND (err u122))
(define-constant ERR-VOTING-PERIOD-ENDED (err u123))
(define-constant ERR-INSUFFICIENT-VOTING-POWER (err u124))
(define-constant ERR-INSURANCE-CLAIM-REJECTED (err u125))
(define-constant ERR-NFT-NOT-FOUND (err u126))
(define-constant ERR-CROSS-CHAIN-BRIDGE-PAUSED (err u127))

;; NEW DATA VARIABLES
(define-data-var total-loans-issued uint u0)
(define-data-var total-collateral-locked uint u0)
(define-data-var governance-proposal-counter uint u0)
(define-data-var total-staked-tokens uint u0)
(define-data-var insurance-pool-balance uint u0)
(define-data-var cross-chain-nonce uint u0)

;; NEW MAPS
(define-map lending-pools
  { token: principal }
  {
    total-supplied: uint,
    total-borrowed: uint,
    supply-rate: uint,
    borrow-rate: uint,
    collateral-factor: uint,
    liquidation-threshold: uint,
    is-active: bool
  }
)

(define-map user-loans
  { loan-id: uint }
  {
    borrower: principal,
    collateral-token: principal,
    collateral-amount: uint,
    borrowed-token: principal,
    borrowed-amount: uint,
    interest-rate: uint,
    liquidation-price: uint,
    creation-time: uint,
    is-active: bool
  }
)

(define-map user-supplies
  { user: principal, token: principal }
  {
    amount: uint,
    earned-interest: uint,
    last-update-time: uint
  }
)

(define-map price-oracles
  { token: principal }
  {
    price: uint,
    decimals: uint,
    last-update-time: uint,
    oracle-address: principal,
    is-active: bool
  }
)

(define-map oracle-feeds
  { feed-id: uint }
  {
    name: (string-ascii 32),
    token: principal,
    data-source: (string-ascii 64),
    update-frequency: uint,
    is-verified: bool
  }
)

;; YIELD FARMING & STAKING
(define-map farming-pools
  { farm-id: uint }
  {
    name: (string-ascii 32),
    staking-token: principal,
    reward-token: principal,
    total-staked: uint,
    reward-rate: uint,
    start-time: uint,
    end-time: uint,
    is-active: bool
  }
)

(define-map user-stakes
  { user: principal, farm-id: uint }
  {
    staked-amount: uint,
    reward-debt: uint,
    last-stake-time: uint,
    lock-end-time: uint
  }
)

(define-map governance-proposals
  { proposal-id: uint }
  {
    title: (string-ascii 64),
    description: (string-ascii 256),
    proposer: principal,
    voting-start: uint,
    voting-end: uint,
    votes-for: uint,
    votes-against: uint,
    executed: bool,
    proposal-type: uint
  }
)

(define-map user-votes
  { user: principal, proposal-id: uint }
  {
    vote: bool,
    voting-power: uint,
    timestamp: uint
  }
)

(define-map governance-tokens
  { user: principal }
  {
    balance: uint,
    delegated-to: (optional principal),
    voting-power: uint
  }
)

;; INSURANCE SYSTEM
(define-map insurance-policies
  { policy-id: uint }
  {
    holder: principal,
    coverage-amount: uint,
    premium-paid: uint,
    coverage-type: uint,
    start-time: uint,
    end-time: uint,
    is-active: bool
  }
)

(define-map insurance-claims
  { claim-id: uint }
  {
    policy-id: uint,
    claimant: principal,
    claim-amount: uint,
    claim-type: uint,
    evidence-hash: (buff 32),
    status: uint,
    submitted-at: uint
  }
)

;; NFT COLLATERAL SYSTEM
(define-map nft-collateral
  { nft-id: uint }
  {
    owner: principal,
    collection-address: principal,
    token-id: uint,
    appraised-value: uint,
    is-collateralized: bool,
    loan-id: (optional uint)
  }
)

(define-map nft-collections
  { collection: principal }
  {
    floor-price: uint,
    is-approved: bool,
    collateral-factor: uint,
    last-price-update: uint
  }
)

;; CROSS-CHAIN BRIDGE SYSTEM
(define-map bridge-transactions
  { tx-id: uint }
  {
    sender: principal,
    recipient: (buff 20),
    token: principal,
    amount: uint,
    target-chain: uint,
    status: uint,
    created-at: uint,
    completed-at: (optional uint)
  }
)
(define-map supported-chains
  { chain-id: uint }
  {
    name: (string-ascii 32),
    is-active: bool,
    bridge-fee-bps: uint,
    confirmation-blocks: uint
  }
)

;; NEW COUNTERS
(define-data-var next-loan-id uint u1)
(define-data-var next-farm-id uint u1)
(define-data-var next-proposal-id uint u1)
(define-data-var next-policy-id uint u1)
(define-data-var next-claim-id uint u1)
(define-data-var next-nft-id uint u1)
(define-data-var next-bridge-tx-id uint u1)
(define-data-var next-oracle-feed-id uint u1)

;; LENDING & BORROWING SYSTEM
(define-public (create-lending-pool 
  (token principal) 
  (collateral-factor uint) 
  (liquidation-threshold uint)
)
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (asserts! (is-token-whitelisted token) ERR-INVALID-TOKEN)
    (asserts! (<= collateral-factor u8000) ERR-INVALID-AMOUNT) ;; Max 80% collateral factor
    (asserts! (<= liquidation-threshold u9000) ERR-INVALID-AMOUNT) ;; Max 90% liquidation threshold
    
    (map-set lending-pools
      { token: token }
      {
        total-supplied: u0,
        total-borrowed: u0,
        supply-rate: u500, ;; 5% default
        borrow-rate: u800, ;; 8% default
        collateral-factor: collateral-factor,
        liquidation-threshold: liquidation-threshold,
        is-active: true
      }
    )
    (ok token)
  )
)


(define-public (supply-to-lending-pool (token principal) (amount uint))
  (let
    (
      (pool (unwrap! (map-get? lending-pools { token: token }) ERR-POOL-NOT-FOUND))
      (current-supply (default-to { amount: u0, earned-interest: u0, last-update-time: u0 } 
                       (map-get? user-supplies { user: tx-sender, token: token })))
    )
    (asserts! (get is-active pool) ERR-PROTOCOL-PAUSED)
    (asserts! (> amount u0) ERR-INVALID-AMOUNT)
    
    ;; Update user supply
    (map-set user-supplies
      { user: tx-sender, token: token }
      {
        amount: (+ (get amount current-supply) amount),
        earned-interest: (get earned-interest current-supply),
        last-update-time: stacks-block-height
      }
    )
    
    ;; Update pool totals
    (map-set lending-pools
      { token: token }
      (merge pool { total-supplied: (+ (get total-supplied pool) amount) })
    )
    
    (ok amount)
  )
)

(define-read-only (get-loan (loan-id uint))
  (map-get? user-loans { loan-id: loan-id })
)

(define-read-only (get-lending-pool (token principal))
  (map-get? lending-pools { token: token })
)

;; PRICE ORACLE SYSTEM
(define-public (add-price-oracle 
  (token principal) 
  (initial-price uint) 
  (decimals uint) 
  (oracle-address principal)
)
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (asserts! (is-token-whitelisted token) ERR-INVALID-TOKEN)
    
    (map-set price-oracles
      { token: token }
      {
        price: initial-price,
        decimals: decimals,
        last-update-time: stacks-block-height,
        oracle-address: oracle-address,
        is-active: true
      }
    )
    (ok token)
  )
)

(define-public (update-token-price (token principal) (new-price uint))
  (let
    (
      (oracle (unwrap! (map-get? price-oracles { token: token }) ERR-ORACLE-NOT-FOUND))
    )
    (asserts! (is-eq tx-sender (get oracle-address oracle)) ERR-NOT-AUTHORIZED)
    (asserts! (get is-active oracle) ERR-ORACLE-NOT-FOUND)
    
    (map-set price-oracles
      { token: token }
      (merge oracle { 
        price: new-price,
        last-update-time: stacks-block-height
      })
    )
    (ok new-price)
  )
)

(define-read-only (get-token-price (token principal))
  (default-to u100000000 ;; Default price if oracle not found
    (get price (map-get? price-oracles { token: token })))
)

(define-read-only (is-price-fresh (token principal) (max-age uint))
  (let
    (
      (oracle (map-get? price-oracles { token: token }))
    )
    (match oracle
      oracle-data (< (- stacks-block-height (get last-update-time oracle-data)) max-age)
      false
    )
  )
)

;; YIELD FARMING & STAKING
(define-public (create-farming-pool
  (name (string-ascii 32))
  (staking-token principal)
  (reward-token principal)
  (reward-rate uint)
  (duration uint)
)
  (let
    (
      (farm-id (var-get next-farm-id))
      (start-time stacks-block-height)
      (end-time (+ stacks-block-height duration))
    )
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (asserts! (is-token-whitelisted staking-token) ERR-INVALID-TOKEN)
    (asserts! (is-token-whitelisted reward-token) ERR-INVALID-TOKEN)
    
    (map-set farming-pools
      { farm-id: farm-id }
      {
        name: name,
        staking-token: staking-token,
        reward-token: reward-token,
        total-staked: u0,
        reward-rate: reward-rate,
        start-time: start-time,
        end-time: end-time,
        is-active: true
      }
    )
    
    (var-set next-farm-id (+ farm-id u1))
    (ok farm-id)
  )
)

(define-public (stake-in-farm (farm-id uint) (amount uint) (lock-period uint))
  (let
    (
      (farm (unwrap! (map-get? farming-pools { farm-id: farm-id }) ERR-POOL-NOT-FOUND))
      (current-stake (default-to { staked-amount: u0, reward-debt: u0, last-stake-time: u0, lock-end-time: u0 }
                      (map-get? user-stakes { user: tx-sender, farm-id: farm-id })))
    )
    (asserts! (get is-active farm) ERR-FARMING-NOT-ACTIVE)
    (asserts! (> amount u0) ERR-INVALID-AMOUNT)
    (asserts! (<= stacks-block-height (get end-time farm)) ERR-DEADLINE-PASSED)
    
    (map-set user-stakes
      { user: tx-sender, farm-id: farm-id }
      {
        staked-amount: (+ (get staked-amount current-stake) amount),
        reward-debt: u0,
        last-stake-time: stacks-block-height,
        lock-end-time: (+ stacks-block-height lock-period)
      }
    )
    
    (map-set farming-pools
      { farm-id: farm-id }
      (merge farm { total-staked: (+ (get total-staked farm) amount) })
    )
    
    (var-set total-staked-tokens (+ (var-get total-staked-tokens) amount))
    (ok amount)
  )
)

(define-public (unstake-from-farm (farm-id uint) (amount uint))
  (let
    (
      (farm (unwrap! (map-get? farming-pools { farm-id: farm-id }) ERR-POOL-NOT-FOUND))
      (user-stake (unwrap! (map-get? user-stakes { user: tx-sender, farm-id: farm-id }) ERR-INSUFFICIENT-BALANCE))
    )
    (asserts! (>= (get staked-amount user-stake) amount) ERR-INSUFFICIENT-BALANCE)
    (asserts! (>= stacks-block-height (get lock-end-time user-stake)) ERR-LOCK-PERIOD-NOT-EXPIRED)
    
    (map-set user-stakes
      { user: tx-sender, farm-id: farm-id }
      (merge user-stake { staked-amount: (- (get staked-amount user-stake) amount) })
    )
    
    (map-set farming-pools
      { farm-id: farm-id }
      (merge farm { total-staked: (- (get total-staked farm) amount) })
    )
    
    (var-set total-staked-tokens (- (var-get total-staked-tokens) amount))
    (ok amount)
  )
)

(define-read-only (get-farming-pool (farm-id uint))
  (map-get? farming-pools { farm-id: farm-id })
)

(define-read-only (get-user-stake (user principal) (farm-id uint))
  (map-get? user-stakes { user: user, farm-id: farm-id })
)