;; PoolRidge Bond Protocol - Simplified Core Contract
;; Tri-token ecosystem: Pool, Ridge, and Bond tokens

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-insufficient-balance (err u101))
(define-constant err-invalid-amount (err u102))
(define-constant err-pool-not-found (err u103))
(define-constant err-bond-not-mature (err u104))
(define-constant err-insufficient-collateral (err u105))

;; Token definitions
(define-fungible-token pool-token)
(define-fungible-token ridge-token)
(define-fungible-token bond-token)

;; Data Variables
(define-data-var total-liquidity uint u0)
(define-data-var protocol-fee-rate uint u30) ;; 0.3% = 30 basis points
(define-data-var bond-interest-rate uint u500) ;; 5% = 500 basis points
(define-data-var min-collateral-ratio uint u15000) ;; 150% = 15000 basis points

;; Data Maps
(define-map liquidity-pools
  { provider: principal }
  {
    pool-balance: uint,
    ridge-balance: uint,
    timestamp: uint
  }
)

(define-map bonds
  { bond-id: uint }
  {
    owner: principal,
    principal-amount: uint,
    maturity-block: uint,
    interest-rate: uint,
    is-redeemed: bool
  }
)

(define-map trading-positions
  { trader: principal, position-id: uint }
  {
    collateral: uint,
    size: uint,
    entry-price: uint,
    is-long: bool,
    is-active: bool
  }
)

(define-data-var next-bond-id uint u0)
(define-data-var next-position-id uint u0)

;; Read-only functions
(define-read-only (get-pool-balance (provider principal))
  (default-to 
    { pool-balance: u0, ridge-balance: u0, timestamp: u0 }
    (map-get? liquidity-pools { provider: provider })
  )
)

(define-read-only (get-bond (bond-id uint))
  (map-get? bonds { bond-id: bond-id })
)

(define-read-only (get-position (trader principal) (position-id uint))
  (map-get? trading-positions { trader: trader, position-id: position-id })
)

(define-read-only (get-total-liquidity)
  (ok (var-get total-liquidity))
)

(define-read-only (get-token-balance (token-type (string-ascii 10)) (account principal))
  (if (is-eq token-type "pool")
    (ok (ft-get-balance pool-token account))
    (if (is-eq token-type "ridge")
      (ok (ft-get-balance ridge-token account))
      (if (is-eq token-type "bond")
        (ok (ft-get-balance bond-token account))
        (err u106)
      )
    )
  )
)

;; Private functions
(define-private (calculate-fee (amount uint))
  (/ (* amount (var-get protocol-fee-rate)) u10000)
)

(define-private (calculate-bond-return (principal-amount uint) (blocks uint))
  (let (
    (interest (/ (* principal-amount (var-get bond-interest-rate) blocks) (* u10000 u52560)))
  )
    (+ principal-amount interest)
  )
)

;; Public functions - Liquidity Management
(define-public (add-liquidity (amount uint))
  (let (
    (sender tx-sender)
    (current-pool (get-pool-balance sender))
    (new-pool-balance (+ (get pool-balance current-pool) amount))
    (pool-tokens-to-mint amount)
    (ridge-tokens-to-mint (/ amount u10))
  )
    (asserts! (> amount u0) err-invalid-amount)
    
    ;; Mint pool tokens
    (try! (ft-mint? pool-token pool-tokens-to-mint sender))
    
    ;; Mint ridge tokens (10:1 ratio)
    (try! (ft-mint? ridge-token ridge-tokens-to-mint sender))
    
    ;; Update liquidity pool
    (map-set liquidity-pools
      { provider: sender }
      {
        pool-balance: new-pool-balance,
        ridge-balance: (+ (get ridge-balance current-pool) ridge-tokens-to-mint),
        timestamp: block-height
      }
    )
    
    ;; Update total liquidity
    (var-set total-liquidity (+ (var-get total-liquidity) amount))
    
    (ok { pool-tokens: pool-tokens-to-mint, ridge-tokens: ridge-tokens-to-mint })
  )
)

(define-public (remove-liquidity (pool-token-amount uint))
  (let (
    (sender tx-sender)
    (current-pool (get-pool-balance sender))
    (pool-balance (get pool-balance current-pool))
  )
    (asserts! (>= (ft-get-balance pool-token sender) pool-token-amount) err-insufficient-balance)
    (asserts! (> pool-token-amount u0) err-invalid-amount)
    
    ;; Burn pool tokens
    (try! (ft-burn? pool-token pool-token-amount sender))
    
    ;; Update liquidity pool
    (map-set liquidity-pools
      { provider: sender }
      (merge current-pool { pool-balance: (- pool-balance pool-token-amount) })
    )
    
    ;; Update total liquidity
    (var-set total-liquidity (- (var-get total-liquidity) pool-token-amount))
    
    (ok pool-token-amount)
  )
)

;; Bond functions
(define-public (create-bond (amount uint) (lock-blocks uint))
  (let (
    (sender tx-sender)
    (bond-id (var-get next-bond-id))
    (maturity-block (+ block-height lock-blocks))
  )
    (asserts! (> amount u0) err-invalid-amount)
    (asserts! (> lock-blocks u0) err-invalid-amount)
    
    ;; Mint bond tokens
    (try! (ft-mint? bond-token amount sender))
    
    ;; Create bond record
    (map-set bonds
      { bond-id: bond-id }
      {
        owner: sender,
        principal-amount: amount,
        maturity-block: maturity-block,
        interest-rate: (var-get bond-interest-rate),
        is-redeemed: false
      }
    )
    
    ;; Increment bond ID
    (var-set next-bond-id (+ bond-id u1))
    
    (ok bond-id)
  )
)

(define-public (redeem-bond (bond-id uint))
  (let (
    (sender tx-sender)
    (bond-data (unwrap! (get-bond bond-id) err-pool-not-found))
    (is-owner (is-eq sender (get owner bond-data)))
    (is-mature (>= block-height (get maturity-block bond-data)))
    (blocks-held (- block-height (get maturity-block bond-data)))
    (return-amount (calculate-bond-return (get principal-amount bond-data) blocks-held))
  )
    (asserts! is-owner err-owner-only)
    (asserts! is-mature err-bond-not-mature)
    (asserts! (not (get is-redeemed bond-data)) err-invalid-amount)
    
    ;; Burn bond tokens
    (try! (ft-burn? bond-token (get principal-amount bond-data) sender))
    
    ;; Mark as redeemed
    (map-set bonds
      { bond-id: bond-id }
      (merge bond-data { is-redeemed: true })
    )
    
    (ok return-amount)
  )
)

;; Trading functions
(define-public (open-position (collateral uint) (size uint) (is-long bool))
  (let (
    (sender tx-sender)
    (position-id (var-get next-position-id))
    (required-collateral (/ (* size (var-get min-collateral-ratio)) u10000))
  )
    (asserts! (>= collateral required-collateral) err-insufficient-collateral)
    (asserts! (> size u0) err-invalid-amount)
    
    ;; Create position
    (map-set trading-positions
      { trader: sender, position-id: position-id }
      {
        collateral: collateral,
        size: size,
        entry-price: u1000000, ;; Placeholder price
        is-long: is-long,
        is-active: true
      }
    )
    
    ;; Increment position ID
    (var-set next-position-id (+ position-id u1))
    
    (ok position-id)
  )
)

(define-public (close-position (position-id uint))
  (let (
    (sender tx-sender)
    (position (unwrap! (get-position sender position-id) err-pool-not-found))
  )
    (asserts! (get is-active position) err-invalid-amount)
    
    ;; Deactivate position
    (map-set trading-positions
      { trader: sender, position-id: position-id }
      (merge position { is-active: false })
    )
    
    (ok true)
  )
)

;; Governance functions (Ridge token holders)
(define-public (update-protocol-fee (new-fee uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (<= new-fee u1000) err-invalid-amount) ;; Max 10%
    (var-set protocol-fee-rate new-fee)
    (ok true)
  )
)

(define-public (update-bond-rate (new-rate uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (<= new-rate u2000) err-invalid-amount) ;; Max 20%
    (var-set bond-interest-rate new-rate)
    (ok true)
  )
)

;; Token transfer functions
(define-public (transfer-pool-token (amount uint) (recipient principal))
  (ft-transfer? pool-token amount tx-sender recipient)
)

(define-public (transfer-ridge-token (amount uint) (recipient principal))
  (ft-transfer? ridge-token amount tx-sender recipient)
)

(define-public (transfer-bond-token (amount uint) (recipient principal))
  (ft-transfer? bond-token amount tx-sender recipient)
)
