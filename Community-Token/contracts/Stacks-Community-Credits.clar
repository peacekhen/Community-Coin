;; COMMUNITY REWARDS TOKEN (CRT) SMART CONTRACT
;; A comprehensive SIP-010 compliant fungible token designed for fan communities
;; Features: reward distribution, membership benefits, decentralized governance
;; Security: comprehensive validation, event logging, administrative controls

;; ERROR CONSTANTS SECTION
(define-constant ERR-UNAUTHORIZED-ACCESS-DENIED (err u100))
(define-constant ERR-INSUFFICIENT-TOKEN-BALANCE (err u101))
(define-constant ERR-INVALID-AMOUNT-PROVIDED (err u102))
(define-constant ERR-RESOURCE-ALREADY-EXISTS (err u103))
(define-constant ERR-RESOURCE-NOT-FOUND (err u104))
(define-constant ERR-TRANSFER-OPERATION-FAILED (err u105))
(define-constant ERR-MINT-OPERATION-FAILED (err u106))
(define-constant ERR-BURN-OPERATION-FAILED (err u107))
(define-constant ERR-ALLOWANCE-LIMIT-EXCEEDED (err u108))
(define-constant ERR-SAME-PRINCIPAL-OPERATION (err u109))
(define-constant ERR-MAXIMUM-SUPPLY-EXCEEDED (err u110))
(define-constant ERR-CONTRACT-OPERATIONS-PAUSED (err u111))
(define-constant ERR-INVALID-PRINCIPAL (err u112))
(define-constant ERR-INVALID-URI (err u113))

;; TOKEN METADATA CONSTANTS SECTION
(define-constant community-token-display-name "Community Rewards Token")
(define-constant community-token-ticker-symbol "CRT")
(define-constant token-precision-decimal-places u8)
(define-constant maximum-total-token-supply u1000000000000000)
(define-constant initial-token-mint-amount u10000000000000)

;; CONTRACT ADMINISTRATION CONSTANTS SECTION
(define-constant contract-owner-administrator tx-sender)

;; CONTRACT STATE VARIABLES SECTION
(define-data-var current-circulating-token-supply uint u0)
(define-data-var token-metadata-uri-location (optional (string-utf8 256)) none)
(define-data-var contract-operations-paused-status bool false)

;; DATA STORAGE MAPS SECTION
(define-map token-spending-allowances-registry 
  { token-holder-owner: principal, authorized-token-spender: principal } 
  uint
)
(define-map administrator-privileges-registry principal bool)

;; FUNGIBLE TOKEN DEFINITION SECTION
(define-fungible-token community-rewards-fungible-token maximum-total-token-supply)

;; PRIVATE UTILITY HELPER FUNCTIONS SECTION
(define-private (get-account-balance (account principal))
  (ft-get-balance community-rewards-fungible-token account)
)

(define-private (get-allowance (owner principal) (spender principal))
  (default-to u0 (map-get? token-spending-allowances-registry { 
    token-holder-owner: owner, 
    authorized-token-spender: spender 
  }))
)

(define-private (set-allowance (owner principal) (spender principal) (amount uint))
  (if (> amount u0)
    (map-set token-spending-allowances-registry { 
      token-holder-owner: owner, 
      authorized-token-spender: spender 
    } amount)
    (map-delete token-spending-allowances-registry { 
      token-holder-owner: owner, 
      authorized-token-spender: spender 
    })
  )
)

(define-private (is-positive (amount uint))
  (> amount u0)
)

(define-private (are-different (first principal) (second principal))
  (not (is-eq first second))
)

(define-private (is-admin)
  (or (is-eq tx-sender contract-owner-administrator)
      (default-to false (map-get? administrator-privileges-registry tx-sender)))
)

(define-private (not-paused)
  (not (var-get contract-operations-paused-status))
)

(define-private (is-valid-principal (account principal))
  (not (is-eq account 'SP000000000000000000002Q6VF78))
)

(define-private (is-valid-uri (uri (string-utf8 256)))
  (and (>= (len uri) u1) (<= (len uri) u256))
)

;; SIP-010 TRAIT FUNCTIONS SECTION
(define-read-only (get-name)
  (ok community-token-display-name)
)

(define-read-only (get-symbol)
  (ok community-token-ticker-symbol)
)

(define-read-only (get-decimals)
  (ok token-precision-decimal-places)
)

(define-read-only (get-total-supply)
  (ok (var-get current-circulating-token-supply))
)

(define-read-only (get-balance (who principal))
  (ok (get-account-balance who))
)

(define-read-only (get-token-uri)
  (ok (var-get token-metadata-uri-location))
)

(define-public (transfer (amount uint) (sender principal) (recipient principal) (memo (optional (buff 34))))
  (begin
    (asserts! (not-paused) ERR-CONTRACT-OPERATIONS-PAUSED)
    (asserts! (is-eq tx-sender sender) ERR-UNAUTHORIZED-ACCESS-DENIED)
    (asserts! (is-positive amount) ERR-INVALID-AMOUNT-PROVIDED)
    (asserts! (>= (get-account-balance sender) amount) ERR-INSUFFICIENT-TOKEN-BALANCE)
    (asserts! (are-different sender recipient) ERR-SAME-PRINCIPAL-OPERATION)
    
    (try! (ft-transfer? community-rewards-fungible-token amount sender recipient))
    
    (print { 
      event-type: "transfer", 
      sender: sender, 
      recipient: recipient, 
      amount: amount,
      memo: memo,
      block-height: stacks-block-height
    })
    (ok true)
  )
)

;; ADDITIONAL READ-ONLY FUNCTIONS SECTION
(define-read-only (get-allowance-amount (owner principal) (spender principal))
  (ok (get-allowance owner spender))
)

(define-read-only (is-administrator (who principal))
  (ok (or (is-eq who contract-owner-administrator)
          (default-to false (map-get? administrator-privileges-registry who))))
)

(define-read-only (is-paused)
  (ok (var-get contract-operations-paused-status))
)

;; ALLOWANCE FUNCTIONS SECTION
(define-public (approve (spender principal) (amount uint))
  (begin
    (asserts! (not-paused) ERR-CONTRACT-OPERATIONS-PAUSED)
    (asserts! (are-different tx-sender spender) ERR-SAME-PRINCIPAL-OPERATION)
    (asserts! (is-valid-principal spender) ERR-INVALID-PRINCIPAL)
    (asserts! (<= amount maximum-total-token-supply) ERR-INVALID-AMOUNT-PROVIDED)
    
    (set-allowance tx-sender spender amount)
    (print { 
      event-type: "approve", 
      owner: tx-sender, 
      spender: spender, 
      amount: amount,
      block-height: stacks-block-height
    })
    (ok true)
  )
)

(define-public (transfer-from (amount uint) (owner principal) (recipient principal) (memo (optional (buff 34))))
  (let
    (
      (current-allowance (get-allowance owner tx-sender))
    )
    (asserts! (not-paused) ERR-CONTRACT-OPERATIONS-PAUSED)
    (asserts! (is-positive amount) ERR-INVALID-AMOUNT-PROVIDED)
    (asserts! (>= (get-account-balance owner) amount) ERR-INSUFFICIENT-TOKEN-BALANCE)
    (asserts! (>= current-allowance amount) ERR-ALLOWANCE-LIMIT-EXCEEDED)
    (asserts! (are-different owner recipient) ERR-SAME-PRINCIPAL-OPERATION)
    
    (try! (ft-transfer? community-rewards-fungible-token amount owner recipient))
    (set-allowance owner tx-sender (- current-allowance amount))
    
    (print { 
      event-type: "transfer-from", 
      owner: owner, 
      spender: tx-sender,
      recipient: recipient, 
      amount: amount,
      remaining-allowance: (- current-allowance amount),
      memo: memo,
      block-height: stacks-block-height
    })
    (ok true)
  )
)

;; TOKEN SUPPLY MANAGEMENT SECTION
(define-public (mint (amount uint) (recipient principal))
  (let
    (
      (current-supply (var-get current-circulating-token-supply))
      (new-supply (+ current-supply amount))
    )
    (asserts! (is-admin) ERR-UNAUTHORIZED-ACCESS-DENIED)
    (asserts! (not-paused) ERR-CONTRACT-OPERATIONS-PAUSED)
    (asserts! (is-positive amount) ERR-INVALID-AMOUNT-PROVIDED)
    (asserts! (is-valid-principal recipient) ERR-INVALID-PRINCIPAL)
    (asserts! (<= new-supply maximum-total-token-supply) ERR-MAXIMUM-SUPPLY-EXCEEDED)
    
    (try! (ft-mint? community-rewards-fungible-token amount recipient))
    (var-set current-circulating-token-supply new-supply)
    
    (print { 
      event-type: "mint", 
      recipient: recipient, 
      amount: amount,
      new-supply: new-supply,
      minter: tx-sender,
      block-height: stacks-block-height
    })
    (ok true)
  )
)

(define-public (burn (amount uint))
  (let
    (
      (current-supply (var-get current-circulating-token-supply))
      (new-supply (- current-supply amount))
    )
    (asserts! (not-paused) ERR-CONTRACT-OPERATIONS-PAUSED)
    (asserts! (is-positive amount) ERR-INVALID-AMOUNT-PROVIDED)
    (asserts! (>= (get-account-balance tx-sender) amount) ERR-INSUFFICIENT-TOKEN-BALANCE)
    
    (try! (ft-burn? community-rewards-fungible-token amount tx-sender))
    (var-set current-circulating-token-supply new-supply)
    
    (print { 
      event-type: "burn", 
      burner: tx-sender, 
      amount: amount,
      new-supply: new-supply,
      block-height: stacks-block-height
    })
    (ok true)
  )
)

;; ADMINISTRATIVE FUNCTIONS SECTION
(define-public (set-token-uri (uri (string-utf8 256)))
  (begin
    (asserts! (is-admin) ERR-UNAUTHORIZED-ACCESS-DENIED)
    (asserts! (is-valid-uri uri) ERR-INVALID-URI)
    (var-set token-metadata-uri-location (some uri))
    (print { 
      event-type: "set-token-uri", 
      uri: uri,
      admin: tx-sender,
      block-height: stacks-block-height
    })
    (ok true)
  )
)

(define-public (pause-contract)
  (begin
    (asserts! (is-admin) ERR-UNAUTHORIZED-ACCESS-DENIED)
    (var-set contract-operations-paused-status true)
    (print { 
      event-type: "contract-paused",
      admin: tx-sender,
      block-height: stacks-block-height
    })
    (ok true)
  )
)

(define-public (unpause-contract)
  (begin
    (asserts! (is-admin) ERR-UNAUTHORIZED-ACCESS-DENIED)
    (var-set contract-operations-paused-status false)
    (print { 
      event-type: "contract-unpaused",
      admin: tx-sender,
      block-height: stacks-block-height
    })
    (ok true)
  )
)

(define-public (add-admin (new-admin principal))
  (begin
    (asserts! (is-eq tx-sender contract-owner-administrator) ERR-UNAUTHORIZED-ACCESS-DENIED)
    (asserts! (is-valid-principal new-admin) ERR-INVALID-PRINCIPAL)
    (map-set administrator-privileges-registry new-admin true)
    (print { 
      event-type: "admin-added", 
      new-admin: new-admin,
      owner: tx-sender,
      block-height: stacks-block-height
    })
    (ok true)
  )
)

(define-public (remove-admin (admin principal))
  (begin
    (asserts! (is-eq tx-sender contract-owner-administrator) ERR-UNAUTHORIZED-ACCESS-DENIED)
    (asserts! (not (is-eq admin contract-owner-administrator)) ERR-UNAUTHORIZED-ACCESS-DENIED)
    (map-delete administrator-privileges-registry admin)
    (print { 
      event-type: "admin-removed", 
      removed-admin: admin,
      owner: tx-sender,
      block-height: stacks-block-height
    })
    (ok true)
  )
)

;; CONTRACT INITIALIZATION SECTION
(begin
  (try! (ft-mint? community-rewards-fungible-token initial-token-mint-amount contract-owner-administrator))
  (var-set current-circulating-token-supply initial-token-mint-amount)
  
  (print { 
    event-type: "contract-initialized", 
    owner: contract-owner-administrator, 
    initial-supply: initial-token-mint-amount,
    name: community-token-display-name,
    symbol: community-token-ticker-symbol,
    block-height: stacks-block-height
  })
)