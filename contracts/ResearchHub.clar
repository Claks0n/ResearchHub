;; ResearchHub - Academic research validation and peer review platform
;; Researchers earn tokens based on paper validation and quality reviews

;; Error codes
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_NOT_FOUND (err u101))
(define-constant ERR_ALREADY_EXISTS (err u102))
(define-constant ERR_INVALID_INPUT (err u103))
(define-constant ERR_ALREADY_VERIFIED (err u104))
(define-constant ERR_ALREADY_RATED (err u105))
(define-constant ERR_SELF_RATING (err u106))
(define-constant ERR_EMPTY_STRING (err u107))
(define-constant ERR_INVALID_RATING (err u108))
(define-constant ERR_INVALID_PAPER_ID (err u109))
(define-constant ERR_EMPTY_HASH (err u110))

;; Constants
(define-constant MAX_RATING u5)
(define-constant REVIEW_REWARD u10)
(define-constant EXCELLENCE_REWARD u20)
(define-constant PUBLICATION_REWARD u50)

;; Data maps
(define-map researchers
  { researcher-id: principal }
  { name: (string-ascii 50), field: (string-ascii 20), reputation: uint, tokens: uint, published: bool }
)

(define-map research-papers
  { paper-id: uint }
  { 
    author: principal, 
    description: (string-ascii 500), 
    paper-hash: (buff 32),
    timestamp: uint, 
    verified: bool,
    review-count: uint,
    endorsement-count: uint,
    quality-rating: uint,
    rating-count: uint
  }
)

(define-map paper-reviews
  { paper-id: uint, reviewer: principal }
  { reviewed: bool }
)

(define-map paper-endorsements
  { paper-id: uint, endorser: principal }
  { endorsement-level: uint, endorsement-date: uint }
)

(define-map quality-ratings
  { paper-id: uint, rater: principal }
  { rating: uint }
)

;; Variables
(define-data-var next-paper-id uint u1)
(define-data-var action-counter uint u0)

;; Helper functions
(define-private (is-valid-paper-id (paper-id uint))
  (< paper-id (var-get next-paper-id))
)

;; Researcher functions
(define-public (register-researcher (name (string-ascii 50)) (field (string-ascii 20)))
  (let ((caller tx-sender))
    (asserts! (> (len name) u0) ERR_EMPTY_STRING)
    (asserts! (or (is-eq field "biology") (is-eq field "physics") (is-eq field "chemistry")) ERR_INVALID_INPUT)
    (asserts! (is-none (map-get? researchers {researcher-id: caller})) ERR_ALREADY_EXISTS)
    (ok (map-set researchers 
      {researcher-id: caller} 
      {name: name, field: field, reputation: u0, tokens: u100, published: false}))
  )
)

(define-public (update-researcher (name (string-ascii 50)) (field (string-ascii 20)))
  (let ((caller tx-sender))
    (asserts! (> (len name) u0) ERR_EMPTY_STRING)
    (asserts! (or (is-eq field "biology") (is-eq field "physics") (is-eq field "chemistry")) ERR_INVALID_INPUT)
    (asserts! (is-some (map-get? researchers {researcher-id: caller})) ERR_NOT_FOUND)
    (ok (map-set researchers 
      {researcher-id: caller} 
      (merge (unwrap! (map-get? researchers {researcher-id: caller}) ERR_NOT_FOUND)
             {name: name, field: field})))
  )
)

;; Paper functions
(define-public (submit-paper (description (string-ascii 500)) (paper-hash (buff 32)))
  (let ((caller tx-sender)
        (paper-id (var-get next-paper-id)))
    (asserts! (> (len description) u0) ERR_EMPTY_STRING)
    (asserts! (> (len paper-hash) u0) ERR_EMPTY_HASH)
    (asserts! (is-some (map-get? researchers {researcher-id: caller})) ERR_NOT_FOUND)
    (var-set action-counter (+ (var-get action-counter) u1))
    
    (map-set research-papers 
      {paper-id: paper-id} 
      { 
        author: caller, 
        description: description, 
        paper-hash: paper-hash,
        timestamp: (var-get action-counter), 
        verified: false,
        review-count: u0,
        endorsement-count: u0,
        quality-rating: u0,
        rating-count: u0
      })
    (var-set next-paper-id (+ paper-id u1))
    (ok paper-id)
  )
)

(define-public (review-paper (paper-id uint))
  (let ((caller tx-sender))
    (asserts! (is-valid-paper-id paper-id) ERR_INVALID_PAPER_ID)
    (asserts! (is-some (map-get? researchers {researcher-id: caller})) ERR_NOT_FOUND)
    (asserts! (is-some (map-get? research-papers {paper-id: paper-id})) ERR_NOT_FOUND)
    
    (let ((paper (unwrap! (map-get? research-papers {paper-id: paper-id}) ERR_NOT_FOUND)))
      (asserts! (not (is-eq caller (get author paper))) ERR_SELF_RATING)
      (asserts! (is-none (map-get? paper-reviews {paper-id: paper-id, reviewer: caller})) ERR_ALREADY_VERIFIED)
      
      (map-set paper-reviews 
        {paper-id: paper-id, reviewer: caller} 
        {reviewed: true})
      
      (let ((new-review-count (+ (get review-count paper) u1))
            (paper-author (unwrap! (map-get? researchers {researcher-id: (get author paper)}) ERR_NOT_FOUND))
            (reviewer-researcher (unwrap! (map-get? researchers {researcher-id: caller}) ERR_NOT_FOUND)))
        
        (map-set research-papers 
          {paper-id: paper-id} 
          (merge paper {
            review-count: new-review-count,
            verified: (>= new-review-count u3)
          }))
        
        (map-set researchers 
          {researcher-id: caller} 
          (merge reviewer-researcher {
            tokens: (+ (get tokens reviewer-researcher) u5),
            reputation: (+ (get reputation reviewer-researcher) u1)
          }))
        
        (if (and (>= new-review-count u3) (not (get verified paper)))
          (map-set researchers 
            {researcher-id: (get author paper)} 
            (merge paper-author {
              tokens: (+ (get tokens paper-author) PUBLICATION_REWARD),
              reputation: (+ (get reputation paper-author) u10),
              published: true
            }))
          true)
        
        (ok new-review-count)
      )
    )
  )
)

(define-public (endorse-paper (paper-id uint) (endorsement-level uint))
  (let ((caller tx-sender))
    (asserts! (is-valid-paper-id paper-id) ERR_INVALID_PAPER_ID)
    (asserts! (> endorsement-level u0) ERR_INVALID_INPUT)
    (asserts! (is-some (map-get? researchers {researcher-id: caller})) ERR_NOT_FOUND)
    (asserts! (is-some (map-get? research-papers {paper-id: paper-id})) ERR_NOT_FOUND)
    
    (let ((paper (unwrap! (map-get? research-papers {paper-id: paper-id}) ERR_NOT_FOUND)))
      (asserts! (get verified paper) ERR_UNAUTHORIZED)
      
      (map-set paper-endorsements 
        {paper-id: paper-id, endorser: caller} 
        {endorsement-level: endorsement-level, endorsement-date: (var-get action-counter)})
      
      (let ((new-endorsement-count (+ (get endorsement-count paper) endorsement-level))
            (paper-author (unwrap! (map-get? researchers {researcher-id: (get author paper)}) ERR_NOT_FOUND)))
        
        (map-set research-papers 
          {paper-id: paper-id} 
          (merge paper {endorsement-count: new-endorsement-count}))
        
        (map-set researchers 
          {researcher-id: (get author paper)} 
          (merge paper-author {
            tokens: (+ (get tokens paper-author) (* REVIEW_REWARD endorsement-level))
          }))
        
        (ok new-endorsement-count)
      )
    )
  )
)

(define-public (rate-paper-quality (paper-id uint) (rating uint))
  (let ((caller tx-sender))
    (asserts! (is-valid-paper-id paper-id) ERR_INVALID_PAPER_ID)
    (asserts! (and (>= rating u1) (<= rating MAX_RATING)) ERR_INVALID_RATING)
    (asserts! (is-some (map-get? researchers {researcher-id: caller})) ERR_NOT_FOUND)
    (asserts! (is-some (map-get? research-papers {paper-id: paper-id})) ERR_NOT_FOUND)
    
    (let ((paper (unwrap! (map-get? research-papers {paper-id: paper-id}) ERR_NOT_FOUND)))
      (asserts! (not (is-eq caller (get author paper))) ERR_SELF_RATING)
      (asserts! (is-none (map-get? quality-ratings {paper-id: paper-id, rater: caller})) ERR_ALREADY_RATED)
      
      (map-set quality-ratings 
        {paper-id: paper-id, rater: caller} 
        {rating: rating})
      
      (let ((current-total-rating (* (get quality-rating paper) (get rating-count paper)))
            (new-rating-count (+ (get rating-count paper) u1))
            (new-total-rating (+ current-total-rating rating))
            (new-average-rating (/ new-total-rating new-rating-count))
            (paper-author (unwrap! (map-get? researchers {researcher-id: (get author paper)}) ERR_NOT_FOUND))
            (rater-researcher (unwrap! (map-get? researchers {researcher-id: caller}) ERR_NOT_FOUND)))
        
        (map-set research-papers 
          {paper-id: paper-id} 
          (merge paper {
            quality-rating: new-average-rating,
            rating-count: new-rating-count
          }))
        
        (map-set researchers 
          {researcher-id: caller} 
          (merge rater-researcher {
            tokens: (+ (get tokens rater-researcher) u2),
            reputation: (+ (get reputation rater-researcher) u1)
          }))
        
        (if (>= rating u4)
          (map-set researchers 
            {researcher-id: (get author paper)} 
            (merge paper-author {
              tokens: (+ (get tokens paper-author) EXCELLENCE_REWARD),
              reputation: (+ (get reputation paper-author) u5)
            }))
          true)
        
        (ok new-average-rating)
      )
    )
  )
)

;; Read-only functions
(define-read-only (get-researcher-info (researcher-id principal))
  (map-get? researchers {researcher-id: researcher-id})
)

(define-read-only (get-paper (paper-id uint))
  (map-get? research-papers {paper-id: paper-id})
)

(define-read-only (get-paper-review (paper-id uint) (reviewer principal))
  (map-get? paper-reviews {paper-id: paper-id, reviewer: reviewer})
)

(define-read-only (get-paper-endorsement (paper-id uint) (endorser principal))
  (map-get? paper-endorsements {paper-id: paper-id, endorser: endorser})
)

(define-read-only (get-quality-rating (paper-id uint) (rater principal))
  (map-get? quality-ratings {paper-id: paper-id, rater: rater})
)

(define-read-only (get-total-papers)
  (- (var-get next-paper-id) u1)
)