;; Rural Healthcare Access Platform
;; A comprehensive platform for managing rural healthcare services,
;; provider scheduling, mobile clinic coordination, and specialist consultations

;; Constants
(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-UNAUTHORIZED (err u100))
(define-constant ERR-NOT-FOUND (err u101))
(define-constant ERR-INVALID-TIME (err u102))
(define-constant ERR-ALREADY-EXISTS (err u103))
(define-constant ERR-INVALID-STATUS (err u104))
(define-constant ERR-APPOINTMENT-CONFLICT (err u105))

;; Provider status types
(define-constant STATUS-AVAILABLE u1)
(define-constant STATUS-BUSY u2)
(define-constant STATUS-OFFLINE u3)

;; Appointment status types
(define-constant APPT-SCHEDULED u1)
(define-constant APPT-CONFIRMED u2)
(define-constant APPT-COMPLETED u3)
(define-constant APPT-CANCELLED u4)

;; Clinic route status
(define-constant ROUTE-PLANNED u1)
(define-constant ROUTE-ACTIVE u2)
(define-constant ROUTE-COMPLETED u3)

;; Data structures
(define-map healthcare-providers
  { provider-id: uint }
  {
    name: (string-ascii 50),
    specialty: (string-ascii 30),
    location: (string-ascii 100),
    status: uint,
    rating: uint,
    contact: (string-ascii 50)
  }
)

(define-map appointments
  { appointment-id: uint }
  {
    patient-id: principal,
    provider-id: uint,
    appointment-time: uint,
    duration: uint,
    service-type: (string-ascii 50),
    status: uint,
    notes: (string-ascii 200),
    location: (string-ascii 100)
  }
)

(define-map mobile-clinic-routes
  { route-id: uint }
  {
    clinic-name: (string-ascii 50),
    route-date: uint,
    stops: (list 10 (string-ascii 100)),
    estimated-times: (list 10 uint),
    status: uint,
    assigned-providers: (list 5 uint)
  }
)

(define-map specialist-consultations
  { consultation-id: uint }
  {
    patient-id: principal,
    local-provider-id: uint,
    specialist-id: uint,
    consultation-time: uint,
    consultation-type: (string-ascii 30),
    status: uint,
    diagnosis: (string-ascii 200),
    recommendations: (string-ascii 300)
  }
)

(define-map transportation-requests
  { request-id: uint }
  {
    patient-id: principal,
    pickup-location: (string-ascii 100),
    destination: (string-ascii 100),
    requested-time: uint,
    appointment-id: (optional uint),
    status: uint,
    driver-assigned: (optional principal)
  }
)

;; Counter variables
(define-data-var provider-counter uint u0)
(define-data-var appointment-counter uint u0)
(define-data-var route-counter uint u0)
(define-data-var consultation-counter uint u0)
(define-data-var transport-counter uint u0)

;; Provider management functions
(define-public (register-provider 
    (name (string-ascii 50))
    (specialty (string-ascii 30))
    (location (string-ascii 100))
    (contact (string-ascii 50))
  )
  (let 
    (
      (provider-id (+ (var-get provider-counter) u1))
    )
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-UNAUTHORIZED)
    (map-set healthcare-providers
      { provider-id: provider-id }
      {
        name: name,
        specialty: specialty,
        location: location,
        status: STATUS-AVAILABLE,
        rating: u5,
        contact: contact
      }
    )
    (var-set provider-counter provider-id)
    (ok provider-id)
  )
)

(define-public (update-provider-status (provider-id uint) (new-status uint))
  (let
    (
      (provider (unwrap! (map-get? healthcare-providers { provider-id: provider-id }) ERR-NOT-FOUND))
    )
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-UNAUTHORIZED)
    (asserts! (or (is-eq new-status STATUS-AVAILABLE) 
                  (is-eq new-status STATUS-BUSY)
                  (is-eq new-status STATUS-OFFLINE)) ERR-INVALID-STATUS)
    (map-set healthcare-providers
      { provider-id: provider-id }
      (merge provider { status: new-status })
    )
    (ok true)
  )
)

;; Appointment scheduling functions
(define-public (schedule-appointment
    (provider-id uint)
    (appointment-time uint)
    (duration uint)
    (service-type (string-ascii 50))
    (location (string-ascii 100))
  )
  (let
    (
      (appointment-id (+ (var-get appointment-counter) u1))
      (provider (unwrap! (map-get? healthcare-providers { provider-id: provider-id }) ERR-NOT-FOUND))
    )
    (asserts! (is-eq (get status provider) STATUS-AVAILABLE) ERR-INVALID-STATUS)
    (asserts! (> appointment-time burn-block-height) ERR-INVALID-TIME)
    (map-set appointments
      { appointment-id: appointment-id }
      {
        patient-id: tx-sender,
        provider-id: provider-id,
        appointment-time: appointment-time,
        duration: duration,
        service-type: service-type,
        status: APPT-SCHEDULED,
        notes: "",
        location: location
      }
    )
    (var-set appointment-counter appointment-id)
    (ok appointment-id)
  )
)

(define-public (confirm-appointment (appointment-id uint))
  (let
    (
      (appointment (unwrap! (map-get? appointments { appointment-id: appointment-id }) ERR-NOT-FOUND))
    )
    (asserts! (is-eq (get status appointment) APPT-SCHEDULED) ERR-INVALID-STATUS)
    (map-set appointments
      { appointment-id: appointment-id }
      (merge appointment { status: APPT-CONFIRMED })
    )
    (ok true)
  )
)

;; Mobile clinic routing functions
(define-public (create-clinic-route
    (clinic-name (string-ascii 50))
    (route-date uint)
    (stops (list 10 (string-ascii 100)))
    (estimated-times (list 10 uint))
    (assigned-providers (list 5 uint))
  )
  (let
    (
      (route-id (+ (var-get route-counter) u1))
    )
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-UNAUTHORIZED)
    (asserts! (> route-date burn-block-height) ERR-INVALID-TIME)
    (map-set mobile-clinic-routes
      { route-id: route-id }
      {
        clinic-name: clinic-name,
        route-date: route-date,
        stops: stops,
        estimated-times: estimated-times,
        status: ROUTE-PLANNED,
        assigned-providers: assigned-providers
      }
    )
    (var-set route-counter route-id)
    (ok route-id)
  )
)

(define-public (activate-route (route-id uint))
  (let
    (
      (route (unwrap! (map-get? mobile-clinic-routes { route-id: route-id }) ERR-NOT-FOUND))
    )
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-UNAUTHORIZED)
    (asserts! (is-eq (get status route) ROUTE-PLANNED) ERR-INVALID-STATUS)
    (map-set mobile-clinic-routes
      { route-id: route-id }
      (merge route { status: ROUTE-ACTIVE })
    )
    (ok true)
  )
)

;; Specialist consultation functions
(define-public (request-specialist-consultation
    (local-provider-id uint)
    (specialist-id uint)
    (consultation-time uint)
    (consultation-type (string-ascii 30))
  )
  (let
    (
      (consultation-id (+ (var-get consultation-counter) u1))
    )
    (asserts! (> consultation-time burn-block-height) ERR-INVALID-TIME)
    (map-set specialist-consultations
      { consultation-id: consultation-id }
      {
        patient-id: tx-sender,
        local-provider-id: local-provider-id,
        specialist-id: specialist-id,
        consultation-time: consultation-time,
        consultation-type: consultation-type,
        status: APPT-SCHEDULED,
        diagnosis: "",
        recommendations: ""
      }
    )
    (var-set consultation-counter consultation-id)
    (ok consultation-id)
  )
)

;; Transportation coordination functions
(define-public (request-transportation
    (pickup-location (string-ascii 100))
    (destination (string-ascii 100))
    (requested-time uint)
    (appointment-id (optional uint))
  )
  (let
    (
      (request-id (+ (var-get transport-counter) u1))
    )
    (asserts! (> requested-time burn-block-height) ERR-INVALID-TIME)
    (map-set transportation-requests
      { request-id: request-id }
      {
        patient-id: tx-sender,
        pickup-location: pickup-location,
        destination: destination,
        requested-time: requested-time,
        appointment-id: appointment-id,
        status: APPT-SCHEDULED,
        driver-assigned: none
      }
    )
    (var-set transport-counter request-id)
    (ok request-id)
  )
)

(define-public (assign-driver (request-id uint) (driver principal))
  (let
    (
      (request (unwrap! (map-get? transportation-requests { request-id: request-id }) ERR-NOT-FOUND))
    )
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-UNAUTHORIZED)
    (map-set transportation-requests
      { request-id: request-id }
      (merge request { driver-assigned: (some driver), status: APPT-CONFIRMED })
    )
    (ok true)
  )
)

;; Read-only functions
(define-read-only (get-provider (provider-id uint))
  (map-get? healthcare-providers { provider-id: provider-id })
)

(define-read-only (get-appointment (appointment-id uint))
  (map-get? appointments { appointment-id: appointment-id })
)

(define-read-only (get-clinic-route (route-id uint))
  (map-get? mobile-clinic-routes { route-id: route-id })
)

(define-read-only (get-consultation (consultation-id uint))
  (map-get? specialist-consultations { consultation-id: consultation-id })
)

(define-read-only (get-transport-request (request-id uint))
  (map-get? transportation-requests { request-id: request-id })
)

(define-read-only (get-provider-count)
  (var-get provider-counter)
)

(define-read-only (get-appointment-count)
  (var-get appointment-counter)
)
