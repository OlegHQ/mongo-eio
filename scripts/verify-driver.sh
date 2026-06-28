#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "$0")/.." && pwd)"

POSTER_MONGO_HOST="${POSTER_MONGO_HOST:-oracle-vm}"
POSTER_MONGO_PORT="${POSTER_MONGO_PORT:-27017}"
RUN_STANDALONE_CONTAINER="${RUN_STANDALONE_CONTAINER:-0}"
STANDALONE_CONTAINER_NAME="${STANDALONE_CONTAINER_NAME:-mongo-eio-standalone-e2e-$$}"
STANDALONE_PORT="${STANDALONE_PORT:-27022}"
RUN_AUTH_CONTAINER="${RUN_AUTH_CONTAINER:-1}"
AUTH_CONTAINER_NAME="${AUTH_CONTAINER_NAME:-mongo-eio-auth-e2e-$$}"
AUTH_PORT="${AUTH_PORT:-27018}"
RUN_TLS_CONTAINER="${RUN_TLS_CONTAINER:-1}"
TLS_CONTAINER_NAME="${TLS_CONTAINER_NAME:-mongo-eio-tls-e2e-$$}"
TLS_PORT="${TLS_PORT:-27019}"
TLS_CERT_DIR=""
RUN_RS_CONTAINER="${RUN_RS_CONTAINER:-1}"
RS_CONTAINER_NAME="${RS_CONTAINER_NAME:-mongo-eio-rs-e2e-$$}"
RS_PORT="${RS_PORT:-27020}"
RS_NAME="${RS_NAME:-rs0}"
RUN_FAILOVER_CONTAINER="${RUN_FAILOVER_CONTAINER:-1}"
FAILOVER_CONTAINER_PREFIX="${FAILOVER_CONTAINER_PREFIX:-mongo-eio-failover-e2e-$$}"
FAILOVER_PORT1="${FAILOVER_PORT1:-27023}"
FAILOVER_PORT2="${FAILOVER_PORT2:-27024}"
FAILOVER_PORT3="${FAILOVER_PORT3:-27025}"
FAILOVER_RS_NAME="${FAILOVER_RS_NAME:-failoverRs}"
RUN_RETRY_CONTAINER="${RUN_RETRY_CONTAINER:-1}"
RETRY_CONTAINER_NAME="${RETRY_CONTAINER_NAME:-mongo-eio-retry-e2e-$$}"
RETRY_PORT="${RETRY_PORT:-27021}"
RETRY_RS_NAME="${RETRY_RS_NAME:-retryRs}"

run() {
  printf '==> %s\n' "$*"
  "$@"
}

cleanup_standalone_container() {
  if [[ -n "${STANDALONE_CONTAINER_NAME:-}" ]]; then
    docker rm -f "${STANDALONE_CONTAINER_NAME}" >/dev/null 2>&1 || true
  fi
}

cleanup_auth_container() {
  if [[ -n "${AUTH_CONTAINER_NAME:-}" ]]; then
    docker rm -f "${AUTH_CONTAINER_NAME}" >/dev/null 2>&1 || true
  fi
}

cleanup_tls_container() {
  if [[ -n "${TLS_CONTAINER_NAME:-}" ]]; then
    docker rm -f "${TLS_CONTAINER_NAME}" >/dev/null 2>&1 || true
  fi
  if [[ -n "${TLS_CERT_DIR:-}" ]]; then
    rm -rf "${TLS_CERT_DIR}"
  fi
}

cleanup_rs_container() {
  if [[ -n "${RS_CONTAINER_NAME:-}" ]]; then
    docker rm -f "${RS_CONTAINER_NAME}" >/dev/null 2>&1 || true
  fi
}

cleanup_failover_containers() {
  if [[ -n "${FAILOVER_CONTAINER_PREFIX:-}" ]]; then
    docker rm -f \
      "${FAILOVER_CONTAINER_PREFIX}-1" \
      "${FAILOVER_CONTAINER_PREFIX}-2" \
      "${FAILOVER_CONTAINER_PREFIX}-3" >/dev/null 2>&1 || true
  fi
}

cleanup_retry_container() {
  if [[ -n "${RETRY_CONTAINER_NAME:-}" ]]; then
    docker rm -f "${RETRY_CONTAINER_NAME}" >/dev/null 2>&1 || true
  fi
}

cleanup_all() {
  cleanup_standalone_container
  cleanup_tls_container
  cleanup_rs_container
  cleanup_failover_containers
  cleanup_retry_container
  cleanup_auth_container
}

generate_tls_certs() {
  TLS_CERT_DIR="$(mktemp -d)"
  openssl req -x509 -newkey rsa:2048 -days 1 -nodes \
    -keyout "${TLS_CERT_DIR}/ca.key" \
    -out "${TLS_CERT_DIR}/ca.pem" \
    -subj "/CN=mongo-eio-test-ca" >/dev/null 2>&1

  cat >"${TLS_CERT_DIR}/server.cnf" <<'EOF'
[req]
distinguished_name=req_distinguished_name
req_extensions=v3_req
prompt=no

[req_distinguished_name]
CN=localhost

[v3_req]
subjectAltName=@alt_names

[alt_names]
DNS.1=localhost
IP.1=127.0.0.1
EOF

  openssl req -newkey rsa:2048 -nodes \
    -keyout "${TLS_CERT_DIR}/server.key" \
    -out "${TLS_CERT_DIR}/server.csr" \
    -config "${TLS_CERT_DIR}/server.cnf" >/dev/null 2>&1
  openssl x509 -req \
    -in "${TLS_CERT_DIR}/server.csr" \
    -CA "${TLS_CERT_DIR}/ca.pem" \
    -CAkey "${TLS_CERT_DIR}/ca.key" \
    -CAcreateserial \
    -out "${TLS_CERT_DIR}/server.crt" \
    -days 1 \
    -extensions v3_req \
    -extfile "${TLS_CERT_DIR}/server.cnf" >/dev/null 2>&1
  cat "${TLS_CERT_DIR}/server.crt" "${TLS_CERT_DIR}/server.key" >"${TLS_CERT_DIR}/server.pem"
  chmod 0755 "${TLS_CERT_DIR}"
  chmod 0644 "${TLS_CERT_DIR}"/*.pem
}

trap cleanup_all EXIT

if [[ "${RUN_STANDALONE_CONTAINER}" == "1" ]]; then
  if ! command -v docker >/dev/null 2>&1; then
    echo "docker not found; set RUN_STANDALONE_CONTAINER=0 to use an external MongoDB" >&2
    exit 1
  fi

  cleanup_standalone_container
  run docker run -d --rm --name "${STANDALONE_CONTAINER_NAME}" \
    -p "127.0.0.1:${STANDALONE_PORT}:27017" \
    mongo:7 --bind_ip_all

  standalone_ready=0
  for _ in $(seq 1 60); do
    if docker exec "${STANDALONE_CONTAINER_NAME}" mongosh --quiet \
      --eval "db.adminCommand({ ping: 1 }).ok" >/dev/null 2>&1
    then
      standalone_ready=1
      break
    fi
    sleep 1
  done
  if [[ "${standalone_ready}" != "1" ]]; then
    echo "standalone MongoDB container did not become ready within timeout" >&2
    exit 1
  fi

  POSTER_MONGO_HOST="127.0.0.1"
  POSTER_MONGO_PORT="${STANDALONE_PORT}"
fi

run opam exec -- dune runtest --root "${ROOT}"

run env POSTER_MONGO_HOST="${POSTER_MONGO_HOST}" \
  POSTER_MONGO_PORT="${POSTER_MONGO_PORT}" \
  opam exec -- dune exec --root "${ROOT}" ./e2e/mongo_driver_e2e.exe

run env POSTER_MONGO_HOST="${POSTER_MONGO_HOST}" \
  POSTER_MONGO_PORT="${POSTER_MONGO_PORT}" \
  opam exec -- dune exec --root "${ROOT}" ./e2e/mongo_admin_e2e.exe

run env POSTER_MONGO_HOST="${POSTER_MONGO_HOST}" \
  POSTER_MONGO_PORT="${POSTER_MONGO_PORT}" \
  opam exec -- dune exec --root "${ROOT}" ./e2e/mongo_pool_e2e.exe

run env POSTER_MONGO_HOST="${POSTER_MONGO_HOST}" \
  POSTER_MONGO_PORT="${POSTER_MONGO_PORT}" \
  opam exec -- dune exec --root "${ROOT}" ./e2e/mongo_eio_direct_e2e.exe

if [[ "${RUN_TLS_CONTAINER}" == "1" ]]; then
  if ! command -v docker >/dev/null 2>&1; then
    echo "docker not found; set RUN_TLS_CONTAINER=0 to skip TLS container smoke" >&2
    exit 1
  fi
  if ! command -v openssl >/dev/null 2>&1; then
    echo "openssl not found; set RUN_TLS_CONTAINER=0 to skip TLS container smoke" >&2
    exit 1
  fi

  cleanup_tls_container
  generate_tls_certs
  run docker run -d --rm --name "${TLS_CONTAINER_NAME}" \
    -p "127.0.0.1:${TLS_PORT}:27017" \
    -v "${TLS_CERT_DIR}:/certs:ro" \
    mongo:7 --tlsMode requireTLS \
    --tlsCertificateKeyFile /certs/server.pem \
    --tlsCAFile /certs/ca.pem \
    --tlsAllowConnectionsWithoutCertificates \
    --bind_ip_all

  tls_passed=0
  for _ in $(seq 1 60); do
    if env MONGO_TLS_HOST="127.0.0.1" \
      MONGO_TLS_PORT="${TLS_PORT}" \
      MONGO_TLS_CA_FILE="${TLS_CERT_DIR}/ca.pem" \
      opam exec -- dune exec --root "${ROOT}" ./e2e/mongo_tls_e2e.exe
    then
      tls_passed=1
      break
    fi
    sleep 1
  done
  cleanup_tls_container
  if [[ "${tls_passed}" != "1" ]]; then
    echo "TLS container smoke did not pass within timeout" >&2
    exit 1
  fi
fi

if [[ "${RUN_RS_CONTAINER}" == "1" ]]; then
  if ! command -v docker >/dev/null 2>&1; then
    echo "docker not found; set RUN_RS_CONTAINER=0 to skip replica-set container smoke" >&2
    exit 1
  fi

  cleanup_rs_container
  run docker run -d --rm --name "${RS_CONTAINER_NAME}" \
    -p "127.0.0.1:${RS_PORT}:27017" \
    mongo:7 --replSet "${RS_NAME}" --bind_ip_all

  rs_ready=0
  for _ in $(seq 1 60); do
    if docker exec "${RS_CONTAINER_NAME}" mongosh --quiet \
      --eval "try { rs.initiate({_id: '${RS_NAME}', members: [{ _id: 0, host: '127.0.0.1:27017' }]}) } catch (e) { if (!String(e).includes('already initialized')) throw e }; db.adminCommand({ ping: 1 }).ok" >/dev/null 2>&1
    then
      rs_ready=1
      break
    fi
    sleep 1
  done
  if [[ "${rs_ready}" != "1" ]]; then
    echo "replica-set container did not initialize within timeout" >&2
    exit 1
  fi

  rs_passed=0
  for _ in $(seq 1 60); do
    if env MONGO_RS_HOST="127.0.0.1" \
      MONGO_RS_PORT="${RS_PORT}" \
      MONGO_RS_NAME="${RS_NAME}" \
      opam exec -- dune exec --root "${ROOT}" ./e2e/mongo_replicaset_e2e.exe
    then
      rs_passed=1
      break
    fi
    sleep 1
  done
  cleanup_rs_container
  if [[ "${rs_passed}" != "1" ]]; then
    echo "replica-set container smoke did not pass within timeout" >&2
    exit 1
  fi
fi

if [[ "${RUN_FAILOVER_CONTAINER}" == "1" ]]; then
  if ! command -v docker >/dev/null 2>&1; then
    echo "docker not found; set RUN_FAILOVER_CONTAINER=0 to skip failover smoke" >&2
    exit 1
  fi

  cleanup_failover_containers
  run docker run -d --rm --network host --name "${FAILOVER_CONTAINER_PREFIX}-1" \
    mongo:7 --replSet "${FAILOVER_RS_NAME}" --port "${FAILOVER_PORT1}" \
    --bind_ip_all
  run docker run -d --rm --network host --name "${FAILOVER_CONTAINER_PREFIX}-2" \
    mongo:7 --replSet "${FAILOVER_RS_NAME}" --port "${FAILOVER_PORT2}" \
    --bind_ip_all
  run docker run -d --rm --network host --name "${FAILOVER_CONTAINER_PREFIX}-3" \
    mongo:7 --replSet "${FAILOVER_RS_NAME}" --port "${FAILOVER_PORT3}" \
    --bind_ip_all

  failover_ready=0
  for _ in $(seq 1 90); do
    if docker exec "${FAILOVER_CONTAINER_PREFIX}-1" mongosh --quiet \
      --host 127.0.0.1 --port "${FAILOVER_PORT1}" \
      --eval "try { rs.initiate({_id: '${FAILOVER_RS_NAME}', members: [{ _id: 0, host: '127.0.0.1:${FAILOVER_PORT1}' }, { _id: 1, host: '127.0.0.1:${FAILOVER_PORT2}' }, { _id: 2, host: '127.0.0.1:${FAILOVER_PORT3}' }], settings: { electionTimeoutMillis: 1000 }}) } catch (e) { if (!String(e).includes('already initialized')) throw e }; db.adminCommand({ ping: 1 }).ok" >/dev/null 2>&1
    then
      failover_ready=1
      break
    fi
    sleep 1
  done
  if [[ "${failover_ready}" != "1" ]]; then
    echo "failover replica-set container did not initialize within timeout" >&2
    exit 1
  fi

  failover_primary_ready=0
  for _ in $(seq 1 90); do
    if docker exec "${FAILOVER_CONTAINER_PREFIX}-1" mongosh --quiet \
      --host 127.0.0.1 --port "${FAILOVER_PORT1}" \
      --eval "rs.status().members.some(m => m.stateStr === 'PRIMARY')" | grep -q true
    then
      failover_primary_ready=1
      break
    fi
    sleep 1
  done
  if [[ "${failover_primary_ready}" != "1" ]]; then
    echo "failover replica set did not elect a primary within timeout" >&2
    exit 1
  fi

  failover_passed=0
  for _ in $(seq 1 60); do
    if env MONGO_FAILOVER_HOSTS="127.0.0.1:${FAILOVER_PORT1},127.0.0.1:${FAILOVER_PORT2},127.0.0.1:${FAILOVER_PORT3}" \
      MONGO_FAILOVER_RS_NAME="${FAILOVER_RS_NAME}" \
      opam exec -- dune exec --root "${ROOT}" ./e2e/mongo_failover_e2e.exe
    then
      failover_passed=1
      break
    fi
    sleep 1
  done
  cleanup_failover_containers
  if [[ "${failover_passed}" != "1" ]]; then
    echo "failover replica-set smoke did not pass within timeout" >&2
    exit 1
  fi
fi

if [[ "${RUN_RETRY_CONTAINER}" == "1" ]]; then
  if ! command -v docker >/dev/null 2>&1; then
    echo "docker not found; set RUN_RETRY_CONTAINER=0 to skip retry failpoint smoke" >&2
    exit 1
  fi

  cleanup_retry_container
  run docker run -d --rm --name "${RETRY_CONTAINER_NAME}" \
    -p "127.0.0.1:${RETRY_PORT}:27017" \
    mongo:7 --replSet "${RETRY_RS_NAME}" --setParameter enableTestCommands=1 \
    --bind_ip_all

  retry_ready=0
  for _ in $(seq 1 60); do
    if docker exec "${RETRY_CONTAINER_NAME}" mongosh --quiet \
      --eval "try { rs.initiate({_id: '${RETRY_RS_NAME}', members: [{ _id: 0, host: '127.0.0.1:27017' }]}) } catch (e) { if (!String(e).includes('already initialized')) throw e }; db.adminCommand({ ping: 1 }).ok" >/dev/null 2>&1
    then
      retry_ready=1
      break
    fi
    sleep 1
  done
  if [[ "${retry_ready}" != "1" ]]; then
    echo "retry replica-set container did not initialize within timeout" >&2
    exit 1
  fi

  retry_passed=0
  for _ in $(seq 1 60); do
    if env MONGO_RETRY_HOST="127.0.0.1" \
      MONGO_RETRY_PORT="${RETRY_PORT}" \
      MONGO_RETRY_RS_NAME="${RETRY_RS_NAME}" \
      opam exec -- dune exec --root "${ROOT}" ./e2e/mongo_retry_e2e.exe
    then
      retry_passed=1
      break
    fi
    sleep 1
  done
  cleanup_retry_container
  if [[ "${retry_passed}" != "1" ]]; then
    echo "retry failpoint smoke did not pass within timeout" >&2
    exit 1
  fi
fi

if [[ "${RUN_AUTH_CONTAINER}" == "1" ]]; then
  if ! command -v docker >/dev/null 2>&1; then
    echo "docker not found; set RUN_AUTH_CONTAINER=0 to skip auth container smoke" >&2
    exit 1
  fi

  cleanup_auth_container
  run docker run -d --rm --name "${AUTH_CONTAINER_NAME}" \
    -p "127.0.0.1:${AUTH_PORT}:27017" \
    -e MONGO_INITDB_ROOT_USERNAME=root \
    -e MONGO_INITDB_ROOT_PASSWORD=secret \
    mongo:7

  for _ in $(seq 1 60); do
    if env MONGO_AUTH_URI="mongodb://root:secret@127.0.0.1:${AUTH_PORT}/admin?authMechanism=SCRAM-SHA-256" \
      MONGO_AUTH_BAD_URI="mongodb://root:wrong@127.0.0.1:${AUTH_PORT}/admin?authMechanism=SCRAM-SHA-256" \
      opam exec -- dune exec --root "${ROOT}" ./e2e/mongo_auth_e2e.exe
    then
      cleanup_auth_container
      exit 0
    fi
    sleep 1
  done

  echo "auth container smoke did not pass within timeout" >&2
  exit 1
fi
