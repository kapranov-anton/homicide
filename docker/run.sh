#!/bin/bash
echo "Sleeping on Postgres at db:5432"
until nc -z db 5432; do
    echo "$(date) - waiting for postgres..."
    sleep 2
done

postgrest postgres://${POSTGREST_DBUSER}:${POSTGREST_DBPASS}@${POSTGREST_DBHOST}:${POSTGREST_DBPORT}/${POSTGREST_DBNAME} \
    --port 3000 \
    --schema ${POSTGREST_SCHEMA} \
    --anonymous ${POSTGREST_ANONYMOUS} \
    --pool ${POSTGREST_POOL} \
    --jwt-secret ${POSTGREST_JWT_SECRET} \
    --max-rows ${POSTGREST_MAX_ROWS}
