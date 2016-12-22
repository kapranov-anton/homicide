FROM begriffs/postgrest
RUN apt-get update && apt-get install -y netcat

ENV POSTGREST_VERSION=0.3.2.0 \
    POSTGREST_DBHOST=db_host \
    POSTGREST_DBPORT=5432 \
    POSTGREST_DBNAME=db_name \
    POSTGREST_DBUSER=db_user \
    POSTGREST_DBPASS=db_pass \
    POSTGREST_SCHEMA=public \
    POSTGREST_ANONYMOUS=postgres \
    POSTGREST_JWT_SECRET=secret \
    POSTGREST_MAX_ROWS=1000000 \
    POSTGREST_POOL=200
ADD run.sh /run.sh
CMD /run.sh
