FROM haskell:8

RUN apt-get update && apt-get install -y postgresql libpq-dev
ENV PORT 8000
ENV DATABASE_URL "host=peertube_postgres_1 user=sss password=sss dbname=sss"
ENV LOG true

RUN useradd -ms /bin/bash slimemold

USER slimemold

WORKDIR /app

VOLUME /app

ENTRYPOINT ["sh", "./startup-dev.sh"]
