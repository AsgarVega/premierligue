FROM swipl:8.4.2 as Base
COPY ./app /app
# ENTRYPOINT [ "swipl /app/start.pl" ]
