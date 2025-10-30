
FROM swipl:latest
WORKDIR /app
COPY . /app

EXPOSE 8080

CMD ["swipl", "-q", "-g", "start", "supermarket.pl"]
