FROM alpine:3.20

RUN apk add --no-cache dovecot

# unprivileged owner of the mail storage
RUN addgroup -S vmail \
    && adduser -S -G vmail -h /srv/mail vmail \
    && mkdir -p /srv/mail \
    && chown -R vmail:vmail /srv/mail

COPY dovecot.conf /etc/dovecot/dovecot.conf

EXPOSE 143

CMD ["dovecot", "-F"]
