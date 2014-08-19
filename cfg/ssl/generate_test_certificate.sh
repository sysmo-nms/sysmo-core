#!/bin/sh

echo "Generating test certificate for server localhost."
echo "Used by the supercast server."
echo "MANDATORY: \"Common Name\" must be \"localhost\"."
echo "Other fields can be ignored."
echo -n "Press enter to begin: "
read
echo 
openssl genrsa -out key.pem 1024
openssl req -new -key key.pem -out request.pem
openssl x509 -req -days 30 -in request.pem -signkey key.pem -out certificate.pem
