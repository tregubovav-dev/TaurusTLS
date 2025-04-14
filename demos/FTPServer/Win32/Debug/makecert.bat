openssl ecparam -genkey -name secp521r1 -out privkey.pem
openssl req -new -x509 -nodes -key privkey.pem -config openssl-min-req.cnf -subj "/CN=localhost" -out ca.pem
