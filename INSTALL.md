== BUILD ==

Enms is developed using 'Erlang R15B02 (erts-5.9.2)' but should work with any
Erlang versions. 

$ make


== DOC ==

(Partial) Documentation is generated using the Erlang 'edoc' tool. It is 
accessible in the 'doc' directory in each modules after issuing the following
command:

$ make doc


== RUNNING SERVER ==

Actualy, Enms only have a SNMP module wich have incomplete and indocumented 
API.
Before everything, you must modify the "enms.rel" file. Every modules versions
must match the versions found in your erlang path.

The following command will create a local release and start the server:

$ make start

Note that a 'sudo erl' is used to allow the snmp module to listen to the root 
port 162. You can change this by modifying the 'snmp' part of the 'sys.config'
file, and the Makefile.
Snmp configuration refere to the Erlang/OTP snmp module. Refere to the 
concerned documentation to modify these values.


== RUNNING CLI CLIENT == 

You can use the command line client "clifs" using the following commande in
another shell:

$ make clifs


== BUGS ==

This is an Alpha work. It is not meant to be used in a production environment
for now.
