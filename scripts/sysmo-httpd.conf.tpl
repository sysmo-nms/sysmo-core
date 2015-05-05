Listen 8080

SysmoDocRoot=@sysmoDocRoot@

<VirtualHost *:8080>
    <Directory SysmoDocRoot>
        SetOutputFilter DEFLATE
        Options -Indexes
        AllowOverride None
        Require all granted
    </Directory>

	ServerAdmin webmaster@localhost
	DocumentRoot SysmoDocRoot

	ErrorLog ${APACHE_LOG_DIR}/error.log
	CustomLog ${APACHE_LOG_DIR}/access.log combined

</VirtualHost>
