#!/bin/sh

echo "Content-Type: text/html"
echo ""

echo "<html>"
echo "<head><title>Very simple CGI</title></head>"
echo "<body>This process $$ serving '$REQUEST_METHOD $PATH_TRANSLATED'</body>"
echo "</html>"
