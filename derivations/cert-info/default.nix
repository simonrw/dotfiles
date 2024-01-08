{
  openssl,
  bat,
  writeShellScriptBin,
  ...
}:
writeShellScriptBin "cert-info" ''
  #!/usr/bin/env bash

  set -euo pipefail

  if [ ! $# -eq 1 ]; then
      echo "Usage: $0 <hostname>" >&2
      exit 1
  fi

  if [ -f $1 ]; then
    ${openssl}/bin/openssl x509 -noout -text -in $1 | \
    ${bat}/bin/bat
  else
    ${openssl}/bin/openssl s_client -showcerts -connect $1:443 2>/dev/null | \
    ${openssl}/bin/openssl x509 -inform pem -noout -text | \
    ${bat}/bin/bat
  fi
''
