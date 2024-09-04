# vim: ft=python
import argparse
from enum import Enum, auto
import sys
import ssl

import requests
from cryptography import x509
from cryptography.hazmat.backends import default_backend
from cryptography.hazmat.primitives import hashes
from cryptography.hazmat.primitives.serialization import Encoding
from cryptography.x509.ocsp import OCSPRequestBuilder, OCSPResponseStatus


def load_certificate_from_url(url):
    """Retrieve and return the certificate from a given URL."""
    cert_pem = ssl.get_server_certificate((url, 443))
    cert = x509.load_pem_x509_certificate(cert_pem.encode("ascii"), default_backend())
    return cert


def get_ocsp_url(cert):
    """Extract the OCSP URL from the certificate."""
    aia = cert.extensions.get_extension_for_class(x509.AuthorityInformationAccess)
    for access_description in aia.value:
        if access_description.access_method == x509.AuthorityInformationAccessOID.OCSP:
            return access_description.access_location.value
    return None


def get_issuer_certificate(cert):
    """Fetch the issuer's certificate using the AIA extension."""
    aia = cert.extensions.get_extension_for_class(x509.AuthorityInformationAccess)
    issuer_url = None

    for access_description in aia.value:
        if (
            access_description.access_method
            == x509.AuthorityInformationAccessOID.CA_ISSUERS
        ):
            issuer_url = access_description.access_location.value
            break

    if issuer_url:
        response = requests.get(issuer_url)
        if response.status_code == 200:
            issuer_cert = x509.load_der_x509_certificate(
                response.content, default_backend()
            )
            return issuer_cert
    return None


class CheckResult(Enum):
    revoked = auto()
    good = auto()
    unknown = auto()


def check_cert_revocation(cert, issuer_cert, ocsp_url) -> CheckResult:
    """Check the revocation status of the certificate."""
    builder = OCSPRequestBuilder()
    builder = builder.add_certificate(cert, issuer_cert, hashes.SHA1())
    req = builder.build()

    headers = {"Content-Type": "application/ocsp-request"}
    ocsp_request_data = req.public_bytes(Encoding.DER)

    response = requests.post(ocsp_url, headers=headers, data=ocsp_request_data)
    ocsp_resp = x509.ocsp.load_der_ocsp_response(response.content)

    if ocsp_resp.response_status == OCSPResponseStatus.SUCCESSFUL:
        if ocsp_resp.certificate_status == x509.ocsp.OCSPCertStatus.REVOKED:
            return CheckResult.revoked
        elif ocsp_resp.certificate_status == x509.ocsp.OCSPCertStatus.GOOD:
            return CheckResult.good
        else:
            return CheckResult.unknown
    else:
        raise RuntimeError("OCSP request failed")


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("url")
    parser.add_argument("-v", "--verbose", action="store_true", default=False)
    args = parser.parse_args()

    cert = load_certificate_from_url(args.url)
    ocsp_url = get_ocsp_url(cert)
    issuer_cert = get_issuer_certificate(cert)

    if ocsp_url and issuer_cert:
        status = check_cert_revocation(cert, issuer_cert, ocsp_url)
        if args.verbose:
            print(status)
        match status:
            case CheckResult.good:
                raise SystemExit(0)
            case CheckResult.revoked:
                raise SystemExit(1)
            case CheckResult.unknown:
                raise SystemExit(20)
    else:
        print("Could not retrieve OCSP URL or issuer certificate.", file=sys.stderr)
        raise SystemExit(1)
