"""
mitmproxy addon that records Claude Code's OTLP exports to daily JSONL files under ~/.local/share/otel-tap and exposes them as the `otel` view in telemetry.duckdb.
"""

import base64
import datetime
import json
import logging
import pathlib

import duckdb
from google.protobuf.json_format import MessageToDict
from google.protobuf.message import DecodeError
from mitmproxy import http
from opentelemetry.proto.collector.logs.v1.logs_service_pb2 import ExportLogsServiceRequest
from opentelemetry.proto.collector.metrics.v1.metrics_service_pb2 import ExportMetricsServiceRequest

DATA_DIR = pathlib.Path.home() / ".local" / "share" / "otel-tap"
DATABASE = DATA_DIR / "telemetry.duckdb"

REQUEST_TYPES = {
    "/v1/logs": ExportLogsServiceRequest,
    "/v1/metrics": ExportMetricsServiceRequest,
}

# The auth token must not end up in a plain file on disk.
REDACTED_HEADERS = {"authorization"}

VIEW_SQL = f"""
CREATE OR REPLACE VIEW otel AS
SELECT * FROM read_json(
    '{DATA_DIR}/*.jsonl',
    format = 'newline_delimited',
    columns = {{
        'received_at': 'TIMESTAMPTZ',
        'signal': 'VARCHAR',
        'host': 'VARCHAR',
        'status': 'INTEGER',
        'headers': 'MAP(VARCHAR, VARCHAR)',
        'body_b64': 'VARCHAR',
        'decoded': 'JSON'
    }}
)
"""


class OtelTap:
    def __init__(self):
        self.view_created = DATABASE.exists()

    def response(self, flow: http.HTTPFlow):
        self.record(flow)

    def error(self, flow: http.HTTPFlow):
        self.record(flow)

    def record(self, flow: http.HTTPFlow):
        request_type = REQUEST_TYPES.get(flow.request.path)
        if request_type is None:
            return

        received_at = datetime.datetime.fromtimestamp(flow.request.timestamp_start, datetime.UTC)
        entry = {
            "received_at": received_at.isoformat(),
            "signal": flow.request.path.removeprefix("/v1/"),
            "host": flow.request.pretty_host,
            "status": flow.response.status_code if flow.response else None,
            "headers": {
                name: value
                for name, value in flow.request.headers.items()
                if name.lower() not in REDACTED_HEADERS
            },
            "body_b64": base64.b64encode(flow.request.raw_content or b"").decode(),
            "decoded": decode(request_type, flow.request.content or b""),
        }

        DATA_DIR.mkdir(parents=True, exist_ok=True)
        with (DATA_DIR / f"{received_at:%Y-%m-%d}.jsonl").open("a") as f:
            f.write(json.dumps(entry) + "\n")

        # DuckDB cannot create a view over a glob that matches no files.
        if not self.view_created:
            with duckdb.connect(DATABASE) as db:
                db.execute(VIEW_SQL)
            self.view_created = True


def decode(request_type, body: bytes) -> dict | None:
    try:
        return MessageToDict(request_type.FromString(body))
    except DecodeError:
        logging.warning("could not decode %s payload", request_type.__name__)
        return None


addons = [OtelTap()]
