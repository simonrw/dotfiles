package zedconfig

import (
	"bytes"
	"encoding/json"
	"errors"
	"fmt"
	"os"
)

func MergeFiles(targetPath, sourcePath string) ([]byte, error) {
	sourceData, err := os.ReadFile(sourcePath)
	if err != nil {
		return nil, fmt.Errorf("read source %q: %w", sourcePath, err)
	}

	targetData, err := os.ReadFile(targetPath)
	if err != nil && !errors.Is(err, os.ErrNotExist) {
		return nil, fmt.Errorf("read target %q: %w", targetPath, err)
	}

	return Merge(targetData, sourceData)
}

func Merge(targetData, sourceData []byte) ([]byte, error) {
	target, err := decodeJSONC(targetData)
	if err != nil {
		return nil, fmt.Errorf("parse target JSONC: %w", err)
	}

	source, err := decodeJSONC(sourceData)
	if err != nil {
		return nil, fmt.Errorf("parse source JSONC: %w", err)
	}

	merged := mergeMaps(target, source)

	var out bytes.Buffer
	encoder := json.NewEncoder(&out)
	encoder.SetIndent("", "  ")
	if err := encoder.Encode(merged); err != nil {
		return nil, fmt.Errorf("encode merged JSON: %w", err)
	}

	return out.Bytes(), nil
}

func decodeJSONC(data []byte) (map[string]any, error) {
	stripped, err := stripJSONC(data)
	if err != nil {
		return nil, err
	}

	var decoded map[string]any
	if len(bytes.TrimSpace(stripped)) == 0 {
		return map[string]any{}, nil
	}
	if err := json.Unmarshal(stripped, &decoded); err != nil {
		return nil, err
	}
	if decoded == nil {
		return map[string]any{}, nil
	}

	return decoded, nil
}

func mergeMaps(target, source map[string]any) map[string]any {
	merged := make(map[string]any, len(target)+len(source))
	for key, value := range target {
		merged[key] = value
	}

	for key, sourceValue := range source {
		if sourceMap, ok := sourceValue.(map[string]any); ok {
			if targetMap, ok := merged[key].(map[string]any); ok {
				merged[key] = mergeMaps(targetMap, sourceMap)
				continue
			}
		}

		merged[key] = sourceValue
	}

	return merged
}

func stripJSONC(data []byte) ([]byte, error) {
	withoutComments, err := stripComments(data)
	if err != nil {
		return nil, err
	}

	return stripTrailingCommas(withoutComments), nil
}

func stripComments(data []byte) ([]byte, error) {
	var out bytes.Buffer
	inString := false
	escaped := false

	for i := 0; i < len(data); i++ {
		ch := data[i]

		if inString {
			out.WriteByte(ch)
			if escaped {
				escaped = false
				continue
			}
			if ch == '\\' {
				escaped = true
				continue
			}
			if ch == '"' {
				inString = false
			}
			continue
		}

		if ch == '"' {
			inString = true
			out.WriteByte(ch)
			continue
		}

		if ch == '/' && i+1 < len(data) {
			next := data[i+1]
			if next == '/' {
				i += 2
				for i < len(data) && data[i] != '\n' && data[i] != '\r' {
					i++
				}
				if i < len(data) {
					out.WriteByte(data[i])
				}
				continue
			}
			if next == '*' {
				i += 2
				for i+1 < len(data) && !(data[i] == '*' && data[i+1] == '/') {
					if data[i] == '\n' || data[i] == '\r' {
						out.WriteByte(data[i])
					}
					i++
				}
				if i+1 >= len(data) {
					return nil, errors.New("unterminated block comment")
				}
				i++
				continue
			}
		}

		out.WriteByte(ch)
	}

	return out.Bytes(), nil
}

func stripTrailingCommas(data []byte) []byte {
	var out bytes.Buffer
	inString := false
	escaped := false

	for i := 0; i < len(data); i++ {
		ch := data[i]

		if inString {
			out.WriteByte(ch)
			if escaped {
				escaped = false
				continue
			}
			if ch == '\\' {
				escaped = true
				continue
			}
			if ch == '"' {
				inString = false
			}
			continue
		}

		if ch == '"' {
			inString = true
			out.WriteByte(ch)
			continue
		}

		if ch == ',' {
			next := i + 1
			for next < len(data) && isJSONWhitespace(data[next]) {
				next++
			}
			if next < len(data) && (data[next] == '}' || data[next] == ']') {
				continue
			}
		}

		out.WriteByte(ch)
	}

	return out.Bytes()
}

func isJSONWhitespace(ch byte) bool {
	return ch == ' ' || ch == '\n' || ch == '\r' || ch == '\t'
}
