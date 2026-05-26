package codexconfig

import (
	"bytes"
	"fmt"
	"os"

	"github.com/pelletier/go-toml/v2"
)

func MergeFiles(targetPath, sourcePath string) ([]byte, error) {
	sourceData, err := os.ReadFile(sourcePath)
	if err != nil {
		return nil, fmt.Errorf("read source %q: %w", sourcePath, err)
	}

	targetData, err := os.ReadFile(targetPath)
	if err != nil {
		return nil, fmt.Errorf("read target %q: %w", targetPath, err)
	}

	return Merge(targetData, sourceData)
}

func Merge(targetData, sourceData []byte) ([]byte, error) {
	var target map[string]any
	if err := toml.Unmarshal(targetData, &target); err != nil {
		return nil, fmt.Errorf("parse target TOML: %w", err)
	}

	var source map[string]any
	if err := toml.Unmarshal(sourceData, &source); err != nil {
		return nil, fmt.Errorf("parse source TOML: %w", err)
	}

	merged := mergeMaps(target, source)

	var out bytes.Buffer
	encoder := toml.NewEncoder(&out)
	encoder.SetIndentTables(true)
	if err := encoder.Encode(merged); err != nil {
		return nil, fmt.Errorf("encode merged TOML: %w", err)
	}

	return out.Bytes(), nil
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
