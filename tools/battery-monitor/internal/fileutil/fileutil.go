package fileutil

import (
	"fmt"
	"io/ioutil"
	"strconv"
	"strings"
)

// ReadFileContentTrimmed reads a file and returns the trimmed content.
func ReadFileContentTrimmed(filePath string) (string, error) {
	content, err := ioutil.ReadFile(filePath)
	if err != nil {
		return "", fmt.Errorf("failed to read file %s: %w", filePath, err)
	}
	return strings.TrimSpace(string(content)), nil
}

// ReadIntFromFile reads a file and parses its trimmed content as an integer.
func ReadIntFromFile(filePath string) (int, error) {
	content, err := ReadFileContentTrimmed(filePath)
	if err != nil {
		return 0, err
	}
	value, err := strconv.Atoi(content)
	if err != nil {
		return 0, fmt.Errorf("failed to parse content '%s' as integer from file %s: %w", content, filePath, err)
	}
	return value, nil
}